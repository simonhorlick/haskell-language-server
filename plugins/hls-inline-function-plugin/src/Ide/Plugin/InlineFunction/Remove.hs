-- | Decide whether inlining every call site leaves the function's
-- definition unused, and build the edits that delete it.
--
-- Removal is deliberately conservative: the definition is only deleted
-- when the plugin can prove nothing will reference it afterwards. The
-- proof obligations are split into pure pieces so each can be tested on
-- its own:
--
-- * 'definitionSpans' -- the definition is a top-level binding whose
--   type signature (if any) covers it alone, and yields the source
--   spans deletion must cover.
-- * 'refsCovered' -- every occurrence of the name in a module lies
--   inside a region the rewrite eliminates (a call site's head
--   reference, or the definition itself).
-- * 'sitesDischarged' -- retrie actually grafted every requested call
--   site; a site it refused (to avoid variable capture, say) keeps its
--   reference alive.
-- * 'deletionEdits' -- the whole-line edits that delete the spans.
--
-- The caller additionally checks the name is not exported: an exported
-- function can be referenced from modules the reference index has not
-- seen yet, so only a module-private definition is ever removed.
module Ide.Plugin.InlineFunction.Remove
  ( definitionSpans
  , refsCovered
  , sitesDischarged
  , deletionEdits
  ) where

import           Control.Monad                     (guard)
import           Data.Generics                     (Data, listify)
import qualified Data.Map                          as M
import           Data.Maybe                        (mapMaybe)
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import           Development.IDE.GHC.Compat
import           Ide.Plugin.InlineFunction.Resolve
import           Ide.Plugin.InlineFunction.Util
import           Language.LSP.Protocol.Types       (Position (..), Range (..),
                                                    TextEdit (..))

-- | The source spans deleting @name@'s definition must cover: the
-- 'FunBind' itself and its type signature, when it has one. 'Nothing'
-- when the definition cannot be deleted cleanly:
--
-- * the binding is not top-level -- deleting a @let@ or @where@ binding
--   can leave the enclosing construct empty, so local bindings are left
--   alone;
-- * a type signature mentioning the name also covers other names
--   (@e, f :: Int@) -- deleting it would strip their signature, and
--   keeping it would leave a signature without a binding.
--
-- Other declarations that would dangle -- a fixity declaration, an
-- @INLINE@ or @SPECIALISE@ pragma, an @ANN@ -- all carry a located
-- reference to the name, so 'refsCovered' vetoes removal for them.
definitionSpans :: RenamedSource -> Name -> Maybe [RealSrcSpan]
definitionSpans rn name = do
  let (group, _, _, _, _) = rn
  guard (name `elem` collectHsValBinders CollNoDictBinders (hs_valds group))
  bind    <- findBinder rn name
  bindSp  <- toRealSrcSpan (getLocA bind)
  sigSps  <- traverse deletableSigSpan (sigsMentioning name rn)
  pure (bindSp : sigSps)
  where
    deletableSigSpan :: LSig GhcRn -> Maybe RealSrcSpan
    deletableSigSpan lsig = case unLoc lsig of
      TypeSig _ [lname] _ | unLoc lname == name -> toRealSrcSpan (getLocA lsig)
      _                                         -> Nothing

-- | Every signature that references @name@ anywhere in the module. The
-- top-level binding's own type signature is the expected hit; anything
-- else (a multi-name signature, a pragma) blocks removal.
sigsMentioning :: Data a => Name -> a -> [LSig GhcRn]
sigsMentioning name = listify mentions
  where
    mentions :: LSig GhcRn -> Bool
    mentions lsig = not (null (listify (\n -> n == name) lsig))

-- | Whether every occurrence of @name@ in the renamed source lies inside
-- one of the covered spans. The rewrite only consumes the reference
-- heading each call site (argument subtrees are spliced back verbatim),
-- so the caller passes those head spans plus -- in the defining module --
-- the spans of the definition being deleted. An occurrence anywhere else
-- (an argument of another call, operator position at the wrong arity, a
-- visible type application, a @proc@ block, a fixity declaration or
-- pragma) survives the rewrite, so removal must be refused. An occurrence
-- without a real span cannot be verified and also refuses.
refsCovered :: [RealSrcSpan] -> Name -> RenamedSource -> Bool
refsCovered covered name rn = all coveredBy (nameRefSpans name rn)
  where
    coveredBy l = case toRealSrcSpan l of
      Nothing -> False
      Just sp -> any (`containsSpan` sp) covered

-- | Whether retrie actually rewrote every requested call site, judged
-- from the graft locations it reported. A site is discharged when a
-- graft landed on its application span, or on its head reference (the
-- lambda fallback rewrites just the bare head, leaving the argument in
-- place). A bare-reference site that heads another requested site has no
-- graft of its own -- the enclosing application's template consumes the
-- head -- so it is discharged along with that site.
sitesDischarged :: [CallSite] -> [SrcSpan] -> Bool
sitesDischarged sites grafts = all discharged sites
  where
    graftSet = S.fromList (mapMaybe toRealSrcSpan grafts)
    grafted s =
      s.application `S.member` graftSet || s.headRef `S.member` graftSet
    discharged s = grafted s || subsumed s
    subsumed s =
      null s.arguments
        && any
             (\o ->
                o.application /= s.application
                  && o.headRef == s.application
                  && grafted o)
             sites

-- | Whole-line deletion edits covering the given spans, positioned in
-- the exact-printed module text the rewrite's other edits also refer to
-- (the caller vets them against the real document the same way). Each
-- contiguous block of deleted lines swallows one following blank line,
-- so deleting a definition does not leave a double blank behind.
deletionEdits :: T.Text -> [RealSrcSpan] -> [TextEdit]
deletionEdits printed spans =
  [ TextEdit
      (Range (Position (fromIntegral a) 0) (Position (fromIntegral (b + 1)) 0))
      ""
  | (a, b) <- map extend (runs (S.toAscList lineSet))
  ]
  where
    docLines = M.fromList (zip [0 :: Int ..] (T.lines printed))
    lineSet =
      S.fromList
        [ l
        | sp <- spans
        , l <- [srcSpanStartLine sp - 1 .. srcSpanEndLine sp - 1]
        ]
    blank i = maybe False (T.null . T.strip) (M.lookup i docLines)
    extend (a, b)
      | (b + 1) `S.notMember` lineSet, blank (b + 1) = (a, b + 1)
      | otherwise = (a, b)
    -- group an ascending line list into contiguous (first, last) runs
    runs = foldr step []
      where
        step l ((a, b) : rest) | l + 1 == a = (l, b) : rest
        step l acc             = (l, l) : acc
