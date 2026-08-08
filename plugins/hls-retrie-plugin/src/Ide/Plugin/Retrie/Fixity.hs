{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Retrie.Fixity
  ( fixityEnvFor
  , FixedModule (..)
  , fixedModule
  ) where

import           Control.Monad                  (forM)
import           Data.Generics                  (everything, mkQ)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set                       as S
import           Development.IDE.Core.RuleTypes (TcModuleResult (..))
import           Development.IDE.GHC.Compat
import           Retrie.ExactPrint              (Annotated, fix, makeDeltaAst,
                                                 transformA, unsafeMkA)
import           Retrie.Fixity                  (FixityEnv, mkFixityEnv)

-- | Build the in-scope fixity environment that retrie needs to
-- parenthesize substituted operator expressions correctly.
--
-- Fixities of imported operators live in interface files and can only be
-- fetched per name via 'lookupFixityRn' in the renamer monad, so we collect
-- the operators used in the module and look each one up.
fixityEnvFor :: HscEnv -> TcGblEnv -> RenamedSource -> IO FixityEnv
fixityEnvFor hscEnv tcg rn =
  fmap (mkFixityEnv . fromMaybe [] . snd) $
    initTcWithGbl hscEnv tcg (realSrcLocSpan (mkRealSrcLoc "<dummy>" 1 1)) $
      forM (S.toList (collectOpNames rn)) $ \name -> do
        fixity <-
          handleGhcException
            (const $ pure defaultFixity)
            (lookupFixityRn name)
        let fs = occNameFS (nameOccName name)
        pure (fs, (fs, fixity))

collectOpNames :: RenamedSource -> S.Set Name
collectOpNames = S.fromList . everything (<>) ([] `mkQ` opName)
  where
    opName :: HsExpr GhcRn -> [Name]
    opName (OpApp _ _ (L _ (HsVar _ ident)) _) = [unLocWithUserRdr ident]
    opName _                                   = []

-- | A module's parsed source with its operator chains re-associated
-- according to the module's in-scope fixities and the environment used
-- to do it.
--
-- The parser nests all operator chains left-associated, so any AST handed to
-- retrie must first be re-associated with the same fixities retrie
-- will parenthesize with; otherwise 'Retrie.Expr.parenify' reads the
-- wrong top operator off a substituted chain and drops or adds
-- parentheses.
data FixedModule = FixedModule
  { fmFixities :: FixityEnv
  , fmSource   :: Annotated ParsedSource
  }

-- | Build a 'FixedModule' from a typechecked module and its parsed
-- source. The source is taken separately rather than from 'tmrParsed'
-- so callers can supply the comment-preserving parse
-- ('GetAnnotatedParsedSource').
fixedModule :: HscEnv -> TcModuleResult -> ParsedSource -> IO FixedModule
fixedModule hscEnv check source = do
  fixities <- fixityEnvFor hscEnv (tmrTypechecked check) (tmrRenamed check)
  fixedSource <- transformA (unsafeMkA (makeDeltaAst source) 0) (fix fixities)
  pure FixedModule { fmFixities = fixities, fmSource = fixedSource }
