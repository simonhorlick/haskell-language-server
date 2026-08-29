{-# LANGUAGE CPP #-}
-- | Language extensions a spliced body needs in the target module.
module Ide.Plugin.Retrie.Extensions
  ( spliceExtensions
  , spliceableInto
  ) where

import           Data.Data                  (Data)
import           Data.List                  (intercalate, nub)
import           Data.Maybe                 (isJust)

import           Development.IDE.GHC.Compat (Extension (..), GhcPs)
import           GHC                        (HsConDetails (RecCon), HsExpr (..),
                                             HsRecFields (..),
                                             HsTupArg (Missing),
                                             HsUntypedSplice (HsQuasiQuote, HsUntypedSpliceExpr),
                                             Pat (..))
import qualified GHC.Data.EnumSet           as EnumSet
#if MIN_VERSION_ghc(9,10,0)
import           GHC                        (HsLamVariant (..))
#endif

import           Retrie.SYB                 (everything, extQ, mkQ)

import           Ide.Plugin.Retrie.Imports  (TargetScope (..))

-- | The extensions the node's syntax needs wherever it is printed.
spliceExtensions :: Data a => a -> [Extension]
spliceExtensions = nub . everything (++) ([] `mkQ` exprExts `extQ` patExts)
  where
    exprExts :: HsExpr GhcPs -> [Extension]
    exprExts (RecordCon _ _ HsRecFields{rec_dotdot})
      | isJust rec_dotdot                         = [RecordWildCards]
#if MIN_VERSION_ghc(9,10,0)
    exprExts (HsLam _ LamCase _)                  = [LambdaCase]
    exprExts (HsLam _ LamCases _)                 = [LambdaCase]
#else
    exprExts HsLamCase{}                          = [LambdaCase]
#endif
    exprExts HsMultiIf{}                          = [MultiWayIf]
    exprExts HsGetField{}                         = [OverloadedRecordDot]
    exprExts HsProjection{}                       = [OverloadedRecordDot]
    exprExts (ExplicitTuple _ args _)
      | any isMissing args                        = [TupleSections]
    exprExts (HsUntypedSplice _ HsQuasiQuote{})   = [QuasiQuotes]
    exprExts (HsUntypedSplice _ HsUntypedSpliceExpr{}) = [TemplateHaskell]
    exprExts HsTypedSplice{}                      = [TemplateHaskell]
    exprExts _                                    = []

    isMissing Missing{} = True
    isMissing _         = False

    patExts :: Pat GhcPs -> [Extension]
    patExts (ConPat _ _ (RecCon HsRecFields{rec_dotdot}))
      | isJust rec_dotdot = [RecordWildCards]
    patExts ViewPat{}     = [ViewPatterns]
    patExts _             = []

-- | Whether a target with the given scope can take a splice needing
-- these extensions; the reason when it cannot.
spliceableInto :: [Extension] -> TargetScope -> Either String ()
spliceableInto needed TargetScope{tsExtensions} =
  case filter (not . (`EnumSet.member` tsExtensions)) needed of
    []      -> Right ()
    missing ->
      Left $ "the inlined body needs " ++ intercalate ", " (map show missing)
        ++ " enabled in the target"
