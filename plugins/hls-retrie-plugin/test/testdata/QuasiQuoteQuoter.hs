module QuasiQuoteQuoter (str) where

import           Language.Haskell.TH.Lib   (litE, stringL)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | A trivial quasi-quoter: @[str|...|]@ expands to the string literal of
-- its contents. Enough to give 'QuasiQuoteDef' a quasi-quote body to inline.
str :: QuasiQuoter
str =
  QuasiQuoter
    { quoteExp  = \s -> litE (stringL s)
    , quotePat  = error "str: pattern context unsupported"
    , quoteType = error "str: type context unsupported"
    , quoteDec  = error "str: declaration context unsupported"
    }
