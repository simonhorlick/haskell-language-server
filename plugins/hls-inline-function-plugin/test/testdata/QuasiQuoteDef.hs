{-# LANGUAGE QuasiQuotes #-}
module QuasiQuoteDef (greeting) where

import           QuasiQuoteQuoter (str)

-- | The body is a quasi-quote, which only parses because this module
-- enables QuasiQuotes.
greeting :: String
greeting = [str|hello|]
