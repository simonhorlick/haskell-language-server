module QualifiedDoM ((>>=), (>>)) where

import Prelude hiding ((>>), (>>=))
import qualified Prelude

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = (Prelude.>>=)

(>>) :: Maybe a -> Maybe b -> Maybe b
(>>) = (Prelude.>>)
