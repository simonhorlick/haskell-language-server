module ConLabelNoDisamb where

import ConLabelDef (R (R), e)

-- without DisambiguateRecordFields a construction label resolves by
-- spelling, and a local field occupies it: the site must be refused
data S = S { name :: Int }

f :: R
f = e "x"
