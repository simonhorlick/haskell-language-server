module ImportedPatSynNoExt where

import ImportedPatSynDef (e)

-- PatternSynonyms is off here: the import needs the keyword, so refuse
f :: Int
f = e 2
