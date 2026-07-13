module WhereMultiEqn (partitionNodeResults, propagate) where

import           Data.Either        (partitionEithers)
import           Data.List.NonEmpty (nonEmpty)

data NodeResult = ErrorNode [Int] | SuccessNode [Int]

partitionNodeResults
    :: [(a, NodeResult)]
    -> ([(a, [Int])], [(a, [Int])])
partitionNodeResults = partitionEithers . map f
  where f (a, ErrorNode errs)   = Left (a, errs)
        f (a, SuccessNode imps) = Right (a, imps)

propagate :: NodeResult -> NodeResult
propagate n@(SuccessNode imps) =
  let results = map (\i -> (i, n)) imps
      (errs, _) = partitionNodeResults results
  in case nonEmpty errs of
       Nothing -> n
       Just _  -> n
propagate n = n
