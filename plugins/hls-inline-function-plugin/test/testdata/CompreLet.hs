module CompreLet (subjects) where

-- Inlining the let-bound 'generalThings' splices its multi-line list
-- comprehension into the shallower 'in' body. The continuation lines
-- hang left of the head -- legal inside brackets -- so their column
-- deltas are negative; unnormalized they underflowed at the shallower
-- splice site and landed left of the case alternative's layout,
-- breaking the parse. retrie's normalizeHangingComprehensions now
-- clamps them to the comprehension's own anchor. Reduced from ghcide's
-- findLocalCompletions ('Inline generalCompls', soak violation).
subjects :: [[String]] -> [String]
subjects ds = case ds of
    (d : _) ->
        let generalThings = [ n ++ suffix
                | n <- d
                , let suffix = "!" ]
            others = ["end"]
        in
           generalThings ++ others
    [] -> []
