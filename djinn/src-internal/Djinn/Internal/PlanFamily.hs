-- | Lazy admission of source-ordered optional search families. The tag is
-- scheduling policy only; a rejected family's generator must remain unforced.
module Djinn.Internal.PlanFamily (nextAdmittedPlanFamily) where

-- | Return the first admitted family and the unobserved source remainder.
-- Test the tag before inspecting the family's list, including its first node.
-- Neither an admitted family's tail nor later families are forced here.
nextAdmittedPlanFamily
    :: (tag -> Bool)
    -> [(tag, [plan])]
    -> Maybe (tag, [plan], [(tag, [plan])])
nextAdmittedPlanFamily suppress families = case families of
    [] -> Nothing
    (tag, plans) : remaining
        | suppress tag -> nextAdmittedPlanFamily suppress remaining
        | otherwise -> Just (tag, plans, remaining)
