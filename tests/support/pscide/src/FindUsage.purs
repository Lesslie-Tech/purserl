module FindUsage where

import FindUsage.Definition (usageId, ($%), Usage(..))
import FindUsage.Reexport (toBeReexported)

usagePatternMatch ∷ Usage → Usage
usagePatternMatch x = case x of
  Used _ → x
  _ → x

usageFn ∷ ∀ a. a → a
usageFn = usageId toBeReexported

usageOperator ∷ Usage
usageOperator = 1 $% 2
