module CtorParentProv (Name(..)) where

-- A type 'Name' whose constructor is also called 'Name'. Bringing the
-- constructor into scope through its parent ('Name(Name)') also brings the
-- type 'Name' into scope.
data Name = Name Int
