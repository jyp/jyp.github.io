-- Database programming in Curry: family relationships

data Person = Adolf | Sybilla | Gustaf | Silvia | Victoria | Philip | Madeleine

parent :: Person -> Person -> Success
parent Adolf         Gustaf = success
parent Sybilla       Gustaf = success

parent Gustaf    Victoria   = success
parent Gustaf    Philip     = success
parent Gustaf    Madeleine  = success

parent Silvia        Victoria   = success
parent Silvia        Philip     = success
parent Silvia        Madeleine  = success

-- parent x y  ⇔  x ∈ parents y 
parents :: Person -> [Person]
parents Gustaf = [Adolf,Sybilla]
parents Silvia = []
parents Victoria = [Silvia,Gustaf]
-- ...

grandparent y x = parent y z & parent z x
   where z free

-- grandparent y x ⇔ y ∈ grandparents x
grandparents :: Person -> [Person]
grandparents x = --- concat (map parents (parents x))
                 [y | z <- parents x, 
                      y <- parents z]


---- We can ask who is a parent; who is a child...

-- query: parent Gustaf x where x free


-- exercises: define grandparent, grandgrandparent, ancestor, descendents

sibling :: Person -> Person -> Success
sibling x y = parent z x & parent z y
   where z free

-- exercise: cousin


data Gender = Male | Female

gender :: Person -> Gender
gender Adolf = Male
gender Gustaf = Male
gender Sybilla = Female
gender Silvia = Female
gender Madeleine = Female
gender Victoria = Male
gender Philip = Male


male :: Person -> Success
male x = gender x =:= Male
female x = gender x =:= Female

father :: Person -> Person -> Success
father y x = male y & parent y x

mother :: Person -> Person -> Success
mother y x = female y & parent y x






