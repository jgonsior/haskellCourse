data MyBoolean = Yes | No deriving Show

if' :: MyBoolean -> t -> t -> t
if' Yes a b = a
if' No a b  = b


not' :: MyBoolean -> MyBoolean
not' Yes = No
not' No  = Yes

or' :: MyBoolean -> MyBoolean -> MyBoolean
or' Yes Yes = Yes
or' Yes No  = Yes
or' No Yes  = Yes
or' No No   = No

and' :: MyBoolean -> MyBoolean -> MyBoolean
and' Yes Yes = Yes
and' Yes No  = No
and' No Yes  = No
and' No No   = No

not'' t =
  case t of
    Yes -> No
    No  -> Yes
