data MyBoolean = Yes | No

if' :: MyBoolean -> t -> t -> t
if' Yes a b = a
if' No a b = b
