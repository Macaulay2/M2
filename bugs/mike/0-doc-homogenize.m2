The doc for 'basis' (basis-doc.m2) is wrong, and also overwhelming and unclear.

The part that is clear is wrong. (well, part is...!)

-1 default value: infinity (or is it?)
error message is given if not finitely generated 
output is actually generators over the coefficient ring,
NOT the field, in the default situation.
"Variables" option is hard for DE to understand.
Unclear what can be done with maps between modules
over different rings. Maybe a comment?

The input variable "i" could be an integer rather than a list.

(Also needs to be made functorial: see DE's code pushToField).
