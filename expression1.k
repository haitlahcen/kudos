(Lam Identity:* -> *.
 Lam unIdentity:Pi A:*. (Identity A) -> A.

 Lam Product:* -> * -> *.
 Lam Pair:Pi A:*. Pi B:*. A -> B -> (Product A B).

 Lam Coproduct:* -> * -> *.
 Lam Left:Pi A:*. Pi B:*. A -> (Coproduct A B).
 Lam Right:Pi A:*. Pi B:*. B -> (Coproduct A B).

 Lam Maybe:* -> *.
 Lam Just:Pi A:*. A -> (Maybe A).
 Lam Nothing:Pi A:*. Maybe A.

 Lam List:* -> *.
 Lam Cons:Pi A:*. A -> (List A) -> (List A).
 Lam Nil:Pi A:*. List A.

 Lam A:*.
 Lam x:Identity A.
 unIdentity A x)

(Lam A:*. Pi R:*. (A -> R) -> R)
(Lam A:*.
 Lam idA:Pi R:*. (A -> R) -> R.
 idA A (Lam x:A. x))

(Lam A:*. Lam B:*. Pi R:*. (A -> B -> R) -> R)
(Lam A:*.
 Lam B:*.
 Lam x:A.
 Lam y:B.
 Lam R:*.
 Lam f:A -> B -> R.
 f x y)

(Lam A:*. Lam B:*. Pi R:*. (A -> R) -> (B -> R) -> R)
(Lam A:*.
 Lam B:*.
 Lam x:A.
 Lam R:*.
 Lam f:A -> R.
 Lam g:B -> R.
 f x)
(Lam A:*.
 Lam B:*.
 Lam y:B.
 Lam R:*.
 Lam f:A -> R.
 Lam g:B -> R.
 g y)

(Lam A:*. Pi R:*. (A -> R) -> R -> R)
(Lam A:*.
 Lam x:A.
 Lam R:*.
 Lam just:A -> R.
 Lam nothing:R.
 just x)
(Lam A:*.
 Lam R:*.
 Lam just:A -> R.
 Lam nothing:R.
 nothing)

(Lam A:*. Pi R:*. (A -> R -> R) -> R -> R)
(Lam A:*.
 Lam x:A.
 Lam xs:Pi R:*. (A -> R -> R) -> R -> R.
 Lam R:*.
 Lam cons:A -> R -> R.
 Lam nil:R.
 cons x (xs R cons nil))
(Lam A:*.
 Lam R:*.
 Lam cons:A -> R -> R.
 Lam nil:R.
 nil)