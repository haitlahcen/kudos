(Lam Number:*.
 Lam Succ:Number -> Number.
 Lam Zero:Number.
 Lam Plus:Number -> Number -> Number.
 Lam foldN:Number -> Pi R:*. (R -> R) -> R -> R.

 Lam Vect:Number -> * -> *.
 Lam Cons:Pi k:Number. Pi A:*. A -> (Vect k A) -> (Vect (Succ k) A).
 Lam Nil:Pi A:*. Vect Zero A.
 Lam foldV:Pi k:Number.
           Pi A:*. (Vect k A) -> Pi R:*. (A -> R -> R) -> R -> R.

 foldV (Succ (Succ (Succ Zero)))
       Nat
       (Cons (Succ (Succ Zero)) Nat 34
        (Cons (Succ Zero) Nat 23
         (Cons Zero Nat 45 (Nil Nat))))
       Nat
       (Lam x:Nat. Lam y:Nat. x)
       12)

(Pi N:*. (N -> N) -> N -> N)
(Lam x:Pi N:*. (N -> N) -> N -> N.
 Lam N:*.
 Lam Succ:N -> N.
 Lam Zero:N.
 Succ (x N Succ Zero))
(Lam N:*.
 Lam Succ:N -> N.
 Lam Zero:N.
 Zero)
(Lam y:Pi N:*. (N -> N) -> N -> N.
 y (Pi N:*. (N -> N) -> N -> N)
   (Lam x:Pi N:*. (N -> N) -> N -> N.
    Lam N:*.
    Lam Succ:N -> N.
    Lam Zero:N.
    Succ (x N Succ Zero)))
(Lam k:Pi N:*. (N -> N) -> N -> N.
 Lam R:*.
 Lam succ:R -> R.
 Lam zero:R.
 k R succ zero)

(Lam k:Pi N:*. (N -> N) -> N -> N.
 Lam A:*.
 Pi R:*. (A -> R -> R) -> R -> R)
(Lam k:Pi N:*. (N -> N) -> N -> N.
 Lam A:*.
 Lam x:A.
 Lam xs:Pi R:*. (A -> R -> R) -> R -> R.
 Lam R:*.
 Lam Cons:A -> R -> R.
 Lam Nil:R.
 Cons x (xs R Cons Nil))
(Lam A:*.
 Lam R:*.
 Lam Cons:A -> R -> R.
 Lam Nil:R.
 Nil)
(Lam k:Pi N:*. (N -> N) -> N -> N.
 Lam A:*.
 Lam v:Pi R:*. (A -> R -> R) -> R -> R.
 Lam R:*.
 Lam cons:A -> R -> R.
 Lam nil:R.
 v R cons nil)