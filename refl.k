let

  Eq:Pi A:*. (A -> A -> *) =
    Lam A:*.
    Lam x:A.
    Lam y:A.
    Pi Prop:A -> *.
    (Prop x) -> (Prop y),

  Refl:Pi A:*. Pi x:A. (Eq A x x) =
    Lam A:*.
    Lam x:A.
    Lam Prop:A -> *.
    Lam propX:Prop x.
    propX,

  rewrite:Pi A:*.
          Pi x:A.
          Pi y:A.
          Pi eqXY:Eq A x y.
          Pi Prop:A -> *.
          Pi propX:Prop x.
          (Prop y) =
    Lam A:*.
    Lam x:A.
    Lam y:A.
    Lam eqXY:Pi Prop:A -> *. (Prop x) -> (Prop y).
    Lam Prop:A -> *.
    Lam propX:Prop x.
    (eqXY Prop propX)

in
let

  sym:Pi A:*. Pi x:A. Pi y:A. Pi eqXY:Eq A x y. (Eq A y x) =
    Lam A:*.
    Lam x:A.
    Lam y:A.
    Lam eqXY:Eq A x y.
    (rewrite A x y eqXY (Lam z:A. Eq A z x) (Refl A x))

in
let

  Number:* = Pi N:*. ((N -> N) -> N -> N),

  Succ:(Number -> Number) =
    Lam x:Pi N:*. (N -> N) -> N -> N.
    Lam N:*.
    Lam Succ:N -> N.
    Lam Zero:N.
    (Succ (x N Succ Zero)),

  Zero:Number =
    Lam N:*.
    Lam Succ:N -> N.
    Lam Zero:N.
    Zero,

  Plus:(Number -> Number -> Number) =
    Lam y:Pi N:*. (N -> N) -> N -> N.
      y (Pi N:*. (N -> N) -> N -> N)
        (Lam x:Pi N:*. (N -> N) -> N -> N.
         Lam N:*.
         Lam Succ:N -> N.
         Lam Zero:N.
         Succ (x N Succ Zero)),

  foldNum:(Number -> Pi R:*. (R -> R) -> R -> R) =
    Lam n:Pi N:*. (N -> N) -> N -> N.
    Lam R:*.
    Lam Succ:R -> R.
    Lam Zero:R.
    (n R Succ Zero),

  plusZeroLeftNeutral:Pi n:Number. (Eq Number (Plus Zero n) n) =
    let N:* = (Pi N:*. (N -> N) -> N -> N)
    in
      Lam n:N.
      Refl N n,

  Vect:(Number -> * -> *) =
    Lam k:Pi N:*. (N -> N) -> N -> N.
    Lam A:*.
    Pi R:*. (A -> R -> R) -> R -> R,

  Cons:Pi k:Number. Pi A:*. (A -> (Vect k A) -> (Vect (Succ k) A)) =
    Lam k:Pi N:*. (N -> N) -> N -> N.
    Lam A:*.
    Lam x:A.
    Lam xs:Pi R:*. (A -> R -> R) -> R -> R.
    Lam R:*.
    Lam Cons:A -> R -> R.
    Lam Nil:R.
    Cons x (xs R Cons Nil),

  Nil:Pi A:*. (Vect Zero A) =
    Lam A:*.
    Lam R:*.
    Lam Cons:A -> R -> R.
    Lam Nil:R.
    Nil,

  Append:Pi m:Number. Pi n:Number. Pi A:*. ((Vect m A) -> (Vect n A) -> (Vect (Plus m n) A)) =
    let N:* = Pi N:*. ((N -> N) -> N -> N)
    in
      (Lam m:N.
       Lam n:N.
       Lam A:*.
       Lam u:Pi R:*. (A -> R -> R) -> R -> R.
       u (Pi R:*. (A -> R -> R) -> R -> R)
         (Lam x:A.
          Lam v:Pi R:*. (A -> R -> R) -> R -> R.
          Lam R:*.
          Lam Cons:A -> R -> R.
          Lam Nil:R.
          Cons x (v R Cons Nil)))

in Cons Zero
        Nat
        33
        (rewrite Number
                 (Plus Zero Zero)
                 Zero
                 (plusZeroLeftNeutral Zero)
                 (Lam x:Number. Vect x Nat)
                 (Append Zero
                         Zero
                         Nat
                         (Nil Nat)
                         (Nil Nat)))