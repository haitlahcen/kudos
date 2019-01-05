(Lam Hom: * -> * -> *.

 Lam id: Pi A:*. Hom A A.

 Lam comp: Pi A:*.
              Pi B:*.
              Pi C:*.
              (Hom A B) -> (Hom B C) -> (Hom A C).

 Lam Functor: * -> *.

 Lam pure: Pi A:*.
           Hom A (Functor A).

 Lam map: Pi A:*.
          Pi B:*.
          (Hom A B) -> (Hom (Functor A) (Functor B)).

 Lam Bifunctor: * -> * -> *.

 Lam bimap: Pi A:*.
            Pi B:*.
            Pi C:*.
            Pi D:*.
            (Hom A C) -> (Hom B D) -> (Hom (Bifunctor A B) (Bifunctor C D)).

 Lam Profunctor: * -> * -> *.

 Lam dimap: Pi A:*.
            Pi B:*.
            Pi C:*.
            Pi D:*.
            (Hom C A) -> (Hom B D) -> (Hom (Profunctor A B) (Profunctor C D)).
            *)

(Lam A:*. Lam B:*. A -> B)
(Lam A:*. Lam x:A. x)
(Lam A:*. Lam B:*. Lam C:*. Lam f:A -> B. Lam g:B -> C. Lam x:A. g (f x))

(Lam A:*. Pi R:*. (A -> R) -> R -> R)
(Lam A:*. Lam x:A. Lam R:*. Lam k:A -> R. Lam s:R. k x)
(Lam A:*.
 Lam B:*.
 Lam f:A -> B.
 Lam fa: Pi R:*. (A -> R) -> R -> R.
 Lam R:*.
 Lam k:B -> R.
 Lam s:R.
 fa R (Lam x:A. k (f x)) s)

(Lam A:*. Lam B:*. Pi R:*. (A -> R) -> (B -> R) -> R)
(Lam A:*.
 Lam B:*.
 Lam C:*.
 Lam D:*.
 Lam f:A -> C.
 Lam g:B -> D.
 Lam fab: Pi R:*. (A -> R) -> (B -> R) -> R.
 Lam R:*.
 Lam l:C -> R.
 Lam r:D -> R.
 fab R (Lam x:A. l (f x)) (Lam y:B. r (g y)))

(Lam A:*. Lam B:*. A -> B)
(Lam A:*.
 Lam B:*.
 Lam C:*.
 Lam D:*.
 Lam ca:C -> A.
 Lam bd:B -> D.
 Lam ab: A -> B.
 Lam x:C.
 bd (ab (ca x)))