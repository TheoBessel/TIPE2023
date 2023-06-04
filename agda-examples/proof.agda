module Proof  where

open import Agda.Primitive

-- ======================================================================
-- Define == operator
-- ======================================================================

data Equal {a : Level} {X : Set a} : X -> X -> Set a where
  equal : {x : X} ->  Equal x x

_==_ : _
_==_ = Equal

-- ======================================================================
-- Define natural numbers and opperations
-- ======================================================================

data N : Set where
  zero : N
  suc : N -> N

_+_ : N -> N -> N
zero + n = n
suc m + n = suc (m + n)

_*_ : N -> N -> N
zero * n = zero
suc m * n = (m * n) + n

one : N
one = suc zero

two : N
two = suc one

-- ======================================================================
-- Prove some properties on equality
-- ======================================================================

=-refl : (n : N) -> n == n
=-refl n = equal

=-cong : {A B : Set} -> (f : A -> B) -> {x y : A} -> x == y -> f x == f y
=-cong f equal = equal

=-stab : (a b : N) -> (c : N) -> a == b -> (a + c) == (b + c)
=-stab a b c = \ x -> (=-cong (\ x -> (x + c)) x)

=-rev-stab : (a b : N) -> (c : N) -> a == b -> (c + a) == (c +  b)
=-rev-stab a b c  = \x -> (=-cong (\x -> (c + x)) x)

=-sym : {a b : N} -> a == b -> b == a
=-sym equal = =-cong ( \ x -> x) equal

=-trans : (a b c : N) -> a == b -> b == c -> a == c
=-trans zero zero zero p1 p2 = equal
=-trans zero zero (suc c) equal ()
=-trans zero (suc b) c () p2
=-trans (suc a) .(suc a) .(suc a) equal equal = equal

-- ======================================================================
-- Prove some properties on addition
-- ======================================================================

+-assoc : (a b c : N) -> ((a + b) + c) == (a + (b + c))
+-assoc zero b c = equal
+-assoc (suc a) b c = =-sym (
  =-trans (suc a + (b + c)) (suc (a + (b + c))) ((suc a + b) + c) equal (
    =-trans (suc (a + (b + c))) (suc ((a + b) + c)) ((suc a + b) + c) (
      =-cong suc (=-sym (+-assoc a b c))
    ) equal
  )
)

+-zero : (n : N) -> (n + zero) == n
+-zero zero = equal
+-zero (suc n) = =-cong suc (+-zero n)

+-suc-rev : (a b : N) -> (a + suc b) == suc (a + b) 
+-suc-rev zero b = equal
+-suc-rev (suc a) b = =-cong suc (+-suc-rev a b)

+-comm : (a b : N) -> (a + b) == (b + a)
+-comm a zero  = +-zero a
+-comm a (suc b) = (
  =-trans (a + suc b) (suc (a + b)) ((suc b) + a) (+-suc-rev a b) (
    =-sym (=-cong suc (+-comm b a))
  )
)

+-perm : (a b c d : N) -> ((a + b) + (c + d)) == ((a + c) + (b + d))
+-perm zero b c d = (
  =-trans ((zero + b) + (c + d)) (b + (c + d)) ((zero + c) + (b + d)) equal (
    =-trans (b + (c + d)) ((b + c) + d) ((zero + c) + (b + d)) (=-sym (+-assoc b c d)) (
      =-trans ((b + c) + d) ((c + b) + d) ((zero + c) + (b + d)) (
        =-stab (b + c) (c + b) d (+-comm b c)
      ) (
        =-trans ((c + b) + d) (c + (b + d)) ((zero + c) + (b + d)) (+-assoc c b d) equal
      )
    )
  )
)
+-perm (suc a) b c d = (
  =-trans ((suc a + b) + (c + d)) ((suc (a + b)) + (c + d)) ((suc a + c) + (b + d)) equal (
    =-trans (suc (a + b) + (c + d)) (suc ((a + b) + (c + d))) ((suc a + c) + (b + d)) equal (
      =-trans (suc ((a + b) + (c + d))) (suc ((a + c) + (b + d))) ((suc a + c) + (b + d)) (=-cong suc (+-perm a b c d)) (
        =-sym (
          =-trans ((suc a + c) + (b + d)) ((suc (a + c)) + (b + d)) (suc ((a + c) + (b + d))) equal equal
        )
      )
    )
  )
)

-- ======================================================================
-- Prove some properties on multiplication
-- ======================================================================

*+-dist : (k n m : N) -> (k * (n + m)) == ((k * n) + (k * m))
*+-dist zero n m = equal
*+-dist (suc k) n m = (
  =-trans (suc k * (n + m)) ((k * (n + m)) + (n + m)) (((suc k * n) + (suc k * m))) equal (=-sym (
    =-trans ((suc k * n) + (suc k * m)) (((k * n) + n) + ((k * m) + m)) ((k * (n + m)) + (n + m)) equal (
      =-trans (((k * n) + n) + ((k * m) + m)) (((k * n) + (k * m)) + (n + m)) ((k * (n + m)) + (n + m)) (+-perm (k * n) n (k * m) m) (=-sym (=-stab (k * (n + m)) ((k * n) + (k * m)) (n + m) (*+-dist k n m))))
    )
  )
)

*-zero : (n : N) -> (n * zero) == zero
*-zero zero = equal
*-zero (suc n) (
  = =-trans (suc n * zero) ((n * zero) + zero) zero equal (
    =-trans ((n * zero) + zero) (n * zero) zero (+-zero (n * zero)) (*-zero n)
  )
)

*-one : (n : N) -> (n * one) == n
*-one zero = equal
*-one (suc n) = (
  =-trans (suc n * one) ((n * one) + one) (suc n) equal (
    =-trans ((n * one) + one) (n + one) (suc n) (
      =-stab (n * one) (n) one (*-one n)
    ) (
      =-trans (n + one) (one + n) (suc n) (+-comm n one) equal
    )
  )
)

*-comm : (a b : N) -> (a * b) == (b * a)
*-comm zero b = (
  =-trans (zero * b) zero (b * zero) equal (=-sym (*-zero b))
)
*-comm (suc a) b = (  
  =-trans (suc a * b) ((a * b) + b) (b * suc a) equal (
    =-trans ((a * b) + b) (b + (a * b)) (b * suc a) (+-comm (a * b) b) (
      =-trans (b + (a * b)) (b + (b * a)) (b * suc a) (
        =-rev-stab (a * b) (b * a) b (*-comm  a b)
      ) (
        =-trans (b + (b * a)) ((b * one) + (b * a)) (b * (suc a)) (=-sym (
          =-stab (b * one) (b) (b * a) (*-one b)
        )) (
          =-trans ((b * one) + (b * a)) (b * (one + a)) (b * suc a) (=-sym (*+-dist b one a)) equal
        )
      )
    )
  )
)

-- ======================================================================
-- Prove the correction of sum function defined below
-- ======================================================================

sum : N -> N
sum zero = zero
sum (suc n) = suc n + sum n

sum-proof : (n : N) -> (two * (sum n)) == (n * (suc n))
sum-proof zero = equal
sum-proof (suc n) = (
  =-trans (two * sum (suc n)) (two * ((suc n) + (sum n))) (suc n * suc (suc n)) equal (
    =-trans (two * (suc n + sum n)) ((two * (suc n)) + (two * (sum n))) (suc n * suc (suc n)) (*+-dist two (suc n) (sum n)) (
      =-trans ((two * suc n) + (two * sum n)) ((two * suc n) + (n * (suc n))) ((suc n * suc (suc n))) (
        =-rev-stab (two * sum n) (n * suc n) (two * suc n) (sum-proof n)
      )(
        =-trans ((two * suc n) + (n * suc n)) (((suc n) * two) + (n * suc n)) (suc n * suc (suc n)) (
          =-stab (two * suc n) (suc n * two) (n * suc n) (*-comm two (suc n))
        )(
          =-trans (((suc n) * two) + (n * suc n)) (((suc n) * two) + ((suc n) * n)) (suc n * suc (suc n)) (
            =-rev-stab (n * suc n) (suc n * n) (suc n * two) (*-comm n (suc n))
          ) (=-sym (*+-dist (suc n) two n))
        )
      )
    )
  )
)