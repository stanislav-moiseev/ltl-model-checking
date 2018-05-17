module LTL
  ( Formula
  , ltl2gnba
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Printf

import           Buchi

-- | @p@ represents atomic propositions.
data Formula p
  = FTrue
  | Prop p
  | Not (Formula p)
  | Conj (Formula p) (Formula p)
  | Next (Formula p)
  | Until (Formula p) (Formula p)
  deriving (Eq, Ord)

instance (Show p) => Show (Formula p) where
  show FTrue             = "⊤"
  show (Prop p)          = show p
  show (Not phi)         = printf "¬%s" (show phi)
  show (Conj phi1 phi2)  = printf "(%s ∧ %s)" (show phi1) (show phi2)
  show (Next phi)        = printf "X%s" (show phi)
  show (Until phi1 phi2) = printf "(%s U %s)" (show phi1) (show phi2)

reduceNegs :: Formula p -> Formula p
reduceNegs (Not (Not phi))   = reduceNegs phi
reduceNegs (Not phi)         = Not (reduceNegs phi)
reduceNegs (Prop p)          = Prop p
reduceNegs (Conj phi1 phi2)  = Conj (reduceNegs phi1) (reduceNegs phi2)
reduceNegs (Next phi)        = Next (reduceNegs phi)
reduceNegs (Until phi1 phi2) = Until (reduceNegs phi1) (reduceNegs phi2)

neg (Not phi) = phi
neg phi       = Not phi

-- | The set @closure φ@ is the set of all subformulae of @reduceNegs φ@ and
-- their negations (as obtained by the 'neg' function).
closure :: (Ord p) => Formula p -> Set (Formula p)
closure FTrue             = S.fromList [FTrue, Not FTrue]
closure (Prop p)          = S.fromList [Prop p, Not (Prop p)]
closure (Not phi)         = S.fromList [phi, Not phi]
closure (Conj phi1 phi2)  = S.unions [ S.fromList [Conj phi1 phi2, Not (Conj phi1 phi2)]
                                     , closure phi1
                                     , closure phi2 ]
closure (Next phi)        = S.unions [ S.fromList [Next phi, Not (Next phi)]
                                     , closure phi ]
closure (Until phi1 phi2) = S.unions [ S.fromList [Until phi1 phi2, Not (Until phi1 phi2)]
                                     , closure phi1
                                     , closure phi2 ]

-- | @elementarySets cl@ is the set of all elementary sets of formulae S ⊆ cl
-- where @cl == closure(φ)@ for some LTL formula φ.
-- The assumption @cl == closure(φ)@ is not checked.
elementarySets :: (Ord p) => Set (Formula p) -> Set (Set (Formula p))
elementarySets closure_phi =
  S.filter (\s -> propConsistent s && untilConsistent s && maximal s) $
  S.powerSet $ closure_phi
  where
    propConsistent s =
      (and [ (Conj phi1 phi2 `S.member` s) == (phi1 `S.member` s && phi2 `S.member` s)
           | Conj phi1 phi2 <- S.toList closure_phi]) &&
      (and [not $ (neg phi) `S.member` s | phi <- S.toList s]) &&
      ((FTrue `S.member` closure_phi) <= (FTrue `S.member` s))

    untilConsistent s =
      and [ (phi2 `S.member` s) <= (Until phi1 phi2 `S.member` s) &&
            (Until phi1 phi2 `S.member` s && not (phi2 `S.member` s)) <= (phi1 `S.member` s)
          | Until phi1 phi2 <- S.toList closure_phi]

    maximal s =
      and [ (not $ phi `S.member` s) <= (neg phi `S.member` s)
          | phi <- S.toList closure_phi]

ltl2gnba
  :: (Ord p)
  => Set p
  -> Formula p
  -> GNBA (Set (Formula p)) (Set p)
ltl2gnba all_props phi = GNBA states initial finals trans'
  where
    phi'        = reduceNegs phi
    closure_phi = closure phi'
    states      = elementarySets closure_phi
    initial     = S.filter (\s -> phi' `S.member` s) states

    finals      = S.fromList [ S.filter (\s -> (Until phi1 phi2 `S.member` s) <= (phi2 `S.member` s)) states
                             | Until phi1 phi2 <- S.toList closure_phi]
    trans b a
      | a /= S.fromList [p | Prop p <- S.toList b] = S.empty
      | otherwise =
          S.filter (\b' -> (and [ Next psi `S.member` b == psi `S.member` b'
                                | Next psi <- S.toList closure_phi ]) &&
                           (and [ Until phi1 phi2 `S.member` b == (phi2 `S.member` b || (phi1 `S.member` b && Until phi1 phi2 `S.member` b'))
                                | Until phi1 phi2 <- S.toList closure_phi]))
          states

    trans_map   = M.fromList [((b, a), trans b a) | b <- S.toList states
                                                  , a <- S.toList $ S.powerSet all_props]

    trans' b a  = let Just b' = (b, a) `M.lookup` trans_map in b'

