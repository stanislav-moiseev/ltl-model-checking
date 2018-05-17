module Buchi
  ( GNBA(..)
  ) where

import           Data.Set (Set)
import qualified Data.Set as S


data GNBA s a = GNBA
  { gnba_states     :: Set s
  , gnba_initial    :: Set s
  , gnba_finals     :: Set (Set s)
  , gnba_transition :: s -> a -> Set s
  }



