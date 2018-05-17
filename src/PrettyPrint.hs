module PrettyPrint where

import           Control.Monad
import           Data.List
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Text.Printf

import           Buchi

printTrans :: (Show a, Show s) => Set a -> GNBA (Set s) (Set a) -> IO ()
printTrans inputs gnba = do
  let trans_list = [((b, a), gnba_transition gnba b a) | b <- S.toList $ gnba_states gnba
                                                       , a <- S.toList $ S.powerSet inputs]
  forM_ trans_list $ \((b, a), b') -> do
    printf "%40s  (+)  %-10s  :->  %s\n" (showListHor $ S.toList b) (show $ S.toList a) (showListHorVert 64 $ map (showListHor . S.toList) $ S.toList b')

showListHor elems = "[" ++ (concat $ intersperse ", " $ map show elems) ++ "]"

showListHorVert indent list = showListHorVert' True indent list
showListHorVert' True indent []            = "∅"
showListHorVert' True indent [elem]        = printf "%s"  elem
showListHorVert' False indent [elem]       = printf "%s%s" (replicate indent ' ') elem
showListHorVert' True indent (elem:elems)  = (printf "%s\n" elem) ++ showListHorVert' False indent elems
showListHorVert' False indent (elem:elems) = (printf "%s%s\n" (replicate indent ' ') elem) ++ showListHorVert' False indent elems
