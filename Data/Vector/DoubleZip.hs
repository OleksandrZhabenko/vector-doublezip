-- |
-- Module      :  Data.Vector.DoubleZip
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Some special functions to work with Vector (with zip).

{-# OPTIONS_GHC -threaded #-}

module Data.Vector.DoubleZip (
  normFst
  , normSnd
  , evalFstFV
  , evalFstFVM
  , evalSndFV
  , evalSndFVM
  , double42Float4
  , float42Double4
) where

import qualified Data.Vector as V
import GHC.Float (double2Float,float2Double)

-- | Norms a tuples in a 'V.Vector' by their first elements so that the greatest by an absolute value first element is equal to 1 (or -1). If all the first 
-- elements are zeros then prints a warning message and exits successfully.
normFst :: (Fractional a, Ord a) => V.Vector (a, b) -> IO (V.Vector (a, b))
normFst v 
 | V.all (\(x,_) -> x == 0) v = putStrLn "Warning: Data.Vector.DoubleZip.normFst: Vector with all zero first parts of the elements." >> return v
 | otherwise = V.mapM (\(x,y) -> return (x / abs (V.maximumBy (\t u -> compare (abs t) (abs u)) . V.map fst $ v),y)) v

-- | Norms a tuples in a 'V.Vector' by their second elements so that the greatest by an absolute value second element is equal to 1 (or -1). If all the second 
-- elements are zeros then prints a warning message and exits successfully.
normSnd :: (Fractional b, Ord b) => V.Vector (a, b) -> IO (V.Vector (a, b))
normSnd v 
 | V.all (\(_,y) -> y == 0) v = putStrLn "Warning: Data.Vector.DoubleZip.normSnd: Vector with all zero second parts of the elements." >> return v
 | otherwise = V.mapM (\(x,y) -> return (x,y / abs (V.maximumBy (\t u -> compare (abs t) (abs u)) . V.map snd $ v))) v

-- | A special function transformation to obtain the resulting 'V.Vector' so that its first elements in the inner tuples are in a special way 
-- normed to 1 (or -1) by 'normFst' and the inner tuples are sequenced one by another as a 2D points of the generating function @f :: a -> b@. 
-- To obtain non-empty result the given second argument must have at least two elements.
evalFstFV :: (Fractional a, Ord a) => (a -> b) -> V.Vector a -> IO (V.Vector ((a, b), (a, b)))
evalFstFV f v 
  | compare (V.length v) 1 == GT = do
     zipped <- normFst . V.zip v $ V.map f v
     return . V.zip zipped . V.unsafeSlice 1 (V.length v - 1) $ zipped
  | otherwise = return V.empty     

-- | The same as 'evalFstFV' but uses a monadic IO function f.
evalFstFVM :: (Fractional a, Ord a) => (a -> IO b) -> V.Vector a -> IO (V.Vector ((a, b), (a, b)))
evalFstFVM f v 
  | compare (V.length v) 1 == GT = do
     v1 <- V.mapM f v
     zipped <- normFst . V.zip v $ v1
     return . V.zip zipped . V.unsafeSlice 1 (V.length v - 1) $ zipped
  | otherwise = return V.empty

-- | A special function transformation to obtain the resulting 'V.Vector' so that its second elements in the inner tuples are in a special way 
-- normed to 1 (or -1) by 'normSnd' and the inner tuples are sequenced one by another as a 2D points of the generating function @f :: a -> b@. 
-- To obtain non-empty result the given second argument must have at least two elements. Is similar to 'evalFstFV'.
evalSndFV :: (Fractional b, Ord b) => (a -> b) -> V.Vector a -> IO (V.Vector ((a, b), (a, b)))
evalSndFV f v 
  | compare (V.length v) 1 == GT = do
     zipped <- normSnd . V.zip v $ V.map f v
     return . V.zip zipped . V.unsafeSlice 1 (V.length v - 1) $ zipped
  | otherwise = return V.empty     

-- | The same as 'evalSndFV' but uses a monadic IO function f.
evalSndFVM :: (Fractional b, Ord b) => (a -> IO b) -> V.Vector a -> IO (V.Vector ((a, b), (a, b)))
evalSndFVM f v 
  | compare (V.length v) 1 == GT = do
     v1 <- V.mapM f v
     zipped <- normSnd . V.zip v $ v1
     return . V.zip zipped . V.unsafeSlice 1 (V.length v - 1) $ zipped
  | otherwise = return V.empty

double42Float4 :: ((Double,Double), (Double,Double)) -> ((Float,Float), (Float,Float))
double42Float4 ((x,y),(z,t)) = ((double2Float x,double2Float y),(double2Float z,double2Float t))

float42Double4 :: ((Float,Float), (Float,Float)) -> ((Double,Double), (Double,Double))
float42Double4 ((x,y),(z,t)) = ((float2Double x,float2Double y),(float2Double z,float2Double t))
