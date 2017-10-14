module Main where

import Prelude

import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (benchmarkToStdout)
import Control.Monad.Eff (Eff)
import Data.Array ((..))
import Data.Foldable (foldMap, foldr, traverse_)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (ala)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

benchSum = mkBenchmark
  { slug: "sum"
  , title: "Finding the sum of an array"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary
  , functions: [ benchFn "foldr" (foldr (+) 0)
               , benchFn "foldMap" (ala Additive foldMap)
               ]
  }

benchProduct :: Benchmark
benchProduct = mkBenchmark
  { slug: "product"
  , title: "Finding the product of an array"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary
  , functions: [ benchFn "foldr" (foldr (*) 1)
               , benchFn "foldMap" (ala Multiplicative foldMap)
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = traverse_ benchmarkToStdout [benchSum, benchProduct]
