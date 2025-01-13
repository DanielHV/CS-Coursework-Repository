{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Data.List (sort)

import LProp
import Semantica
import Equivalencias

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

sortMatrix :: Ord a => [[a]] -> [[a]]
sortMatrix = sort . map sort

specs :: Spec
specs = do
  describe "Semántica" $ do

    let f1 = (Var "s")
    let f2 = (Neg (Var "s"))
    let f3 = ((((Var "p") `Conj` (Var "q")) `Disy` (Var "s")) `Equiv` ((Neg (Var "p")) `Conj` (Var "q")))

    it "subconj" $ do
      subconj ([] :: [Int]) `shouldBe` ([[]] :: [[Int]])
      sortMatrix (subconj [1, 2, 3]) `shouldBe` sortMatrix [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]

    it "eliminaRepetidos" $ do
      eliminaRepetidos ([] :: [Int]) `shouldBe` ([] :: [Int])
      sort (eliminaRepetidos [1,2,3,1,2,3]) `shouldBe` [1,2,3]
      sort (eliminaRepetidos [1,2,3,1]) `shouldBe` [1,2,3]

    it "interp" $ do
      interp ["r","s"] f1 `shouldBe` True
      interp ["r","s"] (Var "z") `shouldBe` False
      interp ["r","s"] (Neg (Var "z")) `shouldBe` True
      interp ["q"] (((Var "p") `Impl` ((Var "r") `Disy` (Var "a"))) `Impl` (Var "r")) `shouldBe` False
      interp ["q","s"] f3 `shouldBe` True

    it "vars" $ do
      sort (vars f1) `shouldBe` ["s"]
      sort (vars f2) `shouldBe` ["s"]
      sort (vars f3) `shouldBe` ["p","q","s"]

    it "estados" $ do
      sortMatrix (estados f1) `shouldBe` sortMatrix [[], ["s"]]
      sortMatrix (estados f2) `shouldBe` sortMatrix [[], ["s"]]
      sortMatrix (estados f3) `shouldBe` sortMatrix [[],["p"],["p","q"],["p","q","s"],["p","s"],["q"],["q","s"],["s"]]

    it "modelos" $ do
      sortMatrix (modelos f1) `shouldBe` [["s"]]
      sortMatrix (modelos f2) `shouldBe` [[]]
      sortMatrix (modelos f3) `shouldBe` [[],["p"],["q","s"]]

    it "tautologia" $ do
      tautologia (Disy (Var "p") (Neg (Var "p"))) `shouldBe` True
      tautologia (Conj (Var "p") (Neg (Var "p"))) `shouldBe` False
      tautologia (Equiv (Var "p") (Var "p")) `shouldBe` True
      tautologia (Equiv (Var "p") (Neg (Var "p"))) `shouldBe` False

    it "satisfacibleEn" $ do
      satisfacibleEn ["r","s"] f1 `shouldBe` interp ["r","s"] f1
      satisfacibleEn ["r","s"] (Var "z") `shouldBe` interp ["r","s"] (Var "z")
      satisfacibleEn ["r","s"] (Neg (Var "z")) `shouldBe` interp ["r","s"] (Neg (Var "z"))
      satisfacibleEn ["q"] (((Var "p") `Impl` ((Var "r") `Disy` (Var "a"))) `Impl` (Var "r")) `shouldBe` interp ["q"] (((Var "p") `Impl` ((Var "r") `Disy` (Var "a"))) `Impl` (Var "r"))
      satisfacibleEn ["q","s"] f3 `shouldBe` interp ["q","s"] f3

    it "satisfacible" $ do
      satisfacible f1 `shouldBe` True
      satisfacible (((Var "p") `Impl` ((Var "r") `Disy` (Var "a"))) `Impl` (Var "r")) `shouldBe` True
      satisfacible f3 `shouldBe` True
      satisfacible (Conj (Var "p") (Neg (Var "p"))) `shouldBe` False
      satisfacible (Equiv (Var "p") (Neg (Var "p"))) `shouldBe` False

    it "insatisfacibleEn" $ do
      insatisfacibleEn ["p"] (Conj (Var "p") (Neg (Var "p"))) `shouldBe` True
      insatisfacibleEn ["p"] (Conj (Var "q") (Neg (Var "p"))) `shouldBe` True
      insatisfacibleEn ["q"] (Conj (Var "q") (Neg (Var "p"))) `shouldBe` False

    it "contrad" $ do
      contrad (Conj (Var "p") (Neg (Var "p"))) `shouldBe` True
      contrad (Disy (Var "p") (Neg (Var "p"))) `shouldBe` False
      contrad (Equiv (Var "p") (Neg (Var "p"))) `shouldBe` True
      contrad (Equiv (Var "p") (Var "p")) `shouldBe` False

    it "estadosConj" $ do
      sortMatrix (estadosConj [f1, f2, f3]) `shouldBe` (sortMatrix [[],["q"],["p"],["p","q"],["s"],["s","q"],["s","p"],["s","p","q"]])
      sortMatrix (estadosConj [Var "p", Var "q", Var "r"]) `shouldBe` sortMatrix (subconj ["p","q","r"])
      estadosConj ([] :: [Prop]) `shouldBe` ([[]] :: [Estado])

    it "satisfacibleEnConj" $ do
      satisfacibleEnConj [] [f1, f2, f3] `shouldBe` False
      satisfacibleEnConj ["s","q"] [f1, f3] `shouldBe` True

    it "consecuencia" $ do
      consecuencia [Impl (Conj (Var "t") (Var "k")) (Var "b"), Impl (Neg (Var "t")) (Var "f"), Conj (Neg (Var "f")) (Var "k")] (Var "b") `shouldBe` True
      consecuencia [Var "p", Var "q"] (Var "r") `shouldBe` False

  describe "Equivalencias" $ do

    it "equiv" $ do
      equiv (Conj (Var "a") (Neg (Var "a"))) (Equiv (Var "a") (Neg (Var "a"))) `shouldBe` True
      equiv (Conj (Var "a") (Var "a")) (Disy (Var "a") (Var "a")) `shouldBe` True
      equiv (Conj (Impl (Var "q") (Var "p")) (Impl (Var "p") (Var "q"))) (Equiv (Var "q") (Var "p")) `shouldBe` True
      equiv (Disy (Var "a") (Neg (Var "a"))) (Conj (Var "a") (Neg (Var "a"))) `shouldBe` False

    it "elimpEquiv" $ do
      elimEquiv (Conj (Disy (Neg (Var "p")) (Var "q")) (Equiv (Var "a") (Var "b"))) `shouldBe` (Conj (Disy (Neg (Var "p")) (Var "q")) (Conj (Impl (Var "a") (Var "b")) (Impl (Var "b") (Var "a"))))
      equiv (Conj (Disy (Neg (Var "p")) (Var "q")) (Equiv (Var "a") (Var "b"))) (elimEquiv (Conj (Disy (Neg (Var "p")) (Var "q")) (Equiv (Var "a") (Var "b")))) `shouldBe` True
      elimEquiv (Conj (Disy (Neg (Var "p")) (Var "q")) (Impl (Var "a") (Var "b"))) `shouldBe` (Conj (Disy (Neg (Var "p")) (Var "q")) (Impl (Var "a") (Var "b")))
      elimEquiv (Equiv (Equiv (Var "a") (Var "b")) (Equiv (Var "c") (Var "d"))) `shouldBe` (Conj (Impl (Conj (Impl (Var "a") (Var "b")) (Impl (Var "b") (Var "a"))) (Conj (Impl (Var "c") (Var "d")) (Impl (Var "d") (Var "c"))))
                                                                                                 (Impl (Conj (Impl (Var "c") (Var "d")) (Impl (Var "d") (Var "c"))) (Conj (Impl (Var "a") (Var "b")) (Impl (Var "b") (Var "a")))))

    it "elimImpl" $ do
      elimImpl (Conj (Disy (Neg (Var "p")) (Var "q")) (Equiv (Var "a") (Var "b"))) `shouldBe` (Conj (Disy (Neg (Var "p")) (Var "q")) (Equiv (Var "a") (Var "b")))
      elimImpl (Conj (Disy (Neg (Var "p")) (Var "q")) (Impl (Var "a") (Var "b"))) `shouldBe` (Conj (Disy (Neg (Var "p")) (Var "q")) (Disy (Neg (Var "a")) (Var "b")))
      elimImpl (Impl (Impl (Var "a") (Var "b")) (Impl (Var "c") (Var "d"))) `shouldBe` (Disy (Neg (Disy (Neg (Var "a")) (Var "b"))) (Disy (Neg (Var "c")) (Var "d")))
