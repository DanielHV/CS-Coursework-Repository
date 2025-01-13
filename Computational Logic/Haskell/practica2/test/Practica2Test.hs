{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Monad (liftM, liftM2)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck   (quickCheck, sample, Arbitrary (arbitrary)
                         , Gen, oneof, elements, sized, property)

import LProp
import Practica2

instance Arbitrary Prop where
  arbitrary  =  sized prop
    where prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Neg subform
                                       , liftM2 Disy subform subform
                                       , liftM2 Conj subform subform
                                       , liftM2 Impl subform subform
                                       , liftM2 Equiv subform' subform'
                                       ]
            where
              atom = oneof [liftM Var (elements $ map (:[]) ['a'..'z']), elements [PFalse,PTrue]]
              subform  =  prop (n `div` 2)
              subform' =  prop (n `div` 4)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = False} specs

prop_conDepth :: Prop -> Bool
prop_conDepth p = numOper p < 2^(profundidad p)

prop_depthCon :: Prop -> Bool
prop_depthCon p = profundidad p <= numOper p

prop_atomCon :: Prop -> Bool
prop_atomCon p = numAtom p <= 2*(numOper p) + 1

specs :: Spec
specs = do
  describe "Práctica 2" $ do

    let f1 = Equiv (Conj (Conj (Disy PFalse (Var "l")) (Impl (Var "f") (Var "g"))) (Disy (Neg PFalse) (Conj (Var "a") (Var "x")))) (Disy (Disy PTrue (Neg PTrue)) (Conj (Impl PTrue (Var "g")) (Impl PTrue (Var "i"))))

    it "esAtomica" $ do
      esAtomica (Var "p") `shouldBe` True
      esAtomica PTrue `shouldBe` True
      esAtomica PFalse `shouldBe` True
      esAtomica f1 `shouldBe` False
      esAtomica (Conj (Neg PFalse) (Equiv (Disy PTrue (Var "o")) (Disy PFalse (Var "b")))) `shouldBe` False
      esAtomica (Disy (Equiv (Impl (Var "r") (Var "w")) (Impl PTrue PFalse)) (Impl (Impl (Conj PTrue PFalse) (Impl PFalse (Var "d"))) (Equiv PFalse (Var "b")))) `shouldBe` False

    it "numOper" $ do
      numOper (Var "a") `shouldBe` 0
      numOper PTrue `shouldBe` 0
      numOper PFalse `shouldBe` 0
      numOper (Conj (Var "a") (Var "b")) `shouldBe` 1
      numOper (Neg (Var "a")) `shouldBe` 1
      numOper (Disy (Var "a") PTrue) `shouldBe` 1
      numOper f1 `shouldBe` 14

    it "profundidad" $ do
      profundidad (Var "a") `shouldBe` 0
      profundidad PTrue `shouldBe` 0
      profundidad PFalse `shouldBe` 0
      profundidad (Conj (Var "a") (Var "b")) `shouldBe` 1
      profundidad (Neg (Var "a")) `shouldBe` 1
      profundidad (Disy (Var "a") PTrue) `shouldBe` 1
      profundidad f1 `shouldBe` 4

    it "numAtom" $ do
      numAtom (Var "a") `shouldBe` 1
      numAtom PTrue `shouldBe` 1
      numAtom PFalse `shouldBe` 1
      numAtom (Conj (Var "a") (Var "b")) `shouldBe` 2
      numAtom (Neg (Var "a")) `shouldBe` 1
      numAtom (Disy (Var "a") PTrue) `shouldBe` 2
      numAtom f1 `shouldBe` 13

    it "elimDobleNeg" $ do
      elimDobleNeg (Var "a") `shouldBe` (Var "a")
      elimDobleNeg PTrue `shouldBe` PTrue
      elimDobleNeg PFalse `shouldBe` PFalse
      elimDobleNeg (Neg (Neg (Var "a"))) `shouldBe` (Var "a")
      elimDobleNeg (Conj (Var "a") (Neg (Neg (Var "a")))) `shouldBe` (Conj (Var "a") (Var "a"))

    it "deMorgan" $ do
      deMorgan (Neg (Disy (Var "p") (Var "q"))) `shouldBe` (Conj (Neg (Var "p")) (Neg (Var "q")))
      deMorgan (Neg (Conj (Var "p") (Var "q"))) `shouldBe` (Disy (Neg (Var "p")) (Neg (Var "q")))
      deMorgan (Neg (Disy (Neg (Var "p")) (Var "q"))) `shouldBe` (Conj (Var "p") (Neg (Var "q")))
      deMorgan (Neg (Conj (Neg (Var "p")) (Var "q"))) `shouldBe` (Disy (Var "p") (Neg (Var "q")))

    it "neg" $ do
      neg (Var "a") `shouldBe` (Neg (Var "a"))
      neg PTrue `shouldBe` PFalse
      neg PFalse `shouldBe` PTrue
      neg (Neg (Var "a")) `shouldBe` (Var "a")

    it "fnn" $ do
      fnn (Var "a") `shouldBe` (Var "a")
      fnn PTrue `shouldBe` PTrue
      fnn PFalse `shouldBe` PFalse
      fnn (Neg (Var "a")) `shouldBe` (Neg (Var "a"))
      fnn (Impl (Var "a") (Var "b")) `shouldBe` (Disy (Neg (Var "a")) (Var "b"))
      fnn (Neg (Disy (Var "a") (Var "b"))) `shouldBe` (Conj (Neg (Var "a")) (Neg (Var "b")))
      fnn (Neg (Disy (Neg (Var "a")) (Var "b"))) `shouldBe` (Conj (Var "a") (Neg (Var "b")))

    it "distr" $ do
      distr (Conj (Var "a") (Var "b")) PTrue `shouldBe` (Conj (Disy (Var "a") PTrue) (Disy (Var "b") PTrue))
      distr (Var "p") (Conj (Var "a") (Var "b")) `shouldBe` (Conj (Disy (Var "p") (Var "a")) (Disy (Var "p") (Var "b")))
      distr PFalse PTrue `shouldBe` (Disy PFalse PTrue)

    it "fnc" $ do
      fnc (Impl (Impl (Neg (Var "p")) (Var "q")) (Impl (Neg (Var "r")) (Var "s"))) `shouldBe` (Conj (Disy (Neg (Var "p")) (Disy (Var "r") (Var "s"))) (Disy (Neg (Var "q")) (Disy (Var "r") (Var "s"))))
      fnc (Neg (Disy (Var "b") (Var "c"))) `shouldBe` (Conj (Neg (Var "b")) (Neg (Var "c")))
      fnc (Disy (Conj (Var "a") (Var "b")) (Var "c")) `shouldBe` (Conj (Disy (Var "a") (Var "c")) (Disy (Var "b") (Var "c")))

  describe "Práctica 2: Pruebas basadas en propiedades" $ do

    it "con p < 2^depth p" $
      property prop_conDepth

    it "depth p <= con p" $
      property prop_depthCon

    it "atom p <= 2con p + 1" $
      property prop_atomCon
