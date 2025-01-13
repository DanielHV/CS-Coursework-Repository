{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Monad (liftM, liftM2)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Data.List (sort)

import LProp
import Practica3

main :: IO ()
main = hspecWith defaultConfig {configFailFast = False} specs

p,q,r,s,t :: String
p = "p"
q = "q"
r = "r"
s = "s"
t = "t"

sortConfig :: Config -> Config
sortConfig (m, f) = (sort m, sort $ map sort f)

specs :: Spec
specs = do
  describe "Práctica 3" $ do

    it "negLit" $ do
      negLit LitTrue `shouldBe` LitFalse
      negLit LitFalse `shouldBe` LitTrue
      negLit (Lit "p") `shouldBe` (LitNeg "p")
      negLit (LitNeg "p") `shouldBe` (Lit "p")

    it "unit" $ do
      (sortConfig $ unit ([Lit p, Lit r], [[Lit q, LitNeg s], [LitNeg q]])) `shouldBe` sortConfig ([Lit p, Lit r, LitNeg q], [[Lit q, LitNeg s]])
      (sortConfig $ unit ([Lit p, Lit r, LitNeg q], [[LitNeg s]])) `shouldBe` sortConfig ([Lit p, Lit r, LitNeg q, LitNeg s], [])
      (sortConfig $ unit ([], [[LitNeg p], [Lit p, Lit r]])) `shouldBe` sortConfig ([LitNeg p], [[Lit p, Lit r]])

    it "elim" $ do
      (sortConfig $ elim ([Lit p], [[Lit p, Lit r], [LitNeg p, Lit q]])) `shouldBe` sortConfig ([Lit p], [[LitNeg p, Lit q]])
      (sortConfig $ elim ([Lit p, LitNeg q], [[Lit s, Lit t], [Lit r, LitNeg q, LitNeg s], [Lit r, LitNeg p]])) `shouldBe` sortConfig ([Lit p, LitNeg q], [[Lit s, Lit t], [Lit r, LitNeg p]])
      (sortConfig $ elim ([LitNeg q, Lit r, LitNeg t], [[Lit s, Lit p, LitNeg t, Lit q]])) `shouldBe` sortConfig ([LitNeg q, Lit r, LitNeg t], [])

    it "red" $ do
      (sortConfig $ red ([Lit p, LitNeg s], [[Lit q, Lit t], [LitNeg r, Lit s, LitNeg t], [LitNeg r]])) `shouldBe` sortConfig ([Lit p, LitNeg s], [[Lit q, Lit t], [LitNeg r, LitNeg t], [LitNeg r]])
      (sortConfig $ red ([LitNeg r, Lit s, LitNeg p], [[LitNeg s]])) `shouldBe` sortConfig ([LitNeg r, Lit s, LitNeg p], [[]])
      (sortConfig $ red ([Lit p], [[Lit s, Lit r], [LitNeg p, Lit q]])) `shouldBe` sortConfig ([Lit p], [[Lit s, Lit r], [Lit q]])

    it "split" $ do
      (sort . (map sortConfig) . split) ([], [[LitNeg p, Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit p, LitNeg s],[LitNeg p, Lit q, LitNeg r, LitNeg s]]) `shouldBe` (sort . (map sortConfig)) [([Lit p], [[LitNeg p, Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit p, LitNeg s],[LitNeg p, Lit q, LitNeg r, LitNeg s]]), ([LitNeg p], [[LitNeg p, Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit p, LitNeg s],[LitNeg p, Lit q, LitNeg r, LitNeg s]])]
      (sort . (map sortConfig) . split) ([Lit p], [[Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit q, LitNeg r, LitNeg s]]) `shouldBe` (sort . (map sortConfig)) [([Lit p, Lit r], [[Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit q, LitNeg r, LitNeg s]]), ([Lit p, LitNeg r], [[Lit r, LitNeg t],[LitNeg q, LitNeg r],[Lit q, LitNeg r, LitNeg s]])]
      (sort . (map sortConfig) . split) ([Lit p], [[LitNeg p, LitNeg q]]) `shouldBe` (sort . (map sortConfig)) [([Lit p, Lit q], [[LitNeg p, LitNeg q]]), ([Lit p, LitNeg q], [[LitNeg p, LitNeg q]])]

    it "conflict" $ do
      conflict ([LitNeg r, Lit s, LitNeg p], [[]]) `shouldBe` True
      conflict ([LitNeg r, Lit p], [[Lit q], []]) `shouldBe` True
      conflict ([LitNeg r, Lit p], [[Lit q], [LitNeg r]]) `shouldBe` False

    it "success" $ do
      success ([Lit s], []) `shouldBe` True
      success ([], []) `shouldBe` True
      success ([LitNeg q, Lit r], []) `shouldBe` True
      success ([LitNeg q, Lit r], [[Lit p], []]) `shouldBe` False

    it "appDPLL" $ do
      (sortConfig $ appDPLL ([Lit p], [[Lit s, Lit r], [LitNeg p, Lit q]])) `shouldBe` sortConfig ([Lit p], [[Lit s, Lit r], [Lit q]])
      (sortConfig $ appDPLL ([Lit p, Lit r, LitNeg q], [[LitNeg s]])) `shouldBe` sortConfig ([Lit p, Lit r, LitNeg q, LitNeg s], [])
      (sortConfig $ appDPLL ([Lit p, Lit r], [[Lit r, LitNeg t], [LitNeg q, LitNeg r], [Lit q, LitNeg r, LitNeg s]])) `shouldBe` sortConfig ([Lit p, Lit r], [[LitNeg q, LitNeg r], [Lit q, LitNeg r, LitNeg s]])

    it "dpll" $ do
      (sortConfig $ dpll ([], [[Lit p, Lit q], [Lit p, LitNeg q], [Lit r, Lit q], [Lit r, LitNeg q]])) `shouldBe` sortConfig ([Lit p, Lit r], [])

  describe "Extras" $ do

    it "propFormula" $ do
      sort (map sort $ propFormula (Disy (Conj (Var p) (Var q)) (Var r))) `shouldBe` sort (map sort [[Lit p, Lit r], [Lit q, Lit r]])
      sort (map sort $ propFormula (Impl (Impl (Neg (Var "p")) (Var "q")) (Impl (Neg (Var "r")) (Var "s")))) `shouldBe` sort (map sort [[LitNeg p, Lit s, Lit r], [LitNeg q, Lit r, Lit s]])

    it "modelo" $ do
      modelo (Conj (Var p) (Neg (Var p))) `shouldBe` Nothing
      modelo (Disy (Var p) (Conj (Var q) (Neg (Var q)))) `shouldBe` Just [Lit p]
      modelo ((((Var p) `Conj` (Var q)) `Disy` (Var s)) `Equiv` ((Neg (Var p)) `Conj` (Var q))) `shouldSatisfy` modeloCheck

modeloCheck :: Maybe Modelo -> Bool
modeloCheck Nothing = False
modeloCheck (Just xs) = l == sort [LitNeg q, LitNeg p, LitNeg s] || l == sort [Lit p, LitNeg q, LitNeg s] || l == sort [Lit q, Lit s, LitNeg p]
  where l = sort xs
