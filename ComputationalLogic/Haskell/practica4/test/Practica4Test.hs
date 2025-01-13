{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Monad (liftM, liftM2)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck   (quickCheck, sample, Arbitrary (arbitrary)
                         , Gen, oneof, elements, sized, property, choose
                         , vectorOf, chooseInt, frequency)

import Data.List (sort, intersect)

import LPred
import Practica4

instance Arbitrary Term where
  arbitrary = do
    t <- chooseInt (0 :: Int, 5)
    n <- chooseInt (1 :: Int, 5)

    case t of
      0 -> liftM2 Fun (elements $ map (:[]) ['f'..'j']) (vectorOf n . elements $ map (Var . (:[])) ['a'..'z'])
      _ -> liftM Var (elements $ map (:[]) ['u'..'z'])

instance Arbitrary Form where
  arbitrary = sized form
    where form n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Neg subform
                                       , liftM2 Disy subform subform
                                       , liftM2 Conj subform subform
                                       , liftM2 Impl subform subform
                                       , liftM2 Equiv subform' subform'
                                       , liftM2 All name subform
                                       , liftM2 Ex name subform
                                       ]
            where
              atom = frequency [ (1, elements [FalseF,TrueF])
                               , (3, liftM2 Pred (elements $ map (:[]) ['P'..'T'])
                                      (vectorOf (min 5 (max 1 (n-1))) arbitrary))
                               , (3, liftM2 Eq arbitrary arbitrary)
                               ]
              name = elements $ map (:[]) ['u'..'z']
              subform  = form (n `div` 2)
              subform' = form (n `div` 4)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = False} specs

f,g,h,i,j :: String
f = "f"
g = "g"
h = "h"
i = "i"
j = "j"

p,q,r,s,t :: String
p = "P"
q = "Q"
r = "R"
s = "S"
t = "T"

u,v,w,x,y,z :: String
u = "u"
v = "v"
w = "w"
x = "x"
y = "y"
z = "z"

specs :: Spec
specs = do
  describe "Práctica 4: Pruebas unitarias. " $ do

    it "fv" $ do
      (sort . fv) (All x (Ex y (Pred p [Var x, Var y]))) `shouldBe` []
      (sort . fv) (All x (Ex y (Pred p [Var z, Var y]))) `shouldBe` [z]
      (sort . fv) (Ex x (All y (Pred p [Var z, Var y]))) `shouldBe` [z]
      (sort . fv) (Impl (Pred r [Var x]) (Eq (Var x) (Fun f [Var y]))) `shouldBe` [x, y]
      (sort . fv) (Neg (Pred s [Var y, Var z])) `shouldBe` [y, z]

    it "bv" $ do
      (sort . bv) (All x (Ex y (Pred p [Var x, Var y]))) `shouldBe` [x, y]
      (sort . bv) (All x (Ex y (Pred p [Var z, Var y]))) `shouldBe` [x, y]
      (sort . bv) (Ex x (All y (Pred p [Var z, Var y]))) `shouldBe` [x, y]
      (sort . bv) (Impl (Pred r [Var x]) (Eq (Var x) (Fun f [Var y]))) `shouldBe` []
      (sort . bv) (Neg (Pred s [Var y, Var z])) `shouldBe` []

    it "alcances" $ do
      alcances (All x (Ex y (Pred p [Var x, Var y]))) `shouldBe` [(All x Void, Ex y (Pred p [Var x, Var y])), (Ex y Void, Pred p [Var x, Var y])]
      alcances (Neg (All x (Ex y (Pred p [Var x, Var y])))) `shouldBe` [(All x Void, Ex y (Pred p [Var x, Var y])), (Ex y Void, Pred p [Var x, Var y])]

    it "substTerm" $ do
      substTerm (Var x) [(x, Var y)] `shouldBe` (Var y)
      substTerm (Var x) [(y, Var x), (x, Var y)] `shouldBe` (Var y)
      substTerm (Var z) [(y, Var x), (x, Var y)] `shouldBe` (Var z)
      substTerm (Var z) [(y, Var z), (x, Var y)] `shouldBe` (Var z)
      substTerm (Fun f [Var x]) [(y, Var z), (x, Var y)] `shouldBe` (Fun f [Var y])
      substTerm (Fun y [Var y]) [(y, Var z)] `shouldBe` (Fun y [Var z])
      substTerm (Fun f [Var x, Var x]) [(x, Var z)] `shouldBe` (Fun f [Var z, Var z])
      substTerm (Var x) [(x, Fun g [Var u, Var v])] `shouldBe` Fun g [Var u, Var v]

    it "substForm" $ do
      esAlphaEq (substForm (All y (Impl (Pred q [Var y]) (Pred r [Var z, Fun f [Var y]]))) [(z, Fun f [Var x])]) (All y (Impl (Pred q [Var y]) (Pred r [Fun f [Var x], Fun f [Var y]]))) `shouldBe` True
      esAlphaEq (substForm (All y (Impl (Pred q [Var y]) (Pred r [Var z, Fun f [Var y]]))) [(y, Var u)]) (All y (Impl (Pred q [Var u]) (Pred r [Var z, Fun f [Var u]]))) `shouldBe` True

    it "esAlphaEq" $ do
      esAlphaEq (Conj (Eq (Var x) (Var y)) (All x (Ex y (Pred s [Var y, Var x])))) (Conj (Eq (Var x) (Var y)) (All u (Ex v (Pred s [Var v, Var u])))) `shouldBe` True
      esAlphaEq (Conj (Eq (Var x) (Var y)) (All x (Ex y (Pred s [Var y, Var x])))) (Conj (Eq (Var u) (Var v)) (All u (Ex v (Pred s [Var v, Var u])))) `shouldBe` False
      esAlphaEq (Conj TrueF FalseF) (Conj TrueF FalseF) `shouldBe` True
      esAlphaEq (Conj TrueF FalseF) (Conj FalseF TrueF) `shouldBe` False
      esAlphaEq (All x (Conj (Pred t [Var x]) (Pred t [Var x]))) (Conj (Pred t [Var y]) (Pred t [Var z])) `shouldBe` False

-- Pruebas unitarias del punto extra.
    it "apSubstForm" $ do
    apSubstForm (All x (Conj (Pred q [Var z, Var y, Var x]) (Ex z (Pred t [fun f [Var z], Var w, Var y])))) [(x, Var a), (y, Var z), (z, [fun g [Var w]])] `shouldBe` (All u (Conj (Pred q [[fun g [Var w]], Var z, Var u]) (Ex v (Pred t [[fun f[Var v]], Var w, Var z]))))
    apSubstForm (All x (Conj (Pred q [Var z, Var y, Var x]) (Ex z (Pred t [fun f [Var z], Var w, Var y])))) [(z, [fun g [Var x]]), (w, [fun h [Var z]]), (y, Var w)] `shouldBe` (All u (Conj (Pred q [[fun g [Var x]], Var w, Var u]) (Ex v (Pred t [[fun f[Var v]], [fun h [Var z]], Var w]))))
    apSubstForm (All x (Impl (Pred q [Var x] (Pred r [Var z, Var x])))) [(z, [fun f [Var x]])] `shouldBe` (All y (Impl (Pred q [Var y] (Pred r [[fun f [Var x]], Var y]))))


  describe "Práctica 4: Pruebas basadas en propiedades. " $ do

    it "alcances: La primera entrada de cada tupla debe tener un cuantificador y la fórmula vacía." $
      property prop_alcances

    it "alcances: Las variables ligadas deben tener algún alcance y cuantificador." $
      property prop_alcances_vl

    it "alphaEq: Una alpha-equivalencia debe reconocerse como alpha-equivalencia de la fórmula original." $
      property prop_alpha_equiv

    it "alphaEq: El número de variables libres y ligadas no cambia con la alpha-equivalencia." $
      property prop_alpha_num_vars

    it "alphaEq: Una alpha-equivalencia genera variables ligadas y libres disjuntas." $
      property prop_vars_disj

    it "cerrUniv: El número de variables ligadas crece y el número de variables libres es 0." $
      property prop_cerr_univ

    it "cerrUniv: Las variables libres se vuelven ligadas con el cuantificador universal." $
      property prop_cerr_univ_q

    it "cerrUniv: El número de cuantificadores universales crece y de existenciales se mantiene igual." $
      property prop_cerr_univ_nq

    it "cerrExis: El número de variables ligadas crece y el número de variables libres es 0." $
      property prop_cerr_exis

    it "cerrExis: Las variables libres se vuelven ligadas con el cuantificador existencial." $
      property prop_cerr_exis_q

    it "cerrExis: El número de cuantificadores existenciales crece y de universales se mantiene igual." $
      property prop_cerr_exis_nq

    it "apSubstForm: El número de variables ligadas se mantiene igual." $
      property prop_app_subst

prop_alcances :: Form -> Bool
prop_alcances f = foldr (\s a -> isValid s && a) True (alcances f)
  where isValid ((Ex _ Void), _)  = True
        isValid ((All _ Void), _) = True
        isValid _                 = False

prop_alcances_vl :: Form -> Bool
prop_alcances_vl f = sort (bv f) == sort (map vars (alcances f))
  where vars (Ex n _, _)  = n
        vars (All n _, _) = n
        vars _ = ""

prop_alpha_equiv :: Form -> Bool
prop_alpha_equiv f = esAlphaEq (alphaEq f) f && esAlphaEq f (alphaEq f)

prop_vars_disj :: Form -> Bool
prop_vars_disj f = let g = alphaEq f in (null . intersect (fv g)) (bv g)

prop_alpha_num_vars :: Form -> Bool
prop_alpha_num_vars f = let g = alphaEq f
                        in (length (fv f), length (bv f)) == (length (fv g), length (bv g))

prop_cerr_univ :: Form -> Bool
prop_cerr_univ f = let g = cerrUniv f in length (bv f) <= length (bv g) && length (fv g) == 0

prop_cerr_univ_q :: Form -> Bool
prop_cerr_univ_q f = all (`elem` c) (fv f)
  where a = alcances (cerrUniv f)
        b = filter esFA a
        c = map (\(All n _, _) -> n) b
        esFA (All _ _, _) = True
        esFA _            = False

prop_cerr_univ_nq :: Form -> Bool
prop_cerr_univ_nq f = let g = cerrUniv f in numForAll f <= numForAll g && numEx f == numEx g

prop_cerr_exis :: Form -> Bool
prop_cerr_exis f = let g = cerrExis f in length (bv f) <= length (bv g) && length (fv g) == 0

prop_cerr_exis_q :: Form -> Bool
prop_cerr_exis_q f = all (`elem` c) (fv f)
  where a = alcances (cerrExis f)
        b = filter esEx a
        c = map (\(Ex n _, _) -> n) b
        esEx (Ex _ _, _) = True
        esEx _           = False

prop_cerr_exis_nq :: Form -> Bool
prop_cerr_exis_nq f = let g = cerrExis f in numEx f <= numEx g && numForAll f == numForAll g

prop_app_subst :: Form -> Subst -> Bool
prop_app_subst f s = let g = apSubstForm f s
                     in length (bv f) == length (bv g)


numForAll :: Form -> Int
numForAll (Neg f)     = numForAll f
numForAll (Conj f g)  = numForAll f + numForAll g
numForAll (Disy f g)  = numForAll f + numForAll g
numForAll (Impl f g)  = numForAll f + numForAll g
numForAll (Equiv f g) = numForAll f + numForAll g
numForAll (All _ f)   = 1 + numForAll f
numForAll (Ex _ f)    = numForAll f
numForAll _           = 0


numEx :: Form -> Int
numEx (Neg f)     = numEx f
numEx (Conj f g)  = numEx f + numEx g
numEx (Disy f g)  = numEx f + numEx g
numEx (Impl f g)  = numEx f + numEx g
numEx (Equiv f g) = numEx f + numEx g
numEx (All _ f)   = numEx f
numEx (Ex _ f)    = 1 + numEx f
numEx _           = 0
