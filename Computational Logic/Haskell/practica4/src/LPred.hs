module LPred ( Nombre
             , Subst
             , Term (..)
             , Form (..)
             ) where

import Data.List (intercalate)

-- | Alias de tipo para hacer explícito cuando una cadena es un nombre de
--   variable.
type Nombre = String

-- | Tipo de dato que representa un término de la lógica de predicados.
data Term = Var Nombre        -- Variable
          | Fun Nombre [Term] -- Función
          deriving Eq

-- | Alias de tipo para sustituciones simultáneas de la forma [x1 := m1, ..., xn := mn].
type Subst = [(Nombre, Term)]

-- | Tipo de dato que representa una fórmula en la lógica de predicados.
data Form = Void               -- Fórmula vacía
          | TrueF              -- Constante de verdad
          | FalseF             -- Constante de falso
          | Pred Nombre [Term] -- Predicado
          | Eq Term Term       -- Igualdad de términos
          | Neg Form           -- Negación
          | Conj Form Form     -- Conjunción
          | Disy Form Form     -- Disyunción
          | Impl Form Form     -- Implicación
          | Equiv Form Form    -- Equivalencia
          | All Nombre Form    -- Para todo
          | Ex Nombre Form     -- Existe
          deriving Eq

instance Show Term where
  show (Var n) = n
  show (Fun n ts) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

-- | Para que en la consola se impriman bonitas las expresiones de la
--   lógica de predicados, y sean fáciles de leer.
instance Show Form where
  show Void          = "()"
  show TrueF         = "⊤"
  show FalseF        = "⊥"
  show (Pred n ts)   = n ++ "(" ++ intercalate "," (map show ts) ++ ")"
  show (Eq p1 p2)    = "(" ++ show p1 ++ " = " ++ show p2 ++ ")"
  show (Neg f)       = "¬"++ show f
  show (Conj p1 p2)  = "(" ++ show p1 ++ " ∧ " ++ show p2 ++ ")"
  show (Disy p1 p2)  = "(" ++ show p1 ++ " ∨ " ++ show p2 ++ ")"
  show (Impl p1 p2)  = "(" ++ show p1 ++ " → " ++ show p2 ++ ")"
  show (Equiv p1 p2) = "(" ++ show p1 ++ " ↔ " ++ show p2 ++ ")"
  show (All n p)     = "∀" ++ n ++ show p
  show (Ex n p)      = "∃" ++ n ++ show p
