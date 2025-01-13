module LProp ( VarName
             , Prop (..)
             , Estado
             ) where

-- | Alias de tipo para hacer explícito cuando una cadena es un nombre de
--   variable.
type VarName = String

-- | Tipo de dato para representar expresiones de la Lógica Proposicional.
data Prop = Var VarName     -- Variable
          | PTrue           -- Constante de verdad True
          | PFalse          -- Constante de verdad False
          | Neg Prop        -- Negación de una fórmula
          | Disy Prop Prop  -- a | b
          | Conj Prop Prop  -- a & b
          | Impl Prop Prop  -- a -> b
          | Equiv Prop Prop -- a <-> b
          deriving Eq

-- | Para que en la consola se impriman bonitas las expresiones de la
--   lógica proposicional, y sean fáciles de leer.
instance Show Prop where
  show PTrue = "⊤"
  show PFalse = "⊥"
  show (Var x) = show x
  show (Neg p) = "¬"++ show p
  show (Disy p1 p2) = "(" ++ show p1 ++ " ∨ " ++ show p2 ++ ")"
  show (Conj p1 p2) = "(" ++ show p1 ++ " ∧ " ++ show p2 ++ ")"
  show (Impl p1 p2) = "(" ++ show p1 ++ " → " ++ show p2 ++ ")"
  show (Equiv p1 p2) = "(" ++ show p1 ++ " ↔ " ++ show p2 ++ ")"

-- | Alias de tipo: un estado es una lista de nombres de variables.
--   Si la variable se encuentra en el estado, la interpretamos como 1, en otro
--   caso como 0.
type Estado = [VarName]
