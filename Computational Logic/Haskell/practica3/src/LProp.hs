module LProp ( VarName
             , Prop (..)
             , Literal (..)
             , Clausula
             , Formula
             , Modelo
             , Config
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

-- | Tipo para definir una literal.
--   Recuerda que una literal es una constante (verdadero, falso), una variable
--   proposicional o su negación.
data Literal = Lit VarName
             | LitNeg VarName
             | LitTrue
             | LitFalse
             deriving (Eq, Ord)

-- | Una cláusula es una disyunción de literales: en vez de usar el constructor
--   `Disy` repetidamente, usamos una lista de literales.
type Clausula = [Literal]
-- | Una fórmula es una conjunción de cláusulas: en vez de usar el constructor
--   `Conj` repetidamente, usamos una lista de cláusulas.
type Formula = [Clausula]

-- | Un modelo es una asignación de valores para literales. Si la literal está
--   presente, asumimos que es verdadera.
type Modelo = [Literal]

-- | Una configuración es un modelo y una fórmula.
--   Algo de la forma M |= F lo representamos como (M, F).
type Config = (Modelo, Formula)

-- | Para que en la consola se impriman bonitas las expresiones de la
--   lógica proposicional, y sean fáciles de leer.
instance Show Prop where
  show PTrue = "⊤"
  show PFalse = "⊥"
  show (Var x) = x
  show (Neg p) = "¬"++ show p
  show (Disy p1 p2) = "(" ++ show p1 ++ " ∨ " ++ show p2 ++ ")"
  show (Conj p1 p2) = "(" ++ show p1 ++ " ∧ " ++ show p2 ++ ")"
  show (Impl p1 p2) = "(" ++ show p1 ++ " → " ++ show p2 ++ ")"
  show (Equiv p1 p2) = "(" ++ show p1 ++ " ↔ " ++ show p2 ++ ")"

-- | Para que en la consola se impriman bonitas las literales.
instance Show Literal where
  show LitTrue  = "⊤"
  show LitFalse = "⊥"
  show (Lit x)  = x
  show (LitNeg p) = "¬" ++ p
