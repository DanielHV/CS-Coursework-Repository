module Practica2 where

import LProp

-- | Decide si una fórmula proposicional es atómica. Es decir, es una variable
--   proposicional, o una constante lógica.
esAtomica :: Prop -> Bool
esAtomica (Var x) = True
esAtomica PTrue = True
esAtomica PFalse = True
esAtomica _ = False

-- | Devuelve el número de operadores lógicos que contiene una fórmula.
numOper :: Prop -> Int
numOper (Neg p) = 1 + numOper p
numOper (Disy p q) = 1 + numOper p + numOper q
numOper (Conj p q) = 1 + numOper p + numOper q
numOper (Impl p q) = 1 + numOper p + numOper q
numOper (Equiv p q) = 1 + numOper p + numOper q
numOper _ = 0

-- | Funcion auxiliar que devuelve el máximo de 2 números dados
maximo :: (Ord a) => a -> a -> a
maximo a b
    | a >= b = a
    | otherwise = b

-- | Devuelve la profundidad del árbol de sintaxis que genera una fórmula.
profundidad :: Prop -> Int
profundidad (Neg p) = 1 + profundidad p
profundidad (Disy p q) = 1 + maximo (profundidad p) (profundidad q)
profundidad (Conj p q) = 1 + maximo (profundidad p) (profundidad q)
profundidad (Impl p q) = 1 + maximo (profundidad p) (profundidad q)
profundidad (Equiv p q) = 1 + maximo (profundidad p) (profundidad q)
profundidad _ = 0

-- | Calcula el número de subfórmulas atómicas presentes en una fórmula dada.
numAtom :: Prop -> Int
numAtom (Neg p) = numAtom p
numAtom (Disy p q) = numAtom p + numAtom q
numAtom (Conj p q) = numAtom p + numAtom q
numAtom (Impl p q) = numAtom p + numAtom q
numAtom (Equiv p q) = numAtom p + numAtom q
numAtom _ = 1

-- | Elimina todas las dobles negaciones presentes en una fórmula.
--    ¬¬A = A
elimDobleNeg :: Prop -> Prop
elimDobleNeg (Neg (Neg p)) = elimDobleNeg p -- Elimina dobles negaciones
elimDobleNeg (Conj p q) = Conj (elimDobleNeg p) (elimDobleNeg q) 
elimDobleNeg (Disy p q) = Disy (elimDobleNeg p) (elimDobleNeg q) 
elimDobleNeg (Impl p q) = Impl (elimDobleNeg p) (elimDobleNeg q) 
elimDobleNeg (Equiv p q) = Equiv (elimDobleNeg p) (elimDobleNeg q) 
elimDobleNeg (Neg (PFalse)) = PTrue
elimDobleNeg (Neg (PTrue)) = PFalse
elimDobleNeg p = p -- Si no hay doble negación

-- | Aplica las leyes de De Morgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan (Neg (Conj p q)) = elimDobleNeg(Disy (Neg p)(Neg q)) -- ¬(p ∧ q) = (¬p ∨ ¬q)
deMorgan (Neg (Disy p q)) = elimDobleNeg(Conj (Neg p)(Neg q)) -- ¬(p ∨ q) = (¬p ∧ ¬q)
deMorgan p = p -- En caso de que la fórmula no sea una negación de una conjunción o disyunción

-- | Calcula la negación de una fórmula de la lógica proposicional, sin generar
--   dobles negaciones.
neg :: Prop -> Prop
neg (Var p) = Neg(Var p)
neg PTrue = PFalse
neg PFalse = PTrue
neg (Conj p q) = deMorgan (Neg (Conj p q))  -- De Morgan: ¬(p ∧ q) = (¬p ∨ ¬q)
neg (Disy p q) = deMorgan (Neg (Disy p q))-- De Morgan: ¬(p ∨ q) = (¬p ∧ ¬q)
neg (Impl p q) = Neg (Impl p q)
neg (Equiv p q) = Neg (Equiv p q)
neg (Neg p) = elimDobleNeg (Neg (Neg p)) 

-- | Calcula la forma normal negativa. Esta consiste en transformar la fórmula
--   de manera que no contenga equivalencias ni implicaciones, y que todas las
--   negaciones presentes afecten únicamente a fórmulas atómicas.
fnn :: Prop -> Prop
fnn (Impl p q) = fnn (Disy (neg (fnn p)) (fnn q))
fnn (Equiv p q) = fnn (Conj (Impl p q) (Impl q p))
fnn p = deMorgan p

-- | Funcion auxiliar para saber si una proposicion es una literal
esLiteral :: Prop -> Bool
esLiteral (Neg prop) = esAtomica prop
esLiteral prop = esAtomica prop

-- | Aplica leyes distributivas sobre formas normales conjuntivas.
--     Supuestos:
--       - Las entradas se encuentran en forma normal conjuntiva.
--       - Si ambas entradas son literales, devolvemos su disyunción.
--       - Dadas las entradas ϕ1 y ϕ2, si ϕ1=ϕ11 ∧ ϕ12 entonces se aplica
--         distributividad como (ϕ11 ∨ ϕ2) ∧ (ϕ12 ∨ ϕ2)
--       - Para ϕ2 es análogo.
distr :: Prop -> Prop -> Prop
distr phi1 phi2 
                | esLiteral phi1 && esLiteral phi2 = (Disy phi1 phi2)
distr (Conj phi11 phi12) phi2 = (Conj (Disy phi11 phi2) (Disy phi12 phi2))
distr phi1 (Conj phi21 phi22) = (Conj (Disy phi1 phi21) (Disy phi1 phi22))

-- | Calcula la forma normal conjuntiva. Esta consiste en transformar la fórmula
--   de manera que sea una conjunción de disyunciones.
fnc :: Prop -> Prop
fnc prop = fncAux (fnn prop)

-- | Funcion auxiliar para forma normal conjuntiva que recibe la formula en fnn 
fncAux :: Prop -> Prop
fncAux prop 
            | esLiteral prop = prop
fncAux (Conj phi1 phi2) = (Conj (fncAux phi1) (fncAux phi2))
fncAux (Disy phi1 phi2) = distr (fncAux phi1) (fncAux phi2)