module Equivalencias where

import LProp
import Semantica

-- | Dadas 2 fórmulas de la lógica proposicional, decide si son equivalentes
--   entre sí; es decir, si su evaluación siempre es la misma dado un estado.
equiv :: Prop -> Prop -> Bool
equiv phi psi = tautologia (Equiv phi psi) 

-- | Dada una fórmula de la lógica proposicional, elimina las equivalencias
--   presentes, usando la equivalencia: a <-> b = (a -> b) ^ (b -> a)
elimEquiv :: Prop -> Prop
elimEquiv (Equiv a b) = Conj (elimEquiv (Impl a b)) (elimEquiv (Impl b a))
elimEquiv PTrue = PTrue
elimEquiv PFalse = PFalse
elimEquiv (Var x) = (Var x)
elimEquiv (Neg p) = (Neg (elimEquiv p))
elimEquiv (Disy p1 p2) = (Disy (elimEquiv p1) (elimEquiv p2))
elimEquiv (Conj p1 p2) = (Conj (elimEquiv p1) (elimEquiv p2))
elimEquiv (Impl p1 p2) = (Impl (elimEquiv p1) (elimEquiv p2))

-- | Dada una fórmula de la lógica proposicional, elimina las implicaciones
--   presentes, usando la equivalencia: a -> b = -a ^ b
elimImpl :: Prop -> Prop
elimImpl (Impl a b) = Disy (elimImpl (Neg a)) (elimImpl b)
elimImpl PTrue = PTrue
elimImpl PFalse = PFalse
elimImpl (Var x) = (Var x)
elimImpl (Neg p) = (Neg (elimImpl p))
elimImpl (Disy p1 p2) = (Disy (elimImpl p1) (elimImpl p2))
elimImpl (Conj p1 p2) = (Conj (elimImpl p1) (elimImpl p2))
elimImpl (Equiv p1 p2) = (Equiv (elimImpl p1) (elimImpl p2))
