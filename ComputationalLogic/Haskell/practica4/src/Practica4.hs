module Practica4 where

import LPred
import Auxiliares


-- | Devuelve la lista de variables libres.
fv :: Form -> [Nombre]
fv Void = []
fv TrueF = []
fv FalseF = []
fv (Pred _ []) = []
fv (Pred _ ts) = eliminar_repetidos (concatMap variables_de_funcion ts)
fv (Eq t1 t2) = eliminar_repetidos (variables_de_funcion t1 ++ variables_de_funcion t2)
fv (Neg f) = eliminar_repetidos (fv f)
fv (Conj f1 f2) = eliminar_repetidos (fv f1 ++ fv f2)
fv (Disy f1 f2) = eliminar_repetidos (fv f1 ++ fv f2)
fv (Impl f1 f2) = eliminar_repetidos (fv f1 ++ fv f2)
fv (Equiv f1 f2) = eliminar_repetidos (fv f1 ++ fv f2)
fv (All x f) = eliminar_repetidos (eliminar_ligadas x (fv f))
fv (Ex x f) = eliminar_repetidos (eliminar_ligadas x (fv f))

-- | Devuelve la lista de variables ligadas.
bv :: Form -> [Nombre]
bv Void = []
bv TrueF = []
bv FalseF = []
bv (Pred _ []) = []
bv (Pred _ ts) = []
bv (Eq t1 t2) = []
bv (Neg f) = bv f
bv (Conj f1 f2) = bv f1 ++ bv f2
bv (Disy f1 f2) = bv f1 ++ bv f2
bv (Impl f1 f2) = bv f1 ++ bv f2
bv (Equiv f1 f2) = bv f1 ++ bv f2
bv (All x f) = [x] ++ bv f
bv (Ex x f) = [x] ++ bv f

-- | Devuelve una lista de tuplas donde la primera entrada sea un
--   cuantificador, y la segunda el alcance de este.
--   Ejemplo:
--     > alcances (All "x" (Ex "z" (Eq (Var "x") (Var "y"))))
--     [ (All "x" Void, Ex "z" (Eq (Var "x") (Var "y")))
--     , (Ex "z" Void, Eq (Var "x") (Var "y"))
--     ]
alcances :: Form -> [(Form, Form)]
alcances Void = []
alcances TrueF = []
alcances FalseF = []
alcances (Pred _ _) = []
alcances (Eq _ _) = []
alcances (Neg f) = alcances f
alcances (Conj f1 f2) = alcances f1 ++ alcances f2
alcances (Disy f1 f2) = alcances f1 ++ alcances f2
alcances (Impl f1 f2) = alcances f1 ++ alcances f2
alcances (Equiv f1 f2) = alcances f1 ++ alcances f2
alcances (All x f) = [(All x Void, f)] ++ alcances f
alcances (Ex x f) = [(Ex x Void, f)] ++ alcances f

-- | Devuelve la cerradura universal de una fórmula. Es decir,
--   liga todas las variables libres, usando el cuantificador
--   universal.
cerrUniv :: Form -> Form
cerrUniv f = foldr (\x acc -> All x acc) f (fv f) 

-- | Devuelve la cerradura existencial de una fórmula. Es decir,
--   liga todas las variables libres, usando el cuantificador
--   existencial.
cerrExis :: Form -> Form
cerrExis f = foldr (\x acc -> Ex x acc) f (fv f) 

-- | Devuelve el término que resulta de aplicar la sustitución dada.
substTerm :: Term -> Subst -> Term
substTerm (Var n) s = case lookup n s of
    Just t -> t
    Nothing -> Var n
substTerm (Fun n ts) s = Fun n (map (\t -> substTerm t s) ts) 

-- | Funcion auxiliar para determinar si dos formulas son equivalentes.
sonEquiv :: Form -> Form -> Bool
sonEquiv Void Void = True
sonEquiv TrueF TrueF = True
sonEquiv FalseF FalseF = True
sonEquiv (Pred _ _) (Pred _ _) = True
sonEquiv (Eq _ _) (Eq _ _)= True
sonEquiv (Neg phi1) (Neg phi2) = sonEquiv phi1 phi2
sonEquiv (Conj phi11 phi12) (Conj phi21 phi22) = (sonEquiv phi11 phi21) && (sonEquiv phi12 phi22)
sonEquiv (Disy phi11 phi12) (Disy phi21 phi22) = (sonEquiv phi11 phi21) && (sonEquiv phi12 phi22)
sonEquiv (Impl phi11 phi12) (Impl phi21 phi22) = (sonEquiv phi11 phi21) && (sonEquiv phi12 phi22)
sonEquiv (Equiv phi11 phi12) (Equiv phi21 phi22) = (sonEquiv phi11 phi21) && (sonEquiv phi12 phi22)
sonEquiv (All _ phi1) (All _ phi2) = sonEquiv phi1 phi2
sonEquiv (Ex _ phi1) (Ex _ phi2) = sonEquiv phi1 phi2
sonEquiv phi1 phi2 = False;

-- | Decide si dos fórmulas dadas son alpha-equivalentes.
esAlphaEq :: Form -> Form -> Bool
esAlphaEq phi1 phi2 = (fv phi1 == fv phi2) 
                    && (not(bv phi1 == bv phi2) || (bv phi1 == [] && (bv phi1 == bv phi2)))  
                    && (sonEquiv phi1 phi2)

-- | Devuelve una fórmula alpha-equivalente a la original, tal que
--   los conjuntos de variables ligadas y libres sean disjuntos.
alphaEq :: Form -> Form
alphaEq = undefined

-- | Funcion auxiliar para sustituir en la hoja del arbol (Predicado)
substInPred :: Form -> Subst -> Form 
substInPred = undefined

-- | Devuelve la fórmula que resulta de aplicar la sustitución dada, sin
--   renombramientos. Este es el primer paso para la función `apSubstForm`
--   que aplica correctamente las reglas de sustitución, evitando la captura
--   de variables libres o la sustitución de variables ligadas.
substForm :: Form -> Subst -> Form
substForm Void subst = Void
substForm TrueF subst = TrueF
substForm FalseF subst = FalseF
substForm (Pred p term) subst = (substInPred (Pred p term) subst)
substForm (Neg t) subst = (Neg (substForm t subst))
substForm (Conj t1 t2) subst = (Conj (substForm t1 subst) (substForm t2 subst))
substForm (Disy t1 t2) subst = (Disy (substForm t1 subst) (substForm t2 subst))
substForm (Impl t1 t2) subst = (Impl (substForm t1 subst) (substForm t2 subst))
substForm (Equiv t1 t2) subst = (Equiv (substForm t1 subst) (substForm t2 subst))
substForm (All x t) subst = (All x (substForm t subst)) 
substForm (Ex x t) subst = (Ex x (substForm t subst))

-- | Aplica la sustitución dada en una fórmula, de manera que
--   el resultado sea una fórmula donde cada cuantificador
--   tiene distintos nombres de variables ligadas asociadas.
--   Cuida que no suceda la captura de variables libres.
apSubstForm :: Form -> Subst -> Form
apSubstForm phi subst = substForm (alphaEq phi) subst

-- | Punto Extra:
--     Desarrolla pruebas unitarias para la función apSubstForm. Al menos 3 casos
--     de prueba no triviales (no únicamente sobre constantes, o puras fórmulas
--     sin cuantificadores).
