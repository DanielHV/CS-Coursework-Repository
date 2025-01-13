module Semantica where

import LProp

-- | Dada una lista, regresa las 2^n posibles sublistas, donde `n` es la
--   longitud de la lista original.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = map (x:) (subconj xs) ++ subconj xs

-- | Dada una lista de elementos que se pueden comparar bajo igualdad, regresa
--   una lista con los mismos elementos, pero sin repeticiones presentes.
eliminaRepetidos :: Eq a => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs)
    | x `elem` xs = eliminaRepetidos xs
    | otherwise = x:eliminaRepetidos xs

-- | Dado un estado y una fórmula proposicional, decide si la fórmula se
--   satisface o no.
interp :: Estado -> Prop -> Bool
interp sigma PTrue = True
interp sigma PFalse = False
interp sigma (Var p) = p `elem` sigma
interp sigma (Neg p) = not (interp sigma p)
interp sigma (Disy p q) = interp sigma p || interp sigma q
interp sigma (Conj p q) = interp sigma p && interp sigma q
interp sigma (Impl p q)
  | interp sigma p && interp sigma q = True
  | not(interp sigma p) && interp sigma q = True
  | not(interp sigma p) && not(interp sigma q) = True
  | interp sigma p && not(interp sigma q) = False
interp sigma (Equiv p q)
  | interp sigma p && interp sigma q = True
  | not(interp sigma p) && interp sigma q = False
  | not(interp sigma p) && not(interp sigma q) = True
  | interp sigma p && not(interp sigma q) = False


-- | Dada una proposición, devuelve los nombres de las variables presentes.
vars :: Prop -> [VarName]
vars PTrue = []
vars PFalse = []
vars (Var p) = [p]
vars (Neg p) = eliminaRepetidos(vars p)
vars (Disy p q) = eliminaRepetidos(vars p ++ vars q)
vars (Conj p q) = eliminaRepetidos(vars p ++ vars q)
vars (Impl p q) = eliminaRepetidos(vars p ++ vars q)
vars (Equiv p q) = eliminaRepetidos(vars p ++ vars q)

{-- Por completar. --}

-- | Dada una proposición, devuelve todos los posibles estados de sus variables.
estados :: Prop -> [Estado]
estados phi = subconj (vars phi)

-- | Dada una proposición, devuelve todos los estados de sus variables que son
--   modelos de la proposición; es decir, la satisfacen.
modelos :: Prop -> [Estado]
modelos phi = generaModelos (estados phi) phi

-- | Funcion auxiliar para modelos que recibe una lista de estados y las agrega
-- a la lista nueva si su interpretacion es True
generaModelos :: [Estado] -> Prop -> [Estado]
generaModelos [] phi = []
generaModelos (x : xs) phi
              | satisfacibleEn x phi = [x] ++ generaModelos xs phi
              | otherwise = generaModelos xs phi

-- | Dada una proposición, decide si la fórmula es una tautología.
tautologia :: Prop -> Bool
tautologia phi = estados phi == modelos phi

-- | Dado un estado y una proposición, decide si esta se satisface.
satisfacibleEn :: Estado -> Prop -> Bool
satisfacibleEn i phi = interp i phi == True

-- | Dada una proposición, decide si es satisfacible bajo algún estado de sus
--   variables.
satisfacible :: Prop -> Bool
satisfacible phi = modelos phi /= []

-- | Dado un estado y una proposición, decide si esta no se satisface.
insatisfacibleEn :: Estado -> Prop -> Bool
insatisfacibleEn i phi = interp i phi == False 

-- | Dada una proposición, decide si la fórmula es una contradicción.
contrad :: Prop -> Bool
contrad phi = modelos phi == []   

-- | Funcion que regresa la conjuncion de todas las formulas de un conjunto gama 
conjuncionConj :: [Prop] -> Prop 
conjuncionConj [] = PTrue
conjuncionConj (x:xs) = Conj x (conjuncionConj xs)

-- | Dada una lista de proposiciones, devuelve todos los posibles estados de las
--   variables presentes en las fórmulas.
estadosConj :: [Prop] -> [Estado]
estadosConj gama = subconj (vars (conjuncionConj gama))

-- | Dado un estado y una lista de proposiciones, decide si se satisfacen todas
--   con el estado.
satisfacibleEnConj :: Estado -> [Prop] -> Bool
satisfacibleEnConj e [] = True
satisfacibleEnConj e (x:xs) = if satisfacibleEn e x then satisfacibleEnConj e xs
                                else False

-- | Dada una lista de proposiciones gama, devuelve todos los estados de las variables que son
--   modelos de gama; es decir, la satisfacen.
modelosConj :: [Prop] -> [Estado]
modelosConj gama = generaModelosConj (estadosConj gama) gama

-- | Funcion auxiliar para modelosConj que recibe una lista de estados y los agrega
-- a la lista nueva si satisfacen a gama
generaModelosConj :: [Estado] -> [Prop] -> [Estado]
generaModelosConj [] gama = []
generaModelosConj (x : xs) gama
              | satisfacibleEnConj x gama = [x] ++ generaModelosConj xs gama
              | otherwise = generaModelosConj xs gama

-- | Dada una lista de proposiciones como premisas y una fórmula como conclusión,
--   decide si se sigue la conclusión de las premisas.
consecuencia :: [Prop] -> Prop -> Bool
consecuencia gama phi = consecuenciaAux (modelosConj gama) phi

-- | Funcion auxiliar para consecuencia que recibe la lista de modelos de un 
-- conjunto gama y una formula phi para determinar si cada modelo de gama
-- es modelo de phi
consecuenciaAux :: [Estado] -> Prop -> Bool
consecuenciaAux [] phi = True;
consecuenciaAux (x : xs) phi = satisfacibleEn x phi && consecuenciaAux xs phi