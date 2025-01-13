module Practica3 where

import LProp
import Auxiliares

-- | Calcula la negación de una literal.
negLit :: Literal -> Literal
negLit (Lit x) = (LitNeg x)
negLit (LitNeg x) = (Lit x)
negLit LitTrue = LitFalse
negLit LitFalse = LitTrue

-- | Aplica la regla de cláusula unitaria. En caso de no haber tal cláusula,
--   devuelve la configuración sin modificarla.
unit :: Config -> Config
unit (m , f) = (m ++ clausula_unitaria f, quitar f)

-- auxiliar para unit
--devuelve la clau en forma de listas 
clausula_unitaria :: Formula -> Modelo
clausula_unitaria [] = []
clausula_unitaria (x : xs) = if revisar x then x ++ clausula_unitaria xs 
                                else clausula_unitaria xs

-- | Aplica la regla de eliminación:
--     M, F |= F, l v C |> M, F |= F
elim :: Config -> Config
elim (m , f) = (m , regla_de_eliminacion(m , f))

-- auxiliar para elim
regla_de_eliminacion :: Config -> Formula
regla_de_eliminacion (m , []) = []
regla_de_eliminacion (m , (x : xs)) = if checar m x then regla_de_eliminacion (m , xs)
                                        else [x] ++ regla_de_eliminacion (m , xs)

-- | Aplica la regla de reducción:
--     M, l |= F, l^c v C |> M, l |= F, C
red :: Config -> Config
red (m , f) = (m , regla_de_reduccion m f)
                    
-- auxiliar para red
regla_de_reduccion :: Modelo -> Formula -> Formula
regla_de_reduccion _ [] = []
regla_de_reduccion [] f = f
regla_de_reduccion (l:ls) f = regla_de_reduccion ls (quitar_literal_de_formula (negLit l) f)

-- | Aplica la regla de split, devolviendo los dos posibles casos. En caso de
--   no existir una literal con la cuál hacer split (porque ya están todas en
--   el modelo), devuelve la misma configuración.
--     M |= F |> [M, l |= F ; M, l^c |= F]
split :: Config -> [Config]
split (m, f) = case literal_no_en_modelo m f of
    Nothing -> [(m, f)]
    Just l -> [(l:m, f), (negLit l:m, f)]

--auxiliar para split
-- devuelve la primera literal que no exista en el modelo dado
literal_no_en_modelo :: Modelo -> Formula -> Maybe Literal
literal_no_en_modelo _ [] = Nothing
literal_no_en_modelo m (c:cs) = case clausula_no_en_modelo m c of
    Nothing -> literal_no_en_modelo m cs
    Just l -> Just l

-- auxiliar para split
-- busca en una clausula si contiene una literal que no exista en el modelo dado y la devuelve
clausula_no_en_modelo :: Modelo -> Clausula -> Maybe Literal
clausula_no_en_modelo _ [] = Nothing
clausula_no_en_modelo m (l:ls)
                    | not (m `contiene_a` l || m `contiene_a` negLit l) = Just l
                    | otherwise = clausula_no_en_modelo m ls

-- auxiliar para split
-- comprueba si una literal existe en una lista de literales
contiene_a :: [Literal] -> Literal -> Bool
contiene_a [] _ = False
contiene_a (l:ls) p = if p == l then True else contiene_a ls p


-- | Verifica si hay un conflicto en la configuración.
conflict :: Config -> Bool
conflict (_, f) = any null f

-- | Verifica si llegamos a éxito en la configuración.
success :: Config -> Bool
success (_, f) = null f

-- | Aplica una de las reglas del algoritmo: unit, elim o red; según sea
--   conveniente para la configuración recibida.
appDPLL :: Config -> Config
appDPLL config =
    case unit config of
        config' | config' /= config -> appDPLL config'
        _ -> case elim config of
                config' | config' /= config -> appDPLL config'
                _ -> case red config of
                        config' | config' /= config -> appDPLL config'
                        _ -> config

-- | Ejecuta el algoritmo completo de DPLL.
dpll :: Config -> Config
dpll (m, f) =
    case unit (m, f)  of
        config' | config' /= (m, f)  -> dpll config'
        _ -> case elim (m, f)  of
            config' | config' /= (m, f)  -> dpll config'
            _ -> case red (m, f)  of
                config' | config' /= (m, f)  -> dpll config'
                _ -> if conflict (m, f) 
                    then (m, f)
                    else if success (m, f) 
                        then (m, f)
                        else
                            case split (m, f)  of
                                (m, f) : _ -> dpll (m, f)
                                [] -> (m, f)

{-- Extras --}

-- | Devuelve el modelo que satisface una proposición, o Nothing en caso
--   de no ser satisfacible.
modelo :: Prop -> Maybe Modelo
modelo = undefined

-- | Convierte una Proposición a una Fórmula en forma normal clausular.
propFormula :: Prop -> Formula
propFormula prop = case prop of
    Var x -> [[Lit x]]
    PTrue -> [[]]
    PFalse -> []
    Neg p -> propFormula (Neg p) 
    Disy p1 p2 -> propFormula p1 ++ propFormula p2
    Conj p1 p2 -> [c1 ++ c2 | c1 <- propFormula p1, c2 <- propFormula p2]
    Impl p1 p2 -> propFormula (Disy (Neg p1) p2) 
    Equiv p1 p2 -> propFormula (Conj (Impl p1 p2) (Impl p2 p1))