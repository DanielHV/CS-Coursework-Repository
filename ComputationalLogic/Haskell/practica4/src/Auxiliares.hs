module Auxiliares where

import LPred

-- Utiliza este archivo para copiar todas las definiciones de funciones que has
-- desarrollado en tus prácticas pasadas, y que desees utilizar en la práctica.

-- elimina elementos repetidos de una lista
eliminar_repetidos :: Eq a => [a] -> [a]
eliminar_repetidos [] = []
eliminar_repetidos (x:xs) = x : eliminar_repetidos (filter (/= x) xs)

-- devuelve una lista con todos las variables de una funcion, si se aplica a un termino se devuelve una lista con dicho termino
variables_de_funcion :: Term -> [Nombre]
variables_de_funcion (Var x) = [x]
variables_de_funcion (Fun _ ts) = eliminar_repetidos (concatMap variables_de_funcion ts)

-- dada una variable v y una lista de variables, elimina de la lista todas las que sean iguales a v
eliminar_ligadas :: Nombre -> [Nombre] -> [Nombre]
eliminar_ligadas x vs = filter (/= x) vs
