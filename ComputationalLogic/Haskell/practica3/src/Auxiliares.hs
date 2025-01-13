module Auxiliares where

import LProp

-- Utiliza este archivo para copiar todas las definiciones de funciones que has
-- desarrollado en tus prácticas pasadas, y que desees utilizar en la práctica.

-- auxiliares para unit

-- dada una clau devuelve si es unitaria o no 
revisar :: Clausula -> Bool
revisar [] = False
revisar (x : xs) = if (xs == []) then True 
                        else False
--quitar las clau unitaria de una formula
quitar :: Formula -> Formula
quitar [] = []
quitar (x : xs) = if revisar x then quitar xs
                    else [x] ++ quitar xs 


-- auxiliares para elim

checar :: Modelo -> Clausula -> Bool
checar []  c = False
checar (x : xs)  c = if rev_clau c x then True
                        else checar xs c

rev_clau :: Clausula -> Literal -> Bool
rev_clau [] l = False
rev_clau (x : xs) l = if l == x then True
                            else rev_clau xs l 


-- auxiliares para red

esta_en_formula :: Literal -> Formula -> Bool
esta_en_formula l [] = False
esta_en_formula l (x:xs)
                | esta_en_clausula l x = True
                | otherwise = esta_en_formula l xs

esta_en_clausula :: Literal -> Clausula -> Bool
esta_en_clausula l [] = False
esta_en_clausula l (x:xs)
                | l == x = True
                | otherwise = esta_en_clausula l xs


quitar_clausula :: Clausula -> Formula -> Formula
quitar_clausula c [] = []
quitar_clausula c (x:xs)
                | c == x = quitar_clausula c xs
                | otherwise = [x] ++ quitar_clausula c xs 

quitar_literal_de_clausula :: Literal -> Clausula -> Clausula
quitar_literal_de_clausula l [] = []
quitar_literal_de_clausula l (x:xs)
                | l == x = quitar_literal_de_clausula l xs
                | otherwise = [x] ++ quitar_literal_de_clausula l xs


quitar_literal_de_formula :: Literal -> Formula -> Formula
quitar_literal_de_formula l [] = []
quitar_literal_de_formula l (c:cs)
                        | l `esta_en_clausula` c = [quitar_literal_de_clausula l c] ++ quitar_literal_de_formula l cs
                        | otherwise = [c] ++ quitar_literal_de_formula l cs


