/**
 * Ejercicio 1 - construye_ld
 * `construye_ld(L, LD)`. `LD` es la versión vista como lista diferencia de `L`.
 *  *Hint:* Puedes usar el predicado `append`.
 *
 *     ?- construye_ld([], LD).
 *     LD = _?-_?.
 *
 *     ?- construye_ld([1,2,3], LD).
 *     LD = [1, 2, 3|_?]-_?.
 */
construye_ld([], X-X) :- !.
construye_ld([X|Xs], [X|Y]-Z) :-
    construye_ld(Xs, Y-Z).

/**
 * Ejercicio 2 - cierra_ld
 * `cierra_ld(LD, L)`: `L` es la versión como lista cerrada de una lista diferencia `LD`.
 *
 *     ?- construye_ld([], LD), cierra_ld(LD, L).
 *     LD = ..., L = [].
 *
 *     ?- construye_ld([1,2,3], LD), cierra_ld(LD, L).
 *     LD = ..., L = [1, 2, 3].
 */
cierra_ld(LD, L) :- cierra_aux(LD, L).

 cierra_aux([] - [], []) :- !.

 cierra_aux([X|Rest]-Tail, [X|L]) :-
    cierra_aux(Rest-Tail, L).

/**
 * Ejercicio 3 - append_ld
 * `append_ld(L1, L2, LD)`: Es tal que al pedir la concatenación de dos listas abiertas,
 *  el resultado de la concatenación de `L1` con `L2` está en `LD`.
 *
 *     ?- construye_ld([], L1), construye_ld([], L2), append_ld(L1, L2, LD), cierra_ld(LD, L).
 *     L1 = ...-..., L2 = ...-..., LD = ...-..., L = [].
 *
 *     ?- construye_ld([1,2,3], L1), construye_ld([4,5,6], L2), append_ld(L1, L2, LD).
 *     L1 = ...-..., L2 = ...-..., LD = [1, 2, 3, 4, 5, 6|_?]-_?.
 *
 *     ?- construye_ld([1,2,3], L1), construye_ld([4,5,6], L2), append_ld(L1, L2, LD), cierra_ld(LD, L).
 *     L1 = ...-..., L2 = ...-..., LD = ...-..., L = [1, 2, 3, 4, 5, 6].
 */
append_ld(X-Y, Y-Z, X-Z).

/**
 * Ejercicio 4 - reversa_ld
 * `reversa_ld(L1, L2)`: `L2` es la lista `L1` en reversa. Da la definición usando listas diferencia.
 *              Nota: *NO* utilices predicados auxiliares, sólo listas diferencia.
 *
 *     ?- reversa_ld([1,2,3,4], L).
 *     L = [4, 3, 2, 1].
 *
 *     ?- reversa_ld([a, b, c, d, e], L).
 *     L = [e, d, c, b, a].
 */
 reversa_ld(L1, L2) :- reversa_ld_aux(L1, []-L2).

reversa_ld_aux([], LD-LD).
reversa_ld_aux([X|Xs], LD-R) :-
    reversa_ld_aux(Xs, [X|LD]-R).

/**
 * Ejercicio 5 - rotacion
 * `rotacion(L1, N, L2)`: `L2` es la lista que resulta de rota `N` veces la lista `L1` a la izquierda.
 *  Asume que la lista `L1` es una lista diferencia, por lo que `L2` también lo será.
 *  Una rotación a la izquierda consiste en mover la cabeza de la lista al final de esta.
 *  Nota: Resuelve el ejercicio utilizando listas diferencia y recursión (no utilices `append` o `append_ld`).
 *
 *     ?- rotacion([1,2,3|X]-X,1,L).
 *     L = [2, 3, 1|_?]-?.
 *
 *     ?- construye_ld([1,2,3], L1), rotacion(L1,2,L2), cierra_ld(L2, L).
 *     L1 = ...-..., L2 = ...-..., L = [3, 1, 2].
 */
rotacion(LD, 0, LD).
rotacion([X|RestoLD]-Ultimo, N, Resultado) :-
    N > 0,
    length(LD, Longitud),
    Rotaciones is N mod Longitud,
    rotacion_aux([X|RestoLD]-Ultimo, Rotaciones, Resultado).

rotacion_aux(L, 0, L).
rotacion_aux([X|RestoLD]-Ultimo, Contador, Resultado) :-
    Contador > 0,
    ContadorActual is Contador - 1,
    append(Ultimo, [X], NuevoUltimo),
    rotacion_aux(RestoLD-NuevoUltimo, ContadorActual, Resultado).
/* creemos que había un problema con los test pues aunque no hubiera nada implementado del 3 al 6 siempre fallaban */ 

/**
 * Ejercicio 6 - mayusculas
 * `mayusculas(L, M)`: `M` es la cadena de texto `L`, con sus caracteres en mayúsculas.
 *  Asume que la cadena `L` contiene únicamente caracteres en el rango ASCII.
 *  Nota: Utiliza el predicado `string_codes` para convertir de cadenas a listas de enteros,
 *        y viceversa.
 *        https://www.swi-prolog.org/pldoc/man?predicate=string_codes/2
 *
 *     ?- mayusculas("prolog", M).
 *     M = "PROLOG".
 *
 *     ?- mayusculas("Haskell!", M).
 *     M = "HASKELL!".
 */
convert_mayus(X, Y) :-
    X >= 97,
    X =< 122,
    Y is X - 32.
convert_mayus(X, X).

mayusculas_ascii([], []) :- !.
mayusculas_ascii([H | T], [H1 | T1]) :-
    convert_mayus(H, Hmayus),
    H1 is Hmayus,
    mayusculas_ascii(T, T1),
    !.

mayusculas(L, R) :-
    string_codes(L, Lcods),
    mayusculas_ascii(Lcods, Rcods),
    string_codes(R, Rcods).

/**
 * Ejercicio 7 - separa
 * `separa(Texto, Separador, Resultado)`: `Resultado` es una lista con las cadenas que resultan
 *  de dividir la cadena `Texto` en cada aparición de `Separador`. Por simplicidad, asume que
 *  `Separador` es un código ASCII.
 *  Nota: Utiliza el predicado `string_codes` para convertir de cadenas a listas de enteros,
 *        y viceversa.
 *        https://www.swi-prolog.org/pldoc/man?predicate=string_codes/2
 *
 *     ?- separa("prolog es un buen lenguaje", 32, R). % 32 es el código ASCII de " " (espacio).
 *     R = ["prolog", "es", "un", "buen", "lenguaje"].
 *
 *     ?- separa("prolog", 111, R). % 111 es el código ASCII de "o".
 *     R = ["pr", "l", "g"].
 */
separa_ascii([], _, []) :- !.
separa_ascii(Cods, _, [H]) :-
    string_codes(H, Cods),
    !.
separa_ascii(Cods, S, [H | T]) :-
    append(Palabra, [S | Resto], Cods),
    string_codes(H, Palabra),
    separa_ascii(Resto, S, T),
    !.

separa(T, S, R) :-
    string_codes(T, Cods),
    separa_ascii(Cods, S, R).

/**
 * Ejercicio 8 - cifrado
 * `cifrado(Text, Llave, Res)`: `Res` es la cadena que resulta de aplicar un cifrado aditivo a
 *  `Text` con el desplazamiento `Llave`.
 *  Un cifrado aditivo consiste en desplazar cada elemento de un mensaje, una cierta cantidad de
 *  posiciones. Si estamos trabajando en ASCII, entonces sería equivalente a sumar (de ahí su nombre)
 *  un valor a cada uno de los caracteres vistos como enteros.
 *  Nota1: Utiliza el predicado `string_codes` para convertir de cadenas a listas de enteros,
 *         y viceversa.
 *         https://www.swi-prolog.org/pldoc/man?predicate=string_codes/2
 *  Nota2: Ten cuidado pues el operador `mod` de prolog, no se comporta de la forma esperada con
 *         números negativos.
 *
 *     ?- cifrado("prolog", 1, R). % "prolog" en ASCII [112, 114, 111, 108, 111, 103]
 *     R = "qspmph".               % "qspmph" en ASCII [113, 115, 112, 109, 112, 104]
 *
 *     ?- separa("qspmph", -1, R).
 *     R = "prolog".
 */
cifrado_ascii([], _, []) :- !.
cifrado_ascii([H | T], S, [H1 | T1]) :-
    H1 is H + S,
    cifrado_ascii(T, S, T1).

cifrado(T, S, R) :- 
    string_codes(T, Cods),
    cifrado_ascii(Cods, S, Rcods),
    string_codes(R, Rcods).

/**
 * Ejercicio Extra - acertijo
 * `acertijo(Solucion)`: Predicado en donde `Solucion` es una lista con 5 `sol(Conductor, Marca, Color, Pais)`
 *  que devuelve una solución válida al siguiente problema.
 *
 * En una carrera de fórmula 1, los primeros 5 lugares de la carrera fueron ocupados por autos de distintos países:
 * a) El conductor del auto rojo es italiano.
 * b) El Monza clasificó delante del de Argentina.
 * c) El Mazda clasificó en tercer lugar.
 * d) Jorge es de Alemania.
 * e) El Monza es de color amarillo.
 * f) El auto de Pablo se clasificó delante del auto de Mario.
 * g) El auto francés es de color amarillo.
 * h) Mario corrió un Ferrari.
 * i) El Susuzi clasificó después del auto de Raúl y no llegó en segundo lugar.
 * j) Jorge se clasificó en tercer lugar y su auto no es azul.
 * k) El Trooper se clasificó luego del de Alemania y antes del rojo.
 * l) El auto negro no llego ni en primero ni en último.
 * m) Pablo es piloto del auto amarillo.
 * n) Carlos terminó después del de Brasil.
 * o) El auto de Argentina es blanco.
 *
 * ¿Cuáles son los corredores, posiciones, marcas y colores de autos que quedaron en cada posición?
 *
 * Hint: Define los predicados `antes(X, Y, L)` y `despues(X, Y, L)` para determinar si `X` aparece
 * en `L` antes/después que `Y`.
 */
conductor(jorge).
conductor(pablo).
conductor(mario).
conductor(carlos).
conductor(raul).

marca(monza).
marca(mazda).
marca(ferrari).
marca(suziku).
marca(trooper).

color(rojo).
color(amarillo).
color(azul).
color(blanco).
color(negro).

pais(italia).
pais(argentina).
pais(alemania).
pais(francia).
pais(brasil).

sol(Conductor, Marca, Color, Pais) :-
    conductor(Conductor),
    marca(Marca),
    color(Color),
    pais(Pais).
