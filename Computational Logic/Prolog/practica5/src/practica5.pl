/**
 * Ejercicio 1 - mcd
 * `mcd(N, X, Y)`: `N` es el máximo común divisor de `X` y `Y`
 *
 *     ?- mcd(6, 4, X).
 *     X = 2.
 *
 *     ?- mcd(52, 2934178, X).
 *     X = 26.
 */
mcd(0, Y, Y).
mcd(X, 0, X).
mcd(X, X, X).
mcd(X, Y, N) :- 
    Y > X, 
    Y1 is Y - X, 
    mcd(X, Y1, N).
mcd(X, Y, N) :- 
    Y < X, 
    X1 is X - Y, 
    mcd(X1, Y, N).

/**
 * Ejercicio 2 - ocurrencias
 * `ocurrencias(L, X, N)`. `N` es el número de veces que `X` aparece en la lista `L`.
 *
 *     ?- ocurrencias([1,2,3], 2, N).
 *     N = 1 .
 *
 *     ?- ocurrencias([1,6,8,4,7,9,6,3,3,7,8,4,2,3], 3, N).
 *     N = 3.
 *
 *     ?- ocurrencias([1,2,3], 5, N).
 *     N = 0.
 */
ocurrencias([], _, 0).
ocurrencias([X | T], X, N) :-
    ocurrencias(T, X, N1),
    N is N1 + 1.
ocurrencias([H | T], X, N) :-
    H \= X,
    ocurrencias(T, X, N).

/**
 * Ejercicio 3 - contiene
 * `contiene(L, X)`: Decide si la lista `L` contiene al elemento `X`.
 *
 *     ?- contiene([1,2,3,4,5], 5).
 *     true.
 *
 *     ?- contiene([1,2,3,4,5], 10).
 *     false.
 */
contiene([X | _], X) :- !.
contiene([H | T], X) :-
    H \= X,
    contiene(T, X).

/**
 * Ejercicio 4 - elimina
 * `elimina(L1, X, L2)`: `L2` es la lista que resulta de eliminar el elemento `X`
 * de la lista `L1`. En particular, si `X` no está en `L1`, entonces la lista se
 * mantiene igual.
 *
 *     ?- elimina([1,2,3,4,5,4,3,2,1], 3, L).
 *     L = [1, 2, 4, 5, 4, 2, 1].
 *
 *     ?- elimina([1,2,3], 5, L).
 *     L = [1, 2, 3].
 */
elimina([], _, []).
elimina([X | T], X, T1) :- 
    elimina(T, X, T1).
elimina([H | T], X, [H | T1]) :-
    H \= X,
    elimina(T, X, T1).


/**
 * Ejercicio 5 - elimina
 * `aplana(L1, L2)`: `L2` es la lista `L1` aplanada. Entendemos "aplanar" como la
 * operación que deja los elementos de las sublistas, en una única lista.
 *
 *     ?- aplana([1,[2,[3]],4], L).
 *     L = [1, 2, 3, 4].
 *
 *     ?- aplana([1,2,3,4], L).
 *     L = [1, 2, 3, 4].
 */
aplana([], []).
aplana([H|T], L2) :-
    is_list(H),
    aplana(H, H1),
    aplana(T, T1),
    append(H1, T1, L2).
aplana([H|T], [H|T1]) :-
    \+ is_list(H),
    aplana(T, T1).

/**
 * Ejercicio 6 - reversa
 * `reversa(L1, L2)`: `L2` es la reversa de la lista `L1`.
 *
 *     ?- reversa([1,2,3,4], L).
 *     L = [4, 3, 2, 1].
 *
 *     ?- reversa([a, b, c, d, e], L).
 *     L = [e, d, c, b, a].
 */
reversa([], []).
reversa([H|T], L) :- reversa(T, TR), append(TR, [H], L).

/**
 * Ejercicio 7 - en_orden
 * `en_orden(T, L)`: `L` es la lista con los elementos de `T`, siguiendo el orden de
 * un recorrido "en orden". Para esto, los árboles están dados con las siguientes
 * definiciones: `empty`, `tree(Nodo, Izquierda, Derecha)`.
 *
 *     ?- en_orden(tree(1, tree(2, tree(3, tree(4, empty, empty), empty), empty), empty), X).
 *     X = [4, 3, 2, 1].
 *
 *     ?- en_orden(tree(1, empty, tree(2, tree(3, empty, empty), empty)), X).
 *     X = [1, 3, 2].
 */
en_orden(empty, []).
en_orden(tree(Nodo, Izquierda, Derecha), Lista) :-
    en_orden(Izquierda, IzquierdaLista),
    en_orden(Derecha, DerechaLista),
    append(IzquierdaLista, [Nodo|DerechaLista], Lista).

/**
 * Ejercicio 8 - Genealogía de matemáticos
 *
 * Define las reglas y hechos que modelan el siguiente problema:
 *
 * > La profesora Lourdes viene de una estirpe de matemáticos muy importantes.
 * > Su asesor de tesis fue el profesor Favio, a quien le apasiona la teoría de la
 * > prueba. Esta rama de la matemática que se enfoca en el estudio de las
 * > demostraciones, surgió con David Hilbert, y algunos de los aportes más
 * > interesantes vinieron por parte de su alumno Schütte. A su vez, Schüte la
 * > enseñó a un matemático alemán de apellido Buchholz, que eventualmente enseñó
 * > a Favio.
 * > Pero la genealogía también incluye algunos matemáticos famosos por sus
 * > aportaciones en probabilidad. Uno de los más famosos es Poisson, que tuvo dos
 * > alumnos destacados: Dirichlet y Chasles. Del lado de Dirichlet prefirieron la
 * > geometría con su estudiante Lipschitz, quien a su vez dirigió la tesis de
 * > Klein, y después Klein asesora a Lindenmann (asesor de tesis de Hilbert).
 * > Del lado de Chasles surge una línea de matemáticos con intereses en la física
 * > y astronomía. El más notable de los estudiantes de Chasles probablemente fue
 * > Hubert Newton, quien usó la geometría para estudiar la trayectoria de meteoros,
 * > pasándole muchas de esas nociones a su estudiante Moore, quien luego asesoró a
 * > Veblen. De Veblen viene un estudiante famoso entre los computólogos:
 * > Alonzo Church quien, junto con su estudiante Alan Turing, desarrollaron mucha
 * > de la teoría que usamos en las Ciencias de la Computación hoy en día.
 *
 * Más adelante, definimos los nombres de los académicos. *No los modifiques.*
 *
 * Define las relaciones necesarias para modelar el problema.
 *
 * Luego, define las siguientes relaciones:
 *  + `descendiente(X, Y)`: `X` desciende de `Y`.
 *  + `ascendiente(X, Y)`: `X` asciende de `Y`.
 */

% Hechos
academico(poisson).
academico(dirichlet).
academico(lipschitz).
academico(klein).
academico(lindenmann).
academico(hilbert).
academico(schute).
academico(buchholz).
academico(favio).
academico(lourdes).
academico(chasles).
academico(newton).
academico(moore).
academico(veblen).
academico(church).
academico(turing).
% Lado 1
descendiente_directo(dirichlet, poisson).
descendiente_directo(lipschitz, dirichlet).
descendiente_directo(klein, lipschitz).
descendiente_directo(lindenmann, klein).
descendiente_directo(hilbert, lindenmann).
descendiente_directo(schute, hilbert).
descendiente_directo(buchholz, schute).
descendiente_directo(favio, buchholz).
descendiente_directo(lourdes, favio).
% Lado 2
descendiente_directo(chasles, poisson).
descendiente_directo(newton, chasles).
descendiente_directo(moore, newton).
descendiente_directo(veblen, moore).
descendiente_directo(church, veblen).
descendiente_directo(turing, church).

% Relaciones
descendiente(X, Y) :- descendiente_directo(X, Y).
descendiente(X, Y) :- descendiente_directo(X, Z), descendiente(Z, Y).

ascendiente(X, Y) :- descendiente(Y, X).

