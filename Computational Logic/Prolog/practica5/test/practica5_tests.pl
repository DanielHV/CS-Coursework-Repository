% Ejercicio 1
:- begin_tests(mcd).

    test(mcd1, [nondet]) :- mcd(6, 4, 2).

    test(mcd2, [nondet]) :- mcd(52, 2934178, 26).

    test(mcd3, [nondet]) :- mcd(12, 30, 6).

    test(mcd4, [nondet]) :- mcd(4208, 288, 16).

    test(mcd5, [nondet]) :- mcd(234, 42, 6).

:- end_tests(mcd).

% Ejercicio 2
:- begin_tests(ocurrencias).

    test(ocurrencias1, [nondet]) :- ocurrencias([1,2,3], 2, 1).

    test(ocurrencias2, [nondet]) :- ocurrencias([1,6,8,4,7,9,6,3,3,7,8,4,2,3], 3, 3).

    test(ocurrencias3, [nondet]) :- ocurrencias([1,2,3], 5, 0).

    test(ocurrencias4, [nondet]) :- ocurrencias([1,2,3], prueba, 0).

    test(ocurrencias5, [nondet]) :- ocurrencias([], 0, 0).

    test(ocurrencias6, [nondet]) :- ocurrencias([a], a, 1).

:- end_tests(ocurrencias).

% Ejercicio 3
:- begin_tests(contiene).

    test(contiene1, condition(true)) :- contiene([1,2,3,4,5], 5).

    test(contiene2, condition(false)) :- contiene([1,2,3,4,5], 10).

    test(contiene3, condition(true)) :- contiene([a, b, c, d], a).

    test(contiene4, condition(false)) :- contiene([a, b, c, d], x).

    test(contiene5, condition(false)) :- contiene([], prueba).

:- end_tests(contiene).

% Ejercicio 4
:- begin_tests(elimina).

    test(elimina1, [nondet]) :- elimina([1,2,3,4,5,4,3,2,1], 3, [1,2,4,5,4,2,1]).

    test(elimina2, [nondet]) :- elimina([1,2,3], 5, [1,2,3]).

    test(elimina3, [nondet]) :- elimina([], prueba, []).

    test(elimina4, [nondet]) :- elimina([a, b, c, d, e], b, [a, c, d, e]).

    test(elimina5, [nondet]) :- elimina([a, b, c, d, e], a, [b, c, d, e]).

    test(elimina6, [nondet]) :- elimina([a, b, c, d, e], e, [a, b, c, d]).

:- end_tests(elimina).

% Ejercicio 5
:- begin_tests(aplana).

    test(aplana1, [nondet]) :- aplana([1,[2,[3]],4], [1,2,3,4]).

    test(aplana2, [nondet]) :- aplana([1,2,3,4], [1,2,3,4]).

    test(aplana3, [nondet]) :- aplana([1,[2],3,4], [1,2,3,4]).

    test(aplana4, [nondet]) :- aplana([1,[[2]],3,4], [1,2,3,4]).

    test(aplana5, [nondet]) :- aplana([1,[2],3,[4]], [1,2,3,4]).

    test(aplana6, [nondet]) :- aplana([], []).

:- end_tests(aplana).

% Ejercicio 6
:- begin_tests(reversa).

    test(reversa1, [nondet]) :- reversa([1,2,3,4], [4,3,2,1]).

    test(reversa2, [nondet]) :- reversa([a, b, c, d, e], [e, d, c, b, a]).

    test(reversa3, [nondet]) :- reversa([], []).

    test(reversa4, [nondet]) :- reversa([hola, mundo], [mundo, hola]).

    test(reversa5, [nondet]) :- reversa([prueba], [prueba]).

:- end_tests(reversa).

% Ejercicio 7
:- begin_tests(en_orden).

    test(en_orden1, [nondet]) :- en_orden(tree(1, tree(2, tree(3, tree(4, empty, empty), empty), empty), empty), [4, 3, 2, 1]).

    test(en_orden2, [nondet]) :- en_orden(tree(a, empty, tree(b, tree(c, empty, empty), empty)), [a, c, b]).

    test(en_orden3, [nondet]) :- en_orden(tree(1, tree(2, tree(5, empty, empty), tree(6, empty, empty)), tree(3, empty, empty)), [5, 2, 6, 1, 3]).

    test(en_orden4, [nondet]) :- en_orden(empty, []).

    test(en_orden5, [nondet]) :- en_orden(tree(1, empty, empty), [1]).

:- end_tests(en_orden).

% Ejercicio 8
:- begin_tests(genealogia).

    test(genealogia1, condition(false)) :- ascendiente(favio, poisson).

    test(genealogia2, condition(true)) :- ascendiente(dirichlet, hilbert).

    test(genealogia3, condition(false)) :- ascendiente(klein, church).

    test(genealogia4, condition(true)) :- ascendiente(newton, turing).

    test(genealogia5, condition(true)) :- ascendiente(lipschitz, lourdes).

    test(genealogia6, condition(true)) :- descendiente(lourdes, lindenmann).

    test(genealogia7, condition(false)) :- descendiente(moore, moore).

    test(genealogia8, condition(false)) :- descendiente(poisson, chasles).

    test(genealogia9, condition(true)) :- descendiente(turing, church).

    test(genealogia10, condition(true)) :- descendiente(buchholz, dirichlet).

:- end_tests(genealogia).
