% Ejercicio 1
:- begin_tests(construye_ld).

    test(construye_ld1) :- construye_ld([], []-[]).

    test(construye_ld2) :- construye_ld([], 1-1).

    test(construye_ld3) :- construye_ld([], true-true).

    test(construye_ld4) :- construye_ld([1,2,3], [1,2,3|[]]-[]).

    test(construye_ld5) :- construye_ld([1,2,3], [1,2,3|1]-1).

    test(construye_ld6) :- construye_ld([1,2,3], [1,2,3|true]-true).

:- end_tests(construye_ld).

% Ejercicio 2
:- begin_tests(cierra_ld).

    test(cierra_ld1) :- cierra_ld([]-[], []).

    test(cierra_ld2) :- cierra_ld([1,2,3|[]]-[], [1,2,3]).

:- end_tests(cierra_ld).

% Inversos
:- begin_tests(inversos).

    test(inversos1) :- construye_ld([1, 2, 3], L1), cierra_ld(L1, L), L = [1, 2, 3].

    test(inversos2) :- construye_ld([], L1), cierra_ld(L1, L), L = [].

    test(inversos3) :- cierra_ld([1, 2, 3 | []]-[], L1), construye_ld(L1, L), L = [1, 2, 3 | []]-[].

:- end_tests(inversos).

% Ejercicio 3
:- begin_tests(append_ld).

    test(append_ld1) :- construye_ld([], L1), construye_ld([], L2), append_ld(L1, L2, LD), cierra_ld(LD, L), L = [].

    test(append_ld2) :- construye_ld([1,2,3], L1), construye_ld([4,5,6], L2), append_ld(L1, L2, LD), cierra_ld(LD, L), L = [1,2,3,4,5,6].

    test(append_ld3) :- construye_ld([1,2,3], L1), construye_ld([4,5,6], L2), append_ld(L1, L2, LD), LD = [1,2,3,4,5,6 | []] - [].

:- end_tests(append_ld).

% Ejercicio 4
:- begin_tests(reversa).

    test(reversa1) :- reversa_ld([1,2,3,4], [4,3,2,1]).

    test(reversa2) :- reversa_ld([a, b, c, d, e], [e, d, c, b, a]).

    test(reversa3) :- reversa_ld([], []).

    test(reversa4) :- reversa_ld([hola, mundo], [mundo, hola]).

    test(reversa5) :- reversa_ld([prueba], [prueba]).

:- end_tests(reversa).

% Ejercicio 5
:- begin_tests(rotacion).

    test(rotacion1) :- construye_ld([1,2,3,4], LD), rotacion(LD, 0, L), cierra_ld(L, [1,2,3,4]).

    test(rotacion2) :- construye_ld([], LD), rotacion(LD, 0, L), cierra_ld(L, []).

    test(rotacion3) :- construye_ld([], LD), rotacion(LD, 5, L), cierra_ld(L, []).

    test(rotacion4) :- construye_ld([1,2,3,4], LD), rotacion(LD, 1, L), cierra_ld(L, [2,3,4,1]).

    test(rotacion5) :- construye_ld([1,2,3,4], LD), rotacion(LD, 2, L), cierra_ld(L, [3,4,1,2]).

    test(rotacion6) :- construye_ld([1,2,3,4], LD), rotacion(LD, 5, L), cierra_ld(L, [2,3,4,1]).

:- end_tests(rotacion).

% Ejercicio 6
:- begin_tests(mayusculas).

    test(mayusculas1) :- mayusculas("prolog", "PROLOG").

    test(mayusculas2) :- mayusculas("Haskell", "HASKELL").

    test(mayusculas3) :- mayusculas("!Haskell!", "!HASKELL!").

    test(mayusculas4) :- mayusculas("!.,", "!.,").

    test(mayusculas5) :- mayusculas("LOGICA", "LOGICA").

    test(mayusculas6) :- mayusculas("", "").

:- end_tests(mayusculas).

% Ejercicio 7
:- begin_tests(separa).

    test(separa1) :- separa("prolog es un buen lenguaje", 32, ["prolog", "es", "un", "buen", "lenguaje"]).

    test(separa2) :- separa("prolog", 111, ["pr", "l", "g"]).

    test(separa3) :- separa("prolog", 64, ["prolog"]).

    test(separa4) :- separa("", 32, []).

:- end_tests(separa).

% Ejercicio 8
:- begin_tests(cifrado).

    test(cifrado1) :- cifrado("prolog", 1, "qspmph").

    test(cifrado2) :- cifrado("qspmph", -1, "prolog").

    test(cifrado3) :- cifrado("", -7, "").
    
:- end_tests(cifrado).
