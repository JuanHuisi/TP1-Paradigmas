:- use_module(piezas).

% EJ 1
/*
    Justificación de Reversibilidad
    Para que el predicado sea reversible debe suceder que:
        1. No se cuelga.
        2. Debe contener al resultado que retorna el programa cuando el argumento tiene la instanciación contraria.
    Para eso, podemos chequear cada uno de los argumentos por separado para ver qué le sucede al predicado.
    
    Reversibilidad en el primer argumento (-Descartar): No es reversible en nuestra implementación. Esto es debido a que length(X, Descartar) genera todas las posibles listas X de longitud Descartar y como ambas variables no están instanciadas generará infinitas posibles unificaciones que luego tratará de unificar con el append(X, Y, L) lo cual nunca valdrá una vez que length(X) > length(L) y Descartar > length(L).
    Ej.: append([_, _, _], y, [a, b]) no tiene sentido, lo cual append([_, _, _, _], y, [a, b]) menos aún pues la cantidad de elementos de la lista original es 2 y no hay forma de generar una sublista de 3 elementos. Entonces, como no generamos una cota máxima para el valor de X y Descartar tiende a generar soluciones aparentemente infinitas, pero que ninguna satisface el último append.

    Reversibilidad en el cuarto argumento (+R): Es reversible en nuestra implementación. Como sublista en su firma de la función tiene (+Descartar, +Tomar, +L, -R) si instanciamos R la respuesta del predicado será un true / false depende de si R unifica con alguna de las sublistas generadas por el predicado para las condiciones de Descartar, Tomar y L en particular.
    R es utilizado tanto en length y append, ambos predicados reversibles. Lo cual, si lo enviamos o no, funcionará como esperamos, siempre y cuando cumplamos con la firma de los demás parámetros evitando recursiones infinitas.

    Reversibilidad en el primer y cuarto argumento(-Descartar, +R): Por el mismo argumento que utilizamos en la justificación del primer argumento (Descartar), por más que instanciemos R va a seguir fallando (se cuelga) puesto que si no instanciamos Descartar siempre llegará el momento en que buscará la forma de generar listas de mayor longitud que la que le proporcionamos en length(X, Descartar), lo cual, jamás unificará con ninguno de los append.
*/
% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :- length(R, Tomar), length(X, Descartar), append(X, Y, L), append(R, _, Y).

% EJ 2
% tablero(+K, -T)
tablero(K, T) :-
    K > 0,
    T = [R1, R2, R3, R4, R5],
    length(R1, K),
    length(R2, K),
    length(R3, K),
    length(R4, K),
    length(R5, K).

% EJ 3 (La ñ rompe todo, asi que el predicado se llamara tamanio)
% tamanio(+M, -F, -C).
tamanio([], 0, 0).
tamanio(M, F, C) :-
    length(M, F),
    nth0(0, M, R),
    length(R, C).

% EJ 4
% coordenadas(+T, -IJ)
coordenadas(T, (I, J)) :-
    tamanio(T, F, C),
    between(1, F, I),
    between(1, C, J).

% EJ 5
% kPiezas(+K, -PS)
kPiezas(K, PS) :-
    nombrePiezas(L),
    partes(L, PS),
    length(PS, K).

partes([], []).
partes([X|Xs], [X|Ys]) :- partes(Xs, Ys).
partes([_|Xs], Ys) :- partes(Xs, Ys).

% EJ 6
% seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I, J), ST) :-
    Descartar is I - 1,
    sublista(Descartar, ALTO, T, R),
    armarSeccion(R, ANCHO, J, ST),
    length(ST, ALTO).

armarSeccion(Rs, _, _, []) :- is_list(Rs).
armarSeccion([R|Rs], ANCHO, J, [ST|STs]) :-
    Descartar is J - 1,
    sublista(Descartar, ANCHO, R, ST),
    armarSeccion(Rs, ANCHO, J, STs).

% EJ 7
% ubicarPieza(+Tablero, +Identificador)
ubicarPieza(Tablero, Identificador) :-
    pieza(Identificador, P),
    tamanio(P, F, C),
    coordenadas(Tablero, IJ),
    seccionTablero(Tablero, F, C, IJ, P).

% EJ 8
% ubicarPiezas(+Tablero, +Poda, +Identificadores)¨
ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [I|Is]) :-
    ubicarPieza(Tablero, I),
    poda(Poda, Tablero),
    ubicarPiezas(Tablero, Poda, Is).

% EJ 9
% llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, Tablero) :-
    tablero(Columnas, Tablero),
    kPiezas(Columnas, PS),
    ubicarPiezas(Tablero, Poda, PS).

% EJ 10
cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

% EJ 11
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

todosGruposLibresModulo5(T) :-
    coordenadasLibres(T, L),
    agrupar(L, G),
    forall(member(X, G), (length(X, N), 0 =:= N mod 5)).

coordenadasLibres(T, L) :- findall(IJ, (coordenadas(T, IJ), esLibre(IJ, T)), L).

esLibre((I, J), T) :-
    nth1(I, T, R),
    nth1(J, R, X),
    var(X).
