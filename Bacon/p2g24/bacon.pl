%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Bacon Pancakes                     %%
%%    María Gracia Hidalgo # 03-36048    %%
%%    Grupo 24                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Carga un archivo pasado por entrada estándar
cargar(X,L):-
    catch(see(X), _, (write('No se encuentra el archivo.'),fail)),
    leer(L),
    seen,
    !.

% Lee los caracteres de un archivo y los coloca en una lista
leer(X):-
    get_char(T),
    T \= end_of_file,
    leer(X1),
    X = [T|X1].

leer([]).

% Verifica que el formato del archivo sea correcto.
verificar(0,[]):-!.

verificar(_,[]):-!, write('Fallo Miserablemente\n'), fail.

verificar(I,_):-I < 0, !, write('Fallo Miserablemente\n'), fail.

verificar(I,['['|L]):-!, N is I + 1, verificar(N,L).

verificar(I,[']'|L]):-!, N is I - 1, verificar(N,L).

verificar(I,[_|L]):-!, verificar(I,L).

% Construye una lista con N ceros
ceros(1, []):-!.

ceros(N, [X|Xs]):-
    N > 1,
    !,
    X is 0,
    M is N - 1,
    ceros(M,Xs).

ceros(_,_):-!,fail.

% Hace las verificaciones necesarias para ejecutar un archivo en brainfuck
correr(I,P,_):-
    cargar(P,L),
    verificar(0,L),
    !,
    ejecutar(I,L).

% Interpreta y ejecuta los elementos contenidos en la lista de instrucciones
interpretar(I, [], I):-!.

%Incremento
interpretar(estado(Anterior, Actual, Posterior), '+', estado(Anterior, Nuevo, Posterior)):-
    Nuevo is Actual+1,
    !.

%Decremento
interpretar(estado(Anterior, Actual, Posterior), '-', estado(Anterior, Nuevo, Posterior)):-
    Nuevo is Actual-1,
    !.

%Avance
interpretar(estado(Anterior, Actual, [P|Ps]), '>',estado([Actual|Anterior], P, Ps)):-!.

%Retroceso
interpretar(estado([A|As], Actual, Posterior), '<', estado(As, A, [Actual|Posterior])):-!.

%Lectura
interpretar(estado(Anterior, _, Posterior), ',', estado(Anterior, Nuevo, Posterior)):-
    !,
    get_single_char(Nuevo).

%Escritura
interpretar(estado(Anterior, Actual, Posterior), '.',estado(Anterior, Actual, Posterior)):-
    !,
    put(Actual).

%Comentarios
interpretar(I, _, I):-!.

%Salta en Lista, apareando corchetes
salta([], _):-!,fail.

salta([']'|C], C):-!.

salta(['['|C], C1):-
    salta(C, C2),
    !,
    salta(C2, C1).

salta([_|C], C1):-
    !,
    salta(C, C1).

% Ejecuta lista de instrucciones proporcionadas sobre la cinta
ejecutar(_, F, [], F):-!.

ejecutar(X, estado(Anterior, 0, Posterior), ['['|T], F):-
    !,
    salta(T, T1),
    ejecutar(X, estado(Anterior, 0, Posterior), T1, F).

ejecutar(X, I, ['['|T], F):-
    !,
    N is X + 1,
    ejecutar(N, I, T, M),
    ejecutar(X, M, ['['|T], F).

ejecutar(_, estado(Anterior, 0, Posterior), [']'|_], estado(Anterior, 0, Posterior)):-!.

ejecutar(_, I, [']'|_], I):-!.

ejecutar(X, I, [E|L], F):-
    !,
    interpretar(I, E, M),
    ejecutar(X, M, L, F).

% Inicializa la cinta y ejecuta las instrucciones suministradas
ejecutar(1, L):-
    !,
    ejecutar(0, estado([],0,[]), L, _).

ejecutar(_, []):-!.

ejecutar(K, L):-
    !,
    ceros(K, I),
    ejecutar(0, estado([], 0, I), L, _).

% Programa Principal Brainfuck
brainfk:-
    !,
    write('Por favor, escriba el nombre del programa en Brainf**k que desea interpretar:'),
    nl,
    read(X),
    nl,
    correr(10000,X,_).
