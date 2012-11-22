abrir(X,L):-
    see(X),
    leer(L),
    seen,
    !.

leer(X):-
    get_char(T),
    T\=end_of_file,
    leer(X1),
    X=[T|X1].

leer([]).

ceros(1,[]):-!. %FIXME

ceros(N,[X|Xs]):-
    N > 0,
    X is 0,
    M is N - 1,
    ceros(M,Xs),
    !.

correr(C, F):-
    atom_chars(C, Is),
    inicial(X),
    ejecutar(0, X, Is, F),!.

inicial(estado([],0,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])).

interpretar(I,[],I).

%Incremento
interpretar(estado(Anterior,Actual,Posterior),'+',estado(Anterior,Nuevo,Posterior)):-
    Nuevo is Actual+1,
    !.

%Decremento
interpretar(estado(Anterior,Actual,Posterior),'-',estado(Anterior,Nuevo,Posterior)):-
    Nuevo is Actual-1,
    !.

%Avance
interpretar(estado(Anterior,Actual,[P|Ps]),'>',estado([Actual|Anterior],P,Ps)):-!.

%Retroceso
interpretar(estado([A|As],Actual,Posterior),'<',estado(As,A,[Actual|Posterior])):-!.

%Lectura
interpretar(estado(Anterior,_,Posterior),',',estado(Anterior,Nuevo,Posterior)):-
    get_single_char(Nuevo),
    !.

%Escritura
interpretar(estado(Anterior,Actual,Posterior),'.',estado(Anterior,Actual,Posterior)):-
    put(Actual),
    !.

%Iteración

%Comentarios
interpretar(I, _, I):-!.


debuguea(M, C):- write(M), write(' '), write(C), put(10).

%Salta en Lista

salta([],_):-
    !,fail.

salta([']'|C],C):-
    !.

salta(['['|C],C1):-
    salta(C, C2),
    !,
    salta(C2, C1),
    !.

salta([_|C],C1):-
    salta(C, C1),
    !.

restar(0,_):-
    !,fail.

restar(X,N):-
    N is X - 1,
    !.

%Interpretar
ejecutar(_, F, [], F):-!.

ejecutar(X, estado(Anterior,0,Posterior), ['['|T], Final):-
    salta(T, T1),
    ejecutar(X, estado(Anterior,0,Posterior), T1, Final),
    !.

ejecutar(X, I, ['['|T], Final):-
    N is X + 1,
    debuguea(I, N),
    ejecutar(N, I, T, M),
    ejecutar(X, M, ['['|T], Final),
    !.

ejecutar(_, estado(Anterior,0,Posterior), [']'|_], estado(Anterior,0,Posterior)):-!.

ejecutar(_, I, [']'|_], I):-!.

ejecutar(X, I, [E|L], F):-
    interpretar(I, E, M),
    ejecutar(X, M, L, F),!.

