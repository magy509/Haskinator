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

correr(C,F):-
    atom_chars(C, Is),
    inicial(X),
    ejecutar(X, Is, F).

inicial(estado([],0,[0,0,0,0,0,0,0,0,0,0])).

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
interpretar(estado(Anterior,Actual,Posterior),',',estado(Anterior,Nuevo,Posterior)):-
    get_single_char(Nuevo),
    !.

%Escritura
interpretar(estado(Anterior,Actual,Posterior),'.',estado(Anterior,Actual,Posterior)):-
    put(Actual),
    !.

%Iteración

%Comentarios
interpretar(I,_,I):-!.


debuguea(M, C):- write(M), write(' '), write(C), put(10).

%Salta en Lista

salta([],_):-
    !,fail.

salta([']'|C],C):-
    debuguea('cierra', C),
    !.

salta(['['|C],C1):-
    debuguea('abre', C),
    salta(C,C2),
    salta(C2,C1),
    !.

salta([H|C],C1):-
    debuguea(H, C),
    not(H=']'), not(H='['), % FIXME
    salta(C,C1),
    !.

%Interpretar
ejecutar(F,[],F):-!.

ejecutar(estado(Anterior,0,Posterior),['['|T],Final):-
    salta(T,T1),
    ejecutar(estado(Anterior,0,Posterior),T1,Final),
    !.

ejecutar(I,['['|T],Final):-
    ejecutar(I,T,M),
    ejecutar(M,['['|T],Final),
    !.

ejecutar(estado(Anterior,0,Posterior),[']'|T],Final):-
    ejecutar(estado(Anterior,0,Posterior),T,Final),
    !.

ejecutar(I,[']'|T],I):-
    !.

ejecutar(I,[E|L],F):-
    interpretar(I,E,M),
    ejecutar(M,L,F),!.


