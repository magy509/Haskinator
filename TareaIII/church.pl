/** Tarea III - Prolog
  * María Gracia Hidalgo
  * CI3661 - Laboratorio de Lenguajes de Programación
*/

% Representación Números Naturales

*(0).
up(*(1)).
up(up(*(2))).
up(up(up(*(3)))).

% Operaciones

suma(*,Y,Y).

suma(up(X),Y,up(Z)):-
    suma(X,Y,Z).

resta(X,Y,Z):-
    suma(Y,Z,X).

producto(_,*,*).

producto(X,up(Y),Z):-
    producto(X,Y,Z1),
    suma(X,Z1,Z).
