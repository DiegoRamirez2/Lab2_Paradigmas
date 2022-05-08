
% Función SubListaAux
sublistaAux([E|R],L) :- append(_,X,L),append([E|R],_,X).
% Función que crea el mazo dobble con números
mod1(M,N,L) :- L is M mod N, L > 0, !.
mod1(_,N,N).
numGenerator(_,0,0,0).
numGenerator(N,I,0,P, Lista) :- I > 0, Aux is (I - 1) div N, cardsSetNthCard(Lista, Aux, P). % Acá se genera la primera cartas
numGenerator(N,I,J,P, Lista) :- I =< N, J > 0, Aux is N * I + J, cardsSetNthCard(Lista, Aux, P). % Acá se generan las N cartas
numGenerator(N,I,J,P, Lista) :- I > N, J > 0, T1 is (J - 1) * ((I - 1) div N - 1) + I, mod1(T1,N,T2), Aux is N * J + T2, cardsSetNthCard(Lista, Aux, P). % Acá se generan las N cartas

aux1(_,N,_,M,[]) :- M > N, !. % Esto es para marcar el total de combinaciones que se harán
aux1(ListaElementos,N,I,J,[P|Ps]) :- numGenerator(N,I,J,P,ListaElementos), J1 is J + 1, aux1(ListaElementos,N,I,J1,Ps). % En el numGenerator de acá se debería hacer uso de la lista con elementos
card(ListaElementos,N,I,L) :- M is N * (N + 1), between(0,M,I), aux1(ListaElementos,N,I,0,L).

dobble(ListaElementos,K,CS) :- N is K - 1, findall(L,card(ListaElementos,N,_,L),CS).
finalDobble(ListaElementos, K, Cards) :- dobble(ListaElementos, K, CS), firstCard(ListaElementos, K, P), append([P], CS, Cards).
% cardsSetNthCard
cardsSetNthCard( [Cabeza|_], 0, Cabeza):- !.
cardsSetNthCard( [_|Cabeza], Posicion, Elemento) :- 
	PosicionAnterior is Posicion-1,
	cardsSetNthCard(Cabeza, PosicionAnterior, Elemento).
% Verificador de Dobble
check([]).
check([C|Cs]) :-
forall(select(X,C,Xs),\+member(X,Xs)),
forall(member(X,Cs),intersection(C,X,[_])),
check(Cs).
% Funciones de apoyo
randomFn(Seed, Result) :- Result is ((Seed * 5) div 3).
getfirstCard([L|_], Cabeza) :- rotate(L, 0, Cabeza).
finalCard(Lista, Cabeza) :- reverse(Lista, P), getfirstCard(P, Cabeza), !.
% Agregar a la Cola
agregarCola(E, [], E).
agregarCola(E, [X|Xs], [X|Ys]) :- agregarCola(E, Xs, Ys).
firstCardAux(Lista, Num, P) :- N is Num - 1, between(0, N, I), cardsSetNthCard(Lista, I, R), agregarCola(R, [], P).
firstCard(Lista, Num, P) :- findall(L, firstCardAux(Lista, Num, L), P).

rotate([],_,_):- !.
rotate(L,0,L).
rotate([C|Q], N, P) :- append(Q, [C], L2), M is N - 1, rotate(L2, M, P), !.
cardRotate(ListElement, Num, Nrotate, P) :- finalDobble(ListElement, Num, CS), rotate(CS, Nrotate, P).

sublistaAux( [_|Cola], 0, Cola):- !.
sublistaAux( [_|Cabeza], Posicion, Elemento) :- 
  PosicionAnterior is Posicion-1,
	sublistaAux(Cabeza, PosicionAnterior, Elemento).

largeList([], 0).
largeList([_|Lista], P) :- largeList(Lista, P1), P is P1 + 1.
cutList(Lista, Num, R) :- var(Num), rotate(Lista, 0, R), !.
cutList(Lista, Num, R) :- largeList(Lista, P1), P1 = Num, rotate(Lista, 0, R), !.
cutList(Lista, Num, R) :- largeList(Lista, P1), P2 is P1 - (Num + 1), sublistaAux(Lista, P2, R).
cardsSetDeleteCard(Lista, Num, R) :- largeList(Lista, P1), (P1 - 1) >= Num, sublistaAux(Lista, Num, R).

% Función 
cardsSet(ListaElementos, NumE, MaxC, Seed, CS) :- 
  finalDobble(ListaElementos, NumE, RC),  
  randomFn(Seed, Result),
  rotate(RC, Result, RotateR),
  cutList(RotateR, MaxC, CSi),
  append(CSi, [ListaElementos], CS).

% cardsSetFindTotalCards
cardsSetFindTotalCards(CS, TC) :- getfirstCard(CS, FC), largeList(FC, P), TC is (P * P) + P + 1.
% Buscar Pertenece
% set_prolog_flag(answer_write_options,[max_depth(0)]).
cardsSetMissingCards(CS2, CS3):- 
  getfirstCard(CS2, C), 
  finalCard(CS2, FC), 
  largeList(C, Result), 
  cardsSet(FC, Result, _, 5, NCS), 
  subtract(NCS, CS2, CS3).


cardsSetToString(CS, R) :- cardsAux(CS, 1, '', R).
cardsAux([_],_,Cadena,R):- string_concat(Cadena, '', R), !.
cardsAux([X|Xs], Num, Cadena, R) :- number_string(Num, Number), string_concat(Number, ': ', X1), string_concat("Tarjeta ", X1, X2), Num2 is Num + 1, atomic_list_concat(X, ', ', X3), string_concat(X2, X3, X4), 
  string_concat(X4, '\n', X5), string_concat(Cadena, X5, Re), cardsAux(Xs, Num2, Re, R).


dobbleGame(NumP, CS, GameMode, Seed, [NumP, [], GameMode, Seed, CS, []]).