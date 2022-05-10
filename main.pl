% Nombre: Diego Ramírez Vivas
% Sección: 
% Profesor de Sección: 
% dobbleGame(numPlayers, CardsSet, GameMode, Seed, G) (aridad = 5)
/*____________________________________________________________________________________________
TDA cardsSet
____________________________________________________________________________________________*/

% Predicados
% cardsSet(Elements, NumE, MaxC, Seed, CS) (aridad = 5)
% cardsSetIsDobble(CardsSet) (aridad = 1)
% cardsSetNthCard(Card, Int, TC) (aridad = 3)
% cardsSetFindTotalCards(Card, TC) (aridad = 2)
% cardsSetMissingCards(CardsSet1, CardsSet2) (aridad = 2)
% cardsSetToString(CardsSet, STR) (aridad = 2)

% Metas primarias: cardsSet, cardsSetIsDobble, cardsSetNthCard, cardsSetFindTotalCards, 
% cardsSetMissingCards, cardsSetToString

% ---------------------------REPRESENTACION-------------------------------------------------------

% El TDA cardsSet se representa como una lista de sublistas (list X list), las cuales contienen
% elementos que representan símbolos de una carta

% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------

% Dominio: Una lista de elementos, tres enteros y un cardsSet
% Descripción: Predicado que construye un Set de cartas
cardsSet(ListaElementos, NumE, MaxC, Seed, CS) :- 
  finalDobble(ListaElementos, NumE, RC),  
  randomFn(Seed, Result),
  rotate(RC, Result, RotateR),
  cutList(RotateR, MaxC, CS).

% Descripción: Predicado que obtiene el Mod siendo mayor que cero
mod1(M,N,L) :- L is M mod N, L > 0, !.
mod1(_,N,N).
% Descripción: Predicado que obtiene los elementos por posición de la lista de elementos
numGenerator(_,0,0,0).
numGenerator(N,I,0,P, Lista) :- I > 0, Aux is (I - 1) div N, cardsSetNthCard(Lista, Aux, P). 
numGenerator(N,I,J,P, Lista) :- I =< N, J > 0, Aux is N * I + J, cardsSetNthCard(Lista, Aux, P). 
numGenerator(N,I,J,P, Lista) :- I > N, J > 0, T1 is (J - 1) * ((I - 1) div N - 1) + I, mod1(T1,N,T2), Aux is N * J + T2, cardsSetNthCard(Lista, Aux, P).
aux1(_,N,_,M,[]) :- M > N, !.
aux1(ListaElementos,N,I,J,[P|Ps]) :- numGenerator(N,I,J,P,ListaElementos), J1 is J + 1, aux1(ListaElementos,N,I,J1,Ps).
card(ListaElementos,N,I,L) :- M is N * (N + 1), between(0,M,I), aux1(ListaElementos,N,I,0,L).
% Descripción: Predicado que genera el Set de cartas en su totalidad
dobble(ListaElementos,K,CS) :- N is K - 1, findall(L,card(ListaElementos,N,_,L),CS).
finalDobble(ListaElementos, K, Cards) :- dobble(ListaElementos, K, CS), firstCard(ListaElementos, K, P), append([P], CS, Cards).

% Descripción: Predicado que genera número pseudo-aleatorio
randomFn(Seed, Result) :- Result is ((Seed * 5) div 3).
% Descripción: Predicado que obtiene la primera carta
getfirstCard([L|_], Cabeza) :- rotate(L, 0, Cabeza), !.
% Descripción: Predicado que obtiene la última carta
finalCard(Lista, Cabeza) :- reverse(Lista, P), getfirstCard(P, Cabeza), !.
% Descripción: Predicado que agrega a la Cola
agregarCola(E, [], E).
agregarCola(E, [X|Xs], [X|Ys]) :- agregarCola(E, Xs, Ys).
% Descripción: Predicado que obtiene la primera carta en caso especifico
firstCardAux(Lista, Num, P) :- N is Num - 1, between(0, N, I), cardsSetNthCard(Lista, I, R), agregarCola(R, [], P).
firstCard(Lista, Num, P) :- findall(L, firstCardAux(Lista, Num, L), P).
% Descripción: Predicado que funciona rota una lista N veces
rotate([],_,_):- !.
rotate(L,0,L).
rotate([C|Q], N, P) :- append(Q, [C], L2), M is N - 1, rotate(L2, M, P), !.
cardRotate(ListElement, Num, Nrotate, P) :- finalDobble(ListElement, Num, CS), rotate(CS, Nrotate, P).
% Descripción: Predicado que obtiene la sublista de una lista
sublistaAux( [_|Cola], 0, Cola):- !.
sublistaAux( [_|Cabeza], Posicion, Elemento) :- 
  PosicionAnterior is Posicion-1,
	sublistaAux(Cabeza, PosicionAnterior, Elemento).
% Descripción: Predicado que obtiene el largo de una lista
largeList([], 0).
largeList([_|Lista], P) :- largeList(Lista, P1), P is P1 + 1.
% Descripción: Predicado que corta una lista dado un Num recibido
cutList(Lista, Num, R) :- var(Num), rotate(Lista, 0, R), !.
cutList(Lista, Num, R) :- largeList(Lista, P1), P1 = Num, rotate(Lista, 0, R), !.
cutList(Lista, Num, R) :- largeList(Lista, P1), P2 is P1 - (Num + 1), sublistaAux(Lista, P2, R).

% Dominio: Un cardsSet
% Descripción: Predicado que verifica si un conjunto de tartas corresponde a un conjunto válido
cardsSetIsDobble(CS):- oneElement(CS), sameN(CS).
oneElement([]).
oneElement([C|Cs]) :- forall(select(X,C,Xs),\+member(X,Xs)), forall(member(X,Cs),intersection(C,X,[_])), oneElement(Cs).
sameN([C|Cs]):- largeList(C, N), sameNAux(N, Cs), !.
sameNAux(N, [C1|Cs]):- largeList(C1, R1), N == R1, sameNAux(N, Cs).
sameNAux(_,[]).

% ---------------------------------SELECTORES---------------------------------------------------

% Dominio: Un cardsSet, un entero y un Card
% Descripción: Predicado que obtiene la carta que se encuentra en la posición del entero ingresado
cardsSetNthCard( [Cabeza|_], 0, Cabeza):- !.
cardsSetNthCard( [_|Cabeza], Posicion, Elemento) :- 
	PosicionAnterior is Posicion-1,
	cardsSetNthCard(Cabeza, PosicionAnterior, Elemento).

% Dominio: Un cardsSet y un entero
% Descripción: Predicado que a partir de una carta de muestra, determina la cantidad
% total de cartas que se deben producir para construir un conjunto válido
cardsSetFindTotalCards(CS, TC) :- largeList(CS, P), TC is ((P-1) * (P-1)) + (P-1) + 1.

% ---------------------------------------OTROS PREDICADOS------------------------------------------

% Dominio: Un cardsSet y un string
% Descripción: Predicado que convierte un conjunto de cartas a una representación basada en strings
cardsSetToString(CS, R) :- cardsAux(CS, 1, '', R).
cardsAux([],_,Cadena,Cadena).
cardsAux([X|Xs], Num, Cadena, R) :- number_string(Num, Number), string_concat(Number, ': ', X1), 
  string_concat('Carta ', X1, X2), Num2 is Num + 1, atomic_list_concat(X, ', ', X3), string_concat(X2, X3, X4), 
  string_concat(X4, '\n', X5), string_concat(Cadena, X5, Re), cardsAux(Xs, Num2, Re, R).

/*____________________________________________________________________________________________
TDA game
____________________________________________________________________________________________*/

% Predicados
% dobbleGame(numPlayers, cardsSet, GameMode, Seed, G) (aridad = 5)
% dobbleGameRegister(User, GIn, GOut) (aridad = 3)
% dobbleGameWhoseTurnIsIt(Game, UserName) (aridad = 2)
% dobbleGamePlay(GIn, Action, GOut) (aridad = 3)
% dobbleGameStatus(Game, Status) (aridad = 2)
% dobbleGameToString(Game, STR) (aridad = 2)

% Metas primarias: dobbleGame, dobbleGameRegister, cardsSetNthCard, dobbleGameWhoseTurnIsIt, 
% dobbleGamePlay, dobbleGameStatus, dobbleGameToString
% Metas secundarios: createUser

% ---------------------------REPRESENTACION-------------------------------------------------------

% El TDA Game se representa como una lista de que contiene el número de máximos jugadores, 
% la lista de jugadores en el juego, el modo de juego, una semilla de juego, el estado del juego,
% el set de cartas y el área de juego
% Dominio:
% Descripción: 
% --------------------------CONSTRUCTOR----------------------------------------------------------

% Dominio: Un entero, un set de cartas, un string que representa el modo de juego, un número entero,
% y un TDA game
% Descripción: Predicado que construye el TDA game en base a los parametros ingresados por el usuario  
dobbleGame(NumP, CS, GameMode, Seed, [NumP, [], GameMode, Seed, 'Jugando', CS, []]).

% Dominio: Un string, un TDA game de entrada y un TDA game de salida
% Descripción: Predicado que registra un jugador en un TDA game
dobbleGameRegister(User, [Gin,X,GM,Seed|Xs], Gout) :- largeList(X, R), R < Gin, createUser(User, UserOut), 
  not(member(UserOut, X)), append(X, [UserOut], X1), Seed1 is Seed + R, randomFn(Seed1, Num), 
  rotate(X1, Num, X2), rotate([Gin,X2,GM,Seed|Xs], 0, Gout), !.

createUser(UserName, [UserName, []]).

% ---------------------------------SELECTORES---------------------------------------------------

% Dominio: Un TDA game y un string
% Descripción: Predicado que obtiene el usuario a quién le corresponde jugar
dobbleGameWhoseTurnIsIt(Gin, User):- cardsSetNthCard(Gin, 1, G1), 
  cardsSetNthCard(G1, 0, G2), cardsSetNthCard(G2, 0, User).

% Dominio: Un TDA game y un String
% Descripción: Predicado que obtiene el estado actual de juego
dobbleGameStatus(Gin, Status):- cardsSetNthCard(Gin, 4, Status).

% Dominio: Un TDA game, un String y un entero
% Descripción: Predicado que obtiene el puntaje de un usuario ingresado
dobbleGameScore(Gin, User, Score):- var(Score), cardsSetNthCard(Gin, 1, X), dobbleGameScoreAux(X, User, Score).
dobbleGameScoreAux([User|_], User1, R):- cardsSetNthCard(User, 0, UserName), 
  atom_string(UserName, User2), User1 == User2, cardsSetNthCard(User, 1, Score), largeList(Score, R1), 
  R is R1 div 2, !.
dobbleGameScoreAux(Players, User, R):- rotate(Players, 1, R1), dobbleGameScoreAux(R1, User, R).

% ---------------------------------MODIFICADORES---------------------------------------------------

% Dominio: Un TDA game de entrada, una lista de tres strings, y un TDA game de salida 
% Descripción: Predicado que permite realizar una acción a partir de una acción especificada en el segundo argumento
dobbleGamePlay(Gin, Action, Gout):- cardsSetNthCard(Gin, 4, Status), Status \= 'Terminado', Action = null, null(Gin, Gout), !.
dobbleGamePlay(Gin, Action, Gout):- cardsSetNthCard(Gin, 4, Status), Status \= 'Terminado', Action = [pass], pass(Gin, Gout), !.
dobbleGamePlay(Gin, Action, Gout):- cardsSetNthCard(Gin, 4, Status), Status \= 'Terminado', Action = [finish], finish(Gin, Gout, R), write(R), !.
dobbleGamePlay(Gin, Action, Gout):- cardsSetNthCard(Gin, 4, Status), Status \= 'Terminado', is_list(Action),
  cardsSetNthCard(Action, 0, Ac), Ac = spotIt, spotIt(Gin, Action, Gout), !.

% Predicado que ejecuta la acción null
null([NumP, ListP, GameMode, Seed, Status, [C1,C2|CS], GameArea], [NumP, ListP, GameMode, Seed, Status, CS, [C1,C2|GameArea]]):- largeList(GameArea, R), R == 0.
% Predicado que ejecuta la acción pass
pass([NumP, ListP, GameMode, Seed, Status, CS, [C1,C2|GameArea]], [NumP, ListP, GameMode, Seed, Status, [C1,C2|CS], GameArea]):- largeList([C1,C2|GameArea], R), R == 2.
% Predicado que ejecuta la acción finish
finish([NumP, ListP, GameMode, Seed, _, CS, GameArea], [NumP, ListP, GameMode, Seed, 'Terminado', CS, GameArea], R):- allScore(ListP, '', R).
% Predicado que ejecuta la acción spotIt
spotIt([NumP, [P1|Ps], GameMode, Seed, Status, CS, [C1,C2|GameArea]], Action,[NumP, ListP, GameMode, Seed, Status, CS, [GameArea]]):-
  cardsSetNthCard(Action, 2, Element), intersection(C1, C2, C3), [Element] == C3, appendPoint(P1, C1, C2, P2), append(Ps, P2, ListP).
% Predicado que le agrega los puntos al jugador que posee el turno
appendPoint([P|_], C1, C2, [[P, [C1, C2]]]).

% ---------------------------------------OTROS PREDICADOS------------------------------------------

% Dominio: Un TDA game y un String
% Descripción: Predicado que convierte un TDA game a una representación en String
dobbleGameToString([Max, ListP, GameMode, _, Status, CS, GameArea], Str):-  
  number_string(Max, MaxStr), string_concat('Maximo Jugadores: ', MaxStr, R0), string_concat('\n', R0, R1), string_concat(R1, '\n', R2),
  playerToString(ListP, 1, '', R3), string_concat(R2, R3, R5),
  atom_string(GameMode, GameMstr), string_concat('Modo de Juego: ', GameMstr, Gstr), string_concat(R5, Gstr, R6), string_concat(R6, '\n', R7),
  atom_string(Status, StatusStr), string_concat('Estado de Juego: ', StatusStr, Sstr), string_concat(R7, Sstr, R8), string_concat(R8, '\n', R9),
  cardsSetToString(CS, SetStr), string_concat('Mazo de Cartas:\n', SetStr, MazoStr), string_concat(R9, MazoStr, R11),
  cardsToString(GameArea, '', AreaStr), string_concat('Area de Juego: ', AreaStr, GAstr), string_concat(R11, GAstr, Str).

% Predicado que convierte a un string los jugadores del juego y las cartas en su posesión
playerToString([],_,Cadena,Cadena).
playerToString([P|Ps], Num, Cadena, Str):- number_string(Num, NumStr), cardsSetNthCard(P, 0, User), cardsSetNthCard(P, 1, Cards), cardsToString(Cards, '', CardsStr),
  string_concat(Cadena, 'Jugador ', R1), string_concat(R1, NumStr, R2), string_concat(R2, ': ', R3), string_concat(R3, User, R4), string_concat(R4, '\n', R5),
  string_concat(R5, 'Cartas: ', R6), string_concat(R6, CardsStr, R7), Num2 is Num + 1, playerToString(Ps, Num2, R7, Str).

% Predicado que convierte las cartas en el set de juego a un string
cardsToString([], Cadena, R):- string_concat(Cadena, '\n', R), !.
cardsToString([X|Xs], Cadena, CardStr):- string_concat(Cadena, '[', R1), atomic_list_concat(X, ',', R2),
 string_concat(R1, R2, R3), string_concat(R3,'] ', R4), cardsToString(Xs, R4, CardStr).
