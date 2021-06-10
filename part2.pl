
grid(4,3).
dot('G',[3,1]).
dot('G',[2,2]).
solve:-
	dot(Char,Q),
	findall(X,dot(Char,X),Children),
	test2(Children,Char).

test2([[X|Y],[F|K]],Char):-
	grid(R,C),

 	test(R,C,X,Y,F,K,Char).
test(R,C,M,N,O,P,Char):-
Z is R*C,
Y is 0,
addinlist(Y,L,Z),
F is (M*C) + N  ,
change(F, L,  Start,Char),
Q is (O*C)+P,
change(Q,L,Goal,Char),
getHeuristic(Start, H, Goal),
path([[Start,null, 0, H, H]],[],Goal,Char).   


addinlist(_,[],0):-!.
addinlist(S,['-'|T],C):-
    S1 is S+1,
    C1 is C-1,
    addinlist(S1,T,C1).

change(Idx, L,  K,Char) :-
  nth0(Idx, L, _, R),
  nth0(Idx, K, Char, R).



substitute(_,[],_,[]):-!.
substitute(X,[X|T],Y,[Y|T1]):-
substitute(X,T,Y,T1),!.
substitute(X,[H|T],Y,[H|T1]):-
substitute(X,T,Y,T1),!.

move(S,Snew,Char):-
	up(S,Snew,Char).
move(S,Snew,Char):-
	left(S,Snew,Char).
move(S,Snew,Char):-
	down(S,Snew,Char).
move(S,Snew,Char):-
	right(S,Snew,Char).



right([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char):-
	 
	 \+ (R3 = Char),
	
	\+ (R6 = Char),

	\+ (R9 = Char),

	\+ (R12 = Char),
	blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char).

blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],S,Char):-
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Char),
	Z is N+1,
	change(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Q,'-'),
	change(Z,Q,S,Char).

up([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char):-
\+ (R1 = Char),
\+ (R2 = Char),
\+ (R3 = Char),

	blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char).

blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],S,Char):-
% get position of blank cell
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Char),

	Z is N-3,
	change(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Q,'-'),
	change(Z,Q,S,Char).

down([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char):-
\+ (R10 = Char),
\+ (R11 = Char),
\+ (R12 = Char),

	
	blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char).

blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],S,Char):-
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Char),
	Z is N+3,
	change(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Q,'-'),
	change(Z,Q,S,Char).





left([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char):-
	\+ (R1 = Char),
\+ (R4 = Char),
\+ (R7 = Char),
\+ (R10 = Char),
	blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Snew,Char).

blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],S,Char):-
	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Char),
	Z is N-1,
	change(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12],Q,'-'),
	change(Z,Q,S,Char).

path([], _, _,Char):-
		write('No solution'),nl,!.
path(Open, Closed, Goal,Char):-
		getBestChild(Open, [Goal, Parent, PC, H, TC], RestOfOpen),
		write('A solution is found'),  nl ,
		printsolution([Goal,Parent, PC, H, TC], Closed),!.
path(Open, Closed, Goal,Char):-
		getBestChild(Open, [State, Parent, PC, H, TC], RestOfOpen),
		%write('Best child chosen is '),write(State),write(' with TC= '),write(TC), nl,
		getchildren(State, Open, Closed, Children, PC, Goal,Char),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent, PC, H, TC] | Closed], Goal,Char).


getchildren(State, Open ,Closed , Children, PC, Goal,Char):-
		bagof(X, moves( State, Open, Closed, X, PC, Goal,Char), Children) .
getchildren(_,_,_, [],_,_,_).


addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-
	TC < TC1, !.
getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).


removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

moves( State, Open, Closed,[Next,State, NPC, H, TC], PC, Goal,Char):-
		move(State,Next,Char),
		\+ member([Next, _, _, _, _],Open),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
		getHeuristic(Next, H, Goal),
		TC is NPC + H.

getHeuristic([], 0, []):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).

getHeuristic([_|T1],H,[_|T2]):-
	getHeuristic(T1,TH, T2),
	H is TH + 1.

printsolution([State, null, PC, H, TC],_):-!,
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.
printsolution([State, Parent, PC, H, TC], Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.
