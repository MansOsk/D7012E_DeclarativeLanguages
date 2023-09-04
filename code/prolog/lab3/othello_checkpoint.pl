/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Måns Oskarsson
%    Student user id  : mnsosk-7
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */





% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 


% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.]]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(B, 1) :- initBoard(B). % Take list out of object.
initialize(B, 2) :- initBoard(B).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

%% IF player 1. Sum = Sum + 1
%% IF player 2. Sum = Sum - 1 

% IF No more moves (check helper function checkmoves), check if P1 or P2 has won.
% Count is stated below..
winner(State, Plyr) :-
		checkMoves(State),
		count(State, 0,0,0,0, P1 ,P2),
		(Plyr = 1, P1 < P2;
		 Plyr = 2, P2 < P1).

/*
   Counts resulting differnece of score.
	+1 if player 1 stone.
	-1 if player 2 stone.
	+0 if .

	Positive number means player 1 has more stones on the field.
	Positive number means player 2 is in lead.

	Negative number means player 2 has more stones on the field.
	Negative number means player 1 is in lead. 	

	If sum = 0 then tie. */

% Pattern matching on positional value of board.
% Handle 3 * 3 states.
% Collects sum of 1 or 2 appearing on board.

% Base case, at end of State, (Board).
% Write to output and return.
count(State,X,Y,R1,R2,Sum,R2) :-
		get(State, [X, Y], Value),
		Value = 1,
		X = 5,
		Y = 5,
		Sum is R1 + 1, !.
count(State,X,Y,R1,R2,R1,Sum2) :-
		get(State, [X, Y], Value),
		Value = 2,
		X = 5,
		Y = 5,
		Sum2 is R2 + 1, !.
count(State,X,Y,R1,R2,R1,R2) :-
		get(State, [X, Y], Value),
		Value \= 1,
		Value \= 2,
		X = 5,
		Y = 5, !.

% When not increment Y... X =< 5.
% Based on value, update R1 or R2. 
count(State,X,Y,R1,R2,Sum,Sum2) :-
		get(State, [X, Y], Value),
		Value = 1,
		X =< 4,
		X2 is X + 1,
		Res is R1 + 1,
		count(State,X2,Y,Res,R2,Sum,Sum2), !.
count(State,X,Y,R1,R2,Sum,Sum2):-
		get(State, [X, Y], Value),
		Value = 2,
		X =< 4,
		X2 is X + 1,
		Res is R2 + 1,
		count(State,X2,Y,R1,Res,Sum,Sum2), !.
count(State,X,Y,R1,R2,Sum,Sum2) :-
		get(State, [X, Y], Value),
		Value \= 1,
		Value \= 2,
		X =< 4,
		X2 is X + 1,
		count(State,X2,Y,R1,R2,Sum,Sum2), !.

% When need of increment Y.. Reset X to 0 !!!
% Do also write based on Value of last X (edge to right).
count(State,X,Y,R1,R2,Sum,Sum2) :-
		get(State, [X, Y], Value),
		Value = 1,
		X > 4,
		Y2 is Y + 1, 
		X2 is 0,
		Res is R1 + 1,
		count(State,X2,Y2,Res,R2,Sum,Sum2), !.
count(State,X,Y,R1,R2,Sum,Sum2) :-
		get(State, [X, Y], Value),
		Value = 2,
		X > 4,
		Y2 is Y + 1, 
		X2 is 0,
		Res is R2 + 1,
		count(State,X2,Y2,R1,Res,Sum,Sum2), !.
count(State,X,Y,R1,R2,Sum,Sum2) :-
		get(State, [X, Y], Value),
		Value \= 1,
		Value \= 2,
		X > 4,
		Y2 is Y + 1,
		X2 is 0,
		count(State,X2,Y2,R1,R2,Sum,Sum2), !.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- checkTie(State).

%Returns true if both have only pass as move left.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

% terminal(State) :-	

% Any moves left, call checkMove.
terminal(State) :- checkMoves(State), !.


% HELPER FUCNTIONS FOR TIE AND TERMINAL.
% Check tie check if score is the same for both player 1 and 2.
checkTie(State) :-
		checkMoves(State),
		count(State,0,0, 0, 0, P1, P2),
		(P1 = P2 -> true ; fail).
  
% If both empty from moves based on state, return true.
checkMoves(State) :- 
		moves(1, State, []), 
   		moves(2, State, []).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

% The general procedure of moves is that it check in all possible directions when the 
% Plyr appears on iteration over board. 
% If an opponent appears in next step for that specific direction, look for dot and return.
% Else return empty list. 
moves(Plyr, State, MvList) :- 
		helper(Plyr, State, 0, 0, [], MvList), !.

% Helper selects one point, go over board..
helper(Plyr, State, X, Y, Ans, Ans) :-
		Y > 5, !.

helper(Plyr, State, X, Y, Ans, Ans2) :-
		X =< 4,
		Y =< 5,
		get(State, [X, Y], Value),
		check_directions(Plyr, State, Value, Value, X, Y, Ans, Res),
		X2 is X + 1, 
		helper(Plyr, State, X2, Y, Res, Ans2), !.

helper(Plyr, State, X, Y, Ans, Ans2) :-
		X > 4,
		Y < 6,
		get(State, [X, Y], Value),
		check_directions(Plyr, State, Value, Value, X, Y, Ans, Res),
		X2 is 0, 
		Y2 is Y + 1,
		helper(Plyr, State, X2, Y2, Res, Ans2), !.


% Check all possible directions from point.
check_directions(Plyr, State, Start, 0, X, Y, Ans, Ans) :- !.
		
% 8 directions...
check_directions(Plyr, State, Start, Current, X, Y, Ans, Ans2) :-
		traverse(State, Plyr, Start, Current, X, Y, 0, -1, Ans, R1),
		traverse(State, Plyr, Start, Current, X, Y, 1, -1, R1, R2),
		traverse(State, Plyr, Start, Current, X, Y, 1,  0, R2, R3),
		traverse(State, Plyr, Start, Current, X, Y, 1,  1, R3, R4),
		traverse(State, Plyr, Start, Current, X, Y, 0,  1, R4, R5),
		traverse(State, Plyr, Start, Current, X, Y, -1, 1, R5, R6),
		traverse(State, Plyr, Start, Current, X, Y, -1, 0, R6, R7),
		traverse(State, Plyr, Start, Current, X, Y, -1, -1, R7, R),
		check_directions(Plyr, State, Start, 0, X, Y, R, Ans2), !.

%% Traverse checks next point and current, and possible out of bounds etc error handling.
traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		Y2 is Y + Dy,
		X2 is X + Dx,
		get(State, [X2, Y2], Value2),
		Value2 = '.', !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		Y2 is Y + Dy,
		X2 is X + Dx,
		not(get(State, [X2, Y2], _)), !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		Start \= 1, 
		Start \= 2, 
		Current \= 0, !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :- 
		Current = 0, !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :- 
		Start \= Plyr, Current \= 0, !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :- 
		Current = '.', !.

traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, R) :-
		Start = Plyr,
		Current \= 0,
		Y2 is Y + Dy,
		X2 is X + Dx,
		get(State, [X2, Y2], Value2),
		Current \= '.',
		(Value2 = 1; Value2 = 2; Value = '.'),
		find_end(State, Plyr, Start, Value2, X2, Y2, Dx, Dy, Ans, R1),
		traverse(State, Plyr, Start, 0, X2, Y2, Dx, Dy, R1, R), !.

% Walk over opponent markers until other action happens. 
% Dot = return cordinations (correct). 
% Player = Return empty list (false). 
% Out of bonds (false.) = return empty list (false)
% Opponent = Continue.

find_end(State,Plyr, Start, Current, X, Y, Dx, Dy, Ans, R1) :-
		Current \= 1,
		Current \= 2,
		(Start = 1 ; Start = 2),
		Current = '.',
		X2 is X,
		Y2 is Y,
		list_concat(Ans, [[X2,Y2]], R1), !.

find_end(State,Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		X2 is X + Dx, 
		Y2 is Y + Dy,
		not(get(State, [X2, Y2], _)), !.

% Out of grid..
find_end(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		Current \= 1,
		Current \= 2,
		Current \= '.', !.

% IF own player occurs, just return the previous results...
find_end(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		Current = Plyr, !.

find_end(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans) :-
		get(State, [X, Y], Value),
		Value \= 1,
		Value \= 2,
		Value \= '.', !.

find_end(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, R) :-
		Current \= Plyr,
		(Value = 1; Value = 2; Value = '.'), 
		(Start = 1 ; Start = 2),
		Current \= '.',
		X2 is X + Dx,
		Y2 is Y + Dy,
		get(State, [X2, Y2], Value),
		find_end(State, Plyr, Start, Value, X2, Y2, Dx, Dy, Ans, R),
		!.

empty_List(List) :- List = [].

% Simple list_concat used in base case for find_end... 
list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).

% check_directions(Plyr, State, Start, Current, X, Y, Ans, R)

%find_end(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, R)
%traverse(State, Plyr, Start, Current, X, Y, Dx, Dy, Ans, Ans)

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextState(2,n,State,State,1):- !.
nextState(1,n,State,State,2):- !.

nextState(2,Move,State,NewState,1):-
		set(State,NS,Move,2),					% Write inserted.
		setNextStone(NS,Move,2,NewState1,n),
 		setNextStone(NewState1,Move,2,NewState2,nw),
 		setNextStone(NewState2,Move,2,NewState3,ne),
 		setNextStone(NewState3,Move,2,NewState4,e),
 		setNextStone(NewState4,Move,2,NewState5,se),
 		setNextStone(NewState5,Move,2,NewState6,s),
 		setNextStone(NewState6,Move,2,NewState7,sw),
 		setNextStone(NewState7,Move,2,NewState,w).

nextState(1,Move,State,NewState,2):-
		set(State,NS,Move,1),					% Write inserted.
 		setNextStone(NS,Move,1,NewState1,n),
 		setNextStone(NewState1,Move,1,NewState2,nw),
 		setNextStone(NewState2,Move,1,NewState3,ne),
 		setNextStone(NewState3,Move,1,NewState4,e),
 		setNextStone(NewState4,Move,1,NewState5,se),
 		setNextStone(NewState5,Move,1,NewState6,s),
 		setNextStone(NewState6,Move,1,NewState7,sw),
 		setNextStone(NewState7,Move,1,NewState,w).

getDir([X,Y],Dr,D1,D2,D3):-
		X1 is X - 1,X2 is X + 1,Y1 is Y - 1,Y2 is Y + 1,
  		X3 is X - 2,X4 is X + 2,Y3 is Y - 2,Y4 is Y + 2,
 		(Dr = n  -> D1 = [X,Y1] , D2 = [X,Y3]  ,D3 = [0,-1];
  		Dr = nw -> D1 = [X1,Y1], D2 = [X3,Y3] ,D3 = [-1,-1];
 		Dr = ne -> D1 = [X2,Y1], D2 = [X4,Y3] ,D3 = [1,-1];
  		Dr = e  -> D1 = [X2,Y] , D2 = [X4,Y]  ,D3 = [1,0];
  		Dr = se -> D1 = [X2,Y2], D2 = [X4,Y4] ,D3 = [1,1];
  		Dr = s  -> D1 = [X,Y2] , D2 = [X,Y4]  ,D3 = [0,1];
  		Dr = sw -> D1 = [X1,Y2], D2 = [X3,Y4] ,D3 = [-1,1];
  		Dr = w  -> D1 = [X1,Y] , D2 = [X3,Y]  ,D3 = [-1,0]).

checkBoundary([X, Y]) :- X >= 0, X =< 5, Y >= 0, Y =< 5.

setNextStone(State,[X,Y],_,NewState,Dir):-
		getDir([X,Y],Dir,_,D2,_),
 		(\+checkBoundary(D2)),
 		NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
		getDir([X,Y],Dir,D1,D2,D3),
		checkBoundary(D1),
		checkBoundary(D2),
		get(State, D1, Value1),
		get(State, D2, Value2),
		Value1 \= Value2,
		Plyr == Value2,
 		Value1 \= '.',
 		set(State, NewState1, D1, Plyr),
 		set(NewState1, NewState2, D1, Plyr),
 		D3 = [X1,X2],
 		Dir2 = [-X1,-X2],
 		setStonesBack(NewState2,NewState,D1,Plyr,Dir2).

setNextStone(State,[X,Y],_,NewState,Dir):-
 		getDir([X,Y],Dir,D1,D2,_),
 		checkBoundary(D1),
 		checkBoundary(D2),
 		get(State, D1, Value1),
 		get(State, D2, _),
 		Value1 == '.',
 		NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 		getDir([X,Y],Dir,D1,D2,_),
 		checkBoundary(D1),
 		checkBoundary(D2),
 		get(State, D1, Value1),
 		get(State, D2, _),
 		Value1 == Plyr,
 		NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 		getDir([X,Y],Dir,D1,D2,_),
 		checkBoundary(D1),
 		checkBoundary(D2),
 		get(State, D1, Value1),
 		get(State, D2, Value2),
 		Value1 \= Plyr,
 		Value2 = '.',
 		NewState = State.

setNextStone(State,[X,Y],Plyr,NewState,Dir):-
 		getDir([X,Y],Dir,D1,D2,_),
 		checkBoundary(D1),
 		checkBoundary(D2),
 		get(State, D1, Value1),
 		get(State, D2, Value2),
 		Value1 == Value2,
 		Plyr \= Value2,
 		Value1 \= '.',
 		setNextStone(State,D1,Plyr,NewState,Dir).


setStonesBack(State,State,[X1,Y1],Plyr,[X2,Y2]):-
 		X is X1+X2, 
 		Y is Y1+Y2,
 		get(State,[X,Y], P1),
 		P1 == Plyr.

setStonesBack(State,NewState,[X1,Y1],Plyr,[X2,Y2]):-
 		X is (X1+X2), 
 		Y is (Y1+Y2),
 		set(State, NewState1 , [X, Y], Plyr),
 		setStonesBack(NewState1,NewState,[X,Y],Plyr,[X2,Y2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr,State,n):- 
		moves(Plyr, State, MvList),  MvList = [], !. 
validmove(Plyr,State,Proposed):-
		moves(Plyr,State,MvList),
 		member(Proposed,MvList), !.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State,2000) :- winner(State,1), !.
h(State,-2000) :- winner(State,2), !.
h(State,0) :- tie(State), !.

h(State,Val):- get(State,[0,0],Elem1),Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 800 + (P2 - P1).

h(State,Val):- get(State,[0,0],Elem1),get(State,[0,5],Elem2),Elem2 == 2 ,Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 1000 + (P2 - P1).
h(State,Val):- get(State,[0,0],Elem1),get(State,[5,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 1000 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,5],Elem2),Elem2 == 2 ,Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 1000 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[5,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 1000 + (P2 - P1).
h(State,Val):- get(State,[5,5],Elem1),get(State,[0,0],Elem2),Elem2 == 2 ,Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 1200 + (P2 - P1).

h(State,Val):-get(State,[0,5],Elem1),Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 800 + (P2 - P1).

h(State,Val):-get(State,[5,0],Elem1),Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 800 + (P2 - P1).

h(State,Val):-get(State,[5,5],Elem1),Elem1 == 2 ,count(State,0,0, 0, 0, P1, P2),Val is 800 + (P2 - P1).


h(State, Val) :-
	count(State,0,0, 0, 0, P1, P2), Val is (P2 - P1).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-2001).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(2001).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
