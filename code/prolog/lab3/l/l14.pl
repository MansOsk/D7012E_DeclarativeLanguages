% Example of how the Prolog database can be extended

maketable :- 
    L = [1,2,3,4,5,6,7,8,9],
    member( X, L), member( Y, L),
    Z is X * Y, asserta( product( X, Y, Z)), fail.


% The built-in procedures bagof, setof, findall

age( peter, 7).
age( ann, 5).
age( pat, 8).
age( tom, 5).

% bagof( Child, age( Child, 5), List).
% bagof( Child, age( Child, Age), List). % reload before this is tested
% bagof( Child, Age ^ age( Child, Age), List).

% setof( Child, Age ^ age( Child, Age), ChildList),setof( Age, Child ^ age( Child, Age), AgeList).
% bagof( Child, Age ^ age( Child, Age), ChildList),bagof( Age, Child ^ age( Child, Age), AgeList).
% setof( Age/Child, age( Child, Age), List). 

numList(F, L, []) :- L < F,!.
numList(F, L, [F | RA]) :-
    NextF is F + 1,
    numList(NextF , L, RA).

%% ?- numlist(1,8,NL),member(X,NL),0 =:= mod(X,2).
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% X = 2 ;
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% X = 4 ;
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% X = 6 ;
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% X = 8.


%% findall(X, (numlist(1,8,NL),member(X,NL),0 =:= mod(X,2)) ,L).
%% L = [2, 4, 6, 8].

%% Find all pairs that share divisors:
%%
%% findall(X-Y, (numlist(1,8,NL),member(X,NL),member(Y,NL),X>Y,Y =\=1, 0 =:= mod(X,Y)), L).
%% L = [4-2, 6-2, 6-3, 8-2, 8-4].

%% Skip Y:
%%
%% bagof(X, Y^(numlist(1,8,NL),member(X,NL),member(Y,NL),X>Y,Y =\=1, 0 =:= mod(X,Y)), L).
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% L = [4, 6, 6, 8, 8].

%% No repetition:
%%
%% setof(X, Y^(numlist(1,8,NL),member(X,NL),member(Y,NL),X>Y,Y =\=1, 0 =:= mod(X,Y)), L).
%% NL = [1, 2, 3, 4, 5, 6, 7, 8],
%% L = [4, 6, 8].

child(martha,charlotte). % charlotte is a child of martha
child(charlotte,caroline).
child(caroline,laura).
child(laura,rose).
child(laura,anna).
child(charlotte,maria).
child(rose,amanda).
 
descend(X,Y) :-
    child(X,Y).          
descend(X,Y) :-
    child(X,Z),          
    descend(Z,Y).

%% findall(X,descend(martha,X),Z).
%% Z = [charlotte, caroline, maria, laura, rose, anna, amanda].

%% findall(X,descend(mary,X),Z).
%% Z = [].

%% findall(Y,descend(martha,X),Z), length(Z,N). % Y is not in the goal
%% Z = [_6148, _6142, _6136, _6130, _6124, _6118, _6112],
%% N = 7.


% 5. Implicit iterative deepening

member(X, [X| _]).
member(X, [_| T]) :- member(X, T).

del(X, [X|L],L).
del(X,[Y|L],[Y|L1]) :- del(X,L,L1).

s(Stacks, [Stack1, [Top | Stack2] | OtherStacks]) :-
  del([Top | Stack1], Stacks, Stacks1),
  del(Stack2, Stacks1, OtherStacks).

goal(Situation) :- member([a,b,c],Situation).

path(Node, Node, [Node]). %single node
path(FirstNode, LastNode, [LastNode|Path]):-
  path(FirstNode, Penultimate, Path), % "penultimate" means "nÃ¤st sista" in Swedish
  s(Penultimate, LastNode),
  not(member(LastNode, Path)).

solve5(Node, Solution):-
  path(Node, GoalNode, Solution),
  goal(GoalNode).%,!. % adding a cut here causes solve5 to return just one solution

% solve5([[c,a,b],[],[]],Solution).



% Breadth-first search

% solveB( Start, Solution):
%    Solution is a path (in reverse order) from Start to a goal

solveB( Start, Solution)  :-
  breadthfirst( [ [Start] ], Solution).%,!. % adding a cut here causes solveB to return just one solution

% breadthfirst( [ Path1, Path2, ...], Solution):
%   Solution is an extension to a goal of one of paths

breadthfirst( [ [Node | Path] | _], [Node | Path])  :-
  goal( Node).

breadthfirst( [Path | Paths], Solution)  :-
  extend( Path, NewPaths),
  conc( Paths, NewPaths, Paths1),
  breadthfirst( Paths1, Solution).

extend( [Node | Path], NewPaths)  :-
  bagof( [NewNode, Node | Path],
         ( s( Node, NewNode), not(member( NewNode, [Node | Path]))),
         NewPaths), !.

extend( _, [] ).              % bagof failed: Node has no successor

conc( [], L, L).
conc( [X| L1], L2, [X| L3]) :- conc( L1, L2, L3).

% solveB([[c,a,b],[],[]],Solution).