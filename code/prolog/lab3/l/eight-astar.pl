astar(Start, Path) :-
    bestfirst(Start, Path2),
    reverse(Path2, Path).

bestfirst( Start, Solution) :-
  expand( [], l( Start, 0/0),  9999, _, yes, Solution).

expand( P, l( N, _), _, _, yes, [N|P])  :-   goal(N).
expand( P, l(N,F/G), Bound, Tree1, Solved, Sol)  :-
  F  =<  Bound,
  (  bagof( M/C, ( s(N,M,C), not(member(M,P))), Succ), 
     !,                                
     succlist( G, Succ, Ts),             
     bestf( Ts, F1),                   
     expand( P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol)
     ;
     Solved = never                      
  ) .
expand( P, t(N,F/G,[T|Ts]), Bound, Tree1, Solved, Sol)  :-
  F  =<  Bound,
  bestf( Ts, BF), min( Bound, BF, Bound1),        
  expand( [N|P], T, Bound1, T1, Solved1, Sol),
  continue( P, t(N,F/G,[T1|Ts]), Bound, Tree1, Solved1, Solved, Sol).
expand( _, t(_,_,[]), _, _, never, _) :- !.
expand( _, Tree, Bound, Tree, no, _)  :-
  f( Tree, F), F > Bound.

continue( _, _, _, _, yes, yes, Sol).
continue( P, t(N,F/G,[T1|Ts]), Bound, Tree1, no, Solved, Sol)  :-
  insert( T1, Ts, NTs),
  bestf( NTs, F1),
  expand( P, t(N,F1/G,NTs), Bound, Tree1, Solved, Sol).
continue( P, t(N,F/G,[_|Ts]), Bound, Tree1, never, Solved, Sol)  :-
  bestf( Ts, F1),
  expand( P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol).

succlist( _, [], []).
succlist( G0, [N/C | NCs], Ts)  :-
  G is G0 + C,
  h( N, H),                   
  F is G + H,
  succlist( G0, NCs, Ts1),
  insert( l(N,F/G), Ts1, Ts).

insert( T, Ts, [T | Ts])  :-
  f( T, F), bestf( Ts, F1),
  F  =<  F1, !.
insert( T, [T1 | Ts], [T1 | Ts1])  :-
  insert( T, Ts, Ts1).

f( l(_,F/_), F).      
f( t(_,F/_,_), F).  
bestf( [T|_], F)  :-  
  f( T, F).
bestf( [], 9999).    
 
min( X, Y, X)  :-  X  =<  Y, !.
min( X, Y, Y).

% ------


s( [Empty | Tiles], [Tile | Tiles1], 1)  :- 
  swap( Empty, Tile, Tiles, Tiles1).        

swap( Empty, Tile, [Tile | Ts], [Empty | Ts] )  :-
  mandist( Empty, Tile, 1).                
swap( Empty, Tile, [T1 | Ts], [T1 | Ts1] )  :-
  swap( Empty, Tile, Ts, Ts1).

mandist( X/Y, X1/Y1, D)  :-    
  dif( X, X1, Dx),
  dif( Y, Y1, Dy),
  D is Dx + Dy.

dif( A, B, D)  :-          
  D is A-B, D >= 0, !
  ;
  D is B-A.

h( [Empty | Tiles], H)  :-
  goal( [Empty1 | GoalSquares] ),
  totdist( Tiles, GoalSquares, D),     
  seq( Tiles, S),                   
  H is D + 3*S.

totdist( [], [], 0).
totdist( [Tile | Tiles], [Square | Squares], D)  :-
  mandist( Tile, Square, D1),
  totdist( Tiles, Squares, D2),
  D is D1 + D2.

seq( [First | OtherTiles], S)  :-
  seq( [First | OtherTiles ], First, S).
seq( [Tile1, Tile2 | Tiles], First, S)  :-
  score( Tile1, Tile2, S1),
  seq( [Tile2 | Tiles], First, S2),
  S is S1 + S2.
seq( [Last], First, S)  :-
  score( Last, First, S).

score( 2/2, _, 1)  :-  !.       
score( 1/3, 2/3, 0)  :-  !.        
score( 2/3, 3/3, 0)  :-  !.
score( 3/3, 3/2, 0)  :-  !.
score( 3/2, 3/1, 0)  :-  !.
score( 3/1, 2/1, 0)  :-  !.
score( 2/1, 1/1, 0)  :-  !.
score( 1/1, 1/2, 0)  :-  !.
score( 1/2, 1/3, 0)  :-  !.
score( _, _, 2).                 

goal( [2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2] ). 

showsol( [] ).
showsol( [P | L] )  :-
  showsol( L),
  nl, write( '---'),
  showpos( P).

showpos( [S0,S1,S2,S3,S4,S5,S6,S7,S8] )  :-
  member( Y, [3,2,1] ),               
  nl, member( X, [1,2,3] ),        
  member( Tile-X/Y,             
          [' '-S0,1-S1,2-S2,3-S3,4-S4,5-S5,6-S6,7-S7,8-S8] ),
  write( Tile),
  fail                     
  ;
  true.      

% astar( [2/2,1/3,3/2,2/3,3/3,3/1,2/1,1/1,1/2], Sol), showsol( Sol).  % Requires 4 steps
% astar( [2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3], Sol), showsol( Sol).  % Requires 5 steps
% astar( [2/2,2/3,1/3,3/1,1/2,2/1,3/3,1/1,3/2], Sol), showsol( Sol).  % Requires 18 steps
% astar( [1/3,3/3,1/2,3/2,2/2,3/1,2/1,1/1,2/3], Sol), showsol( Sol).  % Requires 8 steps

% astar( Pos, Sol), showsol( Sol).  %  print the solution when Pos is the start

