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


% the shortest path example

s(s, e, 2). s(e, s, 2).
s(e, f, 5). s(f, e, 5).
s(f, g, 2). s(g, f, 2).
s(g, t, 2). s(t, g, 2).
s(t, d, 3). s(d, t, 3).
s(d, c, 3). s(c, d, 3).
s(c, b, 2). s(b, c, 2).
s(b, a, 2). s(a, b, 2).
s(a, s, 2). s(s, a, 2).


h(s, 6).
h(a, 5).
h(b, 4).
h(c, 4).
h(d, 3).
h(t, 0).
h(g, 2).
h(f, 4).
h(e, 7).


goal(X) :- X = t.  % t is the goal node
