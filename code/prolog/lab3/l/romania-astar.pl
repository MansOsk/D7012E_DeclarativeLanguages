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


% road map of romania
% facts to find the shortest route
% from arad to bucharest with the A*
% algorithm

goal(bucharest).

%  -->
s(oradea,zerind,71).
s(oradea,sibiu,151).
s(zerind,arad,75).
s(arad,sibiu,140).
s(arad,timisoara,118).
s(timisoara,lugoj,111).
s(lugoj,mehadia,70).
s(mehadia,dobreta,75).
s(dobreta,craiova,120).
s(sibiu,rimnicu,80).
s(sibiu,fagaras,99).
s(rimnicu,craiova,146).
s(rimnicu,pitesti,97).
s(craiova,pitesti,138).
s(fagaras,bucharest,211).
s(pitesti,bucharest,101).
s(giurgiu,bucharest,90).
s(bucharest,urziceni,85).
s(neamt,iasi,87).
s(urziceni,vaslui,142).
s(urziceni,hirsova,98).
s(iasi,vaslui,92).
s(hirsova,eforie,86).

%  <--
s(zerind,oradea,71).
s(sibiu,oradea,151).
s(arad,zerind,75).
s(sibiu,arad,140).
s(timisoara,arad,118).
s(lugoj,timisoara,111).
s(mehadia,lugoj,70).
s(dobreta,mehadia,75).
s(craiova,dobreta,120).
s(rimnicu,sibiu,80).
s(fagaras,sibiu,99).
s(craiova,rimnicu,146).
s(pitesti,rimnicu,97).
s(pitesti,craiova,138).
s(bucharest,fagaras,211).
s(bucharest,pitesti,101).
s(bucharest,giurgiu,90).
s(urziceni,bucharest,85).
s(iasi,neamt,87).
s(vaslui,urziceni,142).
s(hirsova,urziceni,98).
s(vaslui,iasi,92).
s(eforie,hirsova,86).

h(oradea,380).
h(zerind,374).
h(arad,366).
h(timisoara,329).
h(lugoj,244).
h(mehadia,241).
h(dobreta,242).
h(sibiu,253).
h(rimnicu,193).
h(craiova,160).
h(fagaras,178).
h(pitesti,98).
h(giurgiu,77).
h(bucharest,0).
h(neamt,234).
h(urziceni,80).
h(iasi,226).
h(vaslui,199).
h(hirsova,151).
h(eforie,161).

