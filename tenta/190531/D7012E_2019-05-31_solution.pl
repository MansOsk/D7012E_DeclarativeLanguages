% Suggested solution 
% Declarative languages D7012E 2019-05-31
% /Håkan Jonsson


% problem 5

% version 1 with the merge and unique in separate steps
mergeUnq(L1,L2,A) :-
    mergeUnq2(L1,L2,R),
    unique(R,A),!.

mergeUnq2([],L,L).
mergeUnq2(L,[],L).
mergeUnq2([H|T1],[H|T2],L) :-
    mergeUnq2(T1,[H|T2],L), !.
mergeUnq2([H1|L1],[H2|L2],[H1|L]) :-
    H1 =< H2,
    mergeUnq2(L1,[H2|L2],L), !.
mergeUnq2([H1|L1],[H2|L2],[H2|L]) :-
    mergeUnq2([H1|L1],L2,L), !.

unique([],[]).  % to remove duplicates
unique([A],[A]).
unique([H, H | T], L) :-
    write('unique arg 3\n'),
    write(H  + T +'\n'),
    unique([H|T], L).
unique([H|T],[H|L]) :-
    write('unique arg 4\n'),
    write(H + T +'\n'),
    unique(T,L).

% other versions

% merge and unique in one procedure

mergeUnq3([],L,L).
mergeUnq3(L,[],L).
mergeUnq3([H|T1],[H|T2],L) :- mergeUnq3(T1,[H|T2],L),!.
mergeUnq3([H,H|T1],L2,L) :-   mergeUnq([H|T1],L2,L). % to remove duplicates
mergeUnq3(L1,[H,H|T2],L) :-   mergeUnq(L1,[H|T2],L). % to remove duplicates
mergeUnq3([H1|T1],[H2|T2],[H1|L]) :- H1 =< H2, mergeUnq3(T1,[H2|T2],L),!.
mergeUnq3([H1|T1],[H2|T2],[H2|L]) :-           mergeUnq3([H1|T1],T2,L),!.

% concatenate and sort without keeping duplicates

mergeUnq4(L1,L2,L) :-
    conc(L1,L2,L3),
    sort2(L3,L),!.

conc([],L,L).
conc([X|L1],L2,[X|L3]) :- conc(L1,L2,L3).

sort2([],[]).
sort2([H|T],L) :-
    sort2(T,L2),
    ins(H,L2,L).

ins(X,[],[X]).
ins(X,[X|T],[X|T]). % remove duplicates
ins(X,[H|T],[X,H|T]) :- X<H,!.
ins(X,[H|T],[H|L]) :- ins(X,T,L).

% using setof

mergeUnq5(L1,L2,L) :-
    setof(X,(member(X,L1);member(X,L2)),L).

%

mergeUnq6([],L1,L1).
mergeUnq6([H|T], L2, L) :-
    insertUnq(H, L2, L3),
    mergeUnq6(T, L3, L).

insertUnq(X,[],[X]) :- !.
insertUnq(X, [H|T], R) :-
    X<H,
    conc([X], [H|T], R), !.
insertUnq(X, [X|T], [X|T]) :- !.
insertUnq(X, [H|T], [H|R]) :-
    insertUnq(X, T, R).

% problem 6a

swap(t(A,B),t(B,A)).

% other versions

fst(t(A,_),A).
snd(t(_,B),B).
swap2(T1,T2) :-
    fst(T1,F1),
    snd(T2,S2),
    F1=S2,
    snd(T1,S1),
    fst(T2,F2),
    S1=F2.

% proble 6b

swapAll([],[]).
swapAll([H|T],[S|L]) :-
    swap(H,S),
    swapAll(T,L).

% other versions

swapAll2(L1, L2) :- findall(t(A,B), (member(X,L1),swap(X,t(A,B))), L2).

% problem 7

% not asked for in the problem

p(A,B) :- A.
p(A,B) :- B, !, fail.

run :- T = [pair(true,true),pair(true,false),pair(false,true),pair(false,false)], run2(T).
run2([]).
run2([pair(A,B)|T]) :- p(A,B), !,
    write('a='), write(A), write('\tb='), write(B), writeln('\t=> true'), run2(T).
run2([pair(A,B)|T]) :- !,
    write('a='), write(A), write('\tb='), write(B), writeln('\t=> false'), run2(T).

%% p :- a.
%% p :- b.

%% a=true  b=true  => true
%% a=true  b=false => true
%% a=false b=true  => true
%% a=false b=false => false


%% p :- a, !.
%% p :- b.

%% a=true  b=true  => true
%% a=true  b=false => true
%% a=false b=true  => true
%% a=false b=false => false

%% p :- a, !, fail.
%% p :- b.

%% a=true  b=true  => false
%% a=true  b=false => false
%% a=false b=true  => true
%% a=false b=false => false

%% p :- a.
%% p :- b, !, fail.

%% a=true  b=true =>  true
%% a=true  b=false => true
%% a=false b=true =>  false
%% a=false b=false => false


% problem 8

del(X,[X|T],T).
del(X,[Y|T],[Y|T2]) :- del(X,T,T2).

%heap(X) :- X=[t(1,2),t(3,4),t(4,5),t(2,3)]. % ,t(,),t(,),t(,),t(,),t(,),t(,)].
heap(X) :- X=[t(6,5),t(5,4),t(4,3),t(3,2),t(2,1)].
%heap(X) :- X=[t(3,2),t(2,6),t(1,3),t(6,4),t(4,5)].

test(S,E) :- heap(L), path4(S,E,L).

% first try to reach E, then E swapped
path(S,E,L) :- pathHelper(S,E,L); swap(E,E2),pathHelper(S,E2,L).

% if S=t(_,X), E=t(X,_), and both are in L, we are done
pathHelper(t(F1,X),t(X,T2),L) :- 
    del(t(F1,X),L,L2),
    del(t(X,T2),L2,_),!.

% otherwise first try to reach E from S...
pathHelper(t(F1,F2),E,L) :-
    del(t(F1,F2),L,L2),  % take a first step
    member(t(F2,X),L2),  % find a continuation tile
    pathHelper(t(F2,X),E,L2). % probe further from the found tile

% ... then try to reach E from S swapped 
pathHelper(T,E,L) :-
    swap(T,t(F1,F2)),
    del(t(F1,F2),L,L2),  % take a first step
    member(t(F2,X),L2),  % find a continuation tile
    pathHelper(t(F2,X),E,L2). % probe further from the found tile

% other versions

path2(t(A,B), E, L) :-
    pathH(A, E, L); pathH(B, E, L).

pathH(X, t(A,B), _) :-
    X == A; X ==B, !.
pathH(X, E, L) :-
    del(t(A,B), L, L2),
    ( (X==A, pathH(B, E, L2)); (X==B, pathH(A, E, L2))), !. 

