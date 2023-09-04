% Suggested solutions Prolog D7012E 2020-08-19 /Håkan Jonsson, LTU

select1(L,A,B) :-
    member(A,L),
    member(B,L),
    A<B.

%% ?- select1([1,2,3,4],A,B).
%% A = 1,
%% B = 2 ;
%% A = 1,
%% B = 3 ;
%% A = 1,
%% B = 4 ;
%% A = 2,
%% B = 3 ;
%% A = 2,
%% B = 4 ;
%% A = 3,
%% B = 4 ;
%% false.

%% ?-
    

select2(L,A,B) :-
    member(A,L),
    !,
    member(B,L), write(A), write(" "), writeln(B), 
    A<B.

%% ?- select2([1,2,3,4],A,B).
%% A = 1,
%% B = 2 ;
%% A = 1,
%% B = 3 ;
%% A = 1,
%% B = 4.

%% ?-


select3(L,A,B) :-
    member(A,L),
    member(B,L),
    !,
    A<B.


%% ?- select3([1,2,3,4],A,B).
%% false.

%% ?-

% -----------------------------------------------------

% using built-in SWI-Prolog predicates abs and sign:
generate1(N,[]) :- N < 1,!.
generate1(N,L) :- generate1h(N,1,L),!.

generate1h(N, I, [I]) :- abs(N) =:= abs(I).
generate1h(N, I, [I | L2]) :-
    I2 is (abs(I) + 1) * sign(I) * (-1),
    generate1h(N,I2,L2).


% this version below with DIY abs and sign (these were not permitted on the exam)
abz(X,A) :- X =< 0 -> A is -X,!; A is X.
zign(X,S) :- X < 0 -> S is -1, !;
	     X =:= 0 -> S is 0, !;
	     S is 1.

generate2(N,[]) :- N < 1,!.
generate2(N,L) :- generate2h(N,1,L),!.

generate2h(N, I, [I]) :-
    abz(N,N1),
    abz(I, I2),
    N1 =:= I2.
generate2h(N, I, [I | L2]) :-
    abz(I,AI),
    zign(I,SI),
    I2 is (AI + 1) * SI * (-1),
    generate2h(N,I2,L2).

%% ?- true.

%% ?- generate1(7,L).
%% L = [1, -2, 3, -4, 5, -6, 7].

%% ?- generate2(8,L).
%% L = [1, -2, 3, -4, 5, -6, 7, -8].

%% ?- 

% -----------------------------------------------------

makeTwoPiles([],[],[]).
makeTwoPiles([H|T],[H|L],R) :- makeTwoPiles(T,L,R). % alt 1: insert into L, or
makeTwoPiles([H|T],L,[H|R]) :- makeTwoPiles(T,L,R). % alt 2: insert into R

%% ?- makeTwoPiles([1,2,3],L,R).
%% L = [1, 2, 3],
%% R = [] ;
%% L = [1, 2],
%% R = [3] ;
%% L = [1, 3],
%% R = [2] ;
%% L = [1],
%% R = [2, 3] ;
%% L = [2, 3],
%% R = [1] ;
%% L = [2],
%% R = [1, 3] ;
%% L = [3],
%% R = [1, 2] ;
%% L = [],
%% R = [1, 2, 3].

%% ?-
    
sumlist([],0).
sumlist([H|T],Sum) :- sumlist(T,Sum2), Sum is H + Sum2.

partition(L,S1,S2) :-
    makeTwoPiles(L,S1,S2), % distribute elements of L into S1 and S2,
    sumlist(S1,S),         % compute sum of S1 as S (this instantiates/assignes S)
    sumlist(S2,S),         % true iff the sum of S2 is S (assigned on previous line)
    !.                     % one solution is enough

%% ?- partition([2,3,4,5],S1,S2).
%% S1 = [2, 5],
%% S2 = [3, 4].

%% ?- partition([1,2,3,4,5,6,7,8],S1,S2).
%% false.

%% ?- partition([1,2,3,4,5,6,7,8],S1,S2).
%% S1 = [1, 2, 3, 4, 8],
%% S2 = [5, 6, 7].

%% ?- 
