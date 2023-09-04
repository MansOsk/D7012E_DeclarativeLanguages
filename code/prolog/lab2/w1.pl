/* 1.01 */
my_last(X,[X]).
my_last(X,[_|L]) :- my_last(X,L).

/* 1.02 */
my_last2(X,[X,Y]).
my_last2(X,[_|L]) :- my_last2(X,L).

/* ALTERNATIVE SOLUTION */
last_but_one(X,[X,_]).
last_but_one(X,[_,Y|Ys]) :- last_but_one(X,[Y|Ys]).

/* 1.03 */
element_at(X,[X|_], 1).
element_at(X,[_|L],K) :-
    K > 1,
    Kl is K - 1,
    element_at(X,L,Kl).

/* 1.04 */
length([_:X],K).
length([_:L],K) :-
    Kl is K + 1,
    length(L,Kl).
    
/* 1.05 */
my_reverse(L1,L2) :- my_rev(L1,L2,[]).
my_rev([],L2,L2) :- !.
my_rev([X|Xs],L2,Acc) :- my_rev(Xs,L2,[X|Acc]).

/* 1.06 */
is_palindrome(L) :- reverse(L,L).

/* 1.08 */
compress([],[]).
compress([X],[X]).
compress([X,X|Xs],Zs) :- compress([X|Xs],Zs).
compress([X,Y|Ys],[X|Zs]) :- X \= Y, compress([Y|Ys],Zs).

/* 1.09 */
/*
If a list contains repeated elements they should be placed in separate sublists.

Example:
?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
*/
pack([],[]), !.
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

/* 1.10 */

% Use the result of problem 1.09 to implement the so-called 
% run-length encoding data compression method. 
% Consecutive duplicates of elements are encoded as terms [N,E] 
%       where N is the number of duplicates of the element E.

% Example:
% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]

%  1. Count amount of a.
%  2. Return when next letter.
%  3. Sublist items, iterate over sublist.
/*
encode([], []), !.
encode(L, Z) :-
    pack(L, X), % out = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
    iter(X, Z).

iter([], []), !.
iter([X | Xs], [L | R]) :-
    helper(X, L),
    iter(Xs, R).

helper([X], [1,X]).
helper([X | Xs], [N,Y]) :-
    helper(Xs, [N1,Y]),
    N is N1 + 1.
*/
/* Alternative solution */
encode(L1,L2) :- pack(L1,L), transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]) :- length([X|Xs],N), transform(Ys,Zs).

/* 1.12 */
decode([],[]), !.
decode(L, Z) :-
    encode(Z,L).

/* 1.14 */
/*
Example:
?- dupli([a,b,c,c,d],X).
X = [a,a,b,b,c,c,c,c,d,d]
*/

dupli([],[]).
dupli([L1| Ls],[L1, L1 | Rs]) :-
    dupli(Ls, Rs). % Tack hector <3

/*
1.15 (**) Duplicate the elements of a list a given number of times.
Example:
?- dupli([a,b,c],3,X).
X = [a,a,a,b,b,b,c,c,c]

What are the results of the goal:
?- dupli(X,3,Y).
*/

list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).

dupli([],_,[]) :- !.
dupli([L | Ls],N,L2) :- 
    dupli_h(L, N,R),
    dupli(Ls,N,L1),
    list_concat(R, L1, L2).

dupli_h(_, 0, []) :- !.
dupli_h(X, N, [X | Rs]) :-
    N1 is N - 1,
    dupli_h(X, N1, Rs).