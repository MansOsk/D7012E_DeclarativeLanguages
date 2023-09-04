/* EXECUTION OF PROGRAM.
 1. Create tuples with (size, i, j, [xi,xi+1,...,xj])
        Where size is the sum of addition of elements i to j.

 2. Sort all tuples base on size.
        Thus size is a weight.

 3. Take smallest k.
        Select k amount of tuples from sorted, smallest size. 

 4. Print all k amount of tuples selected.

*/

/*
    Tuples are not used in Prolog.
*/

/*
    IN: List with size L
    OUT: Sublists with size L.

    IN: List with size L-1
    Out: Sublists with size L-1.

    ...
    ...
    ...

    IN: List with size 1 or 0.
    OUT: []  = Base case
*/

/*
    Generate sublists from end. 
    Cutlast when using this in recursion. 
*/

%%%%%%%%%%%%%%%%%%%Sublist%%%%%%%%%%%%%%%%%%%%%%%%%

generateList([],R,R,_,_).       
generateList(List,R,NewList,I,J) :-
    List = [_|T],
    sumlist(List, Sum),
    Sub = sub(Sum,I,J,List),
    I2 is I + 1,
    generateList(T,[Sub|R],NewList,I2,J).

cutlast([H, H2 | T], [H |T2]) :- cutlast([H2 | T], T2).
cutlast([_], []).

sublist_helper([],R,R). 
sublist_helper(L,R,SubL) :-
    length(L,J),
    generateList(L,[],X,1,J), % Generate sublists from end of list.
    cutlast(L, Les),  % remove last element, call recursive again.
    append(X,R,Res),   % append resulting sublist to R.
    sublist_helper(Les,Res,SubL). % Recusrive call where last element of list is removed.

%%%%%%%%%%%%%%%%%%%%%%%%%Sort%%%%%%%%%%%%%%%%%%%%%%%%%%
%   https://stackoverflow.com/questions/12715293/prolog-insertion-sort
%   Insert must be rewritten since sub(...) is a relation...

insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]) :-
    X = sub(Sum,_,_,_),             % Database lookup
    X1 = sub(Sum2,_,_,_),           % Database lookup
    Sum =< Sum2, !.
insert(X, [X1|L1], [X1|L]) :- insert(X, L1, L).

insertionSort([], []):- !.
insertionSort([X|L], S):- insertionSort(L, S1), insert(X, S1, S).


%%%%%%%%%%%%%%%%%%%%%%%%%Take%%%%%%%%%%%%%%%%%%%%%%%%%%
take(N,_,Xs) :- N =< 0, !, Xs = []. % Base case where all K are taken.
take(_,[],[]). % If empty...
take(N,[X|Xs],[X | Ys]) :-  % Append one to result, cut from X.
        M is N - 1,
        take(M, Xs, Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%Main%%%%%%%%%%%%%%%%%%%%%%%%%%
print([H | T]) :-  % Recursive through all selected k's.
    H = sub(Sum,I,J,L), % Pattern matching on atom. 
    write(Sum),
    write('\t'),
    write(I),
    write('\t'),
    write(J),
    write('\t'),
    write(L),
    write('\n'),
    print(T).

print_all(L, R) :-
    write("Original list: "),
    write(L),
    write('\n \n Size \t i \t j \t sublist\n'),
    print(R).

smallestKSets([],_) :-
    write("Error empty list").  % Just in case
smallestKSets(L, K) :-
    sublist_helper(L,[],L2),          % Generate sublists. 
    insertionSort(L2,L2Sorted),       % Sort.
    take(K,L2Sorted,R),
    print_all(L, R).

% s [x*(-1)^x | x <- [1..100]]
generate_(0,R,R). % Write to result, base case where n=0.
generate_(N, R, L) :-
    T is N * ((-1)**N),  % Each negative each positive. 
    N2 is N - 1,
    generate_(N2, [T | R], L).

test() :- smallestKSets([-1,2,-3,4,-5],3).
test1() :- generate_(100,[],X), smallestKSets(X,15).
test2() :- smallestKSets([24,-11,-34,42,-24,7,-19,21],6).
test3() :- smallestKSets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],8).
test4() :- smallestKSets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],10).
test5() :- smallestKSets([],10).