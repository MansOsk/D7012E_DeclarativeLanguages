% I/O examples

%%%%% 5

bars([]).
bars([N|L]) :- stars( N), nl, bars( L).

stars( N) :- N > 0, write( *), N1 is N-1, stars(N1).
stars( N) :- N =<0.


%%%%% 10

input(X) :-
    repeat,
    write('Enter a Prolog term and end with a period: '), 
    read(X),
    (extract(X,A,B,C)              % if this ...
        -> writeln("OK Format")    % then ...
        ;  writeln("Bad format"),  % else ...
           fail),
    check(A),
    check(B),
    check(C). %,
    %fail. % keep repeating even if input is good
    % remove fail above to have procedure return
    % when a good input has been entered

extract(X:Y:Z,X,Y,Z).

check(N) :-
    write(N),
    number(N)                           % if this ...
        -> writeln(" is a number")     % then ...
        ;  writeln(" is not a number"), % else ...
           !,    % stop checking when first error is detected
	   fail. % signal fail back, so caller can redo read


%%%%% 12

input_from_stdin(CodedInput,AsAtom,ListOfAtoms) :-
    repeat,
    write('Enter text: '), 
    read_line_to_codes(user_input,CodedInput),
    atom_codes(AsAtom, CodedInput),
    atomic_list_concat(ListOfAtoms, ' ', AsAtom),!.
    % the cut prohibits backtracking, so just one line is read

% BLOCKS EXAMPLE

% 1. Naive depth-first search

member(X, [X| _]).
member(X, [_| T]) :- member(X, T).

del(X, [X|L],L).
del(X,[Y|L],[Y|L1]) :- del(X,L,L1).

s(Stacks, [Stack1, [Top | Stack2] | OtherStacks]) :-
  del([Top | Stack1], Stacks, Stacks1),
  del(Stack2, Stacks1, OtherStacks).

goal(Situation) :- member([a,b,c],Situation).

solve1(S, [S]) :- goal(S).
solve1(S, [S|Sol1]) :- s(S,S1), solve1(S1,Sol1).

% solve1([[c,a,b],[],[]],Solution).


% 2. Do not allow cycles 

solve2(Node, Solution) :- depthfirst([],Node,Solution).

depthfirst(Path,Node,[Node|Path]) :- goal(Node).
depthfirst(Path,Node,Sol) :-
  s(Node,Node1),
  not(member(Node1,Path)),    % already been to this node?
  depthfirst([Node|Path],Node1,Sol).

% solve2([[c,a,b],[],[]],Solution).


% 3. Limit the search depth to a given constant

depthfirst2(Node, [Node], _) :- goal(Node).
depthfirst2(Node, [Node|Sol], Maxdepth):-
  Maxdepth>0,
  s(Node,Node1),
  Max1 is Maxdepth-1,
  depthfirst2(Node1,Sol,Max1).

solve3(Node, Solution) :-
    Max is 2,   % maximum depth is  Max+1  because of the base case in depthfirst2
                % Max=5 gives a solution; higher might give infinitely long solutions
    depthfirst2(Node, Solution, Max).

% solve3([[c,a,b],[],[]],Solution).
%
% This prints the solution and its length, N
% solve3([[c,a,b],[],[]],Solution),length(Solution,N).



% 4. Iterative deepening

helper(Node, Solution,Depth) :-       % First try with depth Depth
    depthfirst2(Node,Solution,Depth). % find solution with limit Depth
helper(Node, Solution,Depth) :-  % If Depth was not enough, 
    NewDepth is Depth + 1,       % try instead with Depth + 1
    % NewDepth < 12,            % Uncomment to get an absolute limit
    helper(Node, Solution, NewDepth). 
    
solve4(Node,Solution) :-
    helper(Node,Solution,1). % start with max depth 1