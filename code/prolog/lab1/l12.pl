%%%%% 5

family(
person( tom,  fox,  date(7,may,1960),  works( bbc,  15200)),
person( ann,  fox,  date(9,may,1961),  unemployed),
[ person( pat,  fox,  date(5,may,1983),  unemployed),
   person( jim,  fox,  date(5,may,1983),  unemployed) ] ).

% family( person( Y,  armstrong,  _,  _),  _,  _).
% family(X,  _,  [ _,  _,  _] ).
% family( _,  person(F,  S,  _,  _),  [ _,  _,  _ |_] ).


%%%%% 6

husband( X) :- family( X,  _,  _).

wife( X) :- family( _,  X,  _).

child( X) :-
    family( _,  _,  Children),
    member( X,  Children).

exists( Persons) :-
    husband( Persons); wife( Persons); child( Persons).

% dateofbirth( person(_,  _,  Date,  _),  Date).
% salary( person(_,  _,  _,  works(_,  S)),  S).
% salary( person(_,  _,  _,  unemployed),  0).
% exists( person( Name,  Surname,  _,  _)).


%%%%% 7

% child( X),  dateofbirth( X,  date( _,  _,  2000)).
% wife( person( Name,  Surname,  _,  works(_,  _))).
% exists( person( Name,  Surname,  date(_,  _,  Year), unemployed)),
%     Year < 1973.
% exists( Person),  
%     dateofbirth( Person,  date(_,  _,  Year)),  Year < 1960,
%     salary( Person,  Salary),  Salary < 8000.


%%%%% 8

total( [],  0).
total( [Person |List],  Sum) :- 
    salary( Person,  S),
    total( List,  Rest),
    Sum is S + Rest.

% total( List_of_people,  Sum_of_their_salaries).
% family( Husband,  Wife,  Children),
%    total( [Husband,  Wife | Children],  Income).


%%%%% 9

% family( Husband,  Wife,  Children),
%     total( [Husband,  Wife | Children],  Income),
%     length( [Husband,  Wife | Children],  N),
%     Income/N < 2000.


%%%%% 10-15

solution1( [] ).
solution1( [X/Y | Others] ) :-
    solution1( Others),  member( Y,  [1,2,3,4,5,6,7,8] ),  
    noattack1( X/Y,  Others).           

noattack1( _,  [] ). 
noattack1( X/Y,  [X1/Y1 | Others] ) :-
    Y =\= Y1,   Y1-Y =\= X1-X,  Y1-Y =\= X-X1,  
    noattack1( X/Y,  Others).

member( Item,  [Item | Rest] ).  % Could use _ instead of Rest
member( Item,  [First | Rest] ) :- % Could use _ instead of First
    member( Item,  Rest).

template1( [1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8] ).

% template1( S),  solution1( S).


%%%%% 17-21

solution2( Queens)  :-
   permutation( [1,2,3,4,5,6,7,8],  Queens),  safe( Queens). 

permutation( [],  [] ). 
permutation( [Head | Tail],  PermList)  :-
   permutation( Tail,  PermTail),  del( Head,  PermList,  PermTail).    

del( Item,  [Item | List],  List). 
del( Item,  [First | List],  [First | List1] )  :-
   del( Item,  List,  List1). 

safe( [] ). 
safe( [Queen | Others] ) :- safe( Others), noattack2( Queen, Others, 1). 

noattack2( _,  [],  _). 
noattack2( Y,  [Y1 | Ylist],  Xdist)  :-
   Y1-Y =\= Xdist,  Y-Y1 =\= Xdist,  Dist1 is Xdist + 1,
   noattack2( Y,  Ylist,  Dist1).

% solution2( S).


%%%%% 24-28

solution3( Ylist)  :-   
      sol( Ylist,  [1,2,3,4,5,6,7,8],  [1,2,3,4,5,6,7,8],                         
                    [-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7],
                    [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] ). 

sol( [],  [],  Dy,  Du,  Dv).
sol( [Y | Ylist],  [X | Dx1],  Dy,  Du,  Dv)  :-
      del( Y,  Dy,  Dy1),  
      U is X-Y,  del( U,  Du,  Du1),  
      V is X+Y,  del( V,  Dv,  Dv1),  
     sol( Ylist,  Dx1,  Dy1,  Du1,  Dv1).  

% solution3( Ylist).


%%%%% 32

f( X, 0) :- X < 3.
f( X, 2) :- 3=<X, X<6.
f( X, 4) :- 6 =< X.

% f(1, Y), 2 < Y.


%%%%% 39

p(1).
p(2) :- !.
p(3).


%%%%% 40

 max1( X, Y, X) :- X >= Y.
  max1( X, Y, Y) :- X < Y.

max2( X, Y, X) :- X >= Y, !.
max2( X, Y, Y).

% max2( 3, 1, X). 
% X = 3.
%
% max2( 3, 1, 1).
% is true because case 1 does not match; we go to the second case but 1 is not max(!)
% better verison is max3:

max3( X, Y, Max) :-
    X >= Y, ! ,Max = X;
    Max = Y.


%%%%% 41

member1( X, [X|L]).
member1( X, [Y|L]) :- member1( X, L).

% member1( X, [a, b, c, a]).

member2( X, [X|L]) :- !.
member2( X, [Y|L]) :- member2( X, L). 

% member2( X, [a, b, c, a]).


%%%%% 42

add( X, L, L) :- member( X, L), !.
add( X, L, [X|L]). 


%%%%% 44

beat( tom, jim). 
beat( ann, tom).
beat( pat, jim).    


%%%%% 45

class( X, fighter) :- beat( X, _), beat(_, X), !.
class( X, winner) :- beat( X, _), !.
class( X, sportsman) :- beat(_, X).


%%%%% 46

likes1( mary, X) :- snake( X), !, fail.
likes1( mary, X) :- animal( X).

likes2( mary, X) :- snake( X), !, fail  
                    ; 
                    animal( X).


%%%%% 47

difference1( X, X):- !, fail.
difference1( X, Y).

difference1( X, Y):- X=Y, !, fail  
                     ; 
                     true.

%%%%% 48

not( P):- P, !, fail
          ;
          true.


%%%%% 49

likes( mary, X) :- animal( X), not(snake( X)).

difference( X, Y) :- not( X = Y).

class2( X, fighter) :-
    beat( X, _),
    beat(_, X).
class2( X, winner) :-
    beat( X, _),
    not(beat(_, X)).
class2( X, sportsman) :-
    beat( _, X),
    not(beat( X, _)).


%%%%% 50

solution1b( [] ).
solution1b( [X/Y | Others] )  :- 
    solution1b( Others),
    member( Y, [1,2,3,4,5,6,7,8] ), 
    not(attacks1b( X/Y, Others)).           

attacks1b( X/Y, Others)  :-
    member(X1/Y1, Others),
    (Y1 = Y; 
     Y1 is Y + X1 - X;                  
     Y1 is Y - X1 + X).


%%%%% 53

p1 :- a, b.
p1 :- c.

p2 :- a, !, b.
p2 :- c. 


%%%%% 54

p3 :- c. 
p3 :- a, !, b.


%%%%% 56

human(joe).
human(sarah).


%%%%% 57

good_standard( cg).
good_standard( waldorf). 

expensive(cg).

reasonable( Restaurant) :- 
    not(expensive( Restaurant)).