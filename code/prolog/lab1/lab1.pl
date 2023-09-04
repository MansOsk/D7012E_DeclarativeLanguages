/*
    Room: [r1,r2,r3]

    CONTENTS:
        r1 = {steel key, mobile two-handed robot}.     

        r2 = {brass key}.

        r3 = {package}.

    START STATE: r1.

        CONSTRAINS:
            - Robot can only hold two items.
            - Robot can only take item if currently in that room.
            - Dropped items stays in current room.

    END STATE:   r2 IFF holding package.

    TRAVERSE:
        r1 <-> r2, IF steel key.
        r1 <-> r3, IF brass key.
        r2 <-> r3, IS NOT POSSIBLE.

    ROBOT:
        Robot = [current_room,item1,item2]

    EXAMPLE OF SOLUTION:       
                    start:  "initate"
                [[r1, _, _],                    
                    state1: pickup steel key.

                [r1, steel key,_]
                    state2: move to r2.

                [r2, steel key, _],
                    state3: pick brass key
                               
                [r2, steel key, brass key]
                    state4: move to r1.

                [r1, steel key, brass key],  
                    state5: move to r3.

                [r3, steel key, brass key]   
                    state6: drop steel key.

                [r3, _, brass key]   
                    state7: take package.

                [r3, package, brass key]
                    state8: move to r1.

                [r2, package, brass key]
                    goal: Robot is in r2 AND has package.
*/

%
% move(State1, action, State2) 
%
% state(Room, Steel, Brass, Package), NewState, Move
%

% From r1 to any
move(state(State,Steel, Brass, Package), Action, NewState) :- 
        State = r1,
        Steel = hand,
        NewState = state(r2,Steel,Brass,Package),
        Action = moveR2.

move(state(State, Steel, Brass, Package), Action, NewState) :-
        State = r1, 
        Brass = hand, 
        NewState = state(r3, Steel, Brass, Package),
        Action = moveR3.

% From r2 to r1.
move(state(State,Steel, Brass, Package), Action, NewState) :- 
        State = r2, 
        Steel = hand,
        NewState = state(r1, Steel, Brass, Package),
        Action = moveR1.

% From r3 to r1.
move(state(State, Steel, Brass, Package), Action, NewState) :-
        State = r3, 
        Brass = hand,
        NewState = state(r1, Steel, Brass, Package),
        Action = moveR1.

% Pickup steel, key and drop key.
move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Steel = State,
        Steel \= hand,
        (not(Brass = hand) ; not(Package = hand)),
        NewState = state(State, hand, Brass, Package),
        Action = pickSteel.

move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Steel = hand,
        NewState = state(State, State, Brass, Package),
        Action = dropSteel.

% Pickup brass, key and drop key.
move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Brass = State,
        Brass \= hand,
        (not(Steel = hand) ; not(Package = hand)),
        NewState = state(State, Steel, hand, Package),
        Action = pickBrass.

move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Brass = hand,
        NewState = state(State, Steel, State, Package),
        Action = dropBrass.

% Pickup package, and drop.
move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Package = State,
        Package \= hand,
        (not(Steel = hand) ; not(Brass = hand)),
        NewState = state(State, Steel, Brass, hand),
        Action = pickPackage.

move(state(State,Steel, Brass, Package), Action, NewState) :- 
        Package = hand,
        NewState = state(State, Steel, Brass, State),
        Action = dropPackage.
    
% check(state(State,Steel, Brass, Package), Finished) :-
%        Package = hand,
%        State = r1,

solveR(state(_,_,_,r2),_,[]).
solveR(State,N,Trace) :-
    N > 0, 
    move(State, Move, NewState),
    solveR(NewState,N-1,TraceCo),
    Trace = [Move|TraceCo].
