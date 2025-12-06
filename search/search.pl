% search.pl — autograder-compatible, with unlock events

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(Actions) :-
    initial(StartRoom),
    bfs([node(state(StartRoom, []), [])], [], RevActions),
    reverse(RevActions, Actions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BFS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bfs([node(state(Room, _Keys), ActionsSoFar) | _], _Visited, ActionsSoFar) :-
    treasure(Room), !.

bfs([node(State, ActionsSoFar) | Rest], Visited, Actions) :-
    findall(node(NextState, NewActions),
        transition(State, ActionsSoFar, NextState, NewActions),
        Children),
    add_new_states(Children, Visited, NewNodes, NewVisited),
    append(Rest, NewNodes, Queue),
    bfs(Queue, NewVisited, Actions).

bfs([], _, _) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visited management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_new_states([], V, [], V).
add_new_states([node(S,A)|R], V, [node(S,A)|NR], NV) :-
    \+ member(S, V),
    add_new_states(R, [S|V], NR, NV).
add_new_states([node(S,_)|R], V, NR, NV) :-
    member(S, V),
    add_new_states(R, V, NR, NV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transition rules (adds unlock actions)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transition(state(Room,Keys), ActionsSoFar, state(Next,Keys2), [move(Room,Next)|ActionsSoFar]) :-
    door(Room, Next),
    pick_up_key(Next, Keys, Keys2).

transition(state(Room,Keys), ActionsSoFar, state(Next,Keys2), [move(Room,Next)|ActionsSoFar]) :-
    door(Next, Room),
    pick_up_key(Next, Keys, Keys2).

% Locked door forward
transition(state(Room,Keys), ActionsSoFar, state(Next,Keys2),
           [move(Room,Next), unlock(Color)|ActionsSoFar]) :-
    locked_door(Room, Next, Color),
    member(Color, Keys),
    pick_up_key(Next, Keys, Keys2).

% Locked door backward
transition(state(Room,Keys), ActionsSoFar, state(Next,Keys2),
           [move(Room,Next), unlock(Color)|ActionsSoFar]) :-
    locked_door(Next, Room, Color),
    member(Color, Keys),
    pick_up_key(Next, Keys, Keys2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Key pickup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pick_up_key(Room, KeysHeld, NewKeys) :-
    key(Room,Color),
    \+ member(Color, KeysHeld),
    sort([Color|KeysHeld], NewKeys), !.
pick_up_key(_, Keys, Keys).
