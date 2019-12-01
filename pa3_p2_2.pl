:- use_module(library(clpfd)).

% create a list with N elements
build(X, N, List)  :- 
    length(List, N), 
    maplist(=(X), List).

% sum up the list
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

testEven :-
    X in 1..10,
    label([X]),
    constraint_Even([X]),
    write(X).

% make the list all even int
constraint_Even([]).
constraint_Even([H|T]) :-
    H mod 2 #= 0,
    constraint_Even(T).

% make the list all even int
constraint_Odd([]).
constraint_Odd([H|T]) :-
    H mod 2 #= 1,
    constraint_Odd(T).

% findSol_Even_Sum(even, 2, 10).
findSol_Even_Sum(even, N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Even(List),
    label(List), print_list(List).

print_list([]).
print_list([H|T]) :-
    format('**printing list : ~w ~n', H),
    print_list(T).

% create a list of length N with distinct elements
createList(F, N, S) :-
    length(F, N),
    F ins 1..S,
    all_distinct(F).

createList_To_Sum(F, N, S) :-
    length(F, N),
    F ins 1..S, sum(F, #=, S),
    all_distinct(F).

% "" -> string, ''->atom
ip --> s.
% i --> ['even'] | ['odd'] | ['both'].
% o --> ['sum'] | ['multiply'] | ['divide'].
s --> ["Find"],["a"],["set"],["of"],i,["that"],op,["to"],number.
i --> even, ["integers"] | odd , ["integers"] | both, ["integers"].
even --> num(N), ["even"].
odd --> num(N), ["odd"].
both --> even, ["and"], odd.
num(N) --> [N].
op --> ["sum"] | ["multiply"] | ["divide"].
number --> [_].

% main("Find a set of 2 odd integers that sum to 16").
main(Input) :- 
    % read(String),
    split_string(Input, " ", " ", StringList),
    % print
    forall(nth0(I, StringList, E), format('List[~w]=~w~n', [I, E])),
    
    (phrase(ip, StringList) -> format('OK!'); format('Invalid String')
    ).