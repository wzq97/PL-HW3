:- use_module(library(clpfd)).

% % create a list with N elements
% build(X, N, List)  :- 
%     length(List, N), 
%     maplist(=(X), List).

% % sum up the list
% sum_list([], 0).
% sum_list([H|T], Sum) :-
%    sum_list(T, Rest),
%    Sum is H + Rest.

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

% helper
print_list([]).
print_list([H|T]) :-
    format('**printing list : ~w ~n', H),
    print_list(T).

print_Sol([]).
print_Sol([H|T]) :-
    format('~w,', H),
    print_Sol(T).

% % create a list of length N with distinct elements
% createList(F, N, S) :-
%     length(F, N),
%     F ins 1..S,
%     all_distinct(F).

createList_To_Sum(F, N, S) :-
    length(F, N),
    F ins 1..S, sum(F, #=, S),
    all_distinct(F).    % make sure all elements are different

% findSol_Even_Sum(2, 10).
findSol_Even_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Even(List),
    label(List), 
    write('At least one solution exists: '),
    print_Sol(List).

findSol_Odd_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Odd(List),
    label(List), print_Sol(List).

ip --> s.
s --> ["Find"],["a"],["set"],["of"],i,["that"],op,["to"],number(S).
i --> even, ["integers"] | odd , ["integers"] | both, ["integers"].
even --> num(N), ["even"].
odd --> num(N), ["odd"].
both --> even, ["and"], odd.
num(N) --> [N].
op --> ["sum"] | ["multiply"].
number(S) --> [S].

% main("Find a set of 2 odd integers that sum to 16").
main(Input) :- 
    split_string(Input, " ", " ", StringList),
    % DEBUG print
    forall(nth0(I, StringList, E), format('List[~w]=~w~n', [I, E])),
    
    (phrase(ip, StringList) -> format('OK!~n'),
    nth0(4,StringList,X),
    number_codes(N,X),
    format('5th elem is = ~w ~n', [N]),
    nth0(10,StringList,Y),
    number_codes(S,Y),
    format('11th elem is = ~w ~n', [S]),
    findSol_Even_Sum(N, S),
    !
    
    ; format('Invalid String~n')
    ).