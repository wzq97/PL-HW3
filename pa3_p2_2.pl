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
print_Sol([H|[]]) :- format('~w', H).
print_Sol([H|T]) :-
    format('~w,', H),
    print_Sol(T).

% % create a list of length N with distinct elements
% createList(F, N, S) :-
%     length(F, N),
%     F ins 1..S,
%     all_distinct(F).

% ////////////////////  SUM   //////////////////////////


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

findSol_Both_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    label(List), 
    write('At least one solution exists: '),
    print_Sol(List).

% //////////////////////////    PRODUCT /////////////////////////////////
% product_of_list([], 1).
% product_of_list([H|T], Product) :- product_of_list(T, Rest), Product is H * Rest.
% prod_list([],0).
% prod_list([H|T], Product) :- product_of_list([H|T], Product).

% list_op_product(L, Op, P) :-
%     identity_element(Op, IE),
%     reverse(L, R), % no foldr in SWI-Prolog, at least
%     foldl(Op, R, IE, P).

% identity_element(add, 0).
% identity_element(mult, 1).

% add(X, Y, R) :- R is X + Y.
% mult(X, Y, R) :- R is X * Y.

createList_To_Product([],0,1).
createList_To_Product([X|Xs],Count,Product) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..128,
    Product #=  X*Product0,
    Newcount is Count-1,
    createList_To_Product(Xs,Newcount,Product0).
% createList_To_Product(F, N, P) :-
%     length(F, N),
%     F ins 1..P, scalar_product(+Cs, +Vs, +Rel, P),%prod_list(F, P),% Product#=M,
%     all_distinct(F).    % make sure all elements are different

% findSol_Even_Product(2, 16).
findSol_Even_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Even(List),
    label(List), 
    write('At least one solution exists: '),
    print_Sol(List).

% findSol_Even_Product(2, 21).
findSol_Odd_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Odd(List),
    label(List), 
    write('At least one solution exists: '),
    print_Sol(List).

findSol_Both_Product(N, S) :- 
    createList_To_Product(List, N, S),
    all_distinct(List),
    label(List), 
    write('At least one solution exists: '),
    print_Sol(List).

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
    ; phrase(ip, StringList) -> format(' No Solution~n')
    ; format('Invalid String~n')
    ).