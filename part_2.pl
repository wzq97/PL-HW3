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

createList_To_Sum([],0,0).
createList_To_Sum([X|Xs],Count,Sum) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..128,
    Sum #=  X+Sum0,
    Newcount is Count-1,
    createList_To_Sum(Xs,Newcount,Sum0).

% createList_To_Sum(F, N, S) :-
%     length(F, N),
%     F ins 1..inf, sum(F, #=, S),
%     all_distinct(F).    % make sure all elements are different

% findSol_Even_Sum(2, 10).
findSol_Even_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Even(List),
    all_distinct(List),
    label(List), 
    %write('At least one solution exists: '),
    print_Sol(List).

findSol_Odd_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Odd(List),
    all_distinct(List),
    label(List), print_Sol(List).



% //////////////////////////    PRODUCT /////////////////////////////////
createList_To_Product([],0,1).
createList_To_Product([X|Xs],Count,Product) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..128,
    Product #=  X*Product0,
    Newcount is Count-1,
    createList_To_Product(Xs,Newcount,Product0).

% findSol_Even_Product(2, 16).
findSol_Even_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Even(List),
    label(List), 
    %write('At least one solution exists: '),
    print_Sol(List).

% findSol_Even_Product(2, 21).
findSol_Odd_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Odd(List),
    label(List), 
    %write('At least one solution exists: '),
    print_Sol(List).

findSol_Both_Sum(N1, N2, S) :- 
    createList_To_Sum(List1, N1, S1),
    constraint_Even(List1),
    all_distinct(List1),
    createList_To_Sum(List2, N2, S2),
    constraint_Odd(List2),
    all_distinct(List2),
    S #= S1+S2,
    label(List1), 
    label(List2), 
    %write('At least one solution exists: '),
    print_Sol(List1),
    write(','),
    print_Sol(List2).


findSol_Both_Product(N1, N2, P) :- 
    createList_To_Product(List1, N1, P1),
    constraint_Even(List1),
    all_distinct(List1),
    createList_To_Product(List2, N2, P2),
    constraint_Odd(List2),
    all_distinct(List2),
    P #= P1*P2,
    label(List1),
    label(List2),
    %write('At least one solution exists: '),
    print_Sol(List1),
    write(','),
    print_Sol(List2).

ip --> s.
s --> ["Find"],["a"],["set"],["of"],i,["that"],op,["to"],number.
i --> even, ["integers"] | odd , ["integers"] | both, ["integers"].
even --> num, ["even"].
odd --> num, ["odd"].
both --> even, ["and"], odd.
num --> [_].
op --> ["sum"] | ["multiply"].
number --> [_].

% main("Find a set of 2 odd integers that sum to 16").
main([Input]) :- 
    atom_string(Input, S),
    split_string(S, ' ', '', StringList),
    % DEBUG print
    %forall(nth0(I, StringList, E), format('List[~w]=~w~n', [I, E])),
    
    (phrase(ip, StringList) ->
        nth0(4,StringList,N1_S),
        number_codes(N1,N1_S),
        nth0(6,StringList,I),
        (I == "and" -> 
            nth0(11,StringList,OP),
            nth0(7,StringList,N2_S),
            number_codes(N2,N2_S),
            nth0(13,StringList,Num_S),
            number_codes(Num,Num_S),
            (OP =="sum" ->
                (findSol_Both_Sum(N1,N2,Num)->!
                ;format('No Solution~n'));
            OP == "multiply" ->
                (findSol_Both_Product(N1,N2,Num)->!
                ;format('No Solution~n'))
            )
        ;
            % i != and
            nth0(8,StringList,OP),
            nth0(5,StringList,EO),
            nth0(10,StringList,Num_S),
            number_codes(Num,Num_S),
            (OP == "sum", EO == "even" ->
                (findSol_Even_Sum(N1,Num)-> !
                ;format('No Solution~n'));
            OP == "multiply", EO == "even" ->
                (findSol_Even_Product(N1,Num)->!
                ;format('No Solution~n'));
            OP == "sum", EO == "odd" ->
                (findSol_Odd_Sum(N1,Num)->!
                ;format('No Solution~n'));
            OP == "multiply", EO == "odd" ->
                (findSol_Odd_Product(N1,Num)->!
                ;format('No Solution~n'))

            )
        )
        
    ; phrase(ip, StringList) -> format('No Solution~n')
    ; format('Invalid String~n')
    ).