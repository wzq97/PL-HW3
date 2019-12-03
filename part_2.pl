:- use_module(library(clpfd)).

% constraint the list with all even integers
constraint_Even([]).
constraint_Even([H|T]) :-
    H mod 2 #= 0,
    constraint_Even(T).

% constraint the list with all odd integers
constraint_Odd([]).
constraint_Odd([H|T]) :-
    H mod 2 #= 1,
    constraint_Odd(T).

% print output
print_Sol([]).
print_Sol([H|[]]) :- format('~w', H).
print_Sol([H|T]) :-
    format('~w,', H),
    print_Sol(T).

% //////////////////// SUM //////////////////////////
% create a list with a desire count and sum
createList_To_Sum([],0,0).
createList_To_Sum([X|Xs],Count,Sum) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..128,
    Sum #=  X+Sum0,
    Newcount is Count-1,
    createList_To_Sum(Xs,Newcount,Sum0).

% function that called by main to find list with N number of
% even integers that sum up to S
findSol_Even_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Even(List),
    all_distinct(List),
    label(List),
    print_Sol(List).

% function that called by main to find list with N number of
% odd integers that sum up to S
findSol_Odd_Sum(N, S) :- 
    createList_To_Sum(List, N, S),
    constraint_Odd(List),
    all_distinct(List),
    label(List), print_Sol(List).

% function that called by main to find list with N number of
% both even and odd integers that sum up to S
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
    print_Sol(List1),
    write(','),
    print_Sol(List2).

% ////////////////////////// PRODUCT/////////////////////////////////
% create a list with a desire count and product
createList_To_Product([],0,1).
createList_To_Product([X|Xs],Count,Product) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..128,
    Product #=  X*Product0,
    Newcount is Count-1,
    createList_To_Product(Xs,Newcount,Product0).

% function that called by main to find list with N number of
% even integers that have a product of P
findSol_Even_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Even(List),
    label(List),
    print_Sol(List).

% function that called by main to find list with N number of
% odd integers that have a product of P
findSol_Odd_Product(N, P) :- 
    createList_To_Product(List, N, P),
    all_distinct(List),
    constraint_Odd(List),
    label(List),
    print_Sol(List).

% function that called by main to find list with N number of
% both even and odd integers that have a product of P
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
    print_Sol(List1),
    write(','),
    print_Sol(List2).

% sentence parsing grammar
ip --> s.
s --> ["Find"],["a"],["set"],["of"],i,["that"],op,["to"],number.
i --> even, ["integers"] | odd , ["integers"] | both, ["integers"].
even --> num, ["even"].
odd --> num, ["odd"].
both --> even, ["and"], odd.
num --> [_].
op --> ["sum"] | ["multiply"].
number --> [_].


main([Input]) :- 
    atom_string(Input, S),
    split_string(S, ' ', '', StringList),
    % check if the input string can be parsed
    % if yes, try to find solution
    % otherwise, it is an invalid string
    (phrase(ip, StringList) ->
        nth0(4,StringList,N1_S),        % read in the number and convert
        number_codes(N1,N1_S),
        nth0(6,StringList,I),
        % if the 7th word is "and", do find solution for both even and odd
        (I == "and" ->
            nth0(11,StringList,OP),     % read in the operation and number 
            nth0(7,StringList,N2_S),    % that needs to sum/multiply to
            number_codes(N2,N2_S),
            nth0(13,StringList,Num_S),
            number_codes(Num,Num_S),
            (OP =="sum" ->              % operation is sum
                (findSol_Both_Sum(N1,N2,Num)->!
                ;format('No Solution~n'));  %if there's no solution
            OP == "multiply" ->         % operation is multiply
                (findSol_Both_Product(N1,N2,Num)->!
                ;format('No Solution~n'))   %if there's no solution
            )
        ;
            % when there is no "and", it means find solution for only even OR odd integers
            nth0(8,StringList,OP),      % read in the operation and number 
            nth0(5,StringList,EO),      % read in whether to find even or odd 
            nth0(10,StringList,Num_S),
            number_codes(Num,Num_S),
            (OP == "sum", EO == "even" ->   % operation is sum with only even int
                (findSol_Even_Sum(N1,Num)-> !
                ;format('No Solution~n'));
            OP == "multiply", EO == "even" ->   % operation is multiply with only even int
                (findSol_Even_Product(N1,Num)->!
                ;format('No Solution~n'));
            OP == "sum", EO == "odd" ->     % operation is sum with only odd int
                (findSol_Odd_Sum(N1,Num)->!
                ;format('No Solution~n'));
            OP == "multiply", EO == "odd" ->    % operation is multiply with only odd int
                (findSol_Odd_Product(N1,Num)->!
                ;format('No Solution~n'))
            )
        )
        
    ; phrase(ip, StringList) -> format('No Solution~n')
    ; format('Invalid String~n')    % sentence can not be parsed by the grammer, then invalid
    ).