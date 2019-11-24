:- use_module(library(clpfd)).

% parser
parser(L,I,Op,Num):-
    read(S1),
    split_string(S1, " ", " ", S),
    findNumber(S,)
    find(S).

findNumber(S)

find([]).
find([H|T]) :- 
    ( H #="even" H #= "odd" H #= "both"
        ),
    H #= "sum"
    H #= "multiply"
    H #= "divide"
    find(T).
    
    
    
    
    char_type(H,digit).


