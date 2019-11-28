:- use_module(library(clpfd)).
:- initialization(main, main).
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