:- use_module(library(clpfd)).
ip --> i, o.
% i --> ['even'] | ['odd'] | ['both'].
% o --> ['+'] | ['*'].
i --> ["even"] | ["odd"] | ["both"].
o --> ["+"] | ["*"].

main :- phrase(ip, ["even","+"]).