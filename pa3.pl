:- use_module(library(clpfd)).

ps :- q2(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6),
out(D1,D2,D3,D4).

out(D1,D2,D3,D4) :- write(D1),write(','),write(D2),write(','),write(D3),write(','),write(D4).

q1(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6) :- D1 in 0..10000, D2 in 0..10000,
                              D3 in 0..10000, D4 in 0..10000,
                              B1 in 0..10001, B2 in 0..10001,
                              B3 in 0..10001, B4 in 0..10001,
                              B5 in 0..10001, B6 in 0..10001.
    

q2(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6) :- q1(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6),
    D1 #\= D2, D1 #\= D3, D1 #\= D4,
    D2 #\= D3, D2 #\= D4,
    D3 #\= D4,
    D1 #\= 1, D2 #\= 1, D3 #\= 1, D4 #\= 1, 
    D1 #\= 3, D2 #\= 3, D3 #\= 3, D4 #\= 3, 
    D1 #\= 8, D2 #\= 8, D3 #\= 8, D4 #\= 8, 
    D1 #\= 120, D2 #\= 120, D3 #\= 120, D4 #\= 120, 
    D1*D2+1 #= B1*B1,     D1*D3+1 #= B2*B2,
    D1*D4+1 #= B3*B3,     D2*D3+1 #= B4*B4,
    D2*D4+1 #= B5*B5,     D3*D4+1 #= B6*B6.
                        