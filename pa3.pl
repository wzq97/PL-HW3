:- use_module(library(clpfd)).
                        
q1(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6) :- D1 in 0..10000, D2 in 0..10000,
                              D3 in 0..10000, D4 in 0..10000,
                              B1 in 0..10001, B2 in 0..10001,
                              B3 in 0..10001, B4 in 0..10001,
                              B5 in 0..10001, B6 in 0..10001,
    D1*D2+1 #= B1*B1,     D1*D3+1 #= B2*B2,
    D1*D4+1 #= B3*B3,     D2*D3+1 #= B4*B4,
    D2*D4+1 #= B5*B5,     D3*D4+1 #= B6*B6.

q2(D1,D2,D3,D4):- D1 in 0..10000, 
                              D2 in 0..10000,
                              D3 in 0..10000,
                              D4 in 0..10000.
                        