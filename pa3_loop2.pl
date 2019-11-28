:- use_module(library(clpfd)).

q1(B) :-
    B in 0..10001.



check1(D1,Flag2) :-
    (check138120(D1,Flag), Flag =:= 1 -> Flag2 is 1
    ; Flag2 is -1
    ).


check2(D1,D2, D_new,B,Flag2) :-

    (check138120(D2,Flag), Flag =:= 1,q1(B),D1*D2+1 =:= B*B -> Flag2 is 1
    ; Flag2 is -1
    ).

check3(D1,D2,D3, D_new,B2,B3) :-
    D1*D3+1 =:= B2*B2,D2*D3+1 =:= B3*B3,
    D3 \== 1, D3 \== 3,D3 \== 8,D3 \== 120,
    !,D_new is D3+1.
check4(D1,D2,D3,D4, D_new,B4,B5,B6) :-
    D1*D4+1 =:= B4*B4,D2*D4+1 =:= B5*B5,
    D3*D4+1 =:= B6*B6,!,D_new is D4+1.


check138120(D,D2) :-
    ( D =:= 1    -> D2 is -1
    ; D =:= 3    -> D2 is -1
    ; D =:= 8    -> D2 is -1
    ; D =:= 120    -> D2 is -1
    ; D2 is 1
    ),write('check138120 '),
    write(D2),nl.

checkSqrt(N1,N2) :-
    S is N1*N2+1,
    sqrt(S,N),
    write(N),
    integer(N).
% check138120(D) :-
%     ( D =:= 1
%     -> fail
%     ; D =:= 3
%     -> fail
%     ; D =:= 8
%     -> fail
%     ; D =:= 120
%     -> fail
%     ).

increment(D, D_new) :- D_new is D+1. 


%case(0,1,2,3).
%case(D1,D2,D3,D4):-.
myloop1(D1,D2,D3,D4,D_new):- D1=<10000, check1(D1,Flag2),
    ( Flag2=:=1 -> D_new is D1,write('myloopp:'),write(D_new),write(' '),write(D1)
    ; D_new is D1+1,myloop1(D_new,D2,D3,D4,D_new)
    ).
myloop2(D1,D2,D3,D4,D_new):- D2=<10000,check2(D1,D2, D_new,B,Flag2),
    ( Flag2=:=1 -> D_new is D2,write('myloopp:'),write(D_new),write(' '),write(D2)
    ; D_new is D2+1,myloop2(D1,D_new,D3,D4,D_new)
    ).


%myloop3(D1,D2,D3,D4): D3=<10000.
%myloop4(D1,D2,D3,D4): D4=<10000.

main:- myloop1(0,1,2,3,D1),
        write(D1),
        TmpD2 is D1+1,
        write(TmpD2),
        myloop2(D1,TmpD2,2,3,D2),
        write(D2),
        TmpD3 is D2+1,
        write(TmpD3).
                %myloop3(D1,D2,TmpD3,3,D3),
                %TmpD4 is D3+1,
                %myloop4(D1,D2,D3,TmpD4,D4).
%main :- .