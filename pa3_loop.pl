:- use_module(library(clpfd)).


digit(D) :-
    between(0,10000,D).

q1(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6) :-
    digit(D1),
    digit(D2),
    digit(D3),
    digit(D4),
    digit(B1),
    digit(B2),
    digit(B3),
    digit(B4),
    digit(B5),
    digit(B6).

q2(D1,D2,D3,D4,B1,B2,B3,B4,B5,B6) :-
    D1 \== D2, D1 \== D3, D1 \== D4,
    D2 \== D3, D2 \== D4,
    D3 \== D4,
    D1 \== 1, D2 \== 1, D3 \== 1, D4 \== 1, 
    D1 \== 3, D2 \== 3, D3 \== 3, D4 \== 3, 
    D1 \== 8, D2 \== 8, D3 \== 8, D4 \== 8, 
    D1 \== 120, D2 \== 120, D3 \== 120, D4 \== 120, 
    D1*D2+1 =:= B1*B1,     D1*D3+1 =:= B2*B2,
    D1*D4+1 =:= B3*B3,     D2*D3+1 =:= B4*B4,
    D2*D4+1 =:= B5*B5,     D3*D4+1 =:= B6*B6.





checkProduct2(D1,D2, D_new,B) :-
    D1*D2+1 =:= B*B,
    !,
    D_new is D2+1.
checkProduct3(D1,D2,D3, D_new,B2,B3) :-
    D1*D3+1 =:= B2*B2,D2*D3+1 =:= B3*B3,!,D_new is D3+1.
checkProduct4(D1,D2,D3,D4, D_new,B4,B5,B6) :-
    D1*D4+1 =:= B4*B4,D2*D4+1 =:= B5*B5,
    D3*D4+1 =:= B6*B6,!,D_new is D4+1.

%check(1, D) :- D is D+1, write(D),nl. 
%check(3, D) :- D is D+1. 
%check(8, D) :- D is D+1. 
%check(120, D) :- D is D+1. 
% check(D) :-
%     ( D =:= 1
%     -> D is D+1
%     ; D =:= 3
%     -> D is D+1
%     ; D =:= 8
%     -> D is D+1
%     ; D =:= 120
%     -> D is D+1
%     ),
%     write(D).

check(D,D2) :-
    ( D =:= 1
    -> D2 is D+1
    ; D =:= 3
    -> D2 is D+1
    ; D =:= 8
    -> D2 is D+1
    ; D =:= 120
    -> D2 is D+1
    ; D2 is D
    ),
    write(D2),nl.

increment(D, D_new) :- D_new is D+1. 


%case(0,1,2,3).
%case(D1,D2,D3,D4):-.
myloop1(D1,D2,D3,D4,D_new):- D1=<10000, check(D1,D_new),write(D_new),!.
myloop2(D1,D2,D3,D4,D_new):-
    D2=<10000, check(D2,D_new),
    checkProduct2(D1,D2, D_new,B).


%myloop3(D1,D2,D3,D4): D3=<10000.
%myloop4(D1,D2,D3,D4): D4=<10000.

main:- myloop1(0,1,2,3,D1),
        write(D1),
        TmpD2 is D1+1,
        write(TmpD2).
        myloop2(D1,TmpD2,2,3,D2),
        write(D2).
                % TmpD3 is D2+1,
                % write(D1).
                %myloop3(D1,D2,TmpD3,3,D3),
                %TmpD4 is D3+1,
                %myloop4(D1,D2,D3,TmpD4,D4).
%main :- .