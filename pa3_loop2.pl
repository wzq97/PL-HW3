:- use_module(library(clpfd)).

q1(B) :-
    B in 0..10001.



check1(D1,Flag2) :-
    (check138120(D1,Flag), Flag =:= 1 -> Flag2 is 1
    ; Flag2 is -1
    ).


check2(D1,D2, D_new,B,Flag2) :-
    (check138120(D2,Flag), Flag =:= 1,
    %get B
    q1(B), D1*D2+1 #= B*B -> Flag2 is 1
    ; Flag2 is -1
    ).
 
check3(D1,D2,D3, D_new,B2,B3,Flag2) :-
    (check138120(D3,Flag), Flag =:= 1,
    %get B
    q1(B2), q1(B3), D1*D3+1 #= B2*B2, D2*D3+1 #= B3*B3 -> Flag2 is 1
    ; Flag2 is -1
    ).

check4(D1,D2,D3,D4, D_new,B4,B5,B6,Flag2) :-
    (check138120(D4,Flag), Flag =:= 1,
    %get B
    q1(B4), q1(B5), q1(B6),D1*D4+1 #= B4*B4,D2*D4+1 #= B5*B5,
    D3*D4+1 #= B6*B6 -> Flag2 is 1
    ; Flag2 is -1
    ).%,format('D1 ~w D2 ~w D3 ~w D4 ~w fLAG2 = ~w',[D1,D2,D3,D4,Flag2]),nl.


check138120(D,D2) :-
    ( D =:= 1    -> D2 is -1
    ; D =:= 3    -> D2 is -1
    ; D =:= 8    -> D2 is -1
    ; D =:= 120    -> D2 is -1
    ; D2 is 1).
    % ),write('check138120 '),
    % format('CHECKING ~w',[D]),
    % format('RETURN FLAG ~w',[D2]),nl.





%case(0,1,2,3).
%case(D1,D2,D3,D4):-.
myloop1(D1,D2,D3,D4,D_new):- D1=<10000, check1(D1,Flag2),
    ( Flag2=:=1 -> D_new is D1,%write('myloopp:'),
    write(D_new),write(',')%,write(D1),nl
    ; D_new is D1+1,myloop1(D_new,D2,D3,D4,D_new)
    ).
myloop2(D1,D2,D3,D4,D_new):- D2=<10000,check2(D1,D2, D_new,B,Flag2),
    ( Flag2=:=1 -> D_new is D2,%write('myloopp2:'),
    write(D_new),write(',')%,write(D2),nl
    ; D_new is D2+1,myloop2(D1,D_new,D3,D4,D_new)
    ).
myloop3(D1,D2,D3,D4,D_new):- D3=<10000,check3(D1,D2,D3, D_new,B2,B3,Flag2),
    ( Flag2=:=1 -> D_new is D3,%write('myloopp3:'),
    write(D_new),write(',')%,write(D3),nl
    ; D_new is D3+1,myloop3(D1,D2,D_new,D4,D_new)
    ).
myloop4(D1,D2,D3,D4,D_new):- %format('D1 ~w D2 ~w D3 ~w D4 ~w D_new = ~w',[D1,D2,D3,D4,D_new]),
    D4=<10000,check4(D1,D2,D3,D4, D_new,B4,B5,B6,Flag2),%format('^^^^Flag2 ~w',[Flag2]),
    ( Flag2=:=1 -> D_new is D4,%write('myloopp4:'),
    write(D_new)%,write(' '),write(D4),nl
    ; D_newnew is D4+1, %format('!!!!Dnew = ~w ', [D_newnew]), 
    myloop4(D1,D2,D3,D_newnew,D_new2)
    ).

%myloop3(D1,D2,D3,D4): D3=<10000.
%myloop4(D1,D2,D3,D4): D4=<10000.

main:-
    myloop1(0,1,2,3,D1),
    %write(D1),
    TmpD2 is D1+1,
    %write(TmpD2),
    myloop2(D1,TmpD2,2,3,D2),
    %write(D2),
    TmpD3 is D2+1,
    %write(TmpD3),
    myloop3(D1,D2,TmpD3,3,D3),
    %format('D3=~w',[D3]),
    TmpD4 is D3+1,
    myloop4(D1,D2,D3,TmpD4,D4).
    %format('TmpD4=~w',[TmpD4]),
    %format('D4=~w',[D4]).
