% product_of_list([], 1).
% product_of_list([H|T], Product) :- product_of_list(T, Rest), Product is H * Rest.
% prod_list([],0).
% prod_list([H|T], Product) :- product_of_list([H|T], Product).



% list_op_product(L, Op, P) :-
%     identity_element(Op, IE),
%     reverse(L, R), % no foldr in SWI-Prolog, at least
%     foldl(Op, R, IE, P).

% identity_element(add, 0).
% identity_element(mult, 1).

% add(X, Y, R) :- R is X + Y.
% mult(X, Y, R) :- R is X * Y.

% find_list(Lst,Product):-Lst ins 1..Product, list_op_product(Lst, P),P#=Product.



list_list_dotProduct([],[],0).
list_list_dotProduct([X|Xs],[Y|Ys],Sum) :-
    X   #>= 0,
    Y   #>= 0,
    Sum #=  X*Y + Sum0,
    list_list_dotProduct(Xs,Ys,Sum0).

calProduct([],0,1).
calProduct([X|Xs],Count,Product) :-
    Count #>= 1,
    X   #>= 1,
    X in 1..Product,
    Product #=  X*Product0,
    Newcount is Count-1,
    calProduct(Xs,Newcount,Product0).
%scalar_product(+Cs, +Vs, +Rel, ?Expr)
