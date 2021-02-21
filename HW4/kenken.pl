% create list of unique elements [1,..,N]
row_constraint(L, N) :- 
    length(L, N),
    fd_domain(L, 1, N),
    fd_all_different(L).

% check that each row satisfies constraints: length N, elements from [1..N], and all different
check_row([], N).
check_row([Head|Tail], N) :-
    row_constraint(Head, N),
    check_row(Tail, N).

label([]).
label([H|T]) :-
    fd_labeling(H),
    label(T).

kenken(N, C, T) :-
    length(T, N),
    check_row(T, N),
    transpose(T, R),
    check_row(R, N),
    check_constraints(T, C),
    label(T).

% transpose matrix then check each row of the transposed matrix
transpose_col([],[],[]).
transpose_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- 
    transpose_col(Rows, Hs, Ts).

transpose([[]|_], []).
transpose(Matrix, [Head|Tail]) :-
    transpose_col(Matrix, Head, TailMatrix),
    transpose(TailMatrix, Tail).

check_constraints(_, []).
check_constraints(Matrix, [H|T]) :-
    constraint(Matrix, H),
    check_constraints(Matrix, T).
    
% get matrix values 
get(Matrix, [R|C], Val) :-
    nth(R, Matrix, Row),
    nth(C, Row, Val).

% arithmetic operations
get_product(_, [], 1).
get_product(T, [Head|Tail], Product) :-
    get(T, Head, Val),
    get_product(T, Tail, New),
    Product #= Val * New.

get_sum(_, [], 0).
get_sum(T, [Head|Tail], Sum) :-
    get(T, Head, Val),
    get_sum(T, Tail, New),
    Sum #= Val + New.

constraint(T, +(S, L)) :-
    get_sum(T, L, S).

constraint(T, *(P, L)) :-
    get_product(T, L, P).

constraint(T, -(D, A, B)) :-
    get(T, A, X),
    get(T, B, Y),
    (D #= X - Y ; D #= Y - X). 

constraint(T, /(Q, A, B)) :-
    get(T, A, X),
    get(T, B, Y),
    (Q #= X/Y; Q #= Y/X). 

% plain kenken without using FDS
% L stores all numbers from 1 to N, replacing fd_domain
plain_domain(L, N) :-
    maplist(between(1,N), L).

% replace fd_all_different
plain_all_different([]).
plain_all_different([H|T]) :-
    member(H, T), !, fail.
plain_all_different([H|T]) :-
    plain_all_different(T).

plain_row_constraint(L, N) :-
    length(L, N),
    plain_domain(L, N),
    plain_all_different(L).

plain_check_row([], N).
plain_check_row([Head|Tail], N) :-
    plain_row_constraint(Head, N),
    plain_check_row(Tail, N).

plain_kenken(N, C, T) :-
    length(T, N),
    plain_check_row(T, N),
    transpose(T, R),
    plain_check_row(R, N),
    check_constraints(T, C).

four_kenken_testcase(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).

noop_kenken_testcase(
    5,
    [
        +(2, [[1|1]]),
        +(38, [[1|2], [1|3], [1|4], [2|2], [3|2], [3|3], [3|4], [4|4], [5|2], [5|3], [5|4]]),
        +(6, [[2|1], [3|1]]),
        *(6, [[4|1], [4|2]]),
        +(4, [[5|1]]),
        +(1, [[4|3]]),
        *(24, [[1|5], [2|3], [2|4], [2|5]]),
        +(9, [[3|5], [4|5], [5|5]])
    ]
).

kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).