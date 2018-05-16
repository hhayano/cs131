/*
 * N: non-negative integer specifying the size of the grid
 * T: a list of N lists. Each list represents a row in the grid.
 * C: a structure with function symbol counts and arity 4.
 *    Its arguments are all lists of N integers, and represent
 *    the tower counts for the top, bottom, left, and right edges, respectively.
 *
 * In this implementation, we expect C and N to be grounded.
 */

tower(Size, Grid, counts(Top, Bot, Left, Right)) :-
  left_tower_count_check(Grid, Left, Size, Size),
  right_tower_count_check(Grid, Right, Size, Size),
  transpose(Grid, Transpose),
  left_tower_count_check(Transpose, Top, Size, Size),
  right_tower_count_check(Transpose, Bot, Size, Size).

left_tower_count_check([], [], _, 0).

left_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  tower_valid_list(First_row, Size),
  count_check(First_row, First_count, 0),
  Iterations0 #=# Iterations - 1,
  left_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

right_tower_count_check([], [], _, 0).

right_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  reverse(First_row, Reversed),
  tower_valid_list(Reversed, Size),
  count_check(Reversed, First_count, 0),
  Iterations0 #=# Iterations - 1,
  right_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

tower_valid_list(Array, Size) :-
  length(Array, Size),
  fd_domain(Array, 1, Size),
  fd_all_different(Array).

count_check([First|Tail], Count, Tallest) :-
  First #># Tallest,
  Count0 #=# Count - 1,
  count_check(Tail, Count0, First);
  First #<# Tallest,
  count_check(Tail, Count, Tallest).

count_check([], 0, _).

% ========================
% Transpose Implementation
% ========================

% A1: Matrix to be transposed.
% A2: The transpose of A1.

transpose([H_row|T_rows], Transpose) :-
  transpose(T_rows, T_cols),
  consColumns(H_row, T_cols, Transpose).

% Base Case
transpose([],[]).

% First Argument: The first column of the matrix represented as a list of elements.
% Second Argument: The rest of the matrix, represented as list of lists.
%                  Each list represents a row instead of a column.
% Third Argument: The resulting matrix of the cons operator. Each list represents rows.
consColumns([HFirst|TFirst], [HRest|TRest], [[HFirst|HRest]|TResult]) :-
  consColumns(TFirst, TRest, TResult).

% Pre-base case
% When taking the transpose of one column, the transpose must be a list of lists of one element each
consColumns([HFirstCol|TFirstCol], [], [[HFirstCol]|TResMtx]) :-
  consColumns(TFirstCol, [], TResMtx).

% Base Case
consColumns([],A,A).


