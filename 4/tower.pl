% ==============================
%     tower() Implementation
% ==============================

% A1: non-negative integer specifying the size of the grid
% A2: a list of N lists. Each list represents a row in the grid.
% A3: a structure with function symbol counts and arity 4.
%         Its arguments are all lists of N integers, and represent
%         the tower counts for the top, bottom, left, and right edges, 
%         respectively.

tower(Size, Grid, counts(Top, Bot, Left, Right)) :-
  left_tower_count_check(Grid, Left, Size, Size),
  right_tower_count_check(Grid, Right, Size, Size),
  transpose(Grid, Transpose),
  left_tower_count_check(Transpose, Top, Size, Size),
  right_tower_count_check(Transpose, Bot, Size, Size).

% left_tower_count_check: Checks the matrix against the count representing
%                         the count of towers seen from the left edge
%                         of the matrix.
% A1: A matrix to check. It is expected to be represented as a list of
%     lists, where each list represents a row in the matrix.
% A2: A count of towers seen from the left side of the matrix represented
%     as a list of integers. The first element of the list is the count
%     for the top most row and the last element is the count of the bottom
%     most row.
% A3: An integer representing the size of the matrix. The matrix is 
%     expected to be a square matrix.
% A4: An integer representing the number of rows that still needs to be
%     checked.

left_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  tower_valid_list(First_row, Size),
  count_check(First_row, First_count, 0),
  Iterations0 #=# Iterations - 1,
  left_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

% Base Case
left_tower_count_check([], [], _, 0).

% right_tower_count_check: Checks the matrix against the count representing
%                         the count of towers seen from the left edge
%                         of the matrix.
% A1: A matrix to check. It is expected to be represented as a list of
%     lists, where each list represents a row in the matrix.
% A2: A count of towers seen from the left side of the matrix represented
%     as a list of integers. The first element of the list is the count
%     for the top most row and the last element is the count of the bottom
%     most row.
% A3: An integer representing the size of the matrix. The matrix is 
%     expected to be a square matrix.
% A4: An integer representing the number of rows that still needs to be
%     checked.

right_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  reverse(First_row, Reversed),
  tower_valid_list(Reversed, Size),
  count_check(Reversed, First_count, 0),
  Iterations0 #=# Iterations - 1,
  right_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

% Base Case
right_tower_count_check([], [], _, 0).

% tower_valid_list: Verifies that a list does not a duplicate values and 
%                   only consists of values between 1 and Size.
% A1: A list to be checked.
% A2: The specified size of the list
tower_valid_list(List, Size) :-
  length(List, Size),
  fd_domain(List, 1, Size),
  fd_all_different(List).

% count_check: Counts the number of towers seen in the list.
% A1: A list to count towers from.
% A2: A count of the towers seen in the list.
% A3: Internal variable used to iterate recursively. The caller should always 
%     use 0 as this argument.
count_check([First|Tail], Count, Tallest) :-
  First #># Tallest,
  Count0 #=# Count - 1,
  count_check(Tail, Count0, First);
  First #<# Tallest,
  count_check(Tail, Count, Tallest).

% Base Case
count_check([], 0, _).

% ==================================
%     plain_tower Implementation
% ==================================

% A1: non-negative integer specifying the size of the grid
% A2: a list of N lists. Each list represents a row in the grid.
% A3: a structure with function symbol counts and arity 4.
%         Its arguments are all lists of N integers, and represent
%         the tower counts for the top, bottom, left, and right edges, 
%         respectively.

plain_tower(Size, Grid, counts(Top, Bot, Left, Right)) :-
  plain_left_tower_count_check(Grid, Left, Size, Size),
  plain_right_tower_count_check(Grid, Right, Size, Size),
  transpose(Grid, Transpose),
  plain_left_tower_count_check(Transpose, Top, Size, Size),
  plain_right_tower_count_check(Transpose, Bot, Size, Size).

% plain_left_tower_count_check: Checks the matrix against the count representing
%                         the count of towers seen from the plain_left edge
%                         of the matrix.
% A1: A matrix to check. It is expected to be represented as a list of
%     lists, where each list represents a row in the matrix.
% A2: A count of towers seen from the plain_left side of the matrix represented
%     as a list of integers. The first element of the list is the count
%     for the top most row and the last element is the count of the bottom
%     most row.
% A3: An integer representing the size of the matrix. The matrix is 
%     expected to be a square matrix.
% A4: An integer representing the number of rows that still needs to be
%     checked.

plain_left_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  valid_list(First_row, Size),
  length([First_row|Rest_rows],Iterations),
  Iterations0 is Iterations - 1,
  plain_count_check_wrapper(First_row, First_count, Size),
  plain_left_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

% Base Case
plain_left_tower_count_check([], [], _, 0).

% plain_right_tower_count_check: Checks the matrix against the count representing
%                         the count of towers seen from the left edge
%                         of the matrix.
% A1: A matrix to check. It is expected to be represented as a list of
%     lists, where each list represents a row in the matrix.
% A2: A count of towers seen from the left side of the matrix represented
%     as a list of integers. The first element of the list is the count
%     for the top most row and the last element is the count of the bottom
%     most row.
% A3: An integer representing the size of the matrix. The matrix is 
%     expected to be a square matrix.
% A4: An integer representing the number of rows that still needs to be
%     checked.

plain_right_tower_count_check([First_row|Rest_rows], [First_count|Rest_count], Size, Iterations) :-
  reverse(First_row, Reversed),
  valid_list(Reversed, Size),
  length([First_row|Rest_rows],Iterations),
  plain_count_check_wrapper(Reversed, First_count, Size),
  Iterations0 is Iterations - 1,
  plain_right_tower_count_check(Rest_rows, Rest_count, Size, Iterations0).

% Base Case
plain_right_tower_count_check([], [], _, 0).

valid_list(List, Size) :-
  length(List, Size),
  create_sorted(Size, Sorted_list),
  permutation(Sorted_list, List).

create_sorted(Size, List) :-
  length(List, Size),
  append(List0, [Size], List),
  Size0 is Size - 1,
  create_sorted(Size0, List0).

create_sorted(0, []).

plain_count_check_wrapper(List, Count, Size) :-
  valid_list(List, Size),
  plain_count_check(List, Count, 0).

plain_count_check([], 0, _).

plain_count_check([H|T], Count, Tallest) :-
  H > Tallest,
  plain_count_check(T, Count0, H),
  Count is Count0 + 1;
  H < Tallest,
  plain_count_check(T, Count, Tallest).

% ================================
%     Transpose Implementation
% ================================

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


