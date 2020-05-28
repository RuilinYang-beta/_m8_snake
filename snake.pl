:-[tests].
:-use_module(library(clpfd)).
:-set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes


snake(RowClues, ColClues, Grid, Trimmed) :-
        copyGrid(Grid,Copied),
        checkRowClues(Copied, RowClues),
        checkColClues(Copied, ColClues),
        extend_grid(Copied, Extended),
        countNeighborsAndNonTouching(Extended),
        checkConnectivity(Extended),
        trim(Extended, Trimmed).


%% ==============================================================
%% =================== constrain on RowClues ====================
%% ==============================================================

checkRowClues([], []).

checkRowClues([Row|Rows], [Clue|Clues]) :-
        Clue #> -1,
        Row ins 0..2,
        sumRow(Row, Clue),
        checkRowClues(Rows, Clues).

checkRowClues([Row|Rows], [Clue|Clues]) :-
        Clue #= -1,
        Row ins 0..2,
        checkRowClues(Rows, Clues).


sumRow(Row, Sum) :- sumRow(Row, 0, Sum).

sumRow([], Acc, Acc).
sumRow([R|Row], Acc, Sum) :-
        count_cell(R, C),
        NewAcc is Acc + C,
        sumRow(Row, NewAcc, Sum).


%% ==============================================================
%% =================== constrain on ColClues ====================
%% ==============================================================

checkColClues(Grid, ColClues) :-
        transpose(Grid, TransGrid),
        checkRowClues(TransGrid, ColClues).

%% ==============================================================
%% ========== constrain neighbors on N/E/S/W direction ==========
%% =========== call check diagonal and head touching ============
%% ==============================================================

%% idea:
%% 1. expand the grid, surrounding the original grid by padding row & col of 0s
%% 2. recursively check all the rows in a grid, by looking at 3 rows a time
%% 3. recursively check all the cells in a row, by looking at a 3x3 subgrid a time
%% 4. check the middle cell, with regards to its 4 neighbors


% ----- [0] the copy function ----- 

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[Var|S]) :- copyRow(R,S),  Var in 0\/2, !.      
copyRow([Clue|R],[Clue|S]) :- Clue #\= -1, copyRow(R,S).


% Some remarks: 
% 1. For copyRow([-1|R],[Var|S]), if -1 applied with this rule, 
%    dont need to try the 3rd rule, hence the ! at the end. 
% 2. To ensure there are only two 1s on the grid (one for head, one for tail), 
%    what is a -1 must be either 0 or 2, impossible to be 1. 

% ----- [1] add padding 0s -----

extend_grid(OldGrid,NewGrid) :-
        transpose(OldGrid,TransGrid),
        extend_grid_rows(TransGrid,RowTransGrid),
        transpose(RowTransGrid,RowGrid),
        extend_grid_rows(RowGrid,NewGrid).


extend_grid_rows([], []).
extend_grid_rows([R|Rs], [NewR|NewRs]) :-
        extend_row(R, NewR),
        extend_grid_rows(Rs, NewRs).


extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).


% ----- [2] check all rows -----

% base case: when there is only 2 rows left
countNeighborsAndNonTouching([R1, R2]):-
		!,checkDiagTouch(R1,R2).
countNeighborsAndNonTouching([R1, R2, R3|Rest]) :-
		check_neighbors_rows(R1, R2, R3),
		checkDiagTouch(R1,R2),
		countNeighborsAndNonTouching([R2,R3|Rest]).

% ----- [3] recursively check a row -----

% base case: when each row only has 2 element.
check_neighbors_rows([_, _], [_, _], [_, _]).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]):-
        check_neighbors_pattern(M,N,E,S,W),
        check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).


% ----- [4] check the middle cell -----

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W):-
        Piece in 1..2,
        N in 0..2,
        E in 0..2,
        S in 0..2,
        W in 0..2,
        count_cell(N,X1),
        count_cell(E,X2),
        count_cell(S,X3),
        count_cell(W,X4),
        Piece #= X1+X2+X3+X4.


count_cell(0, 0).
count_cell(1, 1).
count_cell(2, 1).


%% ==============================================================
%% ================ check for diagonal touching =================
%% ==============================================================

% ----- check that none of these patterns occur -----

touchingDiag([2,0],[0,2]).
touchingDiag([0,2],[2,0]).
touchingDiag([1,0],[0,2]).
touchingDiag([0,1],[2,0]).
touchingDiag([2,0],[0,1]).
touchingDiag([0,2],[1,0]).


checkDiagTouch([_],[_]):-!.
checkDiagTouch([Piece1,Piece2|Row1],[Piece3,Piece4|Row2]):-
		\+ touchingDiag([Piece1,Piece2],[Piece3,Piece4]),
		checkDiagTouch([Piece2|Row1],[Piece4|Row2]).


%% ==============================================================
%% ==================== check connectedness =====================
%% ==============================================================

%% this should be done in the expanded grid

%% Overall Idea: 
%% 1. On one hand, find the head and count how many parts can be traced from the head;
%% 2. On the other hand, count all snake parts regardless of connectivity
%% 3. Succeed if the above 2 numbers are equal; fail otherwise.
checkConnectivity(ExtendedGrid) :-
        findStart(ExtendedGrid, I, J), 
        traceSnake(ExtendedGrid, [I,J], CountConnected), 
        countSnake(ExtendedGrid, CountAll), 
        CountConnected == CountAll.


%% ---------- [1] [count connected parts] ----------
%% 1. Find a way to move within the grid by index, see: 
%%    https://stackoverflow.com/questions/34949724/prolog-iterate-through-matrix
%% 2. Find the start
%% 3. For every move, mark where I came from, where I am, what is my neighbor, shift focus to next nonzero neighbor
%% 4. Each move will increment Count by 1
%% 5. At last check if Count == #number of nonzero cells


% ----- [1.1] indexing a grid -----
% note: index starts at 0
matrix(Matrix, I, J, Value) :-
        nth0(I, Matrix, Row),
        nth0(J, Row, Value).


% ----- [1.2] get the index of the first occurence of 1 -----
findStart(Matrix, I, J) :- 
        matrix(Matrix, I, J, 1), !.    % find one value of 1 is enough


% ----- [1.3] count connected parts from index of head -----
% traceSnake/3: 
% count connected parts with the help of traceSnake/6, 
% which find the next move with the help of getNext/4.
%
traceSnake(Matrix, Head, Count) :- 
        traceSnake(Matrix, [-1,-1], Head, 1, 1, Count).


% ----- [1.3.2] recording number of connected parts -----
% Params:
% 1. Matrix: needless to say
% 2. [P,Q] : the previous cell
% 3. [I,J] : the current cell of focus 
% 4.  0/1  : this is a flag, indicating if I should continue to look for the next move, 
%            when it is 1, after moving I continue to search for the next move;
%            when it is 0, I stop searching and reach the conclusion, 
%            this acts as the base case. 
% 5. Acc   : accumulator, recording the number of parts found so far. 
% 6. Count : number of connected snake parts, only instantiated when flag reaches 0.
% 
traceSnake(_, [_,_], [_,_], 0, Acc, Count) :- Count = Acc.
traceSnake(Matrix, [P,Q], [I,J], 1, Acc, Count) :- 
        getNext(Matrix, [P,Q], [I,J], Next),
        (Next \== [] -> NewAcc is Acc + 1, traceSnake(Matrix, [I,J], Next, 1, NewAcc, Count);
         traceSnake(Matrix, [P,Q], [I,J], 0, Acc, Count)).


% ----- [1.3.1] get the next connected cell -----
% This is a helper function of traceSnake.
% 1. Of the 4 cells at N/E/S/W direction of the current cell [I,J], 
%    exclude cells that have value 0, 
%    exclude cells that is the same as where I came from (I dont want to head back),
%    there should be 0 or 1 option left.
% 2. If there is 0 option left, it means I have reach the end of snake trace, 
%    there is no next move, Next=[].
% 3. If there is 1 option left, that is where I should go, 
%    bind it with Next.
% 
% Params: 
% P,Q: where I came from 
% I,J: where I am
% Next: where I should go
getNext(Matrix, [P,Q],[I,J], Next) :-
        RowUp    is I-1, 
        RowDown  is I+1, 
        ColRight is J+1, 
        ColLeft  is J-1, 
        Neighbors = [[RowUp  , J],                 % North
                     [I      , ColRight],          % East
                     [RowDown, J],                 % South
                     [I      , ColLeft]            % West
                    ], 
        exclude(zeroCell(Matrix), Neighbors, NonZeroNb),
        exclude(equalPQ([P,Q]), NonZeroNb, Result),
        (length(Result, 0) -> Next = []; 
         getHead(Result, Head), Next = Head).


zeroCell(Matrix, [X,Y]) :- 
        matrix(Matrix, X, Y, V), 
        V #= 0.

equalPQ([P,Q], [X,Y]) :- 
        [X,Y] == [P,Q].
       
getHead([H|_], H).


%% ---------- [2] [count snake parts regardless of connectivity] ----------

countSnake(Grid, Count) :- countSnakeGrid(Grid, 0, Count).

countSnakeGrid([], Acc, Acc).
countSnakeGrid([R|Grid], Acc, Count) :- 
        countSnakeRow(R, 0, CountRow), 
        NewAcc is Acc + CountRow, 
        countSnakeGrid(Grid, NewAcc, Count).


countSnakeRow([], Acc, Acc).
countSnakeRow([C|Row], Acc, Count) :- 
        (C in 1..2 -> NewAcc is Acc + 1, countSnakeRow(Row, NewAcc, Count); 
         countSnakeRow(Row, Acc, Count)).





%% ==============================================================
%% =================== trim the expanded grid ===================
%% ==============================================================

trim(Extended, Result) :- 
        trimHeadLast(Extended, RowTrimmed), 
        transpose(RowTrimmed, Trans),
        trimHeadLast(Trans, TransTrimmed), 
        transpose(TransTrimmed, Result). 

trimHeadLast([_|Rest], Trimmed) :- append(Trimmed, [_], Rest).

