:-[tests].
:-use_module(library(clpfd)).
:-set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]):-
    copyRow(Row,RowS),
    copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]):-
    copyRow(R,S).
copyRow([Clue|R],[Clue|S]):-
    copyRow(R,S).

snake(RowClues,ColClues,Grid,Solution):-
    copyGrid(Grid,Solution),
   % checkRowClues(Solution,RowClues),
   % checkColClues(Solution,ColClues),
   % nonTouching(Solution),
    countNeighbors(Solution)
   % snakeConnected(Solution)
   .

count_cell(0,X):-X is 0.
count_cell(_,X):-X is 1.

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W):-
    1 #=< Piece,
    count_cell(N,X1),
    count_cell(E,X2),
    count_cell(S,X3),
    count_cell(W,X4),
    Piece #= X1+X2+X3+X4.

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]):-
    check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).
