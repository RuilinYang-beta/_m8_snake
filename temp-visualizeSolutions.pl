

%% ==============================================================
%% ========================= questions =========================
%% ==============================================================

% ---- [1] about the definition of non-touching, what is not allowed? ----- 

Got 2 solutions of p3x3b (without nonTouching), are they both correct?
they seem both correct to me

?- test(p3x3b, _).
 - [S][#]           
[S] - [#] 			
[#][#][#]
true ;    

 - [S] - 
[S][#] - 
 -  -  - 
true ;    

% ---- [2] about the nonTouching predicate ----- 
seems like it is ommitting solutions that are correct, need investigation. 
see below:
?- test(p3x3b, _).
?- test(p5x5_two, _).         
?- test(pCycle, _).
?- test(p4x4, _).
?- test(p5x5, _).



%% ==============================================================
%% ======================== my solutions ========================
%% ==============================================================

%% ---------------

?- test(p2x2, _).
[S][S]
 -  - 

true ;

%% ---------------


Got 2 solutions of p3x3b, are they both correct?
?- test(p3x3b, _).
 - [S][#]            this solution is ommitted by nonTouching,
[S] - [#] 			 but it seems correct to me? 
[#][#][#]

true ;           
 - [S] - 
[S][#] - 
 -  -  - 

%% ---------------  this one below takes very long 

?- test(p5x5_two, _).     
 -  - [S][#][#]           this solution is ommitted by nonTouching
 -  -  -  - [#]
[#][#][#][#][#]
[#] -  -  -  - 
[#][#][S] -  - 

true ;
[#][#][S] -  - 			  this solution is ommitted by nonTouching too
[#] -  -  -  - 
[#][#][#][#][#]
 -  -  -  - [#]
 -  - [S][#][#]

true 

%% ---------------  

?- test(pCycle, _).
[S] - [S]
[#] - [#]   			  this solution is ommitted by nonTouching
[#] - [#]
[#] - [#]
[#][#][#]

true ;

%% ---------------  the first one below shouldnt be allowed

?- test(p4x4, _).
 - [S] -  -             this one should be ruled out by nonTouching     
 - [#][#][#]			and it did
[S] -  - [#]
[#][#][#][#]

true ;
 - [S][#] -             but this one is also (wrongly) ommitted
 -  - [#][#]
[S] -  - [#]
[#][#][#][#]

true ;

%% ---------------

?- test(p5x5, _).
 - [S][#] -  -          this one is also (wrongly) ommitted
 -  - [#] -  - 
[S] - [#][#][#]
[#] -  -  - [#]
[#][#][#][#][#]

true ;

%% --------------- this one takes forever

?- test(p7x7, _).


%% ---------------


%% ---------------


%% ==============================================================
%% ============== how does the solutions look like ==============
%% ==============================================================
%% 
%% ?- print_solution(p4x4).
%%  - [S][#] - 
%%  -  - [#][#]
%% [S] -  - [#]
%% [#][#][#][#]
%% 
%% 
%% 
%% ?- print_solution(p5x5).
%%  - [S][#][#] - 
%%  -  -  - [#][#]
%% [S][#] -  - [#]
%%  - [#] - [#][#]
%%  - [#][#][#] - 
%% 
%% 
%% 
%% ?- print_solution(p7x7).
%%  - [S] -  - [#][#][#]
%% [#][#] - [#][#] - [#]
%% [#] -  - [#] -  - [#]
%% [#][#][#][#] - [#][#]
%%  -  -  -  -  - [#] - 
%%  -  -  - [S] - [#] - 
%%  -  -  - [#][#][#] - 
%% 
%% 
%% 
%% ?- print_solution(p10x10).
%% [#][#][#][#][#][#][#] - [#][S]
%% [#] -  -  -  -  - [#] - [#] - 
%% [#] -  - [#][#][#][#] - [#] - 
%% [#][#] - [#] -  -  -  - [#][#]
%%  - [#] - [#] - [S] -  -  - [#]
%% [#][#] - [#][#][#] -  - [#][#]
%% [#] -  -  -  -  -  -  - [#] - 
%% [#][#][#][#][#][#][#] - [#][#]
%%  -  -  -  -  -  - [#] -  - [#]
%%  -  -  -  -  -  - [#][#][#][#]
%% 
%% 