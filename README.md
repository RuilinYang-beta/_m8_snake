# Snake - Logic Programming project



## update Thu 28 May 00:50
* nonTouching predicate still doesn't work as expected, see temp-visualizeSolutions.pl for details.
* suggestion on improvment: 
	* For diagonal touching: 
		comment snake/4 like this:  
		```
		snake(RowClues, ColClues, Grid, Trimmed) :- 
	        copyGrid(Grid,Copied),
	        % checkRowClues(Copied, RowClues),
	        % checkColClues(Copied, ColClues),
	        extend_grid(Copied, Extended),
	        countNeighbors(Extended), 
	        % checkConnectivity(Extended),
	        nonTouching(Extended),
	        trim(Extended, Trimmed).
		```
		 This will give you 16 solutions for p3x3, see if nonTouching can 
		 rule out the 2 patterns: 
		 ```
		 # 0
		 0 #
		 ```

		 ```
		 0 #
		 # 0
		 ```
	* For body head touching: 
		The 16 solutions described above doesn't have patterns to rule out. 
		So you have to come up with dummy data to see if it work,
		for example this shouldn't work: 
		
		```
		2 0 0 
		0 1 0
		0 2 0
		```








