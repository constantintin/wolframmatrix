# wolframmatrix
Easily create matrix strings to be used by wolframalpha.
## Usage

``` sh
wolframmatrix-exe "1 2 3 4 5+x 6 7 8+x 19-x"
{{1,2,3},{4,5+x,6},{7,8+x,19-x}}

1   2    3
4 5+x    6
7 8+x 19-x
    
wolframmatrix-exe "1 2 3 4 5+x 6 7 8+x 19-x x" 2
{{1,2},{3,4},{5+x,6},{7,8+x},{19-x,x}}

   1   2
   3   4
 5+x   6
   7 8+x
19-x   x
```

