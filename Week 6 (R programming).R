

# 1. Consider A = matrix(c(2,0,1,3), ncol=2) and B = matrix(c(5,2,4,-1), ncol=2)

A = matrix(c(2,0,1,3), ncol=2)
B = matrix(c(5,2,4,-1), ncol=2)

# > A
#       [,1] [,2]
# [1,]    2    1
# [2,]    0    3
# 
# > B
#       [,1] [,2]
# [1,]    5    4
# [2,]    2   -1

# a) Find A + B

A + B
#       [,1] [,2]
# [1,]    7    5
# [2,]    2    2

# b) Find A - B

A - B
#       [,1] [,2]
# [1,]   -3   -3
# [2,]   -2    4

# 2.  Using the diag() function to build a matrix of size 4 
#     with the following values in the diagonal 4,1,2,3.

diag(c(4,1, 2, 3), 4, 4)

#       [,1] [,2] [,3] [,4]
# [1,]    4    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    2    0
# [4,]    0    0    0    3

# 3. Generate the following matrix:
#   
#   ##      [,1]  [,2]  [,3]  [,4]  [,5]
#   ## [1,]   3     1     1     1     1
#   ## [2,]   2     3     0     0     0
#   ## [3,]   2     0     3     0     0
#   ## [4,]   2     0     0     3     0
#   ## [5,]   2     0     0     0     3
#   Hint: You can use the command diag() to build it.

C <- diag(c(3), 5, 5)
C[,1] <- C[,1] + 2
C[1,] <- C[1,] + 1
C[1,1] <- 3

#       [,1] [,2] [,3] [,4] [,5]
# [1,]    3    1    1    1    1
# [2,]    2    3    0    0    0
# [3,]    2    0    3    0    0
# [4,]    2    0    0    3    0
# [5,]    2    0    0    0    3