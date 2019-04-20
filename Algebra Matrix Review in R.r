"Algebra Matrix Review in R"

## Create Matrix


#Matrix 3 [row] x 1 [column]
a <- matrix(c(.85, .75, .65), nrow=3, ncol = 1, byrow=TRUE)
a


#Check dimensions

dim(a)

Matrix 3 x 2


x <- as.matrix(data.frame(colum1 = c(.85, .75, .65), column2 = c(1.5,1,.5)))
x


## Matrix Algebra

### Addition


# 3 x 2
x <- cbind(c(.85, .75, .65), c(1.5,1,.5))
# 3 x 2
y <- cbind(c(1, 2, 1), c(.5,.6,.5))
x
dim(x)
y
dim(y)
x + y


### Scalar Multiplication

t <- matrix(c(1.5, 1, .5), nrow=3, ncol=1, byrow = TRUE)
c = 2
t * 2


### Matrix Multiplication

Matrix multiplication uses the command `%*%`,  scalar multiplication uses `*`. Since \textbf{x} is a 3-by-2 matrix
and \textbf{y} is a 3-by-2 matrix, we cannot premultiply x by y. We need y to be 2 x 3. The result will be 3 x 3

# 3 x 2
x <- cbind(c(.85, .75, .65), c(1.5,1,.5))
# 3 x 2
y <- rbind(c(1, 2, 1), c(.5,.6,.5))

x %*% y

### Matrix Transpose
x is 3x2. Use transpose `t()` to change it to 2x3. Note we create a new matrix called z and check its dimension.

dim(x)
z <- t(x)
dim(z)


## Matrix Metadata

nrow(x)
ncol(x)
dim(x)
colSums(x)
rowSums(x)
sum(x)
colMeans(x)
rowMeans(x)
mean(x)



