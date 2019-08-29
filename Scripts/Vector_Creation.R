# Vector creation with some variations
# Last updated on 2019.08.28 @ 10:00pm
# Update: restructure examples and add comparisons

# references
# https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/seq
# https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/rep


# create a vector of integers 1 to 10
# use all-purpose concatenate function
# works, but quickly becomes unwieldy

V01 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

print(V01)


# take advantage of the vector's pattern
# the : operator is a special case of seq
# it is specifically made for counting by 1
# check out the vector type in environment
# you can check with the class() function
# do not print out very long vectors!!!

V02 <- 1:10

class(V02)
print(V02)


# make vector of integers using sequence
# seq(from = , to = , by = , length.out = )
# acceptable but not as simple as above
# the default "from" value in seq is 1
# the default "by" value in seq is 1
# notice how the defaults can be used
# look at vector type in environment
# or you can run class() on each one

V03a <- seq(from = 1, to = 10, by = 1)
V03b <- seq(1, 10, 1)
V03c <- seq(1, 10)
V03d <- seq(10)
V03e <- seq.int(10)
V03f <- seq(1, 10, length.out = 10)

print(V03a)
print(V03b)
print(V03c)
print(V03d)
print(V03e)
print(V03f)


# check whether contents are different
# also check if they are the same type

V02 != V03a         # each element diff
sum(V02 != V03a)    # total number diff
which(V02 != V03a)  # locations of diff

class(V02) == class(V03a) # same class?


# integer vector created using a loop
# very unnecessary for a simple vector
# however, pay attention to structure
# we will use this type of loop later
# the [i] fills locations sequentially 
# what does V04 <- integer(10) do here?

V04 <- integer(10)

for (i in 1:10) {
  V04[i] <- i
}

print(V04)


# integer vector created using a loop
# very unnecessary for a simple vector
# however, pay attention to structure
# this illustrates many useful tools 
# more rep examples included later on

n <- 10

V05 <- c(1, rep(0, n))

for (i in 2:10) {
  V05[i] <- V05[i - 1] + 1
}


# numeric vector created using a loop
# prints out the vector at each step
# note what happens in each iteration

V06 <- integer(10)

for (i in 1:10) {
  V06[i] <- i
  print(V06)
}


# numeric vector created using a loop
# print vector at each step, fancier!

V07 <- integer(10)

for (i in 1:10) {
  V07[i] <- i
  print(sprintf("At the end of Loop %d, the vector is {%s}.", i, paste(V07, collapse = ", ")))
}


# without pesky quotes in the print

V08 <- integer(10)

for (i in 1:10) {
  V08[i] <- i
  print(sprintf("At the end of Loop %d, the vector is {%s}.", i, paste(V08, collapse = ", ")), 
        quote = FALSE)
}

# ---------------------------------
# some examples of the rep function

V09 <- rep(V01, times = 2)

V10 <- rep(V01, each = 2)

V09 != V10         # each element diff
sum(V09 != V10)    # total number diff
which(V09 != V10)  # locations of diff

class(V09) == class(V10) # same class?

# if we don't specify times or each
# is V09 like V07 or V08? or neither?

V11 <- rep(V01, 2)

# what happens if we use a vector?

V12 <- rep(V01, c(3, 6, 7, 2, 4, 9, 1, 2, 4, 1))

table(V12)


# ---------------------------------
# more examples of the seq function

# same-spaced vector of given length

V13 <- seq(0, 1, length.out = 101)
print(V13)

# increment down and non-integer step

V14 <- seq(10, 0, -0.1)
print(V14)

# increment down but using length.out

V15 <- seq(10, 0, length.out = 101)
print(V15)

# are the vector results the same?

sum(V15 != V15)
which(V14 != V15)
class(V14) == class(V15)


# ---------------------------------
# Simple loop example w/ two vectors

n <- 10

X       <- numeric(n)
Xsquare <- numeric(n)

for (i in 1:n) {
  X[i]       <- i
  Xsquare[i] <- i^2
}




