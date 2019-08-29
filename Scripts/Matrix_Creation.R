# Basic matrix creation and naming
# Last updated 2019.08.27 @ 08:00pm
# Update: Work in progress!

V1 <- 101:120
matrix(V1, nrow = 2)
matrix(V1, nrow = 4)
matrix(V1, nrow = 5)
matrix(V1, nrow = 10)

V2 <- 81:100
matrix(V2, ncol = 10)
matrix(V2, ncol = 5)
matrix(V2, ncol = 4)
matrix(V2, ncol = 2)

M <- matrix(2 * V1, ncol = 4)
colnames(M) <- c("Col A", "Col B", "Col C", "Col D")
rownames(M) <- c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5")
dimnames(M)
print(M)
M[1, 2]
M["Row 1", "Col B"]
M["Row 1", ]
M[ ,"Col B"]

X_value   <- 1:20
X_squared <- X_value^2
X_cubed   <- X_value^3
DAT <- data.frame(X_value, X_squared, X_cubed)
print(DAT)