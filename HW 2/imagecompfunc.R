library(readr)

# read the csv files into data frames
image1 <- read_csv("images/image1.csv")
image2 <- read_csv("images/image2.csv")
image3 <- read_csv("images/image3.csv")
image4 <- read_csv("images/image4.csv")

# convert data frames into matrices
image1 <- data.matrix(image1)
image2 <- data.matrix(image2)
image3 <- data.matrix(image3)
image4 <- data.matrix(image4)

# image compression function alternative
# compress_image2 <- function(X, k) { # X = n x p image matrix, k = number of principal components
#   V <- t(X) %*% X # V = X'X
#   
#   eigendecomposition <- eigen(V) # eigendecomposition of V: V = U^U'
#   U <- eigendecomposition$vectors # U contains the principal component directions
#   
#   Z <- X %*% U # Z = XU where Z contains the principal component scores
#   
#   Zk <- Z[, 1:k, drop = FALSE] # keep only the first k principal components
#   Uk <- U[, 1:k, drop = FALSE] 
#   
#   Xk <- Zk %*% t(Uk) # rank k approximation: X^(k) = z1u'1 + z2u'2 + ... + zku'k
#   
#   error <- norm(X - Xk, type = "F") # calculate the Frobenius norm error
#   
#   return(list(approximation = Xk, error = error)) # return the rank k approximation and the error
# }
# 
# compress_image2(image1, 10)

compress_image <- function(X, k) {
  pca <- prcomp(X, center = FALSE, scale. = FALSE) # perform PCA on image matrix without centering or scaling
  
  Z <- pca$x # principal component scores
  U <- pca$rotation # principal component directions
  
  Zk <- Z[, 1:k, drop = FALSE] # keep only the first k principal components
  Uk <- U[, 1:k, drop = FALSE]
  
  Xk <- Zk %*% t(Uk) # rank k approximation: X^(k) = z1u'1 + z2u'2 + ... + zku'k
  
  error <- norm(X - Xk, type = "F") # calculate the Frobenius norm error
  
  return(list(approximation = Xk, error = error)) # return the rank k approximation and the error
}

compress_image(image1, 10)
