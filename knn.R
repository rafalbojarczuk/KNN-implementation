
library(pracma)
distances <- function(x, y, p){
  #calculating Minkowski distance between vectors x and y
  if(p != Inf){
    return(nthroot(sum(abs(x - y)^p), p))
  }
  else{
    return(max(abs(x - y)))
  }
}
sample.vec <- function(vec){
  #samples one value from vector (even from vector of length one)
  vec[sample(length(vec), 1)]
}

knn <- function(X, y, Z, k, p){
  #' K-NN
  #'
  #' X - matrix of known observations
  #' y - vector of corresponding labels
  #' Z - matrix containing new rows of data
  #' k - number of neighbors
  #' p - power for calculating Minkowski's distance Lp
  #' function returns matrix of size nrow(Z) x k - containing labels of k closest neighbors to each row of Z
  #' 
  X <- split(X, row(X))
  test <- split(Z, row(Z))
  neirest_neighbors <- vector(mode = "list", length = nrow(Z))
  for(i in 1:nrow(Z)){
    distances <- mapply(distances, X, test[i], p=p)
    indexes <- order(distances)[1:k]
    neirest_neighbors[[i]] <- y[indexes]
  }
  return(do.call(rbind, neirest_neighbors))
}



mode <- function(vec){
  count_table <- tabulate(vec)
  return(sample.vec(which(count_table == max(count_table))))
}

srednia_a <- function(vec)
{
  m <- sum(vec)/length(vec)
  distances_from_mean <- abs(vec-m)
  index <- sample.vec(which(distances_from_mean == min(distances_from_mean)))
  return(vec[index])
}

median <- function(vec){
  ordered <- order(vec)
  if(length(vec)%%2==0){
    index <- sample(c(ordered[length(vec)%/%2], ordered[length(vec)%/%2+1]), 1)
  }
  else{
    index <- ordered[length(vec)%/%2+1]
  }
  return(vec[index])
}

minkara1.5 <- function(vec){
  loss <- function(u, vec){
    sum(abs(vec-u)^1.5)
  }
  u <- unique(vec)
  losses <- sapply(u, FUN=loss, vec=vec)
  index <- sample.vec(which(losses == min(losses)))
  return(u[index])
}

minkara3.0 <- function(vec){
  loss <- function(u, vec){
    sum(abs(vec-u)^3)
  }
  u <- unique(vec)
  losses <- sapply(u, FUN=loss, vec=vec)
  index <- sample.vec(which(losses == min(losses)))

  return(u[index])
}

#Funkcja moda() robi to samo co classify_for_ord_regression(.., fun=mode)
#mozna by zredukowaÄc kod
#ale uznazleem, ze moze dobrym pomyslem bedzie zostawic oddzielna funkcje dla 
#zadania klasyfikacji od funkcji dla zadania regresji porzÄ…dkowej
moda <- function(matrix){
  #' function returns mode of each row of matrix

  return(apply(matrix, MARGIN = 1, FUN = mode))
}

classify_for_ord_regression <- function(matrix, fun){
  #' function applies fun to every row of matrix

  return(apply(matrix, MARGIN = 1, FUN = fun))
}

