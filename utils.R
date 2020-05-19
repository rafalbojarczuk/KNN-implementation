err <- function(y_pred, y_true){
  sum(y_pred != y_true)/length(y_pred)
}

mad <- function(y_pred, y_true){
  sum(abs(y_pred - y_true))/length(y_pred)
}

mse <- function(y_pred, y_true){
  sum((y_pred - y_true)^2)/length(y_pred)
  
}


normalize_columns <- function(X){
  apply(X, MARGIN=2, FUN=function(col) return((col-mean(col))/std(col)))
}

