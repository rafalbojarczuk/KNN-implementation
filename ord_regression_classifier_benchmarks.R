source('knn.R')
source('utils.R')
#Loading data

wine_quality <- read.csv("winequality-red.csv")
affairs <- read.csv("affairs.csv")
cement_strength <- read.csv("cement_strength.csv")
wisconsin_breast <- read.csv("wisconsin_breast_ord.csv")
skill <- read.csv("skill.csv")

create_benchmarks <- function(dataframes, df_names, m = 5){
  #to test the classifier we use m-fold cross validation, we set m=5
  set.seed(997)
  n <- 1
  for(df in dataframes){
    #shuffling dataframe rows and splitting to labels and 
    shuffle <- sample(nrow(df))
    shuffled <- df[shuffle, ]
    y <- shuffled[,1]
    X <- normalize_columns(as.matrix(shuffled[,-1]))
    split_index <- nrow(df)%/%m
  
    benchmark_df <- data.frame(AggregatingFunction=character(),
                               k=integer(),
                               p=double(),
                               ERR=double(),
                               MAD=double(),
                               MSE=double())  
    
    functions <- c(mode, srednia_a, median, minkara1.5, minkara3.0)
    function_names <- c("mode", "srednia_a", "median", "minkara1.5", "minkara3.0")
    neirest_neighbors <- seq(1,19,2)
    metrics <- c(1,2,Inf)
    
    for(j in 1:length(functions)){
        for(p in metrics){
          for(k in neirest_neighbors){
            cat("Evaluating k-nn for: k=", k, "p=", p, "aggregating function=", function_names[j], "- dataframe", df_names[n], "\n")
            ERR <- 0
            MAD <- 0
            MSE <- 0
            #m-fold cross validation
            for(i in 1:m){
              #splitting to test and training sample
              test_start_idx <- (i-1)*split_index+1
              test_end_idx <- i*split_index
              X_test <- X[test_start_idx:test_end_idx, ]
              y_test <- y[test_start_idx:test_end_idx]
              X_train <- X[-(test_start_idx:test_end_idx), ]
              y_train <- y[-(test_start_idx:test_end_idx)]
              
              #better idea would be to calculate knn once for maximum value of k (19) and then take first 1,3,5... etc neighbors
              #but now having written m-fold cross validation split in most inner loop it is troublesome to refactor the code
              nn_matrix <- knn(X_train, y_train, X_test, k, p)
              y_pred <- classify_for_ord_regression(nn_matrix, functions[[j]])
              ERR <- ERR + err(y_pred, y_test)
              MAD <- MAD + mad(y_pred, y_test)
              MSE <- MSE + mse(y_pred, y_test)
            }
            new_row <- data.frame(AggregatingFunction=function_names[j], k=k, p=p, ERR=ERR/m, MAD=MAD/m, MSE=MSE/m)
            benchmark_df <- rbind(benchmark_df, new_row)
        }
      }
    }
    file_name <- paste(df_names[n], "_benchmark", ".csv", sep="")
    write.csv(benchmark_df, file_name)
    n <- n+1
  }
}

data <- list(wine_quality, affairs, cement_strength, wisconsin_breast, skill)
dataframe_names <- c("wine_quality", "affairs", "cement_strength", "wisconsin_breast", "skill")

create_benchmarks(data, dataframe_names)