

# Clearing the Environment
  rm(list = ls(all = TRUE))

# Creating some Custom Functionalities for performing Logistic Regression
  
  # KNN-main functionality
    my_knn_regressor = function(x,y,k=5){
      if (!is.matrix(x))
      {
        x = as.matrix(x)
      }
      if (!is.matrix(y))
      {
        y = as.matrix(y)
      }
      my_knn = list()
      my_knn[['points']] = x
      my_knn[['value']] = y
      my_knn[['k']] = k
      attr(my_knn, "class") = "my_knn_regressor"
      return(my_knn)
    }
  
  # Pair-Wise Distances calculation functionality
    compute_pairwise_distance=function(X,Y){
      xn = rowSums(X ** 2)
      yn = rowSums(Y ** 2)
      outer(xn, yn, '+') - 2 * tcrossprod(X, Y)
    }
    
  # Prediction Functionality for KNN-algorithm
    predict.my_knn_regressor = function(object,x){
      if (!is.matrix(x))
      {
        x = as.matrix(x)
      }
      ##Compute pairwise distance
      dist_pair = compute_pairwise_distance(x,object[['points']])
      ##as.matrix(apply(dist_pair,2,order)<=my_knn[['k']]) orders the points by distance and select the k-closest points
      ##The M[i,j]=1 if x_j is on the k closest point to x_i
      crossprod(apply(dist_pair,1,order) <= object[['k']], object[["value"]]) / object[['k']]
    }

# Implementation --------------------------------------------------------------------

  # Data Prep
    x_data = iris[,c(1:3)]
    y_data = iris[,4]
    
  # Model Implementation
    knn_model = my_knn_regressor(x = x_data, y = y_data, k = 5)
  
  # Predictions
    y_hat = predict(object = knn_model, x = x_data)
    
  # Getting y into one variable for easier predictions
    y = as.numeric(iris$Petal.Width)
  
  # Evaluating the y(Actual_datapoints) and y_hat(predicted_datapoints)
    regr.eval(trues = y, preds = y_hat)

    
    
    
      