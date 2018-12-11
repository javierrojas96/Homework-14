# 1-dimensional data fitting

# install two packages:  nnet and caret

# introduction to caret: http://topepo.github.io/caret/index.html
library(caret)
library(dplyr)

# make a toy dataset
# make_toy_dataset_2d <- function(noise_frequency=0, outfile="toy_data_2d.csv") 
# {
#   x <- expand.grid(seq(-2,2,.1), seq(-2,2,.1)) %>% as.matrix
#   y <- rep(0, nrow(x))
#   for (i in 1:nrow(x)) {
#     if (x[i,1]^2 + 2*x[i,2]^2 < 1)
#       y[i] <- 1
#     if (abs(x[i,1] - x[i,2]) < .1)
#       y[i] <- 1
#     if (runif(1) < noise_frequency)
#       y[i] <- 1 - y[i]
#   }
#   y <- factor(y)
#   
#   plot(x[,1], x[,2], col=as.numeric(y))
#   
#   df <- data.frame(x1=x[,1], x2=x[,2], y=y)
#   write.csv(df, outfile, row.names = F)
# }
# 
prediction_errors <- function(x, y, t_out)
{
 
  y <- factor(y, levels=c(0, 1))

  true_y <- y
  pred_y <- predict(t_out, x)

  n_samples <- nrow(x)
  error <- sum(true_y != pred_y)/n_samples
  return (error)
}
# 
# # let's try and fit with a neural net.  
# # make three datasets
# make_toy_dataset_2d(noise_frequency=.2, outfile="toy_data_2d_train1.csv")
# make_toy_dataset_2d(noise_frequency=.2, outfile="toy_data_2d_train2.csv")
# make_toy_dataset_2d(noise_frequency=.2, outfile="toy_data_2d_test.csv")
# 
# # use train1 to train, train2 to model select, 
# toy_data <- read.csv("toy_data_2d_train1.csv", header=T)
# toy_data$y <- factor(toy_data$y, levels=c(0, 1))
# 
# x <- cbind(toy_data$x1, toy_data$x2)
# colnames(x) <- c("x1", "x2")
# y <- toy_data$y





# fit the data to a neural net, nnet model in caret
# the nnet model has the following parameters:  size, decay
tuning_df <- data.frame(size=9, decay=0)

fitControl <- trainControl(method="none")

 fitControl <- trainControl(## 10-fold CV
   method = "repeatedcv",
   number = 2,
   ## repeated ten times
   repeats = 3)
 


t_out <- caret::train(x=x, y=y, method="nnet",
                      trControl = fitControl,
                      tuneGrid=tuning_df, maxit=1000, MaxNWts = 10000)



#TRAIN
train_error <- prediction_errors(x, y, t_out)


#TEST 
test_error <- prediction_errors(test.x, test.y, t_out)




