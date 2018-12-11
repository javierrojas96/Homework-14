library(dplyr)

show_number <- function(m, i, oriented=T)
{
  im <- matrix(mtrain[i,], byrow=T, nrow=28)
  
  if (oriented) {
    im_orient <- matrix(0, nrow=28, ncol=28)
    for (i in 1:28)
      im_orient[i,] <- rev(im[,i])
    
    im <- im_orient
  }
  image(im)
}


# get the training datasets 

##TRAIN
if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header=F) %>% as.matrix
  train_classification <- mtrain[,1]  # y values
  y <- factor(train_classification, levels=c(0,1))
  mtrain <- mtrain[,-1]/256  # x matrix
  colnames(mtrain) <- 1:(28^2)
  x <- mtrain[1:1000,]
  colnames(mtrain) <- NULL
  rownames(mtrain) <- NULL
}

y <- rep(NA, length(train_classification))

for (i in 1:length(train_classification)){
  cn <- train_classification[i]

  if (cn == 3){
    cn <- 1
  }
  else {
    cn <- 0
  }
  y[i] <- cn

}

y <- factor(y, levels = c(0,1))
y <- y[1:1000]


##TEST 

mtest <- read.csv("mnist_train.csv", header=F) %>% as.matrix
train_classification <- mtest[,1]  # y values
test.y <- factor(train_classification, levels=c(0,1))
mtest <- mtest[,-1]/256  # x matrix
colnames(mtest) <- 1:(28^2)
test.x <- mtest[1001:2000,]
colnames(mtest) <- NULL
rownames(mtest) <- NULL
#}

y <- rep(NA, length(train_classification))

for (i in 1:length(train_classification)){
  cn <- train_classification[i]
  
  if (cn == 3){
    cn <- 1
  }
  else {
    cn <- 0
  }
  y[i] <- cn
  
}

test.y <- factor(y, levels = c(0,1))
test.y <- y[1001:2000]




# look at a sample
show_number(x, 4)

