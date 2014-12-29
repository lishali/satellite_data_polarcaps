# This file contains the random forest section of the analysis

library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)
library(parallel) 
library(cvTools)
library(ROCR)
library(reshape)
library(ggROC)
library(grid)
library(gridBase)
library(gridExtra)

nCores <- 4
registerDoParallel(nCores)

setwd(file.path("/Users/LishaLi/Desktop/test"))
ImageSave <- TRUE


###############################################################################
# Random Forest function catered to formatted image files with fold in last 
# column
#

rf.specific <- function(train, num.tree){
  # Takes image file and returns the random forest class
  # Input:
  #   train - Image file to train on.  It contains features in 
  #           columns 4 up to the second last.  The 3rd column contains the labels
  #   num.tree - the number of trees to use in the forest
  
  rf <- randomForest(x = train[,4:(ncol(train)-1)], 
                         y = droplevels(as.factor(train[,3])), 
                         ntree=num.tree, 
                         confusion = T, 
                         importance = T)
  return(rf)
}

################################################################################
# Generate ROC plot data 
#

roc.data <- function(rf, test){
  # Takes random forest class and another test data set in same format as the 
  # trained dataset and returns the roc S4 class as given in ROCR package
  # Input:
  #   rf - random forest class
  #   test - test dataset
  
  rf.predict.prob <- predict(rf, test[,4:(ncol(test)-1)], type = "prob")  
  preds <- rf.predict.prob[,2] #to get the percentage positive predictions
  for.roc <- prediction(preds, test[[3]])
  for.roc <- performance(for.roc, 'tpr', 'fpr')
  
  return(for.roc)
}
################################################################################
#For image ROC plots: 

roc.generate <- function(rf, test){
  rf.predict.prob <- predict(rf, test[,4:(ncol(test)-1)], type = "prob") #to calculate ROC curves
  preds <- rf.predict.prob[,2] #to get the percentage positive predictions
  for.roc <- prediction(preds, test[[3]])
  return(plot(performance(for.roc, 'tpr', 'fpr')))
}
################################################################################
# AUC values
#

auc.value <- function(rf, test){
  # Takes in a random forest class (trained on some training set), applies 
  # random forest model to a test set in the same format as the training set 
  # and gives the AUC values for the prediction generated. 
  #
  # Input:
  #   rf - an object of class random forest
  #   test - a validation set
  # Output:
  #   auc - the auc for the roc of the model
  
  rf.predict.prob <- predict(rf, test[,4:(ncol(test)-1)], type = "prob") 
  preds <- rf.predict.prob[,2] #to get the percentage positive predictions
  for.roc <- prediction(preds, test[[3]])
  auc.tmp <- performance(for.roc,"auc"); auc <- as.numeric(auc.tmp@y.values)
  return(auc)
}

################################################################################
# Confusion matrix
#

confusion.generate <- function(rf, test){
  # Takes in a random forest class (trained on some training set), applies 
  # random forest model to a test set in the same format as the training set 
  # and gives the confusion matrix of classification results.
  # Input:
  #   rf - an object of class random forest
  #   test - a validation set
  # Output:
  #   confusion.predict - a confusion matrix
  
  rf.predict <- predict(rf, test[,4:6])
  confusion.predict <- table(rf.predict, droplevels(as.factor(test[,3])))
  return(confusion.predict) 
}


################################################################################
################################################################################
##
## Analysis begins here
## 

# Here we load the three image files and set up the 12 fold validation via 
# quadrants.  The resulting file is 'combined' dataframe that contains the 
# validation folds 

image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

image1 <- tbl_df(image1)
image2 <- tbl_df(image2)
image3 <- tbl_df(image3)

# Name the columns usefully
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')

names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# Assign quadrants cross validation groups: 

image1 <- arrange(image1, (x))
half.row <- as.integer(nrow(image1)/2)
image1$block.x <- if((nrow(image1)%%2)==0){c(rep(TRUE,half.row),
                                                  rep(FALSE, half.row))
                     }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image1 <- arrange(image1, (y))
image1$block.y <- if((nrow(image1)%%2)==0){c(rep(TRUE,half.row),
                                                   rep(FALSE, half.row))
                     }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image1 <-  mutate(image1, 
                  fold = ifelse(block.x==TRUE & block.y==FALSE, 2, 1),
                  fold = ifelse(block.x==TRUE & block.y==TRUE, 3, fold), 
                  fold = ifelse(block.x == FALSE & block.y == TRUE, 4, fold))

image2 <- arrange(image2, (x))
half.row <- as.integer(nrow(image2)/2)
image2$block.x <- if((nrow(image2)%%2)==0){c(rep(TRUE,half.row),
                                                      rep(FALSE, half.row))
                        }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image2 <- arrange(image2, (y))
image2$block.y <- if((nrow(image2)%%2)==0){c(rep(TRUE,half.row),
                                                      rep(FALSE, half.row))
                        }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image2 <- mutate(image2, 
                 fold = ifelse(block.x==TRUE & block.y==FALSE, 6, 5), 
                 fold = ifelse(block.x==TRUE & block.y==TRUE, 7, fold), 
                 fold = ifelse(block.x == FALSE & block.y == TRUE, 8, fold))

image3 <- arrange(image3, (x))
half.row <- as.integer(nrow(image3)/2)
image3$block.x <- if((nrow(image3)%%2)==0){c(rep(TRUE,half.row),
                                                      rep(FALSE, half.row))
                        }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image3 <- arrange(image3, (y))
image3$block.y <- if((nrow(image3)%%2)==0){c(rep(TRUE,half.row),
                                                      rep(FALSE, half.row))
                        }else{c(rep(TRUE, half.row), rep(FALSE, half.row+1))}
image3 <- mutate(image3, 
                 fold = ifelse(block.x==TRUE & block.y==FALSE, 10, 9), 
                 fold = ifelse(block.x==TRUE & block.y==TRUE, 11, fold), 
                 fold = ifelse(block.x == FALSE & block.y == TRUE, 12, fold))

combined <- rbind(image1, image2, image3) %>%  # combine the three images 
            dplyr::select(-block.x,-block.y) %>% # removes block.x and block.y
            mutate(label=as.factor(label)) %>%
            filter(label != 0) %>% #filter out unlabelled points.  
            mutate(label = droplevels(label))

# Depending on whether we want to train on all features, or only NDAI, SD and 
# CORR, we can set set TopThree to TRUE

combined.all <- combined #save this one for calculating Gini importance measure 

TopThree <- TRUE
if (TopThree){
  combined <- cbind(combined[,1:6], combined[["fold"]])
  names(combined)[7] <- "fold"
  combined <- tbl_df(combined)
}

################################################################################
# Apply Random Forest model to all 12 Folds

num.tree <- 2 # set the number of trees you would like to train random forest on
num.folds <- 12 # set to the number of folds in your CV set

Random.Forest.folds <- foreach(i = 1:num.folds)%dopar% {
  #This function applies random forest to all 12 folds using the 'combined'
    #dataset given above.  
  #This will save the random forest class generated for each fold as
    #an Rdata file to be retrieved later for plots. This file will be named
    # iRF_block.Rdata, where 'i' is the number of the fold. 
  
  #It also saves the AUC of each CV, as 'AUC_blocki.csv.  Same i as above. 
  
  #Lastly, this function saves the confusion matrix of each cross validation.
    #as ROC_blocki.csv
  
  train <- filter(combined, fold != i) #so we drop the ith fold
  test <- filter(combined, fold == i) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("ROC_block%d.csv", i)
  try(write.csv(conf, file=filename.table))
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("AUC_block%d.csv", i)
  try(write.csv(auc, file=filename.auc))
  filename <- sprintf("%sRF_block.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}

################################################################################
#Random forest model for all 9 features in order to calculate Gini importance

Random.Forest.folds.9 <- foreach(i = 1:num.folds)%dopar% {
  #This function applies random forest to all 12 folds using the 'combined'
  #dataset given above.  
  #This will save the random forest class generated for each fold as
  #an Rdata file to be retrieved later for plots. This file will be named
  # iRF_block.Rdata, where 'i' is the number of the fold. 
  
  #It also saves the AUC of each CV, as 'AUC_blocki.csv.  Same i as above. 
  
  #Lastly, this function saves the confusion matrix of each cross validation.
  #as ROC_blocki.csv
  
  train <- filter(combined.all, fold != i) #so we drop the ith fold
  test <- filter(combined.all, fold == i) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("9ROC_block_%d.csv", i)
  try(write.csv(conf, file=filename.table))
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("9AUC_block%d.csv", i)
  try(write.csv(auc, file=filename.auc))
  filename <- sprintf("%sRF_block_9.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}

################################################################################
# Image-wise validation

# This script applies random forest to all 2/3 of the 3 images and trains on 
# remaining one. 

# This will save the random forest class generated for each image as
# an Rdata file to be retrieved later for plots. This file will be named
# iRF_image.Rdata, where 'i' is 1, 4 or 8, representing image 1, 2, 3.. 

# It also saves the AUC of each CV, as 'AUC_imagei.csv.  Same i as above. 

# Lastly, this script saves the confusion matrix of each cross validation.
# as ROC_imagei.csv

image.cv <- foreach(i = c(1,5,9))%dopar% {

  test <- filter(combined, fold %in% c(i:(i+4))) #so we drop the ith fold
  train <- anti_join(combined, test) #test on the ith fold
  rf <- try(rf.specific(train, num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <-sprintf("ROC_image%d.csv", i)
  try(write.csv(conf, file=filename.table))
  auc <- try(auc.value(rf, test))
  filename.auc <- sprintf("AUC_image%d.csv", i)
  try(write.csv(auc, file=filename.auc))
  filename <- sprintf("%sRF_image.Rdata", i)
  try(save(rf,file =filename))
  return(filename) 
}


################################################################################
# Convergence analysis and validation

# This script trains the random forest model on the first i folds and tests 
# the remain folds

# This will save the random forest class predicted for each test as
# an Rdata file to be retrieved later for plots. This file will be named
# iRF_converge.Rdata, where 'i' is 1-11. 

# It also saves the AUC of each CV, as 'AUC_convergei.csv.  Same i as above. 

# Lastly, this script saves the confusion matrix of each cross validation.
# as ROC_convergei.csv

convergence.cv <- foreach(i = 1:11)%dopar% {
  train <- filter(combined, fold <= i)
  test <- anti_join(combined, train)
  rf <- try(rf.specific(train,num.tree))
  conf <- try(confusion.generate(rf,test))
  filename.table <- sprintf("ROC_converge%d.csv", i)
  try(write.csv(conf, file = filename.table))
  auc <- try(auc.value(rf,test))
  filename.auc <- sprintf("AUC_converge%d.csv", i)
  try(write.csv(auc, file=filename.auc))
  filename <- sprintf("%sRF_converge.Rdata",i)
  try(save(rf, file = filename))
  return(filename)
}


################################################################################
#here we shuffle the folds to test convergence results

combined <- transform(combined, fold = sample(fold, replace=FALSE))

convergence.cv.shuffle.1 <- foreach(i = 1:11)%dopar% {
    
    train <- filter(combined, fold <= i) #so we drop the ith fold
    test <- anti_join(combined, train) #test on the ith fold
    rf <- try(rf.specific(train, num.tree))
    conf <- try(confusion.generate(rf,test))
    filename.table <-sprintf("ROC_converge_shuffle1%d.csv", i)
    try(write.csv(conf, file=filename.table))
    auc <- try(auc.value(rf, test))
    filename.auc <- sprintf("AUC_converge_shuffle1%d.csv", i)
    try(write.csv(auc, file=filename.auc))
    filename <- sprintf("%sRF_converge_shuffle1.Rdata", i)
    try(save(rf,file =filename))
    return(filename) 
}

#shuffled again

combined <- transform(combined, fold = sample(fold, replace=FALSE))

convergence.cv.shuffle.2 <- foreach(i = 1:11)%dopar% {
    
    train <- filter(combined, fold <= i) #so we drop the ith fold
    test <- anti_join(combined, train) #test on the ith fold
    rf <- try(rf.specific(train, num.tree))
    conf <- try(confusion.generate(rf,test))
    filename.table <-sprintf("ROC_converge_shuffle2%d.csv", i)
    try(write.csv(conf, file=filename.table))
    auc <- try(auc.value(rf, test))
    filename.auc <- sprintf("AUC_converge_shuffle2%d.csv", i)
    try(write.csv(auc, file=filename.auc))
    filename <- sprintf("%sRF_converge_shuffle2.Rdata", i)
    try(save(rf,file =filename))
    return(filename) 
}
################################################################################
# ROC curve data frame for plotting (of the 12 folds)
#

ROC.curve.data.frame.begin <- function(filename){
  # First seed the dataframe with the ROC curve information from the 1st fold
  # gives us a dataframe with the 1st fold's ROC curve plotting info (x and y 
  # values)
  # To output ROC information for the convergence data, just substitute 'block'
  # in sprintf below with 'convergence'.  Same with 'image'
  
  load(filename)
  roc.data <- roc.data(rf, filter(combined, fold == 1) )
  x.values <- roc.data@x.values[[1]]
  col.length <- length(x.values)
  ROC.data <- as.data.frame(1:col.length)
  ROC.data[,1] <- "1st fold"
  colnames(ROC.data)[1] <- "fold.number"
  ROC.data$x.values <- x.values
  ROC.data$y.values <- roc.data@y.values[[1]]
  
  return(ROC.data)
}



################################################################################
# Create entire ROC curve dataframe

ROC.curve.data.frame.fold <- function(ROC){

  # Input previous function: ROC is the object return from 
  # ROC.curve.data.frame.begin
  # outputs the entire dataframe of ROC curve data for all other folds.  
  # to output ROC information for the convergence data, just substite 'block'
  # in sprintf below with 'convergence'.  Same with 'image'
  # writes to csv the ROC dataframe to be plotted in the random_forest_plots.R file
  ROC.data <- ROC
  for (i in c(2:6, 8:12)){
      filename <- sprintf("%dRF_block.Rdata", i)
      load(filename)
      rocData <- roc.data(rf, filter(combined, fold == i) )
      x.values <- rocData@x.values[[1]]
      col.length <- length(x.values)
      data <- as.data.frame(1:col.length)
      colnames(data)[1] <- "fold.number"
      fold.name <- sprintf("%d fold", i)
      data[,1] <- fold.name
      data$x.values <- x.values
      data$y.values <- rocData@y.values[[1]]
      ROC.data <- rbind(ROC.data, data)
  }
  write.csv(ROC.data, "ROC_fold_comparison.csv")
  return(ROC.data)
}

ROC.curve.data.frame.converge <- function(ROC){
    
    # Input previous function: ROC.curve.data.frame.begin
    # outputs the entire dataframe of ROC curve data for all other folds.
    # to output ROC information for the convergence data, just substite 'block'
    # in sprintf below with 'convergence'.  Same with 'image'
    # writes to csv the ROC dataframe to be plotted in the random_forest_plots.R file
    ROC.data <- ROC
    for (i in c(2:11)){
        filename <- sprintf("%dRF_converge.Rdata", i)
        load(filename)
        rocData <- roc.data(rf, filter(combined, fold == i) )
        x.values <- rocData@x.values[[1]]
        col.length <- length(x.values)
        data <- as.data.frame(1:col.length)
        colnames(data)[1] <- "fold.number"
        fold.name <- sprintf("%d fold", i)
        data[,1] <- fold.name
        data$x.values <- x.values
        data$y.values <- rocData@y.values[[1]]
        ROC.data <- rbind(ROC.data, data)
    }
    write.csv(ROC.data, "ROC_convergence_comparison.csv")
    return(ROC.data)
}


ROC.curve.data.frame.shuffle1 <- function(ROC){
    
    # Input previous function: ROC.curve.data.frame.begin
    # outputs the entire dataframe of ROC curve data for all other folds.
    # to output ROC information for the convergence data, just substite 'block'
    # in sprintf below with 'convergence'.  Same with 'image'
    # writes to csv the ROC dataframe to be plotted in the random_forest_plots.R file
    ROC.data <- ROC
    for (i in c(2:6, 8:12)){
        filename <- sprintf("1RF_converge_shuffle1.Rdata", i)
        load(filename)
        rocData <- roc.data(rf, filter(combined, fold == i) )
        x.values <- rocData@x.values[[1]]
        col.length <- length(x.values)
        data <- as.data.frame(1:col.length)
        colnames(data)[1] <- "fold.number"
        fold.name <- sprintf("%d fold", i)
        data[,1] <- fold.name
        data$x.values <- x.values
        data$y.values <- rocData@y.values[[1]]
        ROC.data <- rbind(ROC.data, data)
    }
    write.csv(ROC.data, "ROC_converge_shuffle1.csv")
    return(ROC.data)
}

ROC.curve.data.frame.shuffle2 <- function(ROC){
    
    # Input previous function: ROC.curve.data.frame.begin
    # outputs the entire dataframe of ROC curve data for all other folds.
    # to output ROC information for the convergence data, just substite 'block'
    # in sprintf below with 'convergence'.  Same with 'image'
    # writes to csv the ROC dataframe to be plotted in the random_forest_plots.R file
    ROC.data <- ROC
    for (i in c(2:6, 8:12)){
        filename <- sprintf("1RF_converge_shuffle2.Rdata", i)
        load(filename)
        rocData <- roc.data(rf, filter(combined, fold == i) )
        x.values <- rocData@x.values[[1]]
        col.length <- length(x.values)
        data <- as.data.frame(1:col.length)
        colnames(data)[1] <- "fold.number"
        fold.name <- sprintf("%d fold", i)
        data[,1] <- fold.name
        data$x.values <- x.values
        data$y.values <- rocData@y.values[[1]]
        ROC.data <- rbind(ROC.data, data)
    }
    write.csv(ROC.data, "ROC_converge_shuffle2.csv")
    return(ROC.data)
}


################################################################################
# False positive, False negative, True positive, True negative plots


False.positive.False.negative.Plots <- function(image, filename, k){
    # This script generates false positive false negative plots for a prediction
    # rf is an object of the random forest class
    # image is an image file of the format generated above
    # k is the image number (1,2, or 3)
    # outputs 3 pdf files of the raw image with NDAI plotted via geom_point, the
    # correct expert classification plotted, and finally the predictions with
    # factors indication whether the prediction was a false positive, true
    # positive, false negative or true negative.
    load(filename)
    #this is a prediction generated from rf on image
    image$predicted <- predict(rf, image[,4:6])
    
    image <- tbl_df(image) %>%
    mutate(classification = rep(0, nrow(image))) %>%
    mutate(classification =
    ifelse(predicted == label & label == "1",
    "true positive", classification),
    classification = ifelse(predicted == label &
    predicted == "-1",
    "true negative",
    classification),
    classification = ifelse(predicted !=label &
    predicted == "-1",
    "false negative",
    classification),
    classification = ifelse(label != predicted &
    label == "-1",
    "false positive",
    classification))
    
    fpr <- geom_point(aes(x=x, y=y, color=classification))
    image.NDAI <- geom_point(aes(x=x, y=y, color = NDAI))
    raw.image <- geom_point(aes(x=x, y=y, color=as.factor(label)))
    
    colour <- scale_colour_manual(values = c("true positive" = "white",
    "false positive" = "black",
    "true negative" = "#3366FF",
    "false negative" = "#FF00CC",
    "Unknown" = "#999999"))
    
    im3 <- ggplot(image)
    
    filename.classification <- sprintf("classification_image%d.png", k)
    ggsave(filename = filename.classification, plot = im3+fpr+colour)
    
    
    filename.label <- sprintf("label_%d.png", k)
    ggsave(filename = filename.label, plot = im3+raw.image)

    
    filename.NDAI <- sprintf("NDAI_image%d.png", k)
    ggsave(filename = filename.NDAI, plot = im3+image.NDAI)

    
    return(im3+fpr)
}


################################################################################
# ROC curve plot functions

# We gave an example of what files contains the comparison csv,
# this is generated in the random_forest.R file


plot.roc.fold <- function(filename){
    
    
    ROC.data <- read.csv(filename)
    colnames(ROC.data)[2] <- "fold.number"
    colnames(ROC.data)[3] <- "False.positive.rate"
    colnames(ROC.data)[4] <- "True.positive.rate"
    ggsave(filename = "ROC_fold_comparison.png", plot = ggplot(ROC.data, aes(x=False.positive.rate, y = True.positive.rate))+
      geom_line(aes(colour= fold.number, group = fold.number)))
}

plot.roc.converge <- function(filename){
    
    
    ROC.data <- read.csv(filename)
    colnames(ROC.data)[2] <- "number.of.quadrants"
    colnames(ROC.data)[3] <- "False.positive.rate"
    colnames(ROC.data)[4] <- "True.positive.rate"

    ggsave(filename = "ROC_converge1.pdf", plot = ggplot(ROC.data, aes(x=False.positive.rate, y = True.positive.rate))+
    geom_line(aes(colour= number.of.quadrants, group = number.of.quadrants)))
}

plot.roc.shuffle1 <- function(filename){
    
    
    ROC.data <- read.csv(filename)
    colnames(ROC.data)[2] <- "number.of.quadrants"
    colnames(ROC.data)[3] <- "False.positive.rate"
    colnames(ROC.data)[4] <- "True.positive.rate"

    ggsave(filename = "ROC_converge_shuffle1.png", plot = ggplot(ROC.data, aes(x=False.positive.rate, y = True.positive.rate))+
    geom_line(aes(colour= number.of.quadrants, group = number.of.quadrants)))
}
plot.roc.shuffle2 <- function(filename){
    
    
    ROC.data <- read.csv(filename)
    colnames(ROC.data)[2] <- "number.of.quadrants"
    colnames(ROC.data)[3] <- "False.positive.rate"
    colnames(ROC.data)[4] <- "True.positive.rate"
    ggsave(filename ="ROC_converge_shuffle2.png",  ggplot(ROC.data, aes(x=False.positive.rate, y = True.positive.rate))+
    geom_line(aes(colour= number.of.quadrants, group = number.of.quadrants)))
}


################################################################################
# GINI importance from 9 feature training set

##################################################
# Gini Data Frame for Comparison

Gini.data.frame.begin <- function(filename){
    # This script creates a data frame with the Gini importance measure of each
    # feature for the all 12 folds.
    
    load(filename)
    Gini <<- as.data.frame(rf$importance[,4])
    colnames(Gini)[1] <- "1st fold"
    return(Gini)
}
    
compare.gini <- function(gini){
  #feed in output from Gini.data.frame.begin
  Gini <- gini
  for (i in c(2:6, 8:12)){
        # This function generates a dataframe with all 12 folds of the Gini
        # importance measures
        
        filename <- sprintf("%dRF_block_9.Rdata", i)
        load(filename)
        Forest <- sprintf("%dst fold", i)
        Gini <- cbind(Gini, as.data.frame(rf$importance[,4]))
        colnames(Gini)[ncol(Gini)] <- Forest
    }
    return(Gini)
}

################################################################################
# Gives mean and variance summary for Gini dataframe:

Gini.Mean.sd <- function(gini){
  #pass in output from compare.gini
  
    Gini <- gini
    Gini$mean <- rowMeans(Gini)
    rowVars <- function(x) {
      rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
    }
    Gini$variance <- rowVars(as.matrix(Gini[,1:11]))
    Gini$sd <- sqrt(Gini$variance)
    png("Gini_mean_sd.png")
    grid.table(Gini[,12:14],show.rownames=T)
    dev.off()
    return(Gini)
    
}

################################################################################
# Reshape Gini

Reshape.Gini <- function(gini){
    # Reformats the Gini dataframe for plotting of Gini Importance:
    # Outputs the saved plot
    
    Gini <- gini
    Gini <- t(Gini)
    Gini <- as.data.frame(Gini)
    Gini$fold <- c(1:6, 8:12)
    colnames(Gini)[9] <- "fold.number"
    Gini <- melt(Gini, id = "fold.number")
    colnames(Gini)[1] <- "fold"
    colnames(Gini)[2] <- "variable"
    colnames(Gini)[3] <- "GiniImportance"
    
    ggsave(filename = "Gini_Importance.png", plot = ggplot(Gini)+geom_point(aes(x=fold, y = GiniImportance, 
                                colour = variable))+geom_smooth(aes(group = variable, x = fold, y = GiniImportance)))
}

################################################################################
# AUC table
#

AUC.table.begin <- function(filename){
  # Get table of AUC measurements
  
  AUC <- read.csv(filename)
  colnames(AUC)[2] <- "1st fold"
  AUC <- dplyr::select(AUC, -X)
  return(AUC)
}
    
compare.AUC <- function(auc.object){
  #feed in output of AUC.table.begin
  AUC <- auc.object
  for (i in c(2:6, 8:12)){
        # Adds the rest of the AUC data using dataframe generated above
        filename <- sprintf("AUC_block%d.csv", i)
        AUC.num <- sprintf("%dst fold", i)
        file <- read.csv(filename)
        colnames(file)[2] <- AUC.num
        file <- dplyr::select(file, -X)
        AUC <- cbind(AUC, file)
    }
    
    AUC <- as.data.frame(AUC)
    rownames(AUC) <- "AUC"
    AUC <- t(AUC)
    AUC <- as.data.frame(AUC)
    AUC$fold <- rownames(AUC)
    
    png("AUC_12_folds.png")
    grid.table(AUC,show.rownames=T)
    dev.off()
    
    return(AUC)
}

AUC.plot <- function(auc){
  #feed in output of compare.auc
  AUC <- auc
  ggsave(filename ="AUCconverge.pdf", plot = ggplot(AUC)
         +geom_point(aes(x=fold, y = AUC))+geom_smooth(aes(group = 1, x=fold, y = AUC)))
}




################################################################################
# - Image Files  -
#
# If you want to save the images, set ImageSave to TRUE

if (ImageSave){
    
    # Image False Classification plots:
    filename <- "1RF_image.Rdata"
    image <- image1
    False.positive.False.negative.Plots(image, filename, 1)
    
    filename <- "5RF_image.Rdata"
    image <- image2
    False.positive.False.negative.Plots(image, filename, 2)
    
    filename <- "9RF_image.Rdata"
    image <- image3
    False.positive.False.negative.Plots(image, filename, 8)
    
    # ROC comparison plots
    ROC.curve.data.frame.fold(ROC.curve.data.frame.begin(
      "1RF_block.Rdata"))
    ROC.curve.data.frame.converge(ROC.curve.data.frame.begin(
      "1RF_converge.Rdata"))
    ROC.curve.data.frame.shuffle1(ROC.curve.data.frame.begin(
      "1RF_converge_shuffle1.Rdata"))
    ROC.curve.data.frame.shuffle2(ROC.curve.data.frame.begin(
      "1RF_converge_shuffle2.Rdata"))
    
    filename <- "ROC_fold_comparison.csv"
    
    plot.roc.fold(filename)
    
    filename <- "ROC_convergence_comparison.csv"
    
    plot.roc.converge(filename)
    
    filename <- "ROC_converge_shuffle1.csv"
    
    plot.roc.converge(filename)
    
    filename <- "ROC_converge_shuffle2.csv"
    
    plot.roc.shuffle1(filename)
    
    #Gini plots
    Reshape.Gini(compare.gini(Gini.data.frame.begin("1RF_block_9.Rdata")))
    
    #Gini_mean_sd table png
    Gini.Mean.sd(compare.gini(Gini.data.frame.begin("1RF_block_9.Rdata")))
    
    #AUC table
    compare.AUC(AUC.table.begin("AUC_block1.csv"))
    
    #AUC plot 
    
    AUC.plot(compare.AUC(AUC.table.begin("AUC_block1.csv")))
    
    #ROC plots for image 1, 5 and 9 (1,2,3 but labelled thus):
    load("1RF_image.Rdata")
    test <- filter(combined, fold %in% c(1:4))
    pdf("ROC_image1.pdf")
    roc.generate(rf, test)
    dev.off()
    
    load("5RF_image.Rdata")
    test <- filter(combined, fold %in% c(5:8))
    pdf("ROC_image5.pdf")
    plot = roc.generate(rf, test)
    dev.off()
    
    load("9RF_image.Rdata")
    test <- filter(combined, fold %in% c(9:(12)))
    pdf("ROC_image9.pdf")
    roc.generate(rf, test)
    dev.off()
}




