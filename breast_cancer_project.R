library(dplyr)  
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(lattice)
library(tidyverse)
library(plyr)
library(pROC)
library(Rcpp)

############################### PREPROCESSING ###################################

#import dataset
breast_cancer <- read.csv("F:/data engineering/project/cancer selected4/Breast_cancer/breast cancer.csv", header=FALSE)
dim(breast_cancer)

#show the structure of the data

str(breast_cancer)
print(summary(breast_cancer)) 

#rename the columns

names(breast_cancer) <- c("Sample_code_number","Clump_Thickness",
                          "Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape","Marginal_Adhesion",
                          "Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin",
                          "Normal_Nucleoli","Mitoses","Class")
colnames(breast_cancer) 
str(breast_cancer)


#change the data type 

breast_cancer$Bare_Nuclei <- suppressWarnings(as.integer(breast_cancer$Bare_Nuclei))
str(breast_cancer)


#drop id column
dim(breast_cancer)

breast_cancer <- select(breast_cancer ,-1)
dim(breast_cancer)


#replace wrong data 
#breast_cancer[breast_cancer == '?'] <- NA
breast_cancer <- na.omit(breast_cancer)
str(breast_cancer)
dim(breast_cancer)

#replace and change the class column values

breast_cancer$Class[breast_cancer$Class == 2] <- 0
breast_cancer$Class[breast_cancer$Class == 4] <- 1

breast_cancer$Class <- as.factor(breast_cancer$Class)
str(breast_cancer)


#################### APPLY CLASSIFICATION TO THE DATASET  ##################################



# 1 - Determine the number of rows for trainingset

set.seed(1)
Num_sample<- floor(nrow(breast_cancer) * 0.70) 

# 2 - Create a random sample 

sample_rows <- sample(nrow(breast_cancer),Num_sample)

# 3 - Create the training dataset and testset

training_set <- breast_cancer[sample_rows, ]
testing_set <- breast_cancer[-sample_rows, ]


# 4 - Split testingset into input and output

test_input <- select(testing_set ,-10)
test_output <- testing_set["Class"]


# 5 - Build a lending model detecting the breast cancer Using class column

cancer_model_Tree <- rpart(Class ~., data = training_set ,method="class",parms=list(split='inforamtion'))
cancer_model_Tree
rpart.plot(cancer_model_Tree)

# 6 - Make a prediction for the class 

p <- predict(cancer_model_Tree,type = "class")

p

##########################   ACCURACY   ##################################


# 1 - accuracy of training set and visualize it 

Visualization_of_training <- confusionMatrix(training_set$Class ,p,positive='1',dnn = c("Prediction","Reference"))  

print(Visualization_of_training) 

fourfoldplot(Visualization_of_training$table, color = c("cyan", "pink"),
            conf.level = 0, margin = 1, main = "Visualization_of_training")

# 2 - accuracy of testing set and visualize it 

predection =cancer_model_Tree %>%
  predict(test_input,type="class")


Visualization_of_testing <- confusionMatrix(test_output$Class ,predection,positive='1',dnn = c("Prediction","Reference"))   

print(Visualization_of_testing)

fourfoldplot(Visualization_of_testing$table, color = c("gray", "orange"),
             conf.level = 0, margin = 1, main = " Visualization_of_testing")

mean(predection==test_output$Class)


##########################   MODEL SELECTION   ############################

##  k fold cross validation  ##

TPR=c()  #TRUE POSITIVE RATE
FPR=c()  #FALSE POSITIVE RATE
Accuracy=c()

folds = createFolds(training_set$Class,k=10)

cv = lapply(folds,function(x){
  
  trainingOf_set <- training_set[-x, ]
  testingOf_set <- training_set[x, ]
  
  test_inputt <- select(testingOf_set ,-10)
  test_outputt <- testingOf_set["Class"]
  
  cancer_model_Tree <- rpart(Class ~., data = trainingOf_set ,method="class",parms=list(split='inforamtion'))
  predection =cancer_model_Tree %>%
    predict(test_inputt,type="class")
  
  
  cm=table(test_outputt$Class,predection)
  mean2=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  Accuracy <- c(Accuracy,mean2)
  
  y=(cm[1,1])/(cm[1,1]+cm[1,2])
  TPR <- c(TPR,y)
  
  x=(cm[2,1])/(cm[2,2]+cm[2,1])
  FPR <- c(FPR,x)
  
  result <- cbind(Accuracy,TPR,FPR)
  return(result)
  
  
})

d = cv
d
##  Grid search  ##

grid_serch = train(form = Class ~ . , data= training_set , method = 'svmRadial')
grid_serch

grid_serch$bestTune


##  visulize the TPR AND FPR  ##


d = cv
d

total_accuries <- c(d$Fold01[1],d$Fold02[1],d$Fold03[1],d$Fold04[1],d$Fold05[1]
                    ,d$Fold06[1],d$Fold07[1],d$Fold08[1],d$Fold09[1],d$Fold10[1])


print(mean(total_accuries))

TPR <- c(d$Fold01[2],d$Fold02[2],d$Fold03[2],d$Fold04[2],d$Fold05[2]
                    ,d$Fold06[2],d$Fold07[2],d$Fold08[2],d$Fold09[2],d$Fold10[2])

FPR <- c(d$Fold01[3],d$Fold02[3],d$Fold03[3],d$Fold04[3],d$Fold05[3]
                    ,d$Fold06[3],d$Fold07[3],d$Fold08[3],d$Fold09[3],d$Fold10[3])


par(bg="#f7f7f7") 
plot(FPR,TPR,main="",xlab="FPR",ylab="TPR",col =rainbow(7),pch = 16)







