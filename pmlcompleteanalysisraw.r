#Loading the required libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

#Set Seed
set.seed(123)
# Data Loading
train_csv <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test_csv <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
dim(train_csv)
dim(test_csv)

# Data Cleaning
train_csv_clean <- train_csv[,colSums(is.na(train_csv)) == 0]
test_csv_clean <- test_csv[,colSums(is.na(test_csv)) == 0]
dim(train_csv_clean)
dim(test_csv_clean)
View(train_csv_clean)
View(train_csv_clean)
train_csv_final_clean <- train_csv_clean[,-c(1:7)]
test_csv_final_clean <- test_csv_clean[,-c(1:7)]
x<-names(train_csv_final_clean) %in% c("skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
"amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
"stddev_yaw_forearm", "var_yaw_forearm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "kurtosis_roll_belt", "kurtosis_picth_belt",
"kurtosis_yaw_belt", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm")
train_csv_final_clean<-train_csv_final_clean[!x]

# Partioning the data into 60% training dataset and 40% test dataset
trainer<- createDataPartition(y=train_csv_final_clean$classe, p=0.60, list=FALSE)
final_train<- train_csv_final_clean[trainer, ]
final_test<- train_csv_final_clean[-trainer, ]
dim(final_train)
dim(final_test)

# Random Forest Model
model <- randomForest(classe ~., data=final_train, method="class")
predict_model <- predict(model, final_test, type = "class")
confusionMatrix(predict_model, final_test$classe)

# Decision Tree Model
model1<- rpart(classe ~ ., data=final_train, method="class")
predict_model1<- predict(model1, final_test, type = "class")
confusionMatrix(predict_model1, final_test$classe)

#Final Prediction using Random Forest Model
final_prediction <- predict(model, test_csv, type = "class")
print(final_prediction)

#Writing to files
pmlwrite = function(temp){
  n = length(temp)
  for(i in 1:n){
    filename = paste0("problem-id_",i,".txt")
    write.table(temp[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pmlwrite(final_prediction)
