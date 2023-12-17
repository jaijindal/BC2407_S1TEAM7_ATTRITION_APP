################################################################
#   Course        : BC2407 Analytics II Group Project          #
#   Team          : 07                                         #
#   Seminar Group : 01                                         #
################################################################



#-----------------------------CODE INDEX FOR PROF REFERENCE----------------------------------------------------------------

#1) Data Cleaning
#2) Exploratory Data Analysis
#3) Data Visualization
#4) Feature Selection -> Logistic Regression + Random Forest
#5) Sampling for unbiased data set
#6) CART Model 
#7) MARS Model
#8) RANDOM FOREST Model
#9) PERFORMANCE METRIC TABLE OF MODELS COMPILATION





#-----------------------------------Load the necessary package--------------------------------------------------------------
library(earth)
library(glmnet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(car)
library(randomForest)
library(caTools)
library(data.table)
library(rpart)
library(rpart.plot) 
library(knitr)



set.seed(1)

#----------------------Set working directory and read in data (Update Path and Run)----------------------------------------


setwd("D:/jinda/NTU/SEMESTER 4/BC2407-Analytics/BC2407 Course Materials/project/Dataset")

data1.dt <- fread(text="HR_Analytics.csv",stringsAsFactors = T)



#------------------------------------Data Cleaning--------------------------------------------------------------------------


summary(data1.dt)
str(data1.dt)

#count NA
na_count <-sapply(data1.dt, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)  #no null values in data set, hence no omit required 



#Change Business Travel to 0 1 2, then converting it to categorical
#0 is for Non Travel
#1 is for Travel Rarely 
#2 is for Travel_Frequently
data1.dt$BusinessTravel <- gsub('Non-Travel','0',data1.dt$BusinessTravel)
data1.dt$BusinessTravel <- gsub('Travel_Rarely','1',data1.dt$BusinessTravel)
data1.dt$BusinessTravel <- gsub('Travel_Frequently','2',data1.dt$BusinessTravel)
data1.dt$BusinessTravel = as.factor(data1.dt$BusinessTravel)


#Change Attrition to Binary
#1 is for yes
#0 is for no 
data1.dt$Attrition <- ifelse(data1.dt$Attrition=="Yes",1,0)
data1.dt$Attrition <- as.factor(data1.dt$Attrition)


#Assign Department to 0,1,2,3,4,5,6
#0 is for Business Operations and Strategy
#1 is for Engineering 
#2 is for Finance and Accounting
#3 is for Human Resources 
#4 is for Marketing and Communications
#5 is for Product Research & Development
#6 is for Security and Privacy
data1.dt$Department <- gsub('Business Operations and Strategy','0',data1.dt$Department)
data1.dt$Department <- gsub('Engineering','1',data1.dt$Department)
data1.dt$Department <- gsub('Finance and Accounting','2',data1.dt$Department)
data1.dt$Department <- gsub('Human Resources','3',data1.dt$Department)
data1.dt$Department <- gsub('Product Research & Development','5',data1.dt$Department)
data1.dt$Department <- gsub('Marketing and Communications','4',data1.dt$Department)
data1.dt$Department <- gsub('Security & Privacy','6',data1.dt$Department)
data1.dt$Department = as.factor(data1.dt$Department)



#Change Gender to 0,1
#1 is for Male
#0 is for Female 
data1.dt$Gender <- ifelse(data1.dt$Gender=="Male",1,0)
data1.dt$Gender <- as.factor(data1.dt$Gender)


#Change Marital Status to 0,1,2
#0 is for Divorced
#1 is for Married
#2 is for Single
data1.dt$MaritalStatus <- gsub('Divorced','0',data1.dt$MaritalStatus)
data1.dt$MaritalStatus <- gsub('Married','1',data1.dt$MaritalStatus)
data1.dt$MaritalStatus <- gsub('Single','2',data1.dt$MaritalStatus)
data1.dt$MaritalStatus = as.factor(data1.dt$MaritalStatus)


#Change Overtime to 0,1
#0 is for No
#1 is for Yes 
data1.dt$OverTime <- ifelse(data1.dt$OverTime=="Yes",1,0)
data1.dt$OverTime <- as.factor(data1.dt$OverTime)


#Assign Jobrole to 0,1,2,3,4,5,6
#0 is for Software Engineer
#1 is for Product Specialist 
#2 is for Business Operations
#3 is for Finance
#4 is for Information Security
#5 is for Manager
#6 is for (Other)
data1.dt$JobRole <- gsub('Software Engineer','0',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Product Specialist','1',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Business Operations','2',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Finance','3',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Information Security','4',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Manager','5',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Marketing','6',data1.dt$JobRole)
data1.dt$JobRole <- gsub('Human Resources','6',data1.dt$JobRole)
data1.dt$JobRole = as.factor(data1.dt$JobRole)

#conversion of factor datatypes
data1.dt$WorkLifeBalance = as.factor(data1.dt$WorkLifeBalance)
data1.dt$RelationshipSatisfaction = as.factor(data1.dt$RelationshipSatisfaction)
data1.dt$PerformanceRating = as.factor(data1.dt$PerformanceRating)
data1.dt$JobInvolvement = as.factor(data1.dt$JobInvolvement)
data1.dt$EnvironmentSatisfaction = as.factor(data1.dt$EnvironmentSatisfaction)
data1.dt$JobSatisfaction = as.factor(data1.dt$JobSatisfaction)
data1.dt$Education = as.factor(data1.dt$Education)
data1.dt$JobLevel = as.factor(data1.dt$JobLevel)


summary(data1.dt)
str(data1.dt)
#remove those 8 unnecessary columns like Employee Number, Daily Rate, Distance From Home, Education Field, Hourly Rate, Monthly Rate, Over18, Standard Hours
hr.dt=data1.dt


#------------------------------------------ Exploratory Data Analysis-------------------------------------------------------



#Data Exploration for Data Cleaning#Distribution of Monthly Income:
ggplot(hr.dt)+aes(x=MonthlyIncome)+geom_histogram(bins = 50, fill = "#B8DAD9") + ylab("Frequency")
ggplot(hr.dt)+aes(y=MonthlyIncome)+geom_boxplot(fill = "#C0B9BF")


#Extracting Outlier Values
out <- boxplot.stats(hr.dt$MonthlyIncome)$out
range(out)
#Min outlier value is 16595, which is approx. twice the 3rd quartile value of 8379. 
#Rows which contain Outliers
which(hr.dt$MonthlyIncome %in% c(out))
#114 rows out of 1470 rows(7.76% of dataset) are outliers. 
#We can keep these outliers in the dataset as it is possible for certain employees in high ranking positions(e.g. CEO, President etc.) to have a much higher monthly income.


#Distribution of Percentage of Salary Hike (upon base level of pay):
ggplot(hr.dt)+aes(x=PercentSalaryHike)+geom_histogram(bins = 40, fill = "#B8DAD9") + ylab("Frequency")
#Checking for Outliers
ggplot(hr.dt)+aes(y=PercentSalaryHike)+geom_boxplot(fill = "#C0B9BF")#No outliers detected







#--------------------------------------------Data Visualization-------------------------------------------------------------



#Univariate Analysis
#Stacked Bar Charts for Categorical; Boxplots for Continuous; Jitter plots for Continuous discrete
ggplot(hr.dt, aes(x = Age, fill = Attrition)) + geom_bar(position = "fill") + labs(title = "Distribution of Attrition across Age") +  ylab("Proportion")

ggplot(hr.dt) + geom_bar(aes(x=BusinessTravel, fill=Attrition), position="fill") + labs(title = "Distribution of Attrition across Business Travel") + ylab("Proportion") + scale_fill_discrete(labels=c("No", "Yes"))

ggplot(hr.dt) + geom_bar(aes(x=Department, fill=Attrition), position="fill") + labs(title = "Distribution of Attrition across Department") + ylab("Proportion") + scale_fill_discrete(labels=c("No", "Yes"))

ggplot(hr.dt) + geom_bar(aes(x=Education, fill=Attrition), position="fill") + labs(title = "Distribution of Attrition across Education") + ylab("Proportion") + scale_fill_discrete(labels=c("No", "Yes"))

ggplot(hr.dt) + geom_bar(aes(x=Gender, fill=Attrition), position="fill") + labs(title = "Distribution of Attrition across Gender") + ylab("Proportion") + scale_fill_discrete(labels=c("No", "Yes"))
ggplot(hr.dt) + geom_bar(aes(x=Attrition, fill = Gender), stat = "count", position = "fill") + 
  labs(title = "Gender Distribution of Attrition") + ylab("Proportion") + scale_fill_discrete(labels=c("Female", "Male"))

ggplot(hr.dt) + geom_bar(aes(x=JobInvolvement, fill = Attrition), stat = "count", position = "fill")+ labs(title = "Distribution of Attrition across Job Involvement") + ylab("Proportion")

ggplot(hr.dt) + geom_bar(aes(x=JobLevel, fill = Attrition), stat = "count", position = "fill")+ labs(title = "Distribution of Attrition across Job Level") + ylab("Proportion")

ggplot(hr.dt) + geom_bar(aes(x=JobRole, fill = Attrition), stat = "count", position = "fill") + labs(title = "Distribution of Attrition across Job Role") + ylab("Proportion") 

ggplot(hr.dt) + geom_bar(aes(x=JobRole, fill = Attrition), stat = "count", position = "dodge") + labs(title = "Distribution of Attrition across Job Role") + ylab("Count") 

ggplot(hr.dt) + geom_bar(aes(x=JobSatisfaction, fill = Attrition), stat = "count", position = "fill") + labs(title = "Distribution of Attrition across Job Satisfaction") + ylab("Proportion") 

ggplot(hr.dt) + geom_bar(aes(x=EnvironmentSatisfaction, fill = Attrition), stat = "count", position = "fill")+ labs(title = "Distribution of Attrition across Environment Satisfaction") + ylab("Proportion") 

ggplot(hr.dt) + geom_bar(aes(x=RelationshipSatisfaction, fill = Attrition), stat = "count", position = "fill") + labs(title = "Distribution of Attrition across Relationship Satisfaction") + ylab("Proportion") 

ggplot(hr.dt) + geom_jitter(aes(x=Attrition, y=StockOptionLevel)) + labs(title = "Distribution of Attrition across Stock Options Level") + ylab("Count")

ggplot(hr.dt) + geom_boxplot(aes(x=Attrition, y=TotalWorkingYears)) + labs(title = "Total Working Years Distribution of Attrition") + ylab("Total Working Years") 

ggplot(hr.dt) + geom_jitter(aes(x=Attrition, y=TrainingTimesLastYear)) + labs(title = "Distribution of Attrition across number of Trainings Last Year")

ggplot(hr.dt) + geom_bar(aes(x=WorkLifeBalance, fill = Attrition), stat = "count", position = "fill") + labs(title = "Distribution of Attrition across Work Life Balance") + ylab("Proportion") 

ggplot(hr.dt) + geom_boxplot(aes(x=Attrition, y=YearsAtCompany)) + labs(title = "Years in Company Distribution of Attrition") 

ggplot(hr.dt) + geom_boxplot(aes(x=Attrition, y=YearsInCurrentRole)) + labs(title = "Years in Current Role Distribution of Attrition") 

ggplot(hr.dt) + geom_boxplot(aes(x=Attrition, y=YearsSinceLastPromotion)) + labs(title = "Years Since Last Promotion Distribution of Attrition")  

ggplot(hr.dt) + geom_boxplot(aes(x=Attrition, y=YearsWithCurrManager)) + labs(title = "Years With Current Manager Distribution of Attrition")

ggplot(hr.dt)+aes(x=Attrition, y=MonthlyIncome)+geom_boxplot(fill = "#C0B9BF")+ggtitle("Boxplot for Monthly Income")

ggplot(hr.dt)+aes(x=Attrition, y=PercentSalaryHike)+geom_boxplot(fill = "#C0B9BF")+ggtitle("Boxplot for Percent of Salary Hike")

ggplot(hr.dt) + geom_bar(aes(x=MaritalStatus, fill = Attrition), stat = "count", position = "fill") +  labs(x = "Marital Status", y = "Proportion of Attrition") + ggtitle("Stacked Bar Chart for Marital Status")

ggplot(hr.dt) +   geom_bar(aes(x=NumCompaniesWorked, fill = Attrition), stat = "count", position = "fill") +labs(x = "Number of Companies worked at before", y = "Proportion of Attrition") +  ggtitle("Stacked Bar Chart for Amount of Experience")

ggplot(hr.dt) + geom_bar(aes(x=OverTime, fill = Attrition), stat = "count", position = "fill") +  labs(x = "Overtime", y = "Proportion of Attrition") +ggtitle("Stacked Bar Chart for Overtime")

ggplot(hr.dt) +   geom_bar(aes(x=PerformanceRating, fill = Attrition), stat = "count", position = "fill") +labs(x = "PerformanceRating", y = "Proportion of Attrition") +  ggtitle("Stacked Bar Chart for Employee's Performance Ratings")







#-----------------------------------Feature Selection -> Logistic Regression + Random Forest--------------------------------




#---------------------------------Logistic Regression---------------------------------

#4-Department; 9-Job Level
lgreg = glm(Attrition~., data = hr.dt[, -c(4, 9)], family = "binomial" )
summary(lgreg)
vif(lgreg)
varImp(lgreg)

# Get absolute values of coefficient estimates
#coef_abs is a vector of absolute coefficient estimates from a logistic regression model. 
#The larger the absolute value of the coefficient, the more important the corresponding variable is in predicting the outcom
coef_abs <- abs(coef(lgreg))

# Sort variables by coefficient estimates
sorted_vars <- sort(coef_abs, decreasing = TRUE)

# Select the top 50 variables
top_50_vars <- names(sorted_vars)[1:50]

print(top_50_vars)

#selected top 17 features using Logistic Regression

selected_features1= c("JobInolvement","JobRole","BusinessTravel","OverTime","WorkLifeBalance","RelationshipSatisfaction","MaritalStatus"
                      ,"EnvironmentSatisfaction","StockOptionLevel","Gender","Education","TrainingTimeLastYear","YearsSinceLastPromotion",
                      "YearsAtCompany","YearsWithCurrManager","JobSatisfaction","MonthlyIncome")






#------------------------------------Random Forest---------------------------------------

class(hr.dt)
hr_data4 <- as.data.frame(hr.dt)
class(hr_data4)

# split the data into training and testing sets , do the 30-70 split
set.seed(1234)
train_idx <- sample(1:nrow(hr_data4), 0.7*nrow(hr_data4))
train_data <- hr_data4[train_idx, ]
test_data <- hr_data4[-train_idx, ]

# create a random forest model to select the most important features
rf_model<- randomForest(Attrition ~ ., data = train_data, importance = TRUE)

# plot the variable importance measures
varImpPlot(rf_model)

# print out the variable importance scores
print(rf_model$importance)

# create a new dataset with only the selected features
selected_features2 <- names(sort(rf_model$importance[,1], decreasing = TRUE)[1:19])#19 coming out to be optimal
print(selected_features2)
selected_data <- train_data[, c("Attrition", selected_features2), drop = FALSE]

# build a new random forest model using only the selected features
rf_formula <- as.formula(paste("Attrition ~", paste(selected_features2, collapse = "+")))
rf_model2 = randomForest(rf_formula, data = selected_data, importance = TRUE)

# evaluate the performance of the model on the testing data
predictions <- predict(rf_model, test_data[, ])
confusionMatrix(predictions, test_data$Attrition, positive = "1")

# evaluate the performance of the feature selected model on the testing data
predictions <- predict(rf_model2, test_data[, selected_features2])
confusionMatrix(predictions, test_data$Attrition, positive = "1")

#data set with final features
top_features_dataset=intersect(selected_features1, selected_features2)
print(top_features_dataset)
top_features_dataset=c("Attrition",top_features_dataset)
top_features_dataset=hr_data4[,top_features_dataset]




#----------------------------------------------------Sampling for unbiased data set--------------------------------------------------------



#--------------Method 1: Upsampling to 50/50 balance------------------------------------------------------
top_features_dataset1 <- top_features_dataset %>%
  group_by(Attrition) %>%
  sample_n(size = max(table(top_features_dataset$Attrition)), replace = TRUE)

#--------------Method 2: Downsampling to 50/50 balance----------------------------------------------------
top_features_dataset2 <- top_features_dataset %>%
  group_by(Attrition) %>%
  sample_n(size = min(table(top_features_dataset$Attrition)), replace = FALSE)

#--------------Method 3: Downsampling Attrition = 0 and upsampling Attrition = 1 to a 50/50 balance--------------
top_features_dataset3 <- top_features_dataset %>%
  group_by(Attrition) %>%
  sample_n(size = min(735, table(top_features_dataset$Attrition)), replace = (n() < 735)) %>%
  ungroup() %>%
  group_by(Attrition) %>%
  sample_n(size = 735, replace = TRUE) %>%
  ungroup()

#--------------Method 4: Using SMOTE----------------------------------------------------------------------
# Split the data into training and testing sets
set.seed(1)
train_idx <- sample(nrow(top_features_dataset), nrow(top_features_dataset) * 0.7)
hr_train <- top_features_dataset[train_idx, ]
hr_test <- top_features_dataset[-train_idx, ]
str(hr_train)
#hr_data <- top_features_dataset


# Use SMOTE to oversample the minority class
minority_class <- "1"
majority_class <- "0"

#These lines extract the data for the minority and majority classes from the training data set.
minority_data <- hr_train[hr_train$Attrition == minority_class, ]
majority_data <- hr_train[hr_train$Attrition == majority_class, ]

#These lines calculate the number of instances for both classes.
n_minority <- nrow(minority_data)
n_majority <- nrow(majority_data)

#This is a custom function to oversample the minority class. The function takes two arguments: the minority data 
#and the desired count of instances after oversampling. If the data is empty, the function returns the empty data. 
#Otherwise, it calculates the number of times to repeat the data to achieve the desired count and then repeats the data that many times. 
#Finally, it returns the oversampled data up to the desired count.
oversample <- function(top_features_dataset, desired_count) {
  if (nrow(top_features_dataset) == 0) {
    return(top_features_dataset)
  }
  repeats <- ceiling(desired_count / nrow(top_features_dataset))
  oversampled_data <- top_features_dataset[rep(seq_len(nrow(top_features_dataset)), times = repeats), ]
  return(oversampled_data[1:desired_count, ])
}
n_balanced_minority <- round(n_majority * 1.0)
train_balanced_minority <- oversample(minority_data, n_balanced_minority)
#This line combines the majority data with the oversampled minority data to create a balanced training data set. 
#The rbind() function is used to concatenate the two data sets row-wise.
top_features_dataset4 <- rbind(majority_data, train_balanced_minority)

# Check the number of instances in each class after balancing
table(top_features_dataset4$Attrition)


#--------------Checking the class balance of the three datasets---------------------------------------------
table(top_features_dataset1$Attrition)
table(top_features_dataset2$Attrition)
table(top_features_dataset3$Attrition)
table(top_features_dataset4$Attrition)

#we will be going forward with final_dataset4 using SMOTE for our ML MODELS

final_dataset = top_features_dataset4


#-----------------------------------------------------------MODEL DEVELOPMENT---------------------------------------------------------------------



# Define function to calculate performance metrics
cm <- function(actual, predicted) {
  confusion <- table(actual, predicted)
  accuracy <- sum(diag(confusion)) / sum(confusion)
  fp_rate <- confusion[1, 2] / sum(confusion[1, ])
  fn_rate <- confusion[2, 1] / sum(confusion[2, ])
  precision <- confusion[2, 2] / sum(confusion[, 2])
  recall <- confusion[2, 2] / sum(confusion[2, ])
  return(list(accuracy = accuracy, fp_rate = fp_rate, fn_rate = fn_rate,
              precision = precision, recall = recall))
}


#-----------------------------------------------------------TRAIN-TEST SPLIT---------------------------------------------------------------------

#Train-Test Split
set.seed(2023)
train <- sample.split(Y=final_dataset$Attrition, SplitRatio = 0.7)
hr.trainset <- subset(final_dataset, train==T)
hr.testset <- subset(final_dataset, train==F)
summary(hr.trainset)


#-----------------------------------------------------------CART MODEL---------------------------------------------------------------------



#Building Maximal Tree:
cart.max.up <- rpart(Attrition ~ ., data = hr.trainset, method = 'class',
                control = rpart.control(minsplit = 2, cp = 0))

printcp(cart.max.up)
plotcp(cart.max.up)


#Extracting the Optimal Tree:#Compute min CV error + 1SE in Maximal tree
CVerror.cap.up<-cart.max.up$cptable[which.min(cart.max.up$cptable[,"xerror"]), "xerror"] 
                  + cart.max.up$cptable[which.min(cart.max.up$cptable[,"xerror"]), "xstd"]

#Finding the optimal CP region whose CV error is just below CVerror.cap in maximal tree 
i <- 1; 
j<- 4
while (cart.max.up$cptable[i,j] > CVerror.cap.up) {i <- i + 1}

#Getting geometric mean of the two identified CP values in the optimal region if optimal tree has at least one 
cp2.up <- ifelse(i > 1, sqrt(cart.max.up$cptable[i,1] * cart.max.up$cptable[i-1,1]), 1)
split.cp2.up <- cp2.up # save cp2.up to a new variable

cart.opt.up <- prune(cart.max.up, cp = split.cp2.up)

printcp(cart.opt.up)
rpart.plot(cart.opt.up)
print(cart.opt.up)

#Predicting using Trainset & Confusion Matrix
cart.yhat_train <- predict(cart.opt.up, type = "class")
cart.cm_train <- table(hr.trainset$Attrition, cart.yhat_train, deparse.level = 2)

# create performance table using cm() function
cart_metrics <- cm(hr.trainset$Attrition, cart.yhat_train)


#Predicting using Testset & Confusion Matrix
cart.yhat <- predict(cart.opt.up, newdata=hr.testset, type = "class")
cart.yhat
cart.cm <- table(hr.testset$Attrition, cart.yhat, deparse.level = 2)
cart.cm

# create performance table using cm() function
cart_metrics <- cm(hr.testset$Attrition, cart.yhat)

# Create table with performance metrics
models <- c( "CART")
accuracy <- c(cart_metrics$accuracy)
fp_rate <- c(cart_metrics$fp_rate)
fn_rate <- c(cart_metrics$fn_rate)
precision <- c(cart_metrics$precision)
recall <- c(cart_metrics$recall)
performance_table <- data.frame(models, accuracy, fp_rate, fn_rate, precision, recall)
print(performance_table)


#------------------------------------------------------------MARS-------------------------------------------------------------------------

# Running MARS on final_dataset using same train, test datasets
mars_model_train <- earth(Attrition ~ ., data = hr.trainset, degree = 4)
summary(mars_model_train)

plot(mars_model_train)
#Predicting using Trainset & Confusion Matrix
mars_pred_train <- predict(mars_model_train)
mars_pred_factor_train <- ifelse(mars_pred_train >= 0.5, 1, 0)
mars_metrics <- cm(hr.trainset$Attrition, mars_pred_factor_train)
table(mars_pred_factor_train, hr.trainset$Attrition)
evimp(mars_model_train)

#Predicting using Testset & Confusion Matrix
mars_predicted <- predict(mars_model_train, newdata = hr.testset)
mars_predicted <- ifelse(mars_predicted >= 0.5, 1, 0)
mars_metrics <- cm(hr.testset$Attrition, mars_predicted)
table(mars_predicted, hr.testset$Attrition)

# Create table with performance metrics
models <- c( "MARS")
accuracy <- c(mars_metrics$accuracy)
fp_rate <- c(mars_metrics$fp_rate)
fn_rate <- c(mars_metrics$fn_rate)
precision <- c(mars_metrics$precision)
recall <- c(mars_metrics$recall)
performance_table <- data.frame(models, accuracy, fp_rate, fn_rate, precision, recall)
print(performance_table)



#-----------------------------------------------------------RANDOM FOREST------------------------------------------------------------------


#----------------------------Random Forest on Unbiased 13 features (final dataset)---------------------------

# create a random forest model 
rf_model1<- randomForest(Attrition ~ ., data = hr.trainset, importance = TRUE)

# evaluate the performance of the model on the training data
predictions <- predict(rf_model1, newdata = hr.trainset)
rf_metrics <- cm(hr.trainset$Attrition, predictions)
confusionMatrix(predictions, hr.trainset$Attrition, positive = "1")

# evaluate the performance of the model on the testing data
predictions <- predict(rf_model1, newdata = hr.testset)
rf_metrics <- cm(hr.testset$Attrition, predictions)
confusionMatrix(predictions, hr.testset$Attrition, positive = '1')


# Create table with performance metrics
models <- c( "Random Forest")
accuracy <- c(rf_metrics$accuracy)
fp_rate <- c(rf_metrics$fp_rate)
fn_rate <- c( rf_metrics$fn_rate)
precision <- c(rf_metrics$precision)
recall <- c(rf_metrics$recall)
performance_table <- data.frame(models, accuracy, fp_rate, fn_rate, precision, recall)
print(performance_table)








#----------------------------Random Forest on biased 26 features(original biased data set)---------------------------

# create a random forest model to select the most important features
rf_model3<- randomForest(Attrition ~ ., data = hr.trainset, importance = TRUE)


# evaluate the performance of the model on the testing data
predictions <- predict(rf_model3, newdata = hr.testset)
rf_metrics2 <- cm(hr.testset$Attrition, predictions)
confusionMatrix(predictions, hr.testset$Attrition, positive = "1")


# Create table with performance metrics
models <- c( "Random Forest")
accuracy <- c(rf_metrics2$accuracy)
fp_rate <- c(rf_metrics2$fp_rate)
fn_rate <- c( rf_metrics2$fn_rate)
precision <- c(rf_metrics2$precision)
recall <- c(rf_metrics2$recall)
performance_table <- data.frame(models, accuracy, fp_rate, fn_rate, precision, recall)
print(performance_table)




#--------------------------------------------------Creating Final Metric Table--------------------------------------------------



# Create table with performance metrics
models <- c( "CART","MARS","Random Forest")
accuracy <- c(cart_metrics$accuracy, mars_metrics$accuracy, rf_metrics$accuracy)
fp_rate <- c(cart_metrics$fp_rate, mars_metrics$fp_rate, rf_metrics$fp_rate)
fn_rate <- c(cart_metrics$fn_rate, mars_metrics$fn_rate, rf_metrics$fn_rate)
precision <- c(cart_metrics$precision, mars_metrics$precision, rf_metrics$precision)
recall <- c(cart_metrics$recall, mars_metrics$recall, rf_metrics$recall)
performance_table <- data.frame(models, accuracy, fp_rate, fn_rate, precision, recall)


# Create formatted table as a string
table_string <- paste0("| ", paste(performance_table$models, collapse=" | "), " |\n",
                       "|", paste(rep(" --- ", ncol(performance_table)), collapse="|"), "|\n")

for (i in 1:nrow(performance_table)) {
  table_string <- paste0(table_string, "| ", paste(performance_table[i,], collapse=" | "), " |\n")
}



# FINAL TABLE
kable(performance_table, format = "markdown", col.names = c("Model", "Accuracy", "False Positive Rate", "False Negative Rate", "Precision", "Recall"))
