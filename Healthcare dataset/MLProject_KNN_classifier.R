library(caret)
library(pROC)
library(dplyr)
library(mlbench)

health_dataset <- read.csv("D:\\DMML\\Data Sets\\healthcare_train_2v.csv")
# we have three data types in the dataset: categorical, numerical, IDs
# 12 variables and 43400 observations
str(health_dataset)


#-----------------------------------------DATA PREPROCESSING------------------------

# BMI has 3.4% of missing values.
sort(apply(health_dataset,2,function(x){sum(is.na(x))/length(x)}*100), decreasing = TRUE)

# Replacing NA with mean value in bmi
health_dataset$bmi<-ifelse(is.na(health_dataset$bmi), mean(health_dataset$bmi, na.rm = TRUE), health_dataset$bmi)

# Replacing blanks with "unknown", for smoking status
health_dataset$smoking_status[health_dataset$smoking_status == ""] <- NA
health_dataset$smoking_status <- as.factor(ifelse(is.na(health_dataset$smoking_status),"Unknown",paste(health_dataset$smoking_status)))     

#Eliminating attributes which are of no use (IDs)
health_dataset<-subset(health_dataset, select=-c(id))

#removing "others" from gender 
health_dataset<-filter(health_dataset,gender!="Other")

#Converting the hypertension into factor
#health_dataset$hypertension[health_dataset$hypertension == "0"] <- 'No'
#health_dataset$hypertension[health_dataset$hypertension == "1"] <- 'Yes'
#str(health_dataset)
health_dataset$stroke[health_dataset$stroke == "0"] <- 'No'
health_dataset$stroke[health_dataset$stroke == "1"] <- 'Yes'
str(health_dataset)
#health_dataset$hypertension <- as.factor(health_dataset$hypertension)
health_dataset$stroke <- as.factor(health_dataset$stroke)

str(health_dataset)

#----------------------------------Visualization---------------------------------------
# This boxplot demonstrates correlation between stroke and heart_desease. People with a heart disease
# have higher chances to get a stroke.
ggplot(health_dataset, aes(as.factor(heart_disease), age, fill = as.factor(heart_disease)))+
  geom_boxplot()+
  labs(title = "Heart Disease by stoke/non-stroke patients",
       x = "heart_disease")+
  scale_fill_discrete("heart_disease")+
  facet_wrap(~ stroke)+
  theme(plot.title = element_text(hjust = .5))

# Glucose level per jobe type. never_worked group has the lowest glucose lelvel, high stress work type like self-employed
# relatively has the highest glucose value. 
# The same interpretation can be applied to bmi.
ggplot(health_dataset, aes(reorder(work_type, avg_glucose_level), avg_glucose_level, fill = work_type))+
  geom_boxplot()+
  labs(    title = "Glucose level by Work Type",
           x = "work_type")+
  theme(plot.title = element_text(hjust = .5))


#-----------------------------------------Data Partition-------------------------
set.seed(1234)
ind <- sample(2, nrow(health_dataset), replace = T, prob = c(0.75,0.25))
training <- health_dataset[ind == 1,]
test <- health_dataset[ind == 2,]



# Handling imbalance
library(ROSE)
both = ovun.sample(stroke ~., data = training, method = "both",p = 0.3, N = 1102, seed = 222)$data
prop.table(table(both$stroke))
both$stroke

#---------------------------------KNN MODEL--------------------------------------
Control1 <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 3)
set.seed(222)
library(e1071)
fit <- train(stroke ~age +bmi+ avg_glucose_level+ heart_disease+hypertension,
             data = both,
             method = 'knn',
             tuneLength = 20,
             trControl = Control1,
             preProc = c("center","scale"))
fit

#plotting the model
plot(fit)
varImp(fit) # age turns out to be the most important parameter of getting a Stroke
# we can say that as you get older the tension gets higher and chances of getting an stroke increases

pred <- predict(fit,newdata = test)
confusionMatrix(pred, test$stroke,mode='everything')

