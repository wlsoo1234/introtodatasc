## Random Forest Prediction Accurucy

#Random Forest 

data <- read.csv("C:\\Users\\soowe\\OneDrive\\Documents\\GitHub\\introtodatasc\\predictive modelling\\LungCancer2.csv")

library(ggplot2)
library(cowplot)
library(randomForest)
View(data)
head(data)
str(data)    # 1= no, 2=yes

colnames(data) <- tolower(colnames(data))

#formatting to factor
data$gender <- as.factor(data$gender)
data$smoking <- ifelse(test=data$smoking == 1, yes = 0, no = 1)
data$smoking <- as.factor(data$smoking)

data$yellow_fingers <- ifelse(test=data$yellow_fingers == 1, yes = 0, no = 1)
data$yellow_fingers<- as.factor(data$yellow_fingers)
data$anxiety <- ifelse(test=data$anxiety == 1, yes = 0, no = 1)
data$anxiety <- as.factor(data$anxiety)
data$peer_pressure <- ifelse(test=data$peer_pressure == 1, yes = 0, no = 1)
data$peer_pressure <- as.factor(data$peer_pressure)

data$chronic.disease <- ifelse(test=data$chronic.disease == 1, yes = 0, no = 1)
data$chronic.disease <- as.factor(data$chronic.disease)
data$fatigue <- ifelse(test=data$fatigue == 1, yes = 0, no = 1)
data$fatigue <- as.factor(data$fatigue)
data$allergy <- ifelse(test=data$allergy == 1, yes = 0, no = 1)
data$allergy <- as.factor(data$allergy)

data$wheezing <- ifelse(test=data$wheezing == 1, yes = 0, no = 1)
data$wheezing <- as.factor(data$wheezing)
data$alcohol.consuming <- ifelse(test=data$alcohol.consuming == 1, yes = 0, no = 1)
data$alcohol.consuming <- as.factor(data$alcohol.consuming)

data$shortness.of.breath <- ifelse(test=data$shortness.of.breath == 1, yes = 0, no = 1)
data$shortness.of.breath <- as.factor(data$shortness.of.breath)

data$swallowing.difficulty <- ifelse(test=data$swallowing.difficulty == 1, yes = 0, no = 1)
data$swallowing.difficulty <- as.factor(data$swallowing.difficulty)

data$chest.pain <- ifelse(test=data$chest.pain == 1, yes = 0, no = 1)
data$chest.pain <- as.factor(data$chest.pain)
data$lung_cancer <- ifelse(test=data$lung_cancer == "YES", yes = "Unhealthy", no = "Healthy")
data$lung_cancer <- as.factor(data$lung_cancer)

# Barplot to view the result of Lung cancer. The output is shown below.
barplot(table(data$lung_cancer))
table(data$lung_cancer) 

# Next, we need to split the data into training and testing. 80% for training, 20% for testing.
set.seed(123)
samp <- sample(nrow(data), 0.8 * nrow(data))
train <- data[samp, ]
test <- data[-samp, ]

# Moving onto the Data visualization
library(ggplot2)

# This command is used to display a scatter plot. The output looks like below
ggplot(data,aes(age,smoking))+ 
  geom_point(aes(color=lung_cancer))
#This command is used to display a stacked bar chart. The output looks like below
ggplot(data,aes(age)) + 
  geom_histogram(aes(fill=lung_cancer),color='black',bins=50)


# Checks the dimensions of training and testing datase
dim(train)
dim(test)


library(randomForest)

# Let’s build the random forest model

model <- randomForest(lung_cancer ~ ., data = train, ntree = 500, mtry = 4)
model
model$confusion

# The next step is to validate our model using the test data
prediction <- predict(model, newdata = test)
table(prediction, test$lung_cancer)
prediction

# Now, let’s display the predicted vs. the actual values
results<-cbind(prediction,test$lung_cancer)
results
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
View(results)


# Finally, let’s calculate the accuracy of the model
# The output is as shown below
sum(prediction==test$lung_cancer) / nrow(test) 

