#Logistic Regression - Simplilearn

# Define the problem
# Load the libraries
# Acquire the data
# Ingest the data
# Set the working directory
# Explore the data
# Munge the data (if necessary)
# Prepare the data
# Scale the data (if necessary)
# Split the data into train and test sets
# Train the model data through the model
# Validate the model - accuracy, precision, etc.

library(caTools)

data <- read.csv("C:\\Users\\soowe\\OneDrive\\Documents\\GitHub\\introtodatasc\\predictive modelling\\LungCancer2.csv")

head(data)
str(data)    # 1= no, 2=yes

colnames(data) <- tolower(colnames(data))

#formatting to factor
data$gender <- as.factor(data$gender)
data$smoking <- ifelse(test=data$smoking == 1, yes = 0, no = 1)
data$smoking <- as.factor(data$smoking)
#data$yellow_fingers <- ifelse(test=data$yellow_fingers == 1, yes = 0, no = 1)
#data$yellow_fingers<- as.factor(data$yellow_fingers)
#data$anxiety <- ifelse(test=data$anxiety == 1, yes = 0, no = 1)
#data$anxiety <- as.factor(data$anxiety)
#data$peer_pressure <- ifelse(test=data$peer_pressure == 1, yes = 0, no = 1)
#data$peer_pressure <- as.factor(data$peer_pressure)
data$chronic.disease <- ifelse(test=data$chronic.disease == 1, yes = 0, no = 1)
data$chronic.disease <- as.factor(data$chronic.disease)
data$fatigue <- ifelse(test=data$fatigue == 1, yes = 0, no = 1)
data$fatigue <- as.factor(data$fatigue)
data$allergy <- ifelse(test=data$allergy == 1, yes = 0, no = 1)
data$allergy <- as.factor(data$allergy)
#data$wheezing <- ifelse(test=data$wheezing == 1, yes = 0, no = 1)
#data$wheezing <- as.factor(data$wheezing)
#data$alcohol.consuming <- ifelse(test=data$alcohol.consuming == 1, yes = 0, no = 1)
#data$alcohol.consuming <- as.factor(data$alcohol.consuming)
data$coughing <- ifelse(test=data$coughing == 1, yes = 0, no = 1)
data$coughing <- as.factor(data$coughing)
data$shortness.of.breath <- ifelse(test=data$shortness.of.breath == 1, yes = 0, no = 1)
data$shortness.of.breath <- as.factor(data$shortness.of.breath)
#data$swallowing.difficulty <- ifelse(test=data$swallowing.difficulty == 1, yes = 0, no = 1)
#data$swallowing.difficulty <- as.factor(data$swallowing.difficulty)
data$chest.pain <- ifelse(test=data$chest.pain == 1, yes = 0, no = 1)
data$chest.pain <- as.factor(data$chest.pain)
data$lung_cancer <- ifelse(test=data$lung_cancer == "YES", yes = "Unhealthy", no = "Healthy")
data$lung_cancer <- as.factor(data$lung_cancer)


split <- sample.split(data, SplitRatio=0.8)
train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")


mymodel <- glm(lung_cancer ~ ., data=train, family = "binomial")
summary(mymodel)


res <- predict(mymodel, test, type="response")
res
res <- predict(mymodel, train, type="response")
res

confmatrix <- table(Actual_Value=train$lung_cancer, Predicted_value = res > 0.5)
confmatrix


(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
