#Import libraries

library(randomForest)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(caTools)

#Read data
data <- read.csv("https://raw.githubusercontent.com/wlsoo1234/introtodatasc/main/predictive%20modelling/LungCancer2.csv",stringsAsFactors = FALSE)

colnames(data) <- tolower(colnames(data))
data

#formatting 
{
data$gender <- as.factor(data$gender)
data$smoking <- ifelse(data$smoking == 1, yes = 0, no = 1)
data$yellow_fingers <- ifelse(data$yellow_fingers == 1, yes = 0, no = 1)
data$anxiety <- ifelse(data$anxiety == 1, yes = 0, no = 1)
data$peer_pressure <- ifelse(data$peer_pressure == 1, yes = 0, no = 1)
data$chronic.disease <- ifelse(data$chronic.disease == 1, yes = 0, no = 1)
data$fatigue <- ifelse(data$fatigue == 1, yes = 0, no = 1)
data$allergy <- ifelse(data$allergy == 1, yes = 0, no = 1)
data$wheezing <- ifelse(data$wheezing == 1, yes = 0, no = 1)
data$alcohol.consuming <- ifelse(data$alcohol.consuming == 1, yes = 0, no = 1)
data$coughing <- ifelse(data$coughing == 1, yes = 0, no = 1)
data$shortness.of.breath <- ifelse(data$shortness.of.breath == 1, yes = 0, no = 1)
data$swallowing.difficulty <- ifelse(data$swallowing.difficulty == 1, yes = 0, no = 1)
data$chest.pain <- ifelse(data$chest.pain == 1, yes = 0, no = 1)
data$lung_cancer <- ifelse(data$lung_cancer == "YES", yes = "Cancer Positive", no = "Cancer Negative")
data$lung_cancer <- as.factor(data$lung_cancer)
}

##split data into training and testing
samp <- sample.split(data$lung_cancer, SplitRatio = 0.80)

train <- subset(data, samp == TRUE)
testdata<- subset(data, samp == FALSE)

str(train)

#Fitting a random forest model
model0 <- randomForest(lung_cancer ~ ., data = train, ntree = 1000, mtry = 5,importance = TRUE)

#Fitting a Logistic Regression model
model1 <- glm(lung_cancer ~ ., data=data, family = "binomial")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Prediction of lung cancer using the parameter of symptoms and habits'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  tags$h4("Input:"),
                  
                  radioButtons("GENDER", label = "Gender", choices = c("M","F")),
                  sliderInput("AGE", "Age",
                              min = 0, max = 110,
                              value = 50),
                  radioButtons("SMOKING", label = "Smoking?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("YELLOW_FINGERS", label = "Yellow fingers?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("ANXIETY", label = "Often feeling anxious?", choices = c("Yes","No")),
                  radioButtons("PEER_PRESSURE", label = "Peer Pressure?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("CHRONIC.DISEASE", label = "Chronic Disease?", choices = c("Yes","No")),
                  radioButtons("FATIGUE", label = "Often feeling tired? (Fatigue)", choices = c("Yes","No")),
                  radioButtons("ALLERGY", label = "Facing allergy?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("WHEEZING", label = "Wheezing?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("ALCOHOL.CONSUMING", label = "Often drinking alcohol?", choices = c("Yes","No")),
                  radioButtons("COUGHING", label = "Coughing?", choices = c("Yes","No")),
                  radioButtons("SHORTNESS.OF.BREATH", label = "Hard to breath?", choices = c("Yes","No"),selected = "No"),
                  radioButtons("SWALLOWING.DIFFICULTY", label = "Hard to swallow?", choices = c("Yes","No")),
                  radioButtons("CHEST.PAIN", label = "Chest Pain?", choices = c("Yes" ,"No"),selected = "No"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tags$label(h4('Using the Random Forest Model:')), 
                  tableOutput('tabledata1'), # Prediction results table
                  tags$label(h4('Using the Logistic Regression Model:')), 
                  tableOutput('tabledata2') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput1 <- reactive({  
    
    df <- data.frame(
      Name = c("GENDER","AGE","SMOKING","YELLOW_FINGERS","ANXIETY",
               "PEER_PRESSURE","CHRONIC DISEASE","FATIGUE","ALLERGY",
               "WHEEZING","ALCOHOL CONSUMING","COUGHING",
               "SHORTNESS OF BREATH","SWALLOWING DIFFICULTY",
               "CHEST PAIN"),
      
      
      
      Value = as.character(c(input$GENDER, input$AGE, input$SMOKING, input$YELLOW_FINGERS,
                             input$ANXIETY, input$PEER_PRESSURE, input$CHRONIC.DISEASE,
                             input$FATIGUE, input$ALLERGY, input$WHEEZING,
                             input$ALCOHOL.CONSUMING, input$COUGHING,
                             input$SHORTNESS.OF.BREATH, input$SWALLOWING.DIFFICULTY,
                             input$CHEST.PAIN)),
      stringsAsFactors = FALSE)
    
    for(i in 3:15){
      df[i,2] <- ifelse(df[i,2] == "No", "0", "1")
    }
    
    lung_cancer <- "lung_cancer"
    df<- rbind(df, lung_cancer)
    input <- transpose(df)
    
    write.table(input,"rf0.csv", sep=",",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("rf0", ".csv", sep=""),header = TRUE)
    
    if(test$GENDER == F){
      test$GENDER <- "F"
    }else{
      test$GENDER <- "M"
    }
    test$GENDER<- as.character(test$GENDER)
    
    {
    test$GENDER <- factor(test$GENDER , levels = c("M", "F"))
    test$AGE <- as.integer(test$AGE) 
    test$SMOKING <- as.numeric(test$SMOKING) 
    test$YELLOW_FINGERS <- as.numeric(test$YELLOW_FINGERS)
    test$ANXIETY <- as.numeric(test$ANXIETY) 
    test$PEER_PRESSURE <- as.numeric(test$PEER_PRESSURE) 
    test$CHRONIC.DISEASE <- as.numeric(test$CHRONIC.DISEASE)
    test$FATIGUE <- as.numeric(test$FATIGUE)
    test$ALLERGY <- as.numeric(test$ALLERGY)
    test$WHEEZING <- as.numeric(test$WHEEZING)
    test$ALCOHOL.CONSUMING <- as.numeric(test$ALCOHOL.CONSUMING) 
    test$COUGHING <- as.numeric(test$COUGHING)
    test$SHORTNESS.OF.BREATH <- as.numeric(test$SHORTNESS.OF.BREATH)
    test$SWALLOWING.DIFFICULTY <- as.numeric(test$SWALLOWING.DIFFICULTY)
    test$CHEST.PAIN <- as.numeric(test$CHEST.PAIN)
    }
    
    colnames(test) <- tolower(colnames(test))
    
    
    Output <- data.frame(Prediction = predict(model0,test), round(predict(model0,test,type="prob"), 3))
    print(Output)
    
  })
  
  datasetInput2 <- reactive({  
    
    df <- data.frame(
      Name = c("GENDER","AGE","SMOKING","YELLOW_FINGERS","ANXIETY",
               "PEER_PRESSURE","CHRONIC DISEASE","FATIGUE","ALLERGY",
               "WHEEZING","ALCOHOL CONSUMING","COUGHING",
               "SHORTNESS OF BREATH","SWALLOWING DIFFICULTY",
               "CHEST PAIN"),
      
      
      
      Value = as.character(c(input$GENDER, input$AGE, input$SMOKING, input$YELLOW_FINGERS,
                             input$ANXIETY, input$PEER_PRESSURE, input$CHRONIC.DISEASE,
                             input$FATIGUE, input$ALLERGY, input$WHEEZING,
                             input$ALCOHOL.CONSUMING, input$COUGHING,
                             input$SHORTNESS.OF.BREATH, input$SWALLOWING.DIFFICULTY,
                             input$CHEST.PAIN)),
      stringsAsFactors = FALSE)
    
    for(i in 3:15){
      df[i,2] <- ifelse(df[i,2] == "No", "0", "1")
    }
    
    lung_cancer <- "lung_cancer"
    df<- rbind(df, lung_cancer)
    input <- transpose(df)
    
    write.table(input,"lr0.csv", sep=",",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test0 <- read.csv(paste("lr0", ".csv", sep=""),header = TRUE)
    
    if(test0$GENDER == F){
      test0$GENDER <- "F"
    }else{
      test0$GENDER <- "M"
    }
    test0$GENDER<- as.character(test0$GENDER)
    
    {
      test0$GENDER <- factor(test0$GENDER , levels = c("M", "F"))
      test0$AGE <- as.integer(test0$AGE) 
      test0$SMOKING <- as.numeric(test0$SMOKING) 
      test0$YELLOW_FINGERS <- as.numeric(test0$YELLOW_FINGERS)
      test0$ANXIETY <- as.numeric(test0$ANXIETY) 
      test0$PEER_PRESSURE <- as.numeric(test0$PEER_PRESSURE) 
      test0$CHRONIC.DISEASE <- as.numeric(test0$CHRONIC.DISEASE)
      test0$FATIGUE <- as.numeric(test0$FATIGUE)
      test0$ALLERGY <- as.numeric(test0$ALLERGY)
      test0$WHEEZING <- as.numeric(test0$WHEEZING)
      test0$ALCOHOL.CONSUMING <- as.numeric(test0$ALCOHOL.CONSUMING) 
      test0$COUGHING <- as.numeric(test0$COUGHING)
      test0$SHORTNESS.OF.BREATH <- as.numeric(test0$SHORTNESS.OF.BREATH)
      test0$SWALLOWING.DIFFICULTY <- as.numeric(test0$SWALLOWING.DIFFICULTY)
      test0$CHEST.PAIN <- as.numeric(test0$CHEST.PAIN)
    }
    
    colnames(test0) <- tolower(colnames(test0))
    
    res <- predict(model1, test0, type="response")
    res
    
    if(res > 0.5){
      kena = "Cancer Positive"
    }else{
      kena = "Cancer Negative"
    }
    
    Output0 <- data.frame(Prediction = kena, Probability = res)
    print(Output0)
    
  })
  
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Random Forest Prediction results table
  output$tabledata1 <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput1()) 
    } else{
      return("No data input / submitted")
    }
  })
  
  # LR Prediction results table
  output$tabledata2 <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput2()) 
    } else{
      return("No data input / submitted")
    }
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)



