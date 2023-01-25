#Import libraries
data <- read.csv("C:\\Users\\soowe\\OneDrive\\Documents\\GitHub\\introtodatasc\\predictive modelling\\LungCancer2.csv")

library(ggplot2)
library(cowplot)
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
data$lung_cancer <- ifelse(data$lung_cancer == "YES", yes = "Unhealthy", no = "Healthy")
data$lung_cancer <- as.factor(data$lung_cancer)


##split data into training and testing
samp <- sample.split(data$lung_cancer, SplitRatio = 0.80)

train <- subset(data, samp == TRUE)
test<- subset(data, samp == FALSE)



#Fitting a random forest model
model0 <- randomForest(lung_cancer ~ ., data = train, ntree = 500, mtry = 4,importance = TRUE)


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
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
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
    
    
    colnames(test) <- tolower(colnames(test))
    
    
    Output <- data.frame(Prediction = predict(model0,test), round(predict(model0,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)



