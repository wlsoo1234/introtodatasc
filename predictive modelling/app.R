library(shiny)
library(shinythemes)
library(randomForest)
library(data.table)
library(RCurl)
library(caTools)

#Random Forest
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



ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "DATA OF CANCER",
                  tabPanel("Introduction",
                           
                           mainPanel(
                             h1("OUR APP "),
                             h2("What is prediction in data science?"),
                             p("“Prediction” refers to the output of an algorithm after it has been trained on a historical dataset and applied to new data when forecasting the likelihood of a particular outcome, such as whether or not a customer will churn in 30 days."),
                             p("Our project is related to the cancer available in the South East Asia. Hence, here we will be doing a simple prediction of the most common cancer in the world – lung cancer, in order to find out how the prediction goes with the dataset we have."),
                             h2("Lung Cancer "),
                             p("When cancer starts in the lungs, it is called lung cancer. Lung cancer begins in the lungs and may spread to lymph nodes or other organs in the body, such as the brain. Cancer from other organs also may spread to the lungs. When cancer cells spread from one organ to another, they are called metastases."),
                             h2("Available Data"),
                             p("We have a dataset collected from the Kaggle website. It is about the lung cancer data with 309 observations, with 15 independent variables and 1 dependent variable. The 15 independent variable are: gender, age, smoking, yellow_fingers, anxiety, peer_pressure, chronic.disease, fatigue, allergy, wheezing, alcohol.consuming, coughing, shortness.of.breath, swallowing.difficulty, and chest.pain. The only 1 dependent variable is lung_cancer. Except age, all other 14 variables are all binary, which means that they only consists of two possible values: either 1(yes) or 2(no) and M or F. "),
                             h2("Why Choose Logistics Regression and Random Forest Model?"),
                             p("Since we have mentioned above, almost all of the variables in the data are binary, hence, logistic regression and random forest models are to be chosen due to their characteristics as being binary classifier. Both two models can handle the values properly and efficiently, to finally come out with the higher prediction accuracy. "),
                             p("Logistics regression is used to predicts something will happen or not happen. From the results, it can be said that logistic regression is a binary classifier, which the outcome of the response variable is either yes or no, or true or false. In contrast, linear regression is used to predict a continuous variable, like height and weight. It will answer the questions “how much”."),
                             p("Random forest is a popular supervised machine learning algorithm. It can be used for both classification and regression problems. It is based on the concept of ensemble learning, which enables users to combine multiple classifiers to solve a complex problem and to also improve the performance of the model.")
                             
                             
                           ) 
                           
                  ), 
                  tabPanel("Know The Cancers", 
                           sidebarPanel(
                             selectInput("Cancer","Cancer",choices = c("Breast Cancer","Lung Cancer","Colorectum Cancer","Liver Cancer","Cervix Uteri Cancer","Prostate Cancer","Stomach Cancer","non-hodgkin lymphoma","Leukaemia","Nasopharnyx"),selected="Breast Cancer")
                           ),
                           mainPanel(
                             uiOutput("Cancer")
                             
                             
                             
                           ),
                  ),
                  tabPanel("Cancer Statistics", 
                           sidebarPanel(
                             selectInput("Country","Country",choices = c("Brunei","Cambodia","Indonesia","Laos","Malaysia","Myanmar","Philippines","Singapore","Thailand","Vietnam"),selected="Brunei")
                           ),
                           mainPanel(
                             uiOutput("Country")
                             
                            ),
                  ),
                  
                  tabPanel("Random Forest", 
                           sidebarPanel(
                             HTML("<h3>Method: Random Forest</h3>"),
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
                  ))))















# Define server function  
server <- function(input, output, session) {
  
  output$Cancer <- renderUI({
    if(input$Cancer == "Breast Cancer"){
      
      img(h1("Breast Cancer"),
          h5("Breast cancer is known as a disease in which cells in the breast grow abnormally. It is possible for the breast cancer to spread outside the breast via the blood vessels and lymph vessels. When it is spread outside, it is said to have metastasized. "),
          h3("Symptoms"),
          h5("-New lump in the breast or underarm"),
          h5("-Irritation or dimpling of breast skin"),
          h5("-Nipple discharging other than breast milk, including blood"),
          h3("Factors"),
          h5("-Inherited genes that increase cancer risk"),
          h5("-Increasing age"),
          h5("-Having never been pregnant")
          
      )
      
      
      
      
    }else if(input$Cancer == "Lung Cancer"){
      img(h1("Lung Cancer"),
          h5("There are 2 types of lung cancers: non-small cell lung cancer (NSCLC) and small cell lung cancer (SCLC). NSCLC contributes to around 80% to 85% of overall lung cancers. On the other hand, SCLC contributes to about 10% to 15% of overall lung cancers worldwide. About 70% people with SCLC will have cancer that has already spread at the time they are diagnosed. "),
          h3("Symptoms"),
          h5("-Shortness of breath"),
          h5("-Coughing up blood"),
          h5("-Chest pain"),
          h3("Factors"),
          h5("-Smoking"),
          h5("-Genes"),
          h5("-Hazardous Chemicals"),
      )
      
      
      
      
      
      
    }else if(input$Cancer == "Colorectum Cancer"){
      
      img(h1("Colorectum Cancer / Colorectal Cancer "),
          h5("Colorectal cancer is a type of cancer that begins in the colon or the rectum. When the cells start to grow abnormally on the inner lining of colon and rectum, polyps are formed. Among three types of polyps, adenomas and SSP and TSA have higher chance of turning into colorectal cancer, while hyperplastic polyps and inflammatory polyps are not pre-cancerous. "),
          h3("Symptoms"),
          h5("-Change in bowel movement (diarrhea, constipation)"),
          h5("-Hemorrhoids, anal tears"),
          h5("-Blood in the stool"),
          h3("Factors"),
          h5("-A diet low in fruit and vegetables (fiber)"),
          h5("-Overweight and obesity"),
          h5("-Lack of physical activities"),
          h5("-Age > 50"),
          h5("-Inflammatory bowel disease (IBD)"),
          h5("-Family history of colorectal cancer"),
      )
      
      
      
      
    }else if(input$Cancer == "Liver Cancer"){
      
      img(h1("Liver cancer "),
          h5("There are many types of liver cancer, but for the most common types are to be hepatocellular carcinoma (HCC), intrahepatic cholangiocarcinoma (bile duct cancer), and angiosarcoma and hemangiosarcoma. There are also secondary liver cancer where the cancer spot is spread from the primary site, for example, benign liver tumours, haemangioma, and hepatic adenoma. 8 "),
          h3("Symptoms"),
          h5("-Weight loss"),
          h5("-Loss of appetite"),
          h5("-Upper abdominal pain"),
          h3("Factors"),
          h5("-Chronic viral hepatitis"),
          h5("-Heavy alcohol use"),
          h5("-Tobacco use"),
          h5("-Aflatoxins"),
          h5("-Type 2 diabetes/ obese"),
          h5("-Cirrhosis"),
      )
      
      
      
      
      
    }else if(input$Cancer == "Cervix Uteri Cancer"){
      
      img(h1("Cervix Uteri Cancer (Cervical Cancer) "),
          h5("Two types of cervical cancer – squamous cell carcinoma and adenocarcinoma – are available. Squamous cell carcinoma begins in the thin, flat cells lining the outer part of the cervix, which projects into the vagina while adenocarcinoma begins in the column-shaped glandular cells that line the cervical canal. "),
          h3("Symptoms"),
          h5("-vaginal bleeding between periods"),
          h5("-menstrual bleeding that is longer or heavier than usual"),
          h5("-pain during intercourse"),
          h5("-bleeding after intercourse"),
          h5("-pelvic pain"),
          h5("-a change in your vaginal discharge such as more discharge or it may have a strong or unusual colour or smell"),
          h5("-vaginal bleeding after menopause."),
          h3("Factors"),
          h5("-Human Papilloma Virus (HPV)"),
          h5("-weakened immune system"),
          h5("-Has given birth to multiple children or had children at an early age"),
      )
      
      
      
      
    }else if(input$Cancer == "Prostate Cancer"){
      
      img(h1("Prostate Cancer "),
          h5("Basically, prostate cancer grows very slowly and does not show any symptoms. It may not become advanced cancer. Such cancers may not need any treatment. But, when prostate cancers start to spread to the other tissue around the prostate, it is called invasive prostate cancer. "),
          h3("Symptoms"),
          h5("-weak or interrupted urine flow"),
          h5("-urge to urinate frequently at night"),
          h5("-frequent urination"),
          h5("-pain during ejaculation"),
          h5("-pain or burning during urination"),
          h5("-blood in urine or semen"),
          h3("Factors"),
          h5("-Age"),
          h5("-family history"),
          h5("-hormones"),
          h5("-obesity"),
      )
      
      
      
      
    }else if(input$Cancer == "Stomach Cancer"){
      
      img(h1("Stomach Cancer "),
          h5("The most common stomach cancer is known as adenocarcinomas. It contributes to around 90% to 95% of overall stomach cancer. These cancers or tumours develop from the gland cells in the innermost lining of the stomach. "),
          h3("Symptoms"),
          h5("-Indigestion or heartburn"),
          h5("-Pain or discomfort in the abdomen"),
          h5("-Nausea and vomiting, particularly vomiting up solid food shortly after eating"),
          h5("-Diarrhea or constipation"),
          h5("-Bloating of the stomach after meals"),
          h5("-Loss of appetite"),
          h5("-Sensation of food getting stuck in the throat while eating"),
          h5("-Weakness and fatigue"),
          h5("-Vomiting blood or having blood in the stool"),
          h5("-Unexplained weight loss"),
          h3("Factors"),
          h5("-Age (older than 55 years old)"),
          h5("-Gender (Men >2x Women)"),
          h5("-Bacteria (Helicobacter pylori), causing inflamation, probably inheritance"),
          h5("-Family history/ genetics (Lynch syndrome, hereditary breast and ovarian cancer, familial adenomatous polyposis"),
          h5("-Race/ ethnicity (Black, Hispanic, Asian >>White)"),
          h5("-Diet (High sodium diet)"),
      )
      
      
      
      
    }else if(input$Cancer == "non-hodgkin lymphoma"){
      
      img(h1("Non-Hodgkin Lymphoma Cancer "),
          h5("Non-Hodgkin Lymphoma Cancer is a type of cancer that start in the lymphatic system. Lymphatic system is the body’s germ-fighting immune system that contains white blood cells called lymphocytes. When lymphocytes start to grow abnormally forming tumours inside the lymphatic system, non-Hodgkin’s lymphoma cancer happens. The most common subtypes under non-Hodgkin’s lymphoma cancer are diffuse large B-cell lymphoma and follicular lymphoma. "),
          h3("Symptoms"),
          h5("-Swollen lymph nodes in the neck, armpits, and groin which are painless"),
          h5("-Abdominal pain or swelling"),
          h5("-Loss of appetite"),
          h5("-Skin itching"),
          h5("-Chest pain"),
          h5("-Trouble breathing"),
          h3("Factors"),
          h5("-Age (60 ~ 70)"),
          h5("-Bacterial infections (Helicobacter pylori)"),
          h5("-Gender (Men >Women)"),
          h5("-Viruses (Epstein-Barr virus, hepatitis C)"),
          h5("-Immune deficiency disorders (HIV/ AIDS)"),
          h5("-Autoimmune disorders"),
      )
      
      
      
      
    }else if(input$Cancer == "Leukaemia"){
      
      img(h1("Leukaemia Cancer "),
          h5("Leukaemia is type of cancer of the body’s blood forming tissues. It occurs when the production of excess white blood cells that are abnormal and the cells do not function properly due to the mutations in their genetic materials or DNA in the white blood cells. Hence, symptoms and infections will start to occur upon the one with leukaemia. "),
          h3("Symptoms"),
          h5("-Fever or chills"),
          h5("-Persistent fatigue"),
          h5("-Swollen lymph nodes, enlarged liver or spleen"),
          h5("-Easy bleeding, recurrent nosebleeds"),
          h5("-Tiny red spots on the skin (petechiae)"),
          h5("-Excessive sweating especially at night"),
          h5("-Bone pain or tenderness"),
          h3("Factors"),
          h5("-Genetic disorder (Congenital diseases)"),
          h5("-Exposure to certain chemicals"),
          h5("-Smoking"),
          h5("-Family history of leukemia"),
          h5("-Chemotherapy in the past"),
      )
      
      
      
      
    }else{
      img(h1("Nasopharynx Cancer (Nasopharyngeal Cancer) "),
          h5("Nasopharyngeal cancer is cancer that grows in the nasopharynx, which is located at the upper part of the throat behind the nose. Nasopharynx is a box-like chamber in the upper part of pharynx. The oropharynx is below the nasopharynx and the hypopharynx is just below the oropharynx."),
          h3("Symptoms"),
          h5("-A lump in the neck that doesn't go away after 3 weeks"),
          h5("-Hearing loss (usually in 1 ear)"),
          h5("-Tinnitus (hearing sounds that come from inside the body rather than from an outside source)"),
          h5("-Blood in saliva"),
          h5("-Blood discharge from nose"),
          h5("-Nasal congestion or ringing in ear"),
          h5("-Frequent ear infection"),
          h5("-Sore throat"),
          h5("-Headaches"),
          h3("Factors"),
          h5("-Race (More commonly affects people in parts of China, South east asia and Northern Africa"),
          h5("-Diet (High in salt-cured food; fish and meat starting at an early age"),
          h5("-Family history"),
          h5("-Age (Commonly diagnosed in adults aged between 30 to 50)"),
          h5("-Gender (more common in men)"),
          h5("-Epstein-Barr virus"),
      )
      
      
      
      
    }
    
    
    
  })
  output$Country <- renderUI({
    if(input$Country == "Brunei"){
      tags$img(src = "Brunei.png",
               alt = "Flag of Brunei", width = "100%")
      
      
      
    }
    else if(input$Country == "Cambodia"){
      
      tags$img(src = "Cambodia.png",
               alt = "Flag of Cambodia", width = "100%")
      
      
      
      
    }else if(input$Country == "Indonesia"){
      
      tags$img(src = "Indonesia.png",
               alt = "Flag of Indonesia", width = "100%")
      
      
      
      
    }else if(input$Country == "Laos"){
      
      tags$img(src = "Laos.png",
               alt = "Flag of Laos", width = "100%")
      
      
      
      
    }else if(input$Country == "Malaysia"){
      
      tags$img(src = "Malaysia.png",
               alt = "Flag of Malaysia", width = "100%")
      
      
      
      
    }else if(input$Country == "Myanmar"){
      
      tags$img(src = "Myanmar.png",
               alt = "Flag of Myanmar", width = "100%")
      
      
      
      
    }else if(input$Country == "Philippines"){
      
      tags$img(src = "Philippines.png",
               alt = "Flag of Philippines", width = "100%")
      
      
      
      
    }else if(input$Country == "Singapore"){
      
      tags$img(src = "Singapore.png",
               alt = "Flag of Singapore", width = "100%")
      
      
      
      
    }else if(input$Country == "Thailand"){
      
      tags$img(src = "Thailand.png",
               alt = "Flag of Thailand", width = "100%")
      
      
      
      
    }else{
      tags$img(src = "Vietnam.png",
               alt = "Flag of Vietnam", width = "100%")
      
      
      
      
    }
    
    
    
  })
  
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


# Create Shiny object
shinyApp(ui = ui, server = server)