lungData <- read.csv("C:/Users/bblue/OneDrive/Documents/MachineLearning/lungCancer/lungData.csv")
library(ggplot2)
library(stats)
library(dplyr)
library(shiny)
library(shinythemes)
library(randomForest)
#View(lungData)
#colnames(lungData)

ui<- fluidPage(theme= shinytheme("united"),
    headerPanel('Lung Cancer'),
    sidebarPanel(
      h2("Input Params"),
      radioButtons("gender", h3("Enter your gender:"),choices = list("Male" = "M", "Female" = "F"),selected = "M"),
      numericInput("age", h3("Enter your age:"), value = 1),
      radioButtons("smoking", h3("Do you smoke?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("yellow_fingers", h3("Do you have yellow fingers?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("anxiety", h3("Do you have anxiety?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("peer_pressure", h3("Do you have peer pressure?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("chronic.disease", h3("Do you have a chronic disease?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("fatigue", h3("Do you have fatigue?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("allergy", h3("Do you have any allergies?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("wheezing", h3("Do you have common wheezing?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("alcohol.consuming", h3("Do you consume alcohol?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("coughing", h3("Do you cough frequently?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("shortness.of.breath", h3("Do you frequently have shortness of breath?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("swallowing.difficulty", h3("Do you have difficulty swallowing?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      radioButtons("chest.pain", h3("Do you have chest pain?"),choices = list("Yes" = 2, "No" = 1),selected = 1),
      actionButton("submitBtn", "Make Prediction", class="btn btn-primary")
    ), 
    mainPanel(
       tags$label(h3('Cancer Prediction')),
       verbatimTextOutput('contents'),
       tableOutput('tabledata')
    )
               
)

server<-function(input, output, session){
  index = sample(2, nrow(lungData), replace=T, prob=(c(0.7,0.3)))
  Training = lungData[index==1,]
  Testing = lungData[index==2,]
  ggplot(data = lungData, aes(x=AGE, fill=as.factor(LUNG_CANCER)))+ geom_histogram()
  
  RFM = randomForest(as.factor(LUNG_CANCER)~., data=Training)
  
  output$contents <- renderPrint({
    if(input$submitBtn>0){
      
      inputData <- data.frame(GENDER = input$gender, AGE = input$age,
                              SMOKING = input$smoking, YELLOW_FINGERS = input$yellow_fingers,
                              ANXIETY = input$anxiety, PEER_PRESSURE = input$peer_pressure,
                              CHRONIC.DISEASE = input$chronic.disease, FATIGUE = input$fatigue,
                              ALLERGY = input$allergy, WHEEZING = input$wheezing,
                              ALCOHOL.CONSUMING = input$alcohol.consuming, COUGHING = input$coughing,
                              SHORTNESS.OF.BREATH = input$shortness.of.breath, SWALLOWING.DIFFICULTY = input$swallowing.difficulty,
                              CHEST.PAIN = input$chest.pain)
      
      predict(RFM, inputData)
    }
  })
}
shinyApp(ui=ui, server=server)

