library(shiny)
library(datasets)
library(boot)
library(DAAG)
library(class)
library(plotrix)
library(KNNShiny)
library(DT)
library(tidyverse)
library(ROCR)
library(pROC)
library(caret)
library(caTools)
library(randomForest)
library(MLmetrics)

# UI 
ui<-(fluidPage(
  
  # Titre
  titlePanel("TP Analyse Univariee (Travail en plus : Classement)"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3("Donnees"),
      selectInput("dataset", "Selectionnez le dataset:", 
                  choices = c("Employee Attrition")),
      h3("Choisir la portion d'entrainement"),
      sliderInput("trainsplit", "Portion training:",
                  min = 0.05, max = 0.95,
                  value = 0.5, step = 0.05
      ),
      h3("Choisir les parametres d'entrainement"),
      checkboxInput("option2", "Over Sampling", value = FALSE, width = NULL),
      checkboxInput("option3", " Under Sampling", value = FALSE, width = NULL),
      h2("Realisee par les Etudiants : "),
      h3("SABA Amir"),
      h3("LADAYCIA Bouteyna"),
      h3("DEBBAGH Abdelkader Nadir")
    ),
    
    
    
    # MainPanel divisé en TabPanel
    mainPanel(
      
      conditionalPanel(
        condition = "input.dataset == 'Employee Attrition'",
        
        tabsetPanel(
          #Panel de la presentation des donnees
          tabPanel("Presentation des donnees",
                   tabsetPanel(tabPanel("Table", h1("Table"),DT::dataTableOutput("view_spr")),
                               tabPanel("Analyse Univariee", h1("Analyse univariee"),verbatimTextOutput("summary")),
                               tabPanel("Analyse Bivariee", h1("Analyse Bivariee"),plotOutput("plotknn"))
                   )),
          #Panel de l'exploration des variables
          tabPanel("Exploration des variables",
                   tabsetPanel(tabPanel("Table", h1("Table"),DT::dataTableOutput("n")),
                               tabPanel("Analyse Univariee", h1("Analyse univariee"),verbatimTextOutput("r")),
                               tabPanel("Analyse Bivariee", h1("Analyse Bivariee"),plotOutput("o"))
                   )),
          #Panel de classification
          tabPanel("Classification",
                   tabsetPanel(
                     tabPanel("Random Forest", h1("Random Forest"),fluidRow(column(2, h4("Precison testing: ")), column(2, textOutput("Acc_RF"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_RF"))),plotOutput("Classification_rf")),
                     tabPanel("Logistic Regression", h1("Logistic Regression"), fluidRow(column(2, h4("Precison testing: ")), column(2, textOutput("Acc_LR"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_LR"))),
                              plotOutput("classification_lr"))
                   )),
          
          tabPanel("Aide", h1("Aide"),verbatimTextOutput("Help"))
          
        ) 
      ),
    )
  )
)
)


server <- function(input, output){
  
  #Récupération du Dataset selectionné
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Employee Attrition" = read_csv("Employe.csv"))
  })
  
  col <- reactive({
    input$variables
  })
  # Colonnes du tableau statistique
  tabStats <- reactive({
    dataset <- datasetInput()
    #print(dataset[col()])
    # Calculer les effectifs et les effectifs cumulés
    table.tmp <- as.data.frame(table(dataset[col()]))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les fréquences et les fréquences cumulés
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(table.tmp)*100,
                       table.tmp[[3]]/nrow(table.tmp)*100)
    colnames(table.tmp) <- c("Entry", "Effectifs", "Effectifs Cum.","frequence","frenquence cumules")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  tabStatsTime <- reactive({
    dataset <- datasetInput()
    #print(dataset[col()])
    # Calculer les effectifs et les effectifs cumulés
    table.tmp <- as.data.frame(table(dataset$time))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les fréquences et les fréquences cumulés
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(table.tmp)*100,
                       table.tmp[[3]]/nrow(table.tmp)*100)
    colnames(table.tmp) <- c("Entry", "Effectifs", "Effectifs Cum.","frequence","frenquence cumules")
    # Renvoyer le tableau statistique
    table.tmp
  })

  
  # Commande pour le chargement de données dans 'output'
  output$Tableau <- renderTable({tabStats()})
  
  

  

  
  #Affichage du dataset spiral
  output$view_spr = output$view <- DT::renderDataTable({
    datasetInput()
  })
  
  
  pretraitement <- reactive({
    dataset = datasetInput()
    
    sum(is.na(dataset))
    dataset$Education = factor(dataset$Education)
    dataset$EnvironmentSatisfaction = factor(dataset$EnvironmentSatisfaction)
    dataset$JobInvolvement = factor(dataset$JobInvolvement)
    dataset$JobSatisfaction = factor(dataset$JobSatisfaction)
    dataset$PerformanceRating = factor(dataset$PerformanceRating)
    dataset$EnvironmentSatisfaction = factor(dataset$EnvironmentSatisfaction)
    dataset$WorkLifeBalance = factor(dataset$WorkLifeBalance)
    
    dataset$Attrition = factor(dataset$Attrition)
    dataset$BusinessTravel = factor(dataset$BusinessTravel)
    dataset$Department = factor(dataset$Department)
    dataset$EducationField = factor(dataset$EducationField)
    dataset$Gender = factor(dataset$Gender)
    dataset$JobRole = factor(dataset$JobRole)
    dataset$JobLevel = factor(dataset$JobLevel)
    dataset=na.omit(dataset)
    dataset$EmployeeNumber=dataset$Over18=dataset$EmployeeCount=dataset$StandardHours = NULL
    dataset$MaritalStatus=dataset$MonthlyIncome=dataset$PerformanceRating= NULL
    dataset
  })
  
  train_Lr <- reactive({
    set.seed(3000)
    dataset = pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    if (input$option2 == TRUE){
      downSample(train, train$Attrition)
    }
    if (input$option3 == TRUE){
      
      upSample(train, train$Attrition)
      
    }
    
    attLog=glm(Attrition~.,data=train,family = binomial)
    
  })
  
  output$Acc_LR <- renderText({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    accuracy <- table(test$Attrition,predGlm>.5)
    sum(diag(accuracy))/sum(accuracy)*100
    
  })
  output$f_score_LR <- renderText({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    pred <- ifelse(predGlm < 0.5, 1, 2)
    
    F1_Score(as.numeric(as.factor(test$Attrition)), pred)*100
  })
  output$classification_lr <- renderPlot({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    #fg <- predGlm>.5
    #bg <- predGlm<.5
    #pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    #plot(pr)
    x = prediction(predGlm,test$Attrition)
    perf <- performance(x,"prec","rec")
    
    plot(perf)
    
    #pred_grid <-glm(as.factor(Attrition)~g[,1:2],data=train,family = binomial)
    
    #plot(g$JobInvolvement,g$JobSatisfaction, col = pred_grid, xlab="V1", ylab="V2", pch=19)
    #points(test$JobInvolvement,test$JobSatisfaction,col=as.numeric(knn.pred) + 6,pch=19)
  })
  
  
  train_rf<- reactive({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    if (input$option2 == TRUE){
      downSample(train, as.factor(train$Attrition))
    }
    if (input$option3 == TRUE){
      
      upSample(train, as.factor(train$Attrition))
      
    }
    randomForestModel=randomForest(as.factor(Attrition)~.,data=train,ntree=100,nodesize=12)
    
  })  
  output$Acc_RF <- renderText({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test)
    table(test$Attrition,predictRF)
    accuracy <- table(as.factor(test$Attrition),predictRF)
    sum(diag(accuracy))/sum(accuracy)*100
    
  })
  output$f_score_RF <- renderText({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test)
    F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predictRF))*100
  })
  output$Classification_rf <- renderPlot({
    dataset=pretraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    #randomForestModel=train_rf()  
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test,type="response")
    perf <- performance(prediction(as.numeric(predictRF), test$Attrition),"prec","rec")
    plot(perf)
    table(test$Attrition,predictRF)
    accuracy <- table(as.factor(test$Attrition),predictRF)
    print(sum(diag(accuracy))/sum(accuracy))
  })
}




# Associaion entre interface et serveur
shinyApp(ui = ui, server = server)

