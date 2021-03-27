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
library(plyr)
library(corrplot)
library(PRROC)
library(rpart)
library(rpart.plot)

# UI 
ui<-(fluidPage(
  
  # Titre
  titlePanel("Projet Shiny : Employee Attrition"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3("Donnees"),
      selectInput("dataset", "Selectionnez le dataset:", 
                  choices = c("Employee Attrition")),
      h3("Choisir la portion d'entrainement"),
      sliderInput("trainsplit", "Portion training:",
                  min = 0.05, max = 0.95,
                  value = 0.7, step = 0.05
      ),
      h3("Choisir les parametres d'entrainement"),
      radioButtons("radio",
                   label="Selectionnez",
                   choices = list("Over Sampling " = 1, "Under Sampling " = 2,"Sans Sampling" = 3),
                   selected = 1,
                   inline = T,
                   width = "100%"),         
      h2("Realisé par les Etudiants : "),
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
                               tabPanel("Analyse Univariee", h1("Analyse univariee"),
                                        h3("Variables Quantitatives"),
                                        selectInput(
                                          "var_quant_univ", "Veuillez selectionner la variable de votre choix",
                                          c('Age','DistanceFromHome','MonthlyIncome','NumCompaniesWorked',
                                            'PercentSalaryHike','StockOptionLevel','TotalWorkingYears',
                                            'TrainingTimesLastYear','YearsAtCompany','YearsSinceLastPromotion',
                                            'YearsWithCurrManager')
                                        ),
                                        checkboxInput("univ_mode_densite", "Densité", value = FALSE, width = NULL),
                                        fluidRow(column(5,plotOutput("univ_hist")),column(5,plotOutput("univ_boxplot"))),
                                        fluidRow(column(5,plotOutput("univ_normplot")),column(5,plotOutput("univ_22"))),
                                        
                                        h3("Variables Qualitatives"),
                                        selectInput(
                                          "var_qual_univ", "Veuillez selectionner la variable de votre choix",
                                          c("JobInvolvement", "PerformanceRating", "EnvironmentSatisfaction", 
                                            "JobSatisfaction" ,"WorkLifeBalance", "Attrition", "BusinessTravel", 
                                            "Department", "Education", "EducationField", "Gender", "JobLevel", 
                                            "JobRole","MaritalStatus")
                                        ),
                                        checkboxInput("univ_mode_pourcent", "Pourcentage", value = FALSE, width = NULL),
                                        
                                        fluidRow(column(5,plotOutput("univ_bar")),column(5,plotOutput("univ_pie"))),
                                        
                               ),
                               tabPanel("Analyse Bivariee", h1("Analyse Bivariee"),
                                        h3("Analyse Générale"),
                                        fluidRow(column(10,plotOutput("biv_corrplot"))),
                                        
                                        h3("Analyses Cyblées"),
                                        fluidRow(column(5,
                                                    selectInput(
                                                      "biv_v1", "Veuillez selectionner la variable de votre choix",
                                                      selected="Age",
                                                      list(
                                                        `Quantitative`=c('Age','DistanceFromHome','MonthlyIncome','NumCompaniesWorked',
                                                                         'PercentSalaryHike','StockOptionLevel','TotalWorkingYears',
                                                                         'TrainingTimesLastYear','YearsAtCompany','YearsSinceLastPromotion',
                                                                         'YearsWithCurrManager')
                                                        ,
                                                        `Qualitative`= c("JobInvolvement", "PerformanceRating", "EnvironmentSatisfaction", 
                                                                         "JobSatisfaction" ,"WorkLifeBalance", "Attrition", "BusinessTravel", 
                                                                         "Department", "Education", "EducationField", "Gender", "JobLevel", 
                                                                         "JobRole","MaritalStatus")
                                                      )
                                                    ) 
                                                 ),
                                                 column(5,
                                                    selectInput(
                                                      "biv_v2", "Veuillez selectionner la variable de votre choix",
                                                      selected="Attrition",
                                                      list(
                                                        `Quantitative`=c('Age','DistanceFromHome','MonthlyIncome','NumCompaniesWorked',
                                                                         'PercentSalaryHike','StockOptionLevel','TotalWorkingYears',
                                                                         'TrainingTimesLastYear','YearsAtCompany','YearsSinceLastPromotion',
                                                                         'YearsWithCurrManager')
                                                        ,
                                                        `Qualitative`= c("JobInvolvement", "PerformanceRating", "EnvironmentSatisfaction", 
                                                                         "JobSatisfaction" ,"WorkLifeBalance", "Attrition", "BusinessTravel", 
                                                                         "Department", "Education", "EducationField", "Gender", "JobLevel", 
                                                                         "JobRole","MaritalStatus")
                                                      )
                                                    )              
                                                 )
                                        ),
                                        fluidRow(column(5,plotOutput("biv_d1")),column(5,plotOutput("biv_d2"))),
                                        fluidRow(column(5,plotOutput("biv_d3")),column(5,plotOutput("biv_d4")))
                                        

                               )
                   )),
          #Panel de l'exploration des variables
          tabPanel("Exploration des variables",
                   tabsetPanel(tabPanel("Analyse d'Attrition", h1("Analyse d'Attrition"),
                                  h3("Count"),
                                  fluidRow(column(5,plotOutput("attr_bar")),column(5,plotOutput("attr_pie"))),
                                  h3("Corrélation avec Attrition"),
                                  fluidRow(column(5,tableOutput("attr_corr")),column(5,tableOutput("attr_corr_quanti"))),
                                  fluidRow(column(5,plotOutput("attr_graphe_best_quali")),column(5,plotOutput("attr_graphe_best_quanti"))),
                                  h3("Toutes les analyses bivariées avec la variable Attrition sont possible dans l'onglet : Présentation des Données -> Analyse Bivariée")
                                  
                                  
                               ),
                               
                               tabPanel("Analyse Variable D'Intérêt Age", h1("Analyse Variable D'Intérêt Age"),
                                  h3("Attrition ou non par Tranche d'âge"),
                                  fluidRow(column(5,plotOutput("age_bar")),column(5,verbatimTextOutput("age_agger"))),
                                  h3("Moyennes par tranche d'âge"),
                                  fluidRow(column(10,DT::dataTableOutput("age_aggr"))),

                                        
                               ),
                               
                               tabPanel("Analyse Variable D'Intérêt YearsAtCompany ", h1("Analyse Variable D'Intérêt YearsAtCompany"),
                                  h3("Attrition ou non par Tranche d'ancienneté"),
                                  fluidRow(column(10,plotOutput("comp_bar"))),
                                  h3("Moyennes par tranche d'ancienneté"),
                                  fluidRow(column(10,DT::dataTableOutput("comp_aggr"))),
                                        
                               )
                   )),
          #Panel de classification
          tabPanel("Classification",
                   tabsetPanel(
                     tabPanel("Random Forest", h1("Random Forest"),fluidRow(column(2, h4("Precison testing: ")), column(2, textOutput("Acc_RF"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_RF"))),plotOutput("Classification_rf")),
                     tabPanel("Logistic Regression", h1("Logistic Regression"), fluidRow(column(2, h4("Precison testing: ")), column(2, textOutput("Acc_LR"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_LR"))),plotOutput("classification_lr")),
                     tabPanel("Decision Tree", h1("Decision Tree"),fluidRow(column(2, h4("Precison testing: ")), column(2, textOutput("Acc_DT"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_DT"))),fluidRow(column(4, h4("Arbre de décision :")), column(12, plotOutput("DT_tree"))), h4("Courbe PR:"),plotOutput("Classification_DT"))
                   )),
          tabPanel("Application de Surveillance de l'Attrition", h1("Probabilité d'attrition des employés :"),DT::dataTableOutput("Application"))
          #♣tabPanel("Aide", h1("Aide"),verbatimTextOutput("Help"))
          
        ) 
      ),
    )
  )
)
)


server <- function(input, output){
  
  #Récupération du Dataset selectionné
  datasetInput <- reactive({
    read_csv("Employe.csv")
  })
  
  col <- reactive({
    input$variables
  })
  
  #Affichage du dataset spiral
  output$view_spr = output$view <- DT::renderDataTable({
    datasetInput()
  })
  
  pretraitement <- reactive({
    empattr = datasetInput()
    
    empattr$Education = factor(empattr$Education)
    empattr$Education = revalue(empattr$Education, c("1"="Below College", "2"="College", "3"="Bachelor", "4"="Master", "5"="Doctor"))
    
    empattr$EnvironmentSatisfaction = factor(empattr$EnvironmentSatisfaction)
    empattr$EnvironmentSatisfaction = revalue(empattr$EnvironmentSatisfaction, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
    
    empattr$JobInvolvement = factor(empattr$JobInvolvement)
    empattr$JobInvolvement = revalue(empattr$JobInvolvement, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
    
    empattr$JobSatisfaction = factor(empattr$JobSatisfaction)
    empattr$JobSatisfaction = revalue(empattr$JobSatisfaction, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
    
    empattr$PerformanceRating = factor(empattr$PerformanceRating)
    
    empattr$JobLevel = factor(empattr$EnvironmentSatisfaction)
    
    empattr$WorkLifeBalance = factor(empattr$WorkLifeBalance)
    empattr$WorkLifeBalance = revalue(empattr$WorkLifeBalance, c("1"="Bad", "2"="Good", "3"="Better", "4"="Best"))
    
    empattr$Attrition = factor(empattr$Attrition)
    empattr$Attrition = revalue(empattr$Attrition, c("No"="No Leave", "Yes"="Leave"))
    
    empattr$BusinessTravel = factor(empattr$BusinessTravel)
    
    empattr$Department = factor(empattr$Department)
    
    empattr$EducationField = factor(empattr$EducationField)
    
    empattr$Gender = factor(empattr$Gender)
    
    empattr$JobRole = factor(empattr$JobRole)
    
    empattr$JobLevel = factor(empattr$JobLevel)
    
    empattr$MaritalStatus = factor(empattr$MaritalStatus)
    
    empattr$Over18 = factor(empattr$Over18)
    
    empattr = empattr[,which(!(names(empattr) %in% c("EmployeeID", "EmployeeCount", "StandardHours", "Over18")))]
    empattr = na.omit(empattr)
    
    empattr
  })
  

  train_Lr <- reactive({
    set.seed(3000)
    dataset = one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    if (input$radio == 2){
      downSample(train, train$Attrition)
    }
    if (input$radio == 1){
      
      upSample(train, train$Attrition)
      
    }
    
    attLog=glm(Attrition~.,data=train,family = binomial)
    
  })
  
  output$Acc_LR <- renderText({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    accuracy <- table(test$Attrition,predGlm>.5)
    sum(diag(accuracy))/sum(accuracy)*100
    
  })
  output$f_score_LR <- renderText({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    pred <- ifelse(predGlm < 0.5, 1, 2)
    
    F1_Score(as.numeric(as.factor(test$Attrition)), pred)*100
  })
  output$classification_lr <- renderPlot({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    
    scores <- data.frame(predGlm,test$Attrition)
    pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predGlm,
                   scores.class1=scores[scores$test.Attrition=="No Leave",]$predGlm,
                   curve=T)
    
    plot(pr)
  })
  
  
  train_rf<- reactive({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    if (input$radio == 2){
      downSample(train, train$Attrition)
    }
    if (input$radio == 1){
      
      upSample(train,train$Attrition)
      
    }
    randomForestModel=randomForest(as.factor(Attrition)~.,data=train,ntree=100,nodesize=12)
    
  })  
  output$Acc_RF <- renderText({
    dataset=one_hot()
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
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test)
    F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predictRF))*100
  })
  output$Classification_rf <- renderPlot({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    #randomForestModel=train_rf()  
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test,type="response")
    
    scores <- data.frame(predictRF,test$Attrition)
    pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predictRF,
                   scores.class1=scores[scores$test.Attrition=="No Leave",]$predictRF,
                   curve=T)
    
    
    plot(pr)
    
    
  })
  
  
  
  
  
  
  train_DT<- reactive({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    if (input$radio == 2){
      downSample(train, train$Attrition)
    }
    if (input$radio == 1){
      
      upSample(train,train$Attrition)
      
    }
    decisionTreeModel= rpart(Attrition~.,data=train,method="class")
  })  
  output$Acc_DT <- renderText({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    decisionTreeModel=train_DT()
    predDT=predict(decisionTreeModel,newdata = test,type = "class")
    table(test$Attrition,predDT)
    accuracy <- table(as.factor(test$Attrition),predDT)
    sum(diag(accuracy))/sum(accuracy)*100
    
  })
  output$f_score_DT <- renderText({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    decisionTreeModel=train_DT()
    predDT=predict(decisionTreeModel,newdata = test,type = "class")
    F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predDT))*100
  })
  output$Classification_DT <- renderPlot({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    
    decisionTreeModel=train_DT()
    predDT=predict(decisionTreeModel,newdata = test,type = "class")
    
    scores <- data.frame(predDT,test$Attrition)
    pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predDT,
                   scores.class1=scores[scores$test.Attrition=="No Leave",]$predDT,
                   curve=T)
    
    
    plot(pr)
    
    
  })
  
  
  
  one_hot <- reactive({
    dataset<-pretraitement()
    
    dataset$EmployeeID=NULL
    dmy <- dummyVars(~., data = dataset[-7])
    data_one_cod <- data.frame(predict(dmy, newdata = dataset))
    data_one_cod$Attrition=dataset$Attrition
    
    data_one_cod
    
    
  })
  
  option <- reactive({
    dataset<-pretraitement()
    
    dmy <- dummyVars(~., data = dataset[-7])
    data_one_cod <- data.frame(predict(dmy, newdata = dataset))
    data_one_cod$Attrition=dataset$Attrition
    
    data_one_cod
  })
  
  output$ DT_tree <- renderPlot({
    dataset=one_hot()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    
    decisionTreeModel=train_DT()
    
    rpart.plot(decisionTreeModel)
    
  })
  
  output$Application  = output$Application <- DT::renderDataTable({
    
    old_dataset=pretraitement()
    split=sample.split(old_dataset$Attrition,SplitRatio = 0.5)
    train2=subset(old_dataset,split==T)
    test1=subset(old_dataset,split==F)
    x=test1$EmployeeID
    
    print(x)
    dataset=option()
    split=sample.split(dataset$Attrition,SplitRatio = 0.5)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    
    
    train$EmployeeID=NULL
    test$EmployeeID=NULL
    
    #randomForestModel=train_rf()  
    attLog=train_Lr()
    predictLR=predict(attLog,newdata=test,type="response")
    
    
    dataset[,1:65]=NULL
    dataset$EmployeeID=x
    dataset$Department=test1$Department
    dataset$Gender=test1$Gender
    dataset$JobRole=test1$JobRole
    dataset$WorkLifeBalance=test1$WorkLifeBalance
    dataset$Probability_Leave=round(predictLR*100, digits = 0)
    
    dataset
    
  })


############################### FONCTIONS GENERALES
  get_quali <- reactive({
    dataset = pretraitement()
    is.fact <- sapply(dataset, is.factor)
    quali = dataset[,is.fact]
    as.data.frame(quali)
  })
  
  get_quanti <- reactive({
    dataset = pretraitement()
    is.fact <- sapply(dataset, is.factor)
    quanti = dataset[, !is.fact]
    quanti$Attrition = dataset$Attrition
    as.data.frame(quanti)
    
  })
  
  #############################################
  #############################
  #############################
  ######## ONGLET 1 : PRESENTATION DES DONNEES
  #############################################

############################### ANALYSE UNIVARIEE
  ################## Fonctions
  histoUniv = function(df, var, density=FALSE){
    # Changer la couleur de remplissage par groupe
    if (density){
      p = ggplot(data = df, aes(x=df[,var], y=..density..)) +
        geom_histogram(position="identity", fill=4) +
        geom_density(alpha=.2) +
        labs(x=var, y="Densité")
    }else{
      p = ggplot(data = df, aes(x=df[,var])) +
        geom_histogram(position="identity", fill=4) +
        labs(x=var, y="Count")
    }
    
    # Ajouter les moyennes
    p+geom_vline(xintercept = mean(df[,var]), linetype="dashed")+
      ggtitle(paste("Histogramme de ", var, sep=""))
  }

  
  barPlotUniv = function(df, var, pourcent=FALSE){
    if (pourcent){
      res = ggplot(data=df, aes(x=df[,var], y=..count../sum(..count..), fill=4)) +
        labs(y="Pourcentage", x = var)
      
    }else{
      res = ggplot(data=df, aes(x=df[,var], fill=4)) +
        labs(y="Count", x = var)
    }
    
    
    res + geom_bar(stat="count") +
        ggtitle(paste("Barplot de ", var, sep="")) + 
        guides(fill = FALSE) 

  }
  
  boxPlotUniv = function(df, var){
    qplot(y=df[,var], 
          xlab = var,
          ylab = var,
          geom=c("boxplot")) +
      theme(legend.title=element_blank())
  }
  
  normalityPlot = function(df, var){
    qqnorm(df[,var], pch = 1, frame = FALSE)
    qqline(df[,var], col = "steelblue", lwd = 2)
  }
  
  piePlotUniv = function(df, var){
    cc = table(df[,var])
    pie(cc, main = paste("Piechart de la variable ", var, sep=""))
  }
  
  ######## Quantitatives
  output$univ_hist <- renderPlot({
    histoUniv(get_quanti(), input$var_quant_univ, input$univ_mode_densite)
  })
  
  output$univ_boxplot <- renderPlot({
    boxPlotUniv(get_quanti(), input$var_quant_univ)
  })
  
  output$univ_normplot <- renderPlot({
    normalityPlot(get_quanti(), input$var_quant_univ)
  })

  ####### Qualitatives
  output$univ_bar <- renderPlot({
    barPlotUniv(get_quali(), input$var_qual_univ, input$univ_mode_pourcent)
  })
  
  
  output$univ_pie <- renderPlot({
    piePlotUniv(get_quali(), input$var_qual_univ)
  })
############################## ANALYSE BIVARIEE
  boxPlotByQuanti = function(df, col1, col2){
    qplot(x=df[,col2], y = df[,col1], 
          xlab = col2, ylab = col1,
          geom=c("boxplot"), fill=df[,col2]) +
      theme(legend.title=element_blank())
  }
  
  scatterPlot = function(df, col1, col2, coloration){
    ggplot(df, aes(x=df[,col1], y=df[,col2], shape=df[,coloration], color=df[,coloration])) +
      geom_point()+
      labs(x= col1, y=col2, shape=coloration, color=coloration)
  }
  
  histoFctQuali = function(df, vquant, vquali, density=FALSE){
    if (density){
      p = ggplot(data = df, aes(x=df[,vquant], y=..density.., fill=df[,vquali])) +
        geom_histogram(position="identity", alpha=0.3) +
        geom_density(alpha=.01) +
        labs(x=vquant, y="Densité", fill=vquali)
    }else{
      p = ggplot(data = df, aes(x=df[,vquant],fill=df[,vquali])) +
        geom_histogram(position="identity", alpha=0.3) +
        labs(x=vquant, y="Count", fill=vquali)
    }
    
    # Ajouter les moyennes
    p+geom_vline(xintercept = mean(df[,vquant]), linetype="dashed")+
      ggtitle(paste("Histogramme de ", vquant, " en fonction de ", vquali, sep=""))
  }

  barPlotFctQuali = function(df, varqualiPrinc, varqualiSec, stack=TRUE, percent=FALSE){
    # Barplots empilés avec plusieurs groupes
    if (stack){
      if(percent){
        res = ggplot(data=df, aes(x=df[,varqualiPrinc], fill=df[,varqualiSec])) +
          geom_bar(stat="count", position=position_fill(), alpha=0.8)
      }else{
        res = ggplot(data=df, aes(x=df[,varqualiPrinc], fill=df[,varqualiSec])) +
          geom_bar(stat="count", alpha=0.8)
      }
    }
    else{
      res = ggplot(data=df, aes(x=df[,varqualiPrinc], fill=df[,varqualiSec])) +
        geom_bar(stat="count", position=position_dodge(), alpha = 0.8)
    }
    res = res+
      ggtitle(paste(varqualiPrinc, " selon ", varqualiSec, sep=""))
    if (percent){
      res = res + labs(y="Pourcentage", x = varqualiPrinc, fill=varqualiSec)
    }else{
      res = res + labs(y="Count", x = varqualiPrinc, fill=varqualiSec)
    }
    res
  }
  
  boxPlotByQuanti = function(df, col1, col2){
    qplot(x=df[,col2], y = df[,col1], 
          xlab = col2, ylab = col1,
          geom=c("boxplot"), fill=df[,col2]) +
      theme(legend.title=element_blank())
  }
  
  output$biv_corrplot <- renderPlot({
    cor_m = cor(get_quanti()[,-12])
    corrplot(cor_m, method="color", title = "Matrice des corrélations", order="hclust")
  })
  
  output$biv_d1 <- renderPlot({
    dataset = data.frame(pretraitement())
    V1_QUALI = is.factor(dataset[,input$biv_v1])
    V2_QUALI = is.factor(dataset[,input$biv_v2])
    res = NULL
    
    if (V1_QUALI & V2_QUALI){
      res = barPlotFctQuali(dataset, input$biv_v1, input$biv_v2, stack=FALSE)
    }else if (!V1_QUALI & V2_QUALI){
      res = histoFctQuali(dataset, input$biv_v1, input$biv_v2)
    }else if (!V2_QUALI & V1_QUALI){
      res = histoFctQuali(dataset, input$biv_v2, input$biv_v1)

    }else{
      res = scatterPlot(dataset, input$biv_v1, input$biv_v2, coloration="Attrition")
    }
    
    res
  })
  
  output$biv_d2 <- renderPlot({
    dataset = data.frame(pretraitement())
    V1_QUALI = is.factor(dataset[,input$biv_v1])
    V2_QUALI = is.factor(dataset[,input$biv_v2])
    res = NULL
    
    if (V1_QUALI & V2_QUALI){
      res = barPlotFctQuali(dataset, input$biv_v1, input$biv_v2, stack=TRUE)
    }else if (!V1_QUALI & V2_QUALI){
      res = histoFctQuali(dataset, input$biv_v1, input$biv_v2, density=TRUE)
    }
    res
  })
  
  output$biv_d3 <- renderPlot({
    dataset = data.frame(pretraitement())
    V1_QUALI = is.factor(dataset[,input$biv_v1])
    V2_QUALI = is.factor(dataset[,input$biv_v2])
    res = NULL
    if (!V1_QUALI & V2_QUALI){
      res = boxPlotByQuanti(dataset, input$biv_v1, input$biv_v2)
    }else if (V1_QUALI & !V2_QUALI){
      res = boxPlotByQuanti(dataset, input$biv_v2, input$biv_v1)
    }else if (V1_QUALI & V2_QUALI){
      res = barPlotFctQuali(dataset, input$biv_v1, input$biv_v2, stack=TRUE, percent=TRUE)
    }
    res
  })


  #############################################
  #############################
  #############################
  ######## ONGLET 2 : EXPLORATION DES DONNEES
  #############################################
  getCorrQualit = function(v1,v2){
    force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
    rownames(force.df) = c("X2", "Phi2", "Cramer")
    
    # La table de contingence des profils observés
    tab = table(v1, v2)
    # La table de contigence s'il y a indépendence
    tab.indep = tab
    n = sum(tab)
    tab.rowSum = apply(tab, 2, sum)
    tab.colSum = apply(tab, 1, sum)
    
    for(i in c(1:length(tab.colSum))){
      for(j in c(1:length(tab.rowSum))){
        tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
      }
    }
    
    # Calcul du X²
    force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
    # Calcul du Phi²
    force.df[2,1] = force.df[1,1]/n
    # Calcul du Cramer
    force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
    
    force.df
  }
  
  getLeave<-reactive({
    empattr = data.frame(pretraitement())
    X<-split(empattr, empattr$Attrition)
    Leave = X$Leave
    Leave
  })
  getNoLeave <- reactive({
    empattr = data.frame(pretraitement())
    X<-split(empattr, empattr$Attrition)
    NoLeave = X$"No Leave"
    NoLeave
  })
  
  getCorrAttrition = function(var){
    # Calculer la variance intra classe
    l = getLeave()[,var]
    nl = getNoLeave()[,var]
    num_l = length(l)
    num_nl = length(nl)
    moy_generale = mean(data.frame(pretraitement())[,var])
    moy_l = mean(l)
    moy_nl = mean(nl)
    
    VAR_INTERCLASSE = (num_l * (moy_l-moy_generale)^2 + num_nl * (moy_nl - moy_generale)^2)/ (num_l+num_nl)
    VAR_INTRACLASSE = (num_l*sd(l)^2 + num_nl*sd(nl)^2)/(num_l+num_nl)
    
    sqrt(VAR_INTERCLASSE/VAR_INTRACLASSE)
  }
  output$attr_bar <- renderPlot({
    barPlotUniv(data.frame(pretraitement()), "Attrition")
  })
  
  output$attr_pie <- renderPlot({
    piePlotUniv(pretraitement(), "Attrition")
  })
  
  
  output$attr_corr <- renderTable({
    i=0
    for (col in names(get_quali())){
      if (i==0){
        res = data.frame(t(getCorrQualit(get_quali()$Attrition, get_quali()[,col])))
      }else{
        res = rbind(res, t(getCorrQualit(get_quali()$Attrition, get_quali()[,col])))
      }
      i=i+1
    }
    rownames(res) <- names(get_quali())
    res
  }, rownames=TRUE, digits=4)
  
  output$attr_corr_quanti <- renderTable({
    i=1
    res <- as.data.frame(matrix(NA, nrow = 1, ncol = length(names(get_quanti()))-1))
    
    quanti_pur = get_quanti()[-12]
    
    for (col in names(quanti_pur)){
      res[1,i] = getCorrAttrition(col)
      i=i+1
    }
  
    res = t(res)
    rownames(res) = names(quanti_pur)
    colnames(res) = "BETWEEN/WITHIN"
    res = res[order(-res[,"BETWEEN/WITHIN"]),,drop=F]
    res
  }, rownames=TRUE, digits=5)
  
  output$attr_graphe_best_quali <- renderPlot({
    i=0
    for (col in names(get_quali())){
      if (i==0){
        res = data.frame(t(getCorrQualit(get_quali()$Attrition, get_quali()[,col])))
      }else{
        res = rbind(res, t(getCorrQualit(get_quali()$Attrition, get_quali()[,col])))
      }
      i=i+1
    }
    rownames(res) <- names(get_quali())
    res = res[-6,]
    
    p = ggplot(data=NULL, aes(x= reorder(rownames(res), -res[,3]), y=res[,3], fill=4)) +
      labs(y="Cramér (Correlation avec Attrition)")
    
    
    
    p + geom_bar(stat="identity") +
      ggtitle("Barplot des variables qualitatives les plus corrélées avec Attrition") + 
      guides(fill = FALSE)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      xlab("Variables Qualitatives")
    
  })
  
  output$attr_graphe_best_quanti <- renderPlot({
    i=1
    res <- as.data.frame(matrix(NA, nrow = 1, ncol = length(names(get_quanti()))-1))
    
    quanti_pur = get_quanti()[-12]
    
    for (col in names(quanti_pur)){
      res[1,i] = getCorrAttrition(col)
      i=i+1
    }
    
    res = data.frame(t(res))
    rownames(res) = names(quanti_pur)
    colnames(res) = "BETWEEN/WITHIN"
    ord = order(-res[,"BETWEEN/WITHIN"])
    res = data.frame(res[ord,])
    ee = names(quanti_pur)[ord]

    p = ggplot(data=NULL, aes(x= reorder(ee, -res[,1]), y=res[,1], fill=4)) +
      labs(y="BETWEEN/WITHIN (Correlation avec Attrition)")
  
    p + geom_bar(stat="identity") +
      ggtitle("Barplot des variables quantitatives les plus corrélées avec Attrition") + 
      guides(fill = FALSE)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      xlab("Variables Quantitatives")
  })
  
  ################################## ANALYSE PAR RAPPORT A VARIABLE D INTERET
  discAge <- reactive({
    d = data.frame(pretraitement())
    vv <- ifelse(d$Age<20, "1/Age<20", 
          ifelse(d$Age >=20 & d$Age<30, "2/Age20-29",
          ifelse(d$Age >=30 & d$Age<40, "3/Age30-39",
          ifelse(d$Age >=40 & d$Age<50, "4/Age40-49",
          ifelse(d$Age >=50 & d$Age<60, "5/Age50-60", "6/Age>60")))))
    d$AgeClass = as.factor(vv)
    d
  })
  
  output$age_bar <- renderPlot({
    dd = discAge()
    
    res = ggplot(data=dd, aes(x=dd[,"AgeClass"], fill=dd[,"Attrition"])) +
      geom_bar(stat="count", position=position_fill(), alpha=0.8)
    
    res = res+ ggtitle("AgeClass selon Attrition")
      res = res + labs(y="Pourcentage", x = "AgeClass", fill="Attrition")
    res
  })
  
  output$age_aggr <- DT::renderDataTable({
    dd = discAge()
    agg = aggregate(. ~ AgeClass + Attrition, dd, mean)
    agg = agg[,c(1,2,3,4,5,6, 24, 26)]
    for(i in seq(3,8))
      agg[,i] = round(agg[,i], digits=2)

    agg
    
  }, options = list(pageLength = 15))
  
  
  discYearsAtCompany <- reactive({
    d = data.frame(pretraitement())
    vv <- ifelse(d$YearsAtCompany<5, "1/Ancienneté<5", 
          ifelse(d$YearsAtCompany >=5 & d$YearsAtCompany<10, "2/Ancienneté5-10",
          ifelse(d$YearsAtCompany >=10 & d$YearsAtCompany<20, "3/Ancienneté10-19",
          ifelse(d$YearsAtCompany >=20 & d$YearsAtCompany<30, "4/Ancienneté20-29",
          ifelse(d$YearsAtCompany >=30 & d$YearsAtCompany<40, "5/Ancienneté30-39", "6/Ancienneté>40")))))
    d$AnciennClass = as.factor(vv)
    d
  })
  output$comp_bar <- renderPlot({
    dd = discYearsAtCompany()
    
    res = ggplot(data=dd, aes(x=dd[,"AnciennClass"], fill=dd[,"Attrition"])) +
      geom_bar(stat="count", position=position_fill(), alpha=0.8)
    
    res = res+ ggtitle("AnciennClass selon Attrition")
    res = res + labs(y="Pourcentage", x = "AnciennClass", fill="Attrition")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      
    res
  })
  
  output$comp_aggr <- DT::renderDataTable({
    dd = discYearsAtCompany()
    agg = aggregate(. ~ AnciennClass + Attrition, dd, mean)
    agg = agg[,c(1,2,3,4,5,6, 24, 26)]
    for(i in seq(3,8))
      agg[,i] = round(agg[,i], digits=2)
    
    agg
    
  }, options = list(pageLength = 15))
  
}


# Associaion entre interface et serveur
shinyApp(ui = ui, server = server)

