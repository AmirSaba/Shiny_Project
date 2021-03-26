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
                                        fluidRow(column(5,plotOutput("biv_corrplot")),column(5,plotOutput("biv"))),
                                        
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
                   tabsetPanel(tabPanel("Table", h1("Table"),DT::dataTableOutput("n")),
                               tabPanel("Analyse Univariee", h1("Analyse univariee"),
                                        

                               ),
                               
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

############################### GENERAL FUNCTIONS
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

  barPlotFctQuali = function(df, varqualiPrinc, varqualiSec, stack=TRUE){
    # Barplots empilés avec plusieurs groupes
    if (stack){
      res = ggplot(data=df, aes(x=df[,varqualiPrinc], fill=df[,varqualiSec])) +
        geom_bar(stat="count", alpha=0.8)
    }
    else{
      res = ggplot(data=df, aes(x=df[,varqualiPrinc], fill=df[,varqualiSec])) +
        geom_bar(stat="count", position=position_dodge(), alpha = 0.8)
    }
    res = res+
      ggtitle(paste(varqualiPrinc, " selon ", varqualiSec, sep="")) + 
      labs(y="Count", x = varqualiPrinc, fill=varqualiSec)
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
    }
    res
  })


  #############################################
  #############################
  #############################
  ######## ONGLET 2 : EXPLORATION DES DONNEES
  #############################################
  
}




# Associaion entre interface et serveur
shinyApp(ui = ui, server = server)

