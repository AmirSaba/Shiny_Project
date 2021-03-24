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
  titlePanel("TP Analyse Univariée (Travail en plus : Classement)"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3("Données"),
      selectInput("dataset", "Selectionnez le dataset:", 
                  choices = c("Channing House Data", "Flame Data", "Aggregation Data","Employee Attrition")),
      h3("Choisir la portion d'entrainement"),
      sliderInput("trainsplit", "Portion training:",
                  min = 0.05, max = 0.95,
                  value = 0.5, step = 0.05
      ),
      h3("Choisir les paramètres d'entrainement"),
      checkboxInput("option2", "Over Sampling", value = FALSE, width = NULL),
      checkboxInput("option3", " Under Sampling", value = FALSE, width = NULL),
      h2("Réalisée par les Etudiants : "),
      h3("SABA Amir"),
      h3("LADAYCIA Bouteyna"),
      h3("DEBBAGH Abdelkader Nadir")
    ),
    
    
    
    # MainPanel divisÃ© en TabPanel
    mainPanel(
      
      conditionalPanel(
        condition = "input.dataset == 'Employee Attrition'",
        
        tabsetPanel(
          #Panel de la présentation des données
          tabPanel("Présentation des données",
                   tabsetPanel(tabPanel("Table", h1("Table"),DT::dataTableOutput("view_spr")),
                               tabPanel("Analyse Univariée", h1("Analyse univariée"),verbatimTextOutput("summary")),
                               tabPanel("Analyse Bivariée", h1("Analyse Bivariée"),plotOutput("plotknn"))
                              )),
          #Panel de l'exploration des variables
          tabPanel("Exploration des variables",
                   tabsetPanel(tabPanel("Table", h1("Table"),DT::dataTableOutput("n")),
                               tabPanel("Analyse Univariée", h1("Analyse univariée"),verbatimTextOutput("r")),
                               tabPanel("Analyse Bivariée", h1("Analyse Bivariée"),plotOutput("o"))
                   )),
          #Panel de classification
          tabPanel("Classification",
                   tabsetPanel(
                               tabPanel("Random Forest", h1("Random Forest"),fluidRow(column(2, h4("Précison testing: ")), column(2, textOutput("Acc_RF"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_RF"))),plotOutput("Classification_rf")),
                               tabPanel("Logistic Regression", h1("Logistic Regression"), fluidRow(column(2, h4("Précison testing: ")), column(2, textOutput("Acc_LR"))),fluidRow(column(2, h4("F1_score :")), column(2, textOutput("f_score_LR"))),
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
  
  #RÃ©cupÃ©ration du Dataset selectionnÃ©
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Channing House Data" =channing,
           "Flame Data" = read.delim("flame.txt", header = FALSE),
           "Aggregation Data" = read.delim("Aggregation.txt", header = FALSE),
           "Employee Attrition" = read_csv("Employe.csv"))
  })
  
  col <- reactive({
    input$variables
  })
  # Colonnes du tableau statistique
  tabStats <- reactive({
    dataset <- datasetInput()
    #print(dataset[col()])
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(dataset[col()]))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(table.tmp)*100,
                       table.tmp[[3]]/nrow(table.tmp)*100)
    colnames(table.tmp) <- c("Entry", "Effectifs", "Effectifs Cum.","frequence","frenquence cumul?s")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  tabStatsTime <- reactive({
    dataset <- datasetInput()
    #print(dataset[col()])
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(dataset$time))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(table.tmp)*100,
                       table.tmp[[3]]/nrow(table.tmp)*100)
    colnames(table.tmp) <- c("Entry", "Effectifs", "Effectifs Cum.","frequence","frenquence cumul?s")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  tabStatsCens <- reactive({
    dataset <- datasetInput()
    #print(dataset[col()])
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(dataset$cens))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(table.tmp)*100,
                       table.tmp[[3]]/nrow(table.tmp)*100)
    colnames(table.tmp) <- c("Entry", "Effectifs", "Effectifs Cum.","frequence","frenquence cumul?s")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  # Commande pour le chargement de donnÃ©es dans 'output'
  output$Tableau <- renderTable({tabStats()})
  
  
  #Affichage du boxplot Agreggation
  output$boxplot_agg = output$boxplot <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)
    
  })
  
  #Affichage du boxplot spiral
  output$boxplot_spr = output$boxplot <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)
  })
  
  #Affichage du diagramme en baton pour la variable "cens"
  output$Diagramme2 <- renderPlot({
    dataset <- datasetInput()
    barplot(height = table(dataset["cens"]),names = c("0","1"), col="green4",xlab="Cens",ylab="Effectifs")
  })
  
  #Affichage du diagramme en baton pour la variable "entry"
  output$Diagramme <- renderPlot({
    dataset <- datasetInput()
    
    plot(table(channing["entry"]),cex.axis=1.5,cex.main=1.5,col="green4",xlab="entry",ylab="Effectifs")
  })
  
  #Affichage du diagramme en baton pour la variable "entry"
  output$Diagramme3 <- renderPlot({
    dataset <- datasetInput()
    
    plot(ecdf(as.numeric(tabStatsTime()[,5])), xlab="Time", ylab="FrÃ©quence cumulÃ©e", main="FrÃ©quences cumulÃ©es variable Time")
    
  })
  
  #Affichage du diagramme en baton pour la variable "exit"
  output$Diagramme4 <- renderPlot({
    dataset <- datasetInput()
    pie(tabStatsCens()[,2], labels=c("Left", "Died"), radius = 1, main = "Diagramme en Secteurs Cens")
    
  })
  
  
  #Affichage de l'histogramme  pour la variable "Time"
  output$histogramme <- renderPlot({
    dataset <-datasetInput()
    hist(dataset$time, probability=TRUE,cex.axis=1 ,cex.main=1 ,col="blue",xlab="Time",ylab="Freq", main="Histogramme de la variable Time")
    lines(density(dataset$time, bw=25), col="orange", lwd=3, lty=4)
    lines(density(dataset$time, bw=5), col="red", lwd=3, lty=4)
    lines(density(dataset$time, bw = 10), col="magenta", lwd=3, lty=4)
    lines(density(dataset$time), col="yellow", lwd=3, lty=4)
  })
  
  #Affichage de l'analyse univariÃ© Aggregation
  output$summary_agg = output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  #Affichage de l'analyse univariÃ© spiral
  output$summary_spr = output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  #Affichage du dataset Aggregation
  output$view_agg = output$view <- DT::renderDataTable({
    datasetInput()
  })
  
  #Affichage du dataset spiral
  output$view_spr = output$view <- DT::renderDataTable({
    datasetInput()
  })
  
  #################################### PARTIE AGGREGATION
  get_sample <- eventReactive(input$execute_train, {
    d = datasetInput()
    tr <- sample(1:nrow(d),input$trsplit*nrow(d))
    tr
  })
  training_set <- reactive({
    d = datasetInput()
    d$V3 = factor(d$V3)
    
    tr <- get_sample()
    d[tr,]
  })
  
  testing_set <- reactive({
    d = datasetInput()
    d$V3 = factor(d$V3)
    
    tr <- get_sample()
    d[-tr,]
  })
  
  
  trainer_nn <- eventReactive( input$execute_train, {
    d = datasetInput()
    d$V3 = factor(d$V3)
    
    dtrain = training_set()
    dtest = testing_set()
    
    df = dtrain[,1:3]
    lab = dtrain[,3]
    
    nn <- neuralnet(V3 ~ V1 + V2, data=dtrain, hidden=c(input$hidden_size1,input$hidden_size2), act.fct = "logistic",
                    linear.output = FALSE,
                    lifesign = "minimal")
    
    # Compute accuracy
    pr.nn <- compute(nn, dtest[,1:3])
    # Extract results
    pr.nn_ <- pr.nn$net.result
    
    # Accuracy (test set)
    original_values <- max.col(dtest[,3])
    pr.nn_2 <- max.col(pr.nn_)
    outs <- mean(pr.nn_2 == original_values)
    
    nn
  })
  
  output$train_accuracy <- renderText({
    nn = trainer_nn()
    dtrain = training_set()
    # Compute accuracy
    pr.nn <- compute(nn, dtrain[,1:3])
    # Extract results
    pr.nn_ <- pr.nn$net.result
    
    # Accuracy (test set)
    original_values <- max.col(dtrain[,3])
    pr.nn_2 <- max.col(pr.nn_)
    outs <- mean(pr.nn_2 == original_values)
    
    mean(pr.nn_2==dtrain[,3])*100
  })
  
  output$test_accuracy <- renderText({
    nn = trainer_nn()
    dtest = testing_set()
    # Compute accuracy
    pr.nn <- compute(nn, dtest[,1:3])
    # Extract results
    pr.nn_ <- pr.nn$net.result
    
    # Accuracy (test set)
    original_values <- max.col(dtest[,3])
    pr.nn_2 <- max.col(pr.nn_)
    outs <- mean(pr.nn_2 == original_values)
    
    mean(pr.nn_2==dtest[,3])*100
  })
  
  output$confusion_table <- renderPrint({
    nn = trainer_nn();
    dtest = testing_set();
    # Compute accuracy
    pr.nn <- compute(nn, dtest[,1:3]);
    # Extract results
    pr.nn_ <- pr.nn$net.result;
    
    # Accuracy (test set)
    original_values <- max.col(dtest[,3]);
    predicted <- max.col(pr.nn_);
    
    table(predicted, dtest[,3])
  })
  
  output$classification <- renderPlot({
    d = datasetInput()
    nn = trainer_nn()
    # to plot
    if (input$test_checkbox == TRUE & input$train_checkbox == TRUE){
      to_plot_features = d[,1:3]
      to_plot_y = d[,3]
    }else if (input$test_checkbox == TRUE){
      dtest = testing_set()
      to_plot_features = dtest[,1:3]
      to_plot_y = dtest[,3]
    }else if (input$train_checkbox == TRUE){
      dtrain = training_set()
      to_plot_features = dtrain[,1:3]
      to_plot_y = dtrain[,3]
    }else{
      dtrain = training_set()
      to_plot_features = dtrain[1:1,1:3]
      to_plot_y = dtrain[1:1,3]
    }
    
    pr.nn <- compute(nn, to_plot_features)
    # Extract results
    pr.nn_ <- pr.nn$net.result
    
    # Accuracy (test set)
    original_values <- max.col(to_plot_y)
    pr.nn_2 <- max.col(pr.nn_)
    
    
    
    
    
    
    
    
    
    minf1<-min(to_plot_features$V1)
    maxf1<-max(to_plot_features$V1)
    minf2<-min(to_plot_features$V2)
    maxf2<-max(to_plot_features$V2)
    x<-seq(minf1,maxf1,by=0.05)
    y<-seq(minf2,maxf2,by=0.05)
    g<-expand.grid(x,y)
    colnames(g)=c("V1","V2")
    
    
    pred<-compute(nn, g)
    outs <- pred$net.result
    pred <- max.col(outs)
    
    #plot(to_plot_features[,1], to_plot_features[,2], col = pr.nn_2, xlab = "V1", ylab = "V2")
    plot(g$V1,g$V2,col=pred+3)
    points(to_plot_features$V1,to_plot_features$V2,col=pr.nn_2,pch=19)
    
  })
  
  output$plot_agg <- renderPlot({
    d = datasetInput()
    d$V3 = factor(d$V3)
    
    plot(d$V1, d$V2, col = d$V3)
  })
  
  ########################Dataset Flame ########################""
  
  
  get_sample_flm <- reactive({
    
    d = datasetInput()
    colnames(d)=c("feature1","feature2","target")
    tr <- sample(1:nrow(d),input$trainsplit*nrow(d))
    tr
  })
  training_set_flm <- reactive({
    d = datasetInput()
    colnames(d)=c("feature1","feature2","target")
    d[d$target == 1,]$target <-0
    d[d$target == 2,]$target <-1
    tr=get_sample_flm()
    d[tr,]
  })
  
  testing_set_flm <- reactive({
    d = datasetInput()
    colnames(d)=c("feature1","feature2","target")
    tr <- get_sample_flm()
    d[-tr,]
  })
  
  f1<-function(x,y) x*y
  
  
  #Classification
  logistic <- reactive({
    
    
    Xtrain <- training_set_flm()
    
    
    if (input$option1 == TRUE){
      Xopt<-mapply(f1,Xtrain["feature1"],Xtrain["feature1"])
      colnames(Xopt)=c("opt1")
      Xtrain<- cbind(Xtrain,Xopt)
    }
    if (input$option2 == TRUE){
      Xopt<-mapply(f1,Xtrain["feature2"],Xtrain["feature2"])
      colnames(Xopt)=c("opt2")
      Xtrain<- cbind(Xtrain,Xopt)
    }
    if (input$option3 == TRUE){
      Xopt<-mapply(f1,Xtrain["feature1"],Xtrain["feature2"])
      colnames(Xopt)=c("opt3")
      Xtrain<- cbind(Xtrain,Xopt)
    }
    
    glm(target~.,data=Xtrain,family="binomial")
    
  })
  
  #### OUTPUT CLASSES
  output$classification_flm <- renderPlot({
    tr<-testing_set_flm()
    if (input$option1 == TRUE){
      Xopt<-mapply(f1,tr["feature1"],tr["feature1"])
      colnames(Xopt)=c("opt1")
      tr<- cbind(tr,Xopt)
    }
    if (input$option2 == TRUE){
      Xopt<-mapply(f1,tr["feature2"],tr["feature2"])
      colnames(Xopt)=c("opt2")
      tr<- cbind(tr,Xopt)
    }
    if (input$option3 == TRUE){
      Xopt<-mapply(f1,tr["feature1"],tr["feature2"])
      colnames(Xopt)=c("opt3")
      tr<- cbind(tr,Xopt)
    }
    glm.pred<-predict(logistic(),tr,type="response")
    glm.pred<-ifelse(glm.pred >0.5, 1,0)
    #plot(tr$feature1,tr$feature2,col=glm.pred+1)
    
    minf1<-min(tr$feature1)
    maxf1<-max(tr$feature1)
    minf2<-min(tr$feature2)
    maxf2<-max(tr$feature2)
    x<-seq(minf1,maxf1,by=0.05)
    y<-seq(minf2,maxf2,by=0.05)
    g<-expand.grid(x,y)
    colnames(g)=c("feature1","feature2")
    
    
    ##Partie bind data
    if (input$option1 == TRUE){
      Xopt<-mapply(f1,g["feature1"],g["feature1"])
      colnames(Xopt)=c("opt1")
      g<- cbind(g,Xopt)
    }
    if (input$option2 == TRUE){
      Xopt<-mapply(f1,g["feature2"],g["feature2"])
      colnames(Xopt)=c("opt2")
      g<- cbind(g,Xopt)
    }
    if (input$option3 == TRUE){
      Xopt<-mapply(f1,g["feature1"],g["feature2"])
      colnames(Xopt)=c("opt3")
      g<- cbind(g,Xopt)
    }
    
    
    pred<-predict(logistic(),g,type="response")
    pred<-ifelse(pred >0.5, 1,0)
    
    #plot(logistic(),data=tr)
    plot(g$feature1,g$feature2,col=pred+3)
    points(tr$feature1,tr$feature2,col=glm.pred,pch=19)
    
  })
  
  
  
  
  ###
  output$classification1 <- renderPlot({
    d=datasetInput()
    colnames(d)=c("feature1","feature2","target")
    d[d$target == 1,]$target <-0
    d[d$target == 2,]$target <-1
    plot(logistic(),col=d$target+1,which=c(1,1))
  })
  output$classification2 <- renderPlot({
    d=datasetInput()
    colnames(d)=c("feature1","feature2","target")
    d[d$target == 1,]$target <-0
    d[d$target == 2,]$target <-1
    plot(logistic(),col=d$target+1,which=c(2,1))
  })
  output$classification3 <- renderPlot({
    d=datasetInput()
    colnames(d)=c("feature1","feature2","target")
    d[d$target == 1,]$target <-0
    d[d$target == 2,]$target <-1
    plot(logistic(),col=d$target+1,which=c(3,1))
  })
  output$classification4 <- renderPlot({
    d=datasetInput()
    colnames(d)=c("feature1","feature2","target")
    d[d$target == 1,]$target <-0
    d[d$target == 2,]$target <-1
    plot(logistic(),col=d$target+1,which=c(4,1))
  })
  
  #Affichage de l'analyse univari? 
  output$summary_flm <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  #Affichage du simpleplot flame
  output$plot_flm <- renderPlot({
    dataset <- datasetInput()
    plot(dataset$V1, dataset$V2, col = dataset$V3, pch=16)
  })
  
  #Affichage du dataset
  output$view_flm <- DT::renderDataTable({
    datasetInput()
  })
  
  #Affichage du boxplot
  output$boxplot_flm <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)
  })
  
  ################ HELP
  output$Help <- renderText({
    print("The Channing data frame has 462 rows and 5 columns.\n
 This data frame contains the following columns : \n
 sex : A factor for the sex of each resident(\"Male\" or \"female\").\n
 entry : The resident(in months) on entry to the centre\n
 exit : The age (in months) of the resident on death, leaving the centre or July 1,1975 whichever event occurred first.\n
 time : The length of time (in months) that the resident spent at Channing House.\n
 cens : The indictor of right censoring. 1 indicates that the resident died at Channing House, 0 indicates that they left the house prior to July 1, 1975 or that they were still alive and living in the centre at that date."
    )
    
  })
  
  ###Affichage help_flm
  output$Help_flm<-renderText({
    print("Flame est un dataset qui contient des donnÃ©es synthÃ©tiques.\n
Le dataset contient 240 observations, 2 variables et 2 classes")
  })
  
  ###Affichage help_agg
  output$Help_agg<-renderText({
    print("Aggregation est un dataset qui contient des donnÃ©es synthÃ©tiques.\n
           Le dataset contient 788 observations, 2 variables et 7 classes \n
           facilement sÃ©parables. Ce data set peut Ãªtre utilisÃ© dans le but \n
           de tester des mÃ©thodes d'apprentissage")
  })
  
  ###Affichage help_spr
  output$Help_spr<-renderText({
    print("Spiral est un dataset qui contient des donnÃ©es synthÃ©tiques.\n
           Le dataset contient 312 observations, 2 variables et 3 classes \n
           facilement sÃ©parables. Ce data set peut Ãªtre utilisÃ© dans le but \n
           de tester des mÃ©thodes d'apprentissage.")
  })
  
  
  #### KNN Credit card fraud
  output$plotknn <- renderPlot({
    print("start")
    dataset = datasetInput()
    dataset$Class <- factor(dataset$Class)
    samp <- sample(1:nrow(dataset), round(0.1*nrow(dataset)))
    train <- dataset[samp, ]
    test <- dataset[-samp, ]
    print("mid")
    obs <- test[,30]
    knn <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 1)
    print("end")
    library(caret)
    y_pred<-predict(knn, test[,-31], type = "response")
    pred = ifelse(y_pred > 0.5,1,0)
    roc_knn <- roc(test[,-31], pred)
    plot(roc_knn, main = paste0("AUC: ", round(pROC::auc(roc_knn), 3)))
    
  })
  
  
  prétraitement <- reactive({
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
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    dataset = prétraitement()
    if (input$option2 == TRUE){
      downSample(train, train$Attrition)
    }
    if (input$option3 == TRUE){
      
      upSample(train, train$Attrition)
      
    }
    
    attLog=glm(Attrition~.,data=train,family = binomial)
    
  })
  
  output$Acc_LR <- renderText({
    dataset=prétraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    accuracy <- table(test$Attrition,predGlm>.5)
    sum(diag(accuracy))/sum(accuracy)*100
    
  })
  output$f_score_LR <- renderText({
    dataset=prétraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    pred <- ifelse(predGlm < 0.5, 1, 2)
    
    F1_Score(as.numeric(as.factor(test$Attrition)), pred)*100
  })
  output$classification_lr <- renderPlot({
    dataset=prétraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    attLog=train_Lr()
    predGlm=predict(attLog,type="response",newdata=test)
    #fg <- predGlm>.5
    #bg <- predGlm<.5
    #pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    #plot(pr)
    perf <- performance(prediction(predGlm,as.factor(test$Attrition)),"prec","rec")
    
    plot(perf)
  
    #pred_grid <-glm(as.factor(Attrition)~g[,1:2],data=train,family = binomial)
                  
    #plot(g$JobInvolvement,g$JobSatisfaction, col = pred_grid, xlab="V1", ylab="V2", pch=19)
    #points(test$JobInvolvement,test$JobSatisfaction,col=as.numeric(knn.pred) + 6,pch=19)
  })
  
  
train_rf<- reactive({
  dataset=prétraitement()
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
  dataset=prétraitement()
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
  dataset=prétraitement()
  split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
  train=subset(dataset,split==T)
  test=subset(dataset,split==F)
  randomForestModel=train_rf()
  predictRF=predict(randomForestModel,newdata=test)
  F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predictRF))*100
})
  output$Classification_rf <- renderPlot({
    dataset=prétraitement()
    split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
    train=subset(dataset,split==T)
    test=subset(dataset,split==F)
    
    #randomForestModel=train_rf()  
    randomForestModel=train_rf()
    predictRF=predict(randomForestModel,newdata=test,type="response")
    perf <- performance(prediction(as.numeric(predictRF),as.factor(test$Attrition)),"prec","rec")
    plot(perf)
    table(test$Attrition,predictRF)
    accuracy <- table(as.factor(test$Attrition),predictRF)
    print(sum(diag(accuracy))/sum(accuracy))
  })
}




# Associaion entre interface et serveur
shinyApp(ui = ui, server = server)

