shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })  
  
  

  
  

  
  
######################################################### maintenance point ####################################################
  
############################################################ Ridge ########################################################
  
  
  
  
  ##############################################################################  
  getRidgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RidgePreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$RidgeGo,
    {
      library(elasticnet)
      method <- "ridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRidgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$RidgeModelSummary0 <- renderText({
    description("ridge")
  })
  
  ##############################################################################  
  output$RidgeMetrics <- renderTable({
    req(models$ridge)
    models$ridge$results[ which.min(models$ridge$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RidgeRecipe <- renderPrint({
    req(models$ridge)
    models$ridge$recipe
  })  
  
  ############################################################################## 
  output$RidgeModelSummary2 <- renderPrint({
    req(models$ridge)
    print(models$ridge)
  })
  
  
  output$RidgeModelPlots <- renderPlot({
    req(models$ridge)
    plot(models$ridge)
  })
  
  
  
  
############################################################ elastic net ########################################################
  
  
  
  
  ##############################################################################  
  getEnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$EnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$EnetGo,
    {
      library(elasticnet)
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getEnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$EnetModelSummary0 <- renderText({
    description("enet")
  })
  
  ##############################################################################  
  output$EnetMetrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$EnetRecipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })  
  
  ############################################################################## 
  output$EnetModelSummary2 <- renderPrint({
    req(models$enet)
    print(models$enet)
  })
  
  
  output$EnetModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })
  
  
  
  ############################################################ Linear SVM ########################################################
  
  
  
  
  ##############################################################################  
  getSvmlinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SvmlinearPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$SvmlinearGo,
    {
      library(kernlab)
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getSvmlinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$SvmlinearModelSummary0 <- renderText({
    description("svmLinear")
  })
  
  ##############################################################################  
  output$SvmlinearMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$SvmlinearRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  ############################################################################## 
  output$SvmlinearModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })
  
  
  output$SvmlinearModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  
  
  
  ############################################################ Radial basis SVM ########################################################
  
  
  
  
  ##############################################################################  
  getSvmradialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SvmradialPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$SvmradialGo,
    {
      library(kernlab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getSvmradialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$SvmradialModelSummary0 <- renderText({
    description("svmRadial")
  })
  
  ##############################################################################  
  output$SvmradialMetrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$SvmradialRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  ############################################################################## 
  output$SvmradialModelSummary2 <- renderPrint({
    req(models$svmRadial)
    print(models$svmRadial)
  })
  
  
  output$SvmradialModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })
  
  
  
  ############################################################ Regularized Random Forest ########################################################
  
  
  
  
  ##############################################################################  
  getRffRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RffPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$RffGo,
    {
      library(randomForest)
      library(RRF)
      method <- "RRF"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRffRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$RffModelSummary0 <- renderText({
    description("RRF")
  })
  
  ##############################################################################  
  output$RffMetrics <- renderTable({
    req(models$RRF)
    models$RRF$results[ which.min(models$RRF$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RffRecipe <- renderPrint({
    req(models$RRF)
    models$RRF$recipe
  })  
  
  ############################################################################## 
  output$RffModelSummary2 <- renderPrint({
    req(models$RRF)
    print(models$RRF)
  })
  
  
  output$RffModelPlots <- renderPlot({
    req(models$RRF)
    plot(models$RRF)
  })
  
  
  
  
  ############################################################ Bagged CART ########################################################
  
  
  
  
  ##############################################################################  
  getEglmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$EglmPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$EglmGo,
    {
      library(ipred)
      library(e1071)
      library(plyr)
      method <- "treebag"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getEglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$EglmModelSummary0 <- renderText({
    description("treebag")
  })
  
  ##############################################################################  
  output$EglmMetrics <- renderTable({
    req(models$treebag)
    models$treebag$results[ which.min(models$treebag$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$EglmRecipe <- renderPrint({
    req(models$treebag)
    models$treebag$recipe
  })  
  
  ############################################################################## 
  output$EglmModelSummary2 <- renderPrint({
    req(models$treebag)
    print(models$treebag)
  })
  
  
  output$EglmModelPlots <- renderPlot({
    req(models$treebag)
    plot(models$treebag)
  })
  
  
  
  ############################################################ Boosted Tree ########################################################
  
  
  
  
  ##############################################################################  
  getBtRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BtPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$BtGo,
    {
      library(party)
      library(mboost)
      library(plyr)
      library(partykit)
      
      method <- "blackboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBtRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BtModelSummary0 <- renderText({
    description("blackboost")
  })
  
  ##############################################################################  
  output$BtMetrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BtRecipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })  
  
  ############################################################################## 
  output$BtModelSummary2 <- renderPrint({
    req(models$blackboost)
    print(models$blackboost)
  })
  
  
  output$BtModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })
  
  
  
  ############################################################ Conditional Inference Random Forest ########################################################
  
  
  
  
  ##############################################################################  
  getCforestRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CforestPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$CforestGo,
    {
      library(party)
      
      method <- "cforest"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getCforestRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$CforestModelSummary0 <- renderText({
    description("cforest")
  })
  
  ##############################################################################  
  output$CforestMetrics <- renderTable({
    req(models$cforest)
    models$cforest$results[ which.min(models$cforest$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CforestRecipe <- renderPrint({
    req(models$cforest)
    models$cforest$recipe
  })  
  
  ############################################################################## 
  output$CforestModelSummary2 <- renderPrint({
    req(models$cforest)
    print(models$cforest)
  })
  
  
  output$CforestModelPlots <- renderPlot({
    req(models$cforest)
    plot(models$cforest)
  })
  
  
  ############################################################ Boosted Generalized Additive Model ########################################################
  
  
  
  
  ##############################################################################  
  getGamboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GamboostPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GamboostGo,
    {
      library(mboost)
      library(plyr)
      library(import)
      
      method <- "gamboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGamboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$GamboostModelSummary0 <- renderText({
    description("gamboost")
  })
  
  ##############################################################################  
  output$GamboostMetrics <- renderTable({
    req(models$gamboost)
    models$gamboost$results[ which.min(models$gamboost$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GamboostRecipe <- renderPrint({
    req(models$gamboost)
    models$gamboost$recipe
  })  
  
  ############################################################################## 
  output$GamboostModelSummary2 <- renderPrint({
    req(models$gamboost)
    print(models$gamboost)
  })
  
  
  output$GamboostModelPlots <- renderPlot({
    req(models$gamboost)
    plot(models$gamboost)
  })   
  
  
  
############################################################ AutoEncoder Deep Neural Network Model ########################################################
  
  
  
  
  ##############################################################################  
  getDnnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$DnnPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$DnnGo,
    {
      library(deepnet)
      
      method <- "dnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getDnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$DnnModelSummary0 <- renderText({
    description("dnn")
  })
  
  ##############################################################################  
  output$DnnMetrics <- renderTable({
    req(models$dnn)
    models$dnn$results[ which.min(models$dnn$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$DnnRecipe <- renderPrint({
    req(models$dnn)
    models$dnn$recipe
  })  
  
  ############################################################################## 
  output$DnnModelSummary2 <- renderPrint({
    req(models$dnn)
    print(models$dnn)
  })
  
  
  output$DnnModelPlots <- renderPlot({
    req(models$dnn)
    plot(models$dnn)
  })
  
  
  
############################################################ Cubist Model ########################################################
  
  
  
  
  ##############################################################################  
  getCubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CubistPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$CubistGo,
    {
      library(Cubist)
      
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  ##############################################################################  
  output$CubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  ############################################################################## 
  output$CubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  
  output$CubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })   
  
  
  
############################################################ AvNNet Model ########################################################
  
  
  
  
  ##############################################################################  
  getAvnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$AvnPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$AvnGo,
    {
      library(nnet)
      
      method <- "avNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getAvnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$AvnModelSummary0 <- renderText({
    description("avNNet")
  })
  
  ##############################################################################  
  output$AvnMetrics <- renderTable({
    req(models$avNNet)
    models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$AvnRecipe <- renderPrint({
    req(models$avNNet)
    models$avNNet$recipe
  })  
  
  ############################################################################## 
  output$AvnModelSummary2 <- renderPrint({
    req(models$avNNet)
    print(models$avNNet)
  })
  
  
  output$AvnModelPlots <- renderPlot({
    req(models$avNNet)
    plot(models$avNNet)
  })   
 
  
  
############################################################ M5 Model ########################################################
  
  
  
  
  ##############################################################################  
  getM5Recipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$M5Preprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$M5Go,
    {
      library(RWeka)
      
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$M5ModelSummary0 <- renderText({
    description("M5")
  })
  
  ##############################################################################  
  output$M5Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$M5Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  ############################################################################## 
  output$M5ModelSummary2 <- renderPrint({
    req(models$M5)
    print(models$M5)
  })
  
  
  output$M5ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })  
  
  
  
############################################################ Boosted linear Model ########################################################
  
  
  
  
  ##############################################################################  
  getBstLmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BstLmPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$BstLmGo,
    {
      library(bst)
      library(plyr)
      
      method <- "BstLm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBstLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BstLmModelSummary0 <- renderText({
    description("BstLm")
  })
  
  ##############################################################################  
  output$BstLmMetrics <- renderTable({
    req(models$BstLm)
    models$BstLm$results[ which.min(models$BstLm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BstLmRecipe <- renderPrint({
    req(models$BstLm)
    models$BstLm$recipe
  })  
  
  ############################################################################## 
  output$BstLmModelSummary2 <- renderPrint({
    req(models$BstLm)
    print(models$BstLm)
  })
  
  
  output$BstLmModelPlots <- renderPlot({
    req(models$BstLm)
    plot(models$BstLm)
  }) 
  
  
############################################################ Bayesian Additive Regression Trees Model ########################################################
  
  
  
  
  ##############################################################################  
  getBartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BartPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$BartGo,
    {
      library(bartMachine)
      method <- "bartMachine"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BartModelSummary0 <- renderText({
    description("bartMachine")
  })
  
  ##############################################################################  
  output$BartMetrics <- renderTable({
    req(models$bartMachine)
    models$bartMachine$results[ which.min(models$bartMachine$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BartRecipe <- renderPrint({
    req(models$bartMachine)
    models$bartMachine$recipe
  })  
  
  ############################################################################## 
  output$BartModelSummary2 <- renderPrint({
    req(models$bartMachine)
    print(models$bartMachine)
  })
  
  
  output$BartModelPlots <- renderPlot({
    req(models$bartMachine)
    plot(models$bartMachine)
  }) 
  
  
  
############################################################ Linear Model ########################################################
  
  
  
  
  ##############################################################################  
  getLmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$LmPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$LmGo,
    {
      
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$LmModelSummary0 <- renderText({
    description("lm")
  })
  
  ##############################################################################  
  output$LmMetrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$LmRecipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  ############################################################################## 
  output$LmModelSummary2 <- renderPrint({
    req(models$lm)
    print(models$lm)
  })
  
  
  output$LmModelPlots <- renderPlot({
    req(models$lm)
    plot(models$lm)
  }) 
  
  
############################################################ Knn Model ########################################################
  
  
  
  
  ##############################################################################  
  getKnnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$KnnPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$KnnGo,
    {
      
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getKnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$KnnModelSummary0 <- renderText({
    description("knn")
  })
  
  ##############################################################################  
  output$KnnMetrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$KnnRecipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  ############################################################################## 
  output$KnnModelSummary2 <- renderPrint({
    req(models$knn)
    print(models$knn)
  })
  
  
  output$KnnModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })   
  
  

  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
