shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - William Pattison"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               tabPanel("Ridge Model",
                        verbatimTextOutput(outputId = "RidgeModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "RidgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RidgePreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "RidgeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RidgeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RidgeMetrics"),
                        hr(),
                        plotOutput(outputId = "RidgeModelPlots"),
                        verbatimTextOutput(outputId = "RidgeRecipe"),
                        verbatimTextOutput("RidgeModelSummary2")
               ),
               tabPanel("Elastic net Model",
                        verbatimTextOutput(outputId = "EnetModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "EnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "EnetPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "EnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "EnetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "EnetMetrics"),
                        hr(),
                        plotOutput(outputId = "EnetModelPlots"),
                        verbatimTextOutput(outputId = "EnetRecipe"),
                        verbatimTextOutput("EnetModelSummary2")
               ),
               tabPanel("Linear SVM Model",
                        verbatimTextOutput(outputId = "SvmlinearModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "SvmlinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "SvmlinearPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "SvmlinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "SvmlinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "SvmlinearMetrics"),
                        hr(),
                        plotOutput(outputId = "SvmlinearModelPlots"),
                        verbatimTextOutput(outputId = "SvmlinearRecipe"),
                        verbatimTextOutput("SvmlinearModelSummary2")
               ),
               tabPanel("Radial basis SVM Model",
                        verbatimTextOutput(outputId = "SvmradialModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "SvmradialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "SvmradialPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "SvmradialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "SvmradialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "SvmradialMetrics"),
                        hr(),
                        plotOutput(outputId = "SvmradialModelPlots"),
                        verbatimTextOutput(outputId = "SvmradialRecipe"),
                        verbatimTextOutput("SvmradialModelSummary2")
               ),
               tabPanel("Regularized Random Forest Model",
                        verbatimTextOutput(outputId = "RffModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "RffPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RffPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "RffGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RffGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RffMetrics"),
                        hr(),
                        plotOutput(outputId = "RffModelPlots"),
                        verbatimTextOutput(outputId = "RffRecipe"),
                        verbatimTextOutput("RffModelSummary2")
               ),
               tabPanel("Bagged CART",
                        verbatimTextOutput(outputId = "EglmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "EglmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "EglmPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "EglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "EglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "EglmMetrics"),
                        hr(),
                        plotOutput(outputId = "EglmModelPlots"),
                        verbatimTextOutput(outputId = "EglmRecipe"),
                        verbatimTextOutput("EglmModelSummary2")
               ),
               tabPanel("Boosted Tree Model",
                        verbatimTextOutput(outputId = "BtModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "BtPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "BtPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "BtGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BtGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BtMetrics"),
                        hr(),
                        plotOutput(outputId = "BtModelPlots"),
                        verbatimTextOutput(outputId = "BtRecipe"),
                        verbatimTextOutput("BtModelSummary2")
               ),
               tabPanel("Conditional Inference RF",
                        verbatimTextOutput(outputId = "CforestModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "CforestPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "CforestPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "CforestGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CforestGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CforestMetrics"),
                        hr(),
                        plotOutput(outputId = "CforestModelPlots"),
                        verbatimTextOutput(outputId = "CforestRecipe"),
                        verbatimTextOutput("CforestModelSummary2")
               ),
               tabPanel("Boosted Generalized Additive Model",
                        verbatimTextOutput(outputId = "GamboostModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "GamboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "GamboostPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "GamboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GamboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GamboostMetrics"),
                        hr(),
                        plotOutput(outputId = "GamboostModelPlots"),
                        verbatimTextOutput(outputId = "GamboostRecipe"),
                        verbatimTextOutput("GamboostModelSummary2")
               ),
               tabPanel("Stacked AutoEncoder Deep Neural Network",
                        verbatimTextOutput(outputId = "DnnModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "DnnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "DnnPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "DnnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "DnnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "DnnMetrics"),
                        hr(),
                        plotOutput(outputId = "DnnModelPlots"),
                        verbatimTextOutput(outputId = "DnnRecipe"),
                        verbatimTextOutput("DnnModelSummary2")
               ),
               tabPanel("Cubist Model",
                        verbatimTextOutput(outputId = "CubistModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "CubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "CubistPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "CubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CubistMetrics"),
                        hr(),
                        plotOutput(outputId = "CubistModelPlots"),
                        verbatimTextOutput(outputId = "CubistRecipe"),
                        verbatimTextOutput("CubistModelSummary2")
               ),
               tabPanel("Model Averaged Neural Network Model",
                        verbatimTextOutput(outputId = "AvnModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "AvnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "AvnPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "AvnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "AvnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "AvnMetrics"),
                        hr(),
                        plotOutput(outputId = "AvnModelPlots"),
                        verbatimTextOutput(outputId = "AvnRecipe"),
                        verbatimTextOutput("AvnModelSummary2")
               ),
               tabPanel("M5 Model",
                        verbatimTextOutput(outputId = "M5ModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "M5Preprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "M5Preprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "M5Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "M5Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "M5Metrics"),
                        hr(),
                        plotOutput(outputId = "M5ModelPlots"),
                        verbatimTextOutput(outputId = "M5Recipe"),
                        verbatimTextOutput("M5ModelSummary2")
               ),
               tabPanel("Boosted Linear Model",
                        verbatimTextOutput(outputId = "BstLmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "BstLmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "BstLmPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "BstLmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BstLmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BstLmMetrics"),
                        hr(),
                        plotOutput(outputId = "BstLmModelPlots"),
                        verbatimTextOutput(outputId = "BstLmRecipe"),
                        verbatimTextOutput("BstLmModelSummary2")
               ),
               tabPanel("Bayesian Additive Regression Trees Model",
                        verbatimTextOutput(outputId = "BartModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "BartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "BartPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "BartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BartMetrics"),
                        hr(),
                        plotOutput(outputId = "BartModelPlots"),
                        verbatimTextOutput(outputId = "BartRecipe"),
                        verbatimTextOutput("BartModelSummary2")
               ),
               tabPanel("Linear Model",
                        verbatimTextOutput(outputId = "LmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "LmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "LmPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "LmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "LmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "LmMetrics"),
                        hr(),
                        plotOutput(outputId = "LmModelPlots"),
                        verbatimTextOutput(outputId = "LmRecipe"),
                        verbatimTextOutput("LmModelSummary2")
               ),
               tabPanel("Knn Model",
                        verbatimTextOutput(outputId = "KnnModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "KnnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "KnnPreprocess",
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "KnnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "KnnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "KnnMetrics"),
                        hr(),
                        plotOutput(outputId = "KnnModelPlots"),
                        verbatimTextOutput(outputId = "KnnRecipe"),
                        verbatimTextOutput("KnnModelSummary2")
               )
               
               
               
               
            
               
######################################################### maintenance point ####################################################
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
