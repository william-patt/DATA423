library(shiny)
library(shinyjs)
library(datasets)
library(dbscan)
library(OutlierDetection)
library(plotly)
library(modeldata)
library(aplpack)
library(recipes)
library(dplyr)
library(ggplot2)
library(DMwR)
library(solitude)
library(readxl)
library(vcd)


set.seed(50) 

X <- iris[,1:4]

ozone <- read.csv("ozone.csv")
ozone <- dplyr::select(ozone, -c(Day_of_month, Day_of_week))
ozone$Month <- as.factor(ozone$Month)


tempchoice <- ozone[c(2:11)]
choice <- colnames(tempchoice)

smp_size <- floor(0.70 * nrow(ozone))
train_ind <- sample(seq_len(nrow(ozone)), size = smp_size)
train <- ozone[train_ind, ]
test <- ozone[-train_ind, ]

yj_estimates <- recipe(ozone_reading ~ ., data = train) %>%
    step_YeoJohnson(all_numeric()) %>%
    prep(data = train)

baked <- bake(yj_estimates, test) 

train1 <- dplyr::select(train, -c(Month))
test1 <- dplyr::select(test, -c(Month))
rec <- recipe(ozone_reading ~ ., data = train1) %>%
    step_naomit(everything()) %>% 
    step_nzv(all_predictors()) %>%  
    step_lincomb(all_predictors()) %>%   
    step_YeoJohnson(all_predictors()) %>% 
    prep(data = train1)

processed <- bake(rec, test1)

noMonthOzone <- dplyr::select(ozone, -c(Month))

shinyUI(fluidPage(
    useShinyjs(),
    titlePanel("Assignment 2 - William Pattison - outlier detection"),
    tabsetPanel(
        tabPanel("Model based Techniques",
                 tabsetPanel(
                     tabPanel("DBScan",
                              tabsetPanel(
                                  tabPanel("DBScan shoulder value",
                                           plotOutput("DBShoulder"),
                                           sliderInput(inputId = "K", label = "k value (minPts)", 
                                                       min = 1, max = 10, step = 1, value = 5),
                                           p("We want to find the shoulder value (a sharp change in the curve) to determine the eps (radius of neighborhood around a point x) value.")
                                           
                                           
                                  ),
                                  tabPanel("DBScan",
                                           plotlyOutput("DBScan"),
                                           textInput("Eps", label = "Enter eps value:", value = 0.5),
                                           verbatimTextOutput("DBScanP"),
                                           p("DBSCAN is a density-based clusering algorithm which can be used to identify clusters in a data containing noise and outliers."),
                                           p("With the default values of k = 5 and eps = 0.5, there are 3 clusters. Cluster one contains 50, two contains 25 and three contains 25.")
                                  )
                                  
                                  
                              )),
                     tabPanel("Cooks Distance",
                              selectizeInput(inputId = "Data3", 
                                             label = "Data:", choices = c("Ozone data (non transformed data)" = "noMonthOzone", "Yeo-Johnson Transformed" = "processed"), multiple = FALSE),
                              plotOutput("Cooks"),
                              p("Cook's distance is a measure of how much a linear regression is affected by each observation."),
                              p("Non transformed data: There are eight outliers. There seems to be no major pattern."),
                              p("Transformed data: There are three outliers. There seems to be no major pattern.")
                     )
                     
                 )),
        
        tabPanel("Univariate + Bivariate",
                 tabsetPanel(
                     tabPanel("Univariate",
                              selectizeInput(inputId = "Variables", 
                                             label = "Show variable:", choices = choice, multiple = FALSE),
                              selectizeInput(inputId = "Data", 
                                             label = "Data:", choices = c("Ozone data (non transformed data)" = "tempchoice", "Yeo-Johnson Transformed" = "baked"), multiple = FALSE),
                              plotOutput("Univariate"),
                              sliderInput(inputId = "range1", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                              verbatimTextOutput("UnivariateP")
                     ),
                     tabPanel("Bivariate: categorical variable",
                              selectizeInput(inputId = "Data1", 
                                             label = "Data:", choices = c("Ozone data (non transformed data)" = "ozone", "Yeo-Johnson Transformed" = "baked"), multiple = FALSE),
                              plotOutput("Bivariate"),
                              plotOutput("Bag"),
                              sliderInput(inputId = "factor", label = "Factor value", 
                                          min = 1, max = 5, step = 0.1, value = 3),
                              checkboxInput(inputId = "Whiskers", label = "Show whiskers", value = FALSE),
                              p("The factor value is the number by which the bag is multiplied to create the fence. If a value is beyond the fence (light blue) it is marked as an outlier."),
                              p("Non transformed data: There are no outliers at the default factor value of 3."),
                              p("Transformed data: There are no outliers at the default factor value of 3.")
                     ),
                     
                     tabPanel("Bivariate: numeric variable",
                              selectizeInput(inputId = "Data2", 
                                             label = "Data:", choices = c("Ozone data (non transformed data)" = "ozone", "Yeo-Johnson Transformed" = "baked"), multiple = FALSE),
                              plotOutput("Bivariate2"),
                              plotOutput("Bag1"),
                              sliderInput(inputId = "factor1", label = "Factor value", 
                                          min = 1, max = 5, step = 0.1, value = 3),
                              checkboxInput(inputId = "Whiskers1", label = "Show whiskers", value = FALSE),
                              p("Non transformed data: There are two outliers at the default factor value of 3."),
                              p("Transformed data: There are no outliers at the default factor value of 3.")
                     )
                     
                 )),
        
        tabPanel("Multivariate",
                 tabsetPanel(
                     tabPanel("Mahalanobis distance",
                              selectizeInput(inputId = "Data4", 
                                             label = "Data:", choices = c("Ozone data (non transformed data)" = "noMonthOzone", "Yeo-Johnson Transformed" = "processed"), multiple = FALSE),
                              plotOutput("Multivariate1"),
                              sliderInput(inputId = "factor2", label = "Threshold value", 
                                          min = 0, max = 1, step = 0.01, value = 0.99),
                              p("Mahalanobis distance is a multivariate distance measure that measures the distance between a point and a distribution."),
                              p("Non transformed data: There are two outliers at the default threshold of 99%.
                            The first 50% of the observations seem to have slightly higher values on average, however, there is no real pattern."),
                              p("Transformed data: There are no outliers at the default threshold of 99%.")
                     ),
                     tabPanel("Nominal variables",
                              plotOutput("Multivariate2"),
                              p("We can use a mosaic plot to identify intersections that are rare between nominal variables (shown in red)")
                     )
                     
                     
                 )),
        
        tabPanel("Extra: 3D visualisation using the OutlierDetection package",
                 tabsetPanel(
                     tabPanel("Robust Kernal-based Outlier Factor (RKOF)",
                              plotlyOutput("RKOF"),
                              sliderInput(inputId = "K1", label = "No. of nearest neighbours", 
                                          min = 2, max = 10, step = 1, value = 4),
                              sliderInput(inputId = "Cutoff", label = "Threshold value", 
                                          min = 0, max = 1, step = 0.01, value = 0.95),
                              verbatimTextOutput("RKOFP")
                              
                     ),
                     tabPanel("Mahalanobis distance",
                              plotlyOutput("Maha"),
                              sliderInput(inputId = "Cutoff1", label = "Threshold value", 
                                          min = 0, max = 1, step = 0.01, value = 0.95),
                              verbatimTextOutput("MahaP")
                              
                     ),
                     tabPanel("k Nearest Neighbours Distance",
                              plotlyOutput("Nn"),
                              sliderInput(inputId = "K2", label = "No. of nearest neighbours", 
                                          min = 0, max = 10, step = 1, value = 4),
                              sliderInput(inputId = "Cutoff2", label = "Threshold value", 
                                          min = 0, max = 1, step = 0.01, value = 0.95),
                              verbatimTextOutput("NnP")
                              
                     )
                     
                 )
                 
                 
        )
        
        
    ))
)

