library(vcd)
library(GGally)
library(visdat)
library(DT)
library(corrgram)
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(funModeling)
library(gridExtra)
library(scales)

#setwd("P://Desktop//4th year//DATA423//Assignments//Assignment1")
setwd("C://Users//William//Desktop//Uni//4th year//DATA423//Assignments//Assignment1")
dat <- read.csv("Ass1Data.csv", header = TRUE)
dat$Date <- as.Date(dat$Date)
dat$Priority <- factor(dat$Priority, ordered = TRUE,
                       levels = c("Low", "Medium", "High"))
dat$Price <- factor(dat$Price, ordered = TRUE,
                    levels = c("Cheap", "Costly", "Extravagant"))
dat$Speed <- factor(dat$Speed, ordered = TRUE,
                    levels = c("Slow", "Medium", "Fast"))
dat$Duration <- factor(dat$Duration, ordered = TRUE,
                       levels = c("Short", "Long", "Very Long"))
dat$Temp <- factor(dat$Temp, ordered = TRUE,
                   levels = c("Cold", "Warm", "Hot"))
facs <- dat[c(3, 5:14)]
nafacs <- na.omit(facs)
choice <- colnames(facs)


shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Assignment 1 - William Pattison"),
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),
  tabsetPanel(
    tabPanel("Data",
             tabsetPanel(
               tabPanel("Full Summary",
                        verbatimTextOutput(outputId = "Summary")
               ),
               tabPanel("Sensor Summary",
                        verbatimTextOutput(outputId = "Summarysensor"),
                        h3("Missingness"),
                        plotOutput(outputId = "Missing1"),
                        checkboxInput(inputId = "cluster1", label = "Cluster missingness", value = TRUE)
               ),
               tabPanel("Non Sensor Summary",
                        verbatimTextOutput(outputId = "Summarynonsensor"),
                        h3("Missingness"),
                        plotOutput(outputId = "Missing2"),
                        checkboxInput(inputId = "cluster2", label = "Cluster missingness", value = TRUE)
               ),
               tabPanel("Raw Data",
                        DT::dataTableOutput(outputId = "Datatable"),
                        hr(),
                        verbatimTextOutput(outputId = "Dim1"),
                        verbatimTextOutput(outputId = "Dim2")
                        
               )
             )
    ),
    
    tabPanel("Factors",
             tabsetPanel(
               tabPanel("Summary",
                        verbatimTextOutput(outputId = "Summaryfac")
               ),
               
               tabPanel("Barplots",
                        selectizeInput(inputId = "Variables2", label = "Show variable:", choices = choice, multiple = FALSE),
                        plotOutput(outputId = "Fachist")
               ),
               
               tabPanel("Mosaic",
                        selectizeInput(inputId = "Variables1", label = "Show variables:", choices = choice, multiple = TRUE, selected = choice[2:3]),
                        plotOutput(outputId = "Mosaic")
               )
               
             )
    ),
    
    tabPanel("Numeric",
             tabsetPanel(
               tabPanel("Pairs:"),
               
               tabPanel("Sensor 1-10",
                        plotOutput(outputId = "Pair1")
               ),
               tabPanel("Sensor 11-20",
                        plotOutput(outputId = "Pair2")
               ),
               tabPanel("Sensor 21-30",
                        plotOutput(outputId = "Pair3")
               )
               
             ),
             
             tabsetPanel(
               tabPanel("Correlation:"),
               
               tabPanel("All Sensors",
                        plotOutput(outputId = "Cor")
               ),
               
               tabPanel("Sensor 1-10",
                        plotOutput(outputId = "Cor1")
               ),
               tabPanel("Sensor 11-20",
                        plotOutput(outputId = "Cor2")
               ),
               tabPanel("Sensor 21-30",
                        plotOutput(outputId = "Cor3")
               )
               
             ),
             
             tabsetPanel(
               tabPanel("Boxplots:"),
               
               tabPanel("Sensor 1-10",
                        plotOutput(outputId = "Boxplot1"),
                        checkboxInput(inputId = "standardise1", label = "Show standardised", value = FALSE),
                        checkboxInput(inputId = "outliers1", label = "Show outliers", value = TRUE),
                        sliderInput(inputId = "range1", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
               ),
               tabPanel("Sensor 11-20",
                        plotOutput(outputId = "Boxplot2"),
                        checkboxInput(inputId = "standardise2", label = "Show standardised", value = FALSE),
                        checkboxInput(inputId = "outliers2", label = "Show outliers", value = TRUE),
                        sliderInput(inputId = "range2", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
               ),
               tabPanel("Sensor 21-30",
                        plotOutput(outputId = "Boxplot3"),
                        checkboxInput(inputId = "standardise3", label = "Show standardised", value = FALSE),
                        checkboxInput(inputId = "outliers3", label = "Show outliers", value = TRUE),
                        sliderInput(inputId = "range3", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
               )
               
             ),
             
             tabsetPanel(
               tabPanel("Continuity:"),
               
               tabPanel("All sensors + Y",
                        plotOutput(outputId = "Continuity1")
               ),
               tabPanel("Sensor 1-10 + Y",
                        plotOutput(outputId = "Continuity2")
               ),
               tabPanel("Sensor 11-20",
                        plotOutput(outputId = "Continuity3")
               ),
               tabPanel("Sensor 21-30",
                        plotOutput(outputId = "Continuity4")
               )
             ),
             
             tabsetPanel(
               tabPanel("Homogeneity:"),
               
               tabPanel("All sensors",
                        plotOutput(outputId = "Scale")
               )
             )
             
             
             
             
             
    )
  )
))
