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


shinyServer(function(input, output) {
  
  output$Datatable <- DT::renderDataTable({
    DT::datatable(data = dat)
  })
  
  output$Dim1 <- renderPrint({
    cat("Number of observations = ", dim(dat)[1])
  })
  
  output$Dim2 <- renderPrint({
    cat("Number of variables = ", dim(dat)[2])
  })
  
  output$Summary <- renderPrint({
    summary(dat)
  })
  
  output$Summarysensor <- renderPrint({
    summary(dat[15:44])
  })
  
  output$Summarynonsensor <- renderPrint({
    summary(dat[-c(15:44)])
  })
  
  output$Summaryfac <- renderPrint({
    summary(facs)
  })
  
  output$Missing1 <- renderPlot({
    vis_miss(dat[15:44], cluster = input$cluster1) #+
    #labs(title = "Missingness of Sensors")
  })
  
  output$Missing2 <- renderPlot({
    vis_miss(dat[6:14], cluster = input$cluster2) #+
    #labs(title = "Missingness of Non sensor")
  })
  
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$Variables1, collapse = " + ")))
    vcd::mosaic(formula, data = nafacs,
                main = "Frequency novelties", shade = TRUE, legend = TRUE)
  })
  
  output$Fachist <- renderPlot({
    freq(input$Variables2, data = facs)
  })
  
  output$Continuity1 <- renderPlot({
    d <- dat[c(1, 15:44)]  
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] 
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  
    mypalette <- rainbow(ncol(d))
    matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
  })
  
  output$Continuity2 <- renderPlot({
    d <- dat[c(1, 15:24)]  
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] 
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  
    mypalette <- rainbow(ncol(d))
    matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
  })
  
  output$Continuity3 <- renderPlot({
    d <- dat[25:34]  
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] 
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  
    mypalette <- rainbow(ncol(d))
    matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
  })
  
  output$Continuity4 <- renderPlot({
    d <- dat[35:44]  
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] 
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  
    mypalette <- rainbow(ncol(d))
    matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
  })
  
  output$Pair1 <-  renderPlot({
    GGally::ggpairs(dat[15:24])
  })
  
  output$Pair2 <-  renderPlot({
    GGally::ggpairs(dat[25:34])
  })
  
  output$Pair3 <-  renderPlot({
    GGally::ggpairs(dat[35:44])
  })
  
  output$Cor <-  renderPlot({
    corrgram::corrgram(dat[15:44], order = "OLO")
  })
  
  output$Cor1 <-  renderPlot({
    corrgram::corrgram(dat[15:24], order = "OLO")
  })
  
  output$Cor2 <-  renderPlot({
    corrgram::corrgram(dat[25:34], order = "OLO")
  })
  
  output$Cor3 <-  renderPlot({
    corrgram::corrgram(dat[35:44], order = "OLO")
  })
  
  output$Boxplot1 <- renderPlot({
    data <- scale(dat[15:24], center = input$standardise1, scale = input$standardise1)
    boxplot(x = data, use.cols = TRUE, notch = TRUE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$outliers1, 
            range = input$range1, main = "Sensor 1-10")
  })
  
  output$Boxplot2 <- renderPlot({
    data <- scale(dat[25:34], center = input$standardise2, scale = input$standardise2)
    boxplot(x = data, use.cols = TRUE, notch = TRUE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$outliers2, 
            range = input$range2, main = "Sensor 11-20")
  })
  
  output$Boxplot3 <- renderPlot({
    data <- scale(dat[35:44], center = input$standardise3, scale = input$standardise3)
    boxplot(x = data, use.cols = TRUE, notch = TRUE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$outliers3, 
            range = input$range3, main = "Sensor 21-30")
  })
  
  output$Scale <- renderPlot({
    data <- scale(dat[15:44], center = TRUE, scale = TRUE)
    matplot(data, type = "l", col = alpha(rainbow(ncol(data)), 0.3))
  })
  
  
  
  
})


