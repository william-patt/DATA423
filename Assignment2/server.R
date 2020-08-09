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


tempchoice <- ozone[c(1:11)]
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

shinyServer(function(input, output){
    
    output$DBShoulder <- renderPlot({
        data("moons")
        dbscan::kNNdistplot(moons, k = input$K)
    })
    
    output$DBScan <- renderPlotly({
        data("moons")
        cl <- dbscan(moons, eps = input$Eps, minPts = input$K)
        
        plot_ly(moons, x = ~X, y = ~Y, color = cl$cluster+1, type = "scatter", mode = "markers")
    })
    
    output$DBScanP <- renderPrint({
        cl <- dbscan(moons, eps = input$Eps, minPts = input$K)
        cl
    })
    
    RKOF <- reactive({
        dens(X, k = input$K1, cutoff = input$Cutoff )
    })
    
    output$RKOF <- renderPlotly({
        RKOF()[["3Dplot"]] 
    })
    
    output$RKOFP <- renderPrint({
        RKOF()[["Outlier Observations"]] 
    })
    
    Maha <- reactive({
        maha(X, cutoff = input$Cutoff1)
    })
    
    output$Maha <- renderPlotly({
        Maha()[["3Dplot"]] 
    })
    
    output$MahaP <- renderPrint({
        Maha()[["Outlier Observations"]] 
    })
    
    Nn <- reactive({
        nn(X, k = input$K2, cutoff = input$Cutoff2)
    })
    
    output$Nn <- renderPlotly({
        Nn()[["3Dplot"]] 
    })
    
    output$NnP <- renderPrint({
        Nn()[["Outlier Observations"]] 
    })
    
    BaseData <- reactive({
        get(input$Data) 
    })
    
    BaseData1 <- reactive({
        get(input$Data1) 
    })
    
    BaseData2 <- reactive({
        get(input$Data2) 
    })
    
    BaseData3 <- reactive({
        get(input$Data3) 
    })
    
    BaseData4 <- reactive({
        get(input$Data4) 
    })
    
    
    output$Univariate <- renderPlot({
        data <- BaseData()
        dd <- as.data.frame(data)
        boxplot(x = dd[, input$Variables], range = input$range1)
    })
    
    
    output$UnivariateP <- renderPrint({
        data <- BaseData()
        dd <- as.data.frame(data)
        paste("Outliers: ", paste(boxplot.stats(dd[, input$Variables], coef = input$range1)$out
                                  , collapse=", "))
    })
    
    
    Bivariate1 <- reactive({
        boxplot(ozone_reading ~ Month, data = BaseData1(), 
                main = "Ozone reading by month")
    })
    
    output$Bivariate <- renderPlot({
        Bivariate1()
    })
    
    Bivariate2 <- reactive({
        boxplot(ozone_reading ~ pressure_height, data = BaseData2(), 
                main = "Ozone reading by pressure height (m)")
        
        
    })
    
    output$Bivariate2 <- renderPlot({
        Bivariate2()
    })
    
    output$Bag <- renderPlot({
        data <- BaseData1()
        dd <- as.data.frame(data)
        bagplot(x = as.numeric(dd$Month), y = dd$ozone_reading, factor = input$factor, show.whiskers = input$Whiskers)
    })
    
    output$Bag1 <- renderPlot({
        data <- BaseData2()
        dd <- as.data.frame(data)
        bagplot(x = dd$pressure_height, y = dd$ozone_reading, factor = input$factor1, show.whiskers = input$Whiskers1)
        
    })
    
    output$Cooks <- renderPlot({
        data <- BaseData3()
        dd <- as.data.frame(data)
        mod <- lm(ozone_reading ~ ., data=dd)
        cooksd <- cooks.distance(mod)
        plot(cooksd, pch=4, cex=1, main="Influential Obs by Cooks distance")  
        abline(h = 4*mean(cooksd, na.rm=T), col="red")  
        text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
    })
    
    output$Multivariate1 <- renderPlot({
        data <- BaseData4()
        dd <- as.data.frame(data)
        varMat <- var(dd) 
        colM <- colMeans(dd) 
        md2 <- mahalanobis(x =  dd, center = colM, cov = varMat)
        
        threshold <- qchisq(p = input$factor2, df = ncol(dd))  
        
        ggplot(mapping = aes(y = md2, x = (1:length(md2))/length(md2))) +
            geom_point() +
            scale_y_continuous(limits = c(0, max(md2)*1.1)) +
            labs(y = "Mahalanobis distance squared", x = "Complete Observations", title = "Outlier pattern (outliers marked by observation number)") +
            geom_abline(slope = 0, intercept = threshold, color = "red") +
            scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
            theme(legend.position = "bottom") +
            geom_text(aes(label=ifelse(md2>threshold,as.character(rownames(dd)),'')),hjust=0,vjust=0)
    })
    
    
    output$Multivariate2 <- renderPlot({
        mosaic(HairEyeColor, shade = TRUE)
    })
    
})
