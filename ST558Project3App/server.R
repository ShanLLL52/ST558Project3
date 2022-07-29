library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(recipes)
library(caret)

# Read in data
heart <- read_csv("heart.csv")

# Select variable that Interested in
heart <- heart %>% select(Age, Sex, ChestPainType, RestingBP,
                          Cholesterol, HeartDisease)
heart$Sex <- factor(heart$Sex)
heart$ChestPainType <- factor(heart$ChestPainType)
heart$HeartDisease <- factor(heart$HeartDisease)

shinyServer(function(input, output) {
  
  output$img <- renderImage({
    list(src = "dataset-cover")
  } ,deleteFile = FALSE)
  output$text <- renderText({
    "The purpose of the app is to explore data using different plots and tables, and model the data using different supervised learning models."
  })
  
  # Create data for numeric variables
  ndata <- reactive({
    value <- heart %>% select(input$var1)
  })
  # Create data for categorical variables
  cdata <- reactive({
    value <- heart %>% select(input$var2)
  })
  
  # Create Numeric Summary table and contingency table
  output$table <- renderDataTable({
    if (input$type == 'nvar'){
      value <- ndata()
      value %>% summarise(
        "Min" = round(min(!!sym(input$var1)), digits = 2),
        "Avg" = round(mean(!!sym(input$var1)), digits = 2),
        "Median" = round(median(!!sym(input$var1)), digits = 2),
        "Max" = round(max(!!sym(input$var1)), digits = 2),
        "Sd" = round(sd(!!sym(input$var1)), digits = 2),
        "IQR" = round(IQR(!!sym(input$var1)), digits = 2)
      )
    } else{
      value <- cdata()
      t <- as.data.frame(table(value[,1]))
      names(t)[1] <- input$var2
      print(t)
    }
})
  output$plots <- renderPlot({
    value <- ndata()
    if (input$type == 'nvar' & input$plot1 == 'hist'){
      g <- ggplot(data = heart, aes_string(input$var1))
      g + geom_histogram(bins = 30, aes(fill = HeartDisease)) +
        labs(title = paste("Histogram of", input$var1)) +
        scale_fill_discrete(label = c("No", "Yes"))
    } else if (input$type == 'nvar' & input$plot1 == 'box'){
      g <- ggplot(data = heart, aes_string(x = "HeartDisease", input$var1))
      g + geom_boxplot() +
        geom_jitter(aes(color = HeartDisease)) +
        labs(title = paste("Boxplot of", input$var1)) +
        scale_color_discrete(label = c("No", "Yes"))
    } else{
      g <- ggplot(data = heart, aes_string(x = input$var2))
      g + geom_bar(aes(fill = HeartDisease), position = "dodge") +
        labs(title = paste("Bar Plot of", input$var2)) +
        scale_fill_discrete(label = c("No", "Yes"))
    }
  })

  output$lgmod <- renderUI({
    withMathJax(
      helpText('$$\\log(\\frac{P(success|x)}{1-P(success|x)})=\\beta_0+\\beta_1x$$')
      )
  })
  
  output$rf <- renderUI({
    withMathJax(
      helpText('Number of predictors for Classification: $$m=\\sqrt{p}$$'),
      helpText('Number of predictors for Regression: $$m=\\frac{p}{3}$$')
    )
  })
  
  # Logistic Model Fit
  observeEvent(input$submit, {
  output$logsum <- renderPrint({
    set.seed(1)
    trainIndex <- createDataPartition(heart$HeartDisease, p = input$lgt, 
                                      list = FALSE) 
    Train <- heart[trainIndex, ]
    Test <- heart[-trainIndex, ]
    lgrecipe_formula <- reactive({
      heart %>%
        recipe() %>%
        update_role(HeartDisease,new_role = "outcome") %>%
        update_role(!!!input$logpred,new_role = "predictor") %>% 
        prep() %>%
        formula()
    })
    if (input$lgcrossv){
      fit <- train(lgrecipe_formula(),
                   data = Train,
                   method = "glm",
                   family = "binomial",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5))
      cm <- confusionMatrix(data = Test$HeartDisease, 
                      reference = predict(fit, newdata = Test))
      sum <- summary(fit)
      print(list(sum, cm))
    } else{
      fit <- train(lgrecipe_formula(),
                   data = Train,
                   method = "glm",
                   family = "binomial",
                   preProcess = c("center", "scale"))
      cm <- confusionMatrix(data = Test$HeartDisease, 
                            reference = predict(fit, newdata = Test))
      sum <- summary(fit)
      print(list(sum, cm))
    }
  })
  # Classification Tree Fit
  output$ctsum <- renderPrint({
    set.seed(1)
    trainIndex <- createDataPartition(heart$HeartDisease, p = input$ctmod, 
                                      list = FALSE) 
    Train <- heart[trainIndex, ]
    Test <- heart[-trainIndex, ]
    ctrecipe_formula <- reactive({
      heart %>%
        recipe() %>%
        update_role(HeartDisease,new_role = "outcome") %>%
        update_role(!!!input$ctpred,new_role = "predictor") %>% 
        prep() %>%
        formula()
    })
    if (input$ctcrossv){
      fit <- train(ctrecipe_formula(),
                   data = Train,
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.001))
                   )
      confusionMatrix(data = Test$HeartDisease, 
                      reference = predict(fit, newdata = Test))
    } else{
      fit <- train(ctrecipe_formula(),
                   data = Train,
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.001))
      )
      confusionMatrix(data = Test$HeartDisease, 
                      reference = predict(fit, newdata = Test))
    }
  })
  output$ctplot <- renderPlot({
    set.seed(1)
    trainIndex <- createDataPartition(heart$HeartDisease, p = input$ctmod, 
                                      list = FALSE) 
    Train <- heart[trainIndex, ]
    Test <- heart[-trainIndex, ]
    ctrecipe_formula <- reactive({
      heart %>%
        recipe() %>%
        update_role(HeartDisease,new_role = "outcome") %>%
        update_role(!!!input$ctpred,new_role = "predictor") %>% 
        prep() %>%
        formula()
    })
    if (input$ctcrossv){
      fit <- train(ctrecipe_formula(),
                   data = Train,
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.001))
      )
      fit_imp <- varImp(fit)
      plot(fit_imp)
    } else{
      fit <- train(ctrecipe_formula(),
                   data = Train,
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.001))
      )
      fit_imp <- varImp(fit)
      plot(fit_imp)
      }
  })
  # Random Forest Fit
  output$rfplot <- renderPlot({
    set.seed(1)
    trainIndex <- createDataPartition(heart$HeartDisease, p = input$rfmod, 
                                      list = FALSE) 
    Train <- heart[trainIndex, ]
    Test <- heart[-trainIndex, ]
    rfrecipe_formula <- reactive({
      heart %>%
        recipe() %>%
        update_role(HeartDisease,new_role = "outcome") %>%
        update_role(!!!input$rfpred,new_role = "predictor") %>% 
        prep() %>%
        formula()
    })
    if (input$rfcrossv){
      fit <- train(rfrecipe_formula(),
                   data = Train,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = data.frame(mtry = 1:length(input$rfpred)))
      fit_imp <- varImp(fit)
      plot(fit_imp)
    } else{
      fit <- train(rfrecipe_formula(),
                   data = Train,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(mtry = 1:length(input$rfpred)))
      fit_imp <- varImp(fit)
      plot(fit_imp)
    }
  })
  
  output$rfsum <- renderPrint({
    set.seed(1)
    trainIndex <- createDataPartition(heart$HeartDisease, p = input$rfmod, 
                                      list = FALSE) 
    Train <- heart[trainIndex, ]
    Test <- heart[-trainIndex, ]
    rfrecipe_formula <- reactive({
      heart %>%
        recipe() %>%
        update_role(HeartDisease,new_role = "outcome") %>%
        update_role(!!!input$rfpred,new_role = "predictor") %>% 
        prep() %>%
        formula()
    })
    if (input$rfcrossv){
      fit <- train(rfrecipe_formula(),
                   data = Train,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = data.frame(mtry = 1:length(input$rfpred)))
      confusionMatrix(data = Test$HeartDisease, 
                      reference = predict(fit, newdata = Test))
    } else{
      fit <- train(rfrecipe_formula(),
                   data = Train,
                   method = "rf",
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(mtry = 1:length(input$rfpred)))
      confusionMatrix(data = Test$HeartDisease, 
                      reference = predict(fit, newdata = Test))
    }
  })
  })
  df <- reactive({
    df <- data.frame(as.numeric(input$inputage),
                     input$inputsex,
                     input$inputcpt,
                     as.numeric(input$inputrbp),
                     as.numeric(input$inputchol))
    names(df) <- input$predvar
    return(df)
  })
  output$test <- renderDataTable({
    df()
  })
  

  
  output$datas <- renderDataTable({
    tab <- heart %>% select(input$alldata)
    datatable(tab, filter = "top")
  })
  output$row <- 
    renderPrint({
      input[["datas_rows_all"]]
    })
  output$download <- downloadHandler(
      filename = "Filtered Data.csv",
      content = function(file){
        write.csv(heart[input[["datas_rows_all"]], ],
                  file)})



})