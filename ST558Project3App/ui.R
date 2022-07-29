library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(caret)


# Read in data
heart <- read_csv("heart.csv")

# Select variable that Interested in
heart <- heart %>% select(Age, Sex, ChestPainType, RestingBP,
                          Cholesterol, HeartDisease)

shinyUI(dashboardPage(
  skin = "red",
  dashboardHeader(title = "ST558 Project3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Page", tabName = "about"),
      menuItem("Data Exploration Page", tabName = "eda"),
      menuItem("Modeling Page",
               menuSubItem("Modeling Info", tabName = "info"),
               menuSubItem("Model Fitting", tabName = "fit"),
               menuSubItem("Prediction", tabName = "pred")),
      menuItem("Data Page", tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              imageOutput("img"),
              box(
                textOutput("text"),
                br(),
                "The data is the", a(href="https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction", target="_blank", strong("Heart Failure Prediction Dataset")), "in kaggle. The dataset contains 1 binary response and 11 predictors.",
                br(),
                br(),
                "For this app, 5 variabls are investigated."
              ),
              box(
                "The 'About Page' is the introduction page about basic information of the project and dataset.",
                br(),
                br(),
                "The 'Data Exploration Page' is about data visualization.",
                br(),
                br(),
                "The 'Modeling Page' has three subpages about model information, model fitting and model predictions.",
                br(),
                br(),
                "The 'Data Page' can play around the data."
              )
              ),
      
      tabItem(tabName = "eda",
              radioButtons(inputId = "type",
                          label = "Type of Variables",
                          choices = list("Numeric Variable" = "nvar",
                                         "Categorical Variable" = "cvar")),
              
              conditionalPanel(condition = "input.type == 'nvar'",
                               selectInput(inputId = "var1",
                                           label = "Variables",
                                           choices = list("Age",
                                                          "RestingBP",
                                                          "Cholesterol"),
                                           selected = "Age"),
                               
                               selectInput(inputId = "plot1",
                                           label = "Type of Plot",
                                           choices = list("Histogram" = "hist",
                                                          "Box Plot" = "box"))
                               ),
              
              conditionalPanel(condition = "input.type == 'cvar'",
                               selectInput(inputId = "var2",
                                           label = "Variables",
                                           choices = list("Sex",
                                                          "ChestPainType",
                                                          "HeartDisease"),
                                           selected = "HeartDisease"),
                               
                               selectInput(inputId = "plot2",
                                           label = "Type of Plot",
                                           choices = list("Bar Plot" = "bar"))
                               ),
              plotOutput("plots"),
              dataTableOutput("table")
              ),
      
      tabItem(tabName = "info",
              box(
                title = "Logistic Regression Model",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                p(strong("Logistic Regression"), "is an extension of linear regression. It is used to describe and explain the relationship between one", code("binary response"), "and independent variables. It is used when we want to predict a categorical variable based on a set of independent variables."
              ),
              p("Baisc Logistic Regression models success probability using", strong(em("logistic function")), "Since logistic regression model doesn't have a closed form solution, backsolving shows the logit or log-odds of success is linear in the parameters."),
              uiOutput("lgmod"),
              h5(strong("Benefits:")),
              p("1. Logistic regressionn is very useful when we want to predict a binary response.", br(), "2. It is easier to implement, interpret, and very efficient to train.", br(), "3. In low dimensional dataset, it is less likely to over-fitting"),
              h5(strong("Drawbacks:")),
              p("1. The response should be binary.", br(), "2. Observation should be large and indepdently distributed.", br(), "3. Predictors have little or no multicollinearity and the model need to be the linear form."),
              ),
              
              box(
                title = "Classification Tree Model",
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                h4("Tree based method:"),
                p("Split up predictor space into regions, different predictions for each region.", br(), strong("Classification tree"), "if goal is to classify(predict) group menbership - always use most prevalent class as prediction for a given region. Usually use", code("Gini index"), "or", code("entropy/deviance"),".", strong("Regression tree"), "if goal is to predict a continuous response - always use mean of observation as prediction for a given region. Use", code("resursive binary splitting"), "."),
                h5(strong("Benefits:")),
                p("1. Simple to understand and easy to interpret output.", br(),"2. Predictors don't need to be scaled.", br(), "3. No statistical assuptions necessary.", br(), "4. Built in variable selection."),
                h5(strong("Drawbacks:")),
                p("1. Small changes in data can vastly change tree.", br(), "2. Greedy algorithm necessary.", br(), "3. Need to prune.")
              ),
              
              box(
                title = "Random Forest Model",
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                strong("Basic intro of Bagged tree:"),
                p("The", strong("bootstrapping"), "is that we resample from data or a fitted model and apply method or estimation to each resample. We see how the model or method behave.", br(), "For Bootstrap Aggregation(Bagged) for classification tree:", br(), "1. Create a bootstrap sample.", br(), "2. Train tree on the sample.", br(), "3. Repeat B = 1000 times.", br(), "4. Use majority vote as final classification prediction."),
                strong("Idea of Random Forest:"),
                p("Random forest follows Bootstrap Aggregation idea. We will create multiple trees from bootstrap samples and average the results. But, we will use a random subset of predictors for each bootstrap tree fit instead of using all predictors."),
                uiOutput("rf"),
                h5(strong("Benefits:")),
                p("1. Decreases variance over an individual tree fit.", br(), "2. Prevents a good predictors or two from dominaating the tree fits."),
                h5(strong("Drawbacks:")),
                p("1. Loses interpretability.", br(), "2. Takes long computation time.")
                )
              ),
      
      tabItem(tabName = "fit",
              # Logistic Model
              tabBox(title = "Model Fit",
                     id = "tab1",
                     # Logistic Regression Model
                     tabPanel(
                       title = "Logistic Regression Model",
                       numericInput(inputId = "lgt",
                                    label = "Proportion for Training",
                                    value = 0.7,
                                    min = 0.1,
                                    max = 1,
                                    step = 0.1),
                       selectInput(
                         inputId = "logpred",
                         label = "Select variables:",
                         choices = names(heart %>% select(-HeartDisease)),
                         multiple = TRUE,
                         selected = "Age"
                         ),
                       h6(strong("Use 5-fold Cross Validation?")),
                       checkboxInput(inputId = "lgcrossv",
                                     label = "Yes",
                                     value = TRUE
                                     )
                     ),
                     # Classification Tree Model
                     tabPanel(
                       title = "Classification Tree Model",
                       numericInput(inputId = "ctmod",
                                    label = "Proportion for Training",
                                    value = 0.7,
                                    min = 0.1,
                                    max = 1,
                                    step = 0.1),
                       selectInput(
                         inputId = "ctpred",
                         label = "Select variables:",
                         choices = names(heart %>% select(-HeartDisease)),
                         multiple = TRUE,
                         selected = "Age"
                         ),
                       h6(strong("Use 5-fold Cross Validation?")),
                       checkboxInput(inputId = "ctcrossv",
                                     label = "Yes",
                                     value = TRUE
                                     )
                     ),
                     # Random Forest Model
                     tabPanel(
                       title = "Random Forest Model",
                       numericInput(inputId = "rfmod",
                                    label = "Proportion for Training",
                                    value = 0.7,
                                    min = 0.1,
                                    max = 1,
                                    step = 0.1),
                       selectInput(
                         inputId = "rfpred",
                         label = "Select variables:",
                         choices = names(heart %>% select(-HeartDisease)),
                         multiple = TRUE,
                         selected = "Age"
                         ),
                       h6(strong("Use 5-fold Cross Validation?")),
                       checkboxInput(inputId = "rfcrossv",
                                     label = "Yes",
                                     value = TRUE
                                     )
                     )
                     ),
              actionButton(inputId = "submit",
                           label = strong("Fit Model !")),
              tabBox(title = "Model Summary",
                     id = "tab2",
                     tabPanel(
                       title = "Logistic Regression Model",
                       verbatimTextOutput("logsum")
                     ),
                     tabPanel(
                       title = "Classification Tree Model",
                       plotOutput("ctplot"),
                       verbatimTextOutput("ctsum")
                     ),
                     tabPanel(
                       title = "Random Forest Model",
                       plotOutput("rfplot"),
                       verbatimTextOutput("rfsum")
                     ))
              
              ),
      
      tabItem(tabName = "pred",
              box(
                title = "Model",
                status = "danger",
                  radioButtons(inputId = "modtype",
                           label = strong("Choose your Model!"),
                           choices = list("Logistic Regression Model" = "logit",
                                          "Classification Tree Model" = "classtr",
                                          "Random Forest Model" = "randfmod")),
                selectInput(
                  inputId = "predvar",
                  label = "Select variables:",
                  choices = names(heart %>% select(-HeartDisease)),
                  multiple = TRUE,
                  selected = "Age"
                ),
                conditionalPanel(
                  condition = "input.predvar.includes('Age')",
                  textInput(
                    inputId = "inputage",
                    label = "Enter the Age:"
                  )),
                conditionalPanel(
                  condition = "input.predvar.includes('Sex')",
                  textInput(
                    inputId = "inputsex",
                    label = "Enter the Sex:"
                  )),
                conditionalPanel(
                  condition = "input.predvar.includes('ChestPainType')",
                  textInput(
                    inputId = "inputcpt",
                    label = "Enter the ChestPainType:"
                  )),
                conditionalPanel(
                  condition = "input.predvar.includes('RestingBP')",
                  textInput(
                    inputId = "inputrbp",
                    label = "Enter the RestingBP:"
                  )),
                conditionalPanel(
                  condition = "input.predvar.includes('Cholesterol')",
                  textInput(
                    inputId = "inputchol",
                    label = "Enter the Cholesterol:"
                  ))
                
                

              ),
              box(
                title = "Prediction",
                verbatimTextOutput("userpred")
              )
              ),
      
      tabItem(tabName = "data",
              box(
                title = "Data",
                selectInput(
                  inputId = "alldata",
                  label = "Select variables:",
                  choices = names(heart),
                  multiple = TRUE,
                  selected = names(heart)
                  ),
                downloadButton(outputId = "download",
                               label = "Download Filtered Data")
                ),
                box(
                  width = 12,
                  title = "Date for Corresponding Variables",
                  dataTableOutput("datas"),
                  verbatimTextOutput("row")
              )
              )
    )
  )
))

