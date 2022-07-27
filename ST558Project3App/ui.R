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
              h2("model info")
              ),
      
      tabItem(tabName = "fit",
              h2("model fit")
              ),
      
      tabItem(tabName = "pred",
              h2("model prediction")
              ),
      
      tabItem(tabName = "data",
              h2("data page")
              )
    )
  )
))

