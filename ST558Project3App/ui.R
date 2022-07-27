library(shinydashboard)
library(shiny)
library(tidyverse)

heart <- read_csv("heart.csv")

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
                "The data is the", a(href="https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction", target="_blank", strong("Heart Failure Prediction Dataset")), "in kaggle. The dataset contains 1 binary response and 11 predictors."
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
              h2("Eda page")
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

