
library(shiny)
library(leaflet)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    theme = "cgdd.css",
    title = "Visualisation maillage logement",
    tabPanel(title = "Accueil",
             includeMarkdown("text_appli.md")),
    tabPanel(title = "Visualisation maille seule",
             fluidRow(
               column(4,uiOutput("lst_fact3")),
               column(4,uiOutput("lst_lim3")),
               column(4,uiOutput("lst_zon3"))
             ),
             fluidRow(
               # column(12, plotOutput("maillestat"), textOutput("comptage3"))
               column(12, leafletOutput("mailledyn"), textOutput("comptage3"))
             )
    ),
    tabPanel(title = "Indicateurs"
             # ,
             # fluidRow(
             #   column(6,uiOutput("lst_fact3")),
             #   column(6,uiOutput("lst_lim3"))
             # ),
             # fluidRow(
             #   column(12,plotOutput("maillestat"),textOutput("comptage3"))
             # )
    ),
    tabPanel(title = "Données",
             downloadButton('downloadData', 'Télécharger les données de la Maille Logement')
             # fluidRow(
             #   column(6,uiOutput("lst_fact3")),
             #   column(6,uiOutput("lst_lim3"))
             # ),
             # fluidRow(
             #   column(12,plotOutput("maillestat"),textOutput("comptage3"))
             # )
    )
  )
)
