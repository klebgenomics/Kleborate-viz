# This is the UI  for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)
library(vegan)
library(ComplexHeatmap)
library(readxl)
library(pheatmap)
library(ggrepel)
library(shinythemes)
library(RColorBrewer)


######################## *******************************  ************************************** ################
#                                            SHINY UI START                                                     #
######################## *******************************  ************************************** ################  
ui <- fluidPage(
    titlePanel(title=div(img(src="logo.png",height=100,width=200))),
sidebarLayout(    
    sidebarPanel( position =c("left"),  style = "color:#337ab7", 
        fileInput("file", "Load Kleborate output file", 
                  accept = c(".txt")),
        #selectInput("col", "Column", choices = unique(colnames(input$file)
        #)),
        fluidRow(
          column(6,
                 selectInput("select-input",label="Select country or region",choices=c("A","B","C")),
                 sliderInput("slider-input",label="Numbers of resistance classes",value=5,min=1,max=10),
                 dateRangeInput("date-range-input",label="Period"),
                 hr(),
                 submitButton(),
          ),
        ),
       br(),
       h4("Data Summary"),
       tableOutput("summaryTable"),
       br(),
       checkboxGroupInput("species_toggle", label = "Species", choices = c("Klebsiella pneumoniae" = "1","Klebsiella quasipneumoniae" = "2","Klebsiella variicola" = "3","Klebsiella quasivariicola" = "4","Others" = "5"),selected = "" )
       ),
      mainPanel(
        tabsetPanel(
        tabPanel("Summary",
                   br(),
                     plotOutput("resScoreBarBySpecies", height="100px"),
                     br(),
                     plotOutput("virScoreBarBySpecies", height="200px"),
                     br(),br(),br(),br(),
                     div(style = "position:absolute;left:2em;",downloadButton(outputId = "scoreBarBySpecies_plot_download", label = "Download plot"))
            ),
            tabPanel("ST distribution",
                   plotOutput("SThist"),
                   column(6,selectInput("file", label="Colour bars by:", c("virulence_score", "resistance_score"))),
                   #c("virulence_score", "virulence_locus_columns", "resistance_score", "resistance_class_columns")),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "STdist_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6,wellPanel(uiOutput("numBars")))
            ),
            tabPanel("Convergence heatmap", 
                   br(),
                   plotlyOutput("heatmap"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "CovergenceHeatmap_plot_download", label = "Download plot"))
            ),
            tabPanel("Convergence by ST",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "ConvergenceST_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("st_virulence")
            ),
            tabPanel("F1",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F1_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f1")
            ),
            tabPanel("F2",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ),                     
            tabPanel("F3",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ),         
            tabPanel("F4",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ),
            tabPanel("F5",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ), 
            tabPanel("F6",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ), 
            tabPanel("F7",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ), 
                   tabPanel("F8",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ), 
                   tabPanel("F4",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f2")
            ), 
        #textOutput("selected_file")
        ))))))))))
      )
    )
  )
)