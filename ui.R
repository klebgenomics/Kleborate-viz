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
#                                          upload files - fixed for now                                         #
######################## *******************************  ************************************** ################  

kleborate_data <- read.csv("kleborate_output.txt",sep="\t")
column_decoder <- read.csv("column_decoder.txt",sep="\t")
species_toggles = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola")
year_vir_res <- read.csv("mean_vir_res_by_year_jun2020.csv")
sample_vir_res <- read.csv("mean_vir_res_scores_sampletype_jun2020.csv")
ST_data <- read.csv("Kleborate_ST_vir_res_heatmap_toplot_Dec2019.csv")
bar <- read.csv("Kleborate_ST_meta_barchart_toplot_Dec2019_v2.csv")
Eu_KO <- read.csv("EuSCAPE_K_O_analysis.csv")
f7 <- read.csv("EuSCAPE-Kleborate-AMR_comparison_260819_forR.csv")

resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])


######################## *******************************  ************************************** ################
#                                            SHINY UI START                                                     #
######################## *******************************  ************************************** ################  
ui <- fluidPage(
    titlePanel(title=div(img(src="logov2.png",height=100,width=200))),
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
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                     div(style = "position:absolute;left:2em;",downloadButton(outputId = "scoreBarBySpecies_plot_download", label = "Download plot"))
            ),
            tabPanel("ST distribution",
                   plotOutput("SThist"),
                   br(),br(),br(),
                   column(6,selectInput("file", label="Colour bars by:", c("virulence_score", "resistance_score"))),
                   #c("virulence_score", "virulence_locus_columns", "resistance_score", "resistance_class_columns")),
                   br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "STdist_plot_download", label = "Download plot")),
                   br(),br(),br(),
                   column(12,wellPanel(uiOutput("numBars")))
            ),
            tabPanel("Convergence heatmap", 
                   br(),
                   plotlyOutput("heatmap"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "CovergenceHeatmap_plot_download", label = "Download plot"))
            ),
            tabPanel("Convergence by ST",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "ConvergenceST_plot_download", label = "Download plot")),
                   br(),
                   column(6, plotlyOutput("st_virulence"))
            ),
            tabPanel("F1",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F1_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f1"))
            ),
            tabPanel("F2",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F2_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),br(),br(),
                   column(6, plotlyOutput("f2"))
            ),                     
            tabPanel("F3",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F3_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   column(6, plotlyOutput("f3"))
            ),         
            tabPanel("F4",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F4_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f4"))
            ),
            tabPanel("F5",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F5_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f5"))
            ), 
            tabPanel("F6",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F6_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f6"))
            ), 
            tabPanel("F7",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F7_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f7"))
            ), 
                   tabPanel("F8",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F8_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f8"))
            ), 
                   tabPanel("F9",
                   plotlyOutput("st_scatter"),
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   div(style = "position:absolute;left:2em;",downloadButton(outputId = "F9_plot_download", label = "Download plot")),
                   br(),br(),br(),br(),
                   column(6, plotlyOutput("f9"))
            ), 
        textOutput("selected_file")
     )
    )
  )
)