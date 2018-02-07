library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(title=div(img(src="logo.png",height=100,width=200),align="center")),
  
  tabsetPanel(
    tabPanel("Resistance Score", plotOutput("distPlot")), 
    tabPanel("Virulence Score", plotOutput("distPlot2")), 
    tabPanel("ST Distribution",      
      fluidRow(
        column(4,wellPanel(sliderInput(inputId = "bins", label = "Number of bars:",min = 1,max = nlevels(kleborate_data$ST),step =1,
                value = nlevels(kleborate_data$ST)))),
        column(8,plotOutput("distPlot3"))
        
    )),
    tabPanel("Heat Map",
             plotOutput("heatmap"))
    
    )
  )
  
  # fluidRow(
  #   plotOutput("distPlot")
  # ),
  # 
  # fluidRow(
  #   plotOutput("distPlot2")
  # ),
  # 
  # fluidRow(
  #   
  #   column(4,
  #          wellPanel(
  #            sliderInput(inputId = "bins",
  #                                        label = "Number of bars:",
  #                                        min = 1,
  #                                        max = nlevels(kleborate_data$ST), step =1,
  #                                        value = nlevels(kleborate_data$ST)
  #                                      )
  #          )       
  #   ),
  #   
  #   column(8,
  #          plotOutput("distPlot3")
  #   )
  # ),
  # 
  # 
  # fluidRow(
  #   plotOutput("heatmap")
  # )
  # 
  
  # # Sidebar layout with input and output definitions ----
  # sidebarLayout(
  #   
  #   
  #   # Sidebar panel for inputs ----
  #    sidebarPanel(
  #     
  #     #Input: Slider for the number of bins ----
  #     sliderInput(inputId = "bins",
  #                 label = "Number of bars:",
  #                 min = 1,
  #                 max = nlevels(kleborate_data$ST), step =1, 
  #                 value = nlevels(kleborate_data$ST)
  #               )
  # 
  #   ),
  #   # Main panel for displaying outputs ----
  #   mainPanel(
  #     
  #     # Output: Histogram ----
  #     plotOutput(outputId = "distPlot"),
  #     plotOutput(outputId = "distPlot2"),
  #     plotOutput(outputId = "distPlot3"),
  #     plotlyOutput("heatmap")
  #     
  #   )
  # )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
   # x    <- faithful$waiting
  #  bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
  #interactive bar graph
  #ggplot(data=faithful, aes(faithful$waiting)) + 
   # geom_histogram(breaks=bins)
  
  #resistance score
  ggplot(kleborate_data, aes(x = as.factor(resistance_score), fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_brewer(palette = "Accent") + ylab("Number of isolates") + xlab("Resistance score") + labs(fill = "Species")
  
  })
  
  output$distPlot2 <- renderPlot({
    #virulence score
    ggplot(kleborate_data, aes(x = virulence_score, fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_brewer(palette = "Accent") + ylab("Number of isolates") + xlab("Virulence score") + labs(fill = "Species")  
  })
  
  #ST histogram
  output$distPlot3 <- renderPlot({
    ggplot(kleborate_data, aes(x=reorder(ST,ST,function(x)-length(x)))) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0))+ scale_x_discrete(limits = (levels(reorder(kleborate_data$ST,kleborate_data$ST,function(x)-length(x)))[1:input$bins]))
  })
  
  output$heatmap <- renderPlot({
    vir_res <- table(factor(kleborate_data$virulence_score,c(0,1,2,3)),factor(kleborate_data$resistance_score,c(0,1,2)))
    vir_res <- as.data.frame.matrix(vir_res)
    vir_res$vir <- rownames(vir_res)
    vir_res_long <- melt(vir_res)
    ggplot(vir_res_long, aes(x = vir, y = variable)) + geom_tile(aes(fill=value)) + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank()) + ylab("Resistance score") + xlab("Virulence score") + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + labs(fill = "Number of\nisolates") + scale_fill_gradient(low = "#ffffff", high = "#08306b")
  })
  
}

shinyApp(ui = ui, server = server)
