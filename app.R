library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)

#Input data
kleborate_data <- read.csv("kleborate_viz_test_data_mixedSTs.txt",sep="\t")


# Define UI for Shiny App: Kleborate Visualiser
ui <- fluidPage(
  
  # App title
  titlePanel(title=div(img(src="logo.png",height=100,width=200),align="center")),
  
  # Tab layout
  tabsetPanel(
    tabPanel("Resistance Score", plotOutput("ResistancePlot")), 
    tabPanel("Virulence Score", plotOutput("VirulencePlot")), 
    tabPanel("ST Distribution",      
      fluidRow(
        column(4,wellPanel(sliderInput(inputId = "bars", label = "Number of bars:",min = 1,max = nlevels(kleborate_data$ST),step =1,
                value = min(20,nlevels(kleborate_data$ST)))),
        column(8,plotOutput("SThist"))
    )),
    tabPanel("Heat Map", plotOutput("heatmap"))
    )
  )
)

# Define server logic for app
server <- function(input, output) {
  
  #Generate colours for plots (based on logo colours)
  species_cols <- colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(kleborate_data$species))
  
  #Resistance score plot
  output$ResistancePlot <- renderPlot({
  ggplot(kleborate_data, aes(x = as.factor(resistance_score), fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_manual(values = species_cols) + ylab("Number of isolates") + xlab("Resistance score") + labs(fill = "Species")
  
  })
  
  #Virulence score plot
  output$VirulencePlot <- renderPlot({
  ggplot(kleborate_data, aes(x = virulence_score, fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_manual(values = species_cols) + ylab("Number of isolates") + xlab("Virulence score") + labs(fill = "Species")  
  })
  
  #Sequence type histogram (interactive)
  output$SThist <- renderPlot({
    ggplot(kleborate_data, aes(x=reorder(ST,ST,function(x)-length(x)))) + geom_bar(fill = "#F1C280") + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0))+ scale_x_discrete(limits = (levels(reorder(kleborate_data$ST,kleborate_data$ST,function(x)-length(x)))[1:input$bars]))
  })
  
  #Heat map (interactive)
  output$heatmap <- renderPlot({
    vir_res <- table(factor(kleborate_data$virulence_score,c(0,1,2,3)),factor(kleborate_data$resistance_score,c(0,1,2)))
    vir_res <- as.data.frame.matrix(vir_res)
    vir_res$vir <- rownames(vir_res)
    vir_res_long <- melt(vir_res)
    ggplot(vir_res_long, aes(x = vir, y = variable)) + geom_tile(aes(fill=value)) + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank()) + ylab("Resistance score") + xlab("Virulence score") + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + labs(fill = "Number of\nisolates") + scale_fill_gradient(low = "#ffffff", high = "#08306b")
  })
  
}

#Load shiny app
shinyApp(ui = ui, server = server)
