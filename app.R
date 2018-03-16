library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)

#Input data
kleborate_data <- read.csv("kleborate_viz_test_data_mixedSTs.txt",sep="\t")


  
# Define UI for Shiny App: Kleborate Visualiser
ui <- fluidPage(
  
  # App title
  titlePanel(title=div(img(src="logo.png",height=100,width=200),align="center")),
  
  # Tab layout
    tabsetPanel(
    tabPanel("Summary",
      fluidRow(
        column(4,fileInput('file', 'Choose Input Data (csv file)',accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')))
                                #column(8,tableOutput('KleborateSummary'))
    )),
    tabPanel("Resistance Score", plotOutput("ResistancePlot")), 
    tabPanel("Virulence Score", plotOutput("VirulencePlot")), 
    tabPanel("ST Distribution",      
      fluidRow(
        column(4,wellPanel(sliderInput(inputId = "bars", label = "Number of bars:",min = 1,max = nlevels(kleborate_data$ST),step =1,
                value = min(20,nlevels(kleborate_data$ST)))),
        br(),
        selectInput("variable", "Colour bars by:",
                c("Virulence score" = "virulence_score", "Resistance score" = "reistance_score")),
        column(8,plotOutput("SThist"))
    ))),
    tabPanel("Heat Map", plotOutput("heatmap")),
    tabPanel("Scatter plot", plotlyOutput("scatter"))
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
  
  # define colour schemes and text for virulence/resistance
  virulence_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#c6dbef", "#6baed6", "#2171b5", "#08519c", "#08306b"), name = "Virulence score", labels = c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc"))
  resistance_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#fcbba1", "#fb6a4a", "#cb181d"), name = "Resistance score", labels = c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbepenemase +ve", "3: Carbapenemase +ve and colisitin resistance"))

  
  output$SThist <- renderPlot({
    ggplot(kleborate_data, aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(paste("kleborate_data$", input$variable, sep = ""))) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0))+ scale_x_discrete(limits = (levels(reorder(kleborate_data2$ST,kleborate_data2$ST,function(x)-length(x))))) + paste(input$variable, "_scale_fill_manual", sep = ""))
    })
    #ggplot(kleborate_data, aes(x=reorder(ST,ST,function(x)-length(x)))) + geom_bar(fill = "#F1C280") + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0))+ scale_x_discrete(limits = (levels(reorder(kleborate_data$ST,kleborate_data$ST,function(x)-length(x)))[1:input$bars]))
  
  #Heat map (interactive)
  output$heatmap <- renderPlot({
    vir_res <- table(factor(kleborate_data$virulence_score,c(0,1,2,3)),factor(kleborate_data$resistance_score,c(0,1,2)))
    vir_res <- as.data.frame.matrix(vir_res)
    vir_res$vir <- rownames(vir_res)
    vir_res_long <- melt(vir_res)
    ggplot(vir_res_long, aes(x = vir, y = variable)) + geom_tile(aes(fill=value)) + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank()) + ylab("Resistance score") + xlab("Virulence score") + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + labs(fill = "Number of\nisolates") + scale_fill_gradient(low = "#ffffff", high = "#08306b")
  })
  
  # Scatter plot
  output$scatter <- renderPlotly({
    kleb_scatter <- kleborate_data %>% group_by(ST) %>% summarise(mean_vir = mean(virulence_score), mean_res = mean(resistance_score), total  = n())
    #ggplot(kleb_scatter, aes(x=mean_vir, y=mean_res, size = total)) + geom_point()
    plot_ly(data=kleb_scatter, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~log(total, 2)*4, opacity=0.5))
  })
  
}

#Load shiny app
shinyApp(ui = ui, server = server)
