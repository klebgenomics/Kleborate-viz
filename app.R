library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)

#Input data
kleborate_data <- read.csv("kleborate_viz_test_data_mixedSTs.txt",sep="\t")

# Making toggle list
all_species = levels(kleborate_data$species)
species_toggles = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola","Others")

column_decoder <- read.csv("column_decoder.txt",sep="\t")
resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])

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
    
    tabPanel("Resistance Score",
             br(), checkboxGroupInput("res_species_toggle", label = "Toggle species", selected = species_toggles, choices = species_toggles), 
             br(), plotOutput("ResistancePlot")),
    
    tabPanel("Virulence Score",
             br(), checkboxGroupInput("vir_species_toggle", label = "Toggle species", selected = species_toggles, choices = species_toggles), 
             br(), plotOutput("VirulencePlot")), 
    

    tabPanel("ST Distribution",
             plotOutput("SThist"),
             column(6,selectInput("variable", label="Colour bars by:",
                                c("virulence_score", virulence_locus_columns, "resistance_score", resistance_class_columns))),
             column(6,wellPanel(sliderInput(inputId = "bars", label = "Number of bars:",min = 1,max = nlevels(kleborate_data$ST),step =1,value = min(20,nlevels(kleborate_data$ST)))))
    ),

    tabPanel("Heat Map", plotOutput("heatmap")),
    tabPanel("Scatter plot", plotlyOutput("scatter"))
  )
)

# Define server logic for app
server <- function(input, output) {
  #Upload input data
  KleborateData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(kleborate_data)
    data <- read.csv(inFile$datapath,sep="\t")
    data
  })


  #Generate colours for plots (based on logo colours)
  species_cols <- colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(kleborate_data$species))
  
  #Resistance score plot
  res_filtered=reactive({
    filter = input$res_species_toggle
    if ('Klebsiella quasipneumoniae' %in% filter) { filter = c(filter, grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE)) }
    if ('Others' %in% filter) { filter = c(filter, all_species[!all_species %in% c(filter,grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE),species_toggles)]) }
    return(kleborate_data[kleborate_data$species%in%filter,])
  })
  output$ResistancePlot <- renderPlot({
    ggplot(data=res_filtered(), aes(x = as.factor(resistance_score), fill = species)) + 
      geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), 
                         axis.text.y = element_text(colour = "black", size = 12), 
                         axis.title = element_text(colour = "black", size = 14), 
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         axis.line = element_line(colour = "black")) + 
      scale_y_continuous(expand=c(0,0)) + 
      scale_fill_manual(values = species_cols) + 
      ylab("Number of isolates") + 
      xlab("Resistance score") + 
      labs(fill = "Species")
    
  })
  
  #Virulence score plot
  vir_filtered=reactive({
    filter = input$vir_species_toggle
    if ('Klebsiella quasipneumoniae' %in% filter) { filter = c(filter, grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE)) }
    if ('Others' %in% filter) { filter = c(filter, all_species[!all_species %in% c(filter,grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE),species_toggles)]) }
    return(kleborate_data[kleborate_data$species%in%filter,])
  })
  output$VirulencePlot <- renderPlot({
    ggplot(data=vir_filtered(), aes(x = as.factor(virulence_score), fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_manual(values = species_cols) + ylab("Number of isolates") + xlab("Virulence score") + labs(fill = "Species")  
  })

  #Sequence type histogram (interactive)

  # define colour schemes and text for virulence/resistance
  virulence_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#c6dbef", "#6baed6", "#2171b5", "#08519c", "#08306b"), name = "Virulence score", labels = c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc"))
  resistance_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#fcbba1", "#fb6a4a", "#cb181d"), name = "Resistance score", labels = c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbepenemase +ve", "3: Carbapenemase +ve and colisitin resistance"))


  output$SThist <- renderPlot({

    variable_to_stack = kleborate_data[, input$variable]

    if(input$variable == "virulence_score"){
      cols <- c("#deebf7", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08306b")
      labels <- c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc")
      name <- "Virulence score"
    }
    else if(input$variable == "resistance_score"){
      cols <- c("#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#67000d")
      labels <- c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbapenemase +ve", "3: Carbapenemase +ve and colistin resistance")
      name <- "Resistance score"
    }
	# individual genes
 	else {
      variable_to_stack <- (kleborate_data[, input$variable] != "-") *1 #turn this into a binary
      cols <- c("#ffffff", "grey")
      labels <- c("0: absent", "1: present")
      name <- as.character(column_decoder$display.name[column_decoder$column_name ==input$variable])
 	}


    ggplot(kleborate_data, aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0))+ scale_x_discrete(limits = (levels(reorder(kleborate_data$ST,kleborate_data$ST,function(x)-length(x)))[1:input$bars])) + scale_fill_manual(values = cols, labels=labels, name=name)
    })

  #Heat map (interactive)
  output$heatmap <- renderPlot({
    vir_res <- table(factor(KleborateData()$virulence_score,c(0,1,2,3)),factor(KleborateData()$resistance_score,c(0,1,2)))
    vir_res <- as.data.frame.matrix(vir_res)
    vir_res$vir <- rownames(vir_res)
    vir_res_long <- melt(vir_res)
    ggplot(vir_res_long, aes(x = vir, y = variable)) + geom_tile(aes(fill=value)) + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank()) + ylab("Resistance score") + xlab("Virulence score") + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + labs(fill = "Number of\nisolates") + scale_fill_gradient(low = "#ffffff", high = "#08306b")
  })

  # ST tab plots
  st_data <- kleborate_data %>% group_by(ST) %>% summarise(mean_vir = mean(virulence_score), mean_res = mean(resistance_score), total  = n())
  output$st_scatter <- renderPlotly({
    # Create scatterplot
    p <- plot_ly(source="st_scatter") %>%
      add_trace(data=st_data, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~log(total, 2)*4, opacity=0.5), name=" ") %>%
      layout(showlegend = FALSE)

    # Add new trace with coloured point if there is event data
    ed <- event_data("plotly_click", source="st_scatter")
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- st_data[ed$pointNumber+1, ]
      p <- p %>% add_trace(data=selected_st, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~log(total, 2)*4, opacity=0.5), name=" ")
    }

    return(p)
  })

  # TODO: swap placeholder for real plot
  output$st_virulence <- renderPlotly({
    ed <- event_data("plotly_click", source="st_scatter")
    st <- st_data$ST[ed$pointNumber]
    st_data <- kleborate_data[kleborate_data$ST==st, ]
    plot_ly(data=st_data, x=~strain, y=~contig_count, type='bar')
  })

  # TODO: swap placeholder for real plot
  output$st_resistance <- renderPlotly({
    ed <- event_data("plotly_click", source="st_scatter")
    st <- st_data$ST[ed$pointNumber]
    st_data <- kleborate_data[kleborate_data$ST==st, ]
    plot_ly(data=st_data, x=~strain, y=~contig_count, type='bar')
  })
}


#Load shiny app
shinyApp(ui = ui, server = server)
