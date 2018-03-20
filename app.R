library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)

#TO DO: ALEX'S TOGGLE LISTS NEED TO BE MADE REACTIVE ie update with new input data

#Input data
kleborate_data <- read.csv("kleborate_viz_test_data_mixedSTs.txt",sep="\t")

# Making toggle list
all_species = levels(kleborate_data$species)
species_toggles = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola")

column_decoder <- read.csv("column_decoder.txt",sep="\t")
resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])

# Define UI for Shiny App: Kleborate Visualiser
ui <- fluidPage(

  # App title
  titlePanel(title=div(img(src="logo.png",height=100,width=200),align="center"), windowTitle='Kleborate'),

  # Tab layout
  tabsetPanel(
    tabPanel("Summary",
             fluidRow(
               column(4,fileInput('file', 'Choose Input Data (csv file)',accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv'))),
               column(8,tableOutput('summaryTable'))
             )),
    
    tabPanel("Resistance scores by species",
             br(), checkboxGroupInput("res_species_toggle", label = "Toggle species", selected = species_toggles, choices = c(species_toggles, "Others")), 
             br(), plotOutput("ResistancePlot")),
    
    tabPanel("Virulence scores by species",
             br(), checkboxGroupInput("vir_species_toggle", label = "Toggle species", selected = species_toggles, choices = c(species_toggles, "Others")), 
             br(), plotOutput("VirulencePlot")), 
    

    tabPanel("ST distribution",
    		downloadButton(outputId = "STdist_plot_download", label = "Download the plot"),
             plotOutput("SThist"),
             column(6,selectInput("variable", label="Colour bars by:",
                                c("virulence_score", virulence_locus_columns, "resistance_score", resistance_class_columns))),
             column(6,wellPanel(uiOutput("numBars"))))
    ,

    tabPanel("Convergence heatmap", 
             br(), plotlyOutput("heatmap")),
    tabPanel("Convergence by ST",
             plotlyOutput("st_scatter"),
             column(6, plotlyOutput("st_virulence")),
             column(6, plotlyOutput("st_resistance")))
  )
  
)

# Define server logic for app
server <- function(input, output) {
  #Upload input data
  KleborateData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(kleborate_data)
    data <- read.csv(inFile$datapath,sep="\t")
    return(data)
  })
  
  output$numBars <- renderUI({
    sliderInput(inputId = "bars", label = "Number of bars:",min = 1,max = nlevels(KleborateData()$ST),step =1,
                value = min(20,nlevels(KleborateData()$ST)))
  })

  #Summary table: num species, # STs, mean vir, mean resistance
  output$summaryTable <- renderTable(sumTable())
  
  sumTable <- reactive({
    vs <- c("Mean virulence score",round(mean(KleborateData()$virulence_score),2))
    vr <- c("Mean resistance score",round(mean(KleborateData()$resistance_score),2))
    us <- c("Total unique species",nlevels(KleborateData()$species))
    st <- c("Total STs",nlevels(KleborateData()$ST))
    sum_table <- t(data.frame(us,st,vs,vr))
    return(sum_table)
  })    

  #Resistance score plot
  res_filtered=reactive({
    species_cols <- colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(KleborateData()$species))
    filter = input$res_species_toggle
    if ('Klebsiella quasipneumoniae' %in% filter) { filter = c(filter, grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE)) }
    if ('Others' %in% filter) { filter = c(filter, all_species[!all_species %in% c(filter,grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE),species_toggles)]) }
    return(KleborateData()[KleborateData()$species%in%filter,])
  })
  output$ResistancePlot <- renderPlot({
    species_cols <- colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(KleborateData()$species))
    
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
    return(KleborateData()[KleborateData()$species%in%filter,])
  })
  output$VirulencePlot <- renderPlot({
  species_cols <- colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(KleborateData()$species))
   ggplot(data=vir_filtered(), aes(x = as.factor(virulence_score), fill = species)) + geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0)) + scale_fill_manual(values = species_cols) + ylab("Number of isolates") + xlab("Virulence score") + labs(fill = "Species")  
  })

  #Sequence type histogram (interactive)

  # define colour schemes and text for virulence/resistance
  virulence_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#c6dbef", "#6baed6", "#2171b5", "#08519c", "#08306b"), name = "Virulence score", labels = c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc"))
  resistance_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#fcbba1", "#fb6a4a", "#cb181d"), name = "Resistance score", labels = c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbepenemase +ve", "3: Carbapenemase +ve and colisitin resistance"))




SThist_reactive <- reactive({
  

    variable_to_stack = KleborateData()[, input$variable]


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

      variable_to_stack <- (KleborateData()[, input$variable] != "-") *1 #turn this into a binary
      cols <- c("grey", "#67000d")
      labels <- c("0: absent", "1: present")
      name <- as.character(column_decoder$display.name[column_decoder$column_name == input$variable])
 	}

    ggplot(KleborateData(), aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
      geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), 
                         axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), 
                         panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
      ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0)) +
      scale_x_discrete(limits = (levels(reorder(KleborateData()$ST,KleborateData()$ST,function(x)-length(x)))[1:input$bars])) + 
      scale_fill_manual(values = cols, labels=labels, name=name)
    })
    
 output$SThist <- renderPlot ({
 
 	print(SThist_reactive())
 
 })
 
 output$STdist_plot_download <- downloadHandler(
 	filename = function() {"ST_distribution.pdf"},
 	content = function(file) {
 		pdf(file, width = 10, height = 6)
 		print(SThist_reactive())
 		dev.off()
 	}
 )

  #Heat map (interactive)
  output$heatmap <- renderPlotly({
    # create colour palette
    cols <- colorRampPalette(c("#f5ecd1", "#f1c280", "#e67d77"))(100)
    cols <- c("#ffffff", cols)
    
    # format data
    vir_res <- table(factor(KleborateData()$resistance_score,c(0,1,2,3)),factor(KleborateData()$virulence_score,c(0,1,2,3,4,5))) # create dataframe summary of res and vir scores
    vir_res <- as.data.frame.matrix(vir_res) # convert to matrix
    vir_res <- vir_res[ order(-as.numeric(row.names(vir_res))),] # reorder rows - descending
    
    # draw plot
    heatmaply(vir_res, Rowv=NULL, Colv=NULL, ylab = "Resistance score", xlab = "Virulence score", fontsize_row = 12, fontsize_col = 12, subplot_margin = 3, colors = cols, margins = c(40,40), revR=TRUE, key.title = "# genomes", column_text_angle = 0)
    
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
    st_data <- KleborateData()[KleborateData()$ST==st, ]
    plot_ly(data=st_data, x=~strain, y=~contig_count, type='bar')
  })

  # TODO: swap placeholder for real plot
  output$st_resistance <- renderPlotly({
    ed <- event_data("plotly_click", source="st_scatter")
    st <- st_data$ST[ed$pointNumber]
    st_data <- KleborateData()[KleborateData()$ST==st, ]
    plot_ly(data=st_data, x=~strain, y=~contig_count, type='bar')
  })
}


#Load shiny app
shinyApp(ui = ui, server = server)


