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
species_toggles = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola")

column_decoder <- read.csv("column_decoder.txt",sep="\t")
resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])

# Define UI for Shiny App: Kleborate Visualiser
ui <- fluidPage(
	
  # App title
  #titlePanel(title=div(img(src="logo.png",height=100,width=200),align="center"), windowTitle='Kleborate'),

  # common side bar to all plots
  sidebarLayout(
  
  # side bar is where we load data, show summary, and choose species
  	sidebarPanel(
  		div(img(src="logo.png",height=100,width=200)),
  		br(),
		fileInput('file', 'Load Kleborate Output File (txt)',accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
		br(),
		h4("Data Summary"),
		tableOutput('summaryTable'),
		br(),
		checkboxGroupInput("species_toggle", label = "Toggle species", selected = species_toggles, 
      		choices = c(species_toggles, "Others"))
    ),
  
  # main panel has a selection of tabsets to plot different analyses
  mainPanel(
    # Tab layout
    tabsetPanel(
		tabPanel("Summary",
    				br(),
      				plotOutput("resScoreBarBySpecies", height="200px"),
      				#downloadButton(outputId = "resScoreBarBySpecies_plot_download", label = "Download plot"),
      				br(),
      				plotOutput("virScoreBarBySpecies", height="200px"),
      				br(),
      				div(style = "position:absolute;right:1em;",downloadButton(outputId = "scoreBarBySpecies_plot_download", label = "Download plots"))
    	),
    	tabPanel("ST distribution",
             plotOutput("SThist"),
             column(6,selectInput("variable", label="Colour bars by:",
                                c("virulence_score", virulence_locus_columns, "resistance_score", resistance_class_columns)),
                                downloadButton(outputId = "STdist_plot_download", label = "Download the plot")),
             column(6,wellPanel(uiOutput("numBars")))
        ),
    	tabPanel("Convergence heatmap", 
             br(), plotlyOutput("heatmap")
        ),
    	tabPanel("Convergence by ST",
             plotlyOutput("st_scatter"),
             column(6, plotlyOutput("st_virulence"))
    	)
  	) # end tabsetPanel
  ) # end mainPanel
) # end sidebarLayout
) # end ui

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
  output$summaryTable <- renderTable(sumTable(),colnames=F)
  
  sumTable <- reactive({
    vs <- c("Mean virulence score",round(mean(KleborateData()$virulence_score),2))
    vr <- c("Mean resistance score",round(mean(KleborateData()$resistance_score),2))
    us <- c("Total unique species",nlevels(KleborateData()$species))
    st <- c("Total STs",nlevels(KleborateData()$ST))
    sum_table <- t(data.frame(us,st,vs,vr))
    return(sum_table)
  })   

  # Species filter - using reactive values
  kp_complex_spp_names <- c("Klebsiella pneumoniae", "Klebsiella variicola", "Klebsiella quasivariicola", 
                            "Klebsiella quasipneumoniae subsp. quasipneumoniae", "Klebsiella quasipneumoniae subsp. similipneumoniae")
  kp_complex_spp_colours <- c("#875F9A","#e6b89c","#ead2ac","#9cafb7","#4281a4")
  names(kp_complex_spp_colours) <- kp_complex_spp_names

  # reactive values for species list and colours, default = KP complex only
  species_filter=reactiveValues(species_list=kp_complex_spp_names, species_cols = kp_complex_spp_colours)

  observeEvent(input$species_toggle,
  {
    filter = input$species_toggle
    if ('Klebsiella quasipneumoniae' %in% filter) { filter = c(filter, grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE)) }
    if ('Others' %in% filter) { filter = c(filter, all_species[!all_species %in% c(filter,grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE),species_toggles)]) }
    species_filter$species_filtered_data = KleborateData()[KleborateData()$species%in%filter,]
    species_filter$species_cols = c(kp_complex_spp_colours[kp_complex_spp_names[kp_complex_spp_names %in% filter]],
                        colorRampPalette(c("#e67d77", "#f1c280", "#f5ecd1", "#98c4ca", "#7f8288"))(nlevels(KleborateData()$species)-5)
                      )
    names(species_filter$species_cols) = c(kp_complex_spp_names[kp_complex_spp_names %in% filter],
                            levels(KleborateData()$species)[! levels(KleborateData()$species) %in% kp_complex_spp_names])  
    species_filter$species_list = filter  
  })
  
  #Resistance score plot
  resScoreBarBySpecies_reactive <- reactive({

    ggplot(data=species_filter$species_filtered_data, aes(x = as.factor(resistance_score), fill = species)) + 
      geom_bar() + coord_flip() + theme(axis.text.x = element_text(colour = "black", size = 12), 
                         axis.text.y = element_text(colour = "black", size = 12), 
                         axis.title = element_text(colour = "black", size = 18), 
                         plot.title = element_text(color="black", size=18, face="bold"),
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         axis.line = element_line(colour = "black")) + 
      scale_y_continuous(expand=c(0,0)) + 
      scale_fill_manual(values = species_filter$species_cols) + 
      ylab("Number of isolates") + 
      xlab("Resistance score") + 
      labs(fill = "Species") + 
   	  ggtitle("Resistance scores by species")
    
  })
  
  output$resScoreBarBySpecies <- renderPlot ({
 	print(resScoreBarBySpecies_reactive())
  })
   
  #Virulence score plot
  virScoreBarBySpecies_reactive <- reactive({

   ggplot(data=species_filter$species_filtered_data, aes(x = as.factor(virulence_score), fill = species)) + 
   		geom_bar() + coord_flip() + theme(axis.text.x = element_text(colour = "black", size = 12), 
   				axis.text.y = element_text(colour = "black", size = 12), 
   				axis.title = element_text(colour = "black", size = 18), 
   				plot.title = element_text(color="black", size=18, face="bold"),
   				panel.background = element_blank(), 
   				panel.border = element_blank(), 
   				axis.line = element_line(colour = "black")) + 
   				scale_y_continuous(expand=c(0,0)) + 
   				scale_fill_manual(values = species_filter$species_cols) +
   				ylab("Number of isolates") +
   				xlab("Virulence score") +
   				labs(fill = "Species") + 
   				ggtitle("Virulence scores by species")
  })
  
  output$virScoreBarBySpecies <- renderPlot ({
 	print(virScoreBarBySpecies_reactive())
  })
  
  # download PDF of score bar plots
  output$scoreBarBySpecies_plot_download <- downloadHandler(
 	filename = function() {"scoreBarplotsBySpecies.pdf"}, #default filenmae
 	content = function(file) {
 		pdf(file, width = 10, height = 6)
 		print(resScoreBarBySpecies_reactive())
 		print(virScoreBarBySpecies_reactive())
 		dev.off()
 	}
 )


  #Sequence type histogram (interactive)

  # define colour schemes and text for virulence/resistance
  virulence_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#c6dbef", "#6baed6", "#2171b5", "#08519c", "#08306b"), name = "Virulence score", labels = c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc"))
  resistance_score_scale_fill_manual <- scale_fill_manual(values=c("#ffffff", "#fcbba1", "#fb6a4a", "#cb181d"), name = "Resistance score", labels = c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbepenemase +ve", "3: Carbapenemase +ve and colisitin resistance"))


SThist_reactive <- reactive({
  
    #variable_to_stack = KleborateData()[, input$variable]
    variable_to_stack = species_filter$species_filtered_data[, input$variable]

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

      #variable_to_stack <- (KleborateData()[, input$variable] != "-") *1 #turn this into a binary
      variable_to_stack <- (species_filter$species_filtered_data[, input$variable] != "-") *1 #turn this into a binary
      cols <- c("grey", "#67000d")
      labels <- c("0: absent", "1: present")
      name <- as.character(column_decoder$display.name[column_decoder$column_name == input$variable])
 	}

#    ggplot(KleborateData(), aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
    ggplot(species_filter$species_filtered_data, aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
      geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), 
                         axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), 
                         panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
      ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0)) +
#      scale_x_discrete(limits = (levels(reorder(KleborateData()$ST,KleborateData()$ST,function(x)-length(x)))[1:input$bars])) + 
      scale_x_discrete(limits = (levels(reorder(species_filter$species_filtered_data$ST,species_filter$species_filtered_data$ST,function(x)-length(x)))[1:input$bars])) + 
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

  ### Convergence by ST tab
  ## Mean virulence and resistance scores
  # Subset dataframe
  kleborate_data.mean_vir_res <- kleborate_data %>% group_by(ST) %>% summarise(mean_vir = mean(virulence_score), mean_res = mean(resistance_score), total  = n())
  # Scatter plot function, emits event data
  output$st_scatter <- renderPlotly({
    # Create scatterplot
    p <- plot_ly(source='st_scatter') %>%
      add_trace(data=kleborate_data.mean_vir_res, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~log(total, 2)*4, opacity=0.5), name=' ') %>%
      layout(title='Mean ST virulence and resistance score', showlegend = FALSE)
    # Add new trace with coloured point if there is event data
    ed <- event_data('plotly_click', source='st_scatter')
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.mean_vir_res[ed$pointNumber+1, ]
      p <- p %>% add_trace(data=selected_st, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~log(total, 2)*4, opacity=0.5), name=' ')
    }

    return(p)
  })

  ## Mean observation of specific genes within an ST
  # Subset virulence and antibiotic gene columns and group by ST
  vir_ab_cols <- c('Yersiniabactin', 'Colibactin', 'Aerobactin', 'Salmochelin', 'AGly', 'Col', 'Fcyn', 'Flq', 'Gly', 'MLS', 'Ntmdz', 'Phe', 'Rif', 'Sul', 'Tet', 'Tmt', 'Bla', 'Bla_Carb', 'Bla_ESBL', 'Bla_ESBL_inhR', 'Bla_broad', 'Bla_broad_inhR')
  kleborate_data.gene_vir_ab <- kleborate_data[ ,c('ST', vir_ab_cols)]
  for(j in vir_ab_cols) {
    kleborate_data.gene_vir_ab[ ,j] <- gsub('-', 0, kleborate_data.gene_vir_ab [,j])
    kleborate_data.gene_vir_ab[kleborate_data.gene_vir_ab[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_vir_ab <- kleborate_data.gene_vir_ab %>% group_by(ST) %>% summarise(ybt = mean(as.numeric(as.character(Yersiniabactin))), clb = mean(as.numeric(as.character(Colibactin))), iuc = mean(as.numeric(as.character(Aerobactin))), iro = mean(as.numeric(as.character(Salmochelin))), Agly = mean(as.numeric(as.character(AGly))), Col = mean(as.numeric(as.character(Col))), Fcyn = mean(as.numeric(as.character(Fcyn))), Flq = mean(as.numeric(as.character(Flq))), Gly = mean(as.numeric(as.character(Gly))), MLS = mean(as.numeric(as.character(MLS))), Ntmdz = mean(as.numeric(as.character(Ntmdz))), Phe = mean(as.numeric(as.character(Phe))), Rif= mean(as.numeric(as.character(Rif))), Sul = mean(as.numeric(as.character(Sul))), Tet = mean(as.numeric(as.character(Tet))), Tmt = mean(as.numeric(as.character(Tmt))), Bla = mean(as.numeric(as.character(Bla))), Bla_Carb = mean(as.numeric(as.character(Bla_Carb))), Bla_ESBL = mean(as.numeric(as.character(Bla_ESBL))), Bla_ESBL_inhR = mean(as.numeric(as.character(Bla_ESBL_inhR))), Bla_broad = mean(as.numeric(as.character(Bla_broad))), Bla_broad_inhR = mean(as.numeric(as.character(Bla_broad_inhR))))

  # Bar plot function, recieves event data
  output$st_virulence <- renderPlotly({
    ed <- event_data('plotly_click', source='st_scatter')
    title_base <- 'Mean gene presence'
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.gene_vir_ab[ed$pointNumber+1, ]
      st_name <- as.character(selected_st[ ,1])
      st_data <- selected_st[ ,2:length(selected_st)]
      title <- paste(title_base, '-', st_name)
      genes <- colnames(st_data)
      values <- as.numeric(st_data)
      st_data <- data.frame(gene=genes, value=values)
      plot_ly(data=st_data, x=~gene, y=~value, type='bar') %>% layout(title=title_base, yaxis=list(range=c(0, 1)))
    } else {
      genes <- colnames(kleborate_data.gene_vir_ab)[2:length(kleborate_data.gene_vir_ab)]
      values <- rep(0, length(kleborate_data.gene_vir_ab)-1)
      empty_data <- data.frame(gene=genes, value=values)
      plot_ly(data=empty_data, x=~gene, y=~value, type='bar') %>% layout(title=title_base, yaxis=list(range=c(0, 1)))
    }
  })
}


#Load shiny app
shinyApp(ui = ui, server = server)


