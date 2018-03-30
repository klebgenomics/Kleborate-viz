library(shiny)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)

#Input data
kleborate_data <- read.csv("kleborate_viz_test_data_mixedSTs.txt",sep="\t")

column_decoder <- read.csv("column_decoder.txt",sep="\t")
resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])

# Define UI for Shiny App: Kleborate Visualiser
ui <- fluidPage(
	
  # common side bar to all plots
  sidebarLayout(
  
  # side bar is where we load data, show summary, and choose species
  	sidebarPanel(
  		div(img(src="logo.png",height=100,width=200)),
  		br(),
		fileInput('file', 'Load Kleborate Output File (txt)',accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
		h4("Data Summary"),
		tableOutput('summaryTable'),
		h4("Subset for Analysis"),
		uiOutput("species_toggles_labelled_with_numbers"),
		sliderInput(inputId = "res_score_range_slider", label = "Resistance scores:",min = 0,max = 3,step =1,
                value = c(0,3)),
		sliderInput(inputId = "vir_score_range_slider", label = "Virulence scores:",min = 0, max = 5,step =1,
                value = c(0,5))
    ),
  
  # main panel has a selection of tabsets to plot different analyses
  mainPanel(
    # Tab layout
    tabsetPanel(
		tabPanel("Summary",
    				br(),
      				plotOutput("resScoreBarBySpecies", height="260px"),
      				br(),
      				plotOutput("virScoreBarBySpecies", height="260px"),
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
server <- function(input, output, session) {

  # Load input data
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
  
  # Summary table for side bar: num species, # STs, mean vir, mean resistance
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
  ## NOTE: this is creating a new object row_filter$row_filtered_data which is the current data table subsetted to the species set
  ## This is not ideal as it is duplicating the data object in memory, would probably be better to call
  ## KleborateData()[KleborateData()$species %in% row_filter$species_list,] in all plots, 
  ## not sure if we can create an easy container for this
  
  # default: species in the KP complex
  kp_complex_spp_names <- c("Klebsiella pneumoniae", "Klebsiella variicola", "Klebsiella quasivariicola", 
                            "Klebsiella quasipneumoniae subsp. quasipneumoniae", "Klebsiella quasipneumoniae subsp. similipneumoniae")
  kp_complex_spp_colours <- c("#875F9A","#8CBDB2","#F0B663","#ED6060","#EDA483")
  names(kp_complex_spp_colours) <- kp_complex_spp_names

  # species toggles, labeled by number in each species
  output$species_toggles_labelled_with_numbers <- renderUI({
  	species_toggle_labels = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola","Others")
  	species_toggle_labels[1] = paste("Klebsiella pneumoniae  (", sum(KleborateData()$species=="Klebsiella pneumoniae"),")",sep="")
	species_toggle_labels[2] = paste("Klebsiella quasipneumoniae  (", 
  			sum(KleborateData()$species %in% c("Klebsiella quasipneumoniae subsp. quasipneumoniae","Klebsiella quasipneumoniae subsp. similipneumoniae")
  			),")",sep="")
  	species_toggle_labels[3] = paste("Klebsiella variicola  (", sum(KleborateData()$species=="Klebsiella variicola"),")",sep="")
  	species_toggle_labels[4] = paste("Klebsiella quasivariicola  (", sum(KleborateData()$species=="Klebsiella quasivariicola"),")",sep="")
    species_toggle_labels[5] = paste("Others  (", sum( ! (KleborateData()$species %in% kp_complex_spp_names)),")",sep="")
	checkboxGroupInput(inputId="species_toggle", label = "Toggle species", selected = species_toggle_labels[1:4], 
      		choices = species_toggle_labels)
  })

  # reactive values for species list (and species colours), and scores range
  row_filter = reactiveValues(species_list=kp_complex_spp_names, species_cols = kp_complex_spp_colours, resScore_exp=NA, virScore_exp=NA, spp_exp=NA)

	## NOTE to get the current data file subset to these rows use this:
	## 	 KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,]

  # species selection
  observeEvent(input$species_toggle, {
  	all_species = levels(KleborateData()$species)
  	other_species = levels(KleborateData()$species[!(KleborateData()$species %in% kp_complex_spp_names)])
    filter = unlist(strsplit(input$species_toggle,"  "))[seq(1,length(unlist(strsplit(input$species_toggle,"  "))),2)]
    if ('Klebsiella quasipneumoniae' %in% filter) { filter = c(filter, grep("Klebsiella quasip",all_species,value=TRUE,fixed=TRUE)) }
    if ('Others' %in% filter) { 
    	filter = c(filter, 
			all_species[!(all_species %in% kp_complex_spp_names)]
    	) }
    row_filter$row_filtered_data = KleborateData()[KleborateData()$species %in% filter,]
    row_filter$species_cols = c(kp_complex_spp_colours[kp_complex_spp_names[kp_complex_spp_names %in% filter]],
						colorRampPalette(c("#000000", "#5B6894",  "#876738"))(length(other_species))
                      )
    names(row_filter$species_cols) = c(kp_complex_spp_names[kp_complex_spp_names %in% filter],
                            levels(KleborateData()$species)[! levels(KleborateData()$species) %in% kp_complex_spp_names])  
    row_filter$species_list = filter
    row_filter$spp_exp = KleborateData()$species %in% filter
  })
  
  # resistance scores selection
  observeEvent(input$res_score_range_slider, {
  	row_filter$resScore_exp = KleborateData()$resistance_score >= input$res_score_range_slider[1] & KleborateData()$resistance_score <= input$res_score_range_slider[2]
  })
  
  # virulence scores selection
  observeEvent(input$vir_score_range_slider, {
  	row_filter$virScore_exp = KleborateData()$virulence_score >= input$vir_score_range_slider[1] & KleborateData()$virulence_score <= input$vir_score_range_slider[2]
  })
    
  # Resistance score plot - for summary page
  resScoreBarBySpecies_reactive <- reactive({

    ggplot(data=KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,], 
      aes(x = resistance_score, fill = species)) + 
      geom_bar() + coord_flip() + theme(axis.text.x = element_text(colour = "black", size = 12), 
                         axis.text.y = element_text(colour = "black", size = 12), 
                         axis.title = element_text(colour = "black", size = 18), 
                         plot.title = element_text(color="black", size=18, face="bold"),
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         axis.line = element_line(colour = "black")) + 
      scale_x_continuous(breaks=0:3) +		
      scale_y_continuous(expand=c(0,0)) + 
      scale_fill_manual(values = row_filter$species_cols) + 
      ylab("Number of isolates") + 
      xlab("Resistance score") + 
      labs(fill = "Species") + 
   	  ggtitle("Resistance scores by species")
    
  })
  
  output$resScoreBarBySpecies <- renderPlot ({
 	print(resScoreBarBySpecies_reactive())
  })
   
  # Virulence score plot - for summary page
  virScoreBarBySpecies_reactive <- reactive({

    ggplot(data=KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,],
    	aes(x = virulence_score, fill = species)) + 
   		geom_bar() + coord_flip() + theme(axis.text.x = element_text(colour = "black", size = 12), 
   				axis.text.y = element_text(colour = "black", size = 12), 
   				axis.title = element_text(colour = "black", size = 18), 
   				plot.title = element_text(color="black", size=18, face="bold"),
   				panel.background = element_blank(), 
   				panel.border = element_blank(), 
   				axis.line = element_line(colour = "black")) + 
   		scale_x_continuous(breaks=0:5) +		
   		scale_y_continuous(expand=c(0,0)) + 
   		scale_fill_manual(values = row_filter$species_cols) +
   		ylab("Number of isolates") +
   		xlab("Virulence score") +
   		labs(fill = "Species") + 
   		ggtitle("Virulence scores by species")
  })
  
  output$virScoreBarBySpecies <- renderPlot ({
 	print(virScoreBarBySpecies_reactive())
  })
  
  # download PDF of score bar plots - from summary page
  output$scoreBarBySpecies_plot_download <- downloadHandler(
 	filename = function() {"scoreBarplotsBySpecies.pdf"}, #default filenmae
 	content = function(file) {
 		pdf(file, width = 10, height = 6)
 		print(resScoreBarBySpecies_reactive())
 		print(virScoreBarBySpecies_reactive())
 		dev.off()
 	}
 )

  # Sequence type histogram (interactive)


  SThist_reactive <- reactive({
    
    variable_to_stack = as.factor(KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp, input$variable])

    if(input$variable == "virulence_score"){
      cols <- c("#deebf7", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08306b")
      names(cols) <- 0:5
      labels <- c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc")
      names(labels) <- 0:5
      name <- "Virulence score"
    }
    else if(input$variable == "resistance_score"){
      cols <- c("#fcbba1", "#fc9272", "#fb6a4a", "#BE413D")
      names(cols) <- 0:3
      labels <- c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbapenemase +ve", "3: Carbapenemase +ve and colistin resistance")
      names(labels) <- 0:3
      name <- "Resistance score"
    }

	# individual genes
 	else {
      variable_to_stack <- (KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp, input$variable] != "-") *1 #turn this into a binary
      levels(variable_to_stack) <- c("0","1")
      if (input$variable %in% virulence_locus_columns) {
      	cols <- c("grey", "#2171b5") # blue for virulence variables
      }
      else { cols <- c("grey", "#ef3b2c") } # red for resistance variables
      labels <- c("absent", "present")
      name <- as.character(column_decoder$display.name[column_decoder$column_name == input$variable])
 	}

#    ggplot(KleborateData(), aes(x=reorder(ST,ST,function(x)-length(x)), fill = variable_to_stack)) + 
    ggplot(data=KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,], 
    	aes(x=reorder(ST,ST,function(x)-length(x)), fill = variable_to_stack)) + 
    	ggtitle(name) +
      	geom_bar() + 
      	theme(axis.text.x = element_text(colour = "black", size = 12, angle = 45, hjust = 1), 
        	axis.text.y = element_text(colour = "black", size = 12),
        	axis.title = element_text(colour = "black", size = 18), 
            panel.background = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
            axis.line = element_line(colour = "black")) + 
      	ylab("Number of isolates") + 
      	xlab("ST") + 
      	scale_y_continuous(expand=c(0,0)) +
      	scale_x_discrete(limits = (levels(reorder(KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,]$ST,
      		KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,]$ST,
      		function(x)-length(x)))[1:input$bars])) + 
      	scale_fill_manual(values = cols[levels(variable_to_stack)], labels=labels[levels(variable_to_stack)], name=name)
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


