# This is the UI/Server for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
############ ***************************************************** #############
#                                          R PACKAGES DEPENDENCE               #
############ ***************************************************** #############
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
library(lintr)
library(formatR)
library(styler)

####### ***************************************************** ################
#                                          LOAD FILES                        #
####### ***************************************************** ################

#Input data
kleborate_data <- read.csv("data/kleborate_output.txt",sep="\t")
column_decoder <- read.csv("data/column_decoder.txt",sep="\t")
resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])

#######*****************************************************################
#                                            SHINY UI START                #
#######*****************************************************################
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
		tabPanel("Summary by species",
    				br(),
      				plotOutput("resScoreBarBySpecies", height="260px"),
      				br(),
      				plotOutput("virScoreBarBySpecies", height="260px"),
      				br(),
      				div(style = "position:absolute;right:1em;",downloadButton(outputId = "scoreBarBySpecies_plot_download", label = "Download plots"))
    	),
    	tabPanel("Genotypes by ST",
             plotOutput("SThist", height="300px"),
             column(6,selectInput("variable", label="Colour bars by:",
                                c("virulence_score", virulence_locus_columns, "resistance_score", resistance_class_columns)),
                                downloadButton(outputId = "STdist_plot_download", label = "Download the plot"),
                                downloadButton(outputId = "STdist_data_download", label = "Download the data")
         	 ),
             column(6,wellPanel(uiOutput("numBars"))),
             column(12,
             	h4("Resistance vs virulence across all strains (click to select subset)"),
             	plotlyOutput("heatmap", height="200px")
             )
        ),
    	tabPanel("Convergence by ST",
    		br(),
             plotlyOutput("st_scatter", height="300px"),
             br(),
             plotlyOutput("st_res_vir")
    	),
    	tabPanel("K vs O diversity",
    		br(),
             plotlyOutput("k_vs_o_plotlyBubble", height="300px"),
             br(),
             plotlyOutput("k_o_barplot_selected", height="300px")
    	)
  	) # end tabsetPanel
  ) # end mainPanel
) # end sidebarLayout
) # end ui

#######***************************************************** ################
#                          DEFINE SHINY SERVER LOGIC START                  #
#######***************************************************** ################

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
  row_filter = reactiveValues(species_list=kp_complex_spp_names, species_cols = kp_complex_spp_colours, resScore_exp=NA, virScore_exp=NA, spp_exp=NA, selected_st=NA)

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
      name <- "Virulence score by ST (current subset)"
    }
    else if(input$variable == "resistance_score"){
      cols <- c("#fcbba1", "#fc9272", "#fb6a4a", "#BE413D")
      names(cols) <- 0:3
      labels <- c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbapenemase +ve", "3: Carbapenemase +ve and colistin resistance")
      names(labels) <- 0:3
      name <- "Resistance score by ST (current subset)"
    }

	# individual genes
 	else {
      variable_to_stack <- as.factor((variable_to_stack != "-") *1) #turn this into a binary
      levels(variable_to_stack) <- c("0","1")
      if (input$variable %in% virulence_locus_columns) {
      	cols <- c("grey", "#2171b5") # blue for virulence variables
      }
      else { cols <- c("grey", "#ef3b2c") } # red for resistance variables
      labels <- c("absent", "present")
      name <- paste(as.character(column_decoder$display.name[column_decoder$column_name == input$variable]), " by ST (current subset)",sep="")
 	}

    ggplot(data=KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,], 
    	aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
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
      	scale_x_discrete(limits = (levels(reorder(KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,"ST"],
      		KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp,"ST"],
      		function(x)-length(x)))[1:input$bars])) + 
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
 
 output$STdist_data_download <- downloadHandler(
    filename = function() {
      paste(input$variable, "_by_ST__res", input$res_score_range_slider[1],"-",input$res_score_range_slider[2],"_vir",
      			input$vir_score_range_slider[1],"-",input$vir_score_range_slider[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(
      	table(KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp, "ST"],
      		KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp, input$variable]
      	)[rev(order(table(KleborateData()[row_filter$spp_exp & row_filter$resScore_exp & row_filter$virScore_exp, "ST"]))),],
      file, row.names = TRUE)
    }
 )

  # Heat map (interactive)
  output$heatmap <- renderPlotly({
    # create colour palette
    cols <- colorRampPalette(c("#f5ecd1", "#f1c280", "#e67d77"))(100)
    cols <- c("#ffffff", cols)
    
    # format data
#    vir_res <- table(factor(KleborateData()$resistance_score,c(0,1,2,3)),factor(KleborateData()$virulence_score,c(0,1,2,3,4,5))) # create dataframe summary of res and vir scores
	vir_res <- table(factor(KleborateData()[row_filter$spp_exp,]$resistance_score,c(0,1,2,3)),
			factor(KleborateData()[row_filter$spp_exp,]$virulence_score,c(0,1,2,3,4,5))) # create dataframe summary of res and vir scores
    vir_res_heatmaply <- as.data.frame.matrix(vir_res) # convert to matrix for heatmaply
    vir_res_heatmaply <- vir_res_heatmaply[ order(-as.numeric(row.names(vir_res_heatmaply))),] # reorder rows - descending
    NumStrains <- as.matrix(vir_res) # convert to matrix for plotly
        
    # draw plot
#    heatmaply(vir_res_heatmaply, Rowv=NULL, Colv=NULL, ylab = "Resistance score", xlab = #"Virulence score", fontsize_row = 12, fontsize_col = 12, 
#    	subplot_margin = 3, colors = cols, margins = c(40,40), revR=TRUE, key.title = "# #genomes", column_text_angle = 0,
#    	plot_method="ggplot", node_type="scatter", grid_size=20
#    	)

	# heatmap with plotly
	plot_ly(z=~NumStrains, type="heatmap", colors=cols,
		x=c("None", "ybt\n(ICEKp)", "ybt+clb\n(ICEKp)", "iuc\n(VP)","ybt (ICEKp)\n+iuc (VP)", "ybt+clb (ICEKp)\n+iuc (VP)"), y=c("S","ESBL","Carb","Col"),
		xgap=5, ygap=5, showlegend=F
	)

  })
  

  ### Convergence by ST tab
  ## Mean virulence and resistance scores
  
  # Scatter plot function, emits event data
  output$st_scatter <- renderPlotly({
  	kleborate_data.mean_vir_res <- KleborateData()[row_filter$spp_exp,] %>% group_by(ST) %>% summarise(mean_vir = mean(virulence_score), mean_res = mean(resistance_score), total  = n())
  	marker_function <- function(total) {
  		if (nrow(KleborateData()[row_filter$spp_exp,])>100) { return(log(total, 2)*4) }
  		else { return (total*10) }
  	}
  	
  	write.table(kleborate_data.mean_vir_res, file="kleborate_data.mean_vir_res.txt")
  	
    # Create scatterplot
    p <- plot_ly(source='st_scatter') %>%
      add_trace(data=kleborate_data.mean_vir_res, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~marker_function(total), opacity=0.5), name=' ') %>%
      layout(title='Mean virulence and resistance score by ST (click to show details)', 
      	showlegend = FALSE, xaxis=list(title="mean virulence score"), yaxis=list(title="mean resistance score"))
      	
    # Add new trace with coloured point if there is event data
    ed <- event_data('plotly_click', source='st_scatter')
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.mean_vir_res[ed$pointNumber+1, ]
      p <- p %>% add_trace(data=selected_st, x=~mean_vir, y=~mean_res, text=~ST, type='scatter', mode='markers', marker=list(size=~marker_function(total), opacity=0.5), name=' ')
    }
    return(p)
  }) # end renderPlotly()
  
  output$st_res_vir <- renderPlotly({
  
  	data_by_species <- KleborateData()[row_filter$spp_exp,]
  	
  	ed <- event_data('plotly_click', source='st_scatter')
  	if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- levels(as.factor(as.character(data_by_species$ST)))[ed$pointNumber+1]
      data_matrix <- data_by_species[data_by_species$ST %in% selected_st,]
      st_name <- as.character(selected_st)
      main_title = paste("Selected strains:",st_name)
    }
    else {
    	if (nrow(data_by_species) <= 30) {
    		data_matrix <- data_by_species # show all
    		main_title <- "All STs"
    	}
		else {
			if (sum(data_by_species$virulence_score>=3 & data_by_species$resistance_score >=1)==0) {
				kd <- data_by_species %>% group_by(ST) %>% summarise(mean_vir = mean(virulence_score), mean_res = mean(resistance_score), total  = n())
				selected_st <- kd$ST[kd$mean_vir*kd$mean_res==max(kd$mean_vir*kd$mean_res)][1]
				data_matrix <- data_by_species[data_by_species$ST %in% selected_st,]
				st_name <- as.character(selected_st)
				main_title = paste("Most convergent ST:",st_name)
			}
			else { # if there are strains with aerobactin and clinically significant resistance, report them
				data_matrix <- data_by_species[data_by_species$virulence_score>=3 & data_by_species$resistance_score >=1,]
				main_title <- "Convergent strains"
			}
		}
    }
    rownames(data_matrix) <- data_matrix[,1]
      vir_data_matrix <- (data_matrix[,c(virulence_locus_columns)]!="-")*1 # convert to binary matrix
      res_data_matrix <- (data_matrix[,c(resistance_class_columns)]!="-")*2 # convert to binary matrix
      st_data_matrix <- as.data.frame.matrix(cbind(vir_data_matrix,res_data_matrix)) # format for heatmaply
  
  	# res/vir loads calculated from matrix
#  	genefreq <- apply(st_data_matrix,2,mean)
#  	virload <- apply(vir_data_matrix,1,mean)
#  	resload <- apply(res_data_matrix,1,mean)
      
       # cluster rows if ≥3 strains
      if(nrow(st_data_matrix)<3) {rowv=NULL}
      else{rowv=T}
      
      # don't plot strain names if more than 30
#      if(nrow(st_data_matrix)>50) {print_row_names=FALSE}
#      else{print_row_names=TRUE}    

      # draw plot with heatmaply
	heatmaply(st_data_matrix, Rowv=rowv, Colv=NULL, xlab = "genotypes", hide_colorbar=T,
          fontsize_row = 6, fontsize_col = 7,  revC=F,
#          col_side_colors=as.data.frame.matrix(cbind(gene_freq=genefreq)), col_side_palette = colorRampPalette(c("white","black")),
#          row_side_colors=as.data.frame.matrix(cbind(virload,resload)), 
#		row_side_colors=as.data.frame.matrix(data_matrix[,c("virulence_score","resistance_score","num_resistance_classes","num_resistance_genes")]),
#          row_side_palette = colorRampPalette(c("white","black")),
    # 		showticklabels = c(TRUE,print_row_names),
          colors=c("white","#2171b5","#ef3b2c"), main = main_title
	)

  })
  
  
	# K vs O scatter plot by ST, emits event data
	# TO DO: carry over the species & res selections for these plots
  output$k_vs_o_plotlyBubble <- renderPlotly({
  
	  # generate dataframe with simpson diversity and total number of genomes per ST
	  st_vs_k <- table(KleborateData()[row_filter$spp_exp,]$ST,KleborateData()[row_filter$spp_exp,]$K_locus)
	  st_vs_o <- table(KleborateData()[row_filter$spp_exp,]$ST,KleborateData()[row_filter$spp_exp,]$O_locus)
	  div_k <- as.data.frame(diversity(st_vs_k, index = "simpson"))
	  div_o <- as.data.frame(diversity(st_vs_o, index = "simpson"))
	  div_combined <- merge(div_k, div_o, by = 0)
	  colnames(div_combined) <- c("ST", "kdiv", "odiv")
	  div_combined$keff <- 1/(1-div_combined$kdiv)
	  div_combined$oeff <- 1/(1-div_combined$odiv)
	  div_combined$total <- rowSums(st_vs_k)
  
  
      # Create scatterplot
	# TO DO: carry over the species & res selections for these plots
    k_vs_o <- plot_ly(source='k_vs_o_plotlyBubble') %>%
      add_trace(data=div_combined, x = ~div_combined$keff, y = ~div_combined$oeff, size = ~div_combined$total*2, text = ~paste("ST: ", div_combined$ST)) %>%
      layout(title='K and O diversity by ST (click to show details)',xaxis = list(title = "K locus diversity"), yaxis = list(title = "O locus diversity"))
    
  return(k_vs_o)
  })
  

  # plot K locus heatmap for selected data
  output$k_o_barplot_selected <- renderPlotly({
  
  	selected_st <- "ST17" # hard code an example for now, could change to most common ST
  
  ed <- event_data('plotly_click', source='k_vs_o_plotlyBubble')
  	if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- levels(as.factor(as.character(KleborateData()$ST)))[ed$pointNumber+1]
      
    }
    
    data_matrix <- KleborateData()[KleborateData()$ST == selected_st,]
    st_name <- paste(as.character(selected_st))
    main_title = paste("Selected strains:",st_name)
    
    # K vs O heatmap
    k_vs_o <- table(data_matrix$K_locus,data_matrix$O_locus)
    k_vs_o <- as.data.frame.matrix( k_vs_o[rowSums(k_vs_o)>0, colSums(k_vs_o)>0] )
      
    # draw plot with heatmaply
	heatmaply(k_vs_o, main = main_title, fontsize_row = 7, fontsize_col = 7, 
		colors = c("white",colorRampPalette(colors = c("yellow", "darkred"))(max(k_vs_o)))
	)

  })
  
  
} # end server


#Load shiny app
shinyApp(ui = ui, server = server)