# This is the server logic for a Shiny web application.
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
#                                            SHINY SERVER START                                                 #
######################## *******************************  ************************************** ################ 

server <- function(input, output){
  
  #kleborate_data <- reactive({
  #req(input$file)
  #inFile <- input$file
  #data <- read.csv(inFile$datapath,sep="\t")
  #return (data)
  #})
  
  KleborateData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(kleborate_data)
    data <- read.csv(inFile$datapath,sep="\t")
    return(data)
  })
  
  column_decoder <- read.csv("column_decoder.txt",sep="\t")
  resistance_class_columns <- as.character(column_decoder$column_name[column_decoder$type =="resistance_class"])
  virulence_locus_columns <- as.character(column_decoder$column_name[column_decoder$type =="virulence_locus"])
  all_species = levels(kleborate_data$species)
  species_toggles = c("Klebsiella pneumoniae","Klebsiella quasipneumoniae","Klebsiella variicola","Klebsiella quasivariicola")
  
  #files to change - metadata or function
  year_vir_res <- read.csv("mean_vir_res_by_year_jun2020.csv")
  sample_vir_res <- read.csv("mean_vir_res_scores_sampletype_jun2020.csv")
  ST_data <- read.csv("Kleborate_ST_vir_res_heatmap_toplot_Dec2019.csv")
  bar <- read.csv("Kleborate_ST_meta_barchart_toplot_Dec2019_v2.csv")
  Eu_KO <- read.csv("EuSCAPE_K_O_analysis.csv")
  write.table(prop.table(table(Eu_KO$K_locus, Eu_KO$region), 2), file = "K_region_prevalence_by_region_EuSCAPE_290719.csv")
  write.table(prop.table(table(Eu_KO$O_locus, Eu_KO$region), 2), file = "O_locus_prevalence_by_region_EuSCAPE_290719.csv")
  write.table(prop.table(table(Eu_KO$O_type, Eu_KO$region), 2), file = "O_types_prevalence_by_region_EuSCAPE_290719.csv")
  cumulative_K_data <- read.csv("K_region_prevalence_by_region_EuSCAPE_290719_cumulativeplot_v2.csv")
  cumulative_O_data <- read.csv("O_types_prevalence_by_region_EuSCAPE_290719_cumulativeplot.csv")
  f7 <- read.csv("EuSCAPE-Kleborate-AMR_comparison_260819_forR.csv")
  
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
  
  # F1
  f1 <- reactive({
    p1 <- ggplot(year_vir_res, aes(x=year)) + geom_bar(aes(weight =year_vir_res$virulence_score)) + 
    geom_line(aes(y=year_vir_res$Yersiniabactin, color = "red")) + geom_line(aes(y=year_vir_res$Colibactin, color = "blue")) 
    + geom_line(aes(y=year_vir_res$Aerobactin, color = "grey")) + geom_line(aes(y=year_vir_res$Salmochelin, color = "white")) 
    + geom_line(aes(y=year_vir_res$RmpADC, color = "black")) + geom_line(aes(y=year_vir_res$rmpA2, color = "orange")) + theme_bw()
    p2 <- ggplot(year_vir_res, aes(x=year)) + geom_bar(aes(weight =year_vir_res$resistance_score)) + geom_line(aes(y=year_vir_res$Col, color = "red")) 
    + geom_line(aes(y=year_vir_res$Bla_ESBL, color = "black")) + geom_line(aes(y=year_vir_res$Bla_Carb, color = "grey")) + theme_bw()
    p3 <- ggplot(year_vir_res, aes(x=year)) + geom_line(aes(y=year_vir_res$num_resistance_classes, color = "red")) 
    + geom_line(aes(y=year_vir_res$num_resistance_genes, color = "black")) + theme_bw()
    return(f1)
  })
  
  # F2
  f2 <- reactive({
    ggplot(sample_vir_res, aes(x=sample_vir_res$mean_vir, y=sample_vir_res$mean_res)) 
    + geom_point(aes(size=sample_vir_res$number.of.genomes)) + geom_text_repel(label=sample_vir_res$sample)
    ST_vir_res <- read.csv("mean_vir_res_scores_ST_jun2020.csv")
    ggplot(ST_vir_res, aes(x=ST_vir_res$mean_vir, y=ST_vir_res$mean_res)) + geom_point(aes(size=ST_vir_res$number.of.genomes)) 
    + geom_text_repel(label=ST_vir_res$ST)
    return(f2)
  })
  
  # F3
  f3 <- reactive({
    row.names(ST_data) <- ST_data$X
    ST_data <- ST_data [,2:25]
    ST_heatmap <- pheatmap(ST_data)
    ST_heatmap <- pheatmap(ST_data, cluster_cols = FALSE)
    ST_heatmap <- pheatmap(ST_data, cluster_cols = FALSE, cluster_rows = FALSE)
    ST_heatmap <- pheatmap(ST_data, cluster_cols = FALSE, cluster_rows = FALSE, cellheight = 7.5, 
    color = colorRampPalette(c("#ffffff", "#aaaaaa", "#2d2c2c"))(10))
    ST_heatmap <- pheatmap(ST_data, cluster_cols = FALSE, cluster_rows = FALSE, cellheight = 7.5, 
    color = colorRampPalette(c("#ffffff", "#aaaaaa", "#2d2c2c"))(10), angle_col = 45)
    ggplot(bar, aes(fill=bar$category, y=value, x=bar$ST)) + geom_bar(position = "fill", stat = "identity") + coord_flip() + theme_bw() + 
    scale_x_discrete(limits=c("ST258","ST11","ST15","ST512","ST307","ST101","ST16","ST147","ST23","ST14","ST37","ST45","ST17","ST231","ST340","ST35",
    "ST29","ST48", "ST437","ST20","ST405","ST323","ST34","ST86","ST268","ST25","ST395","ST152","ST36","ST3128","ST874","ST13","ST336","ST39","ST392",
    "ST111","ST65","ST661"))
  })
  
  # F4, F5, F6 without example, revisit after
  
  f6 <- reactive({
    ggplot() + geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_asia_w))
    ggplot() + geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_asia_w), color='#a9d7ed')
    ggplot() + geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_asia_w), color='#a9d7ed') + 
    geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_E_Europe), color='#fa9fb5') + geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci,
    y=cf_N_Europe), color='#f768a1') + geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_S_Europe), color='#962e99') + 
    geom_step(f6 = cumulative_K_data, mapping = aes(x=no_K_loci, y=cf_W_Europe), color = '#49006a') + theme_bw()
    O_cumulative <- ggplot() + geom_step(f6 = cumulative_O_data, mapping = aes(x=no_O_type, y=cf_asia_w), color='#a9d7ed') + 
    geom_step(f6 = cumulative_O_data, mapping = aes(x=no_O_type, y=cf_E_Europe), color='#fa9fb5') + geom_step(f6 = cumulative_O_data, 
    mapping = aes(x=no_O_type, y=cf_N_Europe), color='#f768a1') + geom_step(f6 = cumulative_O_data, mapping = aes(x=no_O_type, y=cf_S_Europe), 
    color='#962e99') + geom_step(f6 = cumulative_O_data, mapping = aes(x=no_O_type, y=cf_W_Europe), color = '#49006a') + theme_bw()
    grid.arrange(K_cumulative, O_cumulative, nrow = 1)
  })   
  
  # F7A, change this variable because happens issues
  
  f7 <- reactive({
    amr_df <- data.frame(f7)
    carb_summary <- as.character(amr_df$Bla_carb!="-")
    carb_summary <- replace(carb_summary,carb_summary=="TRUE","Carbapenemase")
    carb_summary <- replace(carb_summary,carb_summary=="FALSE","none")
    amr_df$carb_summary <- factor(carb_summary, levels=c("none","Carbapenemase"))
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_summary)) + geom_boxplot() + geom_jitter() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_hline(yintercept=8, linetype="dashed", color = "red")+theme_classic() + ylab("MIC Imipenem") + xlab("Reported carbapenemase") + 
    scale_y_continuous(trans = "log2")
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_summary)) + geom_boxplot() + geom_jitter(aes(colour=ST_to_plot)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept=8, linetype="dashed", color = "red") + 
    theme_classic() + ylab("MIC Imipenem") + xlab("Reported carbapenemase") + scale_y_continuous(trans = "log2")
    carb_Omp_summary <- as.character(amr_df$Bla_carb)
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb=="-" & amr_df$Omp=="-","none")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb!="-" & amr_df$Omp=="-","Carbapenemase")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb!="-" & amr_df$Omp!="-","Carbapenemase+Omp")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb=="-" & amr_df$Omp!="-","Omp")
    amr_df$carb_Omp_summary <- factor(carb_Omp_summary, levels=c("none","Omp","Carbapenemase","Carbapenemase+Omp"))
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_Omp_summary)) + geom_boxplot() + geom_jitter() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(trans="log2") + 
    geom_hline(yintercept=8, linetype="dashed", color = "red") + theme_classic() + ylab("MIC Meropenem") + xlab("Reported carbapenemase")
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_Omp_summary)) + geom_boxplot() + geom_jitter(aes(colour=ST_to_plot)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(trans="log2") + geom_hline(yintercept=8, linetype="dashed", 
    color = "red")+theme_classic() + ylab("MIC Meropenem") + xlab("Reported carbapenemase")
    amr_df <- data.frame(data)
    carb_summary <- as.character(amr_df$Bla_carb!="-")
    carb_summary <- replace(carb_summary,carb_summary=="TRUE","Carbapenemase")
    carb_summary <- replace(carb_summary,carb_summary=="FALSE","none")
    amr_df$carb_summary <- factor(carb_summary, levels=c("none","Carbapenemase"))
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_summary)) + geom_boxplot() + geom_jitter() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_hline(yintercept=8, linetype="dashed", color = "red")+theme_classic() + ylab("MIC Meropenem") + xlab("Reported carbapenemase") + 
    scale_y_continuous(trans = "log2")
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_summary)) + geom_boxplot() + geom_jitter(aes(colour=CG_to_plot)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_hline(yintercept=8, linetype="dashed", color = "red")+theme_classic() + 
    ylab("MIC Meropenem") + xlab("Reported carbapenemase") + scale_y_continuous(trans = "log2")
    carb_Omp_summary <- as.character(amr_df$Bla_carb)
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb=="-" & amr_df$Omp=="-","none")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb!="-" & amr_df$Omp=="-","Carbapenemase")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb!="-" & amr_df$Omp!="-","Carbapenemase+Omp")
    carb_Omp_summary <- replace(carb_Omp_summary,amr_df$Bla_carb=="-" & amr_df$Omp!="-","Omp")
    amr_df$carb_Omp_summary <- factor(carb_Omp_summary, levels=c("none","Omp","Carbapenemase","Carbapenemase+Omp"))
    ggplot(amr_df, aes(y=Meropenem_MIC,x= carb_Omp_summary)) + geom_boxplot() + geom_jitter(aes(colour=CG_to_plot)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(trans="log2")+ geom_hline(yintercept=8, linetype="dashed", color = "red") + 
    theme_classic() + ylab("MIC Meropenem") + xlab("Reported carbapenemase")
  }) 
  
  # F7B, change this variable because happens issues
  f7b <- reactive({
    amr_df <- data.frame(f7)
    ggplot(amr_df, aes(y=Meropenem_MIC, x=Bla_carb_simplified)) + geom_boxplot() + geom_jitter(aes(colour=Omp_simplified)) + scale_y_continuous(trans="log2") + scale_color_manual(values=c("#000000", "#E69F00", "#CC0011", "#D16E66", "#D16E66", "#CC0011","#D16E66"))+theme_classic()
  })
  
  # F8, fluroquinolone
  f8 <- reactive({
    amr_df <- data.frame(f7)
    ggplot(amr_df, aes(y=Meropenem_MIC, x=Bla_carb_simplified)) + geom_boxplot() + geom_jitter(aes(colour=Omp_simplified)) + scale_y_continuous(trans="log2") + scale_color_manual(values=c("#000000", "#E69F00", "#CC0011", "#D16E66", "#D16E66", "#CC0011","#D16E66"))+theme_classic()
  })
  
  # Species filter - using reactive values
  ## NOTE: this is creating a new object row_filter$row_filtered_data which is the current data table subsetted to the species set
  ## This is not ideal as it is duplicating the data object in memory, would probably be better to call
  ## KleborateData()[KleborateData()$species %in% row_filter$species_list,] in all plots, 
  ## not sure if we can create an easy container for this
  
  # Species filter - using reactive values
  kp_complex_spp_names <- c("Klebsiella pneumoniae", "Klebsiella variicola", "Klebsiella quasivariicola", "Klebsiella quasipneumoniae subsp. quasipneumoniae", "Klebsiella quasipneumoniae subsp. similipneumoniae")
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
 
  output$resScoreBarBySpecies <- renderPlot ({
    print(resScoreBarBySpecies_reactive())
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
    variable_to_stack = species_filter$species_filtered_data[, input$file]
    
    if(input$file == "virulence_score"){
      cols <- c("#deebf7", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08306b")
      labels <- c("0: None", "1: ybt", "2: ybt + clb", "3: iuc (indicates virulence plasmid)", "4: ybt + iuc", "5: ybt + clb + iuc")
      name <- "Virulence score"
    }
    else if(input$file == "resistance_score"){
      cols <- c("#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#67000d")
      labels <- c("0: ESBL and carbapenemase -ve", "1: ESBL +ve", "2: Carbapenemase +ve", "3: Carbapenemase +ve and colistin resistance")
      name <- "Resistance score"
    }
    
    # individual genes
    else {
      
      #variable_to_stack <- (KleborateData()[, input$file] != "-") *1 #turn this into a binary
      variable_to_stack <- (species_filter$species_filtered_data[, input$file] != "-") *1 #turn this into a binary
      cols <- c("grey", "#67000d")
      labels <- c("0: absent", "1: present")
      name <- as.character(column_decoder$display.name[column_decoder$column_name == input$file])
    }
    
    ggplot(KleborateData(), aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
      ggplot(species_filter$species_filtered_data, aes(x=reorder(ST,ST,function(x)-length(x)), fill = as.factor(variable_to_stack))) + 
      geom_bar() + theme(axis.text.x = element_text(colour = "black", size = 12,angle = 45, hjust = 1), 
                         axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(colour = "black", size = 14), 
                         panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
      ylab("Number of isolates") + xlab("ST") + scale_y_continuous(expand=c(0,0)) +
      scale_x_discrete(limits = (levels(reorder(KleborateData()$ST,KleborateData()$ST,function(x)-length(x)))[1:input$bars])) + 
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
  
  #Heatmap (interactive)
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

shinyApp(ui = ui, server = server)
}