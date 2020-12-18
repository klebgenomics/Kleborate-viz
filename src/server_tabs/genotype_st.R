# ST distribution plot 
genotype_st_dist_plot <- reactive({
  # Return until input ui element renders and has a default value
  if (is.null(input$genotype_st_count)) {
    return()
  }
  # NOTE: must use default source of 'A' as heatmaply does not appear to expose the 'source' argument
  ed <- event_data('plotly_click', source='A')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    data_selected$resistance_min <- data_selected$resistance_max <- ed$y - 1
    data_selected$virulence_min <- data_selected$virulence_max <- ed$x - 1
    data_selected$rows <- compute_row_selection()
    # Update input widgets
    updateSliderInput(session, 'res_score_range_slider', value=c(data_selected$resistance_min, data_selected$resistance_max))
    updateSliderInput(session, 'vir_score_range_slider', value=c(data_selected$virulence_min, data_selected$virulence_min))
    # Immediately clear click event (otherwise it prevents any changes to data_selected res/vir values)
    runjs("Shiny.onInputChange('plotly_click-A', 'null');")
  }
  # Get configuration for plot type
  d <- data_loaded$kleborate[data_selected$rows, ]
  if (input$genotype_st_dist_plot_anno=='virulence_score') {
    # Determine colours and label names
    v.virulence_score_labels <- paste0(names(v.virulence_score_names), ": ", v.virulence_score_names)
    names(v.virulence_score_labels) <- names(v.virulence_score_names)
    v.colours <- v.virulence_score_colours[names(v.virulence_score_labels)]
    names(v.colours) <- v.virulence_score_labels
    # Set annotation column
    d$annotation <- v.virulence_score_labels[as.character(d$virulence_score)]
    s.anno_name <- 'Virulence Score'
  } else if (input$genotype_st_dist_plot_anno=='resistance_score') {
    # Determine colours and label names
    v.resistance_score_labels <- paste0(names(v.resistance_score_names), ": ", v.resistance_score_names)
    names(v.resistance_score_labels) <- names(v.resistance_score_names)
    v.colours <- v.resistance_score_colours[names(v.resistance_score_labels)]
    names(v.colours) <- v.resistance_score_labels
    # Set annotation column
    d$annotation <- v.resistance_score_labels[as.character(d$resistance_score)]
    s.anno_name <- 'Resistance Score'
  } else if (input$genotype_st_dist_plot_anno=='Bla_ESBL_simplified') {
    d$annotation <- d$Bla_ESBL_simplified
    # NOTE: placeholder for colours
    n <- length(unique(d$annotation))
    v.colours <- v.ESBL_allele_colours
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla ESBL'
  } else if (input$genotype_st_dist_plot_anno=='Bla_Carb_simplified') {
    d$annotation <- d$Bla_Carb_simplified
    n <- length(unique(d$annotation))
    v.colours <- v.carb_allele_colours #hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla Carb'
  } else {
    if (input$genotype_st_dist_plot_anno %in% v.virulence_loci) {
      v.colours <- c("grey", "#2171b5")
      s.anno_name <- names(v.virulence_loci)[v.virulence_loci==input$genotype_st_dist_plot_anno]
    } else if (input$genotype_st_dist_plot_anno %in% v.resistance_classes & !input$genotype_st_dist_plot_anno %in% c('Bla_ESBL_simplified', 'Bla_Carb_simplified')) {
      v.colours <- c("grey", "#ef3b2c")
      s.anno_name <- names(v.resistance_classes)[v.resistance_classes==input$genotype_st_dist_plot_anno]
    } else {
      stop('Got bad annotation variable')
    }
    names(v.colours) <- c('absent', 'present')
    # Set annotation column
    d$annotation <- ifelse(d[[input$genotype_st_dist_plot_anno]]=='-', 'absent', 'present')
  }
  # Order ST by group size
  v.st_counts <- sort(table(d$ST), decreasing=TRUE)
  v.st_order <- names(v.st_counts)
  d$ST <- factor(d$ST, levels=v.st_order)
  # Select first n STs
  d <- d[d$ST %in% v.st_order[1:input$genotype_st_count], ]
  # Create plot
  g <- ggplot(data=d, aes(x=ST, fill=annotation))
  g <- g + geom_bar()
  g <- g + theme(
    axis.text.x=element_text(colour='black', size=12, angle=45, hjust=1),
    axis.text.y=element_text(colour='black', size=12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = 'black', size = 16),
    legend.text = element_text(colour = 'black', size = 12),
    legend.title = element_text(colour = 'black', size = 16),

    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(colour='black')
  )
  g <- g + ylab('Number of genomes') + xlab('ST')
  g <- g + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours, breaks=names(v.colours), name=s.anno_name, drop=FALSE)
  return(g)
})
output$genotype_st_dist_plot <- renderPlot ({ print(genotype_st_dist_plot()) })
# ST number slider
output$genotype_st_count <- renderUI({
  sliderInput(
    inputId='genotype_st_count',
    label='Number of STs:',
    min=1,
    max=length(unique(data_loaded$kleborate$ST)),
    value=min(20, length(unique(data_loaded$kleborate$ST)))
  )
})