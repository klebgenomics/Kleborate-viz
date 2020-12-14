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
    v.colours <- hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla ESBL'
  } else if (input$genotype_st_dist_plot_anno=='Bla_Carb_simplified') {
    d$annotation <- d$Bla_Carb_simplified
    n <- length(unique(d$annotation))
    v.colours <- hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla Carb'
  } else {
    if (input$genotype_st_dist_plot_anno %in% v.virulence_loci) {
      v.colours <- c("grey", "#2171b5")
      s.anno_name <- names(v.virulence_loci)[v.virulence_loci==input$genotype_st_dist_plot_anno]
    } else if (input$genotype_st_dist_plot_anno %in% v.resistance_classes) {
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
# Download plot button
output$genotype_st_plot_download <- downloadHandler(
  filename=function() {'genotype_st_dist.pdf'},
  content=function(file) {
    pdf(file, width=10, height=6)
    print(genotype_st_dist_plot())
    dev.off()
  }
)
# Download data button
output$genotype_st_data_download <- downloadHandler(
  filename=function() {
    paste0(
      input$genotype_st_dist_plot_anno, 
      '_by_ST__res', 
      input$res_score_range_slider[1],
      '-',
      input$res_score_range_slider[2],
      '_vir',
      input$vir_score_range_slider[1],
      '-',
      input$vir_score_range_slider[2],
      '.csv'
    )
  },
  content=function(s.filename) {
    d <- table(
      data_loaded$kleborate[data_selected$rows,'ST'],
      data_loaded$kleborate[data_selected$rows,input$genotype_st_dist_plot_anno]
    )
    # NOTE: ordering was done previously but I don't see how it would work...
    # v.st_counts <- table(data_loaded$kleborate$ST)
    # v.st_order <- names(v.st_counts)[order(v.st_counts, decreasing=TRUE)]
    # d <- d[order(factor(d$ST, levels=v.st_order)), ]
    write.csv(d, s.filename, row.names=TRUE)
  }
)
# Resistance v virulence heatmap plot
output$res_vir_heatmap <- renderPlotly({ 
  # Colours
  v.colours <- colorRampPalette(c('#f5ecd1', '#f1c280', '#e67d77'))(100)
  v.colours <- c('#ffffff', v.colours)
  # Tranform data
  vir_res <- table(
    factor(data_loaded$kleborate[data_selected$rows, ]$resistance_score, c(0, 1, 2, 3)),
    factor(data_loaded$kleborate[data_selected$rows, ]$virulence_score, c(0, 1, 2, 3, 4, 5))
  )
  # Create matrix for heatmaply, sort rows (descending)
  vir_res_heatmaply <- as.data.frame.matrix(vir_res)
  vir_res_heatmaply <- vir_res_heatmaply[order(-as.numeric(row.names(vir_res_heatmaply))), ]
  # Create plot
  heatmaply(
    vir_res_heatmaply,
    Rowv=NULL,
    Colv=NULL,
    ylab='Resistance score',
    xlab='Virulence score',
    fontsize_row=10,
    fontsize_col=10,
    subplot_margin=3,
    colors=v.colours, 
    margins=c(40, 40),
    revR=TRUE,
    key.title='# genomes',
    column_text_angle=0,
    plot_method='ggplot',
    node_type='scatter',
    grid_size=10,
    source='res_vir_heatmap'
  )
})
