# ST distribution plot 
output$genotype_st_dist_plot <- renderPlotly({
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
  v.prep <- get_plot_metadata_annotation(d, input$genotype_st_dist_plot_anno)
  d <- v.prep$d
  v.colours <- v.prep$colours
  s.anno_name <- v.prep$anno_name
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
  ggplotly(g)
})
# ST number slider
output$genotype_st_count <- renderUI({
  sliderInput(
    inputId='genotype_st_count',
    label='Number of STs:',
    min=1,
    max=length(unique(data_loaded$kleborate$ST)),
    value=min(20, length(unique(data_loaded$kleborate$ST))),
    step=1
  )
})
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
