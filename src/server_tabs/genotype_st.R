# ST distribution plot 
output$genotype_st_dist_plot <- renderPlotly(genotype_st_dist_plot())
genotype_st_dist_data <- reactive({
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
  # Subset specific columns for plotting
  d <- d[ ,c('strain', 'ST', 'annotation')]
  return(list(d=d, colours=v.colours, anno_name=s.anno_name))
})
genotype_st_dist_plot <- reactive({
  # Return until input ui element renders and has a default value
  if (is.null(input$genotype_st_count)) {
    return()
  }
  # Get data to plot
  v.data <- genotype_st_dist_data()
  d <- v.data$d
  v.colours <- v.data$colours
  s.anno_name <- v.data$anno_Name
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

# Download plot/data
output$genotype_st_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$genotype_st_dist_plot_anno, '_by_ST__res_'), '.csv')),
  content=function(s.filename) { write.csv(genotype_st_dist_data()$d, s.filename, row.names=FALSE) }
)
output$genotype_st_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$genotype_st_dist_plot_anno, '_by_ST__res_'), 'pdf')),
  content=function(s.filename) { download_plot(genotype_st_dist_plot, s.filename) }
)
observeEvent(input$genotype_st_plot_download_show, {
  download_modal(downloadButton('genotype_st_plot_download', class='btn-primary'))
})