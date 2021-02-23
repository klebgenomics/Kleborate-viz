# User input
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain')]
    updateSelectInput(session, 'converg_metadata_var', choices=v.cols, selected=v.cols[4])
  }
)
observeEvent(
  data_loaded$metadata,
  {
    c.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain')]
    updateSelectInput(session, 'converg_metadata_col', choices=c.cols, selected=c.cols[3])
  }
)

# Colour generator
converg_metadata_colours <- reactive({
  v.groups <- unique(data_loaded$metadata[[input$converg_metadata_col]])
  v.groups <- sort(v.groups)
  v.colours <- misc_colour_palette(length(v.groups))
  names(v.colours) <- v.groups
  return(v.colours)
})

# Plot
output$prevalence_sample_scatter <- renderPlotly(prevalence_sample_scatter_plot())
prevalence_sample_scatter_data <- reactive({
  d <- inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ])
  d %>%
    group_by(
      converg_metadata_col=d[[input$converg_metadata_col]],
      converg_metadata_var=d[[input$converg_metadata_var]]) %>%
    summarise(
      n=n(),
      mean_virulence_score=mean(virulence_score),
      mean_resistance_score=mean(resistance_score)
    ) -> d
  # Cast continuous any user group/var to discrete
  for (s.colname in c('converg_metadata_col', 'converg_metadata_var')) {
    if (is.numeric(d[[s.colname]])) {
      d[[s.colname]] <- factor(d[[s.colname]], levels=sort(unique(d[[s.colname]])))
    }
  }
  return(d)
})
prevalence_sample_scatter_plot <- reactive({
  v.colours <- converg_metadata_colours()
  d <- prevalence_sample_scatter_data()
  g <- ggplot(d, aes(x=mean_virulence_score, y=mean_resistance_score)) +
    # NOTE: set group to force information in hoverinfo; probably better to directly config hoverinfo
    geom_point(aes(size=n, colour=converg_metadata_col, group=converg_metadata_var)) +
    scale_colour_manual(values=v.colours, breaks=names(v.colours)) +
    theme_bw() + theme(legend.title=element_blank()) +
    xlab("Mean virulence score") +
    ylab("Mean resistance score")
  ggplotly(g)
})

# Download plot/data
output$prevalence_sample_scatter_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$converg_metadata_col, '__', input$converg_metadata_var), 'csv')),
  content=function(s.filename) { write.csv(prevalence_sample_scatter_data(), s.filename, row.names=FALSE) }
)
output$prevalence_sample_scatter_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$converg_metadata_col, '__', input$converg_metadata_var), 'pdf')),
  content=function(s.filename) { download_plot(prevalence_sample_scatter_plot, s.filename) }
)
observeEvent(input$prevalence_sample_scatter_plot_download_show, {
  download_modal(downloadButton('prevalence_sample_scatter_plot_download', class='btn-primary'))
})