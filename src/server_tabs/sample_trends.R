# Get summary data
metadata_summary_sample <- reactive({
 inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) -> d
  d %>%
   group_by(
     sample_trends_col=d[[input$sample_trends_col]],
     sample_trends_var=d[[input$sample_trends_var]]) %>%
   summarise(
     n=n(),
     mean_virulence_score = mean(virulence_score),
     mean_resistance_score = mean(resistance_score)
   )
})

# User input
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain', 'Year', 'year')]
    updateSelectInput(session, 'sample_trends_var', choices=v.cols, selected=v.cols[4])
  }
)
observeEvent(
  data_loaded$metadata,
  {
    c.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain', 'Year', 'year')]
    updateSelectInput(session, 'sample_trends_col', choices=c.cols, selected=c.cols[3])
  }
)

# Colour generator
sample_trends_colours <- reactive({
  v.groups <- unique(data_loaded$metadata[[input$sample_trends_col]])
  v.groups <- sort(v.groups)
  v.colours <- misc_colour_palette(length(v.groups))
  names(v.colours) <- v.groups
  return(v.colours)
})

# Plot
output$prevalence_sample_scatter <- renderPlotly({
  v.colours <- sample_trends_colours()
  d <- metadata_summary_sample()
  g <- ggplot(d, aes(x=mean_virulence_score, y=mean_resistance_score)) +
    # NOTE: set group to force information in hoverinfo; probably better to directly config hoverinfo
    geom_point(aes(size=n, colour=sample_trends_col, group=sample_trends_var)) +
    scale_colour_manual(values=v.colours, breaks=names(v.colours)) +
    theme_bw() + theme(legend.title=element_blank()) +
    xlab("Mean virulence score") +
    ylab("Mean resistance score")
  ggplotly(g)
})