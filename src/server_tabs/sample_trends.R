# Get summary data
#metadata_summary_sample <- reactive({
#  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
#    group_by(data_loaded$metadata$input$sample_trends_col, data_loaded$metadata$input$sample_trends_var) %>%
#    summarise(
#      n=n(), 
#      mean_virulence_score = mean(virulence_score), 
#      mean_resistance_score = mean(resistance_score)
#    )
#})

# Get summary data
metadata_summary_sample <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    group_by(Source, `Sample.type`) %>%
    summarise(
      n=n(), 
      mean_virulence_score = mean(virulence_score), 
      mean_resistance_score = mean(resistance_score)
    )
})

# Colour generator
sample_trends_colours <- reactive({
  v.groups <- unique(data_loaded$metadata[[input$sample_trends_col]])
  v.groups <- sort(v.groups)
  v.colours <- misc_colour_palette(length(v.groups))
  names(v.colours) <- v.groups
  return(v.colours)
})


# Group selector
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain', 'Year', 'year')]
    updateSelectInput(session, 'sample_trends_var', choices=v.cols, selected=v.cols[4])
  }
)

# Colour selector
observeEvent(
  data_loaded$metadata,
  {
    c.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain', 'Year', 'year')]
    updateSelectInput(session, 'sample_trends_col', choices=c.cols, selected=c.cols[3])
  }
)

# Plot
#output$prevalence_sample_scatter <- renderPlotly({
#  g <- ggplot(metadata_summary_sample(), aes(x=mean_virulence_score, y=mean_resistance_score)) +
#    geom_point(aes(size=n, color=metadata_summary_sample[[input$sample_trends_col]])) +
#    scale_colour_manual(values = v.colours, breaks=names(v.colours)) +
#    theme_bw() +
#    xlab("Mean virulence score") +
#    ylab("Mean resistance score")
#  g <- ggplotly(g, dynamicTicks=TRUE)
#  print(g)
#})

# Plot
output$prevalence_sample_scatter <- renderPlotly({
  g <- ggplot(metadata_summary_sample(), aes(x=mean_virulence_score, y=mean_resistance_score)) +
    geom_point(aes(size=n, color=Source)) +
#    scale_colour_manual(values = v.colours, breaks=names(v.colours)) +
    theme_bw() + theme(legend.title = element_blank()) +
    xlab("Mean virulence score") +
    ylab("Mean resistance score")
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})