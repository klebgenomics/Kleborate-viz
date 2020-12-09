# Get summary data
metadata_summary_sample <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    group_by(Source, Classification) %>%
    summarise(
      n=n(), 
      mean_virulence_score = mean(virulence_score), 
      mean_resistance_score = mean(resistance_score)
    )
})
# Plot
output$prevalence_sample_scatter <- renderPlotly({
  # TODO: selection by type (i.e. variables other than sample)
  g <- ggplot(metadata_summary_sample(), aes(x=mean_virulence_score, y=mean_resistance_score)) +
    geom_point(aes(size=n, color=Source)) +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
