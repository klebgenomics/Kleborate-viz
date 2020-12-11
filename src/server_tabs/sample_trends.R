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
  # the number of colours to generate below should be the number of groups, which will be the number of rows in the table genearted above - but this seems to be doing something weird?
  sample.col <- colorRampPalette(c("#875F9A", "#EDA483", "#56c8f5", "#8CBDB2", "#205c38", "#F0B663", "#ED6060"))(nrow(metadata_summary_sample()))
  g <- ggplot(metadata_summary_sample(), aes(x=mean_virulence_score, y=mean_resistance_score)) +
    geom_point(aes(size=n, color=Source)) +
    scale_colour_manual(values = sample.col) +
    theme_bw() +
    xlab("Mean virulence score") +
    ylab("Mean resistance score")
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
