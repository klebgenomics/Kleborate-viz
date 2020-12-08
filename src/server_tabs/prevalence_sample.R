output$prevalence_sample_scatter <- renderPlotly({
  # TODO: selection by type (i.e. variables other than sample)
  g <- ggplot(sample_vir_res, aes(x=mean_vir, y=mean_res)) +
    geom_point(aes(size=total, color=sample)) +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
