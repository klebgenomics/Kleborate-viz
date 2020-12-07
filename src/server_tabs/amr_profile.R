output$amr_profile_dist <- renderPlotly({
  g <- ggplot(data_AMRSimpli, aes(x=Bla_carb_simplified, y=Meropenem_MIC)) +
    geom_boxplot(outlier.shape=NA) + geom_jitter(aes(colour=Omp_simplified)) + scale_y_continuous(trans='log2') +
    xlab('Reported carbapenemase') +
    ylab('MIC Meropenem') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})