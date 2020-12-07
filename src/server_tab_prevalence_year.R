# Mean virulence score
output$prevalence_year_virulence_bar <- renderPlotly({
  g <- ggplot(year_vir_res, aes(x=year)) +
    geom_bar(aes(weight=virulence_score)) +
    xlab('Year') +
    ylab('Mean virulence score') + theme_bw()
  g <- ggplotly(g)
  print(g)
})
# Mean resistance score
output$prevalence_year_resistance_bar <- renderPlotly({
  g <- ggplot(year_vir_res, aes(x=year)) +
    geom_bar(aes(weight=resistance_score)) +
    xlab('Year') +
    ylab('Mean resistance score') + theme_bw()
  g <- ggplotly(g)
  print(g)
})
# Prevalence AMR classes and genes by year
output$prevalence_year_resistance_line <- renderPlotly({ 
  g <- ggplot(year_vir_res, aes(x=year)) +
    geom_line(aes(y=num_resistance_genes), color='red') +
    geom_line(aes(y=num_resistance_classes), color='black') +
    xlab('Year') +
    ylab('Mean AMR classes and genes') + theme_bw()
  g <- ggplotly(g)
  print(g)
})