# Cumulative prevalence K locus
output$cumulative_k_line <- renderPlotly({
  g <- ggplot(cumulative_K_data, aes(x=no_K_loci)) +
    geom_step(mapping=aes(y=cf_asia_w), color='#a9d7ed') +
    geom_step(mapping=aes(y=cf_E_Europe), color='#fa9fb5') +
    geom_step(mapping=aes(y=cf_N_Europe), color='#f768a1') +
    geom_step(mapping=aes(y=cf_S_Europe), color='#962e99') +
    geom_step(mapping=aes(y=cf_W_Europe), color='#49006a') +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
# Cumulative prevalence O locus
output$cumulative_o_line <- renderPlotly({
  g <- ggplot(cumulative_O_data, aes(x=no_O_type, y=region)) +
    geom_step(mapping=aes(y=cf_asia_w), color='#a9d7ed') +
    geom_step(mapping=aes(y=cf_E_Europe), color='#fa9fb5') +
    geom_step(mapping=aes(y=cf_N_Europe), color='#f768a1') +
    geom_step(mapping=aes(y=cf_S_Europe), color='#962e99') +
    geom_step(mapping=aes(y=cf_W_Europe), color='#49006a') +
    xlab('Number of O-types') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})