# Get summary data
metadata_summary_year <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(Bla_ESBL_combined = if_else(Bla_ESBL_acquired == "-" & Bla_ESBL_inhR_acquired == "-", "-", "esbl")) %>%
    group_by(Year) %>%
    summarise(
      n=n(), 
      mean_virulence_score = mean(virulence_score), 
      mean_resistance_score = mean(resistance_score), 
      mean_resistance_genes = mean(num_resistance_genes), 
      mean_resistance_classes = mean(num_resistance_classes), 
      ybt_prevalence = 1-(sum(Yersiniabactin == "-")/n()), 
      clb_prevalence = 1-(sum(Colibactin == "-")/n()), 
      iuc_prevalence = 1-(sum(Aerobactin == "-")/n()), 
      iro_prevalence = 1-(sum(Salmochelin == "-")/n()), 
      rmpADC_prevalence = 1-(sum(RmpADC == "-")/n()), 
      rmpA2_prevalence = 1-(sum(rmpA2 == "-")/n()), 
      esbl_prevalence = 1-(sum(Bla_ESBL_combined == "-")/n()), 
      col_gene_prevalence = 1-(sum(Col_acquired == "-")/n()), 
      col_mut_prevalence = 1-(sum(Col_mutations == "-")/n()), 
      carb_prevalence = 1-(sum(Bla_Carb_acquired == "-")/n())
    ) %>%
    filter(Year != "unknown") -> d
  d$Year <- as.numeric(d$Year)
  return(d)
})
# Mean virulence score
output$prevalence_year_virulence_bar <- renderPlotly({
  g <- ggplot(metadata_summary_year(), aes(x=Year)) +
    geom_bar(aes(weight=mean_virulence_score)) +
    xlab('Year') +
    ylab('Mean virulence score') + theme_bw()
  g <- ggplotly(g)
  print(g)
})
# Mean resistance score
output$prevalence_year_resistance_bar <- renderPlotly({
  g <- ggplot(metadata_summary_year(), aes(x=Year)) +
    geom_bar(aes(weight=mean_resistance_score)) +
    xlab('Year') +
    ylab('Mean resistance score') + theme_bw()
  g <- ggplotly(g)
  print(g)
})
# Prevalence AMR classes and genes by year
output$prevalence_year_resistance_line <- renderPlotly({ 
  g <- ggplot(metadata_summary_year()) + 
    geom_line(aes(x=Year, y=mean_resistance_genes), colour='red') +
    geom_line(aes(x=Year, y=mean_resistance_classes), colour='black') +
    xlab('Year') +
    ylab('Mean AMR classes and acquired genes') + 
    theme_bw()
  g <- ggplotly(g)
  print(g)
})
