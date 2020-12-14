# Get summary data
dload_listen <- reactive({
  list(data_loaded$kleborate, data_loaded$metadata)
})

observeEvent(
  dload_listen(),
  {
    v.years <- as.numeric(data_loaded$metadata$Year)
    v.years <- v.years[!is.na(v.years)]
    updateSliderInput(session, 'year_range_slider', min=min(v.years), max=max(v.years), value=c(min(v.years), max(v.years)))
  }
)
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
      ESBL_prevalence = 1-(sum(Bla_ESBL_combined == "-")/n()), 
      colistin_resistance_gene_prevalence = 1-(sum(Col_acquired == "-")/n()), 
      colistin_resistance_mutation_prevalence = 1-(sum(Col_mutations == "-")/n()), 
      carbapenemase_prevalence = 1-(sum(Bla_Carb_acquired == "-")/n())
    ) %>%
    filter(Year != "unknown") -> d
  d$Year <- as.numeric(d$Year)
  # Apply filter
  d <- d[d$Year>=input$year_range_slider[1] & d$Year<=input$year_range_slider[2], ]
  return(d)
})
# Mean virulence and resistance scores
output$year_mean_scores_line <- renderPlotly({
  g <- ggplot(metadata_summary_year()) +
    geom_line(aes(x=Year, y = mean_virulence_score), colour='#1855b7') +
    geom_line(aes(x=Year, y = mean_resistance_score), colour='#bb363c') +
    xlab('Year') +
    ylab('Mean score') + theme_bw()
  g <- ggplotly(g)
  print(g)
})
# Prevalence AMR classes and genes by year
output$year_mean_resistance_line <- renderPlotly({ 
  g <- ggplot(metadata_summary_year()) + 
    geom_line(aes(x=Year, y=mean_resistance_genes), colour='#bb363c') +
    geom_line(aes(x=Year, y=mean_resistance_classes), colour='black') +
    xlab('Year') +
    ylab('Mean acquired AMR classes and genes') + 
    theme_bw()
  g <- ggplotly(g)
  print(g)
})

# Prevalence of virulence determinants
output$virulence_prevalence_year_line <- renderPlotly({
  g <- ggplot(metadata_summary_year(), aes(x=Year)) +
    geom_line(aes(x=Year, y=ybt_prevalence), colour='#68b297') +
    geom_line(aes(x=Year, y=clb_prevalence), colour='#377c63') +
    geom_line(aes(x=Year, y=iuc_prevalence), colour='#0a1027') +
    geom_line(aes(x=Year, y=iro_prevalence), colour='#6676f3') +
    geom_line(aes(x=Year, y=rmpADC_prevalence), colour='#1855b7') +
    geom_line(aes(x=Year, y=rmpA2_prevalence), colour='#4292c6') +
    xlab('Year') +
    ylab('Prevalence') + theme_bw()
  g <- ggplotly(g)
  print(g)
})

# Prevalence of AMR determinants
output$AMR_prevalence_year_line <- renderPlotly({
  g <- ggplot(metadata_summary_year(), aes(x=Year)) +
    geom_line(aes(x=Year, y=ESBL_prevalence), colour='#f4bdbd') +
    geom_line(aes(x=Year, y=carbapenemase_prevalence), colour='#f26158') +
    geom_line(aes(x=Year, y=colistin_resistance_mutation_prevalence), colour='#bb363c') +
    geom_line(aes(x=Year, y=colistin_resistance_gene_prevalence), colour='black') +
    xlab('Year') +
    ylab('Prevalence') + theme_bw()
  g <- ggplotly(g)
  print(g)
})