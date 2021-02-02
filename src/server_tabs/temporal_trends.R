# Get summary data
dload_listen <- reactive({
  list(data_loaded$kleborate, data_loaded$metadata)
})
observeEvent(
  dload_listen(),
  {
    v.years <- as.numeric(data_loaded$metadata$Year)
    v.years <- v.years[!is.na(v.years)]
    temporal_year_selection$min <- min(v.years)
    temporal_year_selection$max <- max(v.years)
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
    ) -> d
  # Cast to numeric and filter non-numerics
  d$Year <- as.numeric(d$Year)
  d <- d[!is.na(d$Year), ]
  # Apply filter and melt data for plotting
  d <- d[d$Year>=temporal_year_selection$min & d$Year<=temporal_year_selection$max, ]
  d <- melt(d, id.vars=c('Year', 'n'))
  return(d)
})

# User input/histogram
temporal_year_selection <- reactiveValues(min=NULL, max=NULL)
output$temporal_trends_year_hist <- renderPlotly({
  # Do not attempt to plot unless we have defined user inputs
  if (is.null(temporal_year_selection$min) || is.null(temporal_year_selection$max)) {
    return()
  }
  # Handle selection event
  ed <- event_data('plotly_selected', source='temporal_trend_year_hist')
  if(is.null(ed) == FALSE && length(ed) > 0 && nrow(ed) > 0) {
    if (all(ed$y==0)) {
      # Select all years
    } else if (sum(ed$y>0) > 1) {
      # Set new range selection as long as same year isn't selected twice
      temporal_year_selection$min <- floor(min(ed$x))
      temporal_year_selection$max <- ceiling(max(ed$x))
    }
    # Immediately clear select event
    runjs("Shiny.onInputChange('plotly_selected-temporal_trend_year_hist', 'null');")
    data_selected$rows <- compute_row_selection()
  }
  # Get data
  inner_join(data_loaded$metadata, data_loaded$kleborate) %>%
    mutate(Bla_ESBL_combined = if_else(Bla_ESBL_acquired == "-" & Bla_ESBL_inhR_acquired == "-", "-", "esbl")) %>%
    group_by(Year) %>%
    summarise(
      n=n()
    ) -> d
  d$Year <- as.numeric(d$Year)
  d <- d[!is.na(d$Year), ]
  # Set break size
  n.break_size <- max(round((max(d$Year) - min(d$Year)) / 10, 0), 1)
  # Define selected years to annotate
  d$selected <- ifelse(d$Year>=temporal_year_selection$min & d$Year<=temporal_year_selection$max, 'yes', 'no')
  # Plot
  g <- ggplot(d, aes(x=Year, y=n, fill=selected)) + geom_histogram(stat='identity')
  g <- g + theme_bw() + theme(legend.position='none', axis.text.x=element_text(angle=45, hjust=1)) + ylab('') + xlab('')
  g <- g + scale_x_continuous(breaks=seq(min(d$Year), max(d$Year), n.break_size))
  g <- g + scale_fill_manual(breaks=c('yes', 'no'), values=c('grey30', 'grey60'))
  # NOTE: I wanted to add shaded background to indicate selection but rect annotation (and geom_rect) are not honoured by ggplotly
  # g <- g + annotate('rect', xmin=min(d$Year), xmax=max(d$Year), ymin=-Inf, ymax=Inf, alpha=0.2, fill='red')
  ggplotly(g, source='temporal_trend_year_hist') %>%
    layout(xaxis=list(fixedrange=TRUE), yaxis=list(fixedrange=TRUE), dragmode='select', selectdirection='h') %>%
    config(displayModeBar=FALSE)
})

# Generalised plotting functions
temporal_trend_data <- function(v.name_map) {
  # Get and select data
  d <- metadata_summary_year()
  d <- d[d$variable %in% names(v.name_map), ]
  d$variable <- v.name_map[as.character(d$variable)]
  d <- d[ ,c('Year', 'value', 'variable')]
  return(d)
}
temporal_trend_plot <- function(d, v.colours, s.ylab) {
  # Do not attempt to plot unless we have defined user inputs
  if (is.null(temporal_year_selection$min) || is.null(temporal_year_selection$max)) {
    return()
  }
  # Plot
  g <- ggplot(d, aes(x=Year, y=value, colour=variable)) + geom_line()
  g <- g + xlab('Year') + ylab(s.ylab) 
  g <- g + theme_bw()
  g <- g + scale_colour_manual(values=v.colours, breaks=names(v.colours), name='Legend')
  ggplotly(g)
}

# NOTE: functions below are split to allow downloading on data and plots, which each require callable functions without args
# Mean virulence and resistance scores
output$year_mean_scores_line_plot <- renderPlotly(year_mean_scores_line_plot())
year_mean_scores_line_data <- reactive({
  v.name_map <- c(
    'mean_resistance_score'='Mean resistance score',
    'mean_virulence_score'='Mean virulence score'
  )
  temporal_trend_data(v.name_map)
})
year_mean_scores_line_plot <- reactive({
  v.colours <- c(
    'Mean resistance score'='#bb363c', 
    'Mean virulence score'='#1855b7'
  )
  temporal_trend_plot(year_mean_scores_line_data(), v.colours, 'Mean score')
})

# Prevalence AMR classes and genes by year
output$year_mean_resistance_line_plot <- renderPlotly(year_mean_resistance_line_plot())
year_mean_resistance_line_data <- reactive({
  v.name_map <- c(
    'mean_resistance_genes'='Mean resistance genes',
    'mean_resistance_classes'='Mean resistance classes'
  )
  temporal_trend_data(v.name_map)
})
year_mean_resistance_line_plot <- reactive({
  v.colours <- c(
    'Mean resistance genes'='#bb363c',
    'Mean resistance classes'='black'
  )
  temporal_trend_plot(year_mean_resistance_line_data(), v.colours, 'Mean acquired AMR classes and genes')
})

# Prevalence of virulence determinants
output$virulence_prevalence_year_line_plot <- renderPlotly(virulence_prevalence_year_line_plot())
virulence_prevalence_year_line_data <- reactive({
  v.name_map <- c(
    'ybt_prevalence'='YBT prevalance',
    'clb_prevalence'='CLB prevalance',
    'iuc_prevalence'='IUC prevalance',
    'iro_prevalence'='IRO prevalance',
    'rmpADC_prevalence'='rmpADC prevalance',
    'rmpA2_prevalence'='rmpA2 prevalance'
  )
  temporal_trend_data(v.name_map)
})
virulence_prevalence_year_line_plot <- reactive({
  v.colours <- c(
    'YBT prevalance'='#68b297',
    'CLB prevalance'='#377c63',
    'IUC prevalance'='#0a1027',
    'IRO prevalance'='#6676f3',
    'rmpADC prevalance'='#1855b7',
    'rmpA2 prevalance'='#4292c6'
  )
  temporal_trend_plot(virulence_prevalence_year_line_data(), v.colours, 'Prevalance')
})

# Prevalence of AMR determinants
output$AMR_prevalence_year_line_plot <- renderPlotly(AMR_prevalence_year_line_plot())
AMR_prevalence_year_line_data <- reactive({
  v.name_map <- c(
    'ESBL_prevalence'='ESBL prevalance',
    'carbapenemase_prevalence'='Carbapenemase prevalance',
    'colistin_resistance_mutation_prevalence'='Colistin mutation prevalance',
    'colistin_resistance_gene_prevalence'='Colistin gene prevalance'
  )
  temporal_trend_data(v.name_map)
})
AMR_prevalence_year_line_plot <- reactive({
  v.colours <- c(
    'ESBL prevalance'='#f4bdbd',
    'Carbapenemase prevalance'='#f26158',
    'Colistin mutation prevalance'='#bb363c',
    'Colistin gene prevalance'='black'
  )
  temporal_trend_plot(AMR_prevalence_year_line_data(), v.colours, 'Prevalance')
})

# Download data/plot
# Virulence and resistance scores
output$year_mean_scores_line_data_download <- downloadHandler(
  filename=reactive(download_filename('year_mean_scores_line', 'csv')),
  content=function(s.filename) { write.csv(year_mean_scores_line_data(), s.filename, row.names=FALSE) }
)
output$year_mean_scores_line_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'pdf')),
  content=function(s.filename) { download_plot(year_mean_scores_line_plot, s.filename) }
)
observeEvent(input$year_mean_scores_line_plot_download_show, {
  download_modal(downloadButton('year_mean_scores_line_plot_download', class='btn-primary'))
})
# Acquired AMR classes and genes by year
output$year_mean_resistance_line_data_download <- downloadHandler(
  filename=reactive(download_filename('year_mean_resistance_line', 'csv')),
  content=function(s.filename) { write.csv(year_mean_resistance_line_data(), s.filename, row.names=FALSE) }
)
output$year_mean_resistance_line_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'pdf')),
  content=function(s.filename) { download_plot(year_mean_resistance_line_plot, s.filename) }
)
observeEvent(input$year_mean_resistance_line_plot_download_show, {
  download_modal(downloadButton('year_mean_resistance_line_plot_download', class='btn-primary'))
})
# Virulence determinant prevalence by year
output$virulence_prevalence_year_line_data_download <- downloadHandler(
  filename=reactive(download_filename('virulence_prevalence_year_line', 'csv')),
  content=function(s.filename) { write.csv(virulence_prevalence_year_line_data(), s.filename, row.names=FALSE) }
)
output$virulence_prevalence_year_line_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'pdf')),
  content=function(s.filename) { download_plot(virulence_prevalence_year_line_plot, s.filename) }
)
observeEvent(input$virulence_prevalence_year_line_plot_download_show, {
  download_modal(downloadButton('virulence_prevalence_year_line_plot_download', class='btn-primary'))
})
# AMR determinant prevalence by year
output$AMR_prevalence_year_line_data_download <- downloadHandler(
  filename=reactive(download_filename('AMR_prevalence_year_line', 'csv')),
  content=function(s.filename) { write.csv(AMR_prevalence_year_line_data(), s.filename, row.names=FALSE) }
)
output$AMR_prevalence_year_line_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'pdf')),
  content=function(s.filename) { download_plot(AMR_prevalence_year_line_plot, s.filename) }
)
observeEvent(input$AMR_prevalence_year_line_plot_download_show, {
  download_modal(downloadButton('AMR_prevalence_year_line_plot_download', class='btn-primary'))
})