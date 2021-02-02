# Dynamic misc. content
# Data summary in side bar
summary_data <- reactive({
  n.vs <- c('Mean virulence score', round(mean(data_loaded$kleborate$virulence_score), 2))
  n.vr <- c('Mean resistance score', round(mean(data_loaded$kleborate$resistance_score), 2))
  n.us <- c('Total unique species', length(unique(data_loaded$kleborate$species)))
  n.st <- c('Total STs', length(unique(data_loaded$kleborate$ST)))
  d <- t(data.frame(n.us, n.st, n.vs, n.vr))
  return(d)
})
output$summary_data <- renderTable(summary_data(), colnames=F)
# List of species for sidebar list
output$species_display_radio_list <- renderUI({
  v.labels <- lapply(v.kpsc_names, function(s.species) {
    paste0(s.species, ' (', sum(data_loaded$kleborate$species==s.species), ')')
  })
  s.others <- paste0('Others (', sum(! data_loaded$kleborate$species %in% v.kpsc_names), ')')
  v.labels_all <- c(v.labels, s.others)
  checkboxGroupInput(
    inputId='species_selector',
    label=' ',
    selected=c(v.kpsc_names, 'others'),
    choiceNames=v.labels_all,
    choiceValues=c(v.kpsc_names, 'others')
  )
})

# User input/selection events
# Species radio
observeEvent(
  input$species_selector, 
  {
    data_selected$species <- input$species_selector
    data_selected$rows <- compute_row_selection()
  }
)
# Resistance, virulence selector
output$res_var_heatmap <- renderPlotly({
  # Check for events
  ed <- event_data('plotly_selected', source='res_vir_heatmap')
  if(is.null(ed) == FALSE && length(ed) > 0 && nrow(ed) > 0) {
    data_selected$resistance_min <- min(ed$y)
    data_selected$resistance_max <- max(ed$y)
    data_selected$virulence_min <- min(ed$x)
    data_selected$virulence_max <- max(ed$x)
    # Immediately clear select event (otherwise it prevents any changes to data_selected res/vir values)
    runjs("Shiny.onInputChange('plotly_selected-res_vir_heatmap', 'null');")
    data_selected$rows <- compute_row_selection()
  }
  
  # Count scores
  d <- table(
    data_loaded$kleborate$resistance_score, 
    data_loaded$kleborate$virulence
  )
  d <- melt(d)
  colnames(d) <- c('Resistance Score', 'Virulence Score', 'Count')
  # Set selected and stroke
  v.selector_res <- d$`Resistance Score` >= data_selected$resistance_min & d$`Resistance Score` <= data_selected$resistance_max
  v.selector_vir <- d$`Virulence Score` >= data_selected$virulence_min & d$`Virulence Score` <= data_selected$virulence_max
  d$selected <- v.selector_res & v.selector_vir
  d$stroke <- ifelse(d$selected, 1, 0)
  # Render plot
  v.colours <- colorRampPalette(c('#f0e9bf', '#f1c280', '#e67d77', '#ED6060'))(100)
  # NOTE: ggplotly does not honor fill for pch=21 points, had to add separate layers for point and outline
  g <- ggplot(d, aes(x=`Virulence Score`, y=`Resistance Score`))
  g <- g + geom_point(aes(stroke=stroke), pch=21, size=6.2)
  g <- g + geom_point(aes(colour=Count), size=5.5)
  g <- g + theme_bw() + theme(legend.position='none', panel.background=element_rect(fill='#f5f5f5'),
                              plot.background=element_rect(fill='#f5f5f5'))
  g <- g + scale_x_continuous(breaks=0:5)
  g <- g + scale_colour_gradientn(colours=v.colours, limits=c(0, max(d$Count)))
  g <- g + expand_limits(x=c(-0.5, 5.5), y=c(-0.5, 3.5))
  g <- ggplotly(g, source='res_vir_heatmap') %>%   
    layout(xaxis=list(fixedrange=TRUE), yaxis=list(fixedrange=TRUE), dragmode='select') %>% 
    config(displayModeBar=FALSE)
  g$x$data[[1]]$hoverinfo <- 'none'
  g$x$data[[2]]$hoverinfo <- 'none'
  g
})
