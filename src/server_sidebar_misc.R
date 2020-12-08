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
    label='Select species',
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
# Resistance score slider
observeEvent(
  input$res_score_range_slider,
  {
    data_selected$resistance_min <- input$res_score_range_slider[1]
    data_selected$resistance_max <- input$res_score_range_slider[2]
    data_selected$rows <- compute_row_selection()
  }
)
# Resistance score slider
observeEvent(
  input$vir_score_range_slider,
  {
    data_selected$virulence_min <- input$vir_score_range_slider[1]
    data_selected$virulence_max <- input$vir_score_range_slider[2]
    data_selected$rows <- compute_row_selection()
  }
)
