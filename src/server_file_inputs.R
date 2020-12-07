# File inputs
kleborate_data <- reactive({
  if (is.null(input$kleborate_file)) {
    d <- kleborate_data_default
  } else {
    d <- read.csv(input$kleborate_file$datapath, sep='\t', stringsAsFactors=FALSE)
  }
  # Set row selection to all data
  v.species <- unique(d$species)
  data_selected$rows <- rep(TRUE, nrow(d))
  data_selected$species <- c(v.species[v.species %in% v.kpsc_names], 'others')
  # Order species such that KpSC appears first in plots
  d$species <- factor(d$species, levels=c(v.kpsc_names, v.species[! v.species %in% v.kpsc_names]))
  return(d)
})
metadata <- reactive({
  if (is.null(input$metadata_file)) {
    return(metadata_default)
  } else {
    d <- read.csv(input$metadata_file$datapath, sep='\t')
    return(d)
  }
})
mic_data <- reactive({
  if (is.null(input$mic_file)) {
    return(mic_data_default)
  } else {
    d <- read.csv(input$mic_file$datapath, sep='\t')
    return(d)
  }
})