# Load builtin datasets
observeEvent(
  input$dataset_global,
  {
    data_loaded$kleborate <- global_kleborate
    data_loaded$metadata <- global_metadata
    data_loaded$mic_data <- global_mic
    prepare_data_selector(data_loaded$kleborate)
    reset('kleborate_file')
    reset('metadata_file')
    reset('mic_file')
  }
)
observeEvent(
  input$dataset_euscape,
  {
    data_loaded$kleborate <- euscape_kleborate
    data_loaded$metadata <- euscape_metadata
    data_loaded$mic_data <- euscape_mic
    prepare_data_selector(data_loaded$kleborate)
    reset('kleborate_file')
    reset('metadata_file')
    reset('mic_file')
  }
)
# Set up data selection
prepare_data_selector  <- function(d) {
  # Set defaults
  data_selected$resistance_min <- 0
  data_selected$resistance_max <- 3
  data_selected$virulence_min <- 0
  data_selected$virulence_max <- 5
  # Update input sliders
  updateSliderInput(session, 'res_score_range_slider', value=c(data_selected$resistance_min, data_selected$resistance_max))
  updateSliderInput(session, 'vir_score_range_slider', value=c(data_selected$virulence_min, data_selected$virulence_max))
  # Set row selection to all data
  v.species <- unique(d$species)
  data_selected$rows <- rep(TRUE, nrow(d))
  data_selected$species <- c(v.species[v.species %in% v.kpsc_names], 'others')
  # Order species such that KpSC appears first in plots
  d$species <- factor(d$species, levels=c(v.kpsc_names, v.species[! v.species %in% v.kpsc_names]))
}
# Determine file format and read data
read_file <- function(fp, kleborate=FALSE) {
  if (kleborate) {
    d <- read.csv(fp, sep='\t', stringsAsFactors=FALSE)
  } else if (grepl('.csv$', fp)) {
    d <- read.csv(fp, stringsAsFactors=FALSE)
  } else if (grepl('.tsv$', fp)) {
    d <- read.csv(fp, sep='\t', stringsAsFactors=FALSE)
  } else {
    return(NULL)
  }
  return(d)
}
# Process kleborate input
observeEvent(
  input$kleborate_file,
  {
    d <- read_file(input$kleborate_file$datapath, kleborate=TRUE)
    if (is.null(d)) {
      showNotification('Input kleborate file must be in tsv or csv format and have the correct extension', type='error', duration=NULL)
      reset('kleborate_file')
      reset('metadata_file')
      reset('mic_data')
      data_loaded$kleborate <- NULL
      data_loaded$metadata <- NULL
      data_loaded$mic_data <- NULL
      return()
    }
    prepare_data_selector(d)
    data_loaded$kleborate <- d
    # Reset metadata and mic data inputs
    data_loaded$metadata <- NULL
    data_loaded$mic_data <- NULL
    # Reset file input ui
    reset('metadata_file')
    reset('mic_data')
  }
)
# Process metadata input
observeEvent(
  input$metadata_file,
  {
    d <- read_file(input$metadata_file$datapath)
    if (is.null(d)) {
      showNotification('Input metadata file must be in tsv or csv format and have the correct extension', type='error', duration=NULL)
      reset('metadata_file')
      return()
    }
    if (! 'strain' %in% colnames(d)) {
      showNotification('Metadata file must contain a "strain" column', type='error', duration=NULL)
      data_loaded$metadata <- NULL
      reset('metadata_file')
    } else {
      data_loaded$metadata <- d
    }
  }
)
# Process mic input
observeEvent(
  input$mic_file,
  {
    d <- read_file(input$mic_file$datapath)
    if (is.null(d)) {
      showNotification('Input MIC file must be in tsv or csv format and have the correct extension', type='error', duration=NULL)
      reset('mic_file')
      return()
    }
    if (! 'strain' %in% colnames(d)) {
      showNotification('MIC file must contain a "strain" column', type='error', duration=NULL)
      data_loaded$mic_data <- NULL
      reset('mic_file')
    } else {
      data_loaded$mic_data <- d
    }
  }
)