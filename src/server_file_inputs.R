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
read_file <- function(fp, input_name) {
  if (file.info(fp)$size < 1) {
    showNotification(
      paste('Input', input_name, 'file does not contain any data'),
      type='error',
      duration=NULL
    )
    return(NULL)
  }
  if (input_name=='Kleborate' & grepl('.txt$', fp)) {
    d <- read.csv(fp, sep='\t', stringsAsFactors=FALSE)
  } else if (grepl('.csv$', fp)) {
    d <- read.csv(fp, stringsAsFactors=FALSE)
  } else if (grepl('.tsv$', fp)) {
    d <- read.csv(fp, sep='\t', stringsAsFactors=FALSE)
  } else {
    showNotification(
      paste('Input', input_name, 'file must be in tsv or csv format and have the correct extension'),
      type='error', duration=NULL
    )
    return(NULL)
  }
  # Require some data, any data
  if (nrow(d) < 1) {
    showNotification(
      paste('Input', input_name, 'file did not have any rows'),
      type='error',
      duration=NULL
    )
    kleborate_reset()
    return(NULL)
  }
  if (ncol(d) < 1) {
    showNotification(
      paste('Input', input_name, 'file did not have any columns'),
      type='error',
      duration=NULL
    )
    return(NULL)
  }
  return(d)
}
# Process kleborate input
kleborate_reset <- function() {
  data_loaded$kleborate <- NULL
  data_loaded$metadata <- NULL
  data_loaded$mic_data <- NULL
  reset('kleborate_file')
  reset('metadata_file')
  reset('mic_file')
}
kleborate_validate <- function(d) {
  if (! all(v.kleborate_columns_required_base %in% colnames(d))) {
    # First check presence of default columns
    showNotification('Input Kleborate file did not contain required columns', type='error', duration=NULL)
    return(FALSE)
  } else if (! 'resistance_score' %in% colnames(d)) {
    # Resistance info
    showNotification('Kleborate-viz requires that the input Kleborate file contains resistance info', type='error', duration=NULL)
    return(FALSE)
  } else if (! all(v.kleborate_columns_required_res %in% colnames(d))) {
    # Second check for presence of resistance columns
    showNotification('Input Kleborate file did not contain required columns', type='error', duration=NULL)
    return(FALSE)
  } else {
    return(TRUE)
  }
}
observeEvent(
  input$kleborate_file,
  {
    # Read in file and perform validation
    d <- read_file(input$kleborate_file$datapath, 'Kleborate')
    # Short circuit eval
    if (is.null(d) || ! kleborate_validate(d)) {
      kleborate_reset()
      return()
    }
    # Rename strain column if input from pathogenwatch
    if (any(grepl('Genome.Name', colnames(d)))) {
      colnames(d)[grepl('Genome.Name', colnames(d))] <- 'strain'
    }
    # Set as loaded data
    prepare_data_selector(d)
    data_loaded$kleborate <- d
    # Reset metadata and mic data inputs
    data_loaded$metadata <- NULL
    data_loaded$mic_data <- NULL
    # Reset file input ui
    reset('metadata_file')
    reset('mic_file')
  }
)
# Process metadata input
observeEvent(
  input$metadata_file,
  {
    # Read in file and perform basic validation
    d <- read_file(input$metadata_file$datapath, 'metadata')
    if (is.null(d)) {
      data_loaded$metadata <- NULL
      reset('metadata_file') 
      return()
    } 
    if (! 'strain' %in% colnames(d)) {
      showNotification('Metadata file must contain a "strain" column', type='error', duration=NULL)
      data_loaded$metadata <- NULL
      reset('metadata_file')
      return()
    }
    data_loaded$metadata <- d
  }
)
# Process mic input
observeEvent(
  input$mic_file,
  {
    # Read in file and perform basic validation
    d <- read_file(input$mic_file$datapath, 'MIC')
    if (is.null(d)) {
      data_loaded$mic_data <- NULL
      reset('mic_file')
      return()
    }
    if (! 'strain' %in% colnames(d)) {
      showNotification('MIC file must contain a "strain" column', type='error', duration=NULL)
      data_loaded$mic_data <- NULL
      reset('mic_file')
      return()
    } 
    data_loaded$mic_data <- d
  }
)