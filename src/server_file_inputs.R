# Determine file format and read data
read_file <- function(fp) {
  if (grepl('.csv$', fp)) {
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
    d <- read_file(input$kleborate_file$datapath)
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
    # Set row selection to all data
    v.species <- unique(d$species)
    data_selected$rows <- rep(TRUE, nrow(d))
    data_selected$species <- c(v.species[v.species %in% v.kpsc_names], 'others')
    # Order species such that KpSC appears first in plots
    d$species <- factor(d$species, levels=c(v.kpsc_names, v.species[! v.species %in% v.kpsc_names]))
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