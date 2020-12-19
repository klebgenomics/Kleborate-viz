# Load builtin datasets
observeEvent(
  input$dataset_global,
  {
    d.kleborate <- kleborate_summaries(global_kleborate)
    data_loaded$kleborate <- d.kleborate
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
    d.kleborate <- kleborate_summaries(euscape_kleborate)
    data_loaded$kleborate <- d.kleborate
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
kleborate_summaries <- function(d) {
  d %>%
    # simplify omp
    mutate(Omp_mutations_simplified = str_replace_all(Omp_mutations, "-[0-9]+%", "-trunc"), Omp_simple = if_else(Omp_mutations == "-", "wt", "mut")) %>%
    # simplify carbapenemases and combine with omp
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "IMP"), "IMP", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "KPC"), "KPC", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "OXA"), "OXA", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "NDM"), "NDM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "VIM"), "VIM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, ";"), "multiple", Bla_Carb_simplified)) %>%
    mutate(carbapenemase_omp_combination = paste(Bla_Carb_simplified, Omp_simple, sep = " ")) %>%
    # simplify ESBLs and combine with omp
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "CTX-M"), "CTX-M-other", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-14", "CTX-M-14", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-15", "CTX-M-15", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-65", "CTX-M-65", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "SHV"), "SHV", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "TEM"), "TEM", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, ";"), "multiple", Bla_ESBL_simplified)) %>%
    mutate(ESBL_omp_combination = paste(Bla_ESBL_simplified, Omp_simple, sep = " ")) %>%
    # simplify bla acquired and combine with omp
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "TEM"), "TEM", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "OXA"), "OXA", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "LAP"), "LAP", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "DHA"), "DHA", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, ";"), "multiple", Bla_acq_simplified)) %>%
    mutate(Bla_acquired_omp_combination = paste(Bla_acq_simplified, Omp_simple, sep = " "))
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
    # Prepare some initial summaries that are used across several plots
    d <- kleborate_summaries(d)
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
    # Set standard year column name
    if ('year' %in% tolower(colnames(d))) {
      v.selector <- which(tolower(colnames(d))=='year')
      colnames(d)[v.selector] <- 'Year'
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