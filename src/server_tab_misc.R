observe({
  # Kleborate data required
  # NOTE: probably a better way to access all tabs except 'Welcome'
  if (is.null(data_loaded$kleborate)) {
    hideTab(inputId="primary", target="Summary by species")
    hideTab(inputId="primary", target="Genotypes by ST")
    hideTab(inputId="primary", target="Genotypes by metadata")
    hideTab(inputId="primary", target="Convergence by ST")
    hideTab(inputId="primary", target="K/O diversity by ST")
    hideTab(inputId="primary", target="Temporal trends")
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="Summary by species")
    showTab(inputId="primary", target="Genotypes by ST")
    showTab(inputId="primary", target="Convergence by ST")
  }
  # Metadata required
  if (is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Genotypes by metadata")
    hideTab(inputId="primary", target="Sample trends")
  } else {
    showTab(inputId="primary", target="Genotypes by metadata")
    showTab(inputId="primary", target="Sample trends")
  }
  # Metadata required with 'Year' column
  if (! 'Year' %in% colnames(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Temporal trends")
  } else {
    showTab(inputId="primary", target="Temporal trends")
  }
  # Kleborate data +/- metadata required
  v.has_ko_locus_info <- all(c('K_locus', 'O_locus') %in% colnames(data_loaded$kleborate))
  if (! v.has_ko_locus_info) {
    hideTab(inputId="primary", target="K/O diversity by ST")
  } else {
    showTab(inputId="primary", target="K/O diversity by ST")
  }
  if (! v.has_ko_locus_info | is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
  } else {
    showTab(inputId="primary", target="Cumulative K/O prevalence")
  }
  # MIC data required
  if (is.null(data_loaded$mic_data)) {
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="MICs by AMR genotype")
  }
})
# Function to select and set appropriate metadata for plots in:
#   1. genotype by ST
#   2. genotype by metadata
#   3. K/O diversity by ST
get_plot_metadata_annotation <- function(d, s.annotation_name) {
  # Get configuration for plot type
  if (s.annotation_name=='virulence_score') {
    # Determine colours and label names
    v.virulence_score_labels <- paste0(names(v.virulence_score_names), ": ", v.virulence_score_names)
    names(v.virulence_score_labels) <- names(v.virulence_score_names)
    v.colours <- v.virulence_score_colours[names(v.virulence_score_labels)]
    names(v.colours) <- v.virulence_score_labels
    # Set annotation column
    d$annotation <- v.virulence_score_labels[as.character(d$virulence_score)]
    s.anno_name <- 'Virulence Score'
  } else if (s.annotation_name=='resistance_score') {
    # Determine colours and label names
    v.resistance_score_labels <- paste0(names(v.resistance_score_names), ": ", v.resistance_score_names)
    names(v.resistance_score_labels) <- names(v.resistance_score_names)
    v.colours <- v.resistance_score_colours[names(v.resistance_score_labels)]
    names(v.colours) <- v.resistance_score_labels
    # Set annotation column
    d$annotation <- v.resistance_score_labels[as.character(d$resistance_score)]
    s.anno_name <- 'Resistance Score'
  } else if (s.annotation_name=='Bla_ESBL_simplified') {
    d$annotation <- d$Bla_ESBL_simplified
    # NOTE: placeholder for colours
    n <- length(unique(d$annotation))
    v.colours <- v.ESBL_allele_colours
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla ESBL'
  } else if (s.annotation_name=='Bla_Carb_simplified') {
    d$annotation <- d$Bla_Carb_simplified
    n <- length(unique(d$annotation))
    v.colours <- v.carb_allele_colours #hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla Carb'
  } else {
    s.annotation_var <- sub('_(presenceabsence|lineages)', '', s.annotation_name)
    s.annotation_type <- str_extract(s.annotation_name, '(presenceabsence|lineages|)$')
    if (s.annotation_name %in% v.virulence_loci) {
      v.colours <- c("grey", "#2171b5")
      s.anno_name <- names(v.virulence_loci)[v.virulence_loci==s.annotation_name]
    } else if (s.annotation_name %in% v.resistance_classes & !v.virulence_loci %in% c('Bla_ESBL_simplified', 'Bla_Carb_simplified')) {
      v.colours <- c("grey", "#ef3b2c")
      s.anno_name <- names(v.resistance_classes)[v.resistance_classes==s.annotation_name]
    } else {
      stop('Got bad annotation variable')
    }
    # Set annotation column
    if (s.annotation_type == 'presenceabsence') {
      d$annotation <- ifelse(d[[s.annotation_var]]=='-', 'absent', 'present')
      names(v.colours) <- c('absent', 'present')
    } else if (s.annotation_type == 'lineages') {
      # TODO: fix colour usage
      d$annotation <- d[[s.annotation_var]]
      v.colours <- setNames(misc_colour_palette(length(unique(d$annotation))), unique(d$annotation))
    } else if (s.annotation_type == '') {
      # TODO: check if this is needed ay completion of implementation
      d$annotation <- ifelse(d[[s.annotation_var]]=='-', 'absent', 'present')
      names(v.colours) <- c('absent', 'present')
    } else if (is.na(s.annotation_type)) {
      stop('Got bad annotation type')
    }
  }
  return(list(d=d, colours=v.colours, anno_name=s.anno_name))
}
