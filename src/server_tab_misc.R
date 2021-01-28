observe({
  # Kleborate data required
  # NOTE: probably a better way to access all tabs except 'Welcome'
  if (is.null(data_loaded$kleborate)) {
    hideTab(inputId="primary", target="Summary by species")
    hideTab(inputId="primary", target="Genotypes by ST")
    hideTab(inputId="primary", target="Genotypes by metadata")
    hideTab(inputId="primary", target="Convergence by ST")
    hideTab(inputId="primary", target="K/O diversity")
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
    hideTab(inputId="primary", target="K/O diversity")
  } else {
    showTab(inputId="primary", target="K/O diversity")
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
    s.display_name <- 'Virulence Score'
  } else if (s.annotation_name=='resistance_score') {
    # Determine colours and label names
    v.resistance_score_labels <- paste0(names(v.resistance_score_names), ": ", v.resistance_score_names)
    names(v.resistance_score_labels) <- names(v.resistance_score_names)
    v.colours <- v.resistance_score_colours[names(v.resistance_score_labels)]
    names(v.colours) <- v.resistance_score_labels
    # Set annotation column
    d$annotation <- v.resistance_score_labels[as.character(d$resistance_score)]
    s.display_name <- 'Resistance Score'
  } else if (s.annotation_name=='clone_type') {
    v.colours <- v.clone_type_colours
    d$annotation <- d$clone_type
    s.display_name <- 'Clone type'
  } else {
    # Here we handle virulence loci and resistance classes annotations; done collectively as they have been 
    # systematically defined
    # Set annotation column
    if (grepl('_pa', s.annotation_name)) {
      s.annotation_var <- sub('_pa$', '', s.annotation_name)
      d$annotation <- ifelse(d[[s.annotation_var]]=='-', 'absent', 'present')
    } else if (grepl('_(simplified|trunc)$', s.annotation_name)) {
      d$annotation <- d[[s.annotation_name]]
    } else {
      stop('Got bad annotation variable')
    }
    # Get annotation colour
    if(s.annotation_name=='ybt_simplified') {
      v.colours <- v.ybt_lineage_colours
    } else if(s.annotation_name=='clb_simplified') {
      v.colours <- v.clb_lineage_colours
    } else if(s.annotation_name=='iuc_simplified') {
      v.colours <- v.iuc_lineage_colours
    } else if(s.annotation_name=='iro_simplified') {
      v.colours <- v.iro_lineage_colours
    } else if(s.annotation_name=='rmpADC_simplified') {
      v.colours <- v.rmpADC_lineage_colours
    } else if (s.annotation_name=='rmpADC_trunc') {
      v.colours <- v.rmpADC_presence_absence_colours
    } else if(s.annotation_name=='rmpA2_trunc') {
      v.colours <- v.rmpA2_presence_absence_colours
    } else if(s.annotation_name=='Bla_ESBL_simplified') {
      v.colours <- v.ESBL_allele_colours
    } else if(s.annotation_name=='Bla_Carb_simplified') {
      v.colours <- v.carb_allele_colours
    } else if (grepl('_pa$', s.annotation_name)) {
      if (s.annotation_name %in% v.virulence_loci) {
        v.colours <- c("grey", "#2171b5")
      } else if (s.annotation_name %in% v.resistance_classes) {
        v.colours <- c("grey", "#ef3b2c")
      } else {
        stop('Got bad annotation var')
      }
    } else {
      stop('Got bad annotation var')
    }
    # Get display name
    if (s.annotation_name %in% v.virulence_loci) {
      s.display_name <- names(v.virulence_loci)[v.virulence_loci==s.annotation_name]
    } else if (s.annotation_name %in% v.resistance_classes) {
      s.display_name <- names(v.resistance_classes)[v.resistance_classes==s.annotation_name]
    } else {
      stop('Got bad annotation var')
    }
  }
  return(list(d=d, colours=v.colours, anno_name=s.display_name))
}
