observe({
  # Metadata required for Sample, Temporal trends
  if (is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Temporal trends")
  } else {
    showTab(inputId="primary", target="Sample trends")
    showTab(inputId="primary", target="Temporal trends")
  }
  # Metadata and presence of K/O locus info in kleborate input required for K/O locus prevalence
  if (is.null(data_loaded$metadata) & all(! c('K_locus', 'O_locus') %in% colnames(data_loaded$kleborate))) {
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
  } else {
    showTab(inputId="primary", target="Cumulative K/O prevalence")
  }
  # MIC data required for MIC by AMR genotype
  if (is.null(data_loaded$mic_data)) {
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="MICs by AMR genotype")
  }
})