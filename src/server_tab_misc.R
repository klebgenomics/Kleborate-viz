observe({
  # Metadata required for Sample, Temporal trends
  if (is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Temporal trends")
  } else {
    showTab(inputId="primary", target="Sample trends")
    showTab(inputId="primary", target="Temporal trends")
  }
  # Cumulative KO: K/O info in kleborate output and metadata
  # KO diversity: K/O info in kleborate output
  if (all(! c('K_locus', 'O_locus') %in% colnames(data_loaded$kleborate))) {
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
    hideTab(inputId="primary", target="K/O diversity by ST")
  } else {
    showTab(inputId="primary", target="Cumulative K/O prevalence")
    showTab(inputId="primary", target="K/O diversity by ST")
  }
  # MIC data required for MIC by AMR genotype
  if (is.null(data_loaded$mic_data)) {
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="MICs by AMR genotype")
  }
})