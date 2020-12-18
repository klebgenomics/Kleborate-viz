observe({
  # Kleborate data required
  # NOTE: probably a better way to access all tabs except 'Welcome'
  if (is.null(data_loaded$kleborate)) {
    hideTab(inputId="primary", target="Summary")
    hideTab(inputId="primary", target="Genotypes by ST")
    hideTab(inputId="primary", target="Genotypes by metadata")
    hideTab(inputId="primary", target="Convergence by ST")
    hideTab(inputId="primary", target="K/O diversity by ST")
    hideTab(inputId="primary", target="Temporal trends")
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="Summary")
    showTab(inputId="primary", target="Genotypes by ST")
    showTab(inputId="primary", target="Convergence by ST")
  }
  # Metadata required
  if (is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Genotypes by metadata")
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Temporal trends")
  } else {
    showTab(inputId="primary", target="Genotypes by metadata")
    showTab(inputId="primary", target="Sample trends")
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