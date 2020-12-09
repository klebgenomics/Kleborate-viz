observe({
  # Only show metadata tabs when we have metadata
  if (is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="Sample trends")
    hideTab(inputId="primary", target="Temporal trends")
    hideTab(inputId="primary", target="Cumulative K/O prevalence")
  } else {
    showTab(inputId="primary", target="Sample trends")
    showTab(inputId="primary", target="Temporal trends")
    showTab(inputId="primary", target="Cumulative K/O prevalence")
  }
  # Only show AMR profile when we have both metadata and mic
  if (is.null(data_loaded$mic_data) | is.null(data_loaded$metadata)) {
    hideTab(inputId="primary", target="MICs by AMR genotype")
  } else {
    showTab(inputId="primary", target="MICs by AMR genotype")
  }
})