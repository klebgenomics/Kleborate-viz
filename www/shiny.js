// More quickly hide unavailable tabs at load
shinyjs.init = function() { 
  $('#primary li a[data-value="Summary by species"]').hide();
  $('#primary li a[data-value="Genotypes by ST"]').hide();
  $('#primary li a[data-value="Genotypes by metadata"]').hide();
  $('#primary li a[data-value="Convergence by ST"]').hide();
  $('#primary li a[data-value="K/O diversity by ST"]').hide();
  $('#primary li a[data-value="Temporal trends"]').hide();
  $('#primary li a[data-value="Sample trends"]').hide();
  $('#primary li a[data-value="Cumulative K/O prevalence"]').hide();
  $('#primary li a[data-value="MICs by AMR genotype"]').hide();
}