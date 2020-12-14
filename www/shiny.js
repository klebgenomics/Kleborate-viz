// More quickly hide unavailable tabs at load
shinyjs.init = function() { 
  $('#primary li a[data-value="Summary by species"]').parent().hide();
  $('#primary li a[data-value="Genotypes by ST"]').parent().hide();
  $('#primary li a[data-value="Genotypes by metadata"]').parent().hide();
  $('#primary li a[data-value="Convergence by ST"]').parent().hide();
  $('#primary li a[data-value="K/O diversity by ST"]').parent().hide();
  $('#primary li a[data-value="Temporal trends"]').parent().hide();
  $('#primary li a[data-value="Sample trends"]').parent().hide();
  $('#primary li a[data-value="Cumulative K/O prevalence"]').parent().hide();
  $('#primary li a[data-value="MICs by AMR genotype"]').parent().hide();
}