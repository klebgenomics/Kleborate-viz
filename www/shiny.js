// More quickly hide unavailable tabs at load
const tab_names = [
  'Summary by species',
  'Genotypes by ST',
  'Convergence by ST',
  'Genotypes vs metadata',
  'Convergence vs metadata',
  'Temporal trends',
  'Cumulative K/O prevalence',
  'K/O diversity',
  'MICs by AMR genotype'
];
shinyjs.init = function() {
  tab_names.forEach(n => {
    $(`#primary li a[data-value="${n}"]`).parent().hide();
  });
}
