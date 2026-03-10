# Packages
library(shiny)
library(shinyBS)
library(shinyjs)
# Misc
library(tidyverse)
library(reshape2)
library(vegan)  # diversity measurements
# Plotting
library(plotly)
library(heatmaply)

# Set maximum upload file size
options(shiny.maxRequestSize=50*1024^2)

# Builtin datasets, preloading
# Global
global_kleborate <- read.csv('data/global_kleborate.txt', sep='\t', stringsAsFactors=FALSE)
global_metadata <- read.csv('data/global_metadata.csv', stringsAsFactors=FALSE)
global_mic <- NULL
# EUSCAPE
euscape_kleborate <- read.csv('data/euscape_kleborate.txt', sep='\t', stringsAsFactors=FALSE)
euscape_metadata <- read.csv('data/euscape_metadata.csv', stringsAsFactors=FALSE)
euscape_mic <- read.csv('data/euscape_mic.csv', stringsAsFactors=FALSE)

# Kleborate require columns
v.kleborate_columns_required_base <- c(
  'virulence_score',
  'Yersiniabactin',
  'Colibactin',
  'Aerobactin',
  'Salmochelin',
  'RmpADC',
  'rmpA2'
)
v.kleborate_columns_required_res <- c(
  'resistance_score',
  'resistance_gene_count',
  'resistance_class_count',
  'AGly_acquired',
  'Col_acquired',
  'Fcyn_acquired',
  'Flq_acquired',
  'Gly_acquired',
  'MLS_acquired',
  'Phe_acquired',
  'Rif_acquired',
  'Sul_acquired',
  'Tet_acquired',
  'Tgc_acquired',
  'Tmt_acquired',
  'Bla_acquired',
  'Bla_inhR_acquired',
  'Bla_ESBL_acquired',
  'Bla_ESBL_inhR_acquired',
  'Bla_Carb_acquired',
  'Bla_chr',
  'Omp_mutations',
  'Col_mutations'
)

# Kleborate v3.x+ provides additional optional columns
v.kleborate_columns_optional_v3 <- c(
  'Genome.ID',           # Unique genome identifier
  'Kleborate.version',   # Version of Kleborate used for analysis
  'Wrapper.version',     # Version of wrapper script
  'clonal_complex',      # Clonal complex classification (useful for epidemiological grouping)
  'K_type',              # Capsule (K) serotype
  'O_type',              # Lipopolysaccharide (O) serotype
  'gapA', 'infB', 'mdh', 'pgi', 'phoE', 'rpoB', 'tonB'  # MLST allele numbers
)

# Additional columns for quality control in Kleborate v3.x+
v.kleborate_columns_spurious_hits <- c(
  'spurious_ybt_hits',         # Spurious Yersiniabactin hits detected
  'spurious_clb_hits',         # Spurious Colibactin hits detected
  'spurious_abst_hits',        # Spurious Aerobactin hits detected
  'spurious_smst_hits',        # Spurious Salmochelin hits detected
  'spurious_rmst_hits',        # Spurious RmpADC hits detected
  'spurious_virulence_hits',   # Spurious virulence gene hits detected
  'spurious_resistance_hits'   # Spurious resistance gene hits detected
)

# Names
v.kpsc_names <- c(
  'Klebsiella pneumoniae',
  'Klebsiella quasipneumoniae subsp. quasipneumoniae',
  'Klebsiella quasipneumoniae subsp. similipneumoniae',
  'Klebsiella variicola subsp. variicola',
  'Klebsiella variicola subsp. tropica',
  'Klebsiella quasivariicola',
  'Klebsiella africana'
)
# NOTE: value format for virulence loci and resistance classes vectors is:
#   <column_name> or <column_name>_<annotation_type>
# Some column names implicitly encode <annotation_type> and thus it is not required
v.virulence_loci <- list(
  'Yersiniabactin'='Yersiniabactin_pa',
  'Yersiniabactin (lineages)'='ybt_simplified',
  'Colibactin'='Colibactin_pa',
  'Colibactin (lineages)'='clb_simplified',
  'Aerobactin'='Aerobactin_pa',
  'Aerobactin (lineages)'='iuc_simplified',
  'Salmochelin'='Salmochelin_pa',
  'Salmochelin (lineages)'='iro_simplified',
  'RmpADC'='RmpADC_pa',
  'RmpADC (lineages)'='rmpADC_simplified',
  'RmpADC (truncations)'='rmpADC_trunc',
  'RmpA2'='rmpA2_pa',
  'RmpA2 (truncations)'='rmpA2_trunc'
)
v.resistance_classes <- list(
  'AGly'='AGly_acquired_pa',
  'Col (genes)'='Col_acquired_pa',
  'Col (mutations)' = 'Col_mutations_pa',
  'Fcyn'='Fcyn_acquired_pa',
  'Flq (genes)'='Flq_acquired_pa',
  'Flq (mutations)' = 'Flq_mutations_pa',
  'Gly'='Gly_acquired_pa',
  'MLS'='MLS_acquired_pa',
  'Phe'='Phe_acquired_pa',
  'Rif'='Rif_acquired_pa',
  'Sul'='Sul_acquired_pa',
  'Tet'='Tet_acquired_pa',
  'Tmt'='Tmt_acquired_pa',
  'Tgc'='Tgc_acquired_pa',
  'Bla'='Bla_acquired_pa',
  'Bla inhR'='Bla_inhR_acquired_pa',
  'Bla ESBL'='Bla_ESBL_acquired_pa',
  'Bla Carb'='Bla_Carb_acquired_pa',
  'Bla ESBL inhR'='Bla_ESBL_inhR_acquired_pa',
  'Bla ESBL (alleles)'='Bla_ESBL_simplified',
  'Bla Carb (alleles)'='Bla_Carb_simplified'
)
v.genotype_var_choices <- list(
  'Summary'=list(
    'Clone type'='clone_type',
    'Resistance score'='resistance_score',
    'Virulence score'='virulence_score'    
  ),
  'Serotypes'=list(
    'K type'='K_type',
    'O type'='O_type'
  ),
  'Virulence'=v.virulence_loci,
  'Resistance'=v.resistance_classes,
  'Additional Mutations'=list(
    'SHV β-lactamase mutations'='SHV_mutations'
  )
)
v.virulence_score_names <- list(
  '0'='None',
  '1'='ybt',
  '2'='ybt + clb',
  '3'='iuc (VP)',
  '4'='ybt + iuc (VP)',
  '5'='ybt + clb + iuc (VP)'
)
v.resistance_score_names <- c(
  '0'='ESBL-, Carb-',
  '1'='ESBL+, Carb-',
  '2'='Carb+',
  '3'='Carb+, Col+'
)

# Clone type definitions
v.MDR_clones_list = c(
  "ST11",
  "ST258",
  "ST512",
  "ST14",
  "ST15",
  "ST17",
  "ST20",
  "ST29",
  "ST37",
  "ST101",
  "ST147",
  "ST307"
)
v.hv_clones_list = c(
  "ST23",
  "ST86",
  "ST66",
  "ST380",
  "ST25",
  "ST65"
)

# Colours
v.kpsc_colours <- c(
  'Klebsiella pneumoniae'='#E67E78',
  'Klebsiella quasipneumoniae subsp. quasipneumoniae'='#EF9D45',
  'Klebsiella quasipneumoniae subsp. similipneumoniae'='#E9DD50',
  'Klebsiella variicola subsp. variicola'='#79BEA8',
  'Klebsiella variicola subsp. tropica'='#1c8563',
  'Klebsiella quasivariicola'='#607fe6',
  'Klebsiella africana'='#875F9A'
)
other_species_colour_palette <- colorRampPalette(
  c(
  '#f7f7f7',
  '#d9d9d9',
  '#bdbdbd',
  '#969696',
  '#636363',
  '#252525',
  '#000000'        
  )
)
#    '#e85a5a',
#    '#ffffbf',
#    '#5B6894',
#    '#5B6899',
#   '#fdae61',
#    '#66bd63',
#    '#fee08b',
#    '#5B6896',
#    '#875F9A',
#    '#5B6893',
#    '#876738',
#    '#c2bebe',
#    '#a6d96a',
#    '#f6fa7d',

misc_colour_palette <- colorRampPalette(
  c(
    '#E67E78',
    '#EF9D45',
    '#E9DD50',
    '#79BEA8',
    '#3F57A7',
    '#885F9A',
    '#757C82'
  )
)
v.virulence_score_colours <- c(
  '0'='grey', 
  '1'='#79BEA8', 
  '2'='#448D76', 
  '3'='#798EF6',
  '4'='#183BF0', 
  '5'='#091534'
)
v.resistance_score_colours <- c(
  '0'='grey',
  '1'='#f7cac9',
  '2'='#f7786b',
  '3'='#c94c4c'
)
v.clone_type_colours <- c(
  'MDR'='#bb363c',
  'Hv'='#1855b7',
  'unassigned'='#c1bfbf'
)
v.ESBL_allele_colours <- c(
  '-'='#BCBCBC',
  'CTX-M-14'='#E56EAF',
  'CTX-M-15'='#056658',
  'CTX-M-65'='#341F75',
  'CTX-M-other'='#E7298A',
  'SHV'='#71B9D8',
  'TEM'='#E89400',
  'multiple'='#093F7A',
  'other'='#EAC800'
)
v.carb_allele_colours <- c(
  '-' = '#BCBCBC',
  'IMP'='#E56EAF',
  'KPC'='#056658',
  'NDM'='#341F75',
  'OXA'='#E7298A',
  'VIM'='#71B9D8',
  'multiple'='#093F7A',
  'other'='#EAC800'
)
v.ybt_lineage_colours <- c(
  '-'='#EAEAEA',
  'ybt 0'='#c1bfbf',
  'ybt 1'='#b27f91',
  'ybt 2'='#cda12c',
  'ybt 3'='#56a354',
  'ybt 4'='#f28fa2',
  'ybt 5'='#db7723',
  'ybt 6'='#93539d',
  'ybt 7'='#3a85a8',
  'ybt 8'='#7b75cc',
  'ybt 9'='#d9c5ef',
  'ybt 10'='#449d72',
  'ybt 11'='#ebd930',
  'ybt 12'='#6aa3c6',
  'ybt 13'='#a39f93',
  'ybt 14'='#93539d',
  'ybt 15'='#edc59a',
  'ybt 16'='#840639',
  'ybt 17'='#e25065',
  'ybt unknown'='#c1bfbf'
)
v.clb_lineage_colours <- c(
  '-'='#EAEAEA',
  'clb 1'='#6aa3c6',
  'clb 2'='#b27f91',
  'clb 3'='#e25065',
  'clb unknown'='#c1bfbf'
)
v.iro_lineage_colours <- c(
  '-'='#EAEAEA',
  'iro 1'='#e31a1c',
  'iro 2'='#1f78b4',
  'iro 3'='#984ea3',
  'iro 4'='#ff7f00',
  'iro 5'='#fb9a99',
  'iro  unknown'='#c1bfbf',
  'multiple iro'='#000000'
)
v.iuc_lineage_colours <- c(
  '-'='#EAEAEA',
  'iuc 1'='#e31a1c',
  'iuc 2'='#1f78b4',
  'iuc 3'='#33a02c',
  'iuc 4'='#cab2d6',
  'iuc 5'='#fb9a99',
  'iuc 2A'='#a6cee3',
  'iuc unkown'='#c1bfbf',
  'multiple iuc'='#000000'
)
v.rmpADC_lineage_colours <- c(
  '-'='#EAEAEA',
  'rmp 1'='#e31a1c',
  'rmp 2'='#1f78b4',
  'rmp 3'='#984ea3',
  'rmp 2A'='#a6cee3',
  'rmp unknown'='#c1bfbf',
  'multiple rmp'='#000000'
)
v.rmpADC_presence_absence_colours <- c(
  '-'='#EAEAEA',
  'intact'='#9e1c4e',
  'truncated'='#fdb0c0'
)
v.rmpA2_presence_absence_colours <- v.rmpADC_presence_absence_colours

# New colour schemes for Kleborate v3.x variables
# K and O serotypes - using diverse color palettes
v.K_type_colours <- c(
  'unknown (KL107)'='#EAEAEA',
  'K1'='#e41a1c', 'K2'='#377eb8', 'K3'='#4daf4a', 'K5'='#984ea3', 
  'K10'='#ff7f00', 'K11'='#a65628', 'K13'='#f781bf', 'K14'='#999999',
  'K15'='#e41a1c', 'K16'='#377eb8', 'K19'='#4daf4a', 'K20'='#984ea3',
  'K21'='#ff7f00', 'K22'='#a65628', 'K23'='#f781bf', 'K24'='#999999',
  'K25'='#e41a1c', 'K27'='#377eb8', 'K30'='#4daf4a', 'K35'='#984ea3',
  'K47'='#ff7f00', 'K54'='#a65628', 'K57'='#f781bf', 'K62'='#999999',
  'K64'='#e41a1c', 'K67'='#377eb8', 'K71'='#4daf4a', 'K76'='#984ea3',
  'K77'='#ff7f00', 'K78'='#a65628', 'K80'='#f781bf', 'K81'='#999999'
)
v.O_type_colours <- c(
  'unknown'='#EAEAEA',
  'O1'='#e41a1c', 'O2α'='#377eb8', 'O2β'='#4daf4a', 'O3αβ'='#984ea3',
  'O3'='#ff7f00', 'O4'='#a65628', 'O5'='#f781bf', 'O7'='#999999',
  'O8'='#e41a1c', 'O9'='#377eb8', 'O10'='#4daf4a', 'O11'='#984ea3',
  'O12'='#ff7f00', 'O13'='#a65628', 'O16'='#f781bf', 'O16Нess'='#999999',
  'O17'='#e41a1c', 'O18'='#377eb8', 'O19'='#4daf4a', 'O20'='#984ea3',
  'OL2α'='#ff7f00', 'OL2α.1'='#a65628', 'OL2α.2'='#f781bf', 'OL2β'='#999999',
  'OL3α/β'='#e41a1c'
)

# Virulence lineage ST colours (Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC ST)
v.YbST_colours <- c(
  '-'='#EAEAEA',
  '0'='#c1bfbf',
  '1'='#b27f91', '2'='#cda12c', '3'='#56a354', '4'='#f28fa2', 
  '5'='#db7723', '6'='#93539d', '7'='#3a85a8', '8'='#7b75cc',
  '9'='#d9c5ef', '10'='#449d72', '11'='#ebd930', '12'='#6aa3c6',
  '13'='#a39f93', '14'='#93539d', '15'='#edc59a', '16'='#840639',
  '17'='#e25065'
)
v.CbST_colours <- c(
  '-'='#EAEAEA',
  '1'='#6aa3c6', '2'='#b27f91', '3'='#e25065'
)
v.AbST_colours <- c(
  '-'='#EAEAEA',
  '1'='#e31a1c', '2'='#1f78b4', '3'='#33a02c'
)
v.SmST_colours <- c(
  '-'='#EAEAEA',
  '1'='#e31a1c', '2'='#1f78b4', '3'='#984ea3'
)
v.RmST_colours <- c(
  '-'='#EAEAEA',
  '1'='#e31a1c', '2'='#1f78b4', '3'='#984ea3'
)

# SHV β-lactamase mutation tracking
v.SHV_mutations_colours <- c(
  '-'='#BCBCBC',
  'SHV-1'='#4575b4',
  'SHV-5'='#91bfdb',
  'SHV-11'='#e0f3f8',
  'SHV-12'='#ffffbf',
  'SHV-129'='#fee090',
  'other'='#fc8d59',
  'multiple'='#d73027'
)
IconButton <- function(outputId, type, ...) {
  if (type == 'data_dl') {
    s.class <- 'shiny-download-link'
    icon <- icon('table')
  } else if (type == 'graph_modal') {
    s.class <- 'action-button'
    icon <- icon('chart-area')
  } else {
    stop('Got bad IconButton type')
  }
  aTag <- tags$a(
    id=outputId,
    class=paste('btn btn-default', s.class),
    style='padding: 2px 4px',
    href='',
    target='_blank',
    download=NA,
    icon,
    '',
    ...
  )
}
