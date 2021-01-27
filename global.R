# Packages
library(shiny)
library(shinyjs)
# Misc
library(tidyverse)
library(reshape2)
library(vegan)  # diversity measurements
# Plotting
library(plotly)
library(heatmaply)

# Set maximum upload file size
options(shiny.maxRequestSize=20*1024^2)

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
  'num_resistance_genes',
  'num_resistance_classes',
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
v.virulence_loci <- list(
  'Yersiniabactin (presence/absence)'='Yersiniabactin_presenceabsence',
  'Yersiniabactin (lineages)'='Yersiniabactin_lineages',
  'Colibactin (presence/absence)'='Colibactin_presenceabsence',
  'Colibactin (lineages)'='Colibactin_lineages',
  'Aerobactin (presence/absence)'='Aerobactin_presenceabsence',
  'Aerobactin (lineages)'='Aerobactin_lineages',
  'Salmochelin (presence/absence)'='Salmochelin_presenceabsence',
  'Salmochelin (lineages)'='Salmochelin_lineages',
  'RmpADC (presence/absence)'='RmpADC_presenceabsence',
  'RmpADC (lineages)'='RmpADC_lineages',
  'rmpA2 (presence/absence)'='rmpA2_presenceabsence',
)
v.resistance_classes <- list(
  'AGly'='AGly_acquired',
  'Col (genes)'='Col_acquired',
  'Col (mutations)' = 'Col_mutations',
  'Fcyn'='Fcyn_acquired',
  'Flq (genes)'='Flq_acquired',
  'Flq (mutations)' = 'Flq_mutations',
  'Gly'='Gly_acquired',
  'MLS'='MLS_acquired',
  'Phe'='Phe_acquired',
  'Rif'='Rif_acquired',
  'Sul'='Sul_acquired',
  'Tet'='Tet_acquired',
  'Tmt'='Tmt_acquired',
  'Tgc'='Tgc_acquired',
  'Bla'='Bla_acquired',
  'Bla inhR'='Bla_inhR_acquired',
  'Bla ESBL'='Bla_ESBL_acquired',
  'Bla Carb'='Bla_Carb_acquired',
  'Bla ESBL inhR'='Bla_ESBL_inhR_acquired',
  'Bla ESBL alleles'='Bla_ESBL_simplified',
  'Bla Carb alleles'='Bla_Carb_simplified'
)
v.genotype_var_choices <- list(
  'Summary'=list(
    'Resistance score'='resistance_score',
    'Virulence score'='virulence_score'    
  ),
  'Virulence'=v.virulence_loci,
  'Resistance'=v.resistance_classes
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

clone_type_colours <- c(
  'MDR'='#f7786b',
  'Hv'='#798EF6',
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

ybt_lineage_colours <- c(
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

clb_lineage_colours <- c(
  '-'='#EAEAEA',
  'clb 1'='#6aa3c6',
  'clb 2'='#b27f91',
  'clb 3'='#e25065',
  'clb unknown'='#c1bfbf'
)
  
iro_lineage_colours <- c(
  '-'='#EAEAEA',
  'iro 1'='#e31a1c',
  'iro 2'='#1f78b4',
  'iro 3'='#984ea3',
  'iro 4'='#ff7f00',
  'iro 5'='#fb9a99',
  'iro  unknown'='#c1bfbf',
  'multiple iro'='#000000'
)

iuc_lineage_Colours <- c(
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

rmpADC_lineage_colours <- c(
  '-'='#EAEAEA',
  'rmp 1'='#e31a1c',
  'rmp 2'='#1f78b4',
  'rmp 3'='#984ea3',
  'rmp 2A'='#a6cee3',
  'rmp unknown'='#c1bfbf',
  'multiple rmp'='#000000'
)


rmpADC_presence_absence_colours <- c(
  '-'='#EAEAEA',
  'intact'='#9e1c4e',
  'truncated'='#fdb0c0'
)

rmpA2_presence_absence_colours <- rmpADC_presence_absence_colours

# Clone type definitions
MDR_clones_list = c(
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

hv_clones_list = c(
  "ST23",
  "ST86",
  "ST66",
  "ST380",
  "ST25",
  "ST65"
)