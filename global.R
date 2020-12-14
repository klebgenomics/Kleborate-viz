# Packages
library(shiny)
require(shinyjs)
# Misc
library(tidyverse)
require(reshape2)
library(vegan)  # diversity measurements
# Plotting
library(heatmaply)
library(plotly)

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

# Colours and variables names
v.kpsc_names <- c(
  "Klebsiella pneumoniae",
  "Klebsiella quasipneumoniae subsp. quasipneumoniae",
  "Klebsiella quasipneumoniae subsp. similipneumoniae",
  "Klebsiella variicola subsp. variicola",
  "Klebsiella variicola subsp. tropica",
  "Klebsiella quasivariicola",
  "Klebsiella africana"
)
v.kpsc_colours <- c(
  "Klebsiella pneumoniae"="#875F9A",
  "Klebsiella quasipneumoniae subsp. quasipneumoniae"="#EDA483",
  "Klebsiella quasipneumoniae subsp. similipneumoniae"="#56c8f5",
  "Klebsiella variicola subsp. variicola"="#8CBDB2",
  "Klebsiella variicola subsp. tropica"="#205c38",
  "Klebsiella quasivariicola"="#F0B663",
  "Klebsiella africana"="#ED6060"
)
v.other_species_colour_palette <- colorRampPalette(
  c(
    "#e85a5a",
    "#ffffbf",
    "#5B6894",
    "#5B6899",
    "#fdae61",
    "#66bd63",
    "#fee08b",
    "#5B6896",
    "#875F9A",
    "#5B6893",
    "#876738",
    "#c2bebe",
    "#a6d96a",
    "#f6fa7d"
  )
)
v.misc_colour_palette <- colorRampPalette(
  c(
    "#E67E78",
    "#EF9D45",
    "#E9DD50",
    "#79BEA8",
    "#3F57A7",
    "#885F9A",
    "#757C82"
  )
)
v.virulence_loci <- list(
  'Yersiniabactin'='Yersiniabactin',
  'Colibactin'='Colibactin',
  'Aerobactin'='Aerobactin',
  'Salmochelin'='Salmochelin',
  'RmpADC'='RmpADC',
  'rmpA2'='rmpA2'
)
v.resistance_classes <- list(
  'AGly'='AGly_acquired',
  'Col'='Col_acquired',
  'Fcyn'='Fcyn_acquired',
  'Flq'='Flq_acquired',
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
  'Bla ESBL inhR'='Bla_ESBL_inhR_acquired'
)
v.genotype_st_var_choices <- list(
  'Summary'=list('Resistance score'='resistance_score', 'Virulence score'='virulence_score'),
  'Virulence'=v.virulence_loci,
  'Resistance'=v.resistance_classes
)
v.virulence_score_names <- list(
  '0'="None",
  '1'="ybt",
  '2'="ybt + clb",
  '3'="iuc (VP)",
  '4'="ybt + iuc (VP)",
  '5'="ybt + clb + iuc (VP)"
)
v.virulence_score_colours <- c(
  '0'="grey", 
  '1'="#79BEA8", 
  '2'="#448D76", 
  '3'="#798EF6",
  '4'="#183BF0", 
  '5'="#091534"
)
v.resistance_score_names <- c(
  '0'='ESBL-, Carb-',
  '1'='ESBL+, Carb-',
  '2'='Carb+',
  '3'='Carb+, Col+'
)
v.resistance_score_colours <- c(
  '0'='grey',
  '1'='#f7cac9',
  '2'='#f7786b',
  '3'='#c94c4c'
)