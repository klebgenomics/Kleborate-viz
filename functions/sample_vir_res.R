#!/usr/bin/Rscript

# Load libraries
suppressMessages(library(ggplot2))
suppressMessages(library(ggthemes))
suppressMessages(library(shinydashboard))
suppressMessages(library(RColorBrewer))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(heatmaply))
suppressMessages(library(reshape2))
suppressMessages(library(vegan))
suppressMessages(library(ComplexHeatmap))
suppressMessages(library(readxl))
suppressMessages(library(pheatmap))
suppressMessages(library(ggrepel))
suppressMessages(library(shinythemes))

#Input = automatic 
#sample_vir_res <- read.csv("data/mean_vir_res_scores_sampletype.csv")
#ST_vir_res <- read.csv("data/mean_vir_res_scores_ST.csv")

ggplot(sample_vir_res, aes(x=sample_vir_res$mean_vir, y=sample_vir_res$mean_res)) +
geom_point(aes(size=sample_vir_res$number.of.genomes)) + geom_text_repel(label=sample_vir_res$sample)

ggplot(ST_vir_res, aes(x=ST_vir_res$mean_vir, y=ST_vir_res$mean_res)) +
geom_point(aes(size=ST_vir_res$number.of.genomes)) + geom_text_repel(label=ST_vir_res$ST)