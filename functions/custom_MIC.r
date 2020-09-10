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
data <- read.csv("/data/EuSCAPE-Kleborate-AMR_comparison_forR.csv")
amr_df <- data.frame(data)

ggplot(amr_df, aes(y=Meropenem_MIC, x=Bla_carb_simplified)) + geom_boxplot() +
geom_jitter(aes(colour=Omp_simplified)) + scale_y_continuous(trans="log2") +
scale_color_manual(values=c("#000000", "#E69F00", "#CC0011", "#D16E66", "#D16E66", "#CC0011","#D16E66")) + theme_classic()