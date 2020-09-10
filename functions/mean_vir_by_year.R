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
#year_vir_res <- read.csv("data/mean_vir_res_by_year.csv")
#F1_a
ggplot(year_vir_res, aes(x=year)) + geom_bar(aes(weight=virulence_score)) + 
geom_line(aes(y=Yersiniabactin, color="ybt")) + geom_line(aes(y=Colibactin, color="clb")) + 
geom_line(aes(y=Aerobactin, color="iuc")) + geom_line(aes(y=Salmochelin, color="iro")) +
geom_line(aes(y=RmpADC, color="rmpADC")) + geom_line(aes(y=rmpA2)) + theme_tufte() +
scale_y_continuous("Prevalence") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")

#F1_b
ggplot(year_vir_res, aes(x=year)) + 
geom_bar(aes(weight=resistance_score)) +
geom_line(aes(y=Col, color="Col")) + 
geom_line(aes(y=Bla_ESBL, color="ESBL")) +
geom_line(aes(y=Bla_Carb, color="carb")) + theme_tufte() +
scale_y_continuous("Prevalence") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")

#F1_c
ggplot(year_vir_res, aes(x=year)) + 
geom_line(aes(y=num_resistance_classes, color="AMR classes")) +
geom_line(aes(y=num_resistance_genes, color="AMR genes")) + theme_tufte() +
scale_y_continuous("Mean AMR classes/genes") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")