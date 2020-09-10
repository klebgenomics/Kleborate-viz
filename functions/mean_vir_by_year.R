library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(plotly)
library(vegan)
library(ComplexHeatmap)
library(readxl)
library(pheatmap)
library(ggrepel)
library(shinythemes)
library(RColorBrewer)
library(shinydashboard)
library(ggthemes)

#Input = automatic 
#year_vir_res <- read.csv("data/mean_vir_res_by_year.csv")
p1 <- ggplot(year_vir_res, aes(x=year)) + geom_bar(aes(weight=virulence_score)) + 
geom_line(aes(y=Yersiniabactin, color="ybt")) + geom_line(aes(y=Colibactin, color="clb")) + 
geom_line(aes(y=Aerobactin, color="iuc")) + geom_line(aes(y=Salmochelin, color="iro")) +
geom_line(aes(y=RmpADC, color="rmpADC")) + geom_line(aes(y=rmpA2)) + theme_tufte() +
scale_y_continuous("Prevalence") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")

p2 <- ggplot(year_vir_res, aes(x=year)) + 
geom_bar(aes(weight=resistance_score)) +
geom_line(aes(y=Col, color="Col")) + 
geom_line(aes(y=Bla_ESBL, color="ESBL")) +
geom_line(aes(y=Bla_Carb, color="carb")) + theme_tufte() +
scale_y_continuous("Prevalence") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")

p3 <- ggplot(year_vir_res, aes(x=year)) + 
geom_line(aes(y=num_resistance_classes, color="AMR classes")) +
geom_line(aes(y=num_resistance_genes, color="AMR genes")) + theme_tufte() +
scale_y_continuous("Mean AMR classes/genes") +
scale_x_continuous("Year") +
scale_colour_brewer("Label", palette="Set2")