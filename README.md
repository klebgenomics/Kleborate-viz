# Kleborate_viz

![Code Count](https://img.shields.io/github/languages/count/kelwyres/Kleborate_viz)
![Main Code Base](https://img.shields.io/github/languages/top/kelwyres/Kleborate_viz)
![License](https://img.shields.io/badge/License-GPL%20v3-blue)
![Version](https://img.shields.io/badge/version-1.0-red)
![Last Commit](https://img.shields.io/github/last-commit/kelwyres/Kleborate_viz)
![Open Issues](https://img.shields.io/github/issues-raw/kelwyres/Kleborate_viz)
![Repo Size](https://img.shields.io/github/repo-size/kelwyres/Kleborate_viz)

## Table of Contents

  * [Description](#Description)
  * [Dependencies](#Description)
  * [Demonstration](#Demonstration)
  * [Installation](#Installation)
  * [Usage](#Usage)
  * [License](#License)
  * [Version](#Version)
  * [Questions](#Questions)

## Description

Visualization tool for Kleborate [Kleborate repository](https://github.com/katholt/Kleborate).

## Dependencies

The following programs need to be available/installed for correct operation:


* [R](https://www.r-project.org/).
* [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/).

Package Dependencies

KleborateViz requires the following packages (and their dependencies) to be installed for correct operation:

```CRAN```

```

install.packages('shiny')
install.packages('heatmaply')
install.packages('ggplot2')
install.packages('plotly')
install.packages('dplyr')
install.packages('reshape2')
install.packages('vegan')
install.packages('ComplexHeatmap')
install.packages('readxl')
install.packages('pheatmap')
install.packages('ggrepel')
install.packages('shinythemes')
install.packages('RColorBrewer')
install.packages('lintr')
install.packages('formatR')
install.packages('styler')
install.packages('shinyWidgets')
install.packages('ggExtra')
install.packages('shinyjs')
install.packages('colourpicker')
install.packages('bootstraplib')
install.packages('gfonts')
install.packages('thematic')
install.packages('shinyBS')
install.packages('gProfileR')
install.packages('gtable')
install.packages('Cairo') 
install.packages('data.table')
install.packages('ggridges')
install.packages('qicharts2')
install.packages('rintrojs')
install.packages('shinycssloaders')
install.packages('survival')
install.packages('survminer')

```

## CRAN Package

## Docker image

To run Kleborate_viz locally, you can use a snapshot of Kleborate_viz [Docker image](https://hub.docker.com/r/lcerdeira/kleborateviz/) from Docker Hub. You can also try to build the image from the Dockerfile but Kleborate_viz code may not work correctly with the newest versions of the R packages if the changes have broken backward compatibility.

To use the Docker image, you need to have [Docker](https://www.docker.com/) installed. Then use the following code:

```
sudo docker pull lcerdeira/kleborateviz
mkdir ~/customKleborateviz/
cd ~/customKleborateviz/
wget https://github.com/kelwyres/Kleborate_viz/archive/master.zip
unzip master.zip
chmod -R go+rx ~/customKleborateviz/
sudo docker run -d \
	--name customKleborateviz \
	-p <myPort>:3838 \
    -v ~/customKleborateviz/Kleborateviz-master/:/srv/shiny-server/:ro \
    lcerdeira/kleborateviz
```

## Installation

## Usage

## Demonstration

[Video](demo link video).

## Tests

N/A

## Contact

Kleborate_viz is under active development

Please get in touch via the GitHub issues tracker if you have any issues, questions or ideas.

For more on our lab, including other software, see [Holtlab](https://holtlab.net/).