# Install required packages
packages <- c("shiny",
"shinydashboard",
"DT",
"ggplot2",
"dplyr",
"tools",
"jsonlite",
"shinyjs",
"cookies",
"promises",
"future",
"progressr",
"tidyr",
"stringr",
"Matrix",
"ConsensusClusterPlus",
"Rtsne",
"umap",
"RColorBrewer",
"Biobase",
"reshape2",
"uuid",
"data.table")
install.packages(packages)

install.packages('remotes')
remotes::install_github('RGLab/cytolib')

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("flowCore")
BiocManager::install("FlowSOM")


# sudo apt install libprotobuf-dev, libboost-dev 
#         sudo apt-get install libfontconfig1-dev
# sudo apt-get install libxml2-dev