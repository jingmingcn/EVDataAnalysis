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
"FlowSOM",
"flowCore",
"ConsensusClusterPlus",
"Rtsne",
"umap",
"RColorBrewer",
"Biobase",
"reshape2",
"uuid",
"data.table")
install.packages(packages)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("FlowSOM")