FROM bioconductor/bioconductor_docker:devel

WORKDIR /code


RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libhdf5-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'BiocManager::install(c("ConsensusClusterPlus", "Biobase", "FlowSOM"), ask = FALSE)'

RUN install2.r --error \
    shiny \
    shinydashboard \
    shinyjs \
    DT \
    dplyr \
    ggplot2 \
    readr \
    ggExtra \
    bslib \
    data.table \
    tidyr \
    stringr \
    readr \
    purrr \
    harmony \
    scales \
    jsonlite \
    cookies \
    digest \
    promises \
    future \
    progressr \
    Matrix \
    Rtsne \
    umap \
    RColorBrewer \
    reshape2 \
    uuid 

    
COPY . .

RUN chmod -R 777 /code/sessions
RUN chmod -R 777 /code/www/output
RUN chmod -R 777 /code/untitled_consensus_cluster
RUN chmod -R 777 /tmp
RUN chmod -R 777 /code

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
