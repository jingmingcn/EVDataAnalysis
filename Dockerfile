FROM satijalab/seurat:latest

WORKDIR /code

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
    tools \
    jsonlite \
    cookies \
    digest

    
COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
