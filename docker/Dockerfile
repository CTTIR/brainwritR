FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    zlib1g-dev libssl-dev libcurl4-openssl-dev libxml2-dev libglpk-dev libgmp-dev \
    libcairo2-dev libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libjpeg-dev libtiff-dev fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r --error --skipinstalled shiny bslib DBI RSQLite shinyjs qrcode yaml \
    ggplot2 patchwork ggraph igraph ggwordcloud stringi stopwords openxlsx rlang withr

WORKDIR /app
COPY . /app
RUN R -q -e 'stopifnot(capabilities("cairo"))' \
    && R CMD INSTALL . \
    && mkdir -p /app/data \
    && useradd --uid 10001 --create-home brainwriting \
    && chown -R brainwriting:brainwriting /app/data
USER brainwriting
ENV DB_PATH=/app/data/brainwriting.sqlite PORT=3838
EXPOSE 3838
CMD ["R", "-q", "-e", "brainwritR::run_app()"]
