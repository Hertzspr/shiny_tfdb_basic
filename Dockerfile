# Use shiny image
FROM rocker/shiny:4.4.0

# Update system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libcurl4-openssl-dev \
    libssl-dev

# Set shiny working directory
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

# Copy Shiny files
COPY /ui.R ./ui.R
COPY /server.R ./server.R
COPY /global.R ./global.R
COPY /uiHelpers.R ./uiHelpers.R
COPY /transfers_df.RDS ./transfers_df.RDS
COPY /df_long_lat.RDS ./df_long_lat.RDS
COPY /out.png ./out.png
COPY /out@2x.png ./out@2x.png
COPY /in.png ./in.png
COPY /in@2x.png ./in@2x.png 

# Copy renv.lock file
COPY /renv.lock ./renv.lock

# Download renv and restore library
ENV RENV_VERSION 1.0.3
RUN Rscript -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv@v${RENV_VERSION}')"
ENV RENV_PATHS_LIBRARY renv/library
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 3838