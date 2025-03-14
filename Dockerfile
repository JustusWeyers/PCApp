# base Image
FROM rocker/verse:4.3.2
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# ...
RUN apt-get update
RUN apt-get install -y libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev
RUN rm -rf /var/lib/apt/lists/*

# install {remotes} Package
RUN R -e 'install.packages("remotes")'

# other Packages
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.8")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.8.4")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.27")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.48")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.9.1")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.3")'
# RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("zoo",upgrade="never", version = "1.8-12")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.7")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.7")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("gplots",upgrade="never", version = "3.2.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("fresh",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.15.4")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("broom",upgrade="never", version = "1.0.6")'
RUN Rscript -e 'remotes::install_github("gadenbuie/shinyThings@585a40f4015cac4e89851898e20ab4c7547c620c")'
RUN Rscript -e 'remotes::install_github("JustusWeyers/classwiseAcor@680352b83493a9a42e8fbcb8e8a8fe8750cc30b5")'

# install PCApp
COPY PCApp_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz

# set host and port
COPY inst/Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838
CMD R -e "library(PCApp);PCApp::run_app(webmode = TRUE)"