FROM rocker/verse:4.3.2
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.6")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
COPY PCApp_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz

# set host and port
COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838
CMD R -e "library(PCApp);PCApp::run_app()"
