FROM rocker/shiny:4.2.2
ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  make \
  zlib1g-dev\
&& apt-get clean \
&& rm -rf /var/lib/apt/lists/*
COPY renv.lock renv.lock
RUN Rscript -e "options(warn=2); install.packages('renv')"
RUN Rscript -e "renv::restore()"
COPY app /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]