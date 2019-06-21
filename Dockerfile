# R 3.6 was released on 2019-04-26. Installing R packages to the day before 2019-04-01
FROM rocker/tidyverse:3.5.3
# Installing texlive though apt-get cause I was having trouble using TinyTex
RUN apt-get update \
  && apt-get -y --no-install-recommends install texlive-full pdftk
# Workflow manager
RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
# Extra packages for paper writing
RUN R -e "install.packages(c('bookdown','kableExtra', 'cowplot'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
# Other packages
RUN apt-get update \
  && apt-get -y --no-install-recommends install libudunits2-dev libgeos-dev libgeos++-dev libgdal-dev libproj-dev
RUN R -e "install.packages(c('rgbif', 'png', 'raster', 'RStoolbox', 'ggrepel', 'rjson', 'rcrossref', 'bib2df', 'sf', 'rnaturalearth', 'rnaturalearthdata', 'taxize'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
# For pararell processing
RUN apt-get update \
  && apt-get -y --no-install-recommends install libzmq3-dev
RUN R -e "install.packages(c('clustermq'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
RUN R -e "install.packages(c('ggforce'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
RUN R -e "install.packages(c('furrr', 'future.callr', 'filelock'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"



# Install development version of Rmangal. Code at April 8 2019 Note: Rmangal is pretty shit at the moment (May 2019) won't use it for now
# RUN apt-get update \
#  && apt-get -y --no-install-recommends install libjq-dev libprotobuf-dev protobuf-compiler libudunits2-dev libv8-dev
# RUN R -e "remotes::install_github('mangal-wg/rmangal', ref = '0aa4ce97aa4e484a00d422b6f3d6a3420958e852')"
