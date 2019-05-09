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
  && apt-get -y --no-install-recommends install libgeos-dev libgeos++-dev libgdal-dev libproj-dev
RUN R -e "install.packages(c('rgbif', 'png', 'raster', 'RStoolbox', 'ggrepel'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))"
