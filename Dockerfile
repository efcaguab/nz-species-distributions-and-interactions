# R 3.6 was released on 2019-04-26. Installing R packages to the day before 2019-04-01
FROM rocker/tidyverse:3.5.3

# Installing Latex
RUN apt-get update \
  && apt-get -y --no-install-recommends install texlive-full pdftk

# Install R packages
RUN apt-get update \
  && apt-get -y --no-install-recommends install libudunits2-dev libgeos-dev libgeos++-dev libgdal-dev libproj-dev libudunits2-dev libgdal-dev libv8-dev
RUN R -e "install.packages(c('bib2df', 'bookdown', 'brms', 'concaveman', 'countrycode', 'cowplot', 'drake', 'ecospat', 'ggforce', 'ggrepel', 'kableExtra', 'lme4', 'maps', 'png', 'raster', 'rcrossref', 'rgbif', 'rjson', 'rnaturalearth', 'rnaturalearthdata', 'RStoolbox', 'sf', 'taxize', 'tidybayes'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-01'))" \
  && R -e "install.packages(c('CoordinateCleaner'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-06-01'))" \
  && R -e "remotes::install_github('ropensci/rnaturalearthhires', ref = '7a3f0fc8d8d690fae044dd01eb2f14d3a75c92ed')"

## Installing NEOVIM
RUN apt-get update \
    && apt-get -y --no-install-recommends install curl python3-dev python3-pip python3-setuptools
# Install some python pre-requisites for neovim plugnins
COPY init.vim /root/.config/nvim/init.vim
# Pyton requirements for autocompletion
RUN python3 -m pip install wheel \
  && python3 -m pip install pynvim \
# Install neovim software
  && wget https://github.com/neovim/neovim/releases/download/v0.3.8/nvim.appimage \
  && chmod u+x nvim.appimage \
  && ./nvim.appimage --appimage-extract \
  && ln -s /squashfs-root/AppRun /usr/local/bin/nvim \
# Install plugins
  && curl -fLo /root/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim \
  && squashfs-root/AppRun +PlugInstall +qall 
