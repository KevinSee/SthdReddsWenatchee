# Author: Kevin See
# Purpose: Prep steelhead redd data and PIT tag data from the Wenatchee
# Created: 2/3/2023
# Last Modified: 2/3/2023
# Notes:

#-----------------------------------------------------------------
# if needed, install these packages from CRAN
install.packages(c("tidyverse",
                   "readxl",
                   "janitor",
                   "magrittr",
                   "msm",
                   "here",
                   "DescTools",
                   "devtools"))
# if needed, install these packages from GitHub
remotes::install_github("kevinsee/PITcleanr")
remotes::install_github("kevinsee/sroem")
remotes::install_github("wdfw-fp/SthdReddsWenatchee")

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(here)
library(DescTools)
library(PITcleanr)
library(sroem)
# library(SthdReddsWenatchee)
devtools::load_all()

#-----------------------------------------------------------------
# what year(s) are being prepped
yrs = 2014:2022
# how many observers were used for these surveys?
n_observers = "two"

# gather, prepare, wrangle data, and save relevant pieces as .rda objects
# one for each year
prep_wen_sthd_data(query_year = yrs,
                   n_observers = n_observers)
