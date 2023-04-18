# Author: Kevin See
# Purpose: Prep steelhead redd data and PIT tag data from the Wenatchee
# Created: 2/3/2023
# Last Modified: 2/10/2023
# Notes: R will send error if any of the Excel files are open on the user's computer

# #-----------------------------------------------------------------
# # if needed, install these packages from CRAN
# install.packages(c("tidyverse",
#                    "readxl",
#                    "janitor",
#                    "magrittr",
#                    "msm",
#                    "here",
#                    "DescTools",
#                    "devtools"))
# # if needed, install these packages from GitHub
# remotes::install_github("kevinsee/PITcleanr")
# remotes::install_github("kevinsee/sroem")
# remotes::install_github("wdfw-fp/SthdReddsWenatchee")

# if a member of WDFW, you can install a WDFW html template by following
# instructions here: https://github.com/wdfw-fp/wdfwTemplates

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

#-----------------------------------------------------------------
# what year(s) are being prepped
yrs = 2014:2022
# how many observers were used for these surveys?
n_observers = "two"

# gather, prepare, wrangle data, and save relevant pieces as .rda objects
# one for each year
prep_wen_sthd_data(query_year = yrs,
                   n_observers = n_observers,
                   save_rda = T)


#-----------------------------------------------------------------
# Methow
# what year(s) are being prepped
yrs = 2021:2022
# how many observers were used for these surveys?
n_observers = "two"

# gather, prepare, wrangle data, and save relevant pieces as .rda objects
# one for each year
prep_met_sthd_data(query_year = yrs,
                   n_observers = n_observers,
                   save_rda = T)
