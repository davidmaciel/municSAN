## code to prepare `pnad` dataset goes here

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, PNADcIBGE, survey, naniar, daflateBR,
               lubridate, srvyr)



# pnad --------------------------------------------------------------------


