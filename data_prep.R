#image.dir <- here::here("docs/images")
#gis.dir <- here::here("data-raw/gis")
#Default Rmd options
#knitr::opts_chunk$set(echo = FALSE,
#                      message = FALSE,
#                      warning = FALSE,
#                      fig.align = 'center') #allows for inserting R code into captions

#Plotting and data libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ecodata)
library(here)
library(kableExtra)
library(ggrepel)
library(stringr)
#library(patchwork)
library(grid)
library(cowplot)

#GIS libraries
#library(sf)
#library(rgdal)
#library(raster)
#library(ggspatial)
#library(marmap)


#General inline text input for report

#Council
council <- "Mid-Atlantic Fishery Management Council"
council_abbr <- "MAFMC"

#Region identifiers
epu <- "Mid-Atlantic Bight"
epu_abbr <- "MAB"
region <- "Mid-Atlantic"
region_abbr <- "MA" #Some commercial data organized by "MA" or "NE" regions, not by EPU 

shade.alpha <- 0.3
shade.fill <- "lightgrey"
lwd <- 1
pcex <- 2
trend.alpha <- 0.5
trend.size <- 2
hline.size <- 1
hline.alpha <- 0.35
hline.lty <- "dashed"
label.size <- 5
hjust.label <- 1.5
letter_size <- 4
feeding.guilds1<- c("Piscivore","Planktivore","Benthivore","Benthos")
feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
x.shade.min <- 2010
x.shade.max <- 2020

### Commercial Landings
managed_landings <- ecodata::comdat  %>%
  dplyr::filter(stringr::str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
                !stringr::str_detect(Var, "Other"),
                Time >= 1986,
                EPU == epu_abbr)

# HMS Landings
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
  separate(Var, c("Var", "trash"), sep = "_") %>% 
  group_by(YEAR) %>% 
  summarise(Value = sum(Value)) %>% 
  rename( Time = YEAR) %>% 
  mutate(Var = c("HMS Landings"), 
         Units = c("metric tons"), 
         EPU = c("MAB"))

#Total landings
total_landings <- ecodata::comdat  %>%
  dplyr::filter(!stringr::str_detect(Var, "managed species"),
                !stringr::str_detect(Var, "Other"),
                !stringr::str_detect(Var, "Apex"),
                stringr::str_detect(Var, "Landings"),
                Time >= 1986,
                EPU == epu_abbr) %>% 
  rbind(apex)

total_landings_agg <- total_landings %>%
  dplyr::group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Total",hline = mean(Value))

managed_landings_agg <- managed_landings %>%
  dplyr::group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Managed",hline = mean(Value))

Landings_total <- total_landings_agg
Landings_Managed <- managed_landings_agg





## Long term SST

LTSST<- ecodata::long_term_sst 


data <- list(c(Landings_Managed), 
             c(LTSST))

