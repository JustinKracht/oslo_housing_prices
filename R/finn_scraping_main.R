################################################################################
# Scrape Oslo house and apartment price data from finn.no
# Author: Justin Kracht
# Date: 2019 August 22
################################################################################

# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse,
               rvest,
               purrr,
               stringr,
               lubridate,
               httr,
               parallel,
               pbmcapply,
               pbapply)

# Load scraping function
source("R/finn_scraper.R")

# Set options -------------------------------------------------------------

# Set user agent
httr::set_config(httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
pages <- 150 # Set the number of pages to scrape
prog_bar <- TRUE
mc_cores <- 8
# Set the starting page url
url <- "https://www.finn.no/realestate/homes/search.html?location=0.20061"


# Run the scraping tool ---------------------------------------------------

pris_list <- finn_scraper(url = url, 
                          pages = pages, 
                          prog_bar = prog_bar, 
                          mc_cores = mc_cores)
pris_list <- purrr::compact(pris_list) # Remove NULL elements from list
bolig_pris <- do.call(rbind, pris_list)

# Extract the zip code from address and add as new variable
bolig_pris <- mutate(
  bolig_pris, 
  zip_code = stringr::str_extract_all(address,
                                      pattern = "\\d{4}(?=\\sO)")
)

## Save data as RDS
saveRDS(bolig_pris, file = "data/bolig_pris.RDS")