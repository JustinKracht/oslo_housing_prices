################################################################################
# Function for scraping housing data from finn.no
# Author: Justin Kracht
# Date: 2019 August 22
################################################################################

#' Scrape finn.no housing data
#'
#' Given a starting url, scrape a number of finn.no pages to extract housing
#' data such as price, location, etc.
#'
#' @param url url of the page to start scraping on.
#' @param pages how many pages to scrape
#' @param prog_bar display a progress bar?
#' @param mc_cores number of processor cores to use
#'
#' @return A list of length \code{pages} containing data frames of housing data from each page.
#' @export
finn_scraper <- function(url, pages = 10, prog_bar = FALSE, mc_cores = 0) {
  
  listing_scraper <- function(listing_url) {
    ## Function for scraping data from individual listings
    listing_html <- read_html(listing_url)
    
    # Extract asking price, strip spaces, and transform to numeric
    prisantydning <- listing_html %>%
      html_node("div.panel span.u-t3") %>%
      html_text() %>%
      str_extract("\\d+.*(?=(\\skr))") %>%
      str_replace_all("\\s", "") %>%
      as.numeric()
    
    # Extract neighborhood info
    omrade <- listing_html %>%
      html_nodes("a.truncate") %>%
      html_text() %>%
      .[4]
    
    # Extract misc. info
    apt_info <- listing_html %>%
      html_nodes("dl.definition-list dd") %>%
      html_text() %>%
      as.list()
    names(apt_info) <- listing_html %>%
      html_nodes("dl.definition-list dt") %>%
      html_text()
    
    # Extract address
    apt_address <- listing_html %>%
      html_node("p.u-caption") %>%
      html_text()
    
    # Extract transit score from the profil2 website
    finn_code <- as.numeric(apt_info$"FINN-kode")
    profil2_url <- paste0("https://profil2.nabolag.no/", finn_code, "transport")
    walk_score <- tryCatch({
      profil2_html <- read_html(profil2_url)
      profil2_html %>%
        html_nodes("h4.Rating__RatingHeader-ys2jkg-3") %>%
        html_text() %>%
        str_extract("^\\d+") %>%
        as.numeric()
    }, error = function(w) return(NA)
    )
    
    
    # Extract neighborhood scores
    profil2_url <- paste0("https://profil2.nabolag.no/", finn_code, "bomiljo")
    neighborhood_scores <- tryCatch({
      profil2_html <- read_html(profil2_url)
      profil2_html %>%
        html_nodes("h4.Rating__RatingHeader-ys2jkg-3") %>%
        html_text() %>%
        str_extract("^\\d+") %>%
        as.numeric()
    }, error = function(w) return(rep(NA, 4))
    )
    
    # Add transit and neighborhood scores to apt_info list
    apt_info$walk_score <- walk_score
    apt_info$safety_score <- neighborhood_scores[1]
    apt_info$neighbor_score <- neighborhood_scores[2]
    apt_info$garden_score <- neighborhood_scores[3]
    apt_info$road_score <- neighborhood_scores[4]
    
    ## Organize and format the data in a tibble ----
    out <- tibble(
      address = apt_address,
      omrade = omrade,
      zip_code = stringr::str_extract(apt_address,
                                      pattern = "\\d{4}(?=\\sO)"),
      prisantydning = prisantydning,
      totalpris = ifelse(!is.null(apt_info$Totalpris),
                         yes = apt_info$Totalpris %>%
                           str_extract("\\d+(\\s|\\d)*(?=(\\skr))") %>%
                           str_replace_all("\\s", "") %>%
                           as.numeric(),
                         no = NA),
      felleskost = ifelse(!is.null(apt_info$`Felleskost/mnd.`),
                          yes = apt_info$`Felleskost/mnd.` %>%
                            str_extract(".*(?=(\\skr))") %>%
                            str_replace_all("\\s", "") %>%
                            as.numeric(),
                          no = NA),
      boligtype = ifelse(!is.null(apt_info$Boligtype),
                         yes = apt_info$Boligtype,
                         no = NA),
      eieform = ifelse(!is.null(apt_info$Eieform),
                       yes = apt_info$Eieform,
                       no = NA),
      soverom = ifelse(!is.null(apt_info$Soverom),
                       yes = apt_info$Soverom %>% 
                         str_extract("\\d$") %>%
                         as.numeric(),
                       no = NA),
      primaerrom = ifelse(!is.null(apt_info$Primærrom),
                          yes = apt_info$Primærrom %>%
                            str_extract("[\\dd]+") %>%
                            as.numeric(),
                          no = NA),
      bruksareal = ifelse(!is.null(apt_info$Bruksareal),
                          yes = apt_info$Bruksareal %>%
                            str_extract("[\\dd]+") %>%
                            as.numeric(),
                          no = NA),
      etasje = ifelse(!is.null(apt_info$Etasje),
                      yes = apt_info$Etasje %>% as.numeric(),
                      no = NA),
      byggear = ifelse(!is.null(apt_info$Byggeår),
                       yes = apt_info$Byggeår %>% as.numeric(),
                       no = NA),
      energimerking = ifelse(!is.null(apt_info$Energimerking),
                             yes = apt_info$Energimerking %>%
                               str_extract("[A-Z][a-z -]*(?=(\\n))"),
                             no = NA),
      rom = ifelse(!is.null(apt_info$Rom),
                   yes = apt_info$Rom %>% as.numeric(),
                   no = NA),
      tomtareal = ifelse(!is.null(apt_info$Tomteareal),
                         yes = apt_info$Tomteareal %>%
                           str_extract("[\\d]+") %>%
                           as.numeric(),
                         no = NA),
      bruttoareal = ifelse(!is.null(apt_info$Bruttoareal),
                           yes = apt_info$Bruttoareal %>%
                             str_extract("[\\d]+") %>%
                             as.numeric(),
                           no = NA),
      fellesformue = ifelse(!is.null(apt_info$Fellesformue),
                            yes = apt_info$Fellesformue %>%
                              str_extract(".*(?=(\\skr))") %>%
                              str_replace_all("\\s", "") %>%
                              as.numeric(),
                            no = NA),
      formuesverdi = ifelse(!is.null(apt_info$Formuesverdi),
                            yes = apt_info$Formuesverdi %>%
                              str_extract(".*(?=(\\skr))") %>%
                              str_replace_all("\\s", "") %>%
                              as.numeric(),
                            no = NA),
      siste_endret = ifelse(!is.null(apt_info$`Sist endret`),
                            yes = apt_info$`Sist endret` %>% 
                              lubridate::dmy_hm() %>%
                              lubridate::as_datetime(),
                            no = NA),
      walk_score = ifelse(!is.null(apt_info$walk_score),
                          yes = apt_info$walk_score,
                          no = NA),
      safety_score = ifelse(!is.null(apt_info$safety_score),
                            yes = apt_info$safety_score,
                            no = NA),
      neighbor_score = ifelse(!is.null(apt_info$neighbor_score),
                              yes = apt_info$neighbor_score,
                              no = NA),
      garden_score = ifelse(!is.null(apt_info$garden_score),
                            yes = apt_info$garden_score,
                            no = NA),
      road_score = ifelse(!is.null(apt_info$road_score),
                          yes = apt_info$road_score,
                          no = NA),
      listing_url = listing_url
    )
    
    # Return the tibble with the apt. information
    out
  }
  page_scraper <- function(page_url) {
    ## Function for scraping links from one page of listings
    page_html <- read_html(page_url)
    
    # Get links for each ad and format them
    apt_links <- page_html %>%
      html_nodes("a.ads__unit__link") %>%
      html_attr("href")
    apt_links <- paste0("https://www.finn.no", apt_links)
    
    purrr::map_dfr(apt_links, listing_scraper)
  }
  
  # Append filters to end of url
  url_filters <- "&property_type=3&property_type=1&property_type=4&property_type=2"
  page_list <- str_c(url, "&page=", 1:pages, url_filters)
  
  # Return scraping data for every page in pages
  # Parallel processing if mc.cores > 0
  if (mc_cores > 1) {
    if (prog_bar == TRUE) {
      pbmcapply::pbmclapply(page_list, FUN = function(page_url) {
        tryCatch(page_scraper(page_url),
                 error = function(e) NULL)
      }, mc.cores = mc_cores)
    } else if (prog_bar == FALSE) {
      parallel::mclapply(page_list, FUN = function(page_url) {
        tryCatch(page_scraper(page_url),
                 error = function(e) NULL)
      }, mc.cores = mc_cores)
    }
  } else if (mc_cores == 1) {
    if (prog_bar == TRUE) {
      pbapply::pblapply(page_list, FUN = function(page_url) {
        tryCatch(page_scraper(page_url),
                 error = function(e) NULL)
      })
    } else if (prog_bar == FALSE) {
      lapply(page_list, FUN = function(page_url) {
        tryCatch(page_scraper(page_url),
                 error = function(e) NULL)
      }
      )
    }
  }
}