library(rvest)
library(stringr)

#' A function that scrapes craiglist housing posts
#' 
#' This function has a number of arguments that can specify a craigslist
#' housing search that filter down the posts shown. It then returns a
#' data.frame with all the results
#' 
#' @param craigslist_domain The domain of craigslist to search postings for.
#'                          For example, 'vancouver' to search listings on
#'                          vancouver.craigslist.org.
#' @param has_pic Craigslist search filter requiring that results have
#'                pictures
#' @param min_bed,max_bed Craigslist search filters specifying minimum and
#'                        maximum number of bedrooms of the listing
#' @param min_bath Craigslist search filter specifying minimum number of
#'                 bathrooms of the listing
#' @param query    Search term that listings must match
#' @param category  Housing category to search. E.g. 'apa' for 
#'                  apartments/housing for rent, 'swp' for housing swap,,
#'                  'rea' for real estate for sale, 'roo' for rooms & shares,
#'                  'sub' for sublets & temporary,or  'vac' for vacation rentals
#' @param region_lat,region_long After results are collected, these arguments
#'                               specify a rectangular region. The function
#'                               will check whether listings' latitude
#'                               and longitude (when available) are within the
#'                               region (returning TRUE) or not (returning FALSE)
#'                               
#' @return A data.frame with the columns: 
#'        posted - when was the listing posted
#'        updated - when (if any) was the listing updated
#'        available - when is the "available on" date of the listing
#'        title - what is the title of the listing
#'        link - what is the html address of the listing
#'        price - what is the listed price of the listing
#'        location - what is the listed location of the listing
#'        beds - how many bedrooms does the listing have
#'        baths - how many bathrooms does the listing have
#'        sqft - what is the posted area of the listing
#'        address - what (if any) is the address of the listing
#'        lat - what (if posted) is the latitude of the listing
#'        long - what (if posted) is the longitude of the listing)
#'        rent_pp - price divided by beds
#'        ft_pp - sqft divided by beds
#'        in_region - is the lat and long of the listing within the rectangular
#'                    region defined by the region_lat and region_long arguments
#'        still_up - is the posting still up? Always TRUE
#'        category - what is the category specified by the category argument
#' 
scrape_craigslist <- function(
    craigslist_domain = "vancouver",
    has_pic = TRUE,
    min_bed = 1,
    max_bed = 4,
    min_bath = 1,
    query = "kitsilano",
    category = "apa",
    region_lat = c(49.256, 49.28),
    region_long = c(-123.19, -123.127)) {
  
  require(rvest)
  require(stringr)
  
  # A function that runs tryCatch but returns the messages in a list
  # 
  # @param expr The expression to run
  # @return Returns list with $value $warning $error
  #         
  #         If successful, $warning and $error will be \code{NULL}
  #         
  #         If warning, $error will be \code{NULL} ($value will be present)
  #         
  #         If error, $warning and $value will be \code{NULL}
  #         
  #' I didn't write this, see: https://stackoverflow.com/a/24569739/14805829
  myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
      tryCatch(expr, error=function(e) {
        err <<- e
        NULL
      }), warning=function(w) {
        warn <<- w
        invokeRestart("muffleWarning")
      })
    list(value=value, warning=warn, error=err)
  }
  
  print("Scraping search results")
  query_url <- paste0("https://", craigslist_domain, ".craigslist.org/search/",
                      category, "?",
                      paste(c(paste0("hasPic=", ifelse(has_pic, 1, 0)),
                              paste0("min_bedrooms=", min_bed),
                              paste0("max_bedrooms=", max_bed),
                              paste0("min_bathrooms=", min_bath),
                              paste0("query=", query)),
                            collapse = "&"))
  
  raw_query <- read_html(query_url)
  raw_ads <- html_elements(raw_query, ".cl-static-search-result")
  
  out <- data.frame(
    posted = NA, updated = NA, available = NA,
    title = html_attr(raw_ads, "title"),
    link = html_attr(html_elements(raw_ads, "a"), "href"),
    price = html_text2(html_elements(raw_ads, "div.price")),
    location = html_text2(html_elements(raw_ads, "div.location")),
    beds = NA, baths = NA, sqft = NA,  
    address = NA, lat = NA, long = NA
  )
  
  print(paste("Scraping", nrow(out), "post pages"))
  for (i in 1:nrow(out)) {
    for(tries in 1:5) {
      attempt <- myTryCatch(read_html(out$link[i]))
      if(!is.null(attempt$value)) {break}
    }
    
    if(!is.null(attempt$value)) {
      raw_page <- attempt$value
      
      bed_bath_sqft <- html_text2(html_element(raw_page, ".attrgroup"))
      out$beds[i] <- str_extract(bed_bath_sqft, "\\d+BR")
      out$baths[i] <- str_extract(bed_bath_sqft, "\\d+Ba")
      out$sqft[i] <- str_extract(bed_bath_sqft, "\\d+ft2")
      
      if(length(html_elements(raw_page, ".attr+.important+.available-now")) > 0) {
        out$available[i] <- "Now"
      } else {
        attrgroup <- html_text2(html_elements(raw_page, ".attrgroup .important"))
        grep_match <- grep("available", attrgroup, value = TRUE)
        if(length(grep_match > 0)) {
          out$available[i] <- gsub("available ", "", grep_match)}
      }
      
      location <- html_element(raw_page, ".mapbox")
      if(!is.na(location)) {
        out$lat[i] <- 
          as.numeric(html_attr(html_element(location, "div"), "data-latitude"))
        out$long[i] <- 
          as.numeric(html_attr(html_element(location, "div"), "data-longitude"))
        out$address[i] <- html_text2(html_element(location, ".mapaddress"))
      }
      
      dates <- html_elements(raw_page, ".postinginfos .postinginfo.reveal")
      if(length(dates) > 0) {out$posted[i] <- html_text2(dates[1])}
      if(length(dates) > 1) {out$updated[i] <- html_text2(dates[2])}
      if(i %in% round(seq(from = 0, to = nrow(out), by = nrow(out)/10))) {
        print(paste(i, "/", nrow(out), "done"))}
    }
    
    #Pause to avoid getting flagged as spammer by Craigslist
    Sys.sleep(0.25)
  }
  
  out$price <- as.numeric(gsub("[,$]", "", out$price))
  out$beds <- as.numeric(gsub("BR", "", out$beds))
  out$baths <- as.numeric(gsub("Ba", "", out$baths))
  out$sqft <- as.numeric(gsub("ft2", "", out$sqft))
  
  out$rent_pp <- out$price/out$beds
  out$ft_pp <- out$sqft/out$beds
  
  out$in_region <- 
    ifelse(out$lat > min(region_lat) & out$lat < max(region_lat) &
             out$long > min(region_long) & out$long < max(region_long),
           TRUE, FALSE)
  
  out$posted <- gsub("posted: ", "", out$posted)
  out$updated <- gsub("updated: ", "", out$updated)
  out$available <- gsub("now", "Now", out$available)
  out$still_up <- TRUE
  out$category <- category
  
  return(out)
}

