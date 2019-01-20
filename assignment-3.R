library(tidyverse)
library(xml2)
library(RCurl)
library(stringr)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'This is a specific function for retrieving country name, country link, population, and population 
#'rank from the CIA World Factbook.
#' @return A dataframe
#' @examples x <- get_population_ranking()
#' @export get_population_ranking
get_population_ranking <- function(url){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  
  #download url 
  url = str_c(base_url, "fields/335rank.html")
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  #create empty list to store results
  myList<- vector("list", 4)
  xpath <- vector("character", length = 4)
  
  #execute xpath queries
  for (i in seq_len(4)) {
    xpath[[i]] <- xpath_expressions[[i]]
    text = raw_html %>% 
      xml_find_all(xpath[[i]]) %>% 
      as_list() %>% 
      unlist() %>% 
      strsplit("\"$") %>% 
      enframe() %>% 
      select(-name) -> myList[[i]] 
  }
  
  #turn list into df and make adjustments
  output <- data.frame(matrix(unlist(myList), nrow=238))
  colnames(output) <- c("country_link", "country", "population", "rank.population")
  output[[1]] <- str_extract(output[[1]], "[^../]+.{8}")              
  output
}

#' Question 2: Retrieve Land Area
#'This function visits each supplied url and retrieves the land area element from the page.
#' @param country_link A character vector of one or more country_link urls
#'
#' @return A character vector of the land areas
#' @export get_land_area
#'
#' @examples 
#' df <- get_population_ranking()
#' links <- df$country_link
#' get_land_area(country_link = links)

get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  
  #some output vectors
  output <- vector("character", length = length(country_link))
  url <- vector("character", length = length(country_link)) 
  raw_html <- vector("list", length = length(country_link))
  url = str_c(base_url, country_link)
  
  #download the file from country_link and execute the xpath query
  for (i in seq_along(country_link)) {
    raw_html[[i]] <- read_html(getURL(url[i], .encoding = "UTF-8", .opts = list(followlocation = FALSE))) %>% 
    xml_find_all(xpath) %>% 
      as_list() %>% 
      unlist() 
    output[i] <- raw_html[[i]]
  }
  output
}

#' Question 3: Get Population Density
#'This function computes population density for each country based on land area and population.
#' @return A dataframe including land area and population density.
#' @export get_population_density
#'
#' @examples p_density <- get_population_density()
#' 
get_population_density <- function(){
  pop_ranking <- get_population_ranking() #retrieve ranking
  links <- pop_ranking$country_link 
  area <- get_land_area(country_link = links) #retrive land area
  area[10] <- 1000000 #replace 1 million with 1000000
  area <- str_replace_all(area, "[,]", "") #remove commas
  area <- str_extract(area, "[0-9]+.[0-9]*")#remove sq km
  area <- as.numeric(area)
  
  #convert pop to numeric vector:
  pop <- as.character(pop_ranking$population) %>%  str_replace_all("[,]", "") %>% as.numeric()
  
  #create output df:
  pop_ranking <-  mutate (pop_ranking,
    land_area = area, 
    population_density = pop/area)
  pop_ranking
} 

#' Question 4: Get All Provided Rankings
#'This function retrieves the overview of all the available rankings.
#' @return A dataframe
#' @export get_rankings
#'
#' @examples rankings <- get_rankings()
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  text1 = raw_html %>% xml_find_all("//div[@class='field_label']/strong/a") %>% 
    as_list() %>% 
    unlist() %>% 
    str_replace_all("[:]", "") %>% 
    str_to_lower() %>% 
    enframe() %>% 
    select(-name) 
  
  text2 = raw_html %>% xml_find_all("//div[@class='field_label']/strong/a/@href") %>% 
    as_list() %>% 
    unlist() %>%
    str_extract("[^../]+.{8}") %>% 
    enframe() %>% 
    select(-name) 
  
  output <- cbind(text1, text2)
  colnames(output) <- c("characteristic", "characteristic_link")
  #I add .html to the links, to make them work:
  output$characteristic_link <- str_c(output$characteristic_link, ".html") 
  output
}

#' Question 5 - Part 1: Get Ranking
#'This function can retrieve any given ranking as specified by the user.
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#' 
#' @return A dataframe
#' @export get_ranking
#'
#' @examples myranking <- get_ranking()
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  
  url1 <- str_c(base_url, url) 
  xpath <- vector("character", length = 4)
  raw_html <- read_html(getURL(url1, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  myList<- vector("list",4)
  
  for (i in seq_len(4)) {
    xpath[[i]] <- xpath_expressions[[i]]
    #iterate and save into df cols
    text = raw_html %>% 
      xml_find_all(xpath[[i]]) %>% 
      as_list() %>% 
      unlist() -> myList[[i]]
  }
  
  name <- str_c("rank_", characteristic)
  output <- data.frame(matrix(unlist(myList), nrow = length(myList[[1]])))
  output[[1]] <- str_extract(output[[1]], "[^../]+.{8}")    
  output %>% rename(!!characteristic:=X3, country_link = X1, country = X2, !!name:=X4) -> output
  output
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link A character vector with the country-specific url string(s)
#' @param xpath_field_id A character vector denoting the field of interest
#' @param item An integer specifying which item from the field should be extracted
#'
#' @return A character vector
#' @export get_country_characteristic
#'
#' @examples 
#' get_country_characteristic(country_link = "geos/ch.html", xpath_field_id = "field-area", item = 2)
#' 
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath
  xpath <- str_c("//div[@id='", xpath_field_id, "']/div[", item, "]/span[", item, "]")
  url <- vector("character", length = length(country_link)) 
  myList <- vector("list", length = length(country_link))
  url = str_c(base_url, country_link)
    
  #download the file from country_link and execute the xpath query
  for (i in seq_along(country_link)){
    read_html(getURL(url[i], .encoding = "UTF-8", .opts = list(followlocation = FALSE))) %>% 
      xml_find_all(xpath) %>% 
      as_list() %>% 
      unlist() %>%  
      enframe() %>% 
      select(-name) -> myList[[i]]
  }
  output <- as.vector(unlist(myList))
  output
}

#' Question 6: Combine Rankings
#'This function downloads multiple rankings.
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return A large dataframe
#' @export combine_rankings
#'
#' @examples 
combine_rankings <- function(rankings){
  url <- rankings$characteristic_link
  characteristic <- rankings$characteristic
  
  #output storage:
  out <- vector("list", length = (nrow(rankings)))
  
  #loop over urls to retrieve data:
  for (i in seq_along(url)){
    out[[i]]  <- get_ranking(url = url[i], characteristic = characteristic[i])
  }
  
  #join dataframes:
  fin <- reduce(out, full_join)
  fin
}