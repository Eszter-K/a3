library(tidyverse)
library(xml2)
library(RCurl)
library(stringr)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'This is a specific function for retrieving country name, country link, population, and population 
#'rank from the CIA World Factbook.
#'Find the generalized version of the function in 
#' @return 
#' @export
#'
#' @examples

get_population_ranking <- function(url){
  
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  
  url = str_c(base_url, "fields/335rank.html")
  
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  myList<- vector("list",4)
  xpath <- vector("character", length = 4)
  
  for (i in seq_len(4)) {
    xpath[[i]] <- xpath_expressions[[i]]
    text = raw_html %>% xml_find_all(xpath[[i]]) %>% as_list() %>% unlist() %>% 
      strsplit("\"$") %>% enframe() %>% select(-name) -> myList[[i]] 
  }
  
  output <- data.frame(matrix(unlist(myList), nrow=238))
  colnames(output) <- c("country_link", "country", "population", "rank.population")
  output[[1]] <- str_extract(output[[1]], "[^../]+.{8}")              
  output
}

#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return
#' @export
#'
#' @examples

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
    xml_find_all(xpath) %>% as_list() %>% unlist() 
 output[i] <- raw_html[[i]]
  }
  output
}

#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  pop_ranking <- get_population_ranking()
  links <- pop_ranking$country_link
  area <- get_land_area(country_link = links)
  area[10] <- 1000000 #replace 1 million with 1000000
  area <- str_replace_all(area, "[,]", "") #remove commas
  area <- str_extract(area, "[0-9]+.[0-9]*")#remove sq km
  area <- as.numeric(area)
  
  #convert pop to numeric vector
  pop <- as.character(pop_ranking$population) %>%  str_replace_all("[,]", "") %>% as.numeric()
  #create output df
  pop_ranking <-  mutate (pop_ranking,
    land_area = area, population_density = pop/area)
  pop_ranking
} 
pop <- get_population_density()

#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href") #this is unnecesary
  
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  text1 = raw_html %>% xml_find_all("//div[@class='field_label']/strong/a") %>% as_list() %>% unlist() %>%
    str_replace_all("[:]", "") %>% str_to_lower() %>% enframe() %>% select(-name) 
  
  text2 = raw_html %>% xml_find_all("//div[@class='field_label']/strong/a/@href") %>% as_list() %>% unlist() %>%
    str_extract("[^../]+.{8}") %>% enframe() %>% select(-name) 
  
  output <- cbind(text, text2)
  colnames(output) <- c("characteristic_link", "characteristic")
  output
}

#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
}


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}



