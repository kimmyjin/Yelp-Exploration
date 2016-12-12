library(stringr)
library(dplyr)
library(htmltab)
url = "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
population = htmltab(doc = url, which = "//*[@id='mw-content-text']/table[4]") %>%
  as.data.frame() 

population$`2010 population density` = gsub(x = population$`2010 population density`, 
                                            pattern = "(.*\\s)(\\d+.{0,1}\\d+)(.*)", replace = "\\2")
population$`2010 population density` = gsub(x = population$`2010 Census`, ",","") %>% as.numeric()
population = head(population[order(population$`2010 population density`, decreasing = TRUE),] %>% 
  select(City,State, `2010 population density`),20) 
