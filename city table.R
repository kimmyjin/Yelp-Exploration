#loading the packages
library(rvest)
library(dplyr)
library(stringr)
library(htmltab)

#getting the url of the webpage
url = "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"

#scrapping the table from the wikipedia and saving it as dataframe
population = htmltab(doc = url, which = "//*[@id='mw-content-text']/table[4]") %>%
  as.data.frame() 

#extracting numbers from the `2010 population density` column
population$`2010 population density` = gsub(x = population$`2010 population density`, 
                                            pattern = "(.*\\s)(\\d+.{0,1}\\d+)(.*)", replace = "\\2")

#arranging the table with the order of 2010 population density
population$`2010 population density` = str_replace(population$`2010 population density`, ",","")%>%as.numeric()
table = population %>% 
  arrange(desc(`2010 population density`)) %>% 
  head(.,20)

#getting the city names from the re-arranged dataframe
City_name = table$City %>% str_replace(" ","") 
