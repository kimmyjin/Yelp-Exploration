library(rvest)
library(dplyr)
library(stringr)
# retrive page from the Wikipedia url 
page = read_html(paste0("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"))
# retrive table from page
table = page %>%
  # speficy the table nodes
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[4]') %>%
  # retrive table 
  html_table()
# assign table with the first elment of the table list 
table = table[[1]]

table = table %>% select(City, `State[5]`, `2010 population density`)
# clean up variable of City by droppinng numbers following some of the city names 
table$City = str_replace(table$City, "\\[\\d+\\]", "")
# only extract the per sq mi values for `2010 population density` and transform into numeric values 
table$`2010 population density` = str_replace(table$`2010 population density`, "\\d+\\♠", "")
table$`2010 population density` = str_replace_all(table$`2010 population density`, ",", "")
table$`2010 population density` = as.numeric(str_extract(table$`2010 population density`, "\\d+"))

table = table %>% 
  arrange(desc(`2010 population density`)) %>% 
  head(.,20)

# revise column names
names(table)[2] = paste("State")
names(table)[3] = paste("2010 population density(per sq mi)")


city = table$City
city = str_replace(city, " ","")
