park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")


library(tidyverse)

unique(park_visits$parkname)

unique(park_visits$parkname)[str_detect(unique(park_visits$parkname), "Birth")]


park_birth <- park_visits %>% 
  filter(str_detect(tolower(parkname), "birth"))
         
library(tidytext)

park_visits %>% 
  select(parkname) %>% 
  distinct() %>% 
  unnest_tokens(word, parkname, drop = FALSE) %>% 
  inner_join(get_sentiments("nrc"))
