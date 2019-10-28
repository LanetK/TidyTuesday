library(tidyverse)
library(xkcd)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

nyc_squirrels2 <- nyc_squirrels %>% 
  mutate(date = lubridate::parse_date_time(date, "mdy"),
         weekend = lubridate::wday(date) %in% 6:7)

perc_actions <- nyc_squirrels %>% 
  group_by(primary_fur_color) %>% 
  summarize_if(is.logical, sum) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, list(~round(./sum(.)*100)))


nyc_squirrels %>% 
  count(primary_fur_color, highlight_fur_color) %>% 
  filter(n > 3, !is.na(primary_fur_color))

### Do squirrels ____ more on weekends?
dat_sq <- nyc_squirrels2 %>% 
  mutate_if(is.logical, as.numeric) %>% 
  drop_na(primary_fur_color) %>% 
  mutate(color = ifelse(is.na(highlight_fur_color),
                        primary_fur_color,
                        paste0(primary_fur_color, " w/ ", highlight_fur_color)))
names(dat_sq)

choose_activity <- "eating"

squirrel_formula = as.formula(paste0(choose_activity, " ~ ", "weekend"))

wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_sq)

wknd_md %>% broom::tidy() %>% 
  filter(term == "weekend") %>% 
  pull(p.value)

for(cc in unique(dat_sq$color)){
  dat_cc <- dat_sq %>% 
    filter(color == cc)
  
  if(nrow(dat_cc) < 3) {next}
  
  wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_cc)
  
  # summary(wknd_md)
  
  cc_p <- wknd_md %>% broom::tidy() %>% 
    filter(term == "weekend") %>% 
    pull(p.value)
  
  if(cc_p < 0.05){
    cat("We found a link between", cc, "squirrels &", choose_activity, "on weekends! p-value is", round(cc_p, 3), "\n")
  } else {
    cat("We found no link between", cc, "squirrels &", choose_activity, "on weekends. p-value is", round(cc_p, 2), "\n")
  }
}

library(extrafont)
font_import(pattern = "[C/c]omic")
