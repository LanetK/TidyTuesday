library(tidyverse)
library(xkcd)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

phylo_squirrel <- png::readPNG("2019w44/Sciuridae.png")


#Process Squirrel census ----
nyc_squirrels2 <- nyc_squirrels %>% 
  mutate(date = lubridate::parse_date_time(date, "mdy"),
         weekend = lubridate::wday(date) %in% 6:7)

perc_actions <- nyc_squirrels %>% 
  group_by(primary_fur_color) %>% 
  summarize_if(is.logical, sum) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, list(~round(./sum(.)*100)))


use_squirrels <- nyc_squirrels %>% 
  count(primary_fur_color, highlight_fur_color) %>% 
  filter(!is.na(primary_fur_color)) %>% 
  top_n(16, n) %>% 
  mutate(color = ifelse(is.na(highlight_fur_color),
                        tolower(paste0(primary_fur_color, " squirrels")),
                        tolower(paste0(primary_fur_color, " squirrels with ", highlight_fur_color, " highlights")))) 

### Do squirrels ____ more on weekends? -----
dat_sq <- nyc_squirrels2 %>% 
  mutate_if(is.logical, as.numeric) %>% 
  drop_na(primary_fur_color) %>% 
  mutate(highlights = str_split(highlight_fur_color, ", ")) %>% 
  inner_join(use_squirrels) %>% 
  arrange(primary_fur_color, desc(highlight_fur_color))

names(dat_sq)

choose_activity <- "eating"

squirrel_formula = as.formula(paste0(choose_activity, " ~ ", "weekend"))

wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_sq)

wknd_md %>% broom::tidy() %>% 
  filter(term == "weekend") %>% 
  pull(p.value)

#Squirrel raster ----
sq_raster <- phylo_squirrel[,,4] %>% 
  as.data.frame() %>% 
  mutate(y=n() - row_number() + 1) %>% 
  gather(x, value, -y) %>% 
  mutate(x = as.numeric(str_remove(x, "V"))) %>% 
  filter(value >= 0.9) %>% 
  group_by(x = x%/%5, y = y%/%5) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  #Color groups
  mutate(
    color_trigger = (x^2+y^2)^(1/2) %% 12,
    color_group = case_when(
      color_trigger >= 3 & color_trigger <= 5 ~ "Highlight1",
      color_trigger >= 7 & color_trigger <= 8 ~ "Highlight2",
      color_trigger >= 10 & color_trigger <= 11 ~ "Highlight3",
      TRUE ~ "Base"
    )
  )

sq_raster %>% 
  ggplot(aes(x,y)) +
  geom_raster(aes(alpha = value, fill = color_group)) +
  scale_alpha_identity() +
  coord_fixed()


#Run all----


panel_4_plots <- unique(dat_sq$color) %>% 
  purrr::map(function(cc){
    dat_cc <- dat_sq %>% 
      filter(color == cc)
    
    if(nrow(dat_cc) < 3) {return(NULL)}
    
    use_highlights <- dat_cc[["highlights"]][[1]]
    
    wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_cc)
    
    # summary(wknd_md)
    
    cc_p <- wknd_md %>% broom::tidy() %>% 
      filter(term == "weekend") %>% 
      pull(p.value)
    
    
    if(cc_p < 0.05){
      out_text <- paste0("We found a link between ", cc, " & ", choose_activity, 
                        " more on weekdays! (p=", round(cc_p, 3), ")")
    } else {
      out_text <- paste("We found no link between", cc, "&", choose_activity, 
                        "more on weekdays. (p>0.05)")
    }
    
    plot_text <- out_text %>% 
      strwrap(width = 32) %>% 
      paste(collapse = "\n")
    
    sq_fill_fun <- function(col_name) {
      case_when(
        col_name == "Gray" ~ "#BBBBC9",
        col_name == "Cinnamon" ~ "#D2691E",
        col_name == "Black" ~ "#222222",
        TRUE ~ "#efefef"
      )
    }
    
    if(is.na(use_highlights[1])){
      use_highlights <- rep(dat_cc[["primary_fur_color"]][1], 3)
    } else if(length(use_highlights) == 1) {
      use_highlights <- c(use_highlights, use_highlights, dat_cc[["primary_fur_color"]][1])
    } else if(length(use_highlights) == 2) {
      use_highlights <- c(use_highlights[1], use_highlights[2], dat_cc[["primary_fur_color"]][1])
    } else {
      use_highlights <- use_highlights
    }
    
    
    this_squirrel <- sq_raster %>% 
      mutate(use_fill = case_when(
        color_group == "Base" ~ sq_fill_fun(dat_cc[["primary_fur_color"]][1]),
        color_group == "Highlight1" ~ sq_fill_fun(use_highlights[1]),
        color_group == "Highlight2" ~ sq_fill_fun(use_highlights[2]),
        color_group == "Highlight3" ~ sq_fill_fun(use_highlights[3])
      ))
    
    
    x_expand = 50
    y_expand = 1.75
    
    (out_p <- this_squirrel %>% 
        ggplot(aes(x+x_expand/2,y+5)) +
        geom_rect(xmin = 0, xmax = max(this_squirrel$x) + x_expand, 
                  ymin = 0, ymax = max(this_squirrel$y)*y_expand,
                  color = "black", fill="white", size = 1.1) +
        geom_raster(aes(alpha = value, fill = use_fill)) +
        scale_alpha_identity()+
        scale_fill_identity() +
        coord_fixed(xlim = c(0, max(this_squirrel$x) + x_expand), 
                    ylim = c(0, max(this_squirrel$y)*y_expand+15),
                    expand=F) +
        theme_void() +
        theme(text=element_text(size=16,  family="Comic Sans MS")) +
        NULL)
    
    if(cc_p < 0.05) {
      out_p <- out_p +
        annotation_custom(grid::textGrob(plot_text, hjust = 0.5, vjust = 1, x=0.5, y=1,
                                         gp = grid::gpar(fontsize = 10, family = 'Comic Sans MS',
                                                         col = "forestgreen", fontface="bold")),
                          xmin = 11, xmax = max(this_squirrel$x)+ x_expand -5, 
                          ymin = max(this_squirrel$y)*y_expand - 20, 
                          ymax = max(this_squirrel$y)*y_expand -10) +
        NULL
    } else {
      out_p <- out_p +
        annotation_custom(grid::textGrob(plot_text, hjust = 0.5, vjust = 1, x=0.5, y=1,
                                         gp = grid::gpar(fontsize = 10, family = 'Comic Sans MS')),
                          xmin = 11, xmax = max(this_squirrel$x)+ x_expand-5, 
                          ymin = max(this_squirrel$y)*y_expand - 20, 
                          ymax = max(this_squirrel$y)*y_expand -10) +
        NULL
    }
    
    return(out_p)
  })


panel_4_plots[sapply(panel_4_plots, is.null)] <- NULL


png(filename = "tidy_squirrels.png",
    width = 640, height = 760)

gridExtra::grid.arrange(grobs = panel_4_plots, padding = 0)

dev.off()



for(cc in unique(dat_sq$color)){ 
    dat_cc <- dat_sq %>% 
      filter(color == cc)
    
    if(nrow(dat_cc) < 3) {return(NULL)}
    
    use_highlights <- dat_cc[["highlights"]][[1]]
    
    wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_cc)
    
    # summary(wknd_md)
    
    cc_p <- wknd_md %>% broom::tidy() %>% 
      filter(term == "weekend") %>% 
      pull(p.value)
    
    
    if(cc_p < 0.05){
      out_text <- paste0("! We found a link between ", cc, " & ", choose_activity, 
                         " more on weekdays! (p=", round(cc_p, 3), ")")
    } else {
      out_text <- paste0("We found no link between ", cc, " & ", choose_activity, 
                        " more on weekdays. (p=", round(cc_p, 2), ")")
    }

    print(out_text)
  }


