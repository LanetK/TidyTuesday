library(tidyverse)
library(xkcd)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

phylo_squirrel <- png::readPNG("2019w44/Sciuridae.png")

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
                        tolower(paste0(primary_fur_color, " squirrels")),
                        tolower(paste0(primary_fur_color, " squirrels with ", highlight_fur_color, " highlights"))))
names(dat_sq)

choose_activity <- "running"

squirrel_formula = as.formula(paste0(choose_activity, " ~ ", "weekend"))

wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_sq)

wknd_md %>% broom::tidy() %>% 
  filter(term == "weekend") %>% 
  pull(p.value)

panel_4_plots <- unique(dat_sq$color) %>% 
  purrr::map(function(cc){
    dat_cc <- dat_sq %>% 
      filter(color == cc)
    
    if(nrow(dat_cc) < 3) {return(NULL)}
    
    wknd_md <- glm(squirrel_formula, family = "binomial", data = dat_cc)
    
    # summary(wknd_md)
    
    cc_p <- wknd_md %>% broom::tidy() %>% 
      filter(term == "weekend") %>% 
      pull(p.value)
    
    
    if(cc_p < 0.05){
      out_text <- paste("We found a link between", cc, "&", choose_activity, 
                        "on weekends! p-value is", round(cc_p, 3))
    } else {
      out_text <- paste("We found no link between", cc, "&", choose_activity, 
                        "on weekends. p-value is", round(cc_p, 2))
    }
    
    plot_text <-out_text %>% 
      strwrap(width = 28) %>% 
      paste(collapse = "\n")
    
    sq_fill <- case_when(
      dat_cc[["primary_fur_color"]][1] == "Gray" ~ "#CCCCD9",
      dat_cc[["primary_fur_color"]][1] == "Cinnamon" ~ "#D2691E",
      TRUE ~ "#222222"
    )
    
    
    out_p <- ggplot(datascaled, aes(x=x,  y=y)) + 
      geom_point(size = NA) +
      geom_rect(xmin = 5, xmax = 95, ymin = 5, ymax = 195,
                color = "black", fill = "white", size = 1.1) +
      xkcdman(man_mapping, dataman) +
      annotation_custom(grid::textGrob(plot_text, just="top", 
                                       gp = grid::gpar(fontsize = 10, family = 'Comic Sans MS')), 
                        xmin = 12, xmax = 88, ymin = 180, ymax=190) +
      # coord_fixed(expand = F) +
      theme_void() +
      theme(text=element_text(size=16,  family="Comic Sans MS"),
            plot.background = element_rect(fill = "blue")) +
      NULL
    
    if(cc_p < 0.05) {
      out_p <- out_p +
        annotation_raster(phylo_squirrel, xmin = 68, xmax = 94, ymin = 15, ymax = 40)+
        annotation_custom(grid::textGrob("Obviously", just="top", 
                                         gp = grid::gpar(fontsize = 10, family = 'Comic Sans MS')), 
                          xmin = 64, xmax = 94, ymin = 50, ymax=45) +
        NULL
    }
    
    return(out_p)
  })
  

panel_4_plots[sapply(panel_4_plots, is.null)] <- NULL

gridExtra::grid.arrange(grobs = panel_4_plots, padding = 0)
  
  datascaled <- data.frame(x=c(1,100),y=c(1,200))
  xrange <- range(datascaled$x)
  yrange <- range(datascaled$y)
  ratioxy <- diff(xrange) / diff(yrange)
  
  man_mapping <- aes(x=x,
                     y=y,
                     scale=scale,
                     ratioxy=ratioxy,
                     angleofspine = angleofspine,
                     anglerighthumerus = anglerighthumerus,
                     anglelefthumerus = anglelefthumerus,
                     anglerightradius = anglerightradius,
                     angleleftradius = angleleftradius,
                     anglerightleg =  anglerightleg,
                     angleleftleg = angleleftleg,
                     angleofneck = angleofneck)
  
  dataman <- data.frame( x= c(24, 58), y=c(120, 110),
                         scale = c(40, 36),
                         ratioxy = ratioxy,
                         angleofspine =  seq(- pi / 2 + pi/16, -pi/2, l=2) ,
                         anglelefthumerus = -pi/6,
                         anglerighthumerus = pi + pi/6,
                         angleleftradius = c(-pi/4, pi/4),
                         anglerightradius = c(-pi/4, pi),
                         angleleftleg = 3*pi/2  + pi / 12 ,
                         anglerightleg = 3*pi/2  - pi / 12,
                         angleofneck = runif(2, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10))
  
  
  
  
  
  