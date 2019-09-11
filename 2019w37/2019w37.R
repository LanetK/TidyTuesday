library(tidyverse)
library(tidytext)
library(ggvoronoi) # For the undercoloring... probably a better way
library(ggrepel) #For labels if added

# Tidy Tuesday data ----

tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

# Bodies image from phylopic.org
# http://phylopic.org/image/a9f4ebd5-53d7-4de9-9e66-85ff6c2d513e/

bodies_image <- png::readPNG("2019w37/bodies.png")[,,4] 

bodies_image_pro <- bodies_image %>% 
  data.frame() %>% 
  mutate(y = n() - row_number() + 1) %>%
  gather(x, value, -y) %>% 
  mutate(x = as.numeric(substr(x, 2, 6)),
         value = round(value, 2))

#The bodies "negative"
bodies_image2 <- bodies_image_pro %>%  
  filter(between(value, 0, 0.5))

#Body parts ----

tx_1 <- tx_injuries %>% 
  mutate_at(vars(body_part), tolower)

tx_body_sum <- tx_1 %>% count(body_part, sort = T, name = "tx")

bodyparts <- tx_body_sum %>% 
  pull(body_part)

# Use the names of body parts from tx_injuries data as search teams in the safer parks data
# I used tidytext here instead of str_detect because it was picking up false positives... like "hip" in "chipped"

safer_1 <- safer_parks %>% 
  #1-grams
  unnest_tokens(body_part, injury_desc, drop = F) %>% 
  # #Bi-grams
  # bind_rows(safer_parks %>% unnest_tokens(body_part, injury_desc, token = "ngrams", n = 2, drop = F)) %>% 
  filter(body_part %in% bodyparts)

# % of injuries with a matching body part
length(unique(safer_1$acc_id)) / nrow(safer_parks)

# Body part counts in safer_parks
safer_body_sum <- safer_1 %>% count(body_part, sort = T, name = "safer")

# Compare the two tables.... just an FYI
combined_sum <- tx_body_sum %>% 
  left_join(safer_body_sum)

#Reduce and map body parts ----

bodypart_key <- tibble::tribble(
  ~body_part, ~body_part_red,
  "forearm", "arm",
  "hands", "hand",
  "torso", "abdomen",
  "ribs", "abdomen",
  "toes", "toe",
  "neck", "neck | collarbone",
  "collarbone", "neck | collarbone",
  "chin", "chin | cheek | face",
  "face", "chin | cheek | face",
  "cheek", "chin | cheek | face",
  "eye", "eye | eyebrow",
  "eyebrow", "eye | eyebrow",
  "mouth", "mouth | tooth",
  "tooth", "mouth | tooth",
  "teeth",  "mouth | tooth",
  "lip", "mouth | tooth",
  "groin", "groin | butt",
  "butt", "groin | butt",
  "glutes", "groin | butt",
  "vagina", "groin | butt",
  "tailbone", "groin | butt"
)

# Export the below to get a list of body parts for the xy coords
# You don't need to do this since I already have the coords saved in this repo
safer_body_sum %>% 
  drop_na() %>% 
  left_join(bodypart_key) %>% 
  mutate(body_part_red = ifelse(is.na(body_part_red), body_part, body_part_red)) %>% 
  count(body_part = body_part_red, wt=safer, sort=TRUE)

# NOTE: From here, I exported this table to csv/Excel, then uploaded the bodies.png file to 
#    https://www.mobilefish.com/services/record_mouse_coordinates/record_mouse_coordinates.php
#    In order of the table above, I clicked the points on the image to get the xy coords
#    Then combined that table with the one above to have xy coords for each body part

# I use the male and female outlines together, allowing overlapping or close-proximity parts
#    to be on different bodies.
# e.g. the cent of the male is "chest", while the center of the female is "back"

# Final Body part name mapping

bodies_map <- read_csv("2019w37/bodies_map.csv")

#Reduce the body parts to a smaller list and combine with xy coords
bodies_data <- safer_body_sum %>% 
  drop_na() %>% 
  left_join(bodypart_key) %>% 
  mutate(body_part_red = ifelse(is.na(body_part_red), body_part, body_part_red)) %>% 
  count(body_part = body_part_red, wt=safer, sort=TRUE) %>% 
  left_join(bodies_map %>% 
              rename(x=x_coord, y=y_coord)) %>% 
  drop_na(x) %>% 
  mutate(y = nrow(bodies_image) - y +1)

## Intermediate graphic 
# This overlaps the negative outline of the bodies on top of the voronoi of the body part location
# Gradient fill is the count of injuries to that body part
bodies_data %>% 
  ggplot(aes(x, y)) +
  geom_voronoi(aes(fill = n)) +
  scale_fill_gradientn(colors = c("#4040FF", "#40FF40", "#FF4040")) +
  geom_raster(data = bodies_image2, fill = "white") +
  coord_fixed(xlim = c(1, 396), ylim = c(1, 512), expand = F) +
  theme_void()


#Create an alpha layer over this image, with circles on top of the body parts affected.
# I chose to do this rather than put colored circles on top of the image so that...
#   (1) The circles don't leave the boundary of the bodies
#   and (2) Circles dont overlap with each other.
# This is not the most efficient approach, but I didn't want to spend too much time on it.

bodies_data2 <- bodies_data %>% 
  mutate(circle = purrr::pmap(list(x, y, n), function(x, y, n){
    radius = 1
    size = (n)^(1/2) #Circle radius is sqrt of the n
    
    dat <- crossing(
      xx = round(x - radius*size):round(x + radius*size),
      yy = round(y - radius*size):round(y + radius*size)) %>% 
      mutate(dist = ((xx-x)^2 + (yy-y)^2)^(1/2)) %>% 
      filter(dist <= radius*size)
    
    return(dat)
  })) %>% 
  select(-x, -y) %>% 
  unnest() %>% 
  mutate(alpha = dist / max(dist)) %>% #3D effect... alpha value decays further from the center
  rename(x=xx, y=yy) %>% 
  group_by(x, y) %>% 
  filter(alpha == min(alpha)) %>% 
  ungroup()

#Render the image, carving out the fading circles
bodies_image3 <- bodies_image_pro %>% 
  left_join(bodies_data2) %>% 
  mutate(alpha = case_when(
    value < 0.49 ~ 0,
    !is.na(alpha) ~ alpha,
    TRUE ~ value
  ),
  alpha = ifelse(alpha > 0.9, 1, alpha))

#Here I edit the bodies image negative to have wider bounds
bodies_image2a <- bodies_image2 %>% 
  bind_rows(crossing(x=-100:0, y=1:max(bodies_image2$y), value = 0))%>% 
  bind_rows(crossing(x=397:500, y=1:max(bodies_image2$y), value = 0))

# The top layer only
# bodies_image3 %>%
#   ggplot(aes(x, y)) +
#   geom_raster(aes(alpha = alpha)) +
#   scale_alpha_identity() +
#   coord_fixed()

bodies_data %>% 
  ggplot(aes(x, y)) +
  geom_voronoi(aes(fill = n)) +
  scale_fill_gradientn(colors = c("#5384ff", "#9affd0", "#ffed89", 
                                  "#ff9e53", "#ff6141", "#ff25ab"),
                       breaks = seq(0, 1200, length.out = 6)) +
  geom_raster(data = bodies_image2a, fill = "#fcedcc") +
  geom_raster(data = bodies_image3, aes(alpha = alpha),
              fill = "#00436b") +
  scale_alpha_identity() +
  # geom_label_repel(data = bodies_data, aes(label = body_part, fill = n),
  #                  alpha = 0.9) +
  coord_fixed(xlim = c(-100, 500), ylim = c(1, 512), expand = F) +
  labs(
    title = "Heads, Shoulders, Knees, and\nWHOOOoooOOOOooooAAAAaaaaAAAaaaHHHHhhh!!!!!",
    subtitle = "Theme park injuries by body part location",
    caption = "#TidyTuesday, 2019w37
    data: saferparksdata.org | image: phylopic.org
    @RyanTimpe",
    fill = "Count"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#fcedcc"),
    panel.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(face = "bold", size = 12)
  )

# ggsave("2019w37/bodyparts.png", device = "png", height = 6, dpi=600)
