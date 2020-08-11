library(tidyverse)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

# View(tuesdata$avatar)
# View(tuesdata$scene_description)

# PART 1: Non-bending rules ----
#List of rule data frames
avatar_rules <- list()

#Remind/tell someone Aang's the avatar - AIR
avatar_rules[["is_the_avatar"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "[i'm|am|are|be] the avatar"))%>%
  filter(character == "Aang")

#Yip - AIR
avatar_rules[["yip"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "yip"))

#Avatar state - AIR
avatar_rules[["avatar_state"]] <- tuesdata$avatar %>%
  left_join(tuesdata$scene_description) %>%
  filter(str_detect(tolower(scene_description), "avatar state"))

#Katara - hope - WATER
avatar_rules[["hope"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "hope"))%>%
  filter(character == "Katara")

#Sokka - boomerang - WATER
avatar_rules[["boomerang"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "boomerang"))%>%
  filter(character == "Sokka")

#Toph - blind - EARTH
avatar_rules[["blind"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "blind"))%>%
  filter(character == "Toph")

#Joo Dee! - EARTH
avatar_rules[["joodee"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "joo dee"))

#My cabbages! - EARTH
avatar_rules[["cabbages"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "my cabba"))

#Bumi

#Zukko - uncle - FIRE
avatar_rules[["uncle"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "uncle"))%>%
  filter(character == "Zuko")

#Iroh - tea - FIRE
avatar_rules[["tea"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "tea[\\W]"))%>%
  filter(character == "Iroh")

#Azula - father - FIRE
avatar_rules[["father"]] <- tuesdata$avatar %>%
  filter(str_detect(tolower(character_words), "father"))%>%
  filter(character == "Azula")

#Combined these rules into a single data frame
#First link my list names to a character/rule/element
names(avatar_rules)
tibble::tribble(
  ~rule, ~rule_character, ~element, ~description,
  "is_the_avatar", "Aang", "Air", "I'm/You're/He's the Avatar!",
  "yip", "Appa", "Air", "Yip, yip!",
  "avatar_state", "Avatar", "Air", "an Avatar enters the Avatar State",
  "hope", "Katara", "Water", "mentions 'hope'",
  "boomerang", "Sokka", "Water", "talks about his boomerang",
  "tea", "Iroh", "Fire", "mentions 'tea'",
  "uncle", "Zuko", "Fire", "says 'uncle'",
  "father", "Azula", "Fire", "says 'father'",
  "blind", "Toph", "Earth", "has to remind someone she's blind",
  "cabbages", "Cabbage merchant", "Earth", "My cabbages!",
  "joodee", "Joo Dee", "Earth", "Encounter with Joo Dee"
) -> avatar_rule_detail

avatar_rules %>%
  purrr:::map_dfr(~.x, .id="rule") %>%
  count(rule)

avatar_rules %>%
  purrr:::map_dfr(~.x, .id="rule") %>%
  left_join(avatar_rule_detail) %>%
  select(rule,
         id, book, book_num, chapter, chapter_num,
         character, rule_character, element, description) -> dg_characters

#Track bending ----

#First get bending from the scene description in the avatar data file
tuesdata$avatar %>%
  filter(character == "Scene Description") %>%
  filter(str_detect(full_text, "bend")) %>%
  #First layer of bend testing
  mutate(bend1 = case_when(
    str_detect(full_text, "airbend|air bend") ~ "Air",
    str_detect(full_text, "waterbend|water bend|bloodbend|blood bend|icebend|ice bend") ~ "Water",
    str_detect(full_text, "firebend|fire bend|lightning") ~ "Fire",
    str_detect(full_text, "earthbend|earth bend|metalbend|metal bend|sandbend|sand bend") ~ "Earth"
  )) %>%
  mutate(bend2 = case_when(
    str_detect(full_text, "water|Katara") ~ "Water",
    str_detect(full_text, "fire|Zuko|Azula|Iroh") ~ "Fire",
    str_detect(full_text, "earth|Toph|Bumi|king|rock|boulder|Haru") ~ "Earth",
    str_detect(full_text, "air") ~ "Air"
  )) %>%
  mutate(performed_bending = ifelse(is.na(bend1), bend2, bend1)) -> bend_avatar

#then get from scene description file.
# Try to relate each character to their type of bending
tuesdata$avatar %>%
  inner_join(tuesdata$scene_description) %>%
  filter(str_detect(scene_description, "bend")) %>%
  pull(character) %>%
  unique() -> bending_chars

c(NA, "Fire", "AVATAR",
  "Water", "Earth", "Earth",
  "Earth", "Fire", "Fire",
  "Fire", "AVATAR", NA,
  NA, "Fire", NA,
  "Water", "Water", "Water",
  "AVATAR", NA, "Fire",
  "Earth", NA, "Earth",
  "Earth", "Earth", "Earth",
  "Fire", "Earth", NA,
  "Fire", "Fire", "Fire",
  "Water", "Fire", NA,
  "Water", "Fire", "Water",
  "Fire", "Fire"
) -> bending_type

bending_table <- tibble(character = bending_chars,
                        bending = bending_type)

#Add the bending type to the bending scene directions for the character
tuesdata$avatar %>%
  inner_join(tuesdata$scene_description) %>%
  filter(str_detect(scene_description, "bend")) %>%
  left_join(bending_table) %>%
  drop_na(bending) %>%
  #Some clean up in case bending is mentioned in the direction but not performed
  # This is rough, but just matches the described bending with the bender's power
  # Also clears up ambiguity with avatars
  mutate(described_bending = case_when(
    str_detect(tolower(scene_description), "airbend|air bend") ~ "Air",
    str_detect(tolower(scene_description), "waterbend|water bend|bloodbend|blood bend|icebend|ice bend") ~ "Water",
    str_detect(tolower(scene_description), "firebend|fire bend|lightning") ~ "Fire",
    str_detect(tolower(scene_description), "earthbend|earth bend|metalbend|metal bend|sandbend|sand bend") ~ "Earth"
  )) %>%
  mutate(performed_bending = case_when(
    bending == "AVATAR" & !is.na(described_bending) ~ described_bending,
    bending == "AVATAR" & character == "Aang" ~ "Air",
    bending == "AVATAR" & character == "Roku" ~ "Fire",
    bending == "AVATAR" & character == "Kyoshi" ~ "Air",
    is.na(described_bending) ~ bending,
    TRUE ~ described_bending
  )) -> bend_scene

#Combine two tables, keep 1 row per ID * element
bind_rows(bend_avatar, bend_scene) %>%
  drop_na(performed_bending) %>%
  group_by(id, performed_bending) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  mutate(rule = "bending" ,
         rule_character = character,
         element = performed_bending,
         description = "Character bends") %>%
  select(rule,
         id, book, book_num, chapter, chapter_num,
         character, rule_character, element, description) -> dg_bending

#COMBINE RULES ----

#First, estimate timestamps of each ID in the avatar dataframe
# Assume episodes are 21min each and assume
tuesdata$avatar %>%
  #Leave the "Scene Description" characters in
  # This assumes that the more description, the more action, which takes up time
  group_by(book_num) %>%
  mutate(
    num_episodes = max(chapter_num),
    timestamp = row_number()/n() * 21*60*num_episodes) %>%
  ungroup() %>%
  select(id, timestamp) -> avatar_timestamps

bind_rows(dg_bending, dg_characters) %>%
  #Different rule elements has different drinks
  mutate(drinks = case_when(
    rule    == "bending" ~ 1,
    element == "Earth" ~ 3,
    element == "Water" ~ 4,
    element == "Fire" ~ 1,
    element == "Air" ~ 2,
    TRUE  ~ 1
  )) %>%
  arrange(id) %>%
  group_by(book, element) %>%
  mutate(drinks_cum = cumsum(drinks)) %>%
  ungroup() %>%
  left_join(avatar_timestamps) %>%
  mutate(book_name = paste0("Book ", book_num, ": ", book)) -> avatar_dg

#PLOTTTTTT -----

library(tvthemes)
library(ggplot2)
library(extrafont)
#> Registering fonts with R
loadfonts(quiet = T)
# fonttable() %>% View()
# font_import()


col_air   = tvthemes:::theLastAirbender_palette$AirNomads
col_fire  = tvthemes:::theLastAirbender_palette$FireNation
col_earth = tvthemes:::theLastAirbender_palette$EarthKingdom
col_water = tvthemes:::theLastAirbender_palette$WaterTribe

col_air %>% scales::show_col()

avatar_dg %>%
  ggplot(aes(x=timestamp/60, y = drinks_cum, group = element)) +
  geom_line(aes(color = element), size = 1.2, alpha = 0.8) +
  geom_point(data = avatar_dg %>% filter(rule != "bending") %>%
               #Hack to get multiple color scales
               mutate(element = paste0(element, "1")),
             aes(color = element), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Fire1" = col_fire[2], "Air1" = col_air[1],
                                "Earth1" = col_earth[1], "Water1" = col_water[2],
                                #Point colors
                                "Fire" = col_fire[4], "Air" = col_air[5],
                                "Earth" = col_earth[2], "Water" = col_water[1])) +
  facet_wrap(~book_name, nrow = 3, strip.position = "top", scales = "free_x") +
  labs(title = "Avatar: The Last Airbender Drinking Game",
       subtitle = "Cumulative drinks, by element",
       caption = "Data points indicate bonus rules",
       y = NULL, x = "Time (minutes)") +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer",
               title.size = 14) +
  theme(legend.position = "none") -> p_cum

avatar_dg %>%
  count(book_name, element, wt=drinks, name = "drinks_total") %>%
  mutate(element = fct_relevel(element, "Water", "Earth", "Fire", "Air")) %>%
  ggplot(aes(x=element, y = drinks_total, group = element)) +
  geom_col(aes(color = element, fill=element), size = 1, alpha = 1) +
  scale_color_manual(values = c("Fire" = col_fire[2], "Air" = col_air[1],
                                "Earth" = col_earth[1], "Water" = col_water[2])) +
  scale_fill_manual(values = c(                    #Point colors
    "Fire" = col_fire[4], "Air" = col_air[5],
    "Earth" = col_earth[2], "Water" = col_water[1])) +
  facet_wrap(~book_name, nrow = 3, strip.position = "top", scales = "free_x") +
  labs(title = " ",
       subtitle = "Total drinks",
       y = NULL, x = "Element") +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer",
               title.size = 14) +
  theme(legend.position = "none") -> p_total

#Create rules
library(grid)
grobTree( rectGrob(gp=gpar(fill="#ece5d3", col = NA)),
          grid::textGrob("Rules:
Choose an element. Drink (1) sip each time someone bends using that element.
Bonus rules:",
                         just = "left", x = 0.01, y = 0.05, vjust = 0,
                         gp = grid::gpar(fontfamily = "Slayer", fontsize = 9))) -> p_detail

grobTree( rectGrob(gp=gpar(fill="#ece5d3", col = col_water[2])),
          grid::textGrob("Water
Drink (4):
Katara says 'hope'
Sokka talks about his boomerang",
                         just = "left", x = 0.05, y = 0.95, vjust = 1,
                         gp = grid::gpar(fontfamily = "Slayer", fontsize = 8,
                                         col = col_water[1]))) -> p_d_water

grobTree( rectGrob(gp=gpar(fill="#ece5d3", col = col_earth[2])),
          grid::textGrob("Earth
Drink (3):
Toph reminds someone she is blind
Someone talks about Joo Dee
Cabbage merchant has bad luck
",
                         just = "left", x = 0.05, y = 0.95, vjust = 1,
                         gp = grid::gpar(fontfamily = "Slayer", fontsize = 8,
                                         col = col_earth[1]))) -> p_d_earth

grobTree( rectGrob(gp=gpar(fill="#ece5d3", col = col_fire[4])),
          grid::textGrob("Fire
Drink (1):
Zuko says 'uncle'
Iroh mentions 'tea'
Azula says 'father'
",
                         just = "left", x = 0.05, y = 0.95, vjust = 1,
                         gp = grid::gpar(fontfamily = "Slayer", fontsize = 8,
                                         col = col_fire[2]))) -> p_d_fire

grobTree( rectGrob(gp=gpar(fill="#ece5d3", col = col_air[5])),
          grid::textGrob("Air
Drink (2):
Aang says the Avatar
Anyone tells Appa 'Yip, yip!'
Enters the Avatar State
",
                         just = "left", x = 0.05, y = 0.95, vjust = 1,
                         gp = grid::gpar(fontfamily = "Slayer", fontsize = 8,
                                         col = col_air[1]))) -> p_d_air

library(patchwork)

# p_cum + p_total + plot_layout(nrow = 1, width = c(3, 1))

layout <- "AAAB
           AAAB
           AAAB
           AAAB
           AAAB
           AAAB
           AAAB
           AAAB
           CCCC
           DEFG
           DEFG"

p_cum + p_total + p_detail +
  p_d_water + p_d_earth + p_d_fire + p_d_air +
  plot_layout(design=layout) #+
  #' plot_annotation(caption = "#TidyTuesday | 2020 Week 33
  #'                 @ Ryan Timpe .com")

ggsave("2020w33/LastAirbenderDrinkingGame.png", width = 12, height =7)
