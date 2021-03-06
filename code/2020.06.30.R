library(tidyverse)
library(extrafont)
font_import() # to get extra fonts (fonts on your computer)

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

d <- characters %>% 
  select(issue,
         character,
         on_a_date_with_which_character, 
         kiss_with_which_character, 
         hand_holding_with_which_character, 
         dancing_with_which_character, 
         flying_with_another_character,
         arm_in_arm_with_which_character,
         hugging_with_which_character, 
         physical_contact_other,
         carrying_with_which_character,
         shared_bed_with_which_character,
         shared_room_domestically_with_which_character,
         explicitly_states_i_love_you_to_whom) %>% 
  gather(type, with_whom, on_a_date_with_which_character:explicitly_states_i_love_you_to_whom) %>% 
  drop_na(with_whom)

unique(d$character)

# [1] "Cyclops = Scott Summers"               "Marvel Girl/Phoenix = Jean Grey"       "Storm = Ororo Munroe"                 
# [4] "Colossus = Peter (Piotr) Rasputin"     "Banshee = Sean Cassidy"                "Moira MacTaggert (scientist helper)"  
# [7] "Professor X"                           "Nightcrawler = Kurt Wagner"            "Ariel/Sprite/Shadowcat = Kitty Pryde" 
# [10] "Havok = Alex Summers"                  "Angel = Warren Worthington"            "Wolverine = Logan"                    
# [13] "Forge = Name Unknown"                  "Phoenix(2) = Rachel Summers"           "Rogue = Name Unknown"                 
# [16] "Longshot = (unknown real name)"        "Dazzler = Alison Blaire"               "Gambit = Name Unknown"                
# [19] "Mystique = Name Unknown"               "Psylocke = Elizabeth (Betsy) Braddock" "Binary/Ms Marvel = Carol Danvers"     
# [22] "Jubilee = Jubilation Lee"              "Magneto = Erik Magnus"  

# we will keep only their character's name (i.e. remove what comes after the = sign)

unique(d$with_whom)

# in the "with whom" category, we will remove special characters (* symbols) and 
# split thos that have more than 1 into 2 or more columns (separated by commas)

dd <- d %>%
  mutate(character = str_remove_all(character, "=.*"),
         character = str_trim(character, side = "both"),  # trim white spaces
         with_whom = str_remove_all(with_whom, "\\*"),
         character = str_remove_all(character, "\\([^()]+\\)"))  %>% 
  filter(with_whom != 1) %>% 
  mutate(with_whom_unnest = str_split(with_whom, ",")) %>% # splitting the "with whom" variable to create new values for every character "contact"
  unnest(with_whom_unnest) %>% 
  select(-with_whom) %>% # removing the original "with whom" variable
  distinct(issue, character, type, with_whom_unnest) %>% 
  mutate(times = str_extract(with_whom_unnest, "x.*"),  # sometimes a character has been with another more than once, we will extract this info
         times = str_remove_all(times, "x"), # remove the x before the number of times
         times = ifelse(is.na(times) == TRUE, 1, times),
         times = as.numeric(times),
         with_whom_unnest = str_remove_all(with_whom_unnest, "x.*"), # remove number of times from "with whom" character name
         with_whom_unnest = str_trim(with_whom_unnest, side = "both")) %>% # trim 
  filter(!str_detect(times, "[a-z]"), # some included a character's name (they were mistakes in the dataset, I just removed them)
         !str_detect(with_whom_unnest, "unnamed|Unnamed"), # remove unnamed characters
         with_whom_unnest != "", 
         times != "") %>% 
  mutate(with_whom_unnest = ifelse(str_detect(with_whom_unnest, "Prof"), "Professor X", with_whom_unnest)) %>% # Professor X was spelled in many different ways
  group_by(character, type, with_whom_unnest) %>% # I want the number of each category of contacts by chanracter, regardless of issue number
  mutate(sum_times = sum(times)) %>% 
  ungroup() %>% 
  distinct(character, type, with_whom_unnest, sum_times) %>% 
  mutate(secondary = ifelse(with_whom_unnest %in% character, with_whom_unnest, NA)) %>%  # remove those that are only in the with_whom_unnest variable (i.e. NOT main characters)
  drop_na(secondary) 

unique(dd$character)
unique(dd$with_whom_unnest)

# plot
library(tidygraph)
library(ggraph)

dd_edge <- dd %>% 
  rename(from = character,
         to = with_whom_unnest) %>% 
  select(from, to, type, sum_times)

dd_tidy <- tbl_graph(edges = dd_edge, directed = FALSE)
dd_nodes <- data.frame(name = unique(dd_edge$from)) %>% 
  mutate(side = c("right_top","right_top","right_top","right_top","right_top",
                  "top",
                  "left_top", "left_top", "left_top", "left_top", "left_top",
                  "left", 
                  "left_bottom","left_bottom","left_bottom","left_bottom","left_bottom",
                  "bottom",
                  "right_bottom","right_bottom","right_bottom","right_bottom","right_bottom")) # specifying where the nodes end up (I actually did this after drawing the plot) so that I can nuddge them in the right direction

dd_tidy

p <- ggraph(dd_tidy, layout = 'linear', circular = T) + 
  geom_edge_arc(aes(alpha = sum_times, color = type)) +

  geom_node_text(aes(label = dd_nodes$name),
                 repel = T,
                 vjust = ifelse(dd_nodes$side == "top", -1, 0),
                 hjust = ifelse(dd_nodes$side == "right_top", -1, 
                                       ifelse(dd_nodes$side == "left_top", 2, 
                                                     ifelse(dd_nodes$side == "left_bottom", 2, 
                                                                   ifelse(dd_nodes$side == "right_bottom", -1, 
                                                                          ifelse(dd_nodes$side == "left", 2, 0))))),
                 size = 3,
                 family = "CarroisGothicSC-Regular",
                 color = "wheat3") +
  geom_node_point(color = "wheat3", 
                  size = 1) +
  labs(title = "Relationships and Contacts Between X-Men Characters",
       caption = "Data: @ClaremontRun. Visualization: @florencevdubois") + 
  theme_void() +
  ylim(-1.7, 1.7) +
  coord_flip() +
  theme(legend.position = "none",
        text = element_text(size = 10, 
                            family = "CarroisGothicSC-Regular", 
                            color = "wheat3"), 
        plot.title = element_text(hjust = 0.5, 
                                  size = 18, 
                                  face = "bold"),
        plot.caption = element_text(hjust = 0.5,
                                    size = 10),
        panel.background = element_rect(fill = "grey4",
                                        color = "grey4"),
        plot.background = element_rect(fill = "grey4"))
p

ggsave("plot-2020.06.30.png", plot = last_plot(), height = 14, width = 20)


