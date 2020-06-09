library(tidyverse)
library(extrafont)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

table(firsts$category)
table(firsts$gender)

# recode
dat <- firsts %>% 
  mutate(gender = ifelse(str_detect(gender, "Female"), "Female", "Male"), # clean-up gender variable
         person_sh = str_remove_all(person, "\\(.*\\)"), # remove everything in parenthesis
         person_sh = str_remove_all(person, "\\(.*"), # remove everything in parenthesis
         person_sh = str_remove_all(person_sh, "\\[.*\\]"), # remove brackets and their content
         person_sh = str_trim(person_sh, "right"), # remove white space at the end of names
         person_complete = str_c(person_sh, ", ", accomplishment), # create complete name + accomplishment title
         word_count = str_count(person_complete), # count number of words in complete names
         count = 1,
         person_complete = str_replace_all(person_complete, "African-American", "Af-Am"),
         twenty_yr = ifelse(year <= 1780, '1760s-70s', 
                            ifelse(year >= 1781 & year <= 1800, "1780s-90s",
                                          ifelse(year >= 1801 & year <= 1820, "1800s-10s",
                                                 ifelse(year >= 1821 & year <= 1840, "1820s-30s",
                                                        ifelse(year >= 1841 & year <= 1860, "1840s-50s",
                                                               ifelse(year >= 1861 & year <= 1880, "1860s-70s",
                                                                      ifelse(year >= 1881 & year <= 1900, "1880s-90s",
                                                                             ifelse(year >= 1901 & year <= 1920, "1900s-10s",
                                                                                    ifelse(year >= 1921 & year <= 1940, "1920s-30s",
                                                                                           ifelse(year >= 1941 & year <= 1960, "1940s-50s",
                                                                                                  ifelse(year >= 1961 & year <= 1980, "1960s-70s",
                                                                                                         ifelse(year >= 1981 & year <= 2000, "1980s-90s", "2000s-10s"))))))))))))) %>% # create categorical variable for twenty years 
  group_by(twenty_yr, category) %>% 
  mutate(persons_20 = paste0(person_complete, collapse = "; ")) %>% # collapse strings for each twenty years and category
  ungroup() %>% 
  group_by(category) %>% 
  mutate(count_cat = cumsum(count)) %>% # count cumulative sum (not used)
  ungroup() %>% 
  distinct(twenty_yr, count_cat, category, persons_20) %>% 
  group_by(twenty_yr, category) %>% 
  arrange(count_cat) %>% 
  filter(row_number() == n()) 

dat$persons_20 <- gsub("(.{15,}?)\\s", "\\1\n", dat$persons_20) # insert new line every 15 characters

# plot, Arts only
# I had a LOT of help from G. Karamanis here: https://github.com/gkaramanis/tidytuesday/blob/master/2020-week23/marbles-race.R
viz <- dat %>% filter(category == "Arts & Entertainment")

p <- ggplot(viz, aes(x = twenty_yr, y = count_cat))
p1 <- p + 
  scale_y_continuous(name = "", 
                     limits = c(-700,700)) +
  scale_x_discrete(name = "",
                   expand = c(0.2, 0)) +
  annotate("text", x = viz$twenty_yr, # add names 
           y = viz$count_cat, 
           label = viz$persons_20, 
           size = 2.5, 
           color = "white",
           family = "Arial Narrow") +
  annotate("text", x = "1760s-70s", # add title 
           y = 400, 
           label = "First\nAfrican Americans\nin Arts & Entertainment", 
           size = 15, 
           hjust = 0,
           color = "white",
           family = "Andale Mono") +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.background = element_rect(fill = 'grey11', colour = 'grey11'),
        plot.margin = margin(200, 40, 100, 30), # play with margins
        axis.text.x = element_text(colour = "gray69", # modify x axis 
                                   size = 15,
                                   family = "Andale Mono"))
p1

ggsave("2020.06.09.png", plot = last_plot(), height = 30, width = 20)

 