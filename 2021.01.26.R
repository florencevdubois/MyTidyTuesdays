library(tidyverse)
library(tidytuesdayR)
library(countrycode)

plastics <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv', encoding = "UTF-8")

# five biggest plastic producers by continent ?
# except unbranded

summary(plastics)
table(plastics$country)
head(plastics$parent_company)

df <- plastics %>% 
  filter(!parent_company=="Grand Total",
         !country  == "EMPTY") %>% 
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent"),
         parent_company = str_to_lower(parent_company),
         parent_company = str_trim(parent_company, side = "both"),
         parent_company = ifelse(str_detect(parent_company, "bic"), "société bic", parent_company),
         parent_company = ifelse(parent_company == "nestle", "nestlé", parent_company),
         parent_company = ifelse(parent_company == "red bull", "redbull", parent_company)) %>% 
  group_by(continent) %>%
  mutate(sum_cont = sum(grand_total, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(continent, parent_company) %>% 
  mutate(sum_cont_comp = sum(grand_total, na.rm = T)) %>% 
  ungroup %>% 
  distinct(continent, parent_company, sum_cont, sum_cont_comp) %>% 
  mutate(perc_comp_cont = sum_cont_comp/sum_cont*100) %>% 
  filter(!str_detect(parent_company, "unbranded|null|inconnu|assorted")) %>% 
  arrange(-perc_comp_cont) %>% 
  group_by(continent) %>% 
  mutate(rank = row_number(),
         parent_company = str_to_title(parent_company)) %>%
  filter(perc_comp_cont>0,
         rank <= 10) %>% 
  mutate(count =1) %>% # count  those that are common to all continents
  group_by(parent_company) %>% 
  mutate(sum_common = sum(count),
         common_all = ifelse(sum_common == 5,1,0)) %>% 
  ungroup() 
         
p <- ggplot(df, aes(x = reorder(parent_company,perc_comp_cont), y = perc_comp_cont, fill = as.factor(common_all))) +
  geom_histogram(stat = "identity") +
  coord_flip() +
  scale_fill_manual(name = "", 
                    values = c("black", "red")) +
  labs(y = "Share of all plastics found on continent",
       x = "",
       title = "Top Ten Sources of Plastics Found on 5 Continents, 2018-2019",
       subtitle = "The Coca-Cola Company and PepsiCo are in every continent's top ten",
       caption = "Data: @brkfreeplastic and @sarah_sauve; Dataviz: @florencevdubois") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "") +
  facet_wrap(~continent, ncol=5)

ggsave("plot-2021.01.26.png", plot = p, width = 15, height = 6)


