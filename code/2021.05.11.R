library(tidyverse)
library(zipcodeR)
library(maps)
library(viridis)
library(ggthemes)
library(sf) 
library(cartography) 
library(tigris)
library(usmap)
library(mapdata)
library(patchwork)

# broadband data
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

dat <- broadband %>% 
  filter(str_detect(ST,"CA|IL|TX|AZ|PA|NY|OH")) %>% 
  rename(subregion = "COUNTY NAME",
         broad_avail = "BROADBAND AVAILABILITY PER FCC",
         broad_use = "BROADBAND USAGE",
         county_id = "COUNTY ID") %>% 
  mutate(subregion = str_remove_all(subregion, "County"),
        subregion = str_trim(subregion, side = "both"),
        subregion = str_to_lower(subregion),
        broad_avail = as.numeric(broad_avail)*100,
        broad_use = as.numeric(broad_use))

table(dat$ST)

# shapes
counties <- map_data("county")
states_county <- subset(counties, str_detect(region, "california|new york|arizona|illinois|pennsylvania|texas|ohio")) %>% 
  mutate(ST = ifelse(region == "california", "CA",
                     ifelse(region == "new york", "NY",
                            ifelse(region == "arizona", "AZ",
                                   ifelse(region == "illinois", "IL",
                                          ifelse(region == "pennsylvania", "PA",
                                                 ifelse(region == "texas", "TX",
                                                        ifelse(region == "ohio", "OH",NA))))))))

table(states_county$ST, states_county$region)

df <- states_county %>% full_join(dat)

table(df$ST, df$region)

ca_map <- ggplot(data=subset(df, ST=="CA"), 
                 mapping=aes(x=long, 
                             y=lat, 
                             group=group, 
                             fill=broad_avail,  
                             color="white")) + 
  coord_fixed(1.3) + 
  geom_polygon(color="white") + 
  scale_fill_viridis(limits = c(0,100))+
  theme_void()+
  theme(legend.position = "",
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1, 
                             unit = "cm"),
        legend.margin = margin(t = 2, r = 2, b = 5, l = 2, 
                               unit = "mm"))
ny_map <- ggplot(data=subset(df, ST=="NY"), 
                 mapping=aes(x=long, 
                             y=lat, 
                             group=group, 
                             fill=broad_avail,  
                             color="white")) + 
  coord_fixed(1.3) + 
  geom_polygon(color="white") + 
  scale_fill_viridis(name = "", 
                     limits = c(0,100),
                     guide = guide_legend(direction = "vertical",
                                          title.position = "top"))+
  theme_void()+
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, 
                             unit = "cm"),
        legend.margin = margin(t = 2, r = 2, b = 5, l = 2, 
                               unit = "mm"),
        legend.text = element_text(color = "midnightblue")) 
tx_map <- ggplot(data=subset(df, ST=="TX"), 
                 mapping=aes(x=long, 
                             y=lat, 
                             group=group, 
                             fill=broad_avail,  
                             color="white")) + 
  coord_fixed(1.3) + 
  geom_polygon(color="white") + 
  scale_fill_viridis(limits = c(0,100))+
  theme_void()+
  theme(legend.position = "",
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1, 
                             unit = "cm"),
        legend.margin = margin(t = 2, r = 2, b = 5, l = 2, 
                               unit = "mm"))
il_map <- ggplot(data=subset(df, ST=="IL"), 
                 mapping=aes(x=long, 
                             y=lat, 
                             group=group, 
                             fill=broad_avail,  
                             color="white")) + 
  coord_fixed(1.3) + 
  geom_polygon(color="white") + 
  scale_fill_viridis(limits = c(0,100))+
  theme_void()+
  theme(legend.position = "",
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, 
                             unit = "cm"),
        legend.margin = margin(t = 2, r = 2, b = 5, l = 2, 
                               unit = "mm"))

patch <- (il_map+ny_map)/(ca_map+tx_map) +
  plot_annotation(
  title = 'Broadband Access in\nIllinois, New York, California and Texas',
  subtitle = 'Share of people with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps',
  caption = 'Data: Microsoft; Dataviz: @florencevdubois'
) & 
  theme(text = element_text('mono'),
        plot.title = element_text(hjust = .1, vjust = -5, size = 16, color = "midnightblue", face = "bold"), #vjust = -50, 
        plot.subtitle = element_text(hjust = .3, vjust = -7, color = "midnightblue"), #vjust = -75, 
        plot.caption = element_text(hjust = 0.5, color = "midnightblue"))
patch

ggsave(filename = "graphs/plot-2021.05.11.png", plot = last_plot(), width = 10, height = 10)



