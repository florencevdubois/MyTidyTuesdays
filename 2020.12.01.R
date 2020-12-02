library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(patchwork)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

table(shelters$occupancy_date)
summary(shelters$occupancy)
summary(shelters$capacity)
table(shelters$sector)
table(shelters$shelter_city)

# mean temperatures per month in Toronto
# data source: https://www.currentresults.com/Weather/Canada/Ontario/Places/toronto-temperatures-by-month-average.php
# month - high - low (celsius)
# January	0	-7
# February	0	-6
# March	5	-2
# April	12	3
# May	20	11
# June	24	15
# July	28	18
# August	26	18
# September	22	14
# October	15	8
# November	8	2
# December	3	-3

temp <- data.frame("month" = c(1:12), 
                   "high" = c(0, 0, 5, 12, 20, 24, 28, 26, 22, 15, 8, 3), 
                   "low" = c(-7, -6, -2, 3, 11, 15, 18, 18, 14, 8, 2, -3))

d <- shelters %>% 
  separate(occupancy_date, c("year", "month", "day"), "-") %>% # recode dates 
  drop_na(occupancy, capacity) %>% 
  filter(shelter_city == "Toronto") %>% 
  filter(capacity != 0) %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day),
         occ_rate = occupancy/capacity*100, # get rate of occupancy
         count = 1) %>% 
  group_by(year, month, sector) %>% 
  mutate(denom = sum(count),
         num = sum(occ_rate),
         occ_rate_av = num/denom) %>% # get average rate of occupancy per month/year/sector
  ungroup() %>% 
  distinct(month, year, sector, occ_rate_av) %>% 
  left_join(temp) %>% # merge with temperature data
  mutate(month = ifelse(month < 10, str_c("0", month), month),
         date_c = str_c(year, ".", month),
         date_c =  as.numeric(date_c),
         date_merged = str_c(year, "-", month),
         date_viz = zoo::as.yearmon(date_merged))  # recreate date variable # for viz 

summary(d$occ_rate_av)

# plot occupancy rate per month, year
p <- ggplot(data = d[d$sector == "Men",])  +
  geom_line(aes(x = date_viz, y = occ_rate_av, group = sector), size = 1, color = "darkblue") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  #geom_hline(yintercept = 93) +
  annotate("segment", xend = 2020, x = 2018.54, y = 93, yend = 93, colour = "darkblue", alpha = .4, linetype = "dashed") +
  annotate("text", x = 2019.5, y = 94.4, label = "Since July 2018,\nthe occupancy rate\nhas not decreased below 93%.", color =  "darkblue", size = 2.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(color = "darkblue", size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Occupancy rate of shelters for men\nand average temperature range\nin Toronto, 2017-2019",
       y = "",
       x  = "")
p

# temperature plot
p2 <- ggplot() +
  geom_rect(data = d, 
            aes(xmin = date_viz - 0.08, 
                xmax = date_viz + 0.08, 
                ymin = low, ymax = high,
                fill = high)) +
  scale_fill_viridis_c(option = "cividis", alpha = .8) +
  scale_x_yearmon(breaks = NULL) +
  scale_y_continuous(limits = c(-40,55),
                     breaks = NULL) +
  annotate("text", x = 2018, y = -22, label = "Average temperatures\nare between -7 and 0 degrees\nin January.", color =  "darkblue", size = 2.6) +
  annotate("text", x = 2018.45, y = 45, label = "They reach between\n18 and 28 degrees\nin July.", color =  "darkblue", size = 2.6) +
  
  theme_minimal() +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "",
       y = "",
       x  = "")
p2

patch <- p / p2 + 
  plot_layout(heights = unit(c(4, 4), c('cm', 'cm')), widths = unit(c(15), c('cm'))) +
  plot_annotation(caption = 'Data: Open Toronto; Sharla Gelfand. Data viz: @florencevdubois',
                  theme = theme(plot.caption = element_text(color = "darkblue")))
patch

