library(dplyr)
library(ggridges)
library(lubridate)
library(hms)
library(patchwork)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

tickets$date <- as.Date(tickets$issue_datetime)
tickets$date <- as.numeric(tickets$date)

tickets <- tickets %>% 
  drop_na() %>% 
  mutate(hour = hour(issue_datetime), 
         minute = minute(issue_datetime),
         time = 60*hour + minute) %>% 
  filter(!fine==15)

# plot according to time 
p_fine_t = ggplot(data = tickets, 
               mapping = aes(x = time, y = as.factor(fine)))
p_fine_t = p_fine_t + geom_density_ridges(alpha = 0.7, fill = "goldenrod2", scale = 3) +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Time of day",
       y = "Fine amount") +
  scale_x_continuous(breaks = c(1,360,720,1080,1439),
                     labels = c("00:01", "6:00", "12:00", "18:00", "23:59"))
p_fine_t

# plot according to day
p_fine_y = ggplot(data = tickets, 
                mapping = aes(x = date, y = as.factor(fine)))
p_fine_y = p_fine_y + geom_density_ridges(alpha = 0.7, fill = "goldenrod2", scale = 3) +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Day of year",
       y = "") +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(breaks = c(17225,17317,17409,17500),
                     labels = c("Mar. 1", "Jun. 1", "Sept 1.", "Dec. 1"))
p_fine_y

# patchwork
patchwork <- p_fine_t + p_fine_y + 
  plot_annotation(title = 'Number of Tickets Issued According to Fine Amount',
                  subtitle = 'By Time of Day and Day of Year',
                  caption = 'Philly Parking Violations Data',
                  theme = theme(plot.title = element_text(size = 18),
                                plot.subtitle = element_text(size = 12))) 
patchwork

ggsave(filename = "dec3_tidy.png", plot = patchwork, height = 8, width = 8)

