library(tidyverse)
library(extrafont)
library(grid)
library(gridSVG)
library(rsvg)
font_import()

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

summary(departures$fyear)
summary(departures$fyear_gone)

dat <- departures %>% 
  filter(fyear_gone < 2021) %>% 
  drop_na(fyear,fyear_gone) %>% 
  mutate(survival = fyear_gone-fyear,
         name_company = str_c(exec_fullname, coname,sep = ", "),
         name_company = str_squish(name_company)) %>% 
  arrange(fyear, name_company) %>% 
  mutate(id = row_number()) %>% 
  filter(survival>=2,
         departure_code==4)

ggplot(aes(xmin = 0, xmax = survival, x = survival, y = reorder(id,-survival)),
       data = dat)+
  geom_pointrange(color = "grey16", size = .6) +
  geom_text(aes(label = name_company), hjust = -.11, size = 3.5, family = "Helvetica", color = "grey16")+
  geom_text(aes(label = fyear, x = -.5), size = 3.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 1, y = 16.4, label = "2 years", size = 2.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 1.5, y = 9.4, label = "3 years", size = 2.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 2, y = 7.4, label = "4 years", size = 2.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 2.5, y = 5.4, label = "5 years", size = 2.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 3, y = 4.4, label = "6 years", size = 2.5, family = "Helvetica", color = "grey16") +
  annotate("text", x = 3.5, y = 1.4, label = "7 years", size = 2.5, family = "Helvetica", color = "grey16") +
  scale_x_continuous(limits = c(-.5, 14)) +
  labs(title = "Only 16 CEOs stayed in post more than 1 year\n after behavioral or policy-related issues unfolded",
       caption = "Data: Gentry et al. 2021; Dataviz: @florencevdubois")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = .2,
                                  vjust = 2, 
                                  family = "Helvetica", 
                                  #face = "bold", 
                                  size = 24,
                                  color = "grey16"),
        plot.margin = margin(1, 1, 1, .25, "cm"),
        plot.caption = element_text(size = 8, family = "Helvetica", color = "grey16"),
        plot.background = element_rect(fill="white"))



##### maybe 
links <- dat$sources
## Force 'grid' grobs from 'ggplot2' plot
grid.force()
## List all grobs in plot
grid.ls()
## Find the grobs representing the text labels 
tickLabels <- grid.grep("GRID.text", grep=TRUE, global=TRUE)
## Check which one is label names ---- number 1
lapply(tickLabels, function(x) grid.get(x)$label)

## Add hyperlinks to the axis tick labels
grid.hyperlink(tickLabels[[1]],
               href=links,
               group=FALSE)
## Export to SVG (and view in a browser)
ggsave(file="graphs/plot.svg", plot=last_plot(), width=10, height=8)




grid.export("graphs/plot.svg")

write_xml(xml, tf2 <- tempfile(fileext = ".svg"))
rsvg_pdf(tf2, "out.pdf")
