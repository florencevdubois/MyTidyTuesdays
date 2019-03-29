## load packages ##
library(ggplot2)
library(tidyverse)
library(rgdal)
library(rmapshaper)
library(sp)
library(leaflet)

# load pet names data
seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

# load seattle zip code shapefiles
# from http://data-seattlecitygis.opendata.arcgis.com/datasets/83fc2e72903343aabff6de8cb445b81c_2
seattle_zip <- readOGR(dsn = "~/your-path",
              layer = "Zip_Codes")

# plot shapefile
seattle_plot <- ms_simplify(input = seattle_zip, 
                         keep = 0.05, # keep only 5% of the original points
                         keep_shapes = TRUE) # make sure to keep all polygons

plot(seattle_plot)

# transform to a data frame
seattle_df <- broom::tidy(x = seattle_plot,
                     region = "ZIPCODE") # identifies lat and long of each zip code area
head(seattle_df)

# merge this new data frame with everything else in the shapefile 
seattle_df <- right_join(seattle_df, seattle_plot@data,
                    by = c("id" = "ZIPCODE")) # by id and zipcode (same variable)

# plot the map 
ggplot(seattle_df, aes(x = long,
                  y = lat,
                  group = group)) +
  geom_polygon() +
  coord_fixed() + 
  theme_void() 

# it is the whole Seattle area -- I want to keep only Seattle
class(seattle_df$ZIP)
seattle_df$ZIP <- as.numeric(levels(seattle_df$ZIP))[seattle_df$ZIP]

zip_list <- c(98101, 98102, 98103, 98104, 98105, 98106, 98107, 98108, 
              98109, 98112, 98115, 98116, 98117, 98118, 98119, 98121, 
              98122, 98125, 98126, 98133, 98134, 98136, 98144, 98146, 
              98154, 98164, 98174, 98177, 98178, 98195, 98199) # seattle zip codes

seattle_df <- seattle_df %>%
  filter(ZIP %in% zip_list)

# sum pet species by zip code, and make dog ratio
seattle_pets <- seattle_pets %>% 
  mutate(dogs = ifelse(species == "Dog", 1, 0), 
         cats = ifelse(species == "Cat", 1, 0)) %>% 
  group_by(zip_code) %>% 
  mutate(sum_dogs = sum(dogs),
         sum_cats = sum(cats),
         ratio_dog = sum_dogs/sum_cats) %>% 
  distinct(zip_code, .keep_all = TRUE)

# merge geo data with this new pet data 
class(seattle_pets$zip_code)
seattle_pets$zip_code <- as.numeric(seattle_pets$zip_code)

seattle_df <- left_join(seattle_df, seattle_pets,
                   c("ZIP" = "zip_code"))
head(data_frame(seattle_df))

# plot Seattle with dog ratio 
ggplot(seattle_df,
       aes(x = long,
           y = lat,
           group = group,
           fill = ratio_dog)) +
  geom_polygon() +
  coord_fixed() +
  theme_void() +
  scale_fill_viridis_c(breaks=c(1,2,3),
                       labels=c("1x", "2x", "3x")) + 
  labs(x = "",
       y = "",
       title = "Proportion of cats and dogs licensed in Seattle, by zip code", 
       subtitle = "Overall more licensed dogs; licensed cats more frequent in the city center") +
  guides(fill = guide_colourbar(ticks = FALSE,
                                title="Yellow means more dogs")) 

ggsave(filename = "pets_seattle.png", plot = last_plot(), width = 7, height = 6.5)
