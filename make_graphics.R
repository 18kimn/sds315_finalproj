library(tidyverse)
library(tidycensus)
library(tigris)
library(cwi)
library(sf)
library(sysfonts)
library(extrafont)
library(showtext)
library(rio)
library(gganimate)
library(magick)
library(rcartocolor)
#Basic maps of ACS blockgroup-level variables in one go!
#SETUP: Fonts
font_add_google("Heebo", family = "Heebo")
showtext_auto() 
showtext_opts(dpi  = 300)

theme_315 <- function(base_family = "Heebo", base_size = 11, ...) {
  theme_bw(base_family = base_family, base_size = base_size, ...) +
    theme(plot.title.position = "plot",
          plot.caption.position = "panel",
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size*1.2),
          legend.key.width = unit(1.1, "lines"),
          legend.key.height = unit(0.8, "lines"),
          text = element_text(size = base_size), 
          plot.title = element_text(face = "bold", family = base_family, size = 2.2*base_size),
          plot.subtitle = element_text(face = "plain", size = base_size*1.5), 
          plot.caption = element_text(face = "plain"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.border= element_blank()) 
}


col1 <- "#75BEE9"
col2 <- "#C15FA5"

dta <- import("data/a6.xlsx", skip = 1) %>% 
  as_tibble() %>% 
  mutate(year_clean = ifelse(!is.na(as.numeric(year)), year, NA),
         year_clean = zoo::na.locf(as.numeric(year_clean)),
         year = str_remove(year, "\\."),
         across(contains("_tot"), as.numeric)) %>% 
  drop_na(Total, asian_tot) %>% 
  select(-contains("_cit"), measure = year, year = year_clean, -Citizen) %>% 
  rename_at(vars(contains("_tot")), function(x) str_to_title(str_remove(x, "_tot"))) %>% 
  pivot_longer(cols = Total:Hispanic) %>% 
  mutate(name = factor(name, levels= c("Total", "White","Black","Asian","Hispanic")))


dta %>% filter(year == 2016) %>% 
  ggplot() + 
  geom_bar(aes(x = name, y = value, fill = measure), stat = "identity", position = "dodge") + 
  labs(x = NULL, y = "Percent turnout in 2016", fill = NULL, 
       title = "Turnout is starkly unequal across racial lines", 
       subtitle = "National voter turnout among the citizen voting-age population, 2016", 
       caption = "Data from the U.S. Census Bureau.") + 
  scale_fill_manual(values = c("Registered" = col1, "Voted" = col2)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  theme_315() + 
  ggsave("figures/static_bar.png", width = 8, height = 5) 

anim <- dta %>% 
  ggplot() + 
  geom_bar(aes(x = name, y = value, fill = measure), stat = "identity", position = "dodge") + 
  transition_states(year) + 
  labs(x = NULL, y = "Percent turnout in {closest_state}", fill = NULL, 
       title = "Turnout is starkly unequal across racial lines",  
       subtitle = "National voter turnout among the citizen voting-age population, {closest_state}", 
       caption = "Data from the U.S. Census Bureau.") + 
  scale_fill_manual(values = c("Registered" = col1, "Voted" = col2)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  theme_315()

#Takes a ridiculously long time to save..
#The issue with text flashing at different sizes doesn't appear to be a problem with the device used
animate(anim, width = 8, height = 5, units = "in", res = 300, duration = 6, renderer = file_renderer("figures/gif_frames/bar"))
          
filenames <- list.files("figures/gif_frames/bar", pattern = "gganim*", full.names= T)


#combine all of them together -- building the map+plot combination with cowplot, and animating it with imagemagick
gif <- filenames %>% 
  map(function(filename){
    message(filename)
    map <- image_read(filename)
  }) %>% 
  image_join()
image_write_gif(gif, "figures/bar.gif", delay = 1/10)





### the line plot
dta <- read_csv("data/U.S. VEP Turnout 1789-Present - Statistics.csv") 

dta <-  tibble(year = dta$Year, election = "Presidential", turnout = dta$`United States Presidential VEP Turnout Rate`) %>% 
  bind_rows(tibble(year = dta$Year_1, election = "Midterm",
            turnout = dta$`United States VEP Midterm Turnout Rate`))


dta %>% 
  filter(year >= 1880) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = turnout, color = election), size = 1) + 
  labs(y = "Turnout among citizen voting-age population", color = NULL, x = NULL, 
       title = "Voter turnout rates are consistently low", 
       subtitle = "Turnout rates among midterm and presidential elections, 1880-present.",
       caption = "Data from the United States Election Project."
       ) + 
  scale_x_continuous(limits = c(1880,NA)) + 
  scale_y_continuous(limits = c(30, 90)) + 
  scale_color_manual(values = c("Midterm" = col1, "Presidential" = col2)) + 
  theme_315() + 
  ggsave("figures/static_line.png", width = 8 ,height =5)


anim <- dta %>% 
  filter(year >= 1880) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = turnout, color = election), size = 1) + 
  transition_reveal(year) + 
  labs(y = "Turnout among citizen voting-age population", color = NULL, x = NULL, 
       title = "Voter turnout rates are consistently low", 
       subtitle = "Turnout rates among midterm and presidential elections, 1880-{year}",
       caption = "Data from the United States Election Project."
  ) + 
  scale_x_continuous(limits = c(1880,NA)) + 
  scale_y_continuous(limits = c(30, 90)) + 
  scale_color_manual(values = c("Midterm" = col1, "Presidential" = col2)) + 
  theme_315() 

animate(anim, width = 8, height = 5, units = "in", res = 300, duration = 6, renderer = file_renderer("figures/gif_frames/line"))
filenames <- list.files("figures/gif_frames/line", pattern = "gganim*", full.names= T)

gif <- filenames %>% 
  map(function(filename){
    message(filename)
    map <- image_read(filename)
  }) %>% 
  image_join()
image_write_gif(gif, "figures/line.gif", delay = 1/10)

files <- list.files("figures/gif_frames/", recursive = T, full.names=T)
file.remove(files)
