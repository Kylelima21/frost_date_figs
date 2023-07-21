### McFarland Hill weather station frost date figures
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

prodat <- tibble(read.csv("data/mcfarland_2003_2022.csv")) %>% 
  rename_with(., ~tolower(.)) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         datej = as.Date(date, format = "%Y-%m-%d"),
         julian = as.integer(format(datej, "%j"))) %>% 
  select(date, julian, day, month, year, tmin) %>% 
  filter(year != 2021)




#------------------------------------------------#
####            Frost Date Calcs              ####
#------------------------------------------------#

## Last spring frost
spring <- prodat %>%
  group_by(year) %>% 
  filter(julian < 180 & tmin <= 32) %>% 
  slice(which.max(julian)) %>% 
  select(year, month, day, julian, tmin) %>% 
  mutate(last.frost = as.Date(paste(2000, month, day, sep = "-")))


## First fall frost
fall <- prodat %>%
  group_by(year) %>% 
  filter(julian > 180 & tmin <= 32) %>% 
  slice(which.min(julian)) %>% 
  select(year, month, day, julian, tmin) %>% 
  mutate(first.frost = as.Date(paste(2000, month, day, sep = "-")))




#------------------------------------------------#
####             Create Figures               ####
#------------------------------------------------#

### NOAA
#------------------------------------------------#

### Spring figure
## ggplot recreation
spring %>% 
  ggplot(aes(x = year, y = last.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#578e5b") +
  labs(title = "Acadia National Park last spring frost", 
       subtitle = "20 years of data from the McFarland Hill weather station",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))

ggsave("outputs/last_frosts_2003_2022.png", height = 6, width = 8.5, dpi = 350)

lm(last.frost ~ year, data=spring)



### Fall figure
## ggplot recreation
fall %>% 
  ggplot(aes(x = year, y = first.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#C57E21") +
  labs(title = "Acadia National Park first fall frost", 
       subtitle = "20 years of data from the McFarland Hill weather station",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))


ggsave("outputs/first_frosts_2003_2022.png", height = 6, width = 8.5, dpi = 350)

lm(first.frost ~ year, data=fall)



### Frost-free period
## Manipulate and calculate
ff <- left_join(spring, fall, by = "year") %>% 
  select(year, lf.julian = julian.x, ff.julian = julian.y, last.frost, first.frost) %>% 
  mutate(ff.period = ff.julian - lf.julian)

## Create ggplot
ff %>% 
  ggplot(aes(x = year, y = ff.period)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#CF1D10") +
  labs(title = "Acadia National Park frost-free period", 
       subtitle = "Data from the McFarland Hill weather station 2003 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Number of days") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))


ggsave("outputs/frost_free_period_2003_2022.png", height = 6, width = 8.5, dpi = 350)

lm(ff.period ~ year, data=ff)






### NPS
#------------------------------------------------#

### Read and clean
npsdat <- tibble(read.csv("data/export.csv")) %>%
  mutate(date = str_remove(datetime, "\\s\\d*\\:\\d*"),
         date = as.Date(date, format = "%m/%d/%y"),
         day = day(date),
         month = month(date),
         year = as.integer(year(date)),
         julian = as.integer(format(date, "%j"))) %>% 
  mutate(tempc = str_replace(tempc, "-999", "NA"),
         tempf = celsius.to.fahrenheit(.$tempc, round = 0)) %>% 
  filter(tempf > -1000) %>% 
  group_by(day, month, year, julian) %>% 
  summarise(tmin = min(tempf)) %>% 
  mutate(tmin = as.numeric(tmin)) %>% 
  filter(year < 2023 & year > 2002) %>% 
  ungroup()


## Last spring frost
spring2 <- npsdat %>%
  filter(julian < 180 & tmin <= 32) %>% 
  group_by(year) %>% 
  slice(which.max(julian)) %>% 
  select(year, month, day, julian, tmin) %>% 
  mutate(last.frost = as.Date(paste(2000, month, day, sep = "-")))


## First fall frost
fall2 <- npsdat %>%
  group_by(year) %>% 
  filter(julian > 180 & tmin <= 32) %>% 
  slice(which.min(julian)) %>% 
  select(year, month, day, julian, tmin) %>% 
  mutate(first.frost = as.Date(paste(2000, month, day, sep = "-")))


## Spring figure NPS
spring2 %>% 
  ggplot(aes(x = year, y = last.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#578e5b") +
  labs(title = "Acadia National Park last spring frost", 
       subtitle = "NPS data from McFarland Hill weather station 2003 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))

ggsave("outputs/last_frosts_1999_2022.png", height = 6, width = 8.5, dpi = 350)

lm(last.frost ~ year, data=spring2)


## Fall figure NPS
fall2 %>% 
  ggplot(aes(x = year, y = first.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#C57E21") +
  labs(title = "Acadia National Park first fall frost", 
       subtitle = "NPS data from McFarland Hill weather station 2003 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Month and day of year") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))


ggsave("outputs/first_frosts_1999_2022.png", height = 6, width = 8.5, dpi = 350)

lm(first.frost ~ year, data=fall2)




### Frost-free period
## Manipulate and calculate
ff2 <- left_join(spring2, fall2, by = "year") %>% 
  select(year, lf.julian = julian.x, ff.julian = julian.y, last.frost, first.frost) %>% 
  mutate(ff.period = ff.julian - lf.julian)

## Create ggplot
ff2 %>% 
  ggplot(aes(x = year, y = ff.period)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#CF1D10") +
  labs(title = "Acadia National Park frost-free period", 
       subtitle = "Data from the McFarland Hill weather station 2003 - 2022",
       caption = "Figure created by Schoodic Institute",
       x = "Year",
       y = "Number of days") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))


ggsave("outputs/frost_free_period_1983_2022.png", height = 6, width = 8.5, dpi = 350)

lm(ff.period ~ year, data=ff)



npsmerg <- npsdat %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         tmin = as.integer(tmin)) %>% 
  arrange(date) %>% 
  select(date, julian, day, month, year, tmin) %>% 
  filter(year != 2021)

noaamerg <- prodat %>% 
  mutate(date = as.Date(date),
         year = as.integer(year))

tdif <- left_join(npsmerg, noaamerg, by = c("day", "month", "year", "julian", "date")) %>% 
  arrange(date) %>% 
  select(date, day, month, year, julian, tmin.nps = tmin.x, tmin.noaa = tmin.y) %>% 
  mutate(tdiff = abs(tmin.nps - tmin.noaa))

write.csv(tdif, "outputs/tmin_difference.csv", row.names = F)




