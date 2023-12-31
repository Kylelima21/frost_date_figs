### McFarland Hill weather station frost date figures
### Schoodic Institute at Acadia National Park, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(weathermetrics)



#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Read and clean 1982 - 2014 data
t1 <- tibble(read.csv("data/ME170100_2551daily.csv")) %>% 
  select(Day, Month, Year, julian = JD, tavg = TmeanF, tmax = TmaxF, tmin = TminF) %>% 
  rename_with(., ~tolower(.))

## Read and clean 2014 - 2023 data
t2 <- read.csv("data/USR0000MMCF_daily_2014_2023.csv") %>% 
  mutate(day = day(DATE),
         month = month(DATE),
         year = year(DATE),
         datej = as.Date(DATE, format = "%Y-%m-%d"),
         julian = as.integer(format(datej, "%j"))) %>% 
  rename_with(., ~tolower(.)) %>% 
  select(day, month, year, julian, tavg, tmax, tmin)

## Full data
fd <- bind_rows(t1, t2) %>% 
  filter(!is.na(tmin)) %>% 
  filter(year >= 1999 & year < 2023) %>% 
  filter(year != 2021)




## Read and clean 2000 - 2022
t1.2 <- tibble(read.csv("data/export.csv")) %>%
  mutate(date = str_remove(datetime, "\\s\\d*\\:\\d*"),
         date = as.Date(date, format = "%m/%d/%y"),
         day = day(date),
         month = month(date),
         year = as.integer(year(date)),
         julian = as.integer(format(date, "%j"))) %>% 
  filter(year != 1999) %>% 
  mutate(tempc = str_replace(tempc, "-999", "NA"),
         tempf = celsius.to.fahrenheit(.$tempc, round = 0)) %>% 
  filter(tempf > -1000) %>% 
  group_by(day, month, year, julian) %>% 
  summarise(tmin = min(tempf)) %>% 
  mutate(tmin = as.numeric(tmin)) %>% 
  filter(year < 2023) %>% 
  ungroup()

## Read and clean 1999 data
t2.2 <- tibble(read.csv("data/export.csv")) %>%
  mutate(date = str_remove(datetime, "\\s\\d*\\:\\d*"),
         date = as.Date(date, format = "%m/%d/%y"),
         day = day(date),
         month = month(date),
         year = as.integer(year(date)),
         julian = as.integer(format(date, "%j"))) %>% 
  filter(year == 1999) %>% 
  mutate(tempc2 = str_replace(tempc2, "-999", "NA"),
         tempf = celsius.to.fahrenheit(.$tempc2, round = 0)) %>% 
  filter(tempf > -1000) %>% 
  group_by(day, month, year, julian) %>% 
  summarise(tmin = min(tempf)) %>% 
  mutate(tmin = as.numeric(tmin)) %>% 
  ungroup()

## Full data
fd2 <- bind_rows(t1.2, t2.2) %>% 
  filter(!is.na(tmin)) %>% 
  arrange(year, julian)

write.csv(fd2, "outputs/nps_data_daily.csv", row.names = F)



#------------------------------------------------#
####         Clean for frost dates            ####
#------------------------------------------------#

## Last spring frost
spring <- fd %>%
  group_by(year) %>% 
  filter(julian < 180 & tmin <= 32) %>% 
  slice(which.max(julian)) %>% 
  mutate(last.frost = as.Date(paste(2000, month, day, sep = "-"))) %>% 
  select(year, month, day, last.frost, tmin)


## First fall frost
fall <- fd %>%
  group_by(year) %>% 
  filter(julian > 180 & tmin <= 32) %>% 
  slice(which.min(julian)) %>% 
  mutate(first.frost = as.Date(paste(2000, month, day, sep = "-"))) %>% 
  select(year, month, day, first.frost, tmin)


## Last spring frost
spring2 <- fd2 %>%
  filter(julian < 180 & tmin <= 32) %>% 
  group_by(year) %>% 
  slice(which.max(julian)) %>% 
  select(year, month, day, last.frost = julian, tmin) %>% 
  mutate(last.frost = as.Date(paste(2000, month, day, sep = "-")))


## First fall frost
fall2 <- fd2 %>%
  group_by(year) %>% 
  filter(julian > 180 & tmin <= 32) %>% 
  slice(which.min(julian)) %>% 
  select(year, month, day, first.frost = julian, tmin) %>% 
  mutate(first.frost = as.Date(paste(2000, month, day, sep = "-")))


write.csv(spring, "outputs/spring_noaa.csv", row.names = F)
write.csv(spring2, "outputs/spring_nps.csv", row.names = F)

#------------------------------------------------#
####             Create Figures               ####
#------------------------------------------------#

## Spring figure NOAA
spring %>% 
  ggplot(aes(x = year, y = last.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#578e5b") +
  labs(title = "Acadia National Park last spring frost", 
       subtitle = "Data from the McFarland Hill weather station 1983 - 2022",
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

ggsave("outputs/last_frosts_noaa_1999_2022.png", height = 6, width = 8.5, dpi = 350)

lm(last.frost ~ year, data=spring)


## Fall figure NOAA
fall %>% 
  ggplot(aes(x = year, y = first.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#C57E21") +
  labs(title = "Acadia National Park first fall frost", 
       subtitle = "Data from the McFarland Hill weather station 1983 - 2022",
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


ggsave("outputs/first_frosts_noaa_1999_2022.png", height = 6, width = 8.5, dpi = 350)


lm(first.frost ~ year, data=fall)



## Spring figure NPS
spring2 %>% 
  ggplot(aes(x = year, y = last.frost)) +
  geom_line(linewidth = 0.6, color = "gray20") +
  geom_point(size = 1.1, color = "gray20") + 
  geom_smooth(method = "lm", se = F, color = "#578e5b") +
  labs(title = "Acadia National Park last spring frost", 
       subtitle = "NPS data from McFarland Hill weather station 1999 - 2022",
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
       subtitle = "NPS data from McFarland Hill weather station 1999 - 2022",
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








