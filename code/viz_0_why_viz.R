#' why do we need Viz
#' content:
#' 1. Anscombe's Quartet
#' 2. Alberto Cairo' DataSaurus
#' process: done
#' date: 20220901
#' 


# load library required ---------------------------------------------------

library(tidyverse)
library(rvest)
library(ggthemes)


# website info. -----------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Anscombe%27s_quartet"
webpage <- read_html(url)
tbls <- html_nodes(webpage,"table")


# or you can read the following copy & pasted data from wikipedia ---------

mydata <- readxl::read_xlsx("../data/why_viz.xlsx")

mydata <- mydata %>% 
  select(x...1,y...2,group) %>% 
  rename(x = x...1, y = y...2)


# numeric summary ---------------------------------------------------------

mydata %>% 
  group_by(group) %>% 
  summarize(arithmatic_mean_x = mean(x),
            arithmatic_mean_y = mean(y),
            standard_devi_x = sd(x),
            standard_devi_y = sd(y)) %>% 
  ungroup()

# scatter plot ------------------------------------------------------------

mydata %>% 
  ggplot(aes(x,y)) +
  geom_point() + 
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~ factor(group), ncol = 2)

# show formula in the plot

library(ggpubr)

mydata %>% 
  ggplot(aes(x,y)) +
  geom_point() + 
  geom_smooth(method = "lm",se = FALSE) +
  stat_cor(aes(label = paste(..rr.label..)), # adds R^2 value
           r.accuracy = 0.01,
           label.x = 0, label.y = 11.5, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = 0, label.y = 12, size = 4) +
  facet_wrap(~ factor(group), ncol = 2)

# datasurus by Alberto Cairo ----------------------------------------------

# install.packages("datasauRus")

library(datasauRus)

data("datasaurus_dozen")


# numeric summary ---------------------------------------------------------


datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(arithmatic_mean_x = mean(x),
            arithmatic_mean_y = mean(y),
            standard_devi_x = sd(x),
            standard_devi_y = sd(y)) %>% 
  ungroup()


# viz ---------------------------------------------------------------------

ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point() +
  theme_void() +
  geom_smooth(method = "lm",se = FALSE) +
  stat_cor(aes(label = paste(..rr.label..)), # adds R^2 value
           r.accuracy = 0.01,
           label.x = 0, label.y = 85, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = 0, label.y = 95, size = 4) +
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)


# the end -----------------------------------------------------------------


