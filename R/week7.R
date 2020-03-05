# R studio API
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Libraries
library(tidyverse)
library(ggplot2)
library(psych)

# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = as.POSIXct(timeStart, tz = "UTC"),
         timeEnd   = as.POSIXct(timeEnd, tz = "UTC")) %>%
  mutate(condition = recode_factor(condition, "A" = "Block A", "B" = "Block B", "C" = "Control"),
         gender    = recode_factor(gender, "M" = "Male", "F" = "Female")) %>% 
  filter(q6 == 1) %>%
  select(-q6)


# Visualization
pairs.panels(week7_tbl[,-c(1:4)], density = T, ellipses = F)
ggplot(week7_tbl, aes(x = timeStart, y = q1)) +
  geom_point() +
  labs(title = "Fig 1", x = "Date of Experiment", y = "Q1 Score") +
  theme(plot.title = element_text(face = "bold"))
ggplot(week7_tbl, aes(x = q1, y = q2, col = gender)) +
  geom_jitter() +
  labs(title = "Fig 2") +
  theme(plot.title = element_text(face = "bold"))
ggplot(week7_tbl, aes(x = q1, y = q2, label = gender)) +
  geom_jitter() +
  labs(title = "Fig 3") +
  facet_wrap(~ gender) +
  theme(plot.title = element_text(face = "bold"))
week7_tbl %>%
  mutate(timeElapsed = difftime(timeEnd, timeStart, tz, unit = "secs")) %>%
  ggplot(aes(x = gender, y = timeElapsed)) +
  geom_boxplot() +
  labs(title = "Fig 4", x = "Gender", y = "Time Elapsed (secs)")
ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) +
  geom_jitter() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_colour_discrete(name="Experimental Condition") +
  theme_classic() +
  theme(legend.position   = "bottom",
        legend.background = element_rect(fill = "lightgray")) 
  
