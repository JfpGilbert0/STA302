# class activities

#activity 1
library(tidyverse)
library(ggplot2)
tibble(year = 1875:1972,
       level = as.numeric(datasets::LakeHuron)) |>
  ggplot(aes(x = year, y = level)) +
  geom_point()