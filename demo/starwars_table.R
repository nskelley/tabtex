library(tabtex)
library(dplyr)
library(tidyr)

here::i_am("demo/starwars_table.R")

starwars %>%
  filter(!is.na(homeworld) & !is.na(species)) %>%
  group_by(homeworld, species) %>%
  summarise(spec_hw_freq = n()) %>%
  group_by(homeworld) %>%
  mutate(hw_freq = sum(spec_hw_freq)) %>%
  filter(hw_freq >= 3) %>%
  select(-hw_freq) %>%
  pivot_wider(names_from = homeworld, values_from = spec_hw_freq) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x))) %>%
  rename(Species = species) %>%
  tabtex(title = "Species Frequencies by Homeworld", 
         note = "Restricted to homeworlds with at least three characters.",
         out = here::here("demo/out/starwars_homeworlds.tex"))
