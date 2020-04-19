library(tidyverse); library(here)

# Read and prepare seed mass data

read.csv(here("data", "seed mass", "mass.alpine.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, mass) %>%
  group_by(TPLName) %>%
  summarise(Seed.mass = mean(mass)) -> smass

# Prepare traits object

read.csv(here("results", "database", "germination.csv")) %>%
  select(TPLName) %>% # Get names in germination database
  unique %>%
  merge(read.csv(here("data", "taxonomy", "Names.csv"))) %>% # Get Families
  select(TPLName, Familia) %>% 
  unique %>%
  merge(read.csv(here("data", "elevations", "elevations.csv")), all.x = TRUE) %>% # Get Alpine trait
  merge(smass, all.x = TRUE) %>% # Merge seed mass
  merge(read.csv("../baskin/results/dormancy.csv"), all.x = TRUE) -> # Merge dormancy file from common folfer 
  traits

# Save

write.csv(traits, here("results", "database", "traits.csv"), row.names = FALSE)

