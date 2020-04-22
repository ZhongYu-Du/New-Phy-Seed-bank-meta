library(tidyverse); library(here); library(openxlsx)

# Read and prepare seed mass data from A Carta

read.csv(here("data", "seeds", "original", "A Carta", "mass.alpine.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, mass) %>%
  group_by(TPLName) %>%
  summarise(Seed.mass = mean(mass)) -> smass

# Read and prepare seed mass data from Bu

read.xlsx(here("data", "seed mass", "species information and seed mass.xlsx")) %>%
  select(species, `seed.mass.(g,100seeds)-sample1`, 
         `seed.mass.(g,100seeds)-sample2`, `seed.mass.(g,100seeds)-sample3`) %>%
  gather(Trait, Value, -species) %>%
  group_by(species) %>%  summarise(Seed.mass = mean(Value))

# Read and prepare seed mass data from Liu

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

