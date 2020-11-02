library(tidyverse); library(readxl)

# This script imports the primary data from my local drive

# Read germination data

read.csv("../#data/germination/results/alpineseedsDB.csv") -> germination

# Clean species traits

## Life form and elevation revised by P Poschlod

read_excel("../#data/lifeforms/data/alpineseeds/Traits_to_complete_PP_20200510.xlsx") -> life.form

## Read and prepare seed mass

read.csv("../#data/seedmass/results/seedmass.csv") -> seed.mass

## Embryo trait (Filip)

read.csv("../#data/embryos/results/embryos.csv") -> filip

## Prepare traits object

read.csv("../#data/tpl/results/TPLNames.csv") %>%
  mutate(TPLName = paste(New.Genus, New.Species, sep = " ")) %>%
  select(TPLName, Family) %>%
  merge(germination) %>%
  select(TPLName, Family) %>% # Get names in germination database
  unique %>%
  merge(life.form, all.x = TRUE) %>%
  merge(seed.mass, all.x = TRUE) %>% # Merge seed mass
  merge(read.csv("../#data/baskin/results/dormancy.csv"), all.x = TRUE) %>%
  merge(filip, all.x = TRUE) %>%
  select(TPLName, Family, Alpine, `Plant category`, `Lifespan`, Seed.mass, Embryo, Dormancy) -> traits

# SAVE CLEAN FILES

write.csv(germination, "data/germination.csv", row.names = FALSE)
write.csv(traits, "data/traits.csv", row.names = FALSE)
