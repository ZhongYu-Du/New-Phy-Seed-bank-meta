library(tidyverse); library(Taxonstand); library(here); library(openxlsx)

# Read names, TPL them

read.csv(here("results", "database", "germination.raw.csv")) %>%
  pull(Taxon) %>%
#  merge(read.csv(here("data", "taxonomy", "tpl.csv"))) %>%
  #  pull(Species) %>%
  
  unique %>%
  TPL() %>%
  write.csv(here("data", "taxonomy", "TPLNames.csv"))

read.csv(here("data", "taxonomy", "TPLNames.csv")) %>%
  filter(! New.Taxonomic.status %in% c("Accepted", "Unresolved")) %>%
  select(Taxon) %>%
  merge(read.csv(here("data", "taxonomy", "tpl.csv"))) %>%
  pull(Species) %>%
  unique %>%
  TPL() %>%
  write.csv(here("data", "taxonomy", "TPLNames_spelling.csv"))


read.csv(here("data", "taxonomy", "TPLNames_spelling.csv")) %>%
  filter(! New.Taxonomic.status %in% c("Accepted", "Unresolved"))

read.csv(here("data", "taxonomy", "TPLNames.csv")) %>%
  filter(New.Taxonomic.status %in% c("Accepted", "Unresolved")) -> TPLfound
read.csv(here("data", "taxonomy", "TPLNames_spelling.csv")) %>%
  filter(New.Taxonomic.status %in% c("Accepted", "Unresolved")) -> TPLspelling
read.csv(here("data", "taxonomy", "TPLNames_spelling.csv")) %>%
  filter(! New.Taxonomic.status %in% c("Accepted", "Unresolved")) -> TPLmissing

rbind(TPLfound, TPLspelling, TPLmissing) -> tpl

read.csv(here("data", "taxonomy", "tpl.csv")) %>%
  mutate(Original = as.character(Taxon),
         Spelling = ifelse(Taxon %in% TPLfound$Taxon, Original, as.character(Species)),
         Familia = Family) %>%
  select(Original, Spelling, Familia) %>%
  merge(tpl, by.x = "Spelling", by.y = "Taxon") %>%
  select(-X) %>%
  mutate(Original.clean = paste(Genus, Species),
         TPLName = ifelse(New.Taxonomic.status %in% c("Accepted", "Unresolved"),
                          paste(New.Genus, New.Species), Original.clean)) %>%
  select(Original, Spelling, Original.clean, TPLName, Familia, everything()) %>%
  write.csv(here("data", "taxonomy", "Names.csv"), row.names = FALSE)