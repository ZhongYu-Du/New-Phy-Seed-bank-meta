library(tidyverse); library(here); library(openxlsx)

# This is an atempt to clean the mess of the many elevation files..

# Create a merged file with the spp elevations

read.csv(here("data", "elevations", "elevations_original.csv")) %>% # Original file, with more info for lowland species
  merge(read.xlsx(here("data", "elevations", "Species Cavieres.xlsx")), # Merge with Cavieres, who altered some elevations
        by = c("Species", "Region"), all = TRUE) %>%
  mutate(Min.elevation = ifelse(is.na(Min.elevation.y), Min.elevation.x, Min.elevation.y),
         Max.elevation = ifelse(is.na(Max.elevation.y), Max.elevation.x, Max.elevation.y)) %>%
  select(Species, Region, Alpine.x, Min.elevation, Max.elevation) -> sppelevations

# Read Poschlod file with updated Alpine trait

read.xlsx(here("data", "elevations", "Species_rev_Peter_2020_04_15.xlsx")) %>% 
  select(Species, Alpine) %>%
  unique %>%
  group_by(Species) %>%
  filter(length(Species) >1) -> spp # Species with different classification in different regions

read.xlsx(here("data", "elevations", "Species_rev_Peter_2020_04_15.xlsx")) %>% 
  select(Species, Alpine) %>%
  filter(! (Species %in% spp$Species & 
              Alpine == "Generalist")) %>%
  unique -> # I should do the opposite but I trust more the European classification by Peter...
  alpine

# List lowland spp

sppelevations %>%
  filter(Alpine.x == "Lowland") %>%
  select(Species, Alpine.x) %>%
  rename(Alpine = Alpine.x) %>%
  unique -> lowland1 # Manually classified as lowland since beginning
  
sppelevations %>%
  filter(Alpine.x != "Lowland") %>%
  merge(read.csv(here("data", "elevations", "regions.csv"))) %>%
  filter(Max.elevation <= Treeline) %>%
  mutate(Alpine = "Lowland") %>%
  select(Species, Alpine) %>%
  unique -> lowland2 # Classified as lowland because max elevation in the treeline or below

rbind(lowland1, lowland2) -> lowland # Merged list of lowland spp
  
# Merge lowland and alpine, with preference to alpine classification

lowland %>%
  filter(! Species %in% alpine$Species) %>%
  rbind(alpine) -> sppmerge

# Species still missing, but were in original classification file

read.csv(here("data", "elevations", "elevations_original.csv")) %>% 
  filter(! Species %in% sppmerge$Species) %>%
  filter(! is.na(Min.elevation)) %>%
  merge(read.csv(here("data", "elevations", "regions.csv"))) %>%
  mutate(Alpine = ifelse(Min.elevation > 0.7 * Treeline | Species == "Brachyscome barkerae", 
                         "Strict", "Generalist")) %>%
  unique %>%
  select(Species, Alpine) %>%
  unique -> missingspp

# Second merge

rbind(sppmerge, missingspp) %>%
  filter(! (Species == "Astragalus vesiculosus" & Alpine == "Strict")) %>% # By some error this sp is included also in the alps, but it is from Chile
  unique %>%
  rename(TPLName = Species) -> elevations

# Save

write.csv(elevations, here("data", "elevations", "elevations.csv"), row.names = FALSE)
