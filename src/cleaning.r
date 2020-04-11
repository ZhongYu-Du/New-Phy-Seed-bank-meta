library(tidyverse); library(Taxonstand); library(here)

# Cleaning Australian data (Annisa Satyanti, Susanna Venn, Karen Sommerville)

read.csv(here("data", "species", "australian spp.csv")) %>%
  pull(NameAnisa) %>%
  TPL %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " "),
         NameAnisa = Taxon) %>%
  select(Species, NameAnisa) %>%
  merge(read.csv(here("data", "species", "australian spp.csv")) , by = "NameAnisa") %>% 
  select(-NameAnisa) %>% 
  unique %>%
  rename(Taxon = NameDB) %>%
  merge(read.csv(here("data", "germination", "australian germination.csv")) , by = "Taxon") %>%
  mutate(Region = "Australian Alps", Latitude = -36.455887, 
         Longitude = 148.263612, Habitat = NA, 
         Alpine = "Alpine",
         Min.elevation = Min.Elevation, 
         Max.elevation = Max.Elevation) %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation) ->
  Australia

# Cleaning Spanish data (Eduardo Fernández-Pascual)

read.csv(here("data", "species", "spanish spp.csv")) %>%
  pull(Taxon) %>%
  TPL %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " ")) %>%
  select(Species, Taxon) %>%
  merge(read.csv(here("data", "species", "spanish spp.csv")), by = "Taxon") %>% 
  unique %>%
  merge(read.csv(here("data", "germination", "spanish germination.csv")), by = "Taxon") %>%
  mutate(Region = "Cantabrian Mountains", Latitude = 43.019775,
         Longitude = -5.955487, 
         Alpine = "Alpine") %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation) ->
  Spain

# Cleaning Italian data (Andrea Mondoni)

read.csv(here("data", "germination", "italian germination.csv")) %>%
  pull(Taxon) %>% 
  unique %>%
  TPL %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " ")) %>% 
  select(Species, Taxon) %>%
  merge(read.csv(here("data", "germination", "italian germination.csv")), by = "Taxon") %>%
  merge(read.csv(here("data", "species", "flora alpina.csv"))  %>%
          select(Species, Min.elevation, Max.elevation, Alpine) %>%
          unique, by = "Species") %>%
  mutate(Region = ifelse(Accession %in% pull(read.csv(here("data", "species", "apennines seedlots.csv")), Accession), "Apennines", "Alps"),
         Latitude = ifelse(Accession %in% pull(read.csv(here("data", "species", "apennines seedlots.csv")), Accession), 42.469040, 45.832690),
         Longitude = ifelse(Accession %in% pull(read.csv(here("data", "species", "apennines seedlots.csv")), Accession), 13.565342, 6.866233),
         Habitat = NA) %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation) -> 
  Italy

# Cleaning German data (Sergey Rosbakh)

read.csv(here("data", "germination", "german germination.csv"))  %>%
  pull(Taxon) %>% 
  unique %>%
  TPL %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " ")) %>% 
  select(Species, Taxon) %>%
  merge(read.csv(here("data", "germination", "german germination.csv")), by = "Taxon") %>%
  merge(read.csv(here("data", "species", "flora alpina.csv")) %>%
          select(Species, Min.elevation, Max.elevation, Alpine) %>%
          unique, by = "Species") %>%
  mutate(Region = "Northern Alps",
         Habitat = NA,
         Stratification_temperature = ifelse(Stratification_days == 0, NA, Stratification_temperature),
         Stratification = ifelse(Stratification == "0", "N", Stratification),
         Scarification = ifelse(Scarification == "0", "N", Scarification),
         GA3 = "N") %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation) ->
  Germany

# Cleaning Russian data (Sergey Rosbakh)

read.csv(here("data", "species", "russian spp.csv")) %>%
  merge(read.csv(here("data", "germination", "russian germination.csv")), 
        by = "Taxon", all = TRUE) %>% 
  pull(Taxon) %>% 
  unique %>% 
  TPL %>%
  filter(New.Taxonomic.status %in% c("Accepted", "Unresolved")) %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " ")) %>%
  select(Taxon, Species) %>%
  merge(read.csv(here("data", "species", "russian spp.csv")) %>%
          merge(read.csv(here("data", "germination", "russian germination.csv")), 
                by = "Taxon", all = TRUE), by = "Taxon") %>%
  mutate(Region = "Caucasus",
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod))/24,
         Tdif = Tmax - Tmin, Alternating = ifelse(Tmax == Tmin, "N", "Y"),
         Temperature = cut(Tmean, seq(-2.5, 42.5, 5), 
                           labels = c("0C", "5C", "10C", "15C",
                                      "20C", "25C", "30C", "35C", "40")),
         GA3 = ifelse(GA3 == 0, "N", "Y"),
         Scarification = ifelse(Scarification == 0, "N", "Y"),
         Stratification = ifelse(Stratification == 0, "N", "Y"),
         Stratification_temperature = ifelse(Stratification == "N", NA, 0)) %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation,
         Tmean, Tdif, Alternating, Temperature) ->
  Russia

# Cleaning Chilean data (Lohengrin Cavieres, Verónica Briceño)

read.csv(here("data", "germination", "chilean germination.csv")) %>%
  mutate(Taxon = as.character(Taxon),
         Taxon = ifelse(Taxon == "Anatrophyllum cumingii", "Anarthrophyllum cumingii", Taxon),
         Taxon = ifelse(Taxon == "Senecio bipontinus", "Senecio bipontinii", Taxon),
         Taxon = ifelse(Taxon == "Drymis andina", "Drimys andina", Taxon)) %>%
  pull(Taxon) %>% 
  unique %>%
  TPL %>%
  mutate(Species = paste(New.Genus, New.Species, sep = " ")) %>% 
  select(Species, Taxon) %>%
  merge(read.csv(here("data", "germination", "chilean germination.csv")) %>%
          mutate(Taxon = as.character(Taxon),
                 Taxon = ifelse(Taxon == "Anatrophyllum cumingii", "Anarthrophyllum cumingii", Taxon),
                 Taxon = ifelse(Taxon == "Senecio bipontinus", "Senecio bipontinii", Taxon),
                 Taxon = ifelse(Taxon == "Drymis andina", "Drimys andina", Taxon)), by = "Taxon") %>%
  merge(read.csv(here("data", "species", "chilean spp.csv")) , by = "Species") %>%
  mutate(Region = "Andes",
         Latitude = -38.652178,
         Longitude = -70.899528,
         Habitat = NA,
         Alpine = ifelse(Max.elevation > 1500, "Alpine", "Lowland")) %>%
  select(Species,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Scarification, 
         GA3,
         Light, 
         Dish, 
         Tmax, 
         Tmin, 
         Photoperiod, 
         Sown, 
         Germinated, 
         Normal, 
         Habitat, 
         Alpine, 
         Min.elevation, 
         Max.elevation) ->
  Chile

