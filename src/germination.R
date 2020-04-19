library(tidyverse); library(here)

# Join germination data

## Format Australian data (Annisa Satyanti, Susanna Venn, Karen Sommerville)

read.csv(here("data", "germination", "australian germination.csv")) %>%
  mutate(Region = "Australian Alps",
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Australia

## Format Spanish data (Eduardo Fernández-Pascual)

read.csv(here("data", "germination", "spanish germination.csv")) %>%
  mutate(Region = "Cantabrian Mountains",
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Spain

## Format Italian data (Andrea Mondoni)

read.csv(here("data", "germination", "italian germination.csv")) %>%
  mutate(Region = ifelse(Accession %in% pull(read.csv(here("data", "germination", "apennines seedlots.csv")), Accession), "Apennines", "Southern Alps"),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) -> 
  Italy

## Format German data (Sergey Rosbakh)

read.csv(here("data", "germination", "german germination.csv")) %>%
  mutate(Region = "Northern Alps",
         Stratification_temperature = ifelse(Stratification_days == 0, NA, Stratification_temperature),
         Stratification = ifelse(Stratification == "0", "N", Stratification),
         Scarification = ifelse(Scarification == "0", "N", Scarification),
         GA3 = "N",
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Germany

## Format Russian data (Sergey Rosbakh)

read.csv(here("data", "germination", "russian germination.csv")) %>%
  mutate(Region = "Caucasus", 
         GA3 = ifelse(GA3 == 0, "N", "Y"),
         Scarification = ifelse(Scarification == 0, "N", "Y"),
         Stratification = ifelse(Stratification == 0, "N", "Y"),
         Stratification_temperature = ifelse(Stratification == "N", NA, 0),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Russia

## Format Chilean data (Lohengrin Cavieres, Verónica Briceño)

read.csv(here("data", "germination", "chilean germination.csv")) %>%
  mutate(Taxon = as.character(Taxon),
         Taxon = ifelse(Taxon == "Anatrophyllum cumingii", "Anarthrophyllum cumingii", Taxon),
         Taxon = ifelse(Taxon == "Senecio bipontinus", "Senecio bipontinii", Taxon),
         Taxon = ifelse(Taxon == "Drymis andina", "Drimys andina", Taxon), 
         Region = ifelse(Source == "Brice?o", "Southern Andes", "Central Andes"),
         Region = as.factor(Region),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Chile

## Format Chinese data (Haiyan Bu, Kun Liu)

read.csv(here("data", "germination", "chinese germination.csv")) %>% 
  mutate(Taxon = as.character(Taxon),
         Taxon = ifelse(Taxon == "Pennisetum centrasiaticum", "Pennisetum flaccidum", Taxon),
         Taxon = ifelse(Taxon == "Saxifraga montana", "Saxifraga sinomontana", Taxon),
         Taxon = ifelse(Taxon == "Roegneria breviglumis", "Elymus burchan-buddae", Taxon),
         Taxon = as.factor(Taxon),
         Accession = NA,
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N"))) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  China

## Format ENSCOBASE germination (2nd and 3rd submission, Angelino Carta)

read.csv(here("data", "germination", "AlpineDB_AC.csv")) %>% # Species which are also in other sources
  filter(Source == "" & ! Latitude == "Italy") %>% # Remove data from other sources, remove Enscobase data from Italy (probably repeated from A Mondoni)
  rbind(read.csv(here("data", "germination", "AlpineDB_AC_NEWspecies.csv"))) %>% # Species which are NOT in other sources
  filter(! GA3 %in% "KNO3") %>%
  mutate(Taxon = gsub("_", " ", Species),
         Region = paste("Enscobase", Latitude, sep = ": "),
         Source = "Enscobase",
         Accession = Population,
         Country = Latitude,
         Elevation = NA,
         Latitude = NA,
         Longitude = NA,
         Stratification_temperature = NA,
         Stratification_days = NA,
         Dish = NA,
         Tmax = NA,
         Tmin = NA,
         Tdif = NA, 
         Photoperiod = NA, 
         Sown = NA, 
         Normal = Germinable) %>%
  select(Taxon,
         Source, 
         Country, 
         Region, 
         Accession, 
         Latitude, 
         Longitude, 
         Elevation, 
         Dish, 
         Scarification,
         GA3,
         Stratification_temperature, 
         Stratification_days, 
         Stratification, 
         Light,
         Photoperiod, 
         Alternating,
         Tmean,
         Tmax, 
         Tmin,
         Tdif,
         Sown, 
         Germinated, 
         Normal) ->
  Enscobase

## Join germination datasets

rbind(Australia, Chile, China, Enscobase, Germany, Italy, Russia, Spain) %>%
  mutate(Tmax = ifelse(Tdif == -10, 20, Tmax),
         Tmin = ifelse(Tdif == -10, 10, Tmin),
         Tdif = abs(Tdif),
         Temperature = cut(Tmean, seq(-2.5, 37.5, 5), 
                           labels = c("0C", "5C", "10C", "15C",
                                      "20C", "25C", "30C", "35C")),
         Year = NA,
         Length.experiment = NA,
         Scarification = fct_recode(Scarification, c("Y" = "2")),
         Stratification = fct_recode(Stratification, c("Y" = "2")),
         Stratification = as.character(Stratification),
         Stratification = ifelse(Stratification_temperature %in% c("25/15+5", "14/4+25", "22/12+25", "30/20+25", "22/12+12/5+0+10/5"), "Warm", Stratification),
         Stratification = factor(Stratification),
         Accession = as.factor(paste(Country, Accession, Latitude, Longitude)),
         Source = recode(Source, 
                         "Maria Tudela Ecol & Evol" = "Tudela",
                         "Sergey Rosbakh" = "Rosbakh",
                         "Venn Aciphylla" = "Venn",
                         "Venn cool fridge" = "Venn", 
                         "Venn warm fridge" = "Venn",
                         "Venn plate" = "Venn",
                         "Fern?ndez-Pascual Plant Biol" = "Pascual",
                         "LiuGuja" = "Liu",
                         "Sergey Rosbakh unpubl" = "Rosbakh"),
         Region = as.factor(Region),
         Stratification = as.character(Stratification),
         Stratification = ifelse(Stratification_temperature == "Warm", "Warm", Stratification),
         Stratification = ifelse(Stratification == "Y", "Cold", Stratification),
         Stratification = ifelse(! Stratification %in% c("Cold", "Warm"), "None", Stratification),
         Stratification = as.factor(Stratification)) %>%
  rename(Germinable = Normal) %>%
  select(Taxon,
         Source,
         Year,
         Country,
         Region,
         Accession,
         Latitude,
         Longitude,
         Elevation,
         Dish,
         Scarification,
         GA3,
         Stratification,
         Stratification_temperature,
         Stratification_days,
         Light,
         Photoperiod,                
         Alternating,                
         Tdif,    
         Tmax,
         Tmin,
         Tmean,            
         Temperature,               
         Length.experiment, 
         Sown,
         Germinated,
         Germinable) %>%
  filter(! is.na(Germinable)) ->
  germination.raw # Object with all data, unclean

# Create clean file

germination.raw %>%
  merge(read.csv(here("data", "taxonomy", "Names.csv")), by.x = "Taxon", by.y = "Original") %>%
  unique %>% # Remove duplicates (may remove non duplicates with same info)
  merge(read.csv(here("data", "elevations", "elevations.csv")), all.x = TRUE) %>%
  filter(Alpine != "Lowland" | is.na(Alpine)) %>%
  group_by(TPLName, Region, Source, Accession,
           Scarification, GA3, Stratification, Light, Alternating, Tmean, Temperature) %>%
  summarise(Germinated = sum(Germinated), Germinable = sum(Germinable), Sown = sum(Sown)) %>%
  mutate(Germinated = ifelse(Germinated > Germinable, Germinable, Germinated),
         Sown = ifelse(is.na(Sown), Germinable, Sown)) %>%
  group_by() ->
  germination

# Save

write.csv(germination.raw, here("results", "database", "germination.raw.csv"), row.names = FALSE)
write.csv(germination, here("results", "database", "germination.csv"), row.names = FALSE)
