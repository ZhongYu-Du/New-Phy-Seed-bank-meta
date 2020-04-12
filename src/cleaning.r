library(tidyverse); library(Taxonstand); library(here)

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
  mutate(Region = ifelse(Accession %in% pull(read.csv(here("data", "species", "apennines seedlots.csv")), Accession), "Apennines", "Southern Alps"),
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

## Format ENSCOBASE germination (Angelino Carta)

read.csv(here("data", "germination", "enscobase germination.csv")) %>%
  mutate(Region = "Enscobase",
         Source = Reference,
         Accession = NA,
         Country = Population,
         Elevation = NA,
         Latitude = NA,
         Longitude = NA,
         Stratification_temperature = ifelse(cold.stratification..y.n. == "Y", "Cold",
                                             ifelse(warm.stratification..y.n. == "Y", 
                                                    "Warm", "None")),
         Stratification_days = NA,
         Stratification = ifelse(Stratification_temperature == "None", 
                                 "N", "Y"),
         Scarification = Scarification..Y.N.,
         GA3 = GA..mg.l...y.n.,
         Light = Ligh..h...y.n.,
         Light = ifelse(Light == 1, "Y", "N"),
         Dish = Replicates,
         Alternating = ifelse(Talternating..y.n. == "Y", "Y", "N"),
         Tmax = NA,
         Tmin = NA,
         Tdif = NA, 
         Photoperiod = NA, 
         Sown = Sown.per.replicate, 
         Germinated = round((Germination..../100) * Sown, 0),
         Normal = Sown.per.replicate) %>%
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
  merge(read.csv(here("data", "species", "tpl.csv")), by = "Taxon") -> germination.raw

germination.raw %>%
  merge(data.frame(Accession = unique(germination.raw$Accession), 
                 Population = sprintf("Population %s",seq(1 : length(unique(germination.raw$Accession))))), 
      by = "Accession") %>%
  select(Species,
         Taxon,
         Family,
         Source,
         Year,
         Country,
         Region,
         Population,
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
  germination.raw

# Seedlot information

germination.raw %>%
  select(Population, Accession, Country, Region) %>%
  unique ->
  seedlots

# Join species data

## Australia

read.csv(here("data", "species", "australian spp.csv")) %>%
  rename(Min.elevation = Min.Elevation,
         Max.elevation = Max.Elevation) %>%
  rename(Taxon = NameDB) %>%
  mutate(Alpine = "Alpine",
         Region = "Australian Alps") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  AustralianSpp

## Spain

read.csv(here("data", "species", "spanish spp.csv")) %>%
  separate(Habitat, c("Alpine", "Soil", "Frost"), sep = ",") %>%
  mutate(Region = "Cantabrian Mountains") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  SpanishSpp

## Flora alpina

read.csv(here("data", "species", "flora alpina.csv")) %>%
  rename(Taxon = Species) %>%
  mutate(Region = "Southern Alps") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  SAlpsSpp

SAlpsSpp %>%
  mutate(Region = "Northern Alps") ->
  NAlpsSpp

## Russia

read.csv(here("data", "species", "russian spp.csv")) %>%
  filter(! Taxon %in% "") %>%
  mutate(Region = "Caucasus",
         Max.elevation = as.character(Max.elevation), Min.elevation = as.character(Min.elevation),
         Max.elevation = ifelse(Max.elevation == "alpine", 2900, Max.elevation),
         Max.elevation = ifelse(Max.elevation == "subalpine", 2400, Max.elevation),
         Min.elevation = ifelse(Min.elevation == "alpine", 2400, Min.elevation),
         Min.elevation = ifelse(Min.elevation == "subalpine", 1700, Min.elevation),
         Min.elevation = ifelse(Min.elevation == "montane", 1000, Min.elevation),
         Min.elevation = ifelse(Min.elevation == "lowlands", 0, Min.elevation),
         Max.elevation = as.numeric(Max.elevation), Min.elevation = as.numeric(Min.elevation)) %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  RussianSpp

## Chile

read.csv(here("data", "species", "chilean spp.csv")) %>%
  rename(Taxon = Species) %>%
  mutate(Alpine = ifelse(Max.elevation > 1500, "Alpine", "Lowland"),
         Region = "Central Andes") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  CAndesSpp

CAndesSpp %>%
  mutate(Region = "Southern Alps") ->
  SAndesSpp

## Flora of China

read.csv(here("data", "species", "flora of china.csv")) %>%
  select(-Taxon) %>%
  rename(Taxon = Standardized.name) %>%
  mutate(Region = "Qinghai-Tibet Plateau") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  ChineseSpp

## Enscobase

read.csv(here("data", "species", "enscobase spp.csv")) %>%
  filter(! Species %in% "") %>%
  rename(Taxon = Species) %>%
  mutate(Region = "Enscobase") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  EnscobaseSpp

## Join species datasets

rbind(AustralianSpp, SpanishSpp, SAlpsSpp, NAlpsSpp, RussianSpp, 
      CAndesSpp, SAndesSpp, ChineseSpp, EnscobaseSpp) %>%
  merge(read.csv(here("data", "species", "tpl.csv")), by = "Taxon") %>%
  mutate(Region = as.factor(Region),
         Alpine = ifelse(Alpine == "Lowland", "Lowland", "Alpine"),
         Alpine = as.factor(Alpine),
         Max.elevation = as.character(Max.elevation), Min.elevation = as.character(Min.elevation),
         Max.elevation = ifelse(Species == "Microgynoecium tibeticum", 5600, Max.elevation),
         Min.elevation = ifelse(Species == "Microgynoecium tibeticum", 4000, Min.elevation),
         Max.elevation = ifelse(Species == "Anaphalis latialata", 5000, Max.elevation),
         Min.elevation = ifelse(Species == "Anaphalis latialata", 3000, Min.elevation),
         Max.elevation = ifelse(Species == "Astragalus przewalskii", 5000, Max.elevation),
         Min.elevation = ifelse(Species == "Astragalus przewalskii", 3000, Min.elevation),
         Max.elevation = as.numeric(Max.elevation), Min.elevation = as.numeric(Min.elevation)) %>%
  select(Species, Region, Alpine, Min.elevation, Max.elevation) %>%
  filter(! (Species %in% "Cerastium fontanum" & Alpine %in% "Alpine")) %>%
  group_by(Species, Region, Alpine) %>%
  summarise(Min.elevation = min(Min.elevation),
            Max.elevation = max(Max.elevation)) %>%
  group_by() -> 
  species

# Regions

read.csv(here("data", "regions", "regions.csv")) ->
  regions

# Join everything in single file

germination.raw %>%
  select(-c(Latitude, Longitude)) %>%
  merge(species, by = c("Species", "Region")) %>% 
  merge(regions, by = "Region") %>% # This step is removing Enscobase, for which we have no regional treeline info
  mutate(Alpine = as.character(Alpine),
         Alpine = ifelse(Min.elevation >= Treeline * .70, "Strict", Alpine),
         Alpine = ifelse(Alpine == "Alpine", "Generalist", Alpine),
         Alpine = as.factor(Alpine)) %>%
  unique %>% # Remove duplicates (may remove non duplicates with same info)
  group_by(Species, Region, Source, Population, Alpine, 
           Scarification, GA3, Stratification, Light, Alternating, Tmean, Temperature) %>%
  summarise(Germinated = sum(Germinated), Germinable = sum(Germinable)) %>%
  mutate(Germinated = ifelse(Germinated > Germinable, Germinable, Germinated)) %>%
  group_by() ->
  germination

# Save

write.csv(germination, here("results", "database", "germination.csv"), row.names = FALSE)
write.csv(species, here("results", "database", "species.csv"), row.names = FALSE)
write.csv(seedlots, here("results", "database", "seedlots.csv"), row.names = FALSE)
write.csv(regions, here("results", "database", "regions.csv"), row.names = FALSE)

save(germination, species, seedlots, regions,
     file = here("results", "database", "alpineseeds.RData"))

