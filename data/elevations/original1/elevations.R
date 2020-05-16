library(tidyverse); library(Taxonstand); library(here)

## Australia

read.csv(here("data", "elevations", "australian spp.csv")) %>%
  rename(Min.elevation = Min.Elevation,
         Max.elevation = Max.Elevation) %>%
  rename(Taxon = NameDB) %>%
  mutate(Alpine = "Alpine",
         Region = "Australian Alps") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  AustralianSpp

## Spain

read.csv(here("data", "elevations", "spanish spp.csv")) %>%
  separate(Habitat, c("Alpine", "Soil", "Frost"), sep = ",") %>%
  mutate(Region = "Cantabrian Mountains") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  SpanishSpp

## Flora alpina

read.csv(here("data", "elevations", "flora alpina.csv")) %>%
  rename(Taxon = Species) %>%
  mutate(Region = "Southern Alps") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  SAlpsSpp

SAlpsSpp %>%
  mutate(Region = "Northern Alps") ->
  NAlpsSpp

## Russia

read.csv(here("data", "elevations", "russian spp.csv")) %>%
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

read.csv(here("data", "elevations", "chilean spp.csv")) %>%
  rename(Taxon = Species) %>%
  mutate(Alpine = ifelse(Max.elevation > 1500, "Alpine", "Lowland"),
         Region = "Central Andes") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  CAndesSpp

CAndesSpp %>%
  mutate(Region = "Southern Alps") ->
  SAndesSpp

## Flora of China

read.csv(here("data", "elevations", "flora of china.csv")) %>%
  select(-Taxon) %>%
  rename(Taxon = Standardized.name) %>%
  mutate(Region = "Qinghai-Tibet Plateau") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  ChineseSpp

## Enscobase

read.csv(here("data", "elevations", "enscobase spp.csv")) %>%
  filter(! Species %in% "") %>%
  rename(Taxon = Species) %>%
  mutate(Region = "Enscobase") %>%
  select(Taxon, Region, Alpine, Min.elevation, Max.elevation) ->
  EnscobaseSpp

## Join elevations datasets

rbind(AustralianSpp, SpanishSpp, SAlpsSpp, NAlpsSpp, RussianSpp, 
      CAndesSpp, SAndesSpp, ChineseSpp, EnscobaseSpp) %>%
  merge(read.csv(here("data", "taxonomy", "tpl.csv")), by = "Taxon") %>%
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
  elevations

# Save

write.csv(elevations, here("results", "database", "elevations_original.csv"), row.names = FALSE)
