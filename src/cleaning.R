library(tidyverse); library(here); library(readxl)

# WITH GERMINATION INDICES

# Format Australian data (susanna Venn)
# Read from original files with germination indices (S Rosbakh)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "VENN hand joined.xlsx"),
           sheet = 1, skip = 0) %>%
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
         Normal,
         GRS:CVG) ->
  AustraliaVenn

# Format Australian data (Annisa Satyanti)
# Read from original files with germination indices (S Rosbakh)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "Aust Alpine Germination timing 11MAY2020.xlsx"),
           sheet = 4, skip = 0) %>%
  select(Species, Accession, Elevation, Treatment, Rep, seeds, GRS:CVG) -> satyanti1
read_excel(here("data",
                "Sergey Rosbakh (Indices)", "Aust Alpine Germination timing 11MAY2020.xlsx"),
           sheet = 5, skip = 0)  %>%
  select(Species, Accession, Elevation, Treatment, Rep, seeds, GRS:CVG) -> satyanti2
read_excel(here("data",
                "Sergey Rosbakh (Indices)", "Aust Alpine Germination timing 11MAY2020.xlsx"),
           sheet = 6, skip = 0)  %>%
  select(Species, Accession, Elevation, Treatment, Rep, seeds, GRS:CVG) -> satyanti3
read_excel(here("data",
                "Sergey Rosbakh (Indices)", "Aust Alpine Germination timing 11MAY2020.xlsx"),
           sheet = 7, skip = 0)  %>%
  select(Species, Accession, Elevation, Treatment, Rep, seeds, GRS:CVG) -> satyanti4

read.csv(here("data", "Manual edit", "australian germination.csv")) %>%
  filter(Source == "Satyanti") %>%
  select(Accession, Scarification, GA3, Germinated, Sown) %>%
  unique %>%
  group_by(Accession, Scarification,  Scarification, Germinated, GA3) -> SownSeeds

rbind(satyanti1, satyanti2, satyanti3, satyanti4) %>%
  rename(Taxon = Species, Dish = Rep, Normal = seeds) %>%
  mutate(Germinated = GRS) %>%
  merge(read_excel(here("data", "Annisa Satyanti", "Treatments.xlsx"))) %>%
  merge(SownSeeds, by = c("Accession", "Scarification",  "Scarification", "Germinated", "GA3")) %>%
  mutate(Region = "Australian Alps",
         Source = "Satyanti",
         Country = "Australia",
         Latitude = NA, Longitude = NA) %>%
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
         Normal,
         GRS:CVG) ->
  AustraliaSat

# Format Chilean data (Verónica Briceño)
# Read from original files with germination indices (S Rosbakh)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "SESData_VBChile.xlsx"),
           sheet = 2, skip = 0)  %>% # Exclude move-along treatments
  merge(read.csv(here("data", "Verónica Briceño", "SpeciesNames.csv")), by.x ="sp abrev", by.y = "sp.abrev") %>%
  rename(Accession = collect.no,
         Dish = `sp abrev`,
         Sown = seeds_start) %>%
  select(Taxon, Accession, Dish, Sown, GRS:CVG) %>%
  mutate(Taxon = as.character(Taxon),
         Taxon = ifelse(Taxon == "Anatrophyllum cumingii", "Anarthrophyllum cumingii", Taxon),
         Taxon = ifelse(Taxon == "Senecio bipontinus", "Senecio bipontinii", Taxon),
         Taxon = ifelse(Taxon == "Drymis andina", "Drimys andina", Taxon), 
         Region = "Southern Andes",
         Source = "Briceño",
         Country = "Chile", 
         Region = as.factor(Region),
         Latitude = NA, 
         Longitude = NA, 
         Elevation = NA, 
         Scarification = "N",
         GA3 = "N",
         Stratification_temperature = NA, 
         Stratification_days = 0, 
         Stratification = "N", 
         Light = "Y",
         Photoperiod = 12, 
         Alternating = "Y",
         Tmean = 16,
         Tmax = 22, 
         Tmin = 10,
         Tdif = 12,
         Germinated = GRS, 
         Normal = NA) %>%
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
         Normal, 
         GRS:CVG) ->
  ChileBriceño

# Format Italian data (Andrea Mondoni)
# Read from original file with germination indices (S Rosbakh)

cbind(
read_excel(here("data", "Sergey Rosbakh (Indices)", "Alpine andrea + interval.xlsx")),
read_excel(here("data", "Sergey Rosbakh (Indices)", "Andrea Mondoni_Alps_scoring.xlsx")) %>%
  select(GRS:CVG)) %>%
  rename(Accession = Code,
         Taxon = `Scientific name`, 
         Stratification_temperature = `Strat °C`,
         Stratification_days =  `Strat d`,
         Scarification = `Scarif (Y/N)`,
         GA3 = `GA(mg/l)`,
         Photoperiod = `DayLigh (h)`) %>%
  mutate(Source = "Mondoni",
         Country = "Italy",
         Latitude = NA,
         Longitude = NA,
         Elevation = Altitude,
         Dish = Rep,
         Sown = sown, 
         Germinated = germ, 
         Normal =  alive,
         Region = ifelse(Accession %in% pull(read.csv(here("data", "Manual edit", "apennines seedlots.csv")), Accession), "Apennines", "Southern Alps"),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N")),
         Stratification = ifelse(Stratification_days == 0 | is.na(Stratification_days), "N", "Y"),
         Light = ifelse(Photoperiod == 0, "N", "Y"),
         GA3 = ifelse(GA3 %in% c("Y", "250"), "Y", "N"),
         Scarification = ifelse(is.na(Scarification), "N", Scarification)) %>%
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
         Normal, 
         GRS:CVG) -> 
  ItalyMondoni

# Format Spanish data (Eduardo Fernández-Pascual)
# Read from manually edited files

read.csv(here("data", "Manual edit", "spanish germination.csv")) %>%
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
  Spain0

# Add indices (S Rosbakh)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "#47_VANESSA2012.xlsx"),
           sheet = 1, skip = 1) %>%
  filter(Accesión %in% unique(Spain0$Accession)) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...29:CVG...48) -> Spain1

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "#51_Ensayos Picos-Pirineos.xlsx"),
           sheet = 1, skip = 1) %>%
  filter(Accesión %in% unique(Spain0$Accession)) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...45:CVG...64) -> Spain2
colnames(Spain2) <- colnames(Spain1)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "#52_Sara Trabajo Grado.xlsx"),
           sheet = 1, skip = 1) %>%
  filter(Accesión %in% unique(Spain0$Accession)) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...45:CVG...64) -> Spain3
colnames(Spain3) <- colnames(Spain1)

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "#56_CANTABROPYRENAICAE2013.xlsx"),
           sheet = 2, skip = 0) %>%
  filter(Accesión %in% unique(Spain0$Accession)) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...23:CVG...42) -> Spain4
colnames(Spain4) <- colnames(Spain1)

rbind(Spain1, Spain2, Spain3, Spain4) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...29:CVG...38) %>%
  filter(Pretratamiento == "F") %>%
  mutate(Pretratamiento = NA) -> Spain5
colnames(Spain5) <- c("Accession", "Stratification_temperature", "Tmean", "Dish",
                  "GRS", "GRP","MGT", "MGR", "GSP", "UNC", "SYN", "VGT", "SDG", "CVG")

rbind(Spain1, Spain2, Spain3, Spain4) %>%
  select(Accesión, Pretratamiento, Temperatura, Placa, GRS...39:CVG...48) -> Spain6
colnames(Spain6) <- c("Accession", "Stratification_temperature", "Tmean", "Dish",
                      "GRS", "GRP","MGT", "MGR", "GSP", "UNC", "SYN", "VGT", "SDG", "CVG")

rbind(Spain5, Spain6) %>% 
  mutate(Stratification_temperature = ifelse(Stratification_temperature == "C", 3, Stratification_temperature),
         Stratification_temperature = ifelse(Stratification_temperature == "F" & Tmean == "14/4", "14/4+25", Stratification_temperature),
         Stratification_temperature = ifelse(Stratification_temperature == "F" & Tmean == "22/12", "22/12+25", Stratification_temperature),
         Stratification_temperature = ifelse(Stratification_temperature == "F" & Tmean == "30/20", "30/20+25", Stratification_temperature),
         Stratification_temperature = as.factor(Stratification_temperature),
         Tmean = ifelse(Tmean == "14/4", "9", Tmean),
         Tmean = ifelse(Tmean == "22/12", "17", Tmean),
         Tmean = ifelse(Tmean == "30/20", "25", Tmean),
         Tmean = as.numeric(Tmean),
         Dish = as.factor(paste("d", Dish, sep = ""))) -> Spain7

Spain0 %>%
  merge(Spain7, by = c("Accession", "Stratification_temperature", "Tmean", "Dish"), all.x = TRUE) %>%
  filter(! Stratification_temperature %in% c("14/4+25", "22/12+25", "30/20+25")) %>% # Remove move-along treatments
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
         Normal,
         GRS:CVG) ->
  Spain

# Format German data (Sergey Rosbakh)
# Read from original files with germination indices

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "Alps_Rosbakh_indices.xlsx")) %>%
  mutate(Region = "Northern Alps",
         Stratification_temperature = ifelse(Stratification_days == 0, NA, Stratification_temperature),
         Stratification = ifelse(Stratification == "0", "N", Stratification),
         Scarification = ifelse(Scarification == "0", "N", Scarification),
         GA3 = "N",
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N")),
         Taxon = ifelse(Taxon == "Myosotis alpina", "Myosotis alpestris", Taxon)) %>%
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
         Normal,
         GRS:CVG) ->
  Germany

# Format Russian data (Sergey Rosbakh)
# Read from original files with germination indices

read_excel(here("data",
                "Sergey Rosbakh (Indices)", "NCaucasus_Rosbakh_indices.xlsx")) %>%
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
         Normal,
         GRS:CVG) ->
  Russia

# Haiyan Bu 2005
# Read from original files with germination indices by S Rosbakh

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet1

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet2

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet3

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet4

rbind(sheet1, sheet2, sheet3, sheet4) %>%
  mutate(Tmax = 20, Tmin = 5, Photoperiod = 0) -> experiment1

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet1

read_excel(path = here("data", "Sergey Rosbakh (Indices)",   
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 2, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet2

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 3, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet3

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 4, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet4

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 5, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, GRS:CVG)) -> sheet5

rbind(sheet1, sheet2, sheet3, sheet4, sheet5) %>%
  mutate(Tmax = 25, Tmin = 10, Photoperiod = 0) -> experiment2

read_excel(path = here("data", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "species information-2005.xlsx"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  select(`serial number`, Species, `alditude (m)`) %>%
  merge(rbind(experiment1, experiment2)) %>%
  filter(! is.na(`serial number`)) %>%
  rename(Taxon = Species,
         Accession = `serial number`,
         Elevation = `alditude (m)`) %>%
  mutate(Germinated = ifelse(is.na(Germinated), 0, Germinated),
         Time = gsub("day ", "", Time),
         Time = gsub("...", "", Time),
         Source = "Bu", #Bu, H., Chen, X., Xu, X., Liu, K., Jia, P., & Du, G. (2007). Seed mass and germination in an alpine meadow on the eastern Tsinghai–Tibet plateau. Plant Ecology, 191(1), 127-149.
         Country = "China",
         Region = "Qinghai-Tibet Plateau",
         Latitude = 35,
         Longitude = 102,
         Scarification = "N",
         GA3 = "N",
         Stratification_temperature = NA, 
         Stratification_days = 0, 
         Stratification = "N", 
         Light = "N",
         Alternating = "Y",
         Tmean = (Tmax + Tmin) /2,
         Tdif = Tmax - Tmin,
         Sown = 50, 
         Normal = NA) %>%
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
         Time,
         Normal, 
         GRS:CVG) -> Bu2005

# Haiyan Bu 2010
# Read from original files with germination indices by S Rosbakh

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "germination data in light-2010.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, 
                              GRS:CVG)) -> sheet1

read_excel(path = here("data", "Sergey Rosbakh (Indices)",  
                       "germination data in light-2010.xls"), 
           sheet = 2, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish, 
                              GRS:CVG)) -> sheet2

read_excel(path = here("data", "Haiyan Bu", 
                       "germination in light -2010", 
                       "species information and seed mass.xlsx"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  select(`serial number`, species, `altitude(m)`) %>%
  merge(rbind(sheet1, sheet2)) %>%
  filter(! is.na(`serial number`)) %>%
  rename(Taxon = species,
         Accession = `serial number`,
         Elevation = `altitude(m)`) %>%
  mutate(Tmax = 20,
         Tmin = 5,
         Photoperiod = 12,
         Germinated = ifelse(is.na(Germinated), 0, Germinated),
         Time = gsub("day ", "", Time),
         Time = gsub("...", "", Time),
         Source = "Bu", #Bu, H. Y., Wang, X. J., Zhou, X. H., Qi, W., Liu, K., Ge, W. J., ... & Zhang, S. T. (2016). The ecological and evolutionary significance of seed shape and volume for the germination of 383 species on the eastern Qinghai-Tibet plateau. Folia geobotanica, 51(4), 333-341.
         Country = "China",
         Region = "Qinghai-Tibet Plateau",
         Latitude = 35,
         Longitude = 102,
         Scarification = "N",
         GA3 = "N",
         Stratification_temperature = NA, 
         Stratification_days = 0, 
         Stratification = "N", 
         Light = "Y",
         Alternating = "Y",
         Tmean = (Tmax + Tmin) /2,
         Tdif = Tmax - Tmin,
         Sown = 50, 
         Normal = NA) %>%
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
         Time,
         Normal, 
         GRS:CVG) -> Bu2010

# Chinese datasets, prepare germination timing, merge
#rbind(Bu2005, Bu2010) %>% write.csv(here("results", "database", "BuScoring.csv"), 
#                                    row.names = FALSE) # Data with scoring times

rbind(Bu2005, Bu2010) %>%
  group_by(Taxon,
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
           Normal, 
           GRS,
           GRP,
           MGT,
           MGR,
           GSP,
           UNC,
           SYN,
           VGT,
           SDG,
           CVG) %>%
  summarise(Germinated = sum(as.numeric(Germinated))) %>%
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
         Normal, 
         GRS:CVG) %>%
  group_by() -> Bu

# WITHOUT GERMINATION INDICES

# ENSCOBASE germination (2nd and 3rd submission, Angelino Carta)
# Read from original files

read.csv(here("data", "A Carta", "AlpineDB_AC.csv")) %>% # Species which are also in other sources
  filter(Source == "" & ! Latitude == "Italy") %>% # Remove data from other sources, remove Enscobase data from Italy (probably repeated from A Mondoni)
  rbind(read.csv(here("data", "A Carta", "AlpineDB_AC_NEWspecies.csv"))) %>% # Species which are NOT in other sources
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
         Normal = Germinable, 
         GRS = NA,
         GRP = NA,
         MGT = NA,
         MGR = NA,
         GSP = NA,
         UNC = NA,
         SYN = NA,
         VGT = NA,
         SDG = NA,
         CVG = NA) %>%
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
         Normal,
         GRS:CVG) ->
  Enscobase

# Liu et al. 2013 (transcribed by Lydia Guja)
# Read from original files

read_excel(path = here("data", "Lydia Guja (Liu et al.)", 
                       "Liu et al 2013 Table S2.xlsx"), 
           sheet = 2, skip = 0, col_types = "text") %>%
  select(`Published name`, `G5/25 (%)`:`G15 (%)`) %>%
  gather(Treatment, Germination, -`Published name`) %>%
  rename(Taxon =  `Published name`) %>%
  mutate(Tmax = recode(Treatment, `G5/25 (%)` = "25",
                       `G10/20 (%)` = "20", `G15 (%)` = "15"),
         Tmin = recode(Treatment, `G5/25 (%)` = "5",
                       `G10/20 (%)` = "10", `G15 (%)` = "15"),
         Tmax = as.numeric(Tmax),
         Tmin = as.numeric(Tmin),
         Tdif = Tmax - Tmin,
         Tmean = (Tmax + Tmin) / 2,
         Alternating = ifelse(Tdif == 0, "N", "Y"),
         Photoperiod = 0,
         Accession = NA,
         Germinated = round((as.numeric(Germination)/100) * 150, 0),
         Elevation = NA,
         Source = "Liu", #Liu, K., Baskin, J. M., Baskin, C. C., Bu, H., Du, G., & Ma, M. (2013). Effect of diurnal fluctuating versus constant temperatures on germination of 445 species from the eastern Tibet Plateau. PloS one, 8(7).
         Country = "China",
         Region = "Qinghai-Tibet Plateau",
         Latitude = 35,
         Longitude = 102,
         Scarification = "N",
         GA3 = "N",
         Dish = NA, 
         Stratification_temperature = NA, 
         Stratification_days = 0, 
         Stratification = "N", 
         Light = "Y", # They state that seeds were exposed to light everyday while scoring
         Sown = 150, 
         Normal = NA, 
         GRS = NA,
         GRP = NA,
         MGT = NA,
         MGR = NA,
         GSP = NA,
         UNC = NA,
         SYN = NA,
         VGT = NA,
         SDG = NA,
         CVG = NA) %>%
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
         Normal,
         GRS:CVG) -> Liu2013

# Format Australian data (Karen Sommerville and missing Satyanti spp)
# Read from manually edited files

read.csv(here("data", "Manual edit", "australian germination.csv")) %>%
  filter(Source == "Sommerville" | ! Taxon %in% AustraliaSat$Taxon) %>%
  mutate(Region = "Australian Alps",
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N")), 
         GRS = NA,
         GRP = NA,
         MGT = NA,
         MGR = NA,
         GSP = NA,
         UNC = NA,
         SYN = NA,
         VGT = NA,
         SDG = NA,
         CVG = NA) %>%
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
         Normal,
         GRS:CVG) ->
  AustraliaSommerville

# Format Italian data (Maria Tudela)
# Read from manually edited files

read.csv(here("data", "Manual edit", "italian germination.csv")) %>%
  filter(Source == "Maria Tudela Ecol & Evol") %>%
  mutate(Region = ifelse(Accession %in% pull(read.csv(here("data", "Manual edit", "apennines seedlots.csv")), Accession), "Apennines", "Southern Alps"),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N")),
         GRS = NA,
         GRP = NA,
         MGT = NA,
         MGR = NA,
         GSP = NA,
         UNC = NA,
         SYN = NA,
         VGT = NA,
         SDG = NA,
         CVG = NA) %>%
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
         Normal,
         GRS:CVG) -> 
  ItalyTudela

# Format Chilean data (Lohengrin Cavieres)
# Read from manually edited files

read.csv(here("data", "Manual edit", "chilean germination.csv")) %>%
  filter(Source == "Cavieres") %>%
  mutate(Taxon = as.character(Taxon),
         Taxon = ifelse(Taxon == "Anatrophyllum cumingii", "Anarthrophyllum cumingii", Taxon),
         Taxon = ifelse(Taxon == "Senecio bipontinus", "Senecio bipontinii", Taxon),
         Taxon = ifelse(Taxon == "Drymis andina", "Drimys andina", Taxon), 
         Region = ifelse(Source == "Brice?o", "Southern Andes", "Central Andes"),
         Region = as.factor(Region),
         Tmin = ifelse(is.na(Tmin), Tmax, Tmin),
         Tmean = (Tmax * Photoperiod + Tmin * (24 - Photoperiod)) / 24, 
         Tdif = Tmax - Tmin, 
         Alternating = as.factor(ifelse(Tdif != 0, "Y", "N")), 
         GRS = NA,
         GRP = NA,
         MGT = NA,
         MGR = NA,
         GSP = NA,
         UNC = NA,
         SYN = NA,
         VGT = NA,
         SDG = NA,
         CVG = NA) %>%
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
         Normal,
         GRS:CVG) ->
  ChileCavieres

# Join germination datasets

rbind(AustraliaVenn, AustraliaSommerville, AustraliaSat, ChileBriceño, ChileCavieres, Bu, Liu2013, Enscobase, Germany, ItalyMondoni, ItalyTudela, Russia, Spain) %>%
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
         Stratification = as.factor(Stratification),
         Sown = as.numeric(Sown)) %>%
  rename(Germinable = Normal) %>%
  filter(! is.na(Germinated)) %>%
  merge(read.csv("../tpl/results/TPLNames.csv"), by = "Taxon") %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  #unique %>% # Remove duplicates (may remove non duplicates with same info)
  merge(read_excel(here("data", "Peter Poschlod", "Traits_to_complete_PP_20200510.xlsx")), 
        all.x = TRUE, by = "TPLName") %>%
  filter(Alpine %in% c("Strict", "Generalist")) %>%
  #group_by(TPLName, Region, Source, Accession,
  #         Scarification, GA3, Stratification, Light, Alternating, Tmean, Temperature) %>%
  #summarise(Germinated = sum(Germinated), Germinable = sum(Germinable), Sown = sum(Sown)) %>%
  mutate(Germinable = ifelse(is.na(Germinable), Sown, Germinable),
         Sown = ifelse(is.na(Sown), Germinable, Sown),
         Germinated = ifelse(Germinated > Germinable, Germinable, Germinated),
         Germinated = as.numeric(Germinated)) %>%
  group_by()  %>%
  select(TPLName,
         Taxon,
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
         Germinable,
         GRS:CVG) %>%
  filter(! is.na(Germinated)) ->
  germination

# SPECIES TRAITS FILE

# Life form and elevation revised by P Poschlod

read_excel(here("data", "Peter Poschlod", "Traits_to_complete_PP_20200510.xlsx")) -> life.form

# Read and prepare seed mass data from A Carta

read.csv(here("data", "A Carta", "mass.alpine.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  mutate(mass = mass/10) %>% # Kew if for 1000 seeds
  select(TPLName, mass) %>%
  group_by(TPLName) %>%
  summarise(Seed.mass = mean(mass)) -> smassCarta

# Read and prepare seed mass data from Bu

read_excel(here("data", "Haiyan Bu", 
                "germination in light -2010", "species information and seed mass.xlsx")) %>%
  select(species, `seed mass (g,100seeds)-sample1`, 
         `seed mass (g,100seeds)-sample2`, `seed mass (g,100seeds)-sample3`) %>%
  gather(Trait, Value, -species) %>%
  group_by(species) %>%
  rename(Taxon = species) %>%
  summarise(Seed.mass = mean(Value, na.rm = TRUE)) -> SMBu2010

read_excel(path = here("data", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "species information-2005.xlsx"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  select(`serial number`, Species) -> Bu2005spp

read_excel(path = here("data", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "seed mass-2005.xls"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  merge(Bu2005spp) %>%
  mutate(`seed weight (g,100 seeds)` = as.numeric(`seed weight (g,100 seeds)`)) %>%
  rename(Taxon = Species) %>%
  group_by(Taxon) %>%
  summarise(Seed.mass = mean(`seed weight (g,100 seeds)`, na.rm = TRUE)) -> SMBu2005

# Liu paper (Lydia)

read_excel(path = here("data", "Lydia Guja (Liu et al.)", 
                       "Liu et al 2013 Table S2.xlsx"), 
           sheet = 2, skip = 0) %>%
  select(`Published name`, `Seed mass (mg)`) %>%
  rename(Taxon = `Published name`, Seed.mass =  `Seed mass (mg)`) %>%
  mutate(Seed.mass = (Seed.mass/1000)*100) -> #This dataset is in mg per seed 
  LiuSM

# Merge seed mass files

rbind(SMBu2005, SMBu2010, LiuSM) %>%
  merge(read.csv("../tpl/results/TPLNames.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, Seed.mass) %>%
  rbind(smassCarta) %>%
  arrange(TPLName) %>%
  group_by(TPLName) %>%
  summarise(Seed.mass = mean(Seed.mass, na.rm = TRUE)) %>%
  mutate(Seed.mass = 10*Seed.mass) -> # Convert to mg (= g per 1000 seeds)
  seed.mass

# Embryo trait (Filip)

read.csv(here("data", "F Vandelook", 
              "alpine plant traits embryo to seed surface ratio.csv"), sep = ";") %>%
  select(TPLName, Species, Genus, Family) %>%
  mutate(Embryo = ifelse(is.na(Species), Genus, Species),
         Embryo = ifelse(is.na(Embryo), Family, Embryo)) %>%
  select(TPLName, Embryo) -> filip

# Prepare traits object

germination %>%
  select(TPLName) %>% # Get names in germination database
  unique %>%
  merge(life.form, all.x = TRUE) %>%
  merge(seed.mass, all.x = TRUE) %>% # Merge seed mass
  merge(read.csv("../baskin/results/dormancy.csv"), all.x = TRUE) %>%
  merge(filip, all.x = TRUE) -> # Merge embryo
  traits

# Save

write.csv(germination, here("results", "database", "germination.csv"), row.names = FALSE)
write.csv(traits, here("results", "database", "traits.csv"), row.names = FALSE)
