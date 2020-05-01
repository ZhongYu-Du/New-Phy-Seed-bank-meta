library(tidyverse); library(here); library(readxl)

# Format Russian data (Sergey Rosbakh)
# Read from original files

read.csv(here("data", "seeds", "original",
                "Sergey Rosbakh (Caucasus)", "NCaucasus_Rosbakh.csv")) %>%
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

# Format German data (Sergey Rosbakh)
# Read from original files

read_excel(here("data", "seeds", "original",
                "Sergey Rosbakh (Alps)", "Alps_Rosbakh.xlsx")) %>%
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
         Normal) ->
  Germany

# ENSCOBASE germination (2nd and 3rd submission, Angelino Carta)
# Read from original files

read.csv(here("data", "seeds", "original", "A Carta", "AlpineDB_AC.csv")) %>% # Species which are also in other sources
  filter(Source == "" & ! Latitude == "Italy") %>% # Remove data from other sources, remove Enscobase data from Italy (probably repeated from A Mondoni)
  rbind(read.csv(here("data", "seeds", "original", "A Carta", "AlpineDB_AC_NEWspecies.csv"))) %>% # Species which are NOT in other sources
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

# Haiyan Bu 2005
# Read from original files

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet1

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 2, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet2

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 3, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet3

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (5-20) in darkness germiantion data-2005.xls"), 
           sheet = 4, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet4

rbind(sheet1, sheet2, sheet3, sheet4) %>%
  mutate(Tmax = 20, Tmin = 5, Photoperiod = 0) -> experiment1

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet1

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 2, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet2

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 3, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet3

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 4, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet4

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "alternating temperature (10-25) in darkness germiantion data-2005.xls"), 
           sheet = 5, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet5

rbind(sheet1, sheet2, sheet3, sheet4, sheet5) %>%
  mutate(Tmax = 25, Tmin = 10, Photoperiod = 0) -> experiment2

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
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
         Normal) -> Bu2005

# Haiyan Bu 2010
# Read from original files

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in light -2010", 
                       "germination data in light-2010.xls"), 
           sheet = 1, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet1

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in light -2010", 
                       "germination data in light-2010.xls"), 
           sheet = 2, skip = 1, col_types = "text") %>%
  mutate(`serial number` = round(as.numeric(`serial number`), 2)) %>%
  group_by(`serial number`) %>%
  mutate(Dish = paste(`serial number`, row_number())) %>%
  gather(Time, Germinated, -c(`serial number`, Dish)) -> sheet2

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
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
         Normal) -> Bu2010

# Liu et al. 2013 (transcribed by Lydia Guja)
# Read from original files

read_excel(path = here("data", "seeds", "original", "Lydia Guja (Liu et al.)", 
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
         Normal) -> Liu2013

# Chinese datasets, prepare germination timing, merge

rbind(Bu2005, Bu2010) %>% write.csv(here("results", "database", "BuScoring.csv"), 
                                    row.names = FALSE) # Data with scoring times

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
           Normal) %>%
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
         Normal) %>%
  group_by() %>%
  rbind(Liu2013) -> China

# Format Australian data (Annisa Satyanti, Susanna Venn, Karen Sommerville)
# Read from manually edited files

read.csv(here("data", "seeds", "manual edit", "australian germination.csv")) %>%
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

# Format Spanish data (Eduardo Fernández-Pascual)
# Read from manually edited files

read.csv(here("data", "seeds", "manual edit", "spanish germination.csv")) %>%
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

# Format Italian data (Andrea Mondoni)
# Read from manually edited files

read.csv(here("data", "seeds", "manual edit", "italian germination.csv")) %>%
  mutate(Region = ifelse(Accession %in% pull(read.csv(here("data", "seeds", "manual edit", "apennines seedlots.csv")), Accession), "Apennines", "Southern Alps"),
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

# Format Chilean data (Lohengrin Cavieres, Verónica Briceño)
# Read from manually edited files

read.csv(here("data", "seeds", "manual edit", "chilean germination.csv")) %>%
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

# Join germination datasets

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
         Stratification = as.factor(Stratification),
         Sown = as.numeric(Sown)) %>%
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
  filter(! is.na(Germinated)) ->
  germination.raw # Object with all data, unclean

# Create clean file

germination.raw %>%
  merge(read.csv(here("data", "taxonomy", "Names.csv")), by.x = "Taxon", by.y = "Original") %>%
  unique %>% # Remove duplicates (may remove non duplicates with same info)
  merge(read.csv(here("data", "elevations", "elevations.csv")), all.x = TRUE) %>%
  filter(! Alpine %in% c("Lowland", "Unknown")) %>%
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
