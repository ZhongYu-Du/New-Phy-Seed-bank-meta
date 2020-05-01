library(tidyverse); library(here); library(openxlsx); library(readxl)


# Read and prepare seed mass data from A Carta

read.csv(here("data", "seeds", "original", "A Carta", "mass.alpine.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  mutate(mass = mass/10) %>% # Kew if for 1000 seeds
  select(TPLName, mass) %>%
  group_by(TPLName) %>%
  summarise(Seed.mass = mean(mass)) -> smassCarta

# Read and prepare seed mass data from Bu

read.xlsx(here("data", "seeds", "original", "Haiyan Bu", 
               "germination in light -2010", "species information and seed mass.xlsx")) %>%
  select(species, `seed.mass.(g,100seeds)-sample1`, 
         `seed.mass.(g,100seeds)-sample2`, `seed.mass.(g,100seeds)-sample3`) %>%
  gather(Trait, Value, -species) %>%
  group_by(species) %>%
  rename(Taxon = species) %>%
  summarise(Seed.mass = mean(Value, na.rm = TRUE)) -> SMBu2010

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "species information-2005.xlsx"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  select(`serial number`, Species) -> Bu2005spp

read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
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

read_excel(path = here("data", "seeds", "original", "Lydia Guja (Liu et al.)", 
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

read.csv(here("data", "seeds", "original", "F Vandelook", 
              "alpine plant traits embryo to seed surface ratio.csv"), sep = ";") %>%
  select(TPLName, Species, Genus, Family) %>%
  mutate(Embryo = ifelse(is.na(Species), Genus, Species),
         Embryo = ifelse(is.na(Embryo), Family, Embryo)) %>%
  select(TPLName, Embryo) -> filip

# Prepare traits object

read.csv(here("results", "database", "germination.csv")) %>%
  select(TPLName) %>% # Get names in germination database
  unique %>%
  merge(read.csv(here("data", "taxonomy", "Names.csv"))) %>% # Get Families
  select(TPLName, Familia) %>% 
  unique %>%
  merge(read.csv(here("data", "elevations", "elevations.csv")), all.x = TRUE) %>% # Get Alpine trait
  merge(seed.mass, all.x = TRUE) %>% # Merge seed mass
  merge(read.csv("../baskin/results/dormancy.csv"), all.x = TRUE) %>%
  merge(filip, all.x = TRUE) -> # Merge dormancy file from common folfer 
  traits

# Save

write.csv(traits, here("results", "database", "traits.csv"), row.names = FALSE)

# Life form China data

rbind(
read_excel(path = here("data", "seeds", "original", "Lydia Guja (Liu et al.)", 
                       "Liu et al 2013 Table S2.xlsx"), 
           sheet = 2, skip = 0, col_types = "text") %>%
  select(`Published name`, `Functional group`) %>%
  rename(Taxon = `Published name`, Form = `Functional group`),
read_excel(path = here("data", "seeds", "original", "Haiyan Bu", 
                       "germination in darkness-2005", 
                       "species information-2005.xlsx"), 
           sheet = 1, skip = 0, col_types = "text") %>%
  select(Species, `Life form`) %>%
  rename(Taxon = Species, Form = `Life form`)) %>%
  unique %>%
  merge(read.csv("../tpl/results/TPLNames.csv")) %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, Form) %>%
  unique %>%
  group_by(TPLName) %>%
  merge(traits, all.y = TRUE) %>%
  select(TPLName, Familia, Form) %>% write.csv(here("results", "database", "Chinese life forms.csv"), row.names = FALSE)
