
read.csv(here("results", "database", "traits.csv")) %>% 
  pull(TPLName) -> spp

read.csv("../tpl/results/TPLNames.csv") %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, Family, New.Genus, New.Species) %>%
  unique %>%
  filter(TPLName %in% spp) %>%
  write.csv("Species for tree.csv", row.names = FALSE)


read.csv(here("results", "database", "traits.csv")) %>% 
  filter(is.na(Embryo)) %>%
  pull(TPLName) -> spp


read.csv("../tpl/results/TPLNames.csv") %>%
  mutate(TPLName = paste(New.Genus, New.Species)) %>%
  select(TPLName, Family, New.Genus, New.Species) %>%
  unique %>%
  filter(TPLName %in% spp) %>%
  write.csv("Species without embryo.csv", row.names = FALSE)

