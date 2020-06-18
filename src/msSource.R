library(tidyverse); library(here)

read.csv(here("data", "Elevations", "Regions.csv")) %>%
  merge(read.csv(here("results", "database", "germination.csv")), by = "Region", all = TRUE) %>%
  merge(read.csv(here("results", "database", "traits.csv")), by = "TPLName") %>%
  mutate(Region = as.character(Region),
         Region = ifelse(is.na(Macroregion), "Enscobase", Region),
         Region = ifelse(Region %in% c("Northern Alps", "Southern Alps"), "Alps", Region),
         Region = ifelse(Region %in% c("Central Andes", "Southern Andes"), "Andes", Region),
         Region = ifelse(Region %in% c("Qinghai-Tibet Plateau"), "Qinghai-Tibet", Region),
         Region = ifelse(Region %in% c("Cantabrian Mountains"), "Cantabrian Mts", Region),     
         Region = as.factor(Region)) -> dataset

# Numbers for the main text

dataset %>% summary
dataset %>% pull(Region) %>% unique %>% length -> MSregions
dataset %>% tally() -> MSrecords
dataset %>% pull(Source) %>% unique %>% length -> MSgroups
read.csv("../../../#tpl/results/TPLNames.csv") %>% 
  select(Taxon, Family) %>%
  unique %>%
  merge(dataset, by = "Taxon", all.y = TRUE) %>%
  pull(Family) %>% unique %>% length -> MSfamilies
dataset %>% pull(TPLName) %>% unique %>% length -> MSspecies
dataset %>% filter(Alpine == "Strict") %>% pull(TPLName) %>% unique %>% length -> MSstrict
dataset %>% filter(Alpine == "Generalist") %>% pull(TPLName) %>% unique %>% length -> MSgeneral
dataset$Germinable %>% sum -> MSseeds
dataset %>% pull(Tmean) %>% min -> MSminT
dataset %>% pull(Tmean) %>% max -> MSmaxT
dataset %>% filter(Alternating == "Y") %>% tally -> MSaltY
dataset %>% filter(Alternating == "N") %>% tally -> MSaltN
dataset %>% filter(Light == "Y") %>% tally -> MSlightY
dataset %>% filter(Light == "N") %>% tally -> MSlightN
dataset %>% filter(Stratification == "None") %>% tally -> MSstratN
dataset %>% filter(Stratification == "Cold") %>% tally -> MSstratCold
dataset %>% filter(Scarification == "Y") %>% tally -> MSscarY
dataset %>% filter(GA3 == "Y") %>% tally -> MSga3

# Map figure

dataset %>%
  group_by(Region) %>%
  summarise(Longitude = mean(Longitude.x), Latitude = mean(Latitude.x),
            Records =length(TPLName),
            Species = length(unique(TPLName))) -> dfRecords

dataset %>%
  filter(Alpine == "Strict") %>%
  group_by(Region) %>%
  summarise(Strict = length(unique(TPLName))) -> dfStricts

dfRecords %>%
  merge(dfStricts) %>%
  data.frame -> dfNumbers

library(maps); library(mapdata); library(extrafont)

load("../../../#wwfmap/alpinemap/results/alpinemap.RData")

dfNumbers %>%
  ggplot(aes(Longitude, Latitude)) +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group),
               color = "gainsboro", fill = "gainsboro") +
  geom_polygon(data = testolin_simple, aes(x = long, y = lat, group = group),
               color = "purple", fill = "purple") +
  geom_label(aes(label = rownames(dfNumbers)), size = 2.5,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0.25, "lines"),
             alpha = 0.6, fill = "gold") +
  guides(size = F) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-49, 80)) +
  theme(axis.line=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), plot.background=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(margin = margin(r = 11, unit = "pt"))) -> p1

dfNumbers %>%
  select(Region, Records, Species, Strict) %>%
  gridExtra::tableGrob(theme = gridExtra::ttheme_minimal(
    core = list(fg_params = list(cex = .6, hjust = 1, x = 0.9)),
    colhead = list(fg_params = list(cex = .6, hjust = 1, x = 0.9)),
    rowhead = list(fg_params = list(cex = .6)))) -> TableMap

gridExtra::arrangeGrob(p1, TableMap, ncol = 2, widths = 3:2) -> fig1

# ggsave(filename = here("doc", "ms", "Fig1.png"), plot = fig1,
#        path = NULL, scale = 1, width = 150, height = 90, units = "mm", dpi = 600)
# save(p1, file = here("doc", "ms", "fig1.RData"))
