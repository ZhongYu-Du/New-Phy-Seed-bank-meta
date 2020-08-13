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

# Table with the datasets

data.frame(
merge(
merge(
dataset %>%
  mutate(Source = fct_recode(Source, `Fernández-Pascual` = "Fernández-Pascual Plant Biol")) %>%
  mutate(Region = fct_recode(Region, `European Alps` = "Alps")) %>%
  mutate(Region = fct_recode(Region, `Europe` = "Enscobase")) %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         Records = n),
dataset %>%
  mutate(Source = fct_recode(Source, `Fernández-Pascual` = "Fernández-Pascual Plant Biol")) %>%
  mutate(Region = fct_recode(Region, `European Alps` = "Alps")) %>%
  mutate(Region = fct_recode(Region, `Europe` = "Enscobase")) %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  unique %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         Species = n)),
dataset %>%
  filter(Alpine == "Strict") %>%
  mutate(Source = fct_recode(Source, `Fernández-Pascual` = "Fernández-Pascual Plant Biol")) %>%
  mutate(Region = fct_recode(Region, `European Alps` = "Alps")) %>%
  mutate(Region = fct_recode(Region, `Europe` = "Enscobase")) %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  unique %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         `Strict` = n)),
  Source = c("Briceño, unpublished", "Bu et al. (2007, 2008)", "Cavieres & Arroyo (2000), Cavieres & Sierra-Almeida (2018)", 
               "enscobase.maich.gr", "Fernández-Pascual et al. (2017a)", "Liu et al. (2013)", "Mondoni, unpublished", 
               "Mondoni et al. (2009), Mondoni et al. (2012)", "Rosbakh, unpublished", "Rosbakh, unpublished", "Satyanti, unpublished", 
               "Sommerville et al. (2013)", "Tudela-Isanta et al. (2018)", "Venn (2007), Venn & Morgan (2009)")) %>%
  select(Source, Coverage, Records, Species, Strict) -> Table1

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
dataset %>% pull(Seed.mass) %>% na.omit %>% min %>% round(2) -> MSminMass
dataset %>% pull(Seed.mass) %>% na.omit %>% max %>% round(0) -> MSmaxMass 
dataset %>% pull(Seed.mass) %>% na.omit %>% median %>% round(2) -> MSmedianMass 
dataset %>% pull(Embryo) %>% na.omit %>% min %>% round(4) -> MSminEmbryo
dataset %>% pull(Embryo) %>% na.omit %>% max %>% round(2) -> MSmaxEmbryo 
dataset %>% pull(Embryo) %>% na.omit %>% median %>% round(2) -> MSmedianEmbryo

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
               color = "grey96", fill = "grey96") +
  geom_polygon(data = testolin_simple, aes(x = long, y = lat, group = group),
               color = "purple", fill = "purple") +
  geom_label(aes(label = rownames(dfNumbers)), size = 2.5,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0.25, "lines"),
             alpha = 0.6, fill = "gold") +
  guides(size = F) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-49, 80)) +
  ggthemes::theme_tufte() +
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
    core = list(fg_params = list(cex = .6, hjust = 1, x = 0.9, fontfamily = "serif")),
    colhead = list(fg_params = list(cex = .6, hjust = 1, x = 0.9, fontfamily = "serif")),
    rowhead = list(fg_params = list(cex = .6), fontfamily = "serif"))) -> TableMap

gridExtra::arrangeGrob(p1, TableMap, ncol = 2, widths = 3:2) -> fig1

# Figure 2: description of seed dormancy, mass, embryo

dataset %>%
  select(TPLName, Alpine, Dormancy) %>%
  na.omit %>%
  unique %>%
  group_by(Alpine, Dormancy) %>%
  tally %>%
  ggplot(aes(Alpine, n, fill = Dormancy)) +
  geom_col(position = "fill") +
  labs(y = "Frequency", title = "(A) Seed dormancy classes") +
  ggthemes::theme_tufte() +
  theme(legend.position = "right", axis.title.x = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) -> fig2a

dataset %>%
  select(TPLName, Alpine, Seed.mass) %>%
  na.omit %>%
  unique %>%
  ggplot(aes(Alpine, log(Seed.mass), fill = Alpine)) +
  geom_boxplot() +
  labs(y = "Seed mass (log)", title = "(B) Seed mass") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values = c("yellowgreen", "gold")) -> fig2b

dataset %>%
  select(TPLName, Alpine, Embryo) %>%
  na.omit %>%
  unique %>%
  ggplot(aes(Alpine, Embryo, fill = Alpine)) +
  geom_boxplot() +
  labs(y = "Embryo:endosperm", title = "(C) Embryo:endosperm") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values = c("yellowgreen", "gold")) -> fig2c

gridExtra::arrangeGrob(fig2a, fig2b, fig2c, ncol = 3, widths = c(8, 7, 7)) -> fig2

# Figure 3 - MCMC

rbind(
  rbind(
    read.csv(here("results", "analysis", "proportion", "alpR.germ.general.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv(here("results", "analysis", "proportion", "alpR.germ.general.embryo.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv(here("results", "analysis", "proportion", "alpR.germ.general.mass.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv(here("results", "analysis", "proportion", "alpR.germ.type.csv")) %>%
    select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    separate(variable, into = c("Moderator", "Treatment"), sep = ":") %>%
    mutate(Moderator = gsub("Alpine", "", Moderator)) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC)) %>%
  mutate(Moderator = recode(Moderator, Strict = "Strict alpine spp.",
                            Generalist = "Generalist spp.",
                            Mass = "Seed mass",
                            Embryo = "Embryo:endosperm",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:endosperm", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA[3]"),
         Treatment = fct_relevel(Treatment, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(A) Effect on final germination proportions") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig1a

rbind(
  rbind(
    read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.embryo.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.mass.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv(here("results", "analysis", "mgt", "alpR.MGT.type.csv")) %>%
    select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    separate(variable, into = c("Moderator", "Treatment"), sep = ":") %>%
    mutate(Moderator = gsub("Alpine", "", Moderator)) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC)) %>%
  filter(Mean < 0.5) %>%
  mutate(Moderator = recode(Moderator, Strict = "Strict alpine spp.",
                            Generalist = "Generalist spp.",
                            Mass = "Seed mass",
                            Embryo = "Embryo:endosperm",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:endosperm", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA[3]"),
         Treatment = fct_relevel(Treatment, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(B) Effect on mean germination time") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig1b

rbind(
  rbind(
    read.csv(here("results", "analysis", "unc", "alpR.UNC.general.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv(here("results", "analysis", "unc", "alpR.UNC.general.embryo.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv(here("results", "analysis", "unc", "alpR.UNC.general.mass.csv")) %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv(here("results", "analysis", "unc", "alpR.UNC.type.csv")) %>%
    select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    separate(variable, into = c("Moderator", "Treatment"), sep = ":") %>%
    mutate(Moderator = gsub("Alpine", "", Moderator)) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC)) %>%
  filter(Mean < 0.5) %>%
  mutate(Moderator = recode(Moderator, Strict = "Strict alpine spp.",
                            Generalist = "Generalist spp.",
                            Mass = "Seed mass",
                            Embryo = "Embryo:endosperm",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:endosperm", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA[3]"),
         Treatment = fct_relevel(Treatment, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(C) Effect on germination uncertainty") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig1c

gridExtra::arrangeGrob(fig1a, fig1b, fig1c, ncol = 1) -> fig3

# Figure 4 randoms effects

read.csv(here("results", "analysis", "proportion", "alpR.germ.general.csv")) %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA[3]"),
         model = fct_relevel(model, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(A) Effect on final germination proportions") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("orange",  "gold", "yellowgreen", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("orange",  "gold", "yellowgreen", "forestgreen")) -> fig1a

read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.csv")) %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA[3]"),
         model = fct_relevel(model, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(B) Effect on mean germination time") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("orange",  "gold", "yellowgreen", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("orange",  "gold", "yellowgreen", "forestgreen")) -> fig1b

read.csv(here("results", "analysis", "unc", "alpR.UNC.general.csv")) %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA[3]"),
         model = fct_relevel(model, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Effect size", title = "(C) Effect on germination uncertainty") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("orange",  "gold", "yellowgreen", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("orange",  "gold", "yellowgreen", "forestgreen")) -> fig1c

gridExtra::arrangeGrob(fig1a, fig1b, fig1c, ncol = 1) -> fig4

# Figure 5 - Phylogenetic signal

rbind(
read.csv(here("results", "analysis", "proportion", "alpR.germ.general.csv")) %>%
  mutate(Trait = "Germination\n proportion") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower),
read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.csv")) %>%
  mutate(Trait = "MGT") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower),
read.csv(here("results", "analysis", "unc", "alpR.UNC.general.csv")) %>%
  mutate(Trait = "UNC") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower)) %>%
  mutate(Trait = fct_relevel(Trait, c("UNC", "MGT",  "Germination\n proportion")),
         model = recode(model, Tmean = "Temperature", GA3 = "GA[3]"),
         model = fct_relevel(model, c("Temperature", "Alternating", "Light", "Scarification", "Stratification", "GA[3]"))) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, labeller = label_parsed, nrow = 1) +
  geom_point(size = 1, alpha = 0.7) +
  labs(x = "Pagel's lambda") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 1) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(color = c("darkorchid", "gold", "red3")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("darkorchid", "gold", "red3")) +
  scale_x_continuous(breaks =  c(0, 0.5, 1)) -> fig5

# Figure 6 - Ordination

dataset %>% 
  filter(Tmean <= 5) %>% group_by(Region) %>% tally

dataset %>%
  mutate(Macroregion = as.character(Macroregion),
         Macroregion = ifelse(is.na(Macroregion), "Europe", Macroregion),
         Germination = Germinated / Germinable,
         Scarification = ifelse(Scarification == "Y", 1, 0),
         Stratification = ifelse(Stratification == "None", 0, 1),
         GA3 = ifelse(GA3 == "Y", 1, 0),
         Light = ifelse(Light == "Y", 1, 0),
         Alternating = ifelse(Alternating == "Y", 1, 0),
         MGT = ifelse(Germination < .5, NA, MGT), # Only >= 50% germination
         MGT = ifelse(MGT > 30, NA, MGT), # Only MGT < 4 weeks
         UNC = ifelse(Germination < .5, NA, UNC),
         UNC = ifelse(MGT > 30, NA, UNC)) %>%
  group_by(TPLName) %>%
  summarise(Temperature = weighted.mean(Tmean, w = Germination),
            Alternating = weighted.mean(Alternating, w = Germination),
            Light = weighted.mean(Light, w = Germination),
            Scarification = weighted.mean(Scarification, w = Germination),
            Stratification = weighted.mean(Stratification, w = Germination),
            GA3 = weighted.mean(GA3, w = Germination),
            MGT = min(MGT, na.rm = TRUE),
            UNC = mean(UNC, na.rm = TRUE)) %>%
  merge(read.csv(here("results", "database", "traits.csv")), by = "TPLName") %>%
  select(TPLName, Alpine, Plant.category, Life.form, Lifespan, Reproduction.frequency, Dormancy, Seed.mass, Embryo, Temperature:GA3, MGT:UNC) %>%
  filter(!is.nan(Temperature)) %>% 
  mutate(Plant.category = fct_recode(Plant.category, Woody = "Herb, Woody", Woody = "Woody, Subshrub?"),
         Life.form = fct_recode(Life.form, T = "T, H", G = "H, G", CH = "H, CH", T = "T, Hydrophyt", CH = "CH, H", CH = "CH, NP", G = "G, H", H = "H, Hydrophyt"),
         Lifespan = fct_recode(Lifespan, annual = "annual, biennial", annual = "annual, perennial", perennial = "biennial", perennial = "biennial, perennial"),
         Reproduction.frequency = fct_recode(Reproduction.frequency, monocarp = "monocarp, ?polycarp", monocarp = "monocarp, polycarp")) %>%
  filter(! is.nan(MGT) & is.finite(MGT)) -> traits

traits %>% pull(TPLName) %>% unique %>% length -> MSfamdspp

traits %>%
  select(Alpine, Lifespan, Plant.category, Dormancy, Seed.mass:GA3, MGT:UNC) %>%
  FactoMineR::FAMD(graph = FALSE) -> pcaAlpine

traits %>%
  cbind(pcaAlpine$ind$coord[, 1:2]) -> pcaInds

pcaAlpine$quanti.var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, `Seed mass` = "Seed.mass", 
                               `Embryo:endosperm` = "Embryo")) -> pcaVars

pcaAlpine$quali.var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, Graminoid = "Grass", Forb = "Herb", 
                               Annual = "annual", Perennial = "perennial")) -> pcaVars2
  
  # mutate(Variable = fct_recode(Terophyte = "T", Geophyte = "G", Hemicryptophyte = "H",
  #                              Chamaephyte = "CH", Nanophanerophyte = "NP")) -> pcaVars2

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "grey", alpha = 0.5, size = 2.5, shape = 15) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_segment(data = filter(pcaVars, Variable %in% c("Temperature", "Alternating", "Light",
                                                      "Scarification", "Stratification", "GA3")), 
                             aes(x = 0, y = 0, xend = 4*Dim.1, yend = 4*Dim.2)) +
  ggrepel::geom_label_repel(data = filter(pcaVars, Variable %in% c("Temperature", "Alternating",
                                                                   "Scarification", "GA3",
                                                                   "Stratification", "Light")), 
             aes(x = 4*Dim.1, y = 4*Dim.2, label = Variable), 
             size = 2.5, label.r = unit(0, "lines")) +
  labs(title = "(A) Germination cues") -> fig4a

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "grey", alpha = 0.5, size = 2.5, shape = 15) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_segment(data = filter(pcaVars, ! Variable %in% c("Temperature", "Alternating", "Light",
                                                      "Scarification", "Stratification", "GA3")), 
               aes(x = 0, y = 0, xend = 4*Dim.1, yend = 4*Dim.2)) +
  geom_label(data = filter(pcaVars, ! Variable %in% c("Temperature", "Alternating", "Light",
                                                    "Scarification", "Stratification", "GA3")), 
             aes(x = 4*Dim.1, y = 4*Dim.2, label = Variable), 
             size = 2.5, label.r = unit(0, "lines")) +
  labs(title = "(B) Seed traits") -> fig4b

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Alpine), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("Generalist", "Strict")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 2.5, fill = c("yellowgreen", "gold")) +
  scale_color_manual(values = c("yellowgreen", "gold")) +
  labs(title = "(D) Species distribution") -> fig4d

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Dormancy), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("MD", "MPD", "ND", "PD", "PY")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 2.5, fill = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) +
  scale_color_manual(values = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) +
  labs(title = "(C) Dormancy class") -> fig4c

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Plant.category), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("Woody", "Graminoid", "Forb")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 2.5, fill = c("chartreuse3", "khaki2", "burlywood4")) +
  scale_color_manual(values = c("chartreuse3", "khaki2", "burlywood4")) +
  labs(title = "(E) Life form") -> fig4e

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Lifespan), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("Annual", "Perennial")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 2.5, fill = c("red3", "turquoise3")) +
  scale_color_manual(values = c("red3", "turquoise3")) +
  labs(title = "(F) Life span") -> fig4f


gridExtra::arrangeGrob(fig4a, fig4b, fig4c, fig4d, fig4e, fig4f, ncol = 2) -> fig6
