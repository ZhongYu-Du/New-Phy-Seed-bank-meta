library(tidyverse)

# This script is sourced by the Rmarkdown file that creates the manuscript. It contains
# the code to create the figures, tables and numerical output of the manuscript.

# Prepare dataset object to create tables, figures, etc for the manuscript

read.csv("../data/germination.csv") %>%
  merge(read.csv("../data/traits.csv"), by = "TPLName") -> dataset

# Table 1

data.frame(
merge(
merge(
dataset %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         Records = n),
dataset %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  unique %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         Species = n)),
dataset %>%
  filter(Alpine == "Strict") %>%
  select(Source, Region, Country, TPLName, Alpine) %>%
  unique %>%
  group_by(Source, Region) %>%
  tally %>%
  rename(Dataset = Source, 
         `Coverage` = Region,
         `Strict` = n)),
  Source = c("Briceño, unpublished", "Bu et al. (2007, 2008)", "Cavieres & Arroyo (2000), Cavieres & Sierra-Almeida (2018)", 
               "enscobase.maich.gr", "Fernández-Pascual et al. (2017a)", "Liu et al. (2013)", "Mondoni, unpublished", 
               "Mondoni et al. (2009), Mondoni et al. (2012)", "Rosbakh, unpublished", "Rosbakh & Poschlod (2015)", "Satyanti, unpublished", 
               "Sommerville et al. (2013)", "Tudela-Isanta et al. (2018)", "Venn (2007), Venn & Morgan (2009)")) %>%
  select(Source, Coverage, Records, Species, Strict) -> Table1

# Numbers for the main text

read.csv("../data/traits.csv") %>% filter(Embryo.rank == "Species") %>% pull(TPLName) %>% unique %>% length -> MSembrankspp
read.csv("../data/traits.csv") %>% filter(Embryo.rank == "Genus") %>% pull(TPLName) %>% unique %>% length -> MSembrankgen
read.csv("../data/traits.csv") %>% filter(Embryo.rank == "Family") %>% pull(TPLName) %>% unique %>% length -> MSembrankfam

read.csv("../data/traits.csv") %>% filter(Seed.mass.rank == "Species") %>% pull(TPLName) %>% unique %>% length -> MSmassrankspp
read.csv("../data/traits.csv") %>% filter(Seed.mass.rank == "Genus") %>% pull(TPLName) %>% unique %>% length -> MSmassrankgen
dataset %>% summary
dataset %>% pull(Region) %>% unique %>% length -> MSregions
dataset %>% tally() -> MSrecords
dataset %>% pull(Source) %>% unique %>% length -> MSgroups
dataset %>% pull(Family) %>% unique %>% length -> MSfamilies
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
dataset %>% select(TPLName, Seed.mass) %>% na.omit %>% filter(Seed.mass == min(Seed.mass)) %>% pull(TPLName) %>% unique -> MSminMassSpp
dataset %>% pull(Seed.mass) %>% na.omit %>% max %>% round(0) -> MSmaxMass
dataset %>% select(TPLName, Seed.mass) %>% na.omit %>% filter(Seed.mass == max(Seed.mass)) %>% pull(TPLName) %>% unique -> MSmaxMassSpp
dataset %>% pull(Seed.mass) %>% na.omit %>% median %>% round(2) -> MSmedianMass 
dataset %>% pull(Embryo) %>% na.omit %>% min %>% round(4) -> MSminEmbryo
dataset %>% pull(Embryo) %>% na.omit %>% max %>% round(2) -> MSmaxEmbryo 
dataset %>% pull(Embryo) %>% na.omit %>% median %>% round(2) -> MSmedianEmbryo

# Figure 1: description of seed dormancy, mass, embryo

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
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11.5, color = "black")) +
  scale_fill_manual(values = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) -> fig1a

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
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11.5, color = "black")) +
  scale_fill_manual(values = c("yellowgreen", "gold")) -> fig1b

dataset %>%
  select(TPLName, Alpine, Embryo) %>%
  na.omit %>%
  unique %>%
  ggplot(aes(Alpine, Embryo, fill = Alpine)) +
  geom_boxplot() +
  labs(y = "Embryo:seed", title = "(C) Embryo:seed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11.5, color = "black")) +
  scale_fill_manual(values = c("yellowgreen", "gold")) -> fig1c

gridExtra::arrangeGrob(fig1a, fig1b, fig1c, ncol = 3, widths = c(9.3, 6.5, 6.5)) -> fig1

# Figure 2 - MCMCglmm fixed effects

rbind(
  rbind(
    read.csv("../results/MCMCglmm/proportion/alpR.germ.general.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv("../results/MCMCglmm/proportion/alpR.germ.general.embryo.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv("../results/MCMCglmm/proportion/alpR.germ.general.mass.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv("../results/MCMCglmm/proportion/alpR.germ.type.csv") %>%
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
                            Embryo = "Embryo:seed",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:seed", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA3"),
         Treatment = fct_relevel(Treatment, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(A) Effect on final germination proportions") +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig2a

rbind(
  rbind(
    read.csv("../results/MCMCglmm/mgt/alpR.MGT.general.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv("../results/MCMCglmm/mgt/alpR.MGT.general.embryo.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv("../results/MCMCglmm/mgt/alpR.MGT.general.mass.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv("../results/MCMCglmm/mgt/alpR.MGT.type.csv") %>%
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
                            Embryo = "Embryo:seed",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:seed", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA3"),
         Treatment = fct_relevel(Treatment, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(B) Effect on mean germination time") +
  scale_x_continuous(breaks = c(-.2, 0)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12, 
                                   color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig2b

rbind(
  rbind(
    read.csv("../results/MCMCglmm/unc/alpR.UNC.general.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
      mutate(Moderator = "Alone"),
    read.csv("../results/MCMCglmm/unc/alpR.UNC.general.embryo.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Embryo"),
    read.csv("../results/MCMCglmm/unc/alpR.UNC.general.mass.csv") %>%
      select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
      mutate(Moderator = "Mass")) %>%
    rename(Mean = post.mean,
           Lower = l.95..CI,
           Upper = u.95..CI,
           p = pMCMC,
           Treatment = model) %>%
    select(Moderator, Treatment, Mean, Lower, Upper, p),
  read.csv("../results/MCMCglmm/unc/alpR.UNC.type.csv") %>%
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
                            Embryo = "Embryo:seed",
                            Alone = "Main effect"),
         Moderator = fct_relevel(Moderator, c("Embryo:seed", "Seed mass",  "Generalist spp.", "Strict alpine spp.", "Main effect")),
         Treatment = recode(Treatment, Tmean = "Temperature", GA3 = "GA3"),
         Treatment = fct_relevel(Treatment, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light"))) %>%
  ggplot(aes(y = Moderator, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Moderator)) +
  facet_wrap(~ Treatment, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(C) Effect on germination uncertainty") +
  scale_x_continuous(breaks = c(-.1, 0)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("olivedrab",  "turquoise4", "yellowgreen", "gold", "purple")) -> fig2c

gridExtra::arrangeGrob(fig2a, fig2b, fig2c, ncol = 1) -> fig2

# Figure 3 MCMCglmm randoms effects

read.csv("../results/MCMCglmm/proportion/alpR.germ.general.csv") %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA3"),
         model = fct_relevel(model, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(A) Effect on final germination proportions") +
  scale_x_continuous(breaks = c(0, 4)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("firebrick3",  "orange", "darkturquoise", "midnightblue")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("firebrick3",  "orange", "darkturquoise", "midnightblue")) -> fig3a

read.csv("../results/MCMCglmm/mgt/alpR.MGT.general.csv") %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA3"),
         model = fct_relevel(model, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(B) Effect on mean germination time") +
  scale_x_continuous(breaks = c(0, .6)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("firebrick3",  "orange", "darkturquoise", "midnightblue")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("firebrick3",  "orange", "darkturquoise", "midnightblue")) -> fig3b

read.csv("../results/MCMCglmm/unc/alpR.UNC.general.csv") %>%
  select(model, ID:Accession) %>%
  gather(Trait, Value, ID:Accession) %>%
  mutate(Value = gsub(" --", "", Value),
         Value = gsub(")", "", Value),
         Value = gsub("\\(", "", Value)) %>%
  separate(Value, sep = " ", into = c("Mean", "Lower", "Upper")) %>%
  mutate(Mean = as.numeric(Mean),
         Lower = as.numeric(Lower),
         Upper = as.numeric(Upper)) %>%
  mutate(model = recode(model, Tmean = "Temperature", GA3 = "GA3"),
         model = fct_relevel(model, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light")),
         Trait = recode(Trait, Source = "Lab", ID = "Species", Accession = "Seedlot")) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, nrow = 1) +
  geom_point(size = 2) +
  labs(x = "Effect size", title = "(C) Effect on germination uncertainty") +
  scale_x_continuous(breaks = c(0, 1.5)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("firebrick3",  "orange", "darkturquoise", "midnightblue")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("firebrick3",  "orange", "darkturquoise", "midnightblue")) -> fig3c

gridExtra::arrangeGrob(fig3a, fig3b, fig3c, ncol = 1) -> fig3

# Figure 4 - MCMCglmm phylogenetic signal

rbind(
read.csv("../results/MCMCglmm/proportion/alpR.germ.general.csv") %>%
  mutate(Trait = "FGP") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower),
read.csv("../results/MCMCglmm/mgt/alpR.MGT.general.csv") %>%
  mutate(Trait = "MGT") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower),
read.csv("../results/MCMCglmm/unc/alpR.UNC.general.csv") %>%
  mutate(Trait = "UNC") %>% 
  select(model, Trait, lambda:upper) %>%
  rename(Mean = lambda, Upper = upper, Lower = lower)) %>%
  mutate(Trait = fct_relevel(Trait, c("UNC", "MGT",  "FGP")),
         model = recode(model, Tmean = "Temperature", GA3 = "GA3"),
         model = fct_relevel(model, c("Stratification", "GA3", "Scarification",  "Temperature", "Alternating", "Light"))) %>%
  ggplot(aes(y = Trait, x = Mean, 
             xmin = Lower, xmax = Upper,
             color = Trait)) +
  facet_wrap(~ model, nrow = 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black") +
  geom_point(size = 2) +
  labs(x = "Pagel's lambda") +
  scale_x_continuous(breaks = c(0, 1)) +
  geom_errorbarh(height = .3) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text.y = element_text(size = 12,
                                   color = c("darkorchid", "gold", "red3")),
        axis.text.x = element_text(size = 7.5, color = "black"),
        strip.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("darkorchid", "gold", "red3")) -> fig4

# Figure 5 - FAMD ordination

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
  merge(read.csv("../data/traits.csv"), by = "TPLName") %>%
  select(TPLName, Alpine, Dormancy, Seed.mass, Embryo, Temperature:GA3, MGT:UNC) %>%
  filter(!is.nan(Temperature)) %>% 
  filter(! is.nan(MGT) & is.finite(MGT)) -> traits

traits %>% pull(TPLName) %>% unique %>% length -> MSfamdspp

traits %>%
  select(Alpine, Dormancy, Seed.mass:GA3, MGT:UNC) %>%
  FactoMineR::FAMD(graph = FALSE) -> pcaAlpine

traits %>%
  cbind(pcaAlpine$ind$coord[, 1:2]) -> pcaInds

pcaAlpine$quanti.var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, `Seed mass` = "Seed.mass", 
                               `Embryo:seed` = "Embryo")) -> pcaVars

pcaAlpine$quali.var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") -> pcaVars2

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "grey", alpha = 0.5, size = 2.5, shape = 15) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
 geom_label(data = filter(pcaVars, Variable %in% c("Temperature", "Alternating",
                                                                   "Scarification", "GA3",
                                                                   "Stratification", "Light")), 
             aes(x = 4.5*Dim.1, y = 4.5*Dim.2, label = Variable), 
             size = 3) +
  labs(title = "(A) Germination cues") -> fig5a

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "grey", alpha = 0.5, size = 2.5, shape = 15) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars, ! Variable %in% c("Temperature", "Alternating", "Light",
                                                    "Scarification", "Stratification", "GA3")), 
             aes(x = 5.1*Dim.1, y = 5.1*Dim.2, label = Variable), 
             size = 3) +
  labs(title = "(B) Seed traits") -> fig5b


ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Dormancy), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("MD", "MPD", "ND", "PD", "PY")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 3, fill = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) +
  scale_color_manual(values = c("olivedrab", "yellowgreen", "turquoise4", "gold", "purple")) +
  labs(title = "(C) Dormancy class") -> fig5c

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(color = Alpine), alpha = 0.5, size = 2.5, shape = 15, show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank()) +
  scale_x_continuous(name = paste("Axis 1 (", round(pcaAlpine$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pcaAlpine$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = filter(pcaVars2, Variable %in% c("Generalist", "Strict")), 
             aes(x = Dim.1, y = Dim.2, label = Variable), 
             size = 3, fill = c("yellowgreen", "gold")) +
  scale_color_manual(values = c("yellowgreen", "gold")) +
  labs(title = "(D) Species distribution") -> fig5d

gridExtra::arrangeGrob(fig5a, fig5b, fig5c, fig5d, ncol = 2,
                       left = grid::textGrob("Second FAMD axis", rot = 90, gp = grid::gpar(fontsize = 14,
                                                                                           fontfamily = "serif")),
                       bottom = grid::textGrob("First FAMD axis", gp = grid::gpar(fontsize = 14,
                                                                                  fontfamily = "serif"))) -> fig5

# Save figures
# 
# ggsave(fig1, file = "../results/Figure_1.tiff", 
#        path = NULL, scale = 1, width = 180, height = 73, units = "mm", dpi = 600)
# 
# ggsave(fig2, file = "../results/Figure_2.tiff", 
#        path = NULL, scale = 1, width = 180, height = 145, units = "mm", dpi = 600)
# 
# ggsave(fig3, file = "../results/Figure_3.tiff", 
#        path = NULL, scale = 1, width = 180, height = 135, units = "mm", dpi = 600)
# 
# ggsave(fig4, file = "../results/Figure_4.tiff", 
#        path = NULL, scale = 1, width = 180, height = 45, units = "mm", dpi = 600)
# 
# ggsave(fig5, file = "../results/Figure_5.tiff", 
#        path = NULL, scale = 1, width = 180, height = 180, units = "mm", dpi = 600)
# 
