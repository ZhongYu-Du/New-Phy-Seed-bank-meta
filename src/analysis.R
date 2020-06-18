library(tidyverse); library(here)

# Final germination proportion

## Figure for ms

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
        axis.text.y = element_text(color = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")) -> fig1a

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
        axis.text.y = element_text(color = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")) -> fig1b

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
        axis.text.y = element_text(color = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")),
        axis.text.x = element_text(size = 7)) +
  scale_color_manual(values = c("darkorchid", "indianred", "gold", "dodgerblue3", "forestgreen")) -> fig1c

cowplot::plot_grid(fig1a, fig1b, fig1c, ncol = 1) -> fig1

# Final germination proportions

## Full and interaction with seed mass and embryo

rbind(
  read.csv(here("results", "analysis", "proportion", "alpR.germ.general.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    mutate(Effect = "Alone"),
  read.csv(here("results", "analysis", "proportion", "alpR.germ.general.embryo.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Embryo"),
  read.csv(here("results", "analysis", "proportion", "alpR.germ.general.mass.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Mass")) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%  
  ggplot(aes(y = Effect, x = Mean, 
             xmin = Lower, xmax = Upper)) +
  facet_wrap(~ model) +
  geom_point() +

  geom_errorbarh(height = .1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgFull

## Strict vs. generalist

read.csv(here("results", "analysis", "proportion", "alpR.germ.type.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Alpine", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgAlpine

## Macroregions

read.csv(here("results", "analysis", "proportion", "alpR.germ.macroregion.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Macroregion", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgRegion

## Dormancy type

read.csv(here("results", "analysis", "proportion", "alpR.germ.Dormancy.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Dormancy", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgDormancy

## Life form

read.csv(here("results", "analysis", "proportion", "alpR.germ.Lifeform.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifeform", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgLifeform

## Life span

read.csv(here("results", "analysis", "proportion", "alpR.germ.Lifespan.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifespan", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgLifespan

## Plant type

read.csv(here("results", "analysis", "proportion", "alpR.germ.Plantcategory.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Plantcategory", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> fgPlanttype

# Mean germination time

## Full and interaction with seed mass and embryo

rbind(
  read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    mutate(Effect = "Alone"),
  read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.embryo.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Embryo"),
  read.csv(here("results", "analysis", "mgt", "alpR.MGT.general.mass.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Mass")) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%  
  ggplot(aes(y = Effect, x = Mean, 
             xmin = Lower, xmax = Upper)) +
  facet_wrap(~ model) +
  geom_point() +

  geom_errorbarh(height = .1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtFull

## Strict vs. generalist

read.csv(here("results", "analysis", "mgt", "alpR.MGT.type.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Alpine", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtAlpine

## Macroregions

read.csv(here("results", "analysis", "mgt", "alpR.MGT.macroregion.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Macroregion", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtRegion

## Dormancy type

read.csv(here("results", "analysis", "mgt", "alpR.MGT.Dormancy.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Dormancy", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtDormancy

## Life form

read.csv(here("results", "analysis", "mgt", "alpR.MGT.Lifeform.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifeform", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtLifeform

## Life span

read.csv(here("results", "analysis", "mgt", "alpR.MGT.Lifespan.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifespan", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtLifespan

## Plant type

read.csv(here("results", "analysis", "mgt", "alpR.MGT.Plantcategory.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Plantcategory", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +

  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> mgtPlanttype

# Uncertainty

## Full and interaction with seed mass and embryo

rbind(
  read.csv(here("results", "analysis", "unc", "alpR.UNC.general.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
    mutate(Effect = "Alone"),
  read.csv(here("results", "analysis", "unc", "alpR.UNC.general.embryo.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Embryo"),
  read.csv(here("results", "analysis", "unc", "alpR.UNC.general.mass.csv")) %>%
    select(model, post.mean, l.95..CI, u.95..CI, pMCMC) %>% 
    mutate(Effect = "Mass")) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%  
  ggplot(aes(y = Effect, x = Mean, 
             xmin = Lower, xmax = Upper)) +
  facet_wrap(~ model) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncFull

## Strict vs. generalist

read.csv(here("results", "analysis", "unc", "alpR.UNC.type.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Alpine", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncAlpine

## Macroregions

read.csv(here("results", "analysis", "unc", "alpR.UNC.macroregion.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Macroregion", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncRegion

## Dormancy type

read.csv(here("results", "analysis", "unc", "alpR.UNC.Dormancy.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Dormancy", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncDormancy

## Life form

read.csv(here("results", "analysis", "unc", "alpR.UNC.Lifeform.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifeform", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncLifeform

## Life span

read.csv(here("results", "analysis", "unc", "alpR.UNC.Lifespan.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Lifespan", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncLifespan

## Plant type

read.csv(here("results", "analysis", "unc", "alpR.UNC.Plantcategory.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Plantcategory", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank()) -> uncPlanttype