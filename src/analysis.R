library(tidyverse); library(here)

read.csv(here("results", "analysis", "alpR.germ.type.csv")) %>%
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
  labs(title = "Final germination proportion") + 
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank())

read.csv(here("results", "analysis", "alpR.MGT.type.csv")) %>%
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
  labs(title = "Mean germination time") + 
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank())

read.csv(here("results", "analysis", "alpR.UNC.type.csv")) %>%
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
  labs(title = "Germination uncertainty") + 
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank())

read.csv(here("results", "analysis", "alpR.germ.type.mass.csv")) %>%
  select(variable, post.mean, l.95..CI, u.95..CI, pMCMC) %>%
  separate(variable, into = c("Alpine", "Mass", "Treatment"), sep = ":") %>%
  mutate(Alpine = gsub("Alpine", "", Alpine)) %>%
  rename(Mean = post.mean,
         Lower = l.95..CI,
         Upper = u.95..CI,
         p = pMCMC) %>%
  ggplot(aes(y = Alpine, x = Mean, 
             xmin = Lower, xmax = Upper, 
             color = Alpine)) +
  geom_point() +
  labs(title = "Germination uncertainty") + 
  geom_errorbarh(height = .1) +
  facet_grid(~ Treatment, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none", axis.title = element_blank())



