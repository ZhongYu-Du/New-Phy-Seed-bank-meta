library(MCMCglmm)

# This script shows as a sample the code to fit the MCMCglmms models. Because these models take a long time to be computed,
# the final output of the definitive models has been saved in the results folder, and is read from there
# to build the manuscript (by the manuscript-source script).

germination <- read.csv("results/germination.csv") # Read clean germination data
traits <- read.csv("results/traits.csv") # Read clean species traits
nnls <- ape::read.tree("data/carta/ALPINE.tree") # Read phylogenetic tree

# MCMCglmm analyses

## Table for MCMCglmm analyses

traits %>%
  merge(germination, by = "TPLName") %>%
  merge(read.csv("data/fernandezpascual/regions.csv"), by = "Region", all.x = TRUE) %>%
  mutate(TPLName = gsub(" ", "_", TPLName),
         Macroregion = as.character(Macroregion),
         Macroregion = ifelse(is.na(Macroregion), "Europe", Macroregion),
         Stratification2 = as.character(Stratification),
         Scarification = ifelse(Scarification == "Y", 1, 0),
         Stratification = ifelse(Stratification == "None", 0, 1),
         GA3 = ifelse(GA3 == "Y", 1, 0),
         Light = ifelse(Light == "Y", 1, 0),
         Alternating = ifelse(Alternating == "Y", 1, 0),
         percent = Germinated / Germinable) %>%
  rename(embryo = Embryo, mass = Seed.mass) %>%
  mutate(animal = TPLName, ID = TPLName) %>%
  select(animal, 
         ID,
         Alpine,
         Dormancy, embryo, mass,
         Source, Macroregion, Region, Accession,
         Dish, Stratification2,
         Scarification, GA3, 
         Stratification,
         Light, Alternating,
         Tmean, Tdif, Temperature,
         Sown, Germinated, Germinable,
         percent,
         MGT, MGR,
         UNC, SYN) -> alpineDB

## Binomial models

priors <-  list(R = list(V = 1, nu = 50), 
                G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 500), 
                         G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 500),
                         G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 500),
                         G4 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 500),
                         G5 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 500)))

varlist <- c("Tmean", "Light", "Alternating", "Stratification", "Scarification")

### Start loop

List.alp = list()

for (i in 1:6) { 
  fixed <- as.formula(paste("cbind(Germinated, Germinable-Germinated) ~", varlist[i], sep = ""))
  models <- MCMCglmm(fixed = fixed, 
                     random = ~ animal + ID + Source + Region + Accession, 
                     family = "multinomial2", pedigree = nnls, prior = priors, data = alpineDB, 
                     nitt = 500000, thin = 50, burnin = 50000, verbose = F) 
  List.alp[[length(List.alp) + 1]] = models
}


## Gaussian models

priors <- list(R = list(V = 1, nu = 0.2),
               G = list(G1 = list(V = 1, nu = 0.2, alpha.mu = 0, alpha.V = 1e3),
                        G2 = list(V = 1, nu = 0.2, alpha.mu = 0, alpha.V = 1e3),
                        G3 = list(V = 1, nu = 0.2, alpha.mu = 0, alpha.V = 1e3),
                        G4 = list(V = 1, nu = 0.2, alpha.mu = 0, alpha.V = 1e3),
                        G5 = list(V = 1, nu = 0.2, alpha.mu = 0, alpha.V = 1e3)))

varlist <- c("Tmean", "Light", "Alternating", "Stratification", "Scarification", "GA3")

### Start loop

List.alp = list()

for (i in 1:6) {
  fixed <- as.formula(paste("MGT~", varlist[i], sep = ""))
  models <- MCMCglmm(fixed = fixed, 
                     random = ~ animal + ID + Source + Region + Accession, 
                     family = "gaussian", pedigree = nnls, prior = priors, data = alpineDB, 
                     nitt = 500000, thin = 50, burnin = 50000, verbose = F)
  List.alp[[length(List2.alp) + 1]] = models
}
