library(tidyverse); library(lme4); library(cowplot)

respuesta <- function(x) 1/(1+1/(exp(x)))

# ALL SPECIES

load("C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Joined DB/AlpineDB.RData")

# TEMPERATURES

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Stratification == "N" & Alternating == "Y")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Temperature) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Temperature = c(5, 10, 15, 20, 25)), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1], ci[5, 1] + ci[3, 1], ci[6, 1] + ci[3, 1], ci[7, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2], ci[5, 2] + ci[3, 2], ci[6, 2] + ci[3, 2], ci[7, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = "Alternating",
                 Temperature = c(5, 10, 15, 20, 25),
                 Mean = r[, 1],
                 Lower = r[, 2],
                 Upper = r[, 3])

tableTemperatures<- d %>% select(Region, Alpine, Species) %>% unique %>% group_by(Region, Alpine) %>% tally() %>% spread(Alpine, n)  

table1b <- summary(m)

plot1 <- ggplot(e, aes(x = Temperature, y = Mean, ymin = Lower, ymax = Upper, 
                               fill = Temperature)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Temperature), width = 1, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "All species", subtitle = "Germination peaks at 20ºC") +
  ylab(label = "Germination proportion") + xlab(label = "Temperature (ºC)") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_x_continuous(breaks = c(5,10,15,20,25)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_fill_gradient(low = "blue", high = "red")

# STRATIFICATION

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Alternating == "Y" & Stratification != "Warm")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ Stratification + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Stratification = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = c("Unstratified", "Stratified"), Alternating = "Alternating",
                 Temperature = "20ºC",
                 Mean = r[, 1],
                 Lower = r[, 2],
                 Upper = r[, 3])
e$Stratification <- factor(e$Stratification, levels = c("Unstratified", "Stratified"))

tableStratification <- d %>% select(Region, Alpine, Species) %>% unique %>% group_by(Region, Alpine) %>% tally() %>% spread(Alpine, n)  

table2b <- summary(m)

plot2 <- ggplot(e, aes(x = Stratification, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Stratification)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Stratification), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "All species", subtitle = "Increase significant, p <2e-16") +
  ylab(label = "Germination proportion") + xlab(label = "Cold stratification") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("firebrick", "dodgerblue3")) +
  scale_fill_manual(values = c("firebrick", "dodgerblue3"))

# ALTERNATING

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Stratification == "N")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Alternating) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Alternating = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = c("Constant", "Alternating"),
                  Temperature = "20ºC",
                  Mean = r[, 1],
                  Lower = r[, 2],
                  Upper = r[, 3])
e$Alternating <- factor(e$Alternating, levels = c("Constant", "Alternating"))

tableAlternating <- d %>% select(Region, Alpine, Species) %>% unique %>% group_by(Region, Alpine) %>% tally() %>% spread(Alpine, n)   

table3b <- summary(m)

plot3 <- ggplot(e, aes(x = Alternating, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Alternating)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Alternating), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "All species", subtitle = "No significant effect, p = 0.424") +
  ylab(label = "Germination proportion") + xlab(label = "Constant vs. alternating temperatures") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("darkorchid", "gold2")) +
  scale_fill_manual(values = c("darkorchid", "gold2"))

# GENERALIST SPECIES

load("C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Joined DB/AlpineDB.RData")

AlpineDB %>% 
  filter(Alpine == "Generalist") -> AlpineDB

# TEMPERATURES

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Stratification == "N" & Alternating == "Y")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Temperature) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Temperature = c(5, 10, 15, 20, 25)), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1], ci[5, 1] + ci[3, 1], ci[6, 1] + ci[3, 1], ci[7, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2], ci[5, 2] + ci[3, 2], ci[6, 2] + ci[3, 2], ci[7, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = "Alternating",
                Temperature = c(5, 10, 15, 20, 25),
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])

table4b <- summary(m)

plot4 <- ggplot(e, aes(x = Temperature, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Temperature)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Temperature), width = 1, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Generalist species", subtitle = "Same patter, higher germination") +
  ylab(label = "Germination proportion") + xlab(label = "Temperature (ºC)") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_x_continuous(breaks = c(5,10,15,20,25)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_fill_gradient(low = "blue", high = "red")

# STRATIFICATION

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Alternating == "Y" & Stratification != "Warm")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ Stratification + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Stratification = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = c("Unstratified", "Stratified"), Alternating = "Alternating",
                Temperature = "20ºC",
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])
e$Stratification <- factor(e$Stratification, levels = c("Unstratified", "Stratified"))

table5b <- summary(m)

plot5 <- ggplot(e, aes(x = Stratification, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Stratification)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Stratification), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Generalist species", subtitle = "Same effect") +
  ylab(label = "Germination proportion") + xlab(label = "Cold stratification") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("firebrick", "dodgerblue3")) +
  scale_fill_manual(values = c("firebrick", "dodgerblue3"))

# ALTERNATING

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Stratification == "N")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Alternating) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Alternating = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = c("Constant", "Alternating"),
                Temperature = "20ºC",
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])
e$Alternating <- factor(e$Alternating, levels = c("Constant", "Alternating"))

table6b <- summary(m)

plot6 <- ggplot(e, aes(x = Alternating, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Alternating)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Alternating), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Generalist species", subtitle = "Increase significant, p = 3.73e-07") +
  ylab(label = "Germination proportion") + xlab(label = "Constant vs. alternating temperatures") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("darkorchid", "gold2")) +
  scale_fill_manual(values = c("darkorchid", "gold2"))

# STRICT ALPINE SPECIES

load("C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Joined DB/AlpineDB.RData")

AlpineDB %>% 
  filter(Alpine == "Strict") -> AlpineDB

# TEMPERATURES

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Stratification == "N" & Alternating == "Y")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Temperature) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Temperature = c(5, 10, 15, 20, 25)), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1], ci[5, 1] + ci[3, 1], ci[6, 1] + ci[3, 1], ci[7, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2], ci[5, 2] + ci[3, 2], ci[6, 2] + ci[3, 2], ci[7, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = "Alternating",
                Temperature = c(5, 10, 15, 20, 25),
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])

table7b <- summary(m)

plot7 <- ggplot(e, aes(x = Temperature, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Temperature)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Temperature), width = 1, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Strict alpine species", subtitle = "Same pattern, lower germination") +
  ylab(label = "Germination proportion") + xlab(label = "Temperature (ºC)") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_x_continuous(breaks = c(5,10,15,20,25)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_fill_gradient(low = "blue", high = "red")

# STRATIFICATION

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Alternating == "Y" & Stratification != "Warm")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ Stratification + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Stratification = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = c("Unstratified", "Stratified"), Alternating = "Alternating",
                Temperature = "20ºC",
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])
e$Stratification <- factor(e$Stratification, levels = c("Unstratified", "Stratified"))

table8b <- summary(m)

plot8 <- ggplot(e, aes(x = Stratification, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Stratification)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Stratification), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Strict alpine species", subtitle = "Same effect") +
  ylab(label = "Germination proportion") + xlab(label = "Cold stratification") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("firebrick", "dodgerblue3")) +
  scale_fill_manual(values = c("firebrick", "dodgerblue3"))

# ALTERNATING

d <- subset(AlpineDB, Scarification == "N" & GA3 == "N" & Light == "Y" & Temperature == 20 & Stratification == "N")

m <- glmer(cbind(Germinated, Germinable - Germinated) ~ factor(Alternating) + (1|Species) + (1|Region), family = "binomial", data = d)

summary(m)

ci <- confint(m, method = "Wald")

p <- predict(m, expand.grid(Alternating = c("N", "Y")), re.form = NA)

mt <- cbind(p, 
            c(ci[3, 1], ci[4, 1] + ci[3, 1]), 
            c(ci[3, 2], ci[4, 2] + ci[3, 2]))

r <- respuesta(mt)

e <- data.frame(Stratification = "Unstratified", Alternating = c("Constant", "Alternating"),
                Temperature = "20ºC",
                Mean = r[, 1],
                Lower = r[, 2],
                Upper = r[, 3])
e$Alternating <- factor(e$Alternating, levels = c("Constant", "Alternating")) 

table9b <- summary(m)

plot9 <- ggplot(e, aes(x = Alternating, y = Mean, ymin = Lower, ymax = Upper, 
                       fill = Alternating)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, show.legend = FALSE) + 
  geom_errorbar(aes(color = Alternating), width = .2, size = 1, 
                position = position_dodge(.9), show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Strict alpine species", subtitle = "Decrease significant, p = 8.69e-06") +
  ylab(label = "Germination proportion") + xlab(label = "Constant vs. alternating temperatures") +
  theme(text = element_text(size = 8), #legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_manual(values = c("darkorchid", "gold2")) +
  scale_fill_manual(values = c("darkorchid", "gold2"))

plot_grid(plot1, plot4, plot7, nrow = 1) -> plotTemperatures

plot_grid(plot2, plot5, plot8, nrow = 1) -> plotStratification

plot_grid(plot3, plot6, plot9, nrow = 1) -> plotAlternating

# SPECIES and CONTRIBUTORS tables

load("C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Joined DB/AlpineDB.RData")

AlpineDB %>%
  select(Region, Species, Alpine, Min.elevation, Max.elevation, Treeline) %>% 
  arrange(Region, Species, Alpine) %>%
  unique -> TableSpecies

AlpineDB %>%
  select(Source, Species) %>% 
  unique %>%
  group_by(Source) %>%
  tally() -> TableSources

# MAP

library(maps); library(mapdata); library(extrafont)

AlpineDB %>%  
  select(Species, Region, Latitude, Longitude) %>%
  unique %>%
  group_by(Region) %>%
  summarise(n = length(Species), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
  ggplot(aes(Longitude, Latitude, color = Region)) +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
               color = "gainsboro", fill = "gainsboro") +
  geom_point(aes(size = n), alpha = 0.4 ) +
  guides(size = F) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-49, 80)) +
  theme(axis.line=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), plot.background=element_blank(),
        legend.title = element_blank(), 
        legend.position = "right",
        legend.text = element_text(margin = margin(r = 11, unit = "pt"))) -> Mapa

save(Mapa, plotAlternating, plotStratification, plotTemperatures,
     tableAlternating, tableStratification, tableTemperatures,
     TableSpecies, TableSources,
     file = "C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Alpine/Analysis/AlpineOutput.RData")

write.csv(TableSpecies, "C:/EFP/Trabayu/Activos/2018 FICYT3/GT1 Traits/Alpine/Preliminary report/Species.csv", row.names = FALSE)
