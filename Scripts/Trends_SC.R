# ....................................................... #
# Exploration, Trends and Breakpoints for reef fish in SC #
# ......Script adapted by Fernanda Silva 17/08/2022...... #


# Load packages required for this script:
library(tidyverse)
library(corrplot)
library(IEAtools)
library(plyr)

# Reef fish abundance data (sampling standardized)
setwd("C:/Users/silva/Desktop/Mission-Atlantic-WP5")
fishdata <- read.table("general_abundances.csv", h = T, sep = ";")

# Add new columns grouping data
fishdata$totalabundance <- rowSums(fishdata[,c(2:144)], na.rm=TRUE)
fishdata$groupers <- rowSums(fishdata[,c("epinephelus_itajara", "epinephelus_marginatus", "epinephelus_morio",
                                         "hyporthodus_niveatus")], na.rm=TRUE)
fishdata$parrotfish <- rowSums(fishdata[,c("scarus_trispinosus","scarus_zelindae","sparisoma_amplum",
                                           "sparisoma_axillare","sparisoma_frondosum","sparisoma_radians","sparisoma_tuiupiranga")], na.rm=TRUE)
fishdata$omnivore <- rowSums(fishdata[,c("abudefduf_saxatilis","archosargus_rhomboidalis","diplodus_argenteus",
                                         "hypsoblennius_invemar","pomacanthus_arcuatus","pomacanthus_paru","scartella_cristata",
                                         "stephanolepis_hispidus")], na.rm=TRUE)
fishdata$macroalgivore <- rowSums(fishdata[,c("acanthurus_coeruleus","kyphosus_sectatrix","kyphosus_vaigiensis")], na.rm=TRUE)
fishdata$herb_detr <- rowSums(fishdata[,c("acanthurus_bahianus","acanthurus_chirurgus","cryptotomus_roseus","mugil_curema",
                                          "nicholsina_usta","ophioblennius_sp","scarus_trispinosus","scarus_zelindae",
                                          "sparisoma_amplum","sparisoma_axillare","sparisoma_frondosum","sparisoma_radians",
                                          "sparisoma_tuiupiranga","stegastes_fuscus","stegastes_variabilis")], na.rm=TRUE)
fishdata$macrocarnivore <- rowSums(fishdata[,c("carangoides_bartholomaei","caranx_crysos","caranx_latus","centropomus_undecimalis",
                                               "epinephelus_itajara","epinephelus_marginatus","epinephelus_morio",
                                               "fistularia_tabacaria","gymnothorax_miliaris","gymnothorax_moringa",
                                               "gymnothorax_vicinus","hyporthodus_niveatus","lutjanus_analis","lutjanus_jocu",
                                               "mycteroperca_acutirostris","mycteroperca_bonaci","mycteroperca_interstitialis",
                                               "mycteroperca_microlepis","priacanthus_arenatus","pseudocaranx_dentex",
                                               "scomberomorus_sp","seriola_dumerili","seriola_lalandi","seriola_rivoliana",
                                               "sphyraena_barracuda","sphyraena_guachancho","sphyraena_picudilla","sphyraena_tome",
                                               "strongylura_marina","synodus_intermedius","synodus_synodus")], na.rm=TRUE)
fishdata$planktivore <- rowSums(fishdata[,c("chromis_flavicauda","chromis_jubauna","chromis_limbata","chromis_multilineata",
                                            "clepticus_brasiliensis","decapterus_punctatus","engraulis_anchoita",
                                            "harengula_clupeola","paranthias_furcifer","pempheris_schomburgkii",
                                            "ptereleotris_randalli","sardinella_brasiliensis")], na.rm=TRUE)

names(fishdata)
fishdata <- fishdata[,c(1,145:152)]

#### Open predictors ####
setwd('C:/Users/silva/Dropbox/Fernanda_Silva/Tese/Chapter_I')
pressures <- read.table("DataBases/data_region_models.csv", sep = ";", dec = ".", h =T)
pressures <- plyr::ddply(pressures, .(year), numcolwise(mean))
pressures <- pressures[-1,c(1:6,9:14,17)]
pressures[nrow(pressures) + 1,] <- c(2021, rep(NA, 12)) # 2021 has no data

### Normality --------------------------------
# State: Inspect distributions and test for normality

fishdata %>%
  pivot_longer(-year, names_to = "ind", values_to = "val") %>%
  ggplot(aes(val)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(col = "red") +
  facet_wrap(~ind, ncol = 5, scales = "free")

pressures %>%
  pivot_longer(-year, names_to = "ind", values_to = "val") %>%
  ggplot(aes(val)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(col = "red") +
  facet_wrap(~ind, ncol = 5, scales = "free")


# Many indicators are right skewed. Lets create another dataset with transformed values,
# e.g. the fourth root as it is not as as severe as the log.
fishdata_trans <- fishdata
fishdata_trans[ ,-1] <- fishdata_trans[ ,-1]^.25
# fishdata_trans[ ,-1] <- log(fishdata_trans[ ,-1])

# Bind 2 dataframes (indicators + pressures)
fishdata2 <- cbind(fishdata_trans, pressures[,-1])

################## Explore individual trends ####################

# Visualize single (transformed) trends together: Traffic Light Plots
# Indicators
trafficlight(x = fishdata2[,-1], time = fishdata2$year,
             main = "TLP based on quantiles (sorted by first 5-yr mean)")
trafficlight(x = fishdata2[,-1], time = fishdata2$year, method = "intervals",
             main = "TLP based on evenly spaced intervals (sorted by first 5-yr mean)")

############################### Breakpoints in time  ######################################

# Analysis should be done on time series without missing values
names(fishdata)
z <- ts(fishdata_trans[3:9], start = fishdata_trans$year[1],
        end = fishdata_trans$year[length(fishdata_trans$year)], frequency = 1)


# ==================== Bayesian changepoint algorithm  ======================

library(bcp)

bcp_z <- bcp(z, return.mcmc = TRUE)
# invisible(capture.output(bcp_sum <- as.data.frame(summary(bcp_x))))
plot(bcp_z)

bcp_sum <- as.data.frame(summary(bcp_z))

# Get year(s) with highest probability (> 0.7)
invisible(capture.output(bcp_sum <- as.data.frame(summary(bcp_z))))
bcp_sum$id <- 1:((length(z)/7))#divide with the number of indicators (I included 7)
sel <- bcp_sum[which(bcp_z$posterior.prob > 0.7), ]
drop1<-time(z)[sel$id]
drop1<-as.data.frame(drop1)
