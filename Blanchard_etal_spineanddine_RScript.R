library(lme4)
library(sjPlot)

####### Resource Foraging Trials - Comparison Between P. flavicornis and P. laevigata ########
table_disc <- read.csv("ResourceForagingCompetitionData_FormattedForGLM.csv")
Species_disc <- table_disc$Species
Time_Bin_disc <- table_disc$Time_Bin/60
Col_ID_disc <- table_disc$Col_ID
Workers_disc <- table_disc$Workers
Presence_disc <- table_disc$Presence
Col_Size_disc <- table_disc$Col_Size

#Worker Number (Binomial distribution)
glmm_disc_worker <- glmer(cbind(Workers_disc,Col_Size_disc-Workers_disc)~Species_disc + Time_Bin_disc + (1|Col_ID_disc),family = "binomial")
plot_model(glmm_disc_worker,type = "pred",terms = c("Species_disc"))

#Worker Presence (Bernoulli distribution)
no_Presence_disc = 1-Presence_disc
glmm_disc_presence <- glmer(cbind(Presence_disc,no_Presence_disc)~Species_disc + Time_Bin_disc + Col_Size_disc + (1|Col_ID_disc),family = "binomial")
plot_model(glmm_disc_presence,type = "pred",terms = c("Species_disc"))

#Discovery Rate (Geometric distribution); GLM because no random effect (i.e. no Col_ID)
table_time <- read.csv("Data_DiscoveryRate_DiscoveryCompetition.csv")
Discovered_time <- table_time$Discovered
Time_Disc_time <- table_time$Time_Disc
Species_time <- table_time$Species
Col_Size_time <- table_time$Col_Size
glm_disc_time <- glm(cbind(Discovered_time,Time_Disc_time) ~ Species_time + Col_Size_time, family = "binomial")
plot_model(glm_disc_time,type = "pred",terms = c("Species_time"))

###### Competition Trials - Comparison Between Resource Foraging Trials and Competition Trials ######
############ For both P. flavicornis and P. laevigata
table_flav_comp <- read.csv("Data_flavicornis_ForagingCompetitionComparison.csv")
table_lae_comp <- read.csv("Data_laevigata_ForagingCompetitionComparison.csv")
table_flav_comp_time <- read.csv("Data_flavicornis_DiscoveryTimeCompetitionComparison.csv")
table_lae_comp_time <- read.csv("Data_laevigata_DiscoveryTimeCompetitionComparison.csv")
Trial_flav <- table_flav_comp$Trial
Time_Bin_flav <- table_flav_comp$Time_Bin/60
Col_ID_flav <- table_flav_comp$Col_ID
Workers_flav <- table_flav_comp$Workers
Presence_flav <- table_flav_comp$Presence
Col_Size_flav <- table_flav_comp$Col_Size

Trial_lae <- table_lae_comp$Trial
Time_Bin_lae <- table_lae_comp$Time_Bin/60
Col_ID_lae <- table_lae_comp$Col_ID
Workers_lae <- table_lae_comp$Workers
Presence_lae <- table_lae_comp$Presence
Col_Size_lae <- table_lae_comp$Col_Size

#Worker Number and Presence: P. flavicornis
glmm_worker_flav_comp <- glmer(cbind(Workers_flav,Col_Size_flav-Workers_flav)~Trial_flav + Time_Bin_flav + (1|Col_ID_flav),family = "binomial")
no_Presence_flav <- 1-Presence_flav
glmm_presence_flav_comp <- glmer(cbind(Presence_flav,no_Presence_flav)~Trial_flav + Time_Bin_flav + Col_Size_flav + (1|Col_ID_flav),family = "binomial")
plot_model(glmm_worker_flav_comp,type = "pred",terms = c("Trial_flav","Time_Bin_flav[n=1]"))
plot_model(glmm_presence_flav_comp,type = "pred",terms = c("Trial_flav"))

#Worker Investment and Presence: P. laevigata
glmm_worker_lae_comp <- glmer(cbind(Workers_lae,Col_Size_lae-Workers_lae)~Trial_lae + Time_Bin_lae + (1|Col_ID_lae),family = "binomial")
no_Presence_lae = 1-Presence_lae
glmm_presence_lae_comp <- glmer(cbind(Presence_lae,no_Presence_lae)~Trial_lae + Time_Bin_lae + Col_Size_lae + (1|Col_ID_lae),family = "binomial")
plot_model(glmm_worker_lae_comp,type = "pred",terms = c("Trial_lae","Time_Bin_lae[n=1]"))
plot_model(glmm_presence_lae_comp,type = "pred",terms = c("Trial_lae"))

#Discovery Time: P. flavicornis
Discovered_comp_flav_time <- table_flav_comp_time$Discovered
Time_Disc_flav_time <- table_flav_comp_time$Time_Disc
Trial_flav_time <- table_flav_comp_time$Trial
Col_Size_flav_time <- table_flav_comp_time$Col_Size
glm_comp_flav_time <- glm(cbind(Discovered_comp_flav_time,Time_Disc_flav_time) ~ Trial_flav_time + Col_Size_flav_time, family = "binomial")
plot_model(glm_comp_flav_time,type = "pred",terms = c("Trial_flav_time"))

#Discovery Time: P. laevigata
Discovered_comp_lae_time <- table_lae_comp_time$Discovered
Time_Disc_lae_time <- table_lae_comp_time$Time_Disc
Trial_lae_time <- table_lae_comp_time$Trial
Col_Size_lae_time <- table_lae_comp_time$Col_Size
glm_comp_lae_time <- glm(cbind(Discovered_comp_lae_time,Time_Disc_lae_time) ~ Trial_lae_time + Col_Size_lae_time, family = "binomial")
plot_model(glm_comp_lae_time,type = "pred",terms = c("Trial_lae_time"))

####### Analyses ######
summary(glmm_disc_worker)
summary(glmm_disc_occupancy)
summary(glm_disc_time)
summary(glmm_worker_flav_comp)
summary(glmm_presence_flav_comp)
summary(glmm_worker_lae_comp)
summary(glmm_presence_lae_comp)
summary(glm_comp_flav_time)
summary(glm_comp_lae_time)


###### MULTISPECIES: Resource Foraging Trials ######

###Instead of species as focus, using spines as fixed factor and species as a random effect
table_multi <- read.csv("Data_Multispecies_ResourceForaging_FormattedForGLM.csv")
Species_multi <- table_multi$Species
Body <- table_multi$Body
Spine <- table_multi$Spine
Time_multi <- table_multi$Time_Bin/60
Col_ID_multi <- table_multi$Col_ID
Workers_multi <- table_multi$Workers
Presence_multi <- table_multi$Presence
Col_Size_multi <- table_multi$Col_Size

glmm_worker_multispecies <- glmer(cbind(Workers_multi,Col_Size_multi-Workers_multi)~Spine + Body + Time_multi + (1|Col_ID_multi) + (1|Species_multi),family = "binomial")
plot_model(glmm_worker_multispecies ,type = "pred",terms = c("Spine"))

Col_Size_multi_standard <- table_multi$Col_Size/820 #divided by largest colony size to improve statistical stability
no_Presence_multi <- 1-Presence_multi
glmm_presence_multispecies <- glmer(cbind(Presence_multi,no_Presence_multi)~Spine + Body + Time_multi + Col_Size_multi_standard + (1|Col_ID_multi) + (1|Species_multi),family = "binomial")
plot_model(glmm_presence_multispecies ,type = "pred",terms = c("Spine"))

table_multi_time <- read.csv("Data_Multispecies_DiscoveryTime_Foraging.csv")
Discovered_multi_time <- table_multi_time$Discovered
Time_Disc_multi_time <- table_multi_time$Time_Disc
Species_multi_time <- table_multi_time$Species
Spine_multi_time <- table_multi_time$Spine
Body_multi_time <- table_multi_time$Body
Col_Size_multi_time <- table_multi_time$Col_Size/820
#this time use glmm (glmer function) for the geometric/Discovery Time model as using Species as Random Factor
glmm_disc_multi_time <- glmer(cbind(Discovered_multi_time,Time_Disc_multi_time) ~ Spine_multi_time + Body_multi_time + Col_Size_multi_time + (1|Species_multi_time), family = "binomial")
plot_model(glmm_disc_multi_time,type = "pred",terms = c("Spine_multi_time"))

summary(glmm_worker_multispecies)
summary(glmm_presence_multispecies)
summary(glmm_disc_multi_time)

## Multispecies with P. (Cyrtomyrma) spp. removed

table_multi_noCyrto <- read.csv("Data_Multispecies_ResourceForaging_FormattedForGLM_CyrtomyrmaRemoved.csv")
Species_multi_noCyrto <- table_multi_noCyrto$Species
Body_noCyrto <- table_multi_noCyrto$Body
Spine_noCyrto <- table_multi_noCyrto$Spine
Time_multi_noCyrto <- table_multi_noCyrto$Time_Bin/60
Col_ID_multi_noCyrto <- table_multi_noCyrto$Col_ID
Workers_multi_noCyrto <- table_multi_noCyrto$Workers
Presence_multi_noCyrto <- table_multi_noCyrto$Presence
Col_Size_multi_noCyrto <- table_multi_noCyrto$Col_Size

glmm_worker_multispecies_noCyrto <- glmer(cbind(Workers_multi_noCyrto,Col_Size_multi_noCyrto-Workers_multi_noCyrto)~Spine_noCyrto + Body_noCyrto + Time_multi_noCyrto + (1|Col_ID_multi_noCyrto) + (1|Species_multi_noCyrto),family = "binomial")
plot_model(glmm_worker_multispecies_noCyrto,type = "pred",terms = c("Spine_noCyrto"))

Col_Size_multi_standard_noCyrto <- table_multi_noCyrto$Col_Size/820 #divided by largest colony size to improve statistical stability
no_Presence_multi_noCyrto <- 1-Presence_multi_noCyrto
glmm_presence_multispecies_noCyrto <- glmer(cbind(Presence_multi_noCyrto,no_Presence_multi_noCyrto)~Spine_noCyrto + Body_noCyrto + Time_multi_noCyrto + Col_Size_multi_standard_noCyrto + (1|Col_ID_multi_noCyrto) + (1|Species_multi_noCyrto),family = "binomial")
plot_model(glmm_presence_multispecies_noCyrto,type = "pred",terms = c("Spine_noCyrto"))

table_multi_time_noCyrto <- read.csv("Data_Multispecies_DiscoveryTime_Foraging_CyrtomyrmaRemoved.csv")
Discovered_multi_time_noCyrto <- table_multi_time_noCyrto$Discovered
Time_Disc_multi_time_noCyrto <- table_multi_time_noCyrto$Time_Disc
Species_multi_time_noCyrto <- table_multi_time_noCyrto$Species
Spine_multi_time_noCyrto <- table_multi_time_noCyrto$Spine
Body_multi_time_noCyrto <- table_multi_time_noCyrto$Body
Col_Size_multi_time_noCyrto <- table_multi_time_noCyrto$Col_Size/820
#this time use glmm (glmer function) for the geometric/Discovery Time model as using Species as Random Factor
glmm_disc_multi_time_noCyrto <- glmer(cbind(Discovered_multi_time_noCyrto,Time_Disc_multi_time_noCyrto) ~ Spine_multi_time_noCyrto + Body_multi_time_noCyrto + Col_Size_multi_time_noCyrto + (1|Species_multi_time_noCyrto), family = "binomial")
plot_model(glmm_disc_multi_time_noCyrto,type = "pred",terms = c("Spine_multi_time_noCyrto"))

#Correlation Tables

corr_twospp_disc <- table_disc[,c(2,4,5,6)]
cor(corr_twospp_disc,method = "pearson")

corr_twospp_time <- table_time[,c(3,4,5)]
cor(corr_twospp_time,method = "pearson")

corr_twospp_flav_comp <- table_flav_comp[,c(2,4,5,6)]
cor(corr_twospp_flav_comp,method = "pearson")

corr_twospp_lae_comp <- table_lae_comp[,c(2,4,5,6)]
cor(corr_twospp_lae_comp,method = "pearson")

corr_twospp_flav_comp_time <- table_flav_comp_time[,c(3,4,5)]
cor(corr_twospp_flav_comp_time,method = "pearson")

corr_twospp_lae_comp_time <- table_lae_comp_time[,c(3,4,5)]
cor(corr_twospp_lae_comp_time,method = "pearson")

corr_multi_time_noCyrto <- table_multi_time_noCyrto[,c(2,3,5,6,7)]
cor(corr_multi_time_noCyrto,method = "pearson")

corr_multi_disc <- table_multi[,c(2,3,4,6,7,8)]
cor(corr_multi_disc,method = "pearson")

corr_multi_disc_noCyrto <- table_multi_noCyrto[,c(2,3,4,6,7,8)]
cor(corr_multi_disc_noCyrto,method = "pearson")


###### Colley Matrix (2017 data) ######
#Phyloregression With Colley Matrix Data
library(ape)
library(phylolm)
tree <- read.tree("PolyrhachisTree_EcoEvo.tre")
data <- read.table("Colley_All.csv",sep=",",header = T)
Species <- data$Species
table <- data[2:4]
row.names(table) <- Species
BodySize <- table[1]
Spine <- table[2]
Ranking <- table[3]

fit <- phylolm(Ranking ~ Spine + BodySize, data = table, phy = tree, model = "BM")
summary(fit)
plot(data$Spine,data$Ranking,xlab="Spine Length (mm)",ylab="Colley Matrix Ranking",main="Spine Length Influence on Competitive Ability")
abline(summary(fit)$coefficients[1],summary(fit)$coefficients[2],col="black")

