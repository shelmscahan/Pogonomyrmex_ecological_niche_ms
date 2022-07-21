#R scripts for ecological niche modeling, statistical analyses and visualizations for Helms Cahan et al. Evol Ecol ms
setwd("C:/Users/shelm/OneDrive - University of Vermont/Documents/Pace")

options(java.parameters = "-Xmx15g")

library(raster)
library(sp)
library(SSDM)


#worldclim data 30sec
bio_files <- list.files(path="C:/Users/shelm/OneDrive - University of Vermont/Documents/Pace/wc0.5", pattern = "tif", full.names = TRUE)
bio <- stack(bio_files)


#Pogo sites from literature.
Pogo_known <- read.csv("Pogo_locations.csv", header=T)
Pogo_known <- subset(Pogo_known, ID != "X")
coordinates(Pogo_known) = cbind(Pogo_known$long, Pogo_known$lat)
rasValue = extract(bio, Pogo_known)
Pogo_withenv = cbind(Pogo_known, rasValue)
write.table(Pogo_withenv,file="GPS_exp_with_bio.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)
Pogo_known$ID = as.factor(Pogo_known$ID)

#separate out points by type
library(dismo)
library(maptools)
library(yarrr)
Pogo_obs <-read.csv("GPS_exp_with_bio.csv", header=T)
Locs <- data.frame(Pogo_known$long, Pogo_known$lat)
FGH_Locs <- subset(Pogo_known, ID == "FGH", select = c(long, lat))
J_Locs <- subset(Pogo_known, ID == "J", select = c(long, lat))
Pbarb_Locs <- subset(Pogo_known, ID == "barbatus", select = c(long, lat))
Prug_Locs <- subset(Pogo_known, ID == "rugosus", select = c(long, lat))

#reducing to uncorrelated variables only
cor(Pogo_obs[,9:27], method = "pearson")
bio_reduced <- subset(bio, c(1,2,4,6,7,11,12,14,18,19))

# Determine geographic extent of our data
max.lat <- ceiling(max(Locs$Pogo_known.lat))
min.lat <- floor(min(Locs$Pogo_known.lat))
max.lon <- ceiling(max(Locs$Pogo_known.long))
min.lon <- floor(min(Locs$Pogo_known.long))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Crop bioclim data to geographic extent 
bio_reduced <- crop(x = bio_reduced, y = geographic.extent)

library(rJava)
library(ENMTools)
Pbarb_background <- read.csv("All_background.csv", header=TRUE)
Pbarb_background <- data.frame(Pbarb_background)
Pbarb_Locs <- data.frame(Pbarb_Locs)
Pbarb_species <- enmtools.species(presence.points = Pbarb_Locs, background.points = Pbarb_background, species.name = "P. barbatus")
Pbarb_gm <- enmtools.gam(Pbarb_species, env = bio_reduced, bg.source = "env", nback = 1000, test.prop = 0, gam.select = TRUE)
Prug_background <- read.csv("All_background.csv", header=TRUE)
Prug_background <- data.frame(Prug_background)
Prug_Locs <- data.frame(Prug_Locs)
Prug_species <- enmtools.species(presence.points = Prug_Locs, background.points = Prug_background, species.name = "P. rugosus")
Prug_gm <- enmtools.gam(Prug_species, env = bio_reduced, bg.source = "env", nback = 1000, test.prop = 0)
FGH_background <- read.csv("All_background.csv", header=TRUE)
FGH_background <- data.frame(FGH_background)
FGH_Locs <- data.frame(FGH_Locs)
FGH_species <- enmtools.species(presence.points = FGH_Locs, background.points = FGH_background, species.name = "FGH lineages")
FGH_gm <- enmtools.gam(FGH_species, env = bio_reduced, bg.source = "env", nback = 1000, test.prop = 0)
J_background <- read.csv("All_background.csv", header=TRUE)
J_background <- data.frame(J_background)
J_Locs <- data.frame(J_Locs)
J_species <- enmtools.species(presence.points = J_Locs, background.points = J_background, species.name = "J lineages")
J_gm <- enmtools.gam(J_species, env = bio_reduced, bg.source = "env", nback = 1000, test.prop = 0)
# to get direction of coefficients
J_coeff <- enmtools.gam(J_species, env = bio_reduced, bg.source = "env", nback = 1000, test.prop = 0, f= presence ~ wc2.1_30s_bio_1 + wc2.1_30s_bio_2 + wc2.1_30s_bio_4 + wc2.1_30s_bio_8 + wc2.1_30s_bio_9 + 
                       wc2.1_30s_bio_10 + wc2.1_30s_bio_12 + wc2.1_30s_bio_14 + wc2.1_30s_bio_15 + wc2.1_30s_bio_19)

#breadth comparisons
env.breadth(Pbarb_gm, bio_reduced)
env.breadth(Prug_gm, bio_reduced)
env.breadth(FGH_gm, bio_reduced)
env.breadth(J_gm, bio_reduced)
marginal.plots(J_gm, bio_pca$rasters, "PC5")
background.test(Pbarb_species, Prug_species, env = bio_reduced, type = "mx", test.type = "symmetric", nreps = 50)
identity.test(Pbarb_species, Prug_species, env = bio_reduced, type = "mx", reps = 50)
#add pairs to overlap test that you want to compare
raster.overlap(FGH_gm, J_gm)
raster.cor.matrix(bio)
raster.cor.plot(bio)
raster.breadth(J_gm)
raster.breadth(FGH_gm)
raster.breadth(Pbarb_gm)
raster.breadth(Prug_gm)
bio_pca <- raster.pca(bio, 19)
plot(bio_pca$raster, 5)

#comparison figure and statistics for each bioclim factor (change name of column as needed for each one)
FGH_Obs <- subset(Pogo_obs, ID == "FGH")
J_Obs <- subset(Pogo_obs, ID == "J")
Pbarb_Obs <- subset(Pogo_obs, ID == "barbatus")
Prug_Obs <- subset(Pogo_obs, ID == "rugosus")
b <- density(Pbarb_Obs$wc2.1_30s_bio_1)
r <- density(Prug_Obs$wc2.1_30s_bio_1)
F <- density(FGH_Obs$wc2.1_30s_bio_1)
J <- density(J_Obs$wc2.1_30s_bio_1)
plot(r, ylim = c(0,0.5), xlab = "MAT", main = "Mean annual precipitation", col = "darkgray", lwd = 3, xlim = c(0,50))
lines(b, col = "palegreen3", lwd = 3)
lines(F, col = "black", lwd = 3)
lines(J, col = "red", lwd = 3)
legend(5, 0.5, legend=c("P. rugosus", "P. barbatus", "FGH", "J"),
       col=c("darkgray", "palegreen3", "black", "red"), lty=1, cex=1, lwd = 1.5)
#testing for differences in median
Pairwise <- Pogo_obs[Pogo_obs$ID %in% c("FGH", "J"),]
wilcox.test(Pairwise$wc2.1_30s_bio_19~Pairwise$ID)
aggregate(Pogo_obs, by = list(Pogo_obs$ID), FUN = median)
