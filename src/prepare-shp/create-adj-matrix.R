################################################################################
#' @description Create spatial adjacency matrix
#' @return 64x64 adjacency matrix with 0 and 1s
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(spdep)
library(sp)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# define spatial object
geo <- as_Spatial(bangladesh_2)

# create neighbors list
nb <- poly2nb(geo, queen = TRUE)

# plot districts with neighbor links
png("./gen/prepare-shp/audit/neighbors.png", width = 800, height = 800)
plot(geo, border = "black")
plot(nb, coordinates(geo), add = TRUE, col = "red", lwd = 1)
dev.off()

# define adjacency matrix
Amat <- nb2mat(nb, style = "B")
rownames(Amat) <- bangladesh_2$ADM2_EN
colnames(Amat) <- bangladesh_2$ADM2_EN

# prepare matrix for bym2
prep <- prepare_bym2(Amat)

## quality check
qc = rep(0,length(prep$n1))
for (i in 1:length(prep$n1)){
  qc[i] = Amat[prep$n1[i],prep$n2[i]]
}
mean(qc) # should be 1

# Save --------------------------------------------------------------------

saveRDS(prep, file = "./gen/prepare-shp/output/adjacency_matrix.rds")

