################################################################################
#' @description Match GPS coordinates of clusters to BD adm2 districts
#' @return spreadsheet with columns for cluster number and district name
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(rgdal)
library(spdep)  # For spatial adjacency matrix
library(Matrix)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
shp <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# Check the projection and transform if necessary
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

# Create neighbors list
nb <- poly2nb(shp)  

# Create adjacency matrix (binary)
W <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Convert to sparse symmetric matrix
adj_matrix <- forceSymmetric(Matrix(W, sparse = TRUE))

# Convert it to a dense matrix if needed
#adj_matrix <- as.matrix(adj_matrix) 

# Plot districts with neigbor links
# png("./gen/prepare-shp/audit/neighbors.png", width = 800, height = 800)
# plot(shp, main = "Admin2 Districts with Neighbors")
# plot(nb, coordinates(shp), add = TRUE, col = "blue")
# dev.off()

# Save --------------------------------------------------------------------

saveRDS(adj_matrix, file = "./gen/prepare-shp/output/adjacency_b_matrix.rds")

