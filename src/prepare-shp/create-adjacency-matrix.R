################################################################################
#' @description Match GPS coordinates of clusters to BD adm2 districts
#' @return spreadsheet with columns for cluster number and district name
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(rgdal)
library(spdep)  # For spatial adjacency matrix
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
districts <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# Check the projection and transform if necessary
districts <- spTransform(districts, CRS("+proj=longlat +datum=WGS84"))

# Generate adjacency list based on Queen contiguity (adjacency via boundary or corner)
adjacency_list <- poly2nb(districts, queen = TRUE)  

# Convert spatial adjacency list to row-standardized spatial weights list
# Row-standardized matrices are typically used when performing spatial smoothing
# because they ensure that each district has a comparable weight (e.g., sum of weights equal to 1), which helps with model stability.
adjacency_wt_list <- nb2listw(adjacency_list, style = "W")

# Convert row-standardized spatial weights list to a matrix form
adjacency_matrix <- listw2mat(adjacency_wt_list)

W <- adjacency_matrix

# # Convert to row-standardized spatial weights
# # Row-standardized matrices are typically used when performing spatial smoothing
# # because they ensure that each district has a comparable weight (e.g., sum of weights equal to 1), which helps with model stability.
# row_standardized_spatial_adj_matrix <- nb2listw(adjacency_list, style = "W")
# 
# # Convert to matrix form
# row_standardized_adj_matrix <- listw2mat(row_standardized_spatial_adj_matrix)
# 
# heatmap(adj_matrix, Rowv = NA, Colv = NA, col = colorRampPalette(c("white", "blue"))(100))
# 
# 
# # Convert to spatial weights matrix
# spatial_effect_matrix_distance <- nb2listw(distance_weights, style = "W")
# 
# # Convert the spatial weights list to a matrix
# spatial_effect_matrix_mat <- listw2mat(spatial_effect_matrix)
# 
# # View the matrix
# print(spatial_effect_matrix_mat)
# 
# 
# # Convert to adjacency matrix
# W <- nb2mat(adjacency_matrix, zero.policy = TRUE)
# 
# # Check the matrix
print(dim(W))  # Should match the number of districts

# Save --------------------------------------------------------------------

write.csv(W, "./gen/prepare-shp/output/adjacency_matrix.csv", row.names = FALSE)
saveRDS(W, file = "./gen/prepare-shp/output/adjacency_matrix.rds")
