
################################################
# DataDENT Bangladesh nutrition intervention coverage
################################################

# Prepare shp files -------------------------------------------------------

source("./src/prepare-shp/rename-adm1-regions.R", local = new.env())
source("./src/prepare-shp/match-clusters-to-districts.R", local = new.env())
source("./src/prepare-shp/plot-clusters.R", local = new.env())
source("./src/prepare-shp/create-corr-matrix.R", local = new.env())
source("./src/prepare-shp/create-adjacency-matrix.R", local = new.env())

# Prepare DHS data --------------------------------------------------------

source("./src/prepare-dhs/variables-kr.R", local = new.env())
source("./src/prepare-dhs/variables-ir.R", local = new.env())
source("./src/prepare-dhs/variables-hr.R", local = new.env())
source("./src/prepare-dhs/create-input-child.R", local = new.env())
source("./src/prepare-dhs/create-input-mother.R", local = new.env())
source("./src/prepare-dhs/create-input-district.R", local = new.env())

# Calculate direct estimates ----------------------------------------------

source("./src/calculate-direct/calc-direct-child.R", local = new.env())
source("./src/calculate-direct/calc-direct-mother.R", local = new.env())
source("./src/calculate-direct/combine-direct.R", local = new.env())
source("./src/calculate-direct/download-statcompiler.R", local = new.env())
source("./src/calculate-direct/compare-statcompiler.R", local = new.env())

# Model prep --------------------------------------------------------------

source("./src/prepare-model/check-collinearity-mother.R", local = new.env())
source("./src/prepare-model/check-collinearity-child.R", local = new.env())
source("./src/prepare-model/check-predpower-mother.R", local = new.env())
source("./src/prepare-model/check-predpower-child.R", local = new.env())
source("./src/prepare-model/test-priors-mother.R", local = new.env())

# Modeling ----------------------------------------------------------------

source("./src/model/estimate-child-vas.R", local = new.env())
source("./src/model/estimate-child-dwm.R", local = new.env())
source("./src/model/estimate-child-nt_ebf.R", local = new.env())

source("./src/model/estimate-mother-iron.R", local = new.env())
source("./src/model/estimate-mother-anc4.R", local = new.env())
source("./src/model/estimate-mother-anc1tri.R", local = new.env())

source("./src/model/predict-districts.R", local = new.env())
source("./src/model/extract-model-info.R", local = new.env())
source("./src/model/assess-fit.R", local = new.env())

source("./src/model/compare-looic.R", local = new.env())

# Visualizations ----------------------------------------------------------

#source("./src/visualizations/plot-pred.R", local = new.env())
source("./src/visualizations/plot-ind-sep.R", local = new.env())

# Notes -------------------------------------------------------------------






