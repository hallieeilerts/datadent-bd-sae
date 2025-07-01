
################################################
# DataDENT Bangladesh nutrition intervention coverage
################################################

# Prepare shp files -------------------------------------------------------

source("./src/prepare-shp/match-clusters-to-districts.R", local = new.env())
source("./src/prepare-shp/plot-clusters.R", local = new.env())
source("./src/prepare-shp/create-adj-matrix.R", local = new.env())

# Prepare DHS data --------------------------------------------------------

source("./src/prepare-dhs/variables-kr.R", local = new.env())
source("./src/prepare-dhs/variables-ir.R", local = new.env())
source("./src/prepare-dhs/variables-hr.R", local = new.env())
source("./src/prepare-dhs/assess-sample.R", local = new.env())
source("./src/prepare-dhs/create-input-child.R", local = new.env())
source("./src/prepare-dhs/create-input-mother.R", local = new.env())
source("./src/prepare-dhs/calc-district-covar.R", local = new.env())

# Calculate direct estimates ----------------------------------------------

source("./src/calculate-direct/calc-direct-child.R", local = new.env())
source("./src/calculate-direct/calc-direct-mother.R", local = new.env())
source("./src/calculate-direct/combine-direct.R", local = new.env())
source("./src/calculate-direct/download-statcompiler.R", local = new.env())
source("./src/calculate-direct/compare-statcompiler.R", local = new.env())

# Modeling ----------------------------------------------------------------

#source("./src/model/test-model.R", local = new.env())
source("./src/model/fit-model.R", local = new.env())
source("./src/model/fit-model-fe.R", local = new.env())
source("./src/model/fit-summer.R", local = new.env())
#source("./src/model/load-fit.R", local = new.env())

# Visualizations ----------------------------------------------------------

source("./src/visualizations/facet-maps.R", local = new.env())
source("./src/visualizations/tmap.R", local = new.env())
source("./src/visualizations/uncert-int.R", local = new.env())
source("./src/visualizations/compare-summer.R", local = new.env())

# Validation --------------------------------------------------------------

source("./src/validation/aggregate-adm2-pred.R", local = new.env())
source("./src/validation/calc-agg-error.R", local = new.env())



