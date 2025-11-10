
################################################
# DataDENT Bangladesh nutrition intervention coverage
################################################

# Prepare shp files -------------------------------------------------------

source("./src/prepare-shp/match-clusters-to-districts.R", local = new.env())
source("./src/prepare-shp/match-clusters-to-districts-2014.R", local = new.env())
source("./src/prepare-shp/match-clusters-to-districts-2017.R", local = new.env())
source("./src/prepare-shp/plot-clusters.R", local = new.env())
source("./src/prepare-shp/create-adj-matrix.R", local = new.env())

# Prepare DHS data --------------------------------------------------------

source("./src/prepare-dhs/variables-kr.R", local = new.env())
source("./src/prepare-dhs/variables-kr-2017.R", local = new.env())
source("./src/prepare-dhs/variables-ir.R", local = new.env())
source("./src/prepare-dhs/variables-ir-2014.R", local = new.env())
source("./src/prepare-dhs/variables-ir-2017.R", local = new.env())
source("./src/prepare-dhs/variables-hr.R", local = new.env())
source("./src/prepare-dhs/variables-hr-2014.R", local = new.env())
source("./src/prepare-dhs/variables-hr-2017.R", local = new.env())
source("./src/prepare-dhs/variables-br.R", local = new.env())

source("./src/prepare-dhs/assess-sample.R", local = new.env())

source("./src/prepare-dhs/create-input-child.R", local = new.env())
source("./src/prepare-dhs/create-input-birth.R", local = new.env())
source("./src/prepare-dhs/create-input-mother.R", local = new.env())
source("./src/prepare-dhs/create-input-household.R", local = new.env())
source("./src/prepare-dhs/calc-district-covar.R", local = new.env())

# Calculate direct estimates ----------------------------------------------

source("./src/calculate-direct/calc-direct-child.R", local = new.env())
source("./src/calculate-direct/calc-direct-birth.R", local = new.env())
source("./src/calculate-direct/calc-direct-mother.R", local = new.env())
source("./src/calculate-direct/calc-direct-household.R", local = new.env())
source("./src/calculate-direct/combine-direct.R", local = new.env())
source("./src/calculate-direct/download-statcompiler.R", local = new.env())
source("./src/calculate-direct/compare-statcompiler.R", local = new.env())

# Modeling ----------------------------------------------------------------

source("./src/model/explore-covar.R", local = new.env())
source("./src/model/explore-covar-importance.R", local = new.env())
source("./src/model/fit-model.R", local = new.env())
source("./src/model/fit-model-fe.R", local = new.env())
source("./src/model/fit-summer.R", local = new.env())

# Validation --------------------------------------------------------------

source("./src/validation/aggregate-adm2-pred.R", local = new.env())
source("./src/validation/calc-agg-error.R", local = new.env())
source("./src/validation/viz-agg-error.R", local = new.env())

# Results -----------------------------------------------------------------

source("./src/results/gen-results.R", local = new.env())

# Visualizations ----------------------------------------------------------

source("./src/visualizations/facet-maps.R", local = new.env())
source("./src/visualizations/figures-from-results.R", local = new.env())
source("./src/visualizations/tmap.R", local = new.env())
source("./src/visualizations/uncert-int.R", local = new.env())
source("./src/visualizations/compare-summer.R", local = new.env())
source("./src/visualizations/covar-outcome-tables.R", local = new.env())
source("./src/visualizations/composite-coverage.R", local = new.env())

