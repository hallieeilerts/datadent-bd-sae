
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
#source("./src/prepare-dhs/assess-sample.R", local = new.env()) # maps and tables
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
# !! rh_pnc_wm_2days/RH_PCMN_W_MOT doesn't match well with statcompiler

# Modeling ----------------------------------------------------------------

source("./src/model/explore-covar.R", local = new.env())
source("./src/model/explore-covar-importance.R", local = new.env())
#source("./src/model/test-model.R", local = new.env())
source("./src/model/fit-model.R", local = new.env())
source("./src/model/fit-model-fe.R", local = new.env())
source("./src/model/fit-summer.R", local = new.env())
#source("./src/model/load-fit.R", local = new.env())

# Validation --------------------------------------------------------------

source("./src/validation/aggregate-adm2-pred.R", local = new.env())
source("./src/validation/calc-agg-error.R", local = new.env())
source("./src/validation/viz-agg-error.R", local = new.env())

# Results -----------------------------------------------------------------

source("./src/results/gen-results.R", local = new.env())

# Visualizations ----------------------------------------------------------

source("./src/visualizations/facet-maps.R", local = new.env())
source("./src/visualizations/tmap.R", local = new.env())
source("./src/visualizations/uncert-int.R", local = new.env())
source("./src/visualizations/new-uncert.R", local = new.env())
source("./src/visualizations/compare-summer.R", local = new.env())
source("./src/visualizations/covar-outcome-tables.R", local = new.env())
source("./src/visualizations/composite-coverage.R", local = new.env())

# Next steps
# Re-run models for SUMMMER for v_var <- unique(subset(df_ind, covar_grp %in% c("ch_nut", "ch_trtmnt", "ph"))$variable), to make sure they have updated covar sets
# Re-create plots comparing all my models plus full model from SUMMER
# Aggregate estimates again incorporating the weights now included in the direct estimates file***
# Create results file for Maiga -- make wide version
# update models with any changes from Sahoko

# SEPT 1, 2025
# removing n_obs. use un_obs instead. make this change across all calc-direct files
# this un_obs matches what sahoko's n_obs is (number of observations). i need to take into account those with missing values.
