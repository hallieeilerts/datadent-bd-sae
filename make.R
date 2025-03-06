



# Clusters ----------------------------------------------------------------

source("./src/match-clusters-to-districts.R", local = new.env())
source("./src/plot-clusters.R", local = new.env())

# Calculate coverage indicators -------------------------------------------

source("./src/indicators/vas-ch.R", local = new.env())
source("./src/indicators/dwm-ch.R", local = new.env())

# Compile and quality check indicators ------------------------------------

source("./src/indicator-compilation/compile-indicators.R", local = new.env())
source("./src/indicator-compilation/download-statcompiler.R", local = new.env())
source("./src/indicator-compilation/compare-statcompiler.R", local = new.env())
source("./src/indicator-compilation/plot-coverage.R", local = new.env())






# Notes -------------------------------------------------------------------



# read wilson2020 closer to see if ok to extract jittered cluster locations as is
# how to calculate variance? taylor seires?

