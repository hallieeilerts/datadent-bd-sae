# datadent-bd-sae

**Summary**

* Estimate nutrition indicator coverage for all districts
* Use Bayesian spatial model with covariates to avoid unstable estimates due to small sample sizes
  * Estimated prevalence in each district is influenced by its own data, as well as by those of its neighbours
  * The extent to which neighbours influence one another depends on how stable vs. uncertain the estimated effects in each district are, and on the empirical similarity among neighbouring districts.
* Besag, York, and Mollie (BYM) model in which cross-district variance is empirically partitioned into a spatial component, specified using a conditional auto-regressive prior, and a component that does not have a spatial pattern, specified using a Normal distribution
* The model borrows strength through similarity in relation to predictive covariates
  * province, percent of district population living in urban areas, average district wealth index and food security score, and proportion of women who
were illiterate
* Account for complex survey design and post-stratification sample weights in calculating prevalence

**Indicator test set**

1. Vitamin A supplementation for child
2. Iron containing supplement (multi micronutrient supplement) for pregnant women
3. Deworming medication for child
4. Breastfeeding counseling



