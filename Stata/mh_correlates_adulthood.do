use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/Analysis/SNAP adult outcomes/Stata/analysis_SNAP_predictors/Derived data/outcomes_23yo.dta", clear

*SNAP
corr sdq_emotion_py23 bdi_tot_py23

*EDX

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/Programming/autism_cohort/Data/autism_cohort_analysis.dta"

corr y4 y5 y6 y7 y8 y9
