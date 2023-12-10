use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/Analysis/SNAP adult outcomes/Stata/analysis_SNAP_predictors/Raw data/Trajectory paper demographics.dta", clear


foreach domain in  vineland_sd vineland_dlsd vineland_cd {
	replace `domain'_ae = `domain'_ae/12
	drop if `domain'_ae > 18
	hist `domain'_ae, name(`domain'_ae, replace) width(1)
	hist `domain'_raw, name(`domain'_raw, replace)

	graph combine `domain'_ae `domain'_raw, name(`domain', replace)

}

hist vineland_cd_ae if vineland_cd_ae < 2
