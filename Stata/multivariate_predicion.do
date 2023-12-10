* Load model
global model_path "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Outputs/Stata models/SDQ EDA"

est use "$model_path/gsem_multivariate"


*Obtaining predictions from a gsem with random intercept only, data available for a number of waves
cap prog drop my_predict_multivariate
prog define my_predict_multivariate
syntax namelist, predictor_waves(integer)
	preserve
	keep if relative_wave <=0  & relative_wave >=  1- `predictor_waves' 
	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar random_part1 random_part2 random_part3 random_part4 random_part5 
	di "predicting random"
	
	local outcomes = "`e(eqnames)'"
    foreach out in `outcomes' {
		tempvar rand_`out' fixed_`out'
		local rand_vars `rand_vars' `rand_`out''
		local fixed_vars `fixed_vars' `fixed_`out''
    }

	
	tempvar
	predict `rand_vars' , latent
	tempfile tempfile
	
	keep if relative_wave ==0

	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`rand_vars')
	drop _merge

	di "predicting fixed"
	predict `fixed_vars', fixedonly
	
	foreach out in `outcomes' {
		gen `namelist'_`out' = `rand_`out'' + `fixed_`out''
	}
	

end



*Load test data
*load test data
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic_test.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent


forvalues i = 1 (1) 4 {
	timer clear `i'
	timer on `i'
	my_predict_multivariate pred`i', predictor_waves(`i')
	
	timer off `i'
	
	local outcomes "`e(eqnames)'"
	foreach out in `outcomes' {
		gen res`i'_`out' = pred`i'_`out' - `out'
		gen sq_res`i'_`out' = res`i'_`out'^2
	}
	
}
timer list



su sq_res*

su sq_res*sdq_emot_p if relative_wave ==1

save est use "$model_path/multivariate_predictions"
