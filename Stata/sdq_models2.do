cap log close
log using "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Outputs/sdq_models", replace

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent


** Data summaries

su sdq*

su base*

corr sdq*

corr base_sdq*


**** Fitting models

	**** Single timepoint model
regress sdq_emot_p i.studyid2  age_cent  base_sdq_emot_p   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age_cent if relative_wave ==1
est store reg


**** Multi timepoint models

gsem (sdq_emot_p <-  i.studyid2 age_cent base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p M1[ID]@1) 
est store gsem

*Qadratic age
gsem (sdq_emot_p <-  i.studyid2  age_cent age_cent2    base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p  M1[ID]@1)
est store gsem_age2

timer on 1
*Random intercept for age

mixed sdq_emot_p i.studyid2  age_cent     base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p || ID: age_cent, cov(unstructured)
gsem (sdq_emot_p <-  i.studyid2  age_cent     base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p  M1[ID]@1 c.age_cent#M2[ID]@1)
est store gsem_rand_age
timer off 1

timer on 2
*Random intercept for age, random sutdy intercept
gsem (sdq_emot_p <-    age_cent     base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p N[studyid2]@1  M1[studyid2>ID]@1 c.age_cent#M2[studyid2>ID]@1)
est store gsem_rand_age_rand_study
timer off 2

timer list

*Predictions

*Obtaining predictions from a gsem with random intercept only, data available for a number of waves
cap prog drop my_predict
prog define my_predict
syntax namelist, predictor_waves(integer)
	preserve
	keep if relative_wave <=0  & relative_wave >=  1- `predictor_waves' 
	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar random_part 
	predict `random_part' , latent
	tempfile tempfile
	codebook `random_part'
	keep if relative_wave ==0
	count if sdq_emot_p !=.
	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`random_part')
	drop _merge
	tempvar fixed
	predict `fixed', fixedonly
	gen `namelist' = `fixed' + `random_part'
end

*Obtaining predictions from a gsem with random intercept and slope only, data available for a number of waves

cap prog drop my_predict_rand_slope
prog define my_predict_rand_slope
syntax namelist, predictor_waves(integer)
	preserve
	keep if relative_wave <=0  & relative_wave >=  1- `predictor_waves' 
	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar rand_int rand_slope
	predict `rand_int', latent(M1[ID])
	predict `rand_slope', latent(M2[ID])
	tempfile tempfile
	keep if relative_wave ==0 // needs to be upgraded with some sort of pickone code so it selects people with different relative waves
	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`rand_int' `rand_slope')
	drop _merge
	tempvar fixed
	predict `fixed', fixedonly
	gen `namelist' = `fixed' + `rand_int' + age_cent*`rand_slope'
end



*load test data
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic_test.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent

est restore reg
predict pred_stp
gen res_stp = pred_stp - sdq_emot_p
gen sq_res_stp = res_stp^2
* Obtaining predicitons from gsem with differing numbers of waves
est restore gsem

forvalues i = 1 (1) 4 {
	qui my_predict pred`i', predictor_waves(`i')
	gen res`i' = pred`i' - sdq_emot_p
	gen sq_res`i' = res`i'^2
}

est restore gsem_age2
forvalues i = 1 (1) 4 {
	qui my_predict pred_sq`i', predictor_waves(`i')
	gen res_sq`i' = pred_sq`i' - sdq_emot_p
	gen sq_res_agesq`i' = res_sq`i'^2
}

est restore gsem_rand_age
forvalues i = 1 (1) 4 {
	qui my_predict_rand_slope pred_rand_slp`i', predictor_waves(`i')
	gen res_rand_slp`i' = pred_rand_slp`i' - sdq_emot_p
	gen sq_res_rand_slp`i' = res_rand_slp`i'^2
}

qui gsem (sdq_emot_p <-  i.studyid2  age_cent base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p  M1[ID]@1), iterate(1) // restricting to sample with complete data
keep if e(sample)


*MSE

su sq_res* if relative_wave == 1


log close



*Calibration plots
//
// cap prog drop calib_plot
// prog define calib_plot
// args actual pred name title
//
//
// su `pred'
// local min = r(min)
// local max = r(max)
// tempvar calib_points
// regress `actual' `pred'
// predict `calib_points'
// twoway (scatter `actual' `pred', msize(vtiny)) ///
// 		(scatter `calib_points' `pred', msize(tiny)) ///
// 		(function y = x, range(`min' `max')) ///Î
// 		(lowess  `actual' `pred'), ///
// 		xtitle("Predicted") ///
// 		ytitle("Observed") legend(order(2 4 3 )  label(2 "Calibration model") label(3 "equality") label(4 "Lowess")) name(`name', replace) title("`title'") 
// end		
//
// keep if relative_wave == 1
//
//
// forvalues i = 1 (1) 4 {
// 	calib_plot sdq_emot_p pred`i' "calib`i'" "calib`i'"
//
// }
//
// forvalues i = 1 (1) 4 {
// 	calib_plot sdq_emot_p pred_sq`i' "calib_sq`i'" "calib_sq`i'"
//
// }
//
// calib_plot sdq_emot_p pred_stp "stp" "Single timepoint" 

