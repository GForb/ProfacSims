cap log close
log using "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Outputs/sdq_models_multivariate", replace

global model_path "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Outputs/Stata models/SDQ EDA"

*Multivariate with mixed
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent


reshape long sdq, i(ID wave) j(domain)  string 

foreach domain in emot cond pro peer hyp {
	gen ind_`domain' = 0
	replace ind_`domain'  = 1 if domain == "_`domain'_p"
}

encode domain, gen(domain_num)

mixed sdq  ind* c.age_cent#i.domain_num  i.studyid2#i.domain_num  ///
, nocons  /// 
	|| ID: ind* , nocons cov(un)  ///
	|| age_cent: , nocons var residuals(un, t(domain_num))

est store mixed_multivariate
est save "$model_path/mixed_multivariate", replace

preserve
keep if domain_num <3
mixed sdq  ind_emot    c.age_cent#i.ind_emot  i.studyid2#i.ind_emot   ///
, nocons  /// 
	|| ID:  ind_emot  ind_cond , nocons cov(un)  ///
	|| age_cent: , nocons var residuals(un, t(domain_num))
	est store mixed_bivariate
	est save "$model_path/mixed_bivariate", replace

restore
* How to get with gsem

*Using group - didn't work
//
// gsem (sdq <-  c.age_cent  i.studyid2 M1[ID]@1), group(domain_num) ginvariant(none) mean(M1[ID]@0) 
// *groups model 
// * Has the fixed part I want but no correlations in the random part
// * Is it possible to correlate variance and error terms in a grouped gsem? 



*Alternative gsem formulation with wide data - *does not run quickly (potentially does not converge)*

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent


		*Bivariate - no shared latent variable
gsem 	(sdq_cond_p <-  c.age_cent  i.studyid2 M1[ID]@1) ///
		(sdq_emot_p <-  c.age_cent  i.studyid2 M2[ID]@1), ///
		cov(e.sdq_cond_p*e.sdq_emot_p)
est save "$model_path/gsem_bivariate", replace
		
est restore mixed_bivariate
mixed



*Multivarite
gsem 	(sdq_cond_p <-  c.age_cent  i.studyid2 M1[ID]@1) ///
		(sdq_emot_p <-  c.age_cent  i.studyid2 M2[ID]@1) ///
		(sdq_hyp_p <-  c.age_cent  i.studyid2 M3[ID]@1) ///
		(sdq_peer_p <-  c.age_cent  i.studyid2 M4[ID]@1) ///
		(sdq_pro_p <-  c.age_cent  i.studyid2 M5[ID]@1), ///
		cov( ///
			e.sdq_cond_p*e.sdq_emot_p ///
			e.sdq_cond_p*e.sdq_hyp_p ///
			e.sdq_cond_p*e.sdq_peer_p ///
			e.sdq_cond_p*e.sdq_pro_p ///
			e.sdq_emot_p*e.sdq_hyp_p ///
			e.sdq_emot_p*e.sdq_peer_p ///
			e.sdq_emot_p*e.sdq_pro_p ///
			e.sdq_hyp_p*e.sdq_peer_p ///
			e.sdq_hyp_p*e.sdq_pro_p ///
			e.sdq_peer_p*e.sdq_pro_p ///
		)

est save "$model_path/gsem_multivariate", replace
		
est restore mixed_multivariate
mixed

* Mixed multivariate with random slope - bivaraite

*Multivariate with mixed
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
encode studyid, gen(studyid2)
gen age_cent2 = age_cent*age_cent


reshape long sdq, i(ID wave) j(domain)  string 

foreach domain in emot cond pro peer hyp {
	gen ind_`domain' = 0
	replace ind_`domain'  = 1 if domain == "_`domain'_p"
}

encode domain, gen(domain_num)

preserve
keep if domain_num <3
mixed sdq  ind_emot    c.age_cent#i.ind_emot  i.studyid2#i.ind_emot   ///
, nocons  /// 
	|| ID:  ind_emot  ind_cond c.age_cent#i.ind_emot c.age_cent#i.ind_cond , nocons cov(un)  ///
	|| age_cent: , nocons var residuals(un, t(domain_num))
	est store mixed_bivariate
	est save "$model_path/mixed_bivariate_slopes", replace

restore

mixed sdq  ind* c.age_cent#i.domain_num  i.studyid2#i.domain_num  ///
, nocons  /// 
	|| ID: ind*  c.age_cent#i.ind_emot c.age_cent#i.ind_cond c.age_cent#i.ind_hyp c.age_cent#i.ind_peer c.age_cent#i.ind_pro, nocons cov(un)  ///
	|| age_cent: , nocons var residuals(un, t(domain_num))
	
est save "$model_path/mixed_multivariate_slopes", replace

log close
