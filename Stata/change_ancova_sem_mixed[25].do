*Comparison of models when data are complete and incomplete
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/ProfacSims/Data/odin_change.dta", clear

cap prog drop calib_plot
prog define calib_plot
args actual pred name title


su `pred'
local min = r(min)
local max = r(max)
tempvar calib_points
regress `actual' `pred'
predict `calib_points'
twoway (scatter `actual' `pred', msize(vtiny)) ///
		(scatter `calib_points' `pred', msize(tiny)) ///
		(function y = x, range(`min' `max')) ///
		(lowess  `actual' `pred'), ///
		xtitle("Predicted") ///
		ytitle("Observed") legend(order(2 4 3 )  label(2 "Calibration model") label(3 "equality") label(4 "Lowess")) name(`name', replace) title("`title'") 
end		

* Traditional change score analysis
regress change rgroup

* Traditional ANCOVA analysis
regress bdi6 bdi0 rgroup
predict xbreg, xb
predict stdpreg, stdp

calib_plot bdi6 xbreg "c_reg" "Calibraiton - Linear Regression"

* SEM change score analysis
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup)
predict change_bdi0, xb(bdi0)
hist change, normal
* SEM ANCOVA
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup), cov(F1*rgroup@0)
predict xbsem*, xb
summ xb*

calib_plot bdi0 xbsem1  "c_sem1" "Calibraiton - BDI 0 SEM"
calib_plot bdi6 xbsem2  "c_sem2" "Calibraiton - BDI 6 SEM"


regress bdi6 xbreg

gen logbdi0=log(bdi0)
gen logbdi6=log(bdi6)
regress logbdi6 logbdi0 rgroup
predict logxbr , xb



gsem (logbdi0 <- F1@1) (logbdi6 <-F1@1 rgroup)
predict logxbg*, mu



summ logxb*

calib_plot logbdi0 logxbg1  "c_gsem1" "Calibraiton - Log BDI 0 SEM"
calib_plot logbdi6 logxbg2  "c_gsem2" "Calibraiton - Log BDI 6 SEM"

graph combine c_reg c_sem1 c_sem2 c_gsem1 c_gsem2, name(combined_calibration, replace) cols(2) holes(1) 
 
* Traditional ANCOVA analysis
regress bdi6 bdi0 rgroup
predict pred_res


sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup), cov(F1*rgroup@0)
predict pred_sem*, xb(bdi6)




foreach i in 0 0.1 0.5 1 {
	preserve
	gen log_bdi0 = log(bdi0 + `i')
	gen log_bdi6 = log(bdi6 + `i')
	
	regress log_bdi6 log_bdi0 rgroup

	cap noisily sem (log_bdi0 <- F1@1) (log_bdi6 <-F1@1 rgroup), cov(F1*rgroup@0) nolog
	
	di "`i'"
	restore
}


predict F, latent
gen e_hat = bdi0 - F

gen pred_adj = bdi0 - 22.35016  - e_hat

summ pred*

* Traditional change score analysis
regress change rgroup

* Traditional ANCOVA analysis
regress bdi6 bdi0 rgroup

* SEM change score analysis
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup)

* SEM ANCOVA
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup), cov(F1*rgroup@0)

*What if bdi0 and bdi6 are not on the same scale. We have one equation for bdi0 and construct and equation
*where bdi6 is predicted only by a latent variable (here F2) and that equation has no error term (residual variance is set to 0). On the assumption that the reliability of the two measures is the same i.e. that the same proportion of variance is true score the equation for F2 has the same factor loading for F1 and the same error variance as the equation for bdi0. The coefficient estimating the coefficient for rgroup is thus estimated on the scale of bdi0.
*ANCOVA with different scales
sem (bdi0 <- F1@1) (bdi6 <- (F2,init(1))) (F2 <- F1@1 rgroup) ,  var(e.F2@b) var(e.bdi0@b) var(e.bdi6@0) cov(F1*rgroup@0)
*Change score with different scales
sem (bdi0 <- F1@1) (bdi6 <- (F2,init(1))) (F2 <- F1@1 rgroup) ,  var(e.F2@b) var(e.bdi0@b) var(e.bdi6@0)

preserve
*Setup for mixed for Gordon
reshape long bdi, i(subjid) j(time) 
gen rgroup6=cond(rgroup==1 & time==6,1,0)

tab rgroup6 

list in 1/10
*This looks like ANCOVA estimate
mixed bdi time rgroup6 || subjid: , resid(,by(time))
predict pred
su pred if time ==6
*This looks like change-score estimate
mixed bdi time rgroup rgroup6 || subjid: , resid(,by(time))

restore
* Advantages of SEM setup
* - allows missing data using full information ML
* - extends to multiple pre and post randomistion
* - extends idea of ANCOVA to non-gaussian non-identity link

* sem command methld(mlmv) assumes missing variables are conditionally normal
* Comparison of models when data some baselines are missing
replace bdi0=. if _n>250

* Traditional change score analysis
regress change rgroup if _n<=250

* Traditional ANCOVA analysis (regression in SEM will now assumemissing bdi0 are normal)
regress bdi6 bdi0 rgroup
sem (bdi6 <-bdi0 rgroup), method(mlmv)

* SEM change score analysis
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup), method(mlmv)

* SEM ANCOVA method mlmv - assumes that factor is normal and bdi0 conditional on factor is normal
sem (bdi0 <- F1@1) (bdi6 <-F1@1 rgroup), cov(F1*rgroup@0) method(mlmv)


* What about multiple predictors (using my data)

 
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_eda_1tp_train_data.dta", clear

encode studyid, gen(study)


regress y_sdq_emot_p i.study   sdq* age y_age
predict pred_regress if e(sample)

gen in_model = e(sample)

sem (sdq_emot_p <- F1@1) (y_sdq_emot_p <-F1@1 sdq_cond_p sdq_hyp_p sdq_peer_p sdq_pro_p  mcs lsac_k lsac_b age y_age)  if in_model ///
	, cov(F1*sdq_cond_p@0 F1*sdq_hyp_p@0 F1*sdq_peer_p@0 F1*sdq_pro_p@0 F1*mcs@0 F1*lsac_k@0 F1*lsac_b@0  F1*age@0 F1*y_age@0)
predict pred_sem  if in_model, xb(y_sdq_emot_p)

su pred_regress pred_sem  if in_model

* Add in sex - partially observed
regress y_sdq_emot_p i.study   sdq* age y_age sex

* For SEM:
sem (sdq_emot_p <- F1@1) (y_sdq_emot_p <-F1@1 sdq_cond_p sdq_hyp_p sdq_peer_p sdq_pro_p   lsac_k lsac_b age y_age sex), cov(F1*sdq_cond_p@0 F1*sdq_hyp_p@0 F1*sdq_peer_p@0 F1*sdq_pro_p@0 F1*sex@0 F1*lsac_k@0 F1*lsac_b@0  F1*age@0 F1*y_age@0) // drop mcs as sex is missing

* With FIML - works a charm - can include everyone. 
sem (sdq_emot_p <- F1@1 ) (y_sdq_emot_p <-F1@1 sdq_cond_p sdq_hyp_p sdq_peer_p sdq_pro_p  mcs lsac_k lsac_b age y_age sex), ///
 cov(F1*sdq_cond_p@0 F1*sdq_hyp_p@0 F1*sdq_peer_p@0 F1*sdq_pro_p@0 F1*mcs@0 F1*lsac_k@0 F1*lsac_b@0  F1*age@0 F1*y_age@0 F1*sex@0 ) ///
method(mlmv)


* Adjusting for age 'contemprously'
sem (sdq_emot_p <- F1@1 age) (y_sdq_emot_p <-F1@1 sdq_cond_p sdq_hyp_p sdq_peer_p sdq_pro_p  mcs lsac_k lsac_b  y_age sex), ///
 cov(F1*sdq_cond_p@0 F1*sdq_hyp_p@0 F1*sdq_peer_p@0 F1*sdq_pro_p@0 F1*mcs@0 F1*lsac_k@0 F1*lsac_b@0  F1*age@0 F1*y_age@0 F1*sex@0 ) ///
method(mlmv)


regress y_sdq_emot_p i.study   sdq* age y_age
est store A
* Thinking about using mixed
regress y_sdq_emot_p i.study   sdq* age y_age  i.study#c.sdq_emot_p
est store B

lrtest A B


