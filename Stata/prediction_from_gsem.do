cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/ProfacSims"
su time
replace time = (time - r(mean))/r(sd)
su cbcl
replace cbcl = (cbcl - r(mean))/r(sd)
su cbcl2
replace cbcl2 = (cbcl2 - r(mean))/r(sd)

gen time2 = time*time
keep if id == "1_1" | id == "1_2"

import delimited "Data/cbcl_test.csv", clear
est use "Stata Models/gsem_multivariate.ster"

* Does having an additional individula in the dataset make a difference - answer no
preserve 
keep if id == "1_1" 
predict pred1 pred2
list id time pred1 pred2 cbcl cbcl2

restore

preserve 
predict pred1 pred2
list id time pred1 pred2 cbcl cbcl2

restore


drop if id == "1_2"

preserve 
keep if _n ==1
predict pred1 pred2
list id time pred1 pred2 cbcl cbcl2
restore

* Overall how do we validate model
* What are we using the model for - is it prediciton of a single timepoint or is it prediction of a trajectory
* How to get a trajectory with prediction intervals - ask george :) This is what he has done.

* What can we get from the data:
* - average trajectories conditional on predictors, in particular as function of age. Other predictors may be gender, IQ etc. These can be 
* - allow deviances from the average trajectories
* More measures of CBCL mean more information about the individual
* Expect quite high random intercept or slope variances

*This feels like growth curve modelling 101


predict pred1 pred2
predict pred_latent*, latent

gen pred_check1 = pred1 - (pred_fixed1 + pred_latent1 + time*pred_latent2)

list id time pred* cbcl cbcl2

twoway (scatter pred1 time) (scatter cbcl time)

preserve 
cap drop pred*
replace cbcl = . if _n >2
replace cbcl2 = . if _n > 2
predict pred1 pred2
predict pred_latent*, latent
list id time pred* cbcl cbcl2

restore


preserve 
cap drop pred*
replace cbcl = . if _n >4
replace cbcl2 = . if _n > 4
predict pred1 pred2
predict pred_latent*, latent
list id time pred* cbcl cbcl2

restore

preserve 
keep if id == "1_1"
keep if _n ==1
replace cbcl = . 
replace cbcl2 = .
predict pred1 pred2
list id time pred1 pred2 cbcl cbcl2

restore


preserve 
keep if id == "1_1" 
keep if _n ==1
replace cbcl = .
replace cbcl2 = .
predict pred1 pred2
list id time pred1 pred2 cbcl cbcl2

restore
