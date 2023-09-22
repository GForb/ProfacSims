*cd "C:\Users\k1811974\OneDrive - King's College London\PhD\R\ProfacSims\"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/ProfacSims"

import delimited "Data/cbcl.csv", clear

gen time_years = time

su time
replace time = (time - r(mean))/r(sd)
su cbcl
replace cbcl = (cbcl - r(mean))/r(sd)
su cbcl2
replace cbcl2 = (cbcl2 - r(mean))/r(sd)

gen time2 = time*time


rename cbcl cbcl1

reshape long cbcl, i(id wave) j(domain)
	
gen cbcl1 = 0
replace cbcl1 = 1 if domain ==1
gen cbcl2 = 0
replace cbcl2 = 1 if domain ==2

gen time_cbcl11 = time*cbcl1
gen time_cbcl2 = time*cbcl2
	
	
mixed cbcl i.studyid time time2 domain || id:  || domain: , residuals( , by(domain)) reml // how would you make a prediction for a new observation without 
mixed cbcl i.studyid time time2 domain || domain:  || id: , residuals( , by(domain)) reml // how would you make a prediction for a new observation without 

mixed cbcl i.studyid time time2 domain || _all: R.id  || _all: R.domain , residuals( , by(domain)) reml // does not run well



mepoisson cbcl i.studyid time time2 domain || id: || domain: , vce(robust)


mixed cbcl  cbcl1 cbcl2 time_cbcl11 time_cbcl2 , nocons  /// 
	|| studyid: ///
	|| id: cbcl1 cbcl2 , nocons cov(un)  ///
	|| time: , nocons var residuals(un, t(domain))

*Has random intercept for time

mixed cbcl i.studyid cbcl1 cbcl2 time_cbcl11 time_cbcl2 , nocons  /// 
	|| id: cbcl1 cbcl2 time_cbcl11 time_cbcl2, nocons cov(un)  ///
	|| time: , nocons  residuals(un, t(domain)) stddeviations           

	
mixed cbcl  cbcl1 cbcl2 time_cbcl11 time_cbcl2 , nocons  /// 
	|| studyid: ///
	|| id: cbcl1 cbcl2 time_cbcl11 time_cbcl2, nocons cov(un)  ///
	|| time: , nocons  residuals(un, t(domain)) stddeviations           
	
	
*Single timepoint models
su time_years
bysort studyid wave: su time_years

*study
