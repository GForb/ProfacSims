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

	
*Single timepoint models
su time_years
bysort studyid wave: su time_years

gen baseline =.
replace baseline = 1 if (studyid ==1 & wave == 4) | ///
						(studyid ==2 & wave == 2) | ///
						(studyid ==3 & wave == 2) | ///
						(studyid ==4 &  time_years <14 & time_years >10 & wave != 3) | ///
						(studyid ==5 &  time_years <15 & time_years >10 & wave ==1) | ///
						(studyid ==6 &  time_years <15 & time_years >10 & wave ==1) | ///
						(studyid ==7 &  time_years <15 & time_years >10 & wave ==1) 

gen followup = .
replace followup = 1 if (studyid ==1 & wave == 5) | ///
						(studyid ==2 & wave == 3) | ///
						(studyid ==3 & wave == 3) | ///
						(studyid ==4 &  time_years > 14 & time_years <18 & wave != 1) | ///
						(studyid ==5 &  time_years >14 & time_years <18 & wave !=1) | ///
						(studyid ==6 &  time_years >14 & time_years <18 & wave !=1) | ///
						(studyid ==7 &  time_years >14 & time_years<18 & wave !=1) 
						
su time_year if baseline ==1
su time_year if followup ==1

count if baseline == 1 & followup ==1



bysort studyid : su time_years if baseline ==1
bysort studyid : su time_years if followup ==1

keep if baseline ==1 | followup ==1

gen assessment = .
replace assessment = 1 if followup ==1
replace assessment = 0 if baseline ==1 
tab assessment

bysort id assessment: egen max_tp = max(time_years)
keep if time_years == max_tp


rename cbcl2 cbcl_ext
rename cbcl cbcl_int
keep cbcl_int cbcl_ext time_years id assessment studyid
reshape wide cbcl_int cbcl_ext time_years, i(id) j(assessment)

gen fu = time_years1 - time_years0
 bysort studyid: su fu

keep if fu >1 & fu < 5
regress cbcl_int1 cbcl_int0

regress cbcl_int1 cbcl_int0 time_years0

predict res, res

scatter res time_years1
scatter res fu

regress cbcl_int0 time_years0
predict cbcl_int0_age_res, res

regress cbcl_int1 cbcl_int0 time_years0
regress cbcl_int1 cbcl_int0_age_res

regress cbcl_int1 cbcl_int0 time_years0 i.studyid
regress cbcl_int1 cbcl_int0_age_res i.studyid
