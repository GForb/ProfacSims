
cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/ProfacSims"

import delimited "Data/cont_train.csv", clear
append using "Data/cont_test_new_study.dta", gen(test_data)


* Using mixed
mixed y x* || studyid: if test_data !=1, reml
local var_u = exp([lns1_1_1]_cons)^2
local var_e = exp([lnsig_e]_cons)^2

est store A
predict fixed,xb 
predict re_train, reffects

gen total_res = y - fixed
bysort studyid: egen ml_int = mean(total_res)

bysort studyid: egen n = count(y)
gen R = `var_u'/(`var_u' + `var_e'/n)

gen blup = ml_int*R
gen blup_check = blup - re_train


regress total_res ibn.studyid, noconstant



*Using gsem
gsem (y <-  x* M1[studyid])
est store B



est restore A
predict re_train, reffects

est restore B
predict re_train_gsem, latent



est restore A
predict re_test, reffects

est restore B
predict re_test_gsem, latent

