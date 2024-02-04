cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/ProfacSims/Results"

use int_pred_corr2.dta, clear

xtset studyid

regress y x1 i.studyid
est store fixed
mixed y x1 || studyid:, reml
est store random 
xtreg y x1, be

hausman fixed random, equations(1:1)



*fixed effect is 0.41
*pooled is 0.45
*between is 1.54
