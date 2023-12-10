* Tricking mixed into giving out of sample predictions

*Train data
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear

mixed sdq_emot_p age_cent || ID:

predict insample, reffects

*Test data

*Load test data and predict
preserve

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic_test.dta", clear
cap noisily predict test, reffects 
su test
restore

*Append test data and predict 
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
append using  "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic_test.dta"

tab test_data train_data

mixed sdq_emot_p age_cent || ID: if train_data
cap noisily predict if_test, reffects 
bysort train_data: su if_test


*Append "missing rows", then update them

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic1000.dta", clear
append using  "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/SDQ_gen_pop/sdq_gen_pop_non_autistic_test.dta"

gen age_cent2 = age_cent
gen sdq_emot_p2 = sdq_emot_p

replace age_cent =. if test_data
replace sdq_emot_p = . if test_data

mixed sdq_emot_p age_cent || ID:

replace age_cent = age_cent2
replace sdq_emot_p =  sdq_emot_p2

predict missing_trick_test, reffects
su missing_trick_test
