import delimited "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Raw data/SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/cbcl_6_18.csv", clear

replace internalizing_problems_total = . if internalizing_problems_total > 500

* Variables of interest

foreach var of varlist ///
 externalizing_problems_total ///
 internalizing_problems_total ///
 somatic_complaints_total ///
 conduct_problems_total ///
 affective_problems_total ///
 anxiety_problems_total ///
 add_adhd_total  {
	 hist `var', name(`var', replace)
}


corr externalizing_problems_total ///
 internalizing_problems_total ///
 somatic_complaints_total ///
 conduct_problems_total ///
 affective_problems_total ///
 anxiety_problems_total ///
 add_adhd_total
