import delimited "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Raw data/SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/vineland_ii.csv", clear 


tempfile temp
save  `temp'

import delimited "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Raw data/SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/ssc_core_descriptive.csv", clear 


keep individual age_at_ados ados_css sex ethnicity
merge 1:1 individual using `temp'

su age_at_ados
gen age_years = age_at_ados/12
gen fu = 1 if age_years >14 & age_years < 18
gen bl = 1 if age_years >10 & age_years < 14

*find scoring info here: https://www.ualberta.ca/community-university-partnership/media-library/community-university-partnership/resources/tools---assessment/vinelandjune-2012.pdf

gen dls_raw = domestic_raw_score + community_raw_score + personal_raw_score
scatter dls_raw dls_sum

gen dls_raw_mean =dls_raw/3
scatter dls_raw_mean dls_sum if dls_sum <100


hist communication_sum if fu ==1 , name(comm_sum, replace)
hist soc_sum if fu ==1, name(soc_sum, replace)
hist dls_raw if fu ==1, name(dls_sum, replace)

graph combine comm_sum soc_sum dls_sum, name(fu, replace)

hist communication_sum if bl ==1, name(comm_sum, replace)
hist soc_sum if bl ==1, name(soc_sum, replace)
hist dls_sum if bl ==1, name(dls_sum, replace)

graph combine comm_sum soc_sum dls_sum, name(bl, replace)

hist communication_sum if fu ==1 & communication_sum <100, name(comm_sum, replace)
hist soc_sum if fu ==1 & soc_sum <100 , name(soc_sum, replace)
hist dls_sum  if fu ==1 & dls_sum < 100, name(dls_sum, replace)

graph combine comm_sum soc_sum dls_sum, name(fu_trimmed, replace)

hist communication_sum if bl ==1 & communication_sum <100, name(comm_sum, replace)
hist soc_sum if bl ==1 & soc_sum <100 , name(soc_sum, replace)
hist dls_sum  if bl ==1 & dls_sum < 100, name(dls_sum, replace)

graph combine comm_sum soc_sum dls_sum, name(bl_trimmed, replace)


* What has clincial meaning - is it just age equivalents for the vineland?
* Do raw scores mean anything?
* Does it make any sense to model changes in age standardised scores with age?
