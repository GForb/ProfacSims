import delimited "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Raw data/SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/adi_r.csv", clear 


tempfile adi_temp

save `adi_temp', replace

import delimited "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Raw data/SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/vineland_ii.csv", clear 


merge 1:1 individual using `adi_temp'

su soc_sum, detail

hist soc_sum
tab q49_play_peers_current

replace q49_play_peers_current =. if q49_play_peers_current >3
replace q65_friendships_current =. if q65_friendships_current >3


corr  soc_sum q49_play_peers_current
gen soc_sum_trimmed = soc_sum 
repalce soc_sum_trimmed = . of 

polychoric soc_sum q49_play_peers_current 
polychoric soc_sum  q65_friendships_current

 spearman soc_sum q49_play_peers_current q65_friendships_current

  tab q65_friendships_current
