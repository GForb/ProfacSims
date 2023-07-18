cd "C:\Users\k1811974\OneDrive - King's College London\PhD\R\ProfacSims\"

import delimited "Data\cbcl.csv", clear
	
su time
replace time = (time - r(mean))/r(sd)
su cbcl
replace cbcl = (cbcl - r(mean))/r(sd)
su cbcl2
replace cbcl2 = (cbcl2 - r(mean))/r(sd)

gen time2 = time*time

twoway (scatter cbcl time), name(cbcl_time)
twoway (scatter cbcl cbcl2), name(cbcl_cbcl2)

*Do mixed models run
mixed cbcl i.studyid time || id: time, cov(unstructured)
mixed cbcl i.studyid time || id: time, cov(unstructured) reml

mixed cbcl2 i.studyid time || id: time, cov(unstructured)

*Using GSEM
gsem (cbcl <-  i.studyid time M1[id] c.time#M2[id]@1), var(M2[id]@0.01)   iterate(20)  
mat B = e(b)
gsem (cbcl <-  i.studyid time M1[id] c.time#M2[id]@1),  iterate(20)  from(B)


gsem (cbcl2 <-  i.studyid time M1[id] c.time#M2[id]),  iterate(50)   var(M2[id]@0.01) 
mat A = e(b)
gsem (cbcl2 <-  i.studyid time M1[id] c.time#M2[id]), from(A) iterate(50)  

*Using merlin
mixed cbcl2 studyid time || id: time, cov(unstructured)

 merlin (cbcl time studyid M1[id]@1 time#M2[id]@1 , family(gaussian))
 merlin (cbcl2 time studyid M1[id]@1 time#M2[id]@1 , family(gaussian))
 
 merlin (cbcl time studyid M1[id]@1 time#M2[id]@1 , family(gaussian)) ///
		(cbcl2 time studyid M3[id]@1 time#M4[id]@1 , family(gaussian)), ///
		covariance(unstructured)
		
 merlin (cbcl time studyid M1[id]@1 time#M2[id]@1 , family(gaussian)) ///
		(cbcl2 time studyid M3[id]@1 time#M4[id]@1 , family(gaussian))

*Joint model with constraints

gsem (cbcl <-  i.studyid time M1[id]@1 c.time#M2[id]@1) ///
	(cbcl2 <-  i.studyid time N1[id]@1 c.time#N2[id]@1), iterate(10) intpoints(15)

est save "Stata Models/gsem_multivariate_full_no_con"
mat A = e(b)
gsem (cbcl <-  i.studyid time M1[id]@1 c.time#M2[id]@1) ///
	(cbcl2 <-  i.studyid time N1[id]@1 c.time#N2[id]@1), iterate(10) intpoints(15) from(A)
est save "Stata Models/gsem_multivariate"

	
	gsem (cbcl <-  i.studyid time M1[id]@1 c.time#M2[id]@1) ///
	(cbcl2 <-  i.studyid time N1[id]@1 c.time#N2[id]@1)

gsem (cbcl <-  i.studyid time M1[id]@1 c.time#M2[id]@1) ///
	(cbcl2 <-  i.studyid time N1[id]@1 c.time#N2[id]@1) , ///
	var(M1[id]@0.01)  var(N1[id]@0.01) var(M2[id]@0.01)  var(N2[id]@0.01)  ///
	cov(M1[id]*M1[id]@0.01) ///
	cov(M1[id]*N2[id]@0.01) ///
	cov(M1[id]*N1[id]@0.01) ///
	cov(M1[id]*N2[id]@0.01) ///
	cov(M2[id]*N2[id]@0.01) ///
	iterate(1)  intpoints(15)


	
**************** Using mixed output for gsem starting values (limited success)***********************
	
	
mixed cbcl i.studyid time || id: time, cov(unstructured)
mat A = e(b)

mat C = B
mat C[1,1] = A[1, 1..8]
forvalues i = 1 (1) 8 {
	mat C[1,`i'] = A[1,`i']
}
mat C[1,11] = A[1,9]
mat C[1,12] = exp(A[1, 11])^2
mat C[1,13] = exp(A[1, 10])^2
mat C[1,14] =  tanh(A[1,12])*exp(A[1,11]+A[1,10]) // see  https://stats.oarc.ucla.edu/stata/faq/how-can-i-access-the-random-effects-after-xtmixed-using-_diparm/ for extracting from mixed

mat C[1,15] = exp(A[1, 13])^2

mat list C
mat list D

gsem (cbcl <-  i.studyid time M1[id] c.time#M2[id]), from(C) iterate(50) 



************* Using mixed output for gsem starting values - joint model **************************************



 mat GSEM =  e(b)

mat list A

gsem (cbcl <-  i.studyid time M1[id] c.time#M2[id]@1), var(M2[id]@0.01)   iterate(20)  
mat B = e(b)
gsem (cbcl <-  i.studyid time M1[id] c.time#M2[id]@1),  iterate(20)  from(B)
mat G1 = e(b)


gsem (cbcl2 <-  i.studyid time M1[id] c.time#M2[id]),  iterate(50)   var(M2[id]@0.01) 
mat A = e(b)
gsem (cbcl2 <-  i.studyid time M1[id] c.time#M2[id]), from(A) iterate(50) 

mat G2 = e(b)



mat list GSEM
mat GSEM[1,1] = G1[1, 1..11]
mat GSEM[1,12] = G2[1, 1..11]
mat GSEM[1,23] = G1[1, 12..13]
mat GSEM[1,25] = G2[1, 12..13]

gsem (cbcl <-  i.studyid time M1[id]@1 c.time#M2[id]@1) ///
	(cbcl2 <-  i.studyid time N1[id]@1 c.time#N2[id]@1) , ///
	var(M1[id]@0.01)  var(N1[id]@0.01) var(M2[id]@0.01)  var(N2[id]@0.01)  ///
	cov(M1[id]*M1[id]@0.01) ///
	cov(M1[id]*N2[id]@0.01) ///
	cov(M1[id]*N1[id]@0.01) ///
	cov(M1[id]*N2[id]@0.01) ///
	cov(M2[id]*N2[id]@0.01) ///
	iterate(1)  from(GSEM)
