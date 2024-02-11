pmsampsize, type(c) rsquared(0.15) parameters(12) intercept(1) sd(1)
pmsampsize, type(c) rsquared(0.5) parameters(35) intercept(1) sd(1)
pmsampsize, type(c) rsquared(0.5) parameters(57) intercept(1) sd(1)


pmsampsize, type(c) rsquared(0.25) parameters(12) intercept(1) sd(1)
pmsampsize, type(c) rsquared(0.25) parameters(22) intercept(1) sd(1)


pmsampsize, type(c) rsquared(0.15) parameters(6) intercept(1) sd(1) mmoe(1.12)
pmsampsize, type(c) rsquared(0.25) parameters(10) intercept(1) sd(1) mmoe(1.11)

clear
set obs 100 
gen x = rnormal(0, 1)
gen y = rnormal(0,1)
gen z = rnormal(0, 1)
regress y x
predict y_hat
scatter z y_hat
regressz y_hat



    pmsampsize, type(c) rsquared(0.3) parameters(5) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.3) parameters(10) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.3) parameters(15) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.3) parameters(25) intercept(1) sd(1) 
     pmsampsize, type(c) rsquared(0.3) parameters(50) intercept(1) sd(1) 

    
    

    pmsampsize, type(c) rsquared(0.25) parameters(5) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.25) parameters(10) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.25) parameters(15) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.25) parameters(25) intercept(1) sd(1) 

    
    pmsampsize, type(c) rsquared(0.1) parameters(5) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.1) parameters(10) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.1) parameters(15) intercept(1) sd(1) 
    

    pmsampsize, type(c) rsquared(0.05) parameters(3) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.05) parameters(5) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.05) parameters(10) intercept(1) sd(1) 

        pmsampsize, type(c) rsquared(0.15) parameters(5) intercept(1) sd(1) 
    pmsampsize, type(c) rsquared(0.15) parameters(10) intercept(1) sd(1) 
        pmsampsize, type(c) rsquared(0.15) parameters(25) intercept(1) sd(1) 

        
            pmsampsize, type(c) rsquared(0.7) parameters(5) intercept(1) sd(1) 

            pmsampsize, type(c) rsquared(0.6) parameters(30) intercept(1) sd(1) 

            pmsampsize, type(c) rsquared(0.6) parameters(50) intercept(1) sd(1) 
			
			
			
* Sample sizes in SAP

* Note these are calculated assuming one study is used as a hold out study,
            pmsampsize, type(c) rsquared(0.6) parameters(32) intercept(1) sd(1) 
			pmsampsize, type(c) rsquared(0.6) parameters(33) intercept(1) sd(1) 


            pmsampsize, type(c) rsquared(0.36) parameters(27) intercept(1) sd(1) 
			pmsampsize, type(c) rsquared(0.36) parameters(28) intercept(1) sd(1) 

			
            pmsampsize, type(c) rsquared(0.25) parameters(27) intercept(1) sd(1) 
            pmsampsize, type(c) rsquared(0.25) parameters(28) intercept(1) sd(1) 
			

