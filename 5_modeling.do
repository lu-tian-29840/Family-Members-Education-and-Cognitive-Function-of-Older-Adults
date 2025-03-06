//Program: rand_hrs_02
//Project: effect of adult children's education on parental cognitions
//Task: Create variables and analysis
//Author:lutian 

version     16.1
clear       all
set         linesize 80
matrix drop _all


//    #1
//    Open dataset
use  analysis, clear
sort rahhidpn


  
** Get the summary of all variables
	
sum 	cognition edmaxcat                                  /// kid edu
				  pedmaxcat                                /// parental edu
				  female  foreign   hhchild   birthyrC    /// R demo
				  racecat not_con_marr                   /// R demo 
	              redulhs reduhs redusc reducol         /// R edu
                  spedlhs spedhs spedsc spedcol   /// Spouse  edu
				  hitot                                      ///  Household wealth  
				  hibpe     diabe      cancre   lunge        /// R health conditions
	              hearte    strok      cesd              /// R health conditions	  
				  smokev    vigex      drink 
			  

				  				  
//   #1
//   Analysis using OLS for the role parental edu--in paper, add HB one by one (method 2)

 
     *** OLS model fit (p-edu-cognition)
	 local pEdu     i.pedmaxcat // model 1
	 
	 local rDemo	i.female       i.foreign hhchild birthyrC /// model 2
					i.not_con_marr i.black    i.hisp i.other   
					
     local hSes		i.reduhs  i.redusc    i.reducol           /// model 3
					i.spedhs  i.spedsc    i.spedcol           /// 
					hitot                                     
					
	 local rHealth	i.hibpe   i.diabe    i.cancre   i.lunge /// model 4
					i.hearte  i.strok      cesd 
					
	 local hb 		i.smokev     i.vigex  i.drink		 // model 5
	 
	    * Model 1 
		mi estimate, post: reg cognition `pEdu' [pweight=ipw]
			eststo Model1a
			outreg2 using regResults1, replace word dec(3)
			
		* Model 2
		mi estimate, post: reg cognition `pEdu' `rDemo'  [pweight=ipw]
			eststo Model2a
			outreg2 using regResults1, append word dec(3)
	   
		* Model 3, Model 2 + household SES
		mi estimate, post: reg cognition `pEdu' `rDemo' `hSes'  [pweight=ipw]
			eststo Model3a
			outreg2 using regResults1, append word dec(3)                                   
      
	  * Model 4, Model 3 + R health conditions 
		mi estimate, post: reg cognition `pEdu' `rDemo' `hSes' `rHealth' ///
							   [pweight=ipw]
		eststo Model4a
		outreg2 using regResults1, append word dec(3)    

				  
	  * Model 5, Model 4 + R health behabiors
		mi estimate, post: reg cognition `pEdu' `rDemo' `hSes' `rHealth' `hb' ///
							   [pweight=ipw]
		eststo Model5a
	    outreg2 using regResults1, append word dec(3)

	 
 
*** Method 2, add HB one by one
****************************** Table A1 ******************************

 local pEdu     i.pedmaxcat 
 
 local controls    ///                          
				   i.female       i.foreign hhchild birthyrC /// 
				   i.not_con_marr i.black    i.hisp i.other       /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd         
 local hb 		i.smokev     i.vigex  i.drink			   
				   
 	
		** regression on cognition by parental edu, while adding HB one by one

		   mi estimate, post: reg cognition `pEdu'  `controls'  [pweight=ipw]         
		   eststo mX1
		   outreg2 using regResults2, replace excel dec(3)
			 
		   mi estimate, post: reg cognition `pEdu'  `controls' smokev  [pweight=ipw]
		   eststo mX2
			 outreg2 using regResults2, append excel dec(3)
		 
		   mi estimate, post: reg cognition `pEdu'  `controls' vigex   [pweight=ipw]
		   eststo mX3
		   outreg2 using regResults2, append excel dec(3) 
			 
		   mi estimate, post: reg cognition `pEdu'  `controls' drink   [pweight=ipw]
		   eststo mX4
		   outreg2 using regResults2, append excel dec(3)   
		   
		   mi estimate, post: reg cognition `pEdu'  `controls' `hb' [pweight=ipw]
		   eststo mX5                                    
		   outreg2 using regResults2, append excel dec(3)
		   

   
   
   

 //  #2
 //  Post-estimation
				mi test 1.smokev 1.vigex 1.drink
				* collectively 3 health behaviors have a statistically sig. effect on DV
				
	* suest     --> run line 130 - 171 at once. 
	local pEdu     i.pedmaxcat 
				 
	local controls    ///                          
					i.female       i.foreign hhchild birthyrC /// 
					i.not_con_marr i.black    i.hisp i.other  /// 
					i.reduhs  i.redusc    i.reducol           ///     
					i.spedhs  i.spedsc    i.spedcol           /// 
					hitot                                     ///  
					i.hibpe   i.diabe    i.cancre   i.lunge       /// 
					i.hearte  i.strok      cesd         
	local hb 		i.smokev     i.vigex  i.drink	
				  
			capture program drop mysuest
			program define mysuest, eclass properties(mi)
				version 16.1
				args model1 model2 model3 model4 model5
				quietly `model1' [iweight=ipw]
				estimates store est1
				quietly `model2' [iweight=ipw]
				estimates store est2
				quietly `model3' [iweight=ipw]
				estimates store est3
				quietly `model4' [iweight=ipw]
				estimates store est4
				quietly `model5' [iweight=ipw]
				estimates store est5
				suest est1 est2 est3 est4 est5
				estimates drop est1 est2 est3 est4 est5
				ereturn local title "Seemingly unrelated estimation"
			end

			mi estimate, dots post: mysuest ///
				"reg cognition `pEdu' `controls' " ///
				"reg cognition `pEdu' `controls' smokev " ///
				"reg cognition `pEdu' `controls' vigex " ///
				"reg cognition `pEdu' `controls' drink " ///
				"reg cognition `pEdu' `controls' `hb' "

				lincom _b[est1_mean:2.pedmaxcat] - _b[est5_mean:2.pedmaxcat]
				* .010748 p = 0.034
				lincom _b[est1_mean:3.pedmaxcat] - _b[est5_mean:3.pedmaxcat]
				* .0157148 p = 0.024 
				lincom _b[est1_mean:4.pedmaxcat] - _b[est5_mean:4.pedmaxcat]
				* .006238 p = 0.235

 
 
  //  #3
  //  OLS for k-edu
 
    *** OLS model  (k-edu-cognition)
     local kEdu     i.edmaxcat // model 1
	 
	 local rDemo	i.female       i.foreign hhchild birthyrC /// model 2
					i.not_con_marr i.black    i.hisp i.other   
					
     local hSes		i.reduhs  i.redusc    i.reducol           /// model 3
					i.spedhs  i.spedsc    i.spedcol           /// 
					hitot                                     
					
	 local rHealth	i.hibpe    i.diabe      i.cancre   i.lunge /// model 4
					hearte    strok      cesd 
					
	  local hb 		i.smokev     i.vigex  i.drink		 // model 5

	 * Model 1 
		mi estimate, post: reg cognition `kEdu' [pweight=ipw]
			eststo Model1b
			outreg2 using regResults3, replace word dec(3)
			
		* Model 2
		mi estimate, post: reg cognition `kEdu' `rDemo'  [pweight=ipw]
			eststo Model2b
			outreg2 using regResults3, append word dec(3)
	   
		* Model 3, Model 2 + household SES
		mi estimate, post: reg cognition `kEdu' `rDemo' `hSes'  [pweight=ipw]
			eststo Model3b
			outreg2 using regResults3, append word dec(3)                                   
      
	  * Model 4, Model 3 + R health conditions 
		mi estimate, post: reg cognition `kEdu' `rDemo' `hSes' `rHealth' ///
							   [pweight=ipw]
		eststo Model4b
		outreg2 using regResults3, append word dec(3)    

				  
	  * Model 5, Model 4 + R health behabiors
		mi estimate, post: reg cognition `kEdu' `rDemo' `hSes' `rHealth' `hb' ///
							   [pweight=ipw]
		eststo Model5b
	    outreg2 using regResults3, append word dec(3)
	
 
 *** Method 2: regression on cognition by adult children edu, while adding HB one by one; 
****************************** Table A2 ******************************
 
local kEdu     i.edmaxcat 
 
local controls    ///                          
				   i.female       i.foreign hhchild birthyrC /// 
				   i.not_con_marr i.black    i.hisp i.other       /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd         
local hb 		i.smokev     i.vigex  i.drink			   
				   
 	
		** regression on cognition by parental edu, while adding HB one by one

		   mi estimate, post: reg cognition `kEdu'  `controls'  [pweight=ipw]         
		   eststo mY1
		   outreg2 using regResults4, replace excel dec(3)
			 
		   mi estimate, post: reg cognition `kEdu'  `controls' smokev  [pweight=ipw]
		   eststo mY2
			 outreg2 using regResults4, append excel dec(3)
		 
		   mi estimate, post: reg cognition `kEdu'  `controls' vigex   [pweight=ipw]
		   eststo mY3
		   outreg2 using regResults4, append excel dec(3) 
			 
		   mi estimate, post: reg cognition `kEdu'  `controls' drink   [pweight=ipw]
		   eststo mY4
		   outreg2 using regResults4, append excel dec(3)   
		   
		   mi estimate, post: reg cognition `kEdu'  `controls' `hb' [pweight=ipw]
		   eststo mY5                                    
		   outreg2 using regResults4, append excel dec(3)

	
  //  # 4
  //  Post estimation -  model difference for OLS of k-edu-cognition
			local kEdu     i.edmaxcat 
				 
			local controls    ///                          
								   i.female       i.foreign hhchild birthyrC /// 
								   i.not_con_marr i.black    i.hisp i.other  /// 
								   i.reduhs  i.redusc    i.reducol           ///     
								   i.spedhs  i.spedsc    i.spedcol           /// 
								   hitot                                     ///  
								   hibpe     diabe      cancre   lunge       /// 
								   hearte    strok      cesd         
			local hb 		i.smokev     i.vigex  i.drink	
				  
			capture program drop mysuest
			program define mysuest, eclass properties(mi)
				version 16.1
				args mod1 mod2 mod3 mod4 mod5
				quietly `mod1' [iweight=ipw]
				estimates store est1a
				quietly `mod2' [iweight=ipw]
				estimates store est2a
				quietly `mod3' [iweight=ipw]
				estimates store est3a
				quietly `mod4' [iweight=ipw]
				estimates store est4a
				quietly `mod5' [iweight=ipw]
				estimates store est5a
				suest est1a est2a est3a est4a est5a
				estimates drop est1a est2a est3a est4a est5a
				ereturn local title "Seemingly unrelated estimation"
			end

			mi estimate, dots post: mysuest ///
				"reg cognition `kEdu' `controls' " ///
				"reg cognition `kEdu' `controls' smokev " ///
				"reg cognition `kEdu' `controls' vigex " ///
				"reg cognition `kEdu' `controls' drink " ///
				"reg cognition `kEdu' `controls' `hb' "

				lincom _b[est1a_mean:2.edmaxcat] - _b[est5a_mean:2.edmaxcat]
			    * .0205086  0.134 
				lincom _b[est2a_mean:3.edmaxcat] - _b[est4a_mean:3.edmaxcat]
				* .0369181  0.017
				lincom _b[est3a_mean:4.edmaxcat] - _b[est5a_mean:4.edmaxcat]
				* .0342054  0.040
  	
	 	 	  
  //  # 5
  //  Test difference between p-edu and k-edu in terms of multigenerational education 
       
	   
			local pEdu     i.pedmaxcat 
			local kEdu     i.edmaxcat 
			 
			local controls    ///                          
							   i.female       i.foreign hhchild birthyrC /// 
							   i.not_con_marr i.black    i.hisp i.other       /// 
							   i.reduhs  i.redusc    i.reducol           ///     
							   i.spedhs  i.spedsc    i.spedcol           /// 
							   hitot                                     ///  
							   hibpe     diabe      cancre   lunge       /// 
							   hearte    strok      cesd         
			local hb 		i.smokev     i.vigex  i.drink	

			capture program drop mysuest
			program define mysuest, eclass properties(mi)
				version 16.1
				args model1 model2 model3 model4 model5 model6 model7 model8 model9 model10
				* Run models for kEdu (children's education)
				quietly `model1' [iweight=ipw]
				estimates store est1
				quietly `model2' [iweight=ipw]
				estimates store est2
				quietly `model3' [iweight=ipw]
				estimates store est3
				quietly `model4' [iweight=ipw]
				estimates store est4
				quietly `model5' [iweight=ipw]
				estimates store est5

				* Run models for pEdu (parental education)
				quietly `model6' [iweight=ipw]
				estimates store est6
				quietly `model7' [iweight=ipw]
				estimates store est7
				quietly `model8' [iweight=ipw]
				estimates store est8
				quietly `model9' [iweight=ipw]
				estimates store est9
				quietly `model10' [iweight=ipw]
				estimates store est10

				* Combine all models into a suest framework
				suest est1 est2 est3 est4 est5 est6 est7 est8 est9 est10
				estimates drop est1 est2 est3 est4 est5 est6 est7 est8 est9 est10

				ereturn local title "Seemingly unrelated estimation"
			end


			mi estimate, dots post: mysuest ///
				"reg cognition `kEdu' `controls' " ///
				"reg cognition `kEdu' `controls' smokev " ///
				"reg cognition `kEdu' `controls' vigex " ///
				"reg cognition `kEdu' `controls' drink " ///
				"reg cognition `kEdu' `controls' `hb' " ///
				"reg cognition `pEdu' `controls' " ///
				"reg cognition `pEdu' `controls' smokev " ///
				"reg cognition `pEdu' `controls' vigex " ///
				"reg cognition `pEdu' `controls' drink " ///
				"reg cognition `pEdu' `controls' `hb' "

			* Compare coefficients using lincom
			lincom (_b[est1_mean:2.edmaxcat]  - _b[est5_mean:2.edmaxcat]) - ///
				   (_b[est6_mean:2.pedmaxcat] - _b[est10_mean:2.pedmaxcat])
			* p =  0.478

			lincom (_b[est2_mean:3.edmaxcat]  - _b[est5_mean:3.edmaxcat]) - ///
				   (_b[est7_mean:3.pedmaxcat] - _b[est10_mean:3.pedmaxcat])
			* p =  0.068  
				   
			lincom (_b[est3_mean:4.edmaxcat]  - _b[est5_mean:4.edmaxcat]) - ///
				   (_b[est8_mean:4.pedmaxcat] - _b[est10_mean:4.pedmaxcat])
			* p = 0.130

			lincom (_b[est1_mean:4.edmaxcat]  - _b[est5_mean:4.edmaxcat]) - ///
				   (_b[est6_mean:3.pedmaxcat] - _b[est10_mean:3.pedmaxcat])
			* p = 0.135

			lincom (_b[est1_mean:4.edmaxcat]  - _b[est5_mean:4.edmaxcat]) - ///
				   (_b[est6_mean:2.pedmaxcat] - _b[est10_mean:2.pedmaxcat])
			* p = 0.074 
	     
  //  #6
  //  OLS for [p-edu & kid-edu] on cognition, controlling for HB one by one
  
 ******************************************************************************
 * Table 2 in manuscript for journal submission
 ******************************************************************************
	local pEdu     i.pedmaxcat 
	local kEdu     i.edmaxcat 
			 
	local controls    ///                          
							   i.female       i.foreign hhchild birthyrC /// 
							   i.not_con_marr i.black    i.hisp i.other  /// 
							   i.reduhs  i.redusc    i.reducol           ///     
							   i.spedhs  i.spedsc    i.spedcol           /// 
							   hitot                                     ///  
							   hibpe     diabe      cancre   lunge       /// 
							   hearte    strok      cesd         
	local hb 		i.smokev     i.vigex  i.drink	
	
				 
 
			mi estimate, post: reg cognition `pEdu' `kEdu'  `controls'
			eststo mZ1                         
			outreg2 using regResults5, replace excel dec(3)
		   
		   
			mi estimate, post: reg cognition `pEdu' `kEdu'  `controls' smokev 
			eststo mZ2
			outreg2 using regResults5, append excel dec(3)
		   
		   
			mi estimate, post: reg cognition `pEdu' `kEdu'  `controls' vigex 
			eststo mZ3
			outreg2 using regResults5, append excel dec(3)
		   
			mi estimate, post: reg cognition `pEdu' `kEdu'  `controls' drink 
			eststo mZ4
			outreg2 using regResults5, append excel dec(3)
		   
			mi estimate, post: reg cognition `pEdu' `kEdu'  `controls' `hb'
			eststo mZ5                                     
			outreg2 using regResults5, append excel dec(3)
   	   
	   

*******************************************************************************	
*****                      Mediation Test Table A3                 *****
********************************************************************************


     local controls ///                          
    i.female  i.foreign   hhchild   birthyrC   /// 
    i.not_con_marr                            /// 
    i.black   i.hisp      i.other             /// 
    i.reduhs  i.redusc    i.reducol           ///     
    i.spedhs  i.spedsc    i.spedcol           /// 
    hitot                                     ///  
    i.hibpe   i.diabe      i.cancre   i.lunge /// 
    i.hearte    i.strok      cesd     

local edu ///		
    i.pedmaxcat 

local hb1 ///
	i.vigex i.drink

local hb2 ///
	i.vigex i.smokev
	
local hb3 ///
	i.drink i.smokev

* 2. Model 1: Baseline logistic regression
mi estimate, or: logistic smokev `edu' `controls' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic smokev `edu' `controls' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word replace eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			ctitle("model 1")
		


* 3. Model 2: Add HB1
	
mi estimate, or: logistic smokev `edu' `controls' `hb1' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic smokev `edu' `controls' `hb1' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word  eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			append ctitle("model 2")
			
*** Model 3 AND Model 4		

* Model 3
mi estimate, or: logistic drink `edu' `controls' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic drink `edu' `controls' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			append ctitle("model 3")
			
* Model 4: add HB2
mi estimate, or: logistic drink `edu' `controls' `hb2' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic drink `edu' `controls' `hb2' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word  eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			append ctitle("model 4")
			
			
*** Model 5 AND Model 6

* Model 5

mi estimate, or: logistic vigex `edu' `controls' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic vigex `edu' `controls' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			append ctitle("Model 5")
			
* Model 6: add hb3
mi estimate, or: logistic vigex `edu' `controls' `hb3' ///
    [pweight=ipw]

        * Get the aic and bic
		* Initialize scalar for summing log-likelihoods
		scalar loglikelihoods = 0

		* Loop through imputations to extract log-likelihoods
		quietly forvalues i = 1/20 {
			mi extract `i'
			quietly logistic vigex `edu' `controls' `hb3' [pweight=ipw]
			scalar loglikelihoods = loglikelihoods + e(ll)
			use "analysis_modeling.dta", clear
		}

		* Calculate average log-likelihood
		scalar avg_loglikelihood = loglikelihoods / 20

		* Number of parameters
		scalar k = 32

		* Define number of observations
		scalar n = 11815

		* Calculate AIC and BIC
		scalar AIC = -2 * avg_loglikelihood + 2 * k
		scalar BIC = -2 * avg_loglikelihood + k * log(n)

		** Export regression results with AIC and BIC
		outreg2 using logModel1.doc, ///
			word  eform dec(3) ///
			addstat("AIC", AIC, "BIC", BIC) ///
			append ctitle("Model 6")
			
	

