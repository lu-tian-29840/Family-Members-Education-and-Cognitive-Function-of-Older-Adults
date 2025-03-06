// Author:lutian 
// Date: Feb 29, 2024
// Project: Effect of family education on older adults cognitive function
// Task: Attrition Analysis Based on Frist Wave of variables
// Part 1: creation of valirable at the frist wave
// Part 2: imputation
version     16.1
clear       all
set         linesize 80
matrix drop _all


use 	merged_step2.dta, clear



rename r14cogtotp r14cogtot
rename r15cogtotp r15cogtot

mdesc r*cogtot

save    attrition.dta, replace

use     attrition.dta, clear


********************************************************************************
*                  Creating valirable at the frist wave
********************************************************************************

* 1.
* indicator for missingness in DV across all waves
gen             attrCog = 0
foreach var in r3cogtot r4cogtot r5cogtot r6cogtot r7cogtot ///
			   r8cogtot r9cogtot r10cogtot r11cogtot r12cogtot r13cogtot r14cogtot r15cogtot {
    replace attrCog = 1 if `var' == .
}
	tab     attrCog, m
	
	* number of missing waves
	egen missCogWave = rowmiss(r3cogtot r4cogtot r5cogtot r6cogtot r7cogtot ///
							r8cogtot r9cogtot r10cogtot r11cogtot r12cogtot ///
							r13cogtot r14cogtot r15cogtot)
		tab missCogWave, m

	* Identify first observed wave
	gen firstCogW = .
	foreach wave in 3 4 5 6 7 8 9 10 11 12 13 14 15 {
		replace firstCogW = `wave' if missing(firstCogW) & !missing(r`wave'cogtot)
	}
		tab firstCogW, m
	
	* Each participant's first observed cognition score.
	gen firstCog = .
	foreach wave in 3 4 5 6 7 8 9 10 11 12 13 14 15 {
		replace firstCog = r`wave'cogtot if firstCogW == `wave'
	}
		tab firstCog, m
		drop if missing(firstCog)     // n = 11903
	
	* compare participants who dropped out with those who completed all waves
	ttest firstCog, by(attrCog)


* 2.
* Identify first observed wave and value for physical activity, smoking, and drinking

	* Physical activity
	rename r*vgactx r*vigact

	foreach var of varlist r7vigact-r15vigact {
		gen 	byte `var'_b = .
		replace 	 `var'_b = 1 if inlist(`var', 1)
		replace 	 `var'_b = 0 if inlist(`var', 2, 3, 4, 5)
	}

	foreach var of varlist r1vigact-r6vigact {
		clonevar `var'_b = `var'
	}


	* Initialize variables to store the first non-missing wave and value
	gen firstVigbW = .
	gen firstVigb  = .

	* Loop through each wave to identify the first non-missing value
	forvalues w = 1/15 {
		replace firstVigbW = `w' 		 if missing(firstVigbW) & !missing(r`w'vigact_b)
		replace firstVigb = r`w'vigact_b if missing(firstVigb)  & !missing(r`w'vigact_b)
	}

	* Display frequency table for firstVigb
	tab1 firstVigbW firstVigb, m


	* smoking ever
	gen firstSmkW = .
	gen firstSmk = .

	* Loop through each wave to identify the first non-missing smoking value
	forvalues w = 1/15 {
		replace firstSmkW = `w' 	  if missing(firstSmkW) & !missing(r`w'smokev)
		replace firstSmk = r`w'smokev if missing(firstSmk)  & !missing(r`w'smokev)
	}

	* Display frequency table for firstSmk
	tab1 firstSmkW firstSmk, m

	* dirnk ever
	gen firstDrkW = .
	gen firstDrk = .

	* Loop through each wave to identify the first non-missing drinking value
	forvalues w = 1/15 {
		replace firstDrkW = `w' 	 if missing(firstDrkW) & !missing(r`w'drink)
		replace firstDrk = r`w'drink if missing(firstDrk)  & !missing(r`w'drink)
	}

	* Display frequency tables for firstDrkW and firstDrk
	tab1 firstDrkW firstDrk, m

* 3. 
* Same approch for controls

	* 1) marital status 
	gen firstMstatW = .
	gen firstMstat = .

		* Loop through each wave to identify the first non-missing marital status value
		forvalues w = 1/15 {
			replace firstMstatW = `w'      if missing(firstMstatW) & !missing(r`w'mstat)
			replace firstMstat = r`w'mstat if missing(firstMstat)  & !missing(r`w'mstat)
		}

		* Display frequency table for firstMstat
		tab1 firstMstatW firstMstat, m
			
			recode firstMstat (1 2 3 = 1) (4 5 6 = 2) (7 = 3) (8 = 4), gen(firstMs)
			label define firstMs_lbl 1 "Marr/Partenered" 2 "Sep/Div" 3 "Widowed" 4 "NeverMarr"
			label values firstMs firstMs_lbl
			tab firstMs, m


	* 2) Define a list of health condition 
local conditions hibpe diabe cancre lunge hearte stroke

* Loop over each condition
foreach cond in `conditions' {
    * Generate variables to store the first non-missing wave and value
    gen first`cond'W = .
    gen first`cond'  = .

    * Loop through each wave to identify the first non-missing value for the condition
    forvalues w = 1/15 {
        replace first`cond'W = `w' if missing(first`cond'W) & !missing(r`w'`cond')
        replace first`cond' = r`w'`cond' if missing(first`cond') & !missing(r`w'`cond')
    }

    * Display the frequency table for the condition
    tab1 first`cond'W first`cond', m
}

	* 3) CES-D scores
	gen  firstCesdW = .
	gen  firstCesd  = .

		* Loop through each wave to identify the first non-missing CES-D value
		forvalues w = 2/15 {
			replace firstCesdW = `w' 	 if missing(firstCesdW) & !missing(r`w'cesd)
			replace firstCesd = r`w'cesd if missing(firstCesd)  & !missing(r`w'cesd)
		}

		* Display frequency table for firstCesd
		tab1 firstCesdW firstCesd, m

			
	* 4) self rated health
	foreach w of numlist 1/15 {
		gen r`w'srh = 6 - r`w'shlt if !missing(r`w'shlt)
	}

		label define srh_lb 1 "Poor 1" 2 "Fair 2" 3 "Good 3" 4 "Very good 4" 5 "Excellent 5"

		foreach w of numlist 1/15 {
			label values r`w'srh srh_lb
		}
		tab1 r1srh r7srh
		
		* Initialize variables to store the first non-missing wave and value
		gen firstSrhW = .
		gen firstSrh = .

			* Loop through each wave to identify the first non-missing self-rated health value
		forvalues w = 1/15 {
			replace firstSrhW = `w'    if missing(firstSrhW) & !missing(r`w'srh)
			replace firstSrh = r`w'srh if missing(firstSrh)  & !missing(r`w'srh)
		}
		* Display frequency tables for firstSrh and firstSrhW
		tab1 firstSrh firstSrhW, m


* Part 2
* Variable creation

	* 1. race
	tab1    raracem rahispan
	gen     racecat =.
	replace racecat = 0 if raracem  == 1 & rahispan == 0
	replace racecat = 1 if raracem  == 2 & rahispan == 0
	replace racecat = 2 if rahispan == 1
	replace racecat = 3 if raracem  == 3 & rahispan == 0
	tab     racecat, m

		lab var racecat "race category(0-4)"
		lab def racelab 0 "NH white(0)" 1 "NH black(1)" 2 "hisp(2)" 3 "NH others(3)"

		
	tab    racecat, gen (n_)
	rename n_1 white
	rename n_2 black
	rename n_3 hisp
	rename n_4 other

	 * 2. Edu of R's adult children 
    tab     edmax, m
    recode  edmax (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(edmaxcat)

    tab     edmaxcat, gen (e_)
    rename  e_1 kidedlhs
    rename  e_2 kidedhs
    rename  e_3 kidedsc
    rename  e_4 kidedcol
	
	* 3. Edu of R  
   
   recode degree ///
    (0 = 1 "Less than High School") ///
    (1/2 = 2 "High School") ///
    (3 9 = 3 "Some College") ///
    (4/6 = 4 "College Degree"), gen(reducat)

	tab 	   reducat, gen(e_)
	rename e_1 redulhs
	rename e_2 reduhs
	rename e_3 redusc
	rename e_4 reducol

   

* Part 3
* General attrition analysis 

* Predictors of attrition
logit attrCog firstCog birthyr i.racecat reducat ragender firstMs    ///
			  firstCesd firsthibpe firstdiabe firstcancre firstlunge ///
		   	  firsthearte firststroke firstSrh
	
* These factors predict attrition (as shown in previous attrition analysis) and cognitive function. Including them ensures that the continuation model accounts for the selection bias introduced by systematic dropout.
* conclusion: Attrition is significantly associated with several key demographic and health-related factors, indicating differential attrition.


* Part 4
* Create mortality vairable related attrition

* 1): Initialize attrMort as missing
gen 	attrMort = .

* 2): Mark attrMort = 1 (Deceased) based on  11,442 
replace attrMort = 1 if !missing(exdeathyr)

* 3): Mark attrMort = 1 (Deceased) based on knowndeceasedyr if exdeathyr is missing
replace attrMort = 1 if missing(exdeathyr) & !missing(knowndeceasedyr)

* 4): Use lastaliveyr to infer alive status
* Mark attrMort = 0 (Alive) for recent lastaliveyr
replace attrMort = 0 if missing(exdeathyr) & missing(knowndeceasedyr) & lastaliveyr >= 2020

* When we don't have any record of the respondent's death—meaning there is no 
* documented year of death from an exit interview (exdeathyr) or any evidence 
* from broader sources like spouse reports or imputed data (knowndeceasedyr)—we 
* need to rely on the last time the respondent was confirmed to be alive (lastaliveyr).

*If the respondent was known to be alive in 2020 (latest wave) or later, it's reasonable to 
* assume that they are likely still alive, given how recent this confirmation 
* is. In this case, we assign them a status of "alive" in the variable attrMort 
* (represented by the value 0).

	* Mark attrMort = 1 (Likely Deceased) for lastaliveyr
	replace attrMort = 1 if missing(exdeathyr) & missing(knowndeceasedyr) & lastaliveyr < 2020

	*  a. Verify attrMort distribution
	tab attrMort, m

	* b. Cross-check attrMort with key variables
	tab attrMort exdeathyr, m
	tab attrMort knowndeceasedyr, m
	tab attrMort lastaliveyr, m

* 5): Non-Mortality variable
gen 	dFlag = 0         // Default: alive
replace dFlag = 1 if attrMort == 1

gen 	attrNonMort = 0
replace attrNonMort = 1 if attrCog == 1 & attrMort == 0


*  Part 5
*  Mortality-Related attrtion &  Non-Mortality attrition. Follow Weuve 2012 
* DOI: 10.1097/EDE.0b013e318230e861.

gen birthyrC = birthyr - 1950
	* Checking the missing			
	mdesc   ///
		birthyrC    racecat     reducat    ragender   ///
		firstMs    firstCog    firstCesd  firsthibpe  ///
		firstdiabe firstcancre firstlunge firsthearte firststroke firstSrh ///
		rafeduc rameduc  edmaxcat	
		 
	replace rafeduc = . if rafeduc == .d | rafeduc == .r | rafeduc == .m
	replace rameduc = . if rameduc == .d | rameduc == .r | rameduc == .m

	tab1 rafeduc rameduc, m


********************************************************************************
*                       Impute missing data
********************************************************************************	
sort rahhidpn
merge 1:1 rahhidpn using rand_hrs_02

sort  rahhidpn
keep if _merge==3 
drop    _merge
save	 attrition1.dta, replace      // n = 11902

drop if missing(hhchild) // n= 11
drop if missing(hitot) // n =19
drop if missing(smokev) // n =35

* Step 0 
* Create variables
gen     female=.
replace female=1 if ragender==2
replace female=0 if ragender==1

* Recode R's navity
tab       rabplace
recode    rabplace (1/10=0) (11=1), gen(foreign)
lab var   foreign "Foreign Born"
lab def   foreignlab 1 "Born outside of US" 0 "US Born"

* recode kid gender 
tab     kagenderbg
gen     kidfemale=.
replace kidfemale=1 if kagenderbg==2
replace kidfemale=0 if kagenderbg==1



* Step 1
* Impute missing data

mi set wide

local mi_reg birthyrC      female      firstMs    /// 
             firstCog     firstCesd  firsthibpe  firstdiabe ///
             firstcancre  firstlunge firsthearte firststroke firstSrh ///
			 redulhs reduhs redusc reducol /// R edu
             attrMort ///
			 cognition /// outcome in OLS
			 foreign   hhchild   not_con_marr  /// OLS control  
			 spedlhs   spedhs    spedsc     spedcol /// OLS spouse edu
			 kidedlhs  kidedhs   kidedsc    kidedcol    /// OLS kid education
			 hitot                        ///  OLS control 
		     hibpe     diabe      cancre   lunge       /// OLS control
	         hearte    strok      cesd ///
			 vigex drink smokev /// OlS mediators 	
			  
				  
local mi_imp rafeduc rameduc racecat 

mi register regular `mi_reg'
mi register imputed `mi_imp'

mi impute chain ///
    (mlogit) racecat (pmm, knn(5)) rafeduc rameduc ///
    = birthyrC reducat female firstMs ///
      firstCog    firstCesd   firsthibpe firstdiabe ///
      firstcancre firstlunge firsthearte firststroke ///
      firstSrh    edmaxcat, add(20) dots rseed(12345)
				
				
    * Edu of R's parents
	mi passive: gen pedumax = max(rafeduc, rameduc)
	
	mi passive: gen      pedmaxcat = .
	mi passive: replace  pedmaxcat = 1 if pedumax >= 0 & pedumax <= 11
	mi passive: replace  pedmaxcat = 2 if pedumax == 12
	mi passive: replace  pedmaxcat = 3 if pedumax >= 13 & pedumax <= 15
	mi passive: replace  pedmaxcat = 4 if pedumax >= 16 & pedumax <= 17

	mi passive: gen pedlhs = (pedmaxcat == 1)
	mi passive: gen pedhs  = (pedmaxcat == 2)
	mi passive: gen pedsc  = (pedmaxcat == 3)
	mi passive: gen pedcol = (pedmaxcat == 4)



* Step 2
* Attrition analysis

* 1): Define variables for logistic regression
local attVar birthyrC i.white i.black i.hisp i.other i.female ///
					  i.redulhs i.reduhs i.redusc i.reducol ///
					  firstMs ///
					  firstCog firstCesd i.firsthibpe i.firstdiabe i.firstcancre i.firstlunge ///
					  i.firsthearte i.firststroke firstSrh ///
					  i.pedmaxcat i.edmaxcat

* 2): Run logistic regression and save the results
* Use the saving() option with mi estimate to save the model estimates
mi est, saving("/Users/lutian/Desktop/MT/Work/Work 1/my_model", replace): ///
    logit attrMort `attVar'

* 3): Predict probabilities using the saved estimates file
* Use the saved estimates file with mi predict
mi predict pMort using "/Users/lutian/Desktop/MT/Work/Work 1/my_model"
gen        prMort = 1 / (1 + exp(-pMort))

* 4): Summarize the predicted probabilities
* Check the created variable pMort
sum prMort

* 5): Logistic regression for non-mortality-related attrition
* Save results for non-mortality-related attrition
local attVar birthyrC i.white i.black i.hisp i.other i.female ///
					  i.redulhs i.reduhs i.redusc i.reducol   ///
					  firstMs ///
					  firstCog firstCesd i.firsthibpe i.firstdiabe i.firstcancre i.firstlunge ///
					  i.firsthearte i.firststroke firstSrh ///
					  i.pedmaxcat i.edmaxcat

mi est, saving("/Users/lutian/Desktop/MT/Work/Work 1/nonmort_model", replace): ///
    logit attrNonMort `attVar'

* 6): Predict probabilities for non-mortality-related attrition
* Using the saved estimates file for non-mortality-related attrition
mi predict pNonMort using "/Users/lutian/Desktop/MT/Work/Work 1/nonmort_model"

* 7): Transform log-odds into probabilities for non-mortality-related attrition
gen prNonMort = 1 / (1 + exp(-pNonMort))

	* Mortality-related probabilities
	sum prMort
	* Non-mortality-related probabilities
	sum prNonMort

* 8): Non-stabilized IPW for mortality-related and non-mortality-related attrition
gen    ipwMort = 1 / prMort
gen ipwNonMort = 1 / prNonMort

		sum     ipwMort
		sum  ipwNonMort
		hist    ipwMort, bin(30) normal title("Distribution of IPW (Mortality)")
		hist ipwNonMort, bin(30) normal title("Distribution of IPW (Non-Mortality)")


* 9): Stabilized mortality-related weights & non-mortality-related weights
sum             ipwMort
	scalar mean_ipwMort    = r(mean)
	gen    stab_ipwMort    = ipwMort / mean_ipwMort

sum             ipwNonMort
	scalar mean_ipwNonMort = r(mean)
	gen    stab_ipwNonMort = ipwNonMort / mean_ipwNonMort


* 10) Composite stabilized weights
gen ipw = stab_ipwMort * stab_ipwNonMort

* Step 3
* Save
save analysis, replace
