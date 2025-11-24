// Author:lutian 
// Date: Feb 29, 2024
// Project: Effect of family education on older adults cognitive function
// Task: Create variables for OLS models 


version   16.1
clear     all
set       linesize 80
matrix drop _all

//   #1
//   Load data

* 1. load traker file
use    trk2020tr_r, clear
rename *, lower
gen    rahhidpn = hhid + pn
order  rahhidpn, first
sort   rahhidpn

merge 1:1 rahhidpn using rand_hrs_01 //  38,630
sort  rahhidpn
keep if _merge==3 
drop    _merge
save	 merged_step1.dta, replace

clear

* 2. load harmonized cognition data
use    COGIMP9220A_R, clear
rename *, lower
gen    rahhidpn = hhid + pn
order  rahhidpn, first
sort rahhidpn

* 3. merge 
merge 1:1 rahhidpn using merged_step1
sort rahhidpn
keep if _merge==3 
drop    _merge // 37,401  

* 4. Restrict sample to alive kid
keep if if_alive==1 // 34 observations deleted

* 5. Restrict sample to kid age > 25 & < 70

drop if kidage > 70         // 23,145  observations deleted
drop if kidage < 25         //  963 observations deleted
drop if missing(kidage)     // 0; N =  13,259 
 


* 6. Restrict sample:  drop respondents without any children

gen     parent=0
replace parent = 1 if hhchild != 0 

drop if parent==0        // 3 observations deleted

* 7.  Restrict sample:  all variables are NOT proxy interviews
gen proxyflag = 1
forvalues i = 1/15 {
    replace proxyflag = 0 if r`i'proxy == 1  
}
keep if proxyflag == 1 // 1,352  observations deleted


save 	merged_step2.dta, replace // N= 11,904

clear

use 	merged_step2.dta, clear


* 8. check the R birthyear, cohort info
tab birthyr  
tab racohbyr

//  #3
//  Data preparation

* 1. 
* cognition (DV)
* Generate a new variable for the mean cognition score across waves 3 to 15
egen cognition = rowmean(r3cogtot  r4cogtot   r5cogtot   r6cogtot r7cogtot ///
						 r8cogtot  r9cogtot   r10cogtot r11cogtot r12cogtot ///
						 r13cogtot r14cogtotp r15cogtotp) 
	drop if missing(cognition) // 1 deleted
					 
    tab cognition, m // no missing
  
			  
	egen wave_cog = rownonmiss(r3cogtot  r4cogtot    r5cogtot  r6cogtot  r7cogtot ///
							   r8cogtot  r9cogtot    r10cogtot r11cogtot r12cogtot ///
							   r13cogtot r14cogtotp  r15cogtotp)
	tab wave_cog, m



* 2. 
* Controls 

* 1). recode marital status 
gen missing_count = 0  // Initialize counter for missing marital status values

forvalues i = 1/15 {
    replace missing_count = missing_count + (missing(r`i'mstat))
}

	gen not_con_marr = 0  // Initialize as 0 (assume continuously married)

	forvalues i = 1/15 {
		replace not_con_marr = 1 if r`i'mstat != 1 & r`i'mstat != 2 & !missing(r`i'mstat)
	}

* Set to missing if marital status is completely missing across all waves
	replace not_con_marr = . if missing_count == 15
	tab not_con_marr, m

tab not_con_marr edmax, m

	* skip pattern for spouse education
	forvalues i = 1/15 {
		* review marital status 
    tab r`i'mstat

    * Replace spouse education with 0 if spouse edu is missed when R not married/partnered
		replace s`i'edyrs = 0 if missing(s`i'edyrs) & !inlist(r`i'mstat, 1, 2)
	}


* 2). Spouse education years
 
 egen spedmax = rowmax(s1edyrs s2edyrs s3edyrs s4edyrs s5edyrs s6edyrs s7edyrs ///
                     s8edyrs s9edyrs s10edyrs s11edyrs s12edyrs s13edyrs s14edyrs s15edyrs)
					 
	tab       spedmax, m
	lab var   spedmax "Spouse education"
	
	drop if missing(spedmax) // 1 removed
	 
   recode  spedmax (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(spedmaxcat)

   tab     spedmaxcat, gen (p_)
   rename  p_1 spedlhs
   rename  p_2 spedhs
   rename  p_3 spedsc
   rename  p_4 spedcol
   tab     spedmax, m
   
   
	
* 3). mean total value
* Generate a new variable for the mean total value across waves h1 to h15
egen itot = rowmean(h1itot h2itot h3itot h4itot h5itot h6itot h7itot ///
                    h8itot h9itot h10itot h11itot h12itot h13itot ///
                    h14itot h15itot)

	gen       hitot = log(itot)
	lab var   hitot "Household income (logged)"
	tab hitot, m

* 4). household wealth
egen ahous = rowmean(h1ahous h2ahous h3ahous h4ahous h5ahous h6ahous h7ahous ///
                     h8ahous h9ahous h10ahous h11ahous h12ahous h13ahous ///
                     h14ahous h15ahous)

						  
	gen       hahous = log(ahous)
	lab var   hahous "Household wealth (logged)"
	tab hahous, m				


* 5). Respodent health conditions

 foreach condition in hibpe diabe cancre lunge hearte stroke {
    gen `condition' = .
    foreach n of numlist 1/15 {
        replace `condition' = 1 if r`n'`condition' == 1
        replace `condition' = 0 if missing(`condition') & r`n'`condition' == 0
    }
}
tab1 hibpe diabe cancre lunge hearte stroke, m


  * recode self-rated health
foreach w of numlist 1/15 {
    gen r`w'srh = 6 - r`w'shlt if !missing(r`w'shlt)
}

	label define srh_lb 1 "Poor 1" 2 "Fair 2" 3 "Good 3" 4 "Very good 4" 5 "Excellent 5"

	foreach w of numlist 1/15 {
		label values r`w'srh srh_lb
	}
	tab1 r1srh r7srh
	
	gen shlt = .  
	foreach w of numlist 1/15 {
		replace shlt = 1 if inlist(r`w'srh, 1, 2)  // Set to "1" if 'Poor' or 'Fair'
		replace shlt = 0 if missing(shlt) & !missing(r`w'srh) & !inlist(r`w'srh, 1, 2)
	}
tab       shlt, m

lab var   shlt "R self-rated health"


  * recode CESD
egen cesd = rowmean(r2cesd r3cesd r4cesd r5cesd r6cesd ///
                    r7cesd r8cesd r9cesd r10cesd r11cesd ///
                    r12cesd r13cesd r14cesd r15cesd)

 
tab       cesd, m
lab var   cesd "R CESD scores" 	

* save dataset
keep hhidpn     hhid rahhidpn pn                    	/// ID
     kidid      if_alive                               	/// kid ID, vital status
	 kidage     kagenderbg                          	/// kid age gender
	 kaeduc     edmax          /// kid education
         rameduc    rafeduc                 		 /// mother edu father edu
	 cognition  birthyr    exdeathyr exdodsource study  /// R cognition,  exist year
	 raracem    rahispan  								/// race
	 rabplace   ragender   parent  not_con_marr 	   	/// R demo, family structure  
	 raedyrs    spedmax    spedlhs spedhs spedsc spedcol 	/// R and spouse edu
	 hitot              								/// household income
     smokev     vigex      drink           				/// health behvaiors
     hibpe      diabe      cancre   lunge           		/// R health conditions
	 hearte     strok      cesd                 			/// R health conditions
	 study
	 
save  rand_hrs_02, replace
