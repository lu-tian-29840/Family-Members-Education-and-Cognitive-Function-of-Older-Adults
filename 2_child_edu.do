// Author:lutian 
// Date: Feb 29, 2024
// Project: Effect of family education on older adults cognitive function
// Task: prepare data sets of adult children education

*** #1 Prepare education dataset through appending two datasets


* 1. open randhrsfamk1992_2014v1 dataset
use  randhrsfamk1992_2018v2, clear

* Step 1: Flag children who are alive in any wave
foreach 	k of numlist 1/14 {
    gen 	is_alive_wave`k' = 0  // Initialize for wave k
    replace is_alive_wave`k' = 1 if k`k'alive == 1
}

* Step 2: Remove duplicates for each child in each wave
foreach k of numlist 1/14 {
    bysort rahhidpn kidid (is_alive_wave`k'): replace is_alive_wave`k' = is_alive_wave`k'[_N]
}

* Step 3: Count Unique Alive Children at each wave per Household
foreach  k of numlist 1/14 {
    egen num_alive_wave`k' = total(is_alive_wave`k'), by(rahhidpn)
}
sort   rahhidpn
browse rahhidpn kidid k*alive num_alive_wave*


* 2. get the highest education among children

* step 1:  Flag children who are alive in any wave
gen 		if_alive = 0 
foreach 	k of numlist 1/14 {
    replace if_alive = 1 if k`k'alive == 1
}

* Step 2: Identify Maximum Education Level (edmax)
egen edmax = max(kaeduc) if if_alive == 1, by(rahhidpn)
browse rahhidpn kidid kaeduc edmax if_alive

* Step 3: Flag Children with Maximum Education
gen flag_max_kedu = 0
replace flag_max_kedu = 1 if kaeduc == edmax & if_alive == 1


* Step 4: Resolve Ties by Selecting Eldest Child
gen selected_age = .
foreach k of numlist 1/14 {
    replace selected_age = k`k'age if flag_max_kedu == 1 & (selected_age < k`k'age | missing(selected_age))
}

* Step 5: Avoid duplicate by keeping only one child per household
gen valid_age = !missing(selected_age)  // Create an indicator for valid `selected_age`

bysort rahhidpn (flag_max_kedu -valid_age selected_age): keep if _n == _N 


gen kidage = selected_age


* keep kid education, age, alive or not, 
keep rahhidpn hhidpn  hhid  pn             /// ID
     kidid  if_alive   /// 
	 kidage kagenderbg                    ///     
	 kaeduc edmax                   // education
	
	
      
sort rahhidpn

merge 1:1 rahhidpn using randhrs1992_2020v1
keep if _merge==3          /*7,824 observations deleted*/
drop    _merge
save kid_randhrs92_18, replace /*to be merged*/

use kid_randhrs92_18, clear
merge 1:1 rahhidpn using  parentdata
keep if _merge==3  /*3,084 observations deleted*/
drop    _merge

*Save dataset
save rand_hrs_01, replace    // n = 38,631

*Save dataset
save rand_hrs_01, replace

