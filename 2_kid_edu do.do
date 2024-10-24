//Program: rand_hrs_01
//Project: effect of adult children's education on parental cognitions
//Task: data prep
//Author:lutian 

*** #1 Prepare education dataset through appending two datasets


       
* open randhrsfamk1992_2014v1 dataset
use  randhrsfamk1992_2014v1, clear

*browse hhidpn opn kaeduc 

*  To create an average of child education 

bysort hhidpn : egen edmean = mean(kaeduc)  



* to get the highest education among children

bysort hhidpn : egen edmax  = max(kaeduc)

browse hhidpn opn kaeduc edmax edmean

* keep only 1 observation per household

bysort hhidpn: gen n = _n    
drop if n>1                            /*keep the first one in the list*/

* to get the  age among children
gen kidage = .
 
forvalues n = 1/12 {
  replace kidage = k`n'age if k`n'age <.
}
 
tab kidage, m



* get the  alive or not information among children

gen kidalive = .
 
forvalues n = 1/12 {
  replace kidalive = k`n'alive if k`n'alive <.
}
 
tab kidalive, m

* get the kid income range
*recode k3incb, let its values to the same as others

tab          k3incb

recode       k3incb (1 2=5) (3/4=3) (5=4) (6 8=2) (0 7=1)
label def    k3incbval 1 "1.< 10k" 2 "2.10k-35k " 3 " 3.35k-70k" 4 "4.35k+" 5 "5.70k+"
label values k3incb k3incbval
tab          k3incb

tab1 k11incb k12incb

local kidincome k11incb k12incb

foreach v in `kidincome' {
	*create a local to grab the original variable label
	local vLab : variable label `v'
	 *retrieve name of value label
    local vLab : value label `v'
recode       `v' (1=1) (2=2) (3=3) (5=4) (4 6 7=5) 
label def    `v'value 1 "1.< 10k" 2 "2.10k-35k " 3 " 3.35k-70k" 4 "4.35k+" 5 "5.70k+"
label values `v'  `v'value
tab          `v'
}

gen       kidinc = .
forvalues n = 3/12 {
  replace kidinc = k`n'incb if k`n'incb<.
}
 
tab kidinc, m 


* keep kid education, income, age, alive or not, 
keep hhidpn  hhid rahhidpn pn             /// ID
     kidid   opn                          /// kid ID
	 kidage kagenderbg kidalive kidinc    ///     
	 kaeduc edmax edmean                  // education
	
      
sort hhidpn hhid rahhidpn pn

merge 1:1 hhidpn using randhrs1992_2018v1
keep if _merge==3          /*7,824 observations deleted*/
drop    _merge
save kid_randhrs92_18, replace /*to be merged*/

use kid_randhrs92_18, clear
merge 1:1 hhidpn using  parentdata
keep if _merge==3  /*3,084 observations deleted*/
drop    _merge

*Save dataset
save rand_hrs_01, replace

