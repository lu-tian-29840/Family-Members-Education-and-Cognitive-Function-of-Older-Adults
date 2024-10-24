//Program: rand_hrs_01
//Project: effect of adult children's education on parental cognitions
//Task: Create variables 
//Author:lutian 

version   16.1
clear     all
set       linesize 80
matrix drop _all

//   #1
//   Load data
use   rand_hrs_01, clear


//  #2
//  Data preparation

* cognition (DV)
gen       cognition = .
 
forvalues n = 3/12 {
  replace cognition = r`n'cogtot //if r`n'cogtot <.
}
 
tab       cognition, mi

lab var  cognition "Cognition scores"
	 
* Household income and wealth

gen       itot = .
 
forvalues n = 1/12 {
  replace itot = h`n'itot if h`n'itot <.
}

mdesc itot


gen       hitot = log(itot)
lab var   hitot "Household income (logged)"

* household wealth
gen       ahous = .
 
forvalues n = 1/12 {
  replace ahous = h`n'ahous if h`n'ahous <.
}
 
mdesc     ahous

gen       hahous = log(ahous)
lab var   hahous "Household wealth (logged)"


* Spouse education years


gen      spedu = .
 
forvalues n = 1/12 {
  replace spedu = s`n'edyrs  if s`n'edyrs  <.
}
 
tab       spedu, m

lab var   spedu "Spouse education"

** Respodent demogranphic 
 * recode R age

gen       rage = .
 
forvalues n = 1/12 {
  replace rage = r`n'agey_e  if r`n'agey_e <.
}
 
tab       rage, m

lab var   rage "R age"

 * recode marital status 


gen       mstat = .
 
forvalues n = 1/12 {
  replace mstat = r`n'mstat if r`n'mstat <.
}
 
tab       mstat, m


recode    mstat  (1/2=1 "Married") (4/6=2 "Div/Sep") (7=3 "Widow") ///
				(8=4 "NevMar"), gen(marcat) 
				
label var marcat  "Marital Status"

tab       marcat, gen (m_)

 *rename so that I remember what they are
rename m_1 married
rename m_2 sepdiv
rename m_3 widow
rename m_4 nevermarr




 * collasp smoke now 

gen       smoken = .
 
forvalues n = 1/12 {
  replace smoken = r`n'smoken  if r`n'smoken  <.
}
 
tab       smoken, m

lab var   smoken "Smoke now?"


gen       smokev = .
 
forvalues n = 1/12 {
  replace smokev = r`n'smokev  if r`n'smokev  <.
}
 
tab       smokev, m

lab var   smokev "Smoke ever?"

 * collasp vigrous physical activity, wave 1-6: 0/1; wave 7-12, 0/5.
 * recode wave 7-12 into 0/1
 
gen vigex = .
 
forvalues n = 1/6 {
replace vigex = r`n'vigact if r`n'vigact < .
}
 
forvalues n = 7/12 {
      replace vigex = 0 if r`n'vgactx==0 | r`n'vgactx==3 | r`n'vgactx==4 | r`n'vgactx==5
      replace vigex = 1 if r`n'vgactx==1 | r`n'vgactx==2
}
 
tab vigex, m

lab var vigex "Freqency of vigorous physical activity  3+/wk" 
 

 * collasp drink
gen       drink = .
 
forvalues n = 1/12 {
  replace drink  = r`n'drink   if r`n'drink   <.
}
 
tab       drink , m

lab var   drink "R ever drinks any alcohol"
	 

** Respodent health conditions

 * recode hypertension

gen       hibpe = .
 
forvalues n = 1/12 {
  replace hibpe  = r`n'hibpe   if r`n'hibpe   <.
}
 
tab       hibpe , m

lab var   hibpe "R ever had high blood pressure"

 * recode diabetes
 
gen       diabe = .
 
forvalues n = 1/12 {
  replace diabe  = r`n'diabe   if r`n'diabe   <.
}
 
tab       diabe , m

lab var   diabe "R ever had diabetes"	 

 * recode cancer
gen       cancre = .
 
forvalues n = 1/12 {
  replace cancre  = r`n'cancre   if r`n'cancre   <.
}
 
tab       cancre , m

lab var   cancre "R ever had cancer"	

  *recode lung disease

gen       lunge = .
 
forvalues n = 1/12 {
  replace lunge  = r`n'lunge   if r`n'lunge   <.
}
 
tab       lunge , m

lab var   lunge "R ever had lung disease" 

  * recode heart disease

gen       hearte = .
 
forvalues n = 1/12 {
  replace hearte  = r`n'hearte   if r`n'hearte   <.
}
 
tab       hearte , m

lab var   hearte "R ever had heart problems" 

  * recode stroke

gen       stroke = .
 
forvalues n = 1/12 {
  replace stroke  = r`n'stroke   if r`n'stroke   <.
}
 
tab       stroke , m

lab var   stroke "R ever had stroke" 	 

  * recode CESD
gen       cesd = .
 
forvalues n = 2/12 {
  replace cesd  = r`n'cesd   if r`n'cesd   <.
}
 
tab       cesd, m

lab var   cesd "R CESD scores" 	

  * recode self-rated health
gen      shlt = .
 
forvalues n = 1/12 {
  replace shlt  = r`n'shlt   if r`n'shlt   <.
}
 
tab       shlt, m

lab var   shlt "R self-rated health"

* save dataset
save  rand_hrs_02, replace
