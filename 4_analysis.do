//Program: rand_hrs_02
//Project: effect of adult children's education on parental cognitions
//Task: Create variables and analysis
//Author:lutian 




//    #1
//    Open dataset
use  rand_hrs_02, clear
keep hhidpn    hhid rahhidpn pn                    /// ID
     kidid     opn                                 /// kid ID
	 kidage    kagenderbg kidalive           /// kid feature
	 kaeduc    edmax      edmean                   /// kid education
     rameduc   rafeduc                             /// child# mother edu father edu
	 cognition rage       raracem  rahispan        /// R cognition,  demo
	 rabplace  ragender   hhchild   marcat   /// R demo   
	 married   sepdiv     widow    nevermarr       /// R demo 
	 raedyrs   spedu      hitot    hahous          /// household SES
     smoken    smokev     vigex    drink           /// health behvaiors
     hibpe     diabe      cancre   lunge           /// R health conditions
	 hearte    strok      cesd                 // R health conditions



//   #2	 
//   Drop missing

mdesc
* 1) missing data handling  

** hahous: missing data = 6500
    drop hahous
	
* 2) check valid skip for spouse education (missing data for spedu=18,758)
** generate parenal education using mother&father educational
    * create the highest education among mother&father
    gen pedumax  = max(rameduc,rafeduc)

    * check if the new variable makes sense

    browse hhidpn rafeduc rameduc pedumax

    * recode parental highest educational level 
    tab     pedumax
    recode  pedumax (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(pedmaxcat)

    tab     pedmaxcat, gen (p_)
    rename  p_1 pedlhs
    rename  p_2 pedhs
    rename  p_3 pedsc
    rename  p_4 pedcol

* drop mother&father's educational
   drop  rameduc
   drop  rafeduc
    
tab1   married   sepdiv     widow    nevermarr
replace spedu = 0 if spedu==. & married==0


//   #3 
//   eligible sample and missing handling a

* 1) Drop anyone not age eligible, as cognition questions only ask those who are >65 
drop if   rage <65 // 21,141  observations deleted

* 2) Drop adult children who are older than 25 and younger than 70
drop if kidage > 70         // 823 observations deleted
drop if kidage < 25

* 3) Make sure that R should have at least one living adult child, 
keep if kidalive == 1        // 11 observations deleted

* 4) * drop respondents without any children
gen     parent=0
replace parent=1 if hhchild !=0 & kidage > 25 & kidage < 70

drop if parent==0        // 14 observations deleted
 
codebook, compact
*Drop missing values,  3,587  observations deleted)
mark    notMissing
markout notMissing _all, strok
tab     notMissing
drop if notMissing == 0
drop    notMissing

mdesc 

//   #3
//   Create and recode variables

*  create female vairable
gen     female=.
replace female=1 if ragender==2
replace female=0 if ragender==1

*  create a new race variable 
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

tab1 white black hisp other

* Recode R's navity
tab       rabplace
recode    rabplace (1/10=0) (11=1), gen(foreign)

lab var   foreign "Foreign Born"
lab def   foreignlab 1 "Born outside of US" 0 "US Born"

*  center age variable at lowest age --> meaningful intercept
gen       ragec65 = rage-65
label var ragec65 "Age Centered @ 65"

* recode kid gender 
tab     kagenderbg
gen     kidfemale=.
replace kidfemale=1 if kagenderbg==2
replace kidfemale=0 if kagenderbg==1


*** recode kid, spouse and parental education, categorize into 4 categories
 
    * recode kid highest educational level 
    tab     edmax, m
    recode  edmax (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(edmaxcat)

    tab     edmaxcat, gen (e_)
    rename  e_1 kidedlhs
    rename  e_2 kidedhs
    rename  e_3 kidedsc
    rename  e_4 kidedcol
			  
   
    * to get the highest education of spouse
   
   bysort hhidpn : egen spedmax  = max(spedu) 

   * keep only 1 observation per household
   bysort hhidpn: gen p = _n    
   drop if p>1		  
   *0 observations deleted

  
   *recode spouse highest educational level 
   tab     spedmax
   recode  spedmax (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(spedmaxcat)

   tab     spedmaxcat, gen (p_)
   rename  p_1 spedlhs
   rename  p_2 spedhs
   rename  p_3 spedsc
   rename  p_4 spedcol


   ** recode R's own education
   tab raedyrs
   recode  raedyrs (0/11=1 "Less than High School") (12=2 "High School")    ///
              (13/15=3 "Some College") (16/17=4 "College Degree"), gen(educ)

   tab     educ, gen (e_)
   rename  e_1 redulhs
   rename  e_2 reduhs
   rename  e_3 redusc
   rename  e_4 reducol  
   
   * kidalive，kidage kidfemale not significant in subsequent model analysis, drop it
   drop kidalive
   drop kidage
   drop kidfemale
   * therefore, those variables were dropped: 

** Get the summary of all variables

sum 	cognition i.edmaxcat                                  /// kid edu
				  i.pedmaxcat                                /// parental edu
				  i.female  i.foreign   hhchild   ragec65    /// R demo
				  i.racecat i.marcat                         /// R demo 
	              i.educ                                     /// R edu
                  i.spedmaxcat                               /// Spouse  edu
				  hitot                                      ///  Household wealth  
				  hibpe     diabe      cancre   lunge        /// R health conditions
	              hearte    strok      cesd              /// R health conditions	  
				  smoken    smokev     i.vigex   drink      // health behvaiors
				  				  
//   #4 
//   Analysis using OLS for p-edu--in paper, add HB one by one (method 2)

 
     *** OLS model fit (p-edu-cognition)

	    * Model 1 Regression on cognition by parental edu

	reg    cognition i.pedmaxcat                // parental edu
	eststo mA1
	outreg2 using regression_result, replace excel dec(3)

	                          				  
	  * Model 2: Model 1 + R demogranphics
	reg cognition i.pedmaxcat                                /// parental edu
				  i.female  i.foreign   hhchild   ragec65   /// R demo
				  i.sepdiv  i.widow     i.nevermarr         /// R marriage
				  i.black   i.hisp      i.other             // R race
	eststo mB1  
    outreg2 using regression_result, append excel dec(3)
	
               
          *kid: female, alive not significant, delete
	   
	  * Model 3, Model 2 + household SES
	reg cognition i.pedmaxcat                               /// parental edu
				  i.female  i.foreign   hhchild   ragec65   /// R demo
				  i.sepdiv  i.widow     i.nevermarr         /// R marriage
				  i.black   i.hisp      i.other             /// R race
	              i.reduhs  i.redusc    i.reducol           /// R edu
                  i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				  hitot                                     //  household wealth  

        *kid: age, female, alive not significant, delete 
	eststo mC1	
	outreg2 using regression_result, append excel dec(3)
	
	 * Model 4, Model 3 + R health conditions 
	 reg cognition i.pedmaxcat                               /// parent edu
				   i.female  i.foreign   hhchild   ragec65   /// R demo
				   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
				   i.black   i.hisp      i.other             /// R race
				   i.reduhs  i.redusc    i.reducol           /// R edu				    
                   i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				   hitot                                     ///  household wealth  
				   hibpe     diabe      cancre   lunge       /// R health conditions
	               hearte    strok      cesd             // R health conditions
	eststo mD1
    outreg2 using regression_result, append excel dec(3)	
				  
	  * Model 5, Model 4 + R health behabiors
	 reg cognition i.pedmaxcat                               /// parent edu
				   i.female  i.foreign   hhchild   ragec65   /// R demo
				   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
				   i.black   i.hisp      i.other             /// R race
				   i.reduhs  i.redusc    i.reducol           /// R edu				 
                   i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				   hitot                                     ///  household wealth  
				   hibpe     diabe      cancre   lunge       /// R health conditions
	               hearte    strok      cesd              /// R health conditions	  
				   smoken    smokev     vigex    drink      //   health behvaiors

	 eststo mE1
	 outreg2 using regression_result, append excel dec(3)

	 
 
*** 4B Method 2, add HB one by one, Table 5

 local controls    ///                          
				   i.female  i.foreign   hhchild   ragec65   /// 
				   i.sepdiv  i.widow     i.nevermarr         /// 
				   i.black   i.hisp      i.other             /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd         
				   
				   
  local hb        ///
                 vigex drink smokev smoken 	
				 			 
	 
** regression on cognition by parental edu, while adding HB one by one

   reg cognition i.pedmaxcat  `controls'           
   eststo mX0
   outreg2 using M2_OLS1, replace excel dec(3)
  
   reg cognition i.pedmaxcat  `controls' smoken
   eststo mX1
   outreg2 using M2_OLS1, append excel dec(3)
	 
   reg cognition i.pedmaxcat  `controls' smokev 
   eststo mX2
	 outreg2 using M2_OLS1, append excel dec(3)
 
   reg cognition i.pedmaxcat  `controls' vigex 
   eststo mX3
   outreg2 using M2_OLS1, append excel dec(3) 
	 
   reg cognition i.pedmaxcat  `controls' drink 
   eststo mX4
   outreg2 using M2_OLS1, append excel dec(3) 
   
   reg cognition i.pedmaxcat  `controls' `hb' 
   eststo mX5                                    
   outreg2 using M2_OLS1, append excel dec(3)
   
 * mD1= mX0, mX5=mE1
 
 //  #5
 //  Test model difference for the effects parental education by health behaviors
	
    suest mX0 mX1 mX2 mX3 mX4 mX5, coeflegend
    * * mD1= mX0, mX5=mE1
	

				  
    *** Linear combination test
	  * compare difference in parental education due to health behaviors
	  * mE1 = mD1 + all health behabiors
	  * mE1 = mX5, mD1= mX0
       
        lincom [mX0_mean]2.pedmaxcat - [mX5_mean]2.pedmaxcat //P=0.000
		lincom [mX0_mean]3.pedmaxcat - [mX5_mean]3.pedmaxcat //P=0.000
		lincom [mX0_mean]4.pedmaxcat - [mX5_mean]4.pedmaxcat //p=0.022
			  
	  * Interpretation: health behaviors (vigrous physical activity and ever drink ) 
	  * significantly reduce the size the asso btwn  p_edu and cognition.
	  *  at all edu levels
	  
 
  //  #6
  //  OLS for k-edu--in paper, used adding HB one by one (method 2)
 
    *** OLS model fit (k-edu-cognition)

    * Model 1 Regression on cognition by kid edu

	reg    cognition i.edmaxcat                               // kid edu
	eststo mA2
	outreg2 using regression_result1, replace excel dec(3)
	                          				  
	* Model 2: Model 1 + R & kid demogranphics
	reg    cognition i.edmaxcat                               /// kid edu
				     i.female  i.foreign   hhchild   ragec65  /// R demo
				     i.sepdiv  i.widow     i.nevermarr        /// R marriage
				     i.black   i.hisp      i.other            /// R race
                                         
       *vairables about kid: kidfemale not significant, drop it
	 eststo mB2
	 outreg2 using regression_result1, append excel dec(3)  
	 * Model 3, Model 2 + household SES
	reg cognition i.edmaxcat                                /// kid edu
				  i.female  i.foreign   hhchild   ragec65   /// R demo
				  i.sepdiv  i.widow     i.nevermarr         /// R marriage
				  i.black   i.hisp      i.other             /// R race
	              i.reduhs  i.redusc    i.reducol           /// R edu
                  i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				  hitot                                     //  household wealth  

        *variables about kid: kid age significant, delete. Now no variables of 
		* kid features were included, as all of them were non-significant
		
	eststo mC2
	outreg2 using regression_result1, append excel dec(3) 
	
	 * Model 4, Model 3 + R health conditions 
	reg cognition i.edmaxcat                                /// kid edu
				  i.female  i.foreign   hhchild   ragec65   /// R demo
				  i.sepdiv  i.widow     i.nevermarr         /// R marriage 
				  i.black   i.hisp      i.other             /// R race
	              i.reduhs  i.redusc    i.reducol           /// R edu
                  i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				  hitot                                     ///  household wealth   
				  hibpe     diabe      cancre   lunge       /// R health conditions
	              hearte    strok      cesd             // R health conditions
	eststo mD2	
	outreg2 using regression_result1, append excel dec(3) 
	
	  * Model 5, Model 4 + R health behabiors
	 reg cognition i.edmaxcat           /// kid edu
				   i.female  i.foreign   hhchild   ragec65   /// R demo
				   i.reduhs  i.redusc    i.reducol           /// R edu
				   i.sepdiv  i.widow     i.nevermarr         /// R marriage
				   i.black   i.hisp      i.other             /// R race
	               i.reduhs  i.redusc    i.reducol           /// R edu
                   i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
				   hitot                                     ///  household wealth  
				   hibpe     diabe      cancre   lunge       /// R health conditions
	               hearte    strok      cesd             /// R health conditions	  
				   smoken    smokev     vigex   drink       // health behvaiors
    eststo mE2
	outreg2 using regression_result1, append excel dec(3) 
	
	
 
 ** Method 2: regression on cognition by adult children edu, while adding HB one by one; Table 6
 
  local controls    ///                          
				   i.female  i.foreign   hhchild   ragec65   /// 
				   i.sepdiv  i.widow     i.nevermarr         /// 
				   i.black   i.hisp      i.other             /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd         
				   
				   
  local hb        ///
                 smoken    smokev     vigex    drink	
 
   reg cognition i.edmaxcat  `controls'
   eststo mY0                                  
   outreg2 using M2_OLS2, replace excel dec(3)
   
   reg cognition i.edmaxcat  `controls' smoken
   eststo mY1
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.edmaxcat  `controls' smokev 
   eststo mY2
   outreg2 using M2_OLS2, append excel dec(3)
   
   
   reg cognition i.edmaxcat  `controls' vigex 
   eststo mY3
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.edmaxcat  `controls' drink 
   eststo mY4
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.edmaxcat  `controls' `hb'
   eststo mY5                                     
   outreg2 using M2_OLS2, append excel dec(3)
	
* mD2 = mY0, mY5 = mE2

	
  //  # 7
  //  Test model difference for OLS of k-edu-cognition
	
  	
	
	 *** Linear combination test
	   * compare difference in parental education due to health behaviors
  suest mY0 mY1 mY2 mY3 mY4	mY5
        lincom [mY0_mean]2.edmaxcat - [mY5_mean]2.edmaxcat //P=0.057
		lincom [mY0_mean]3.edmaxcat - [mY5_mean]3.edmaxcat //P=0.003
		lincom [mY0_mean]4.edmaxcat - [mY5_mean]4.edmaxcat //p=0.000
       * Interpretation: health behaviors (vigrous physical activity and ever drink)
	   * as significant mediators btwn p_edu and cognition.
	   * except when kids only have high school degree   
	 
	 	 	  
  //  # 8
  //  Test difference between p-edu and k-edu in terms of multigenerational education 
       * mD1= mX0, mX5=mE1; mD2 = mY0, mY5 = mE2
	   * in the case of high shool for P_edu and K_edu, HB matter more
	   * for p_edu or k_edu?
	   * diff in p-edu caused by Hb minus diff in k_edu caused by HB
	   
       suest mX0 mX1 mX2 mX3 mX4 mX5 mY0 mY1 mY2 mY3 mY4 mY5
	   
	   lincom ([mY0_mean]2.edmaxcat  - [mY5_mean]2.edmaxcat) - ///
	          ([mX0_mean]2.pedmaxcat - [mX5_mean]2.pedmaxcat)       
	   * p=0.680
	   * when the max education of kid and parents is high school,
	   * Not find the difference in the role of health behaviors
	   
	   lincom ([mY0_mean]3.edmaxcat - [mY5_mean]3.edmaxcat) - ///
	          ([mX0_mean]3.pedmaxcat - [mX5_mean]3.pedmaxcat)  
	   * p=0.664
	   
	   lincom ([mY0_mean]4.edmaxcat  -  [mY5_mean]4.edmaxcat) - ///
	          ([mX0_mean]4.pedmaxcat -  [mX5_mean]4.pedmaxcat)  
				
				
	   * p=0.005 (look the coef.= .0882544 >0), suggesting HB matters more for 
	   *kid edu, when kid and parents had college degree.
	   
	   * when the max education of kid and parents is college
	   * there is a statistically significant difference in the role of health behaviors
	   * (vigrous physical activity and ever drink) between education and cognition
	   
	   
	
	   lincom ([mY0_mean]4.edmaxcat  -  [mY5_mean]4.edmaxcat) - ///
	          ([mX0_mean]3.pedmaxcat -  [mX5_mean]3.pedmaxcat)  
			  
	   *Coefficient= .0745554, p=0.042
	   * when k_edu is college, p_edu is some college, HB matters more
	   * for k_edu
	   
	   
	   lincom ([mY0_mean]4.edmaxcat  -  [mY5_mean]4.edmaxcat) - ///
	          ([mX0_mean]2.pedmaxcat -  [mX5_mean]2.pedmaxcat)
			  
		*Coefficient=.1052628 , p=0.001
		* when k_edu is college, p_edu is high school, HB matters more 
		* for k_edu
		
		* in sum, k_edu= college is an key factor for HB
	
	     lincom ([mY0_mean]3.edmaxcat  -  [mY5_mean]3.edmaxcat) - ///
	            ([mX0_mean]3.pedmaxcat -  [mX5_mean]3.pedmaxcat)  
	    * not significant  
	     
  //  #9
  //  OLS for [p-edu & kid-edu] on cognition, controlling for HB one by one, table 7
     local controls    ///                          
				   i.female  i.foreign   hhchild   ragec65   /// 
				   i.sepdiv  i.widow     i.nevermarr         /// 
				   i.black   i.hisp      i.other             /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd         
				   
				   
  local hb        ///
                 smoken    smokev     vigex    drink	
				 
 
   reg cognition i.pedmaxcat i.edmaxcat  `controls'
   eststo mZ0                                  
   outreg2 using M2_OLS2, replace excel dec(3)
   
   reg cognition i.pedmaxcat i.edmaxcat  `controls' smoken
   eststo mZ1
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.pedmaxcat i.edmaxcat  `controls' smokev 
   eststo mZ2
   outreg2 using M2_OLS2, append excel dec(3)
   
   
   reg cognition i.pedmaxcat i.edmaxcat  `controls' vigex 
   eststo mZ3
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.pedmaxcat i.edmaxcat  `controls' drink 
   eststo mZ4
   outreg2 using M2_OLS2, append excel dec(3)
   
   reg cognition i.pedmaxcat i.edmaxcat  `controls' `hb'
   eststo mZ5                                     
   outreg2 using M2_OLS2, append excel dec(3)
   
 
   
  //  # 10
  //  Test OLS model difference for the effects [p-edu & kid-edu] on cognition by health behaviors
   suest mZ0 mZ1 mZ2 mZ3 mZ4 mZ5
    
	 *** Linear combination test
	 * compare difference in MG education due to health behaviors
         
        lincom [mZ0_mean]2.pedmaxcat - [mZ5_mean]2.pedmaxcat //p=0.000 
		lincom [mZ0_mean]3.pedmaxcat - [mZ5_mean]3.pedmaxcat //p=0.000 
		lincom [mZ0_mean]4.pedmaxcat - [mZ5_mean]4.pedmaxcat //p=0.031
		
		
        lincom [mZ0_mean]2.edmaxcat - [mZ5_mean]2.edmaxcat   //p=0.057
        lincom [mZ0_mean]3.edmaxcat - [mZ5_mean]3.edmaxcat   //p=0.004
		lincom [mZ0_mean]4.edmaxcat - [mZ5_mean]4.edmaxcat   //p=0.000
   
   
     * Interpretation: health behaviors (vigrous physical activity and ever drink)
	 * as significant mediators btwn p_edu and cognition,
	 * except p-edu is  college.

	 * Interpretation: health behaviors (vigrous physical activity and ever drink)
	 * as significant mediators btwn k_edu and cognition,
	 * when k-edu is some college or college.
	   
	   
	   
	   

*******************************************************************************	
*****                      Mediation Test (path a)                        *****
********************************************************************************
	*** check the role of parental education on health behaviors (mediating path a)
	*** Table 2
   
    ** 1. Currently smokes
	
	* Take a look at your outcome and key independent variables in a scatter plot
	* with a lowess line to see if there may be a relationship between your variables. 
	

	
eststo: quietly  logistic smoken      i.pedmaxcat          ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge       /// R health conditions
	             hearte    strok      cesd           // R health conditions
	estat ic
	* aic bic:  8984.488   9200.307
	
	outreg2 using logreg_result1, replace ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic  smoken     i.pedmaxcat          ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
				 vigex      drink              //   health behvaiors 
	estat ic
	* aic bic:   8925.867   9157.102

	outreg2 using logreg_result1, append ctitle(Odds ratio) eform  excel dec(3)
	
	** 2. Ever smokes
	
eststo: quietly logistic smokev      i.pedmaxcat  ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
	estat ic
	* aic bic:  20211.27   20427.09
	
	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic  smokev i.pedmaxcat  ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			   hitot                                   ///  household wealth  
			   hibpe     diabe      cancre   lunge     /// R health conditions
	           hearte    strok      cesd           /// R health conditions
	           vigex      drink              //   health behvaiors 
	estat ic		
	* aic bic:  19886.64   20117.87

	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	
	
	** 3. Ever drinks
	
eststo: quietly logistic drink       i.pedmaxcat      ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
	estat ic
	* aic bic: 119773.4   19989.22
	
	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic drink       i.pedmaxcat         ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
	             smoken    smokev     vigex               //   health behvaiors
	
	estat ic
	* aic bic:  19384.04   19622.98
	
	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	
	** 4. Vigrous physical activity
	
eststo: quietly logistic vigex       i.pedmaxcat                               ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
    estat ic
	* aic bic:  14121.99   14337.81

	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic vigex       i.pedmaxcat                               ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
                 smoken    smokev     drink               //    health behvaiors
     estat ic
     * aic bic:   14000.24   14239.19
	 
	outreg2 using logreg_result1, append ctitle(Odds ratio) eform excel dec(3)
	

*********************************************************************************
	  
	*** check the role of adult chidlren's education on health behaviors (path a)
	*** Table 3
    ** Currently smokes
	
eststo: quietly logistic  smoken     i.edmaxcat           ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd                // R health conditions
				 
	estat ic
    * aic bic 8956.131    9171.95

	outreg2 using logreg_result2, replace ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic smoken      i.edmaxcat           ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			   hitot                                     ///  household wealth  
			   hibpe     diabe      cancre   lunge       /// R health conditions
	           hearte    strok      cesd                 /// R health conditions
			   vigex      drink                           //   health behvaiors 
				 
    estat ic
	*  aic bic:   8897.029   9128.264
	* 
	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
	** Ever smokes
	
eststo: quietly logistic smokev i.edmaxcat  ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
	
	estat ic
	* aic bic: 20203.93   20419.75
	
	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic  smokev i.edmaxcat  ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
	                 vigex      drink              //   health behvaiors 
				 
	
	estat ic
	* aic bic:  19870.47   20101.71
 
	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
	
	** Ever drinks
	
 eststo: quietly logistic drink  i.edmaxcat      ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
	
	estat ic
	* aic bic: 19747.36   19963.18
 
	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic drink  i.edmaxcat               ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
	             smoken    smokev     vigex               //   health behvaiors
	 	
	estat ic
	* aic bic:  19351.1   19590.05

	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
	** Vigrous physical activity
	
	eststo: quietly logistic vigex  i.edmaxcat                               ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd            // R health conditions
    
	estat ic
	* aic bic:  14104.77   14320.59
	* Log likelihood = -6791.1563 
	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
	eststo: quietly logistic vigex  i.edmaxcat                               ///
	           i.female  i.foreign   hhchild   ragec65   /// R demo
			   i.sepdiv  i.widow     i.nevermarr         /// R marriage 
			   i.black   i.hisp      i.other             /// R race
			   i.reduhs  i.redusc    i.reducol           /// R edu				 
               i.spedhs  i.spedsc    i.spedcol           /// spouse highest edu
			     hitot                                   ///  household wealth  
				 hibpe     diabe      cancre   lunge     /// R health conditions
	             hearte    strok      cesd           /// R health conditions
                 smoken    smokev     drink               //    health behvaiors

	estat ic
	* aic bic:  13988.95   14227.89

	
	outreg2 using logreg_result2, append ctitle(Odds ratio) eform excel dec(3)
	
	  
	  
*********************************************************************************
	  
	*** check the role of [p-edu&k-edu] on health behaviors (path a)	  
	*** Table 4 
	  
      local controls    ///                          
				   i.female  i.foreign   hhchild   ragec65   /// 
				   i.sepdiv  i.widow     i.nevermarr         /// 
				   i.black   i.hisp      i.other             /// 
				   i.reduhs  i.redusc    i.reducol           ///     
                   i.spedhs  i.spedsc    i.spedcol           /// 
				   hitot                                     ///  
				   hibpe     diabe      cancre   lunge       /// 
	               hearte    strok      cesd     
				  
	  local edu         ///		
	           i.pedmaxcat i.edmaxcat
      
	   
	 ** Currently smokes
	
eststo: quietly  logistic  smoken `edu'  `controls'
				 
	estat ic
    * aic bic   8959.838   9198.781
	
	
	outreg2 using logreg_result3, replace ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic smoken `edu'  `controls'        ///
	                             vigex  drink            // other HB
				 
    estat ic
	*  aic bic:  10059.66   10082.79
	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
	** Ever smokes
	
eststo: quietly logistic smokev `edu'  `controls'
	
	estat ic
	* aic bic:   22216.2   22223.91
	
	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly  logistic  smokev `edu'  `controls'        ///
	                               vigex  drink            // other HB
				 
	
	estat ic
	* aic bic:  21860.07    21883.2

	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
	
	** Ever drinks
	
eststo: quietly logistic drink  `edu'  `controls'
	
	estat ic
	* aic bic:  22267.51   22275.22

	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic drink `edu'  `controls'             ///
	                            smoken smokev  vigex         // other HB
	 	
	estat ic
	* aic bic:  21536.36   21567.19
	
	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
	** Vigrous physical activity
	
eststo: quietly logistic vigex  `edu'  `controls'                
    
	estat ic
	* aic bic: 15433.2   15440.91

	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
	
eststo: quietly logistic vigex  `edu'  `controls'                  ///
                                 smoken    smokev     drink         //  other HB

	estat ic
	* aic bic:  14989.68   15020.51
	
	
	outreg2 using logreg_result3, append ctitle(Odds ratio) eform excel dec(3)
