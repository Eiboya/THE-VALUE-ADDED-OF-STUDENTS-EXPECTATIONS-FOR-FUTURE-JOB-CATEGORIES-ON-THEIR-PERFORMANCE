
* ========================== *

***  Outline  ***
/*

*/

/* ==PART II. Naive Propensity Score Methods - HavePlan==

1 Computation of the IPTW estimator for the ATT (Propensity Score Weighting):
    IPTW estimator

2 Naive PSM (Propensity Score Matching): 
    i. 1:1 matching with replacement, caliper=0.25sd of PS → ATT
    - (Then regression for matched sample with weights generated from PSM process)
	ii. 1:1 matching no replacement, caliper=0.25sd of PS → ATT
    - (Then regression for matched sample with weights generated from PSM process) 
	iii. 1:2 matching with replacement, caliper=0.25sd of PS → ATT
    - (Then regression for matched sample with weights generated from PSM process)	
*/

/* ==PART III. Within-Cluster Propensity Score Methods - HavePlan==

1 Computation of the within-cluster IPTW estimator for the ATT (Propensity Score Weighting):

2 Within-Cluster PSM (Propensity Score Matching): 
    i. 1:1 matching with replacement, caliper=0.25sd of PS → ATT
    - (Then regression  for matched sample with weights generated from PSM process)
	ii. 1:1 matching no replacement, caliper=0.25sd of PS → ATT
    - (Then regression  for matched sample with weights generated from PSM process) 
	iii. 1:2 matching with replacement, caliper=0.25sd of PS → ATT
    - (Then regression  for matched sample with weights generated from PSM process)
*/

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

*** ==PART II. Naive Propensity Score - HavePlan==

** 1 Computation of the IPTW estimator for the ATT

// propensity score model
logit HavePlan $x [pweight=W_FSTUWT], vce(cluster CNTSCHID) nolog
predict pscore1, pr

gen attweight = .
replace attweight =(HavePlan ==1) if HavePlan ==1
replace attweight =(HavePlan ==0)*pscore1/(1- pscore1) if HavePlan ==0


forvalues i=1(1)10 {
  reg PV`i'SCIE [pw=attweight] if HavePlan==1, cluster(CNTSCHID)
  gen treated`i'=_b[_cons]
  reg PV`i'SCIE [pw=attweight] if HavePlan==0, cluster(CNTSCHID)
  gen controls`i'=_b[_cons]
  reg science HavePlan [pw=attweight], cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  }

gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "IPW-ATT coefficient: " %9.5f mean_treatment
display "IPW-ATT std.err: " %9.5f (var1treatment+var2treatment)^(1/2)
drop attweight-var2treatment

** 2 Naive PSM (Propensity Score Matching): 
sum pscore1

scalar cal = r(sd)*0.25 // caliper = 1/4 of standard deviation of the ps
*sum `=scalar(cal)'  
///0.0060302336782981


* i. 1:1 matching with replacement

forvalues i=1(1)10 {
  psmatch2 HavePlan, pscore(pscore1) outcome(PV`i'SCIE) common neighbor(1) caliper(`=scalar(cal)') 
  gen ATE`i'=r(ate)
  gen ATT`i'=r(att)
  gen SDATT`i'=r(seatt)
  sum PV`i'SCIE if _treated==1 & _support==1
  gen treated`i'=r(mean)
  sum _PV`i'SCIE if _treated==1 & _support==1
  gen controls`i'=r(mean)
  }
gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
gen var1ATT=(SDATT1^2+SDATT2^2+SDATT3^2+SDATT4^2+SDATT5^2+SDATT6^2+SDATT7^2+SDATT8^2+SDATT9^2+SDATT10^2)/10
gen var2ATT=(1+(1/10))*(1/9)*((ATT1-mean_ATT)^2+(ATT2-mean_ATT)^2+(ATT3-mean_ATT)^2+(ATT4-mean_ATT)^2+(ATT5-mean_ATT)^2+(ATT6-mean_ATT)^2+(ATT7-mean_ATT)^2+(ATT8-mean_ATT)^2+(ATT9-mean_ATT)^2+(ATT10-mean_ATT)^2)
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT
display "ATT std.err: " %9.5f (var1ATT+var2ATT)^(1/2)
sum _pdif
sum _weight
sum _weight if _treated==1
sum _weight if _treated==0
total _weight

pstest $x, both graph 
psgraph

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan  [fweight = _weight] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan [fweight = _weight] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = _weight] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = _weight] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


* ii. 1:1 matching no replacement
drop _PV1SCIE-var2ATT

*psmatch2 HavePlan, pscore(pscore1) outcome(PV1SCIE) ate common neighbor(1) noreplacement caliper(`=scalar(cal)') 

forvalues i=1(1)10 {
  psmatch2 HavePlan, pscore(pscore1) outcome(PV`i'SCIE) common neighbor(1)  noreplacement caliper(`=scalar(cal)') 
  gen ATE`i'=r(ate)
  gen ATT`i'=r(att)
  gen SDATT`i'=r(seatt)
  sum PV`i'SCIE if _treated==1 & _support==1
  gen treated`i'=r(mean)
  sum _PV`i'SCIE if _treated==1 & _support==1
  gen controls`i'=r(mean)
  }
gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
gen var1ATT=(SDATT1^2+SDATT2^2+SDATT3^2+SDATT4^2+SDATT5^2+SDATT6^2+SDATT7^2+SDATT8^2+SDATT9^2+SDATT10^2)/10
gen var2ATT=(1+(1/10))*(1/9)*((ATT1-mean_ATT)^2+(ATT2-mean_ATT)^2+(ATT3-mean_ATT)^2+(ATT4-mean_ATT)^2+(ATT5-mean_ATT)^2+(ATT6-mean_ATT)^2+(ATT7-mean_ATT)^2+(ATT8-mean_ATT)^2+(ATT9-mean_ATT)^2+(ATT10-mean_ATT)^2)
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT
display "ATT std.err: " %9.5f (var1ATT+var2ATT)^(1/2)
sum _pdif
sum _weight
sum _weight if _treated==1
sum _weight if _treated==0
total _weight

pstest $x, both graph 
psgraph

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan [fweight = _weight] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan [fweight = _weight] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = _weight] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = _weight] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

* iii. 1:2 matching with replacement
drop _PV1SCIE-var2ATT

forvalues i=1(1)10 {
  psmatch2 HavePlan, pscore(pscore1) outcome(PV`i'SCIE) common neighbor(2) caliper(`=scalar(cal)') 
  gen ATE`i'=r(ate)
  gen ATT`i'=r(att)
  gen SDATT`i'=r(seatt)
  sum PV`i'SCIE if _treated==1 & _support==1
  gen treated`i'=r(mean)
  sum _PV`i'SCIE if _treated==1 & _support==1
  gen controls`i'=r(mean)
  }
gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
gen var1ATT=(SDATT1^2+SDATT2^2+SDATT3^2+SDATT4^2+SDATT5^2+SDATT6^2+SDATT7^2+SDATT8^2+SDATT9^2+SDATT10^2)/10
gen var2ATT=(1+(1/10))*(1/9)*((ATT1-mean_ATT)^2+(ATT2-mean_ATT)^2+(ATT3-mean_ATT)^2+(ATT4-mean_ATT)^2+(ATT5-mean_ATT)^2+(ATT6-mean_ATT)^2+(ATT7-mean_ATT)^2+(ATT8-mean_ATT)^2+(ATT9-mean_ATT)^2+(ATT10-mean_ATT)^2)
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT
display "ATT std.err: " %9.5f (var1ATT+var2ATT)^(1/2)
sum _pdif
sum _weight
sum _weight if _treated==1
sum _weight if _treated==0
total _weight

pstest $x, both graph 
psgraph

gen _weight2=_weight*2
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan  [fweight = _weight2] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan  [fweight = _weight2] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = _weight2] if _weight!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = _weight2] if _weight!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


*** ==PART III. Within-Cluster Propensity Score - HavePlan==
clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"
 
** 1 Computation of the IPTW estimator for the ATT
bysort CNTRYID: egen slectsize=count(CNTRYID)

*gen a variable that indexes the students within each school
by CNTRYID: generate i = _n


* summarizing the hierarchy
codebook slectsize if i == 1
drop if CNTRYID==158  //CNT158 omitted because all students HavePlan==1
egen c = group(CNTRYID) 
levels c, local(cluster)
gen attweight=.

foreach j of local cluster {
  logit HavePlan $x [pweight=W_FSTUWT] if c==`j', vce(cluster CNTSCHID) nolog
  predict pscore`j' if c==`j', pr
  sum pscore`j' if c==`j'
  replace attweight =(HavePlan ==1) if HavePlan ==1 & c==`j'
  replace attweight =(HavePlan ==0)*(pscore`j')/(1- pscore`j') if HavePlan ==0 & c==`j'
  }

forvalues i=1(1)10 {
  gen treatedoutcome`i'=.
  gen controlsoutcome`i'=.
  }
  
forvalues i=1(1)10 {
  foreach j of local cluster {
  reg PV`i'SCIE [pw=attweight] if HavePlan==1 & (c==`j'), cluster(CNTSCHID) 
  replace treatedoutcome`i'=_b[_cons] if (c==`j') 
  reg PV`i'SCIE [pw=attweight] if HavePlan==0 & (c==`j'), cluster(CNTSCHID)
  replace controlsoutcome`i'=_b[_cons] if (c==`j') 
  }
  reghdfe science HavePlan [pw=attweight], cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  }
  
forvalues i=1(1)10 {
  sum treatedoutcome`i'
  gen treated`i'=r(mean)
  sum controlsoutcome`i'
  gen controls`i'=r(mean)
  }

gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "IPW-ATT coefficient: " %9.5f mean_treatment
display "IPW-ATT std.err: " %9.5f (var1treatment+var2treatment)^(1/2)
drop attweight-var2treatment


* 1:1 with replacement

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"
 

*gen a variable for the selected size of students in each school
bysort CNTRYID: egen slectsize=count(CNTRYID)

*gen a variable that indexes the students within each school
by CNTRYID: generate i = _n


* summarizing the hierarchy
codebook slectsize if i == 1
drop if CNTRYID==158

* balance before matching

egen c = group(CNTRYID)       
levels c, local(cluster)

gen Kcal=.
qui foreach j of local cluster {
  logit HavePlan $x [pweight=W_FSTUWT] if c==`j', vce(cluster CNTSCHID) nolog
  predict pscore`j' if c==`j', pr
  sum pscore`j' if c==`j'
  replace Kcal=r(sd)*0.25 if c==`j'
  sum Kcal if c==`j'
  gen kk_`j' = r(mean) 
  sum kk_`j' 
  scalar kkcal_`j' = r(mean)
}
forvalues i=1(1)10 {
  gen pATE`i'=.
  gen pATT`i'=.
  gen ptreated`i'=.
  gen pcontrols`i'=.
  gen support`i'=.
  gen weight`i'=.
}
forvalues i=1(1)10 {
qui foreach j of local cluster {	
	psmatch2 HavePlan if  (c==`j'), pscore(pscore`j') outcome(PV`i'SCIE) common neighbor(1) caliper(`=scalar(kkcal_`j')') 
	replace pATE`i'=r(ate) if (c==`j')
	replace pATT`i'=r(att) if  (c==`j')
	sum PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace ptreated`i'=r(mean) if  (c==`j')
	sum _PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace pcontrols`i'=r(mean) if  (c==`j')
	replace support`i' = _support if (c==`j')
	replace weight`i' = _weight if  (c==`j')
} 
}
forvalues i=1(1)10 {
  sum pATE`i'
  gen ATE`i'=r(mean)
  sum pATT`i'
  gen ATT`i'=r(mean)
  sum ptreated`i'
  gen treated`i'=r(mean)
  sum pcontrols`i'
  gen controls`i'=r(mean)
}

gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT


pstest $x if weight1!=. , treated(HavePlan) mweight(weight1) raw graph

sum support1 if support1==1
sum support1 if HavePlan==1 & support1==1
sum support1 if HavePlan==0 & support1==1
sum support1 if support1==0
sum support1 if HavePlan==1 & support1==0
sum support1 if HavePlan==0 & support1==0
sum weight1
sum weight1 if HavePlan==1
sum weight1 if HavePlan==0
total weight1

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan  [fweight = weight1] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan  [fweight = weight1] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = weight1] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = weight1] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

* 1:1 without replacement

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

*gen a variable for the selected size of students in each school
bysort CNTRYID: egen slectsize=count(CNTRYID)

*gen a variable that indexes the students within each school
by CNTRYID: generate i = _n


* summarizing the hierarchy
codebook slectsize if i == 1
drop if CNTRYID==158

* balance before matching

egen c = group(CNTRYID)       
levels c, local(cluster)

gen Kcal=.

qui foreach j of local cluster {
  logit HavePlan $x [pweight=W_FSTUWT] if c==`j', vce(cluster CNTSCHID) nolog
  predict pscore`j' if c==`j', pr
  sum pscore`j' if c==`j'
  replace Kcal=r(sd)*0.25 if c==`j'
  sum Kcal if c==`j'
  gen kk_`j' = r(mean) 
  sum kk_`j' 
  scalar kkcal_`j' = r(mean)
}
forvalues i=1(1)10 {
  gen pATE`i'=.
  gen pATT`i'=.
  gen ptreated`i'=.
  gen pcontrols`i'=.
  gen support`i'=.
  gen weight`i'=.
}
forvalues i=1(1)10 {
qui foreach j of local cluster {	
	psmatch2 HavePlan if  (c==`j'), pscore(pscore`j') outcome(PV`i'SCIE) common neighbor(1) noreplacement caliper(`=scalar(kkcal_`j')') 
	replace pATE`i'=r(ate) if (c==`j')
	replace pATT`i'=r(att) if  (c==`j')
	sum PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace ptreated`i'=r(mean) if  (c==`j')
	sum _PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace pcontrols`i'=r(mean) if  (c==`j')
	replace support`i' = _support if (c==`j')
	replace weight`i' = _weight if  (c==`j')
} 
}
forvalues i=1(1)10 {
  sum pATE`i'
  gen ATE`i'=r(mean)
  sum pATT`i'
  gen ATT`i'=r(mean)
  sum ptreated`i'
  gen treated`i'=r(mean)
  sum pcontrols`i'
  gen controls`i'=r(mean)
}

gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT

pstest $x if weight1!=. , treated(HavePlan) mweight(weight1) raw graph

sum support1 if support1==1
sum support1 if HavePlan==1 & support1==1
sum support1 if HavePlan==0 & support1==1
sum support1 if support1==0
sum support1 if HavePlan==1 & support1==0
sum support1 if HavePlan==0 & support1==0
sum weight1
sum weight1 if HavePlan==1
sum weight1 if HavePlan==0
total weight1

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan  [fweight = weight1] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan  [fweight = weight1] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = weight1] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = weight1] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


* 1:2 with replacement

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

*gen a variable for the selected size of students in each school
bysort CNTRYID: egen slectsize=count(CNTRYID)

*gen a variable that indexes the students within each school
by CNTRYID: generate i = _n


* summarizing the hierarchy
codebook slectsize if i == 1
drop if CNTRYID==158

* balance before matching

egen c = group(CNTRYID)       
levels c, local(cluster)

gen Kcal=.
qui foreach j of local cluster {
  logit HavePlan $x [pweight=W_FSTUWT] if c==`j', vce(cluster CNTSCHID) nolog
  predict pscore`j' if c==`j', pr
  sum pscore`j' if c==`j'
  replace Kcal=r(sd)*0.25 if c==`j'
  sum Kcal if c==`j'
  gen kk_`j' = r(mean) 
  sum kk_`j' 
  scalar kkcal_`j' = r(mean)
}
forvalues i=1(1)10 {
  gen pATE`i'=.
  gen pATT`i'=.
  gen ptreated`i'=.
  gen pcontrols`i'=.
  gen support`i'=.
  gen weight`i'=.
}
forvalues i=1(1)10 {
qui foreach j of local cluster {	
	psmatch2 HavePlan if  (c==`j'), pscore(pscore`j') outcome(PV`i'SCIE) common neighbor(2) caliper(`=scalar(kkcal_`j')') 
	replace pATE`i'=r(ate) if (c==`j')
	replace pATT`i'=r(att) if  (c==`j')
	sum PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace ptreated`i'=r(mean) if  (c==`j')
	sum _PV`i'SCIE if _treated==1 & _support==1 &  (c==`j')
	replace pcontrols`i'=r(mean) if  (c==`j')
	replace support`i' = _support if (c==`j')
	replace weight`i' = _weight if  (c==`j')
} 
}
forvalues i=1(1)10 {
  sum pATE`i'
  gen ATE`i'=r(mean)
  sum pATT`i'
  gen ATT`i'=r(mean)
  sum ptreated`i'
  gen treated`i'=r(mean)
  sum pcontrols`i'
  gen controls`i'=r(mean)
}

gen mean_treated=(treated1+treated2+treated3+treated4+treated5+treated6+treated7+treated8+treated9+treated10)/10
gen mean_controls=(controls1+controls2+controls3+controls4+controls5+controls6+controls7+controls8+controls9+controls10)/10
gen mean_ATE=(ATE1+ATE2+ATE3+ATE4+ATE5+ATE6+ATE7+ATE8+ATE9+ATE10)/10
gen mean_ATT=(ATT1+ATT2+ATT3+ATT4+ATT5+ATT6+ATT7+ATT8+ATT9+ATT10)/10
display "treated: " %9.5f mean_treated
display "controls: " %9.5f mean_controls
display "ATE: " %9.5f mean_ATE
display "ATT: " %9.5f mean_ATT


pstest $x if weight1!=. , treated(HavePlan) mweight(weight1) raw graph

sum support1 if support1==1
sum support1 if HavePlan==1 & support1==1
sum support1 if HavePlan==0 & support1==1
sum support1 if support1==0
sum support1 if HavePlan==1 & support1==0
sum support1 if HavePlan==0 & support1==0
sum weight1
sum weight1 if HavePlan==1
sum weight1 if HavePlan==0
total weight1

gen weight12=weight1*2
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan  [fweight = weight12] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons


forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan  [fweight = weight12] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x [fweight = weight12] if weight1!=., cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons

forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x [fweight = weight12] if weight1!=., cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)
gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10
drop treatment1-var2cons
