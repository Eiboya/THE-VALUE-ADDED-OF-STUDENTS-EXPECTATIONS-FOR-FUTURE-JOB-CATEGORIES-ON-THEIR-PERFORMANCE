clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

destring OCOD1, replace
destring OCOD2, replace
destring OCOD3, replace

gen MotherEMP="High" if OCOD1>=1000 & OCOD1<=3999
replace MotherEMP="Medium" if OCOD1>=4000 & OCOD1<=8999
replace MotherEMP="Low" if OCOD1>=9000 & OCOD1<=9699
replace MotherEMP="Armed Force" if OCOD1>=0000 & OCOD1<=999
replace MotherEMP="Unemployed or Vague" if OCOD1>=9997 & OCOD1<=9999

gen FatherEMP="High" if OCOD2>=1000 & OCOD2<=3999
replace FatherEMP="Medium" if OCOD2>=4000 & OCOD2<=8999
replace FatherEMP="Low" if OCOD2>=9000 & OCOD2<=9699
replace FatherEMP="Armed Force" if OCOD2>=0000 & OCOD2<=999
replace FatherEMP="Unemployed or Vague" if OCOD2>=9997 & OCOD2<=9999

encode MotherEMP, generate(MEMP)
encode FatherEMP, generate(FEMP)

global x "ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT i.MEMP i.FEMP SCHLTYPE"


** OLS with covariates
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
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

** FE with covariates
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
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

ivreg2 PV1SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
ivreghdfe PV1SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
ivreg2 PV1SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
ivreghdfe PV1SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)

*  - HavePlan

** OLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
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
sum mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

** FE with covariates
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

sum mean_treatment
display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)


display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2treatment


** OLS with covariates
forvalues i=1(1)10 {
  reg PV`i'SCIE High $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
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

display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

** FE with covariates
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE High $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
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

display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons




ivreg2 PV1SCIE (High=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
ivreghdfe PV1SCIE (High=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
ivreg2 PV1SCIE (High=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
ivreghdfe PV1SCIE (High=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)

//2457 clusters in 2015



*  - High


** OLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (High=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
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

display "High coefficient: " %9.5f mean_treatment
sum mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

** FE with covariates
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (High= guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

sum mean_treatment
display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)


display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2treatment

*****Naive PSM

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "EMOSUPS COMPETE female SCHSIZE STUBEHA TEACHSUP EDUSHORT ESCS GRADE ICTRES SMINS c.STUBEHA#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SCHSIZE#c.SCHSIZE c.EDUSHORT#c.STUBEHA c.SMINS#c.SCHSIZE c.GRADE#c.GRADE c.ESCS#c.EMOSUPS c.TEACHSUP#c.STUBEHA c.ESCS#c.ESCS c.GRADE#c.COMPETE c.GRADE#c.EMOSUPS c.SCHSIZE#c.EMOSUPS c.TEACHSUP#c.COMPETE c.ICTRES#c.SCHSIZE c.ESCS#c.SCHSIZE c.ICTRES#c.ESCS c.EDUSHORT#c.EMOSUPS c.ESCS#c.TEACHSUP"

logit HavePlan $x [pweight=W_FSTUWT], vce(cluster CNTSCHID) nolog
predict pscore1, pr

sum pscore1

scalar cal = r(sd)*0.25 // caliper = 1/4 of standard deviation of the ps
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


clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "ESCS female GRADE SMINS STUBEHA EMOSUPS COMPETE TEACHSUP CLSIZE TEACHBEHA SCHSIZE c.SCHSIZE#c.CLSIZE c.ESCS#c.ESCS c.SCHSIZE#c.SMINS c.SCHSIZE#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SMINS#c.SMINS c.SCHSIZE#c.female c.TEACHBEHA#c.STUBEHA c.TEACHSUP#c.STUBEHA c.COMPETE#c.EMOSUPS c.CLSIZE#c.ESCS c.GRADE#c.ESCS c.SCHSIZE#c.ESCS c.TEACHSUP#c.TEACHSUP c.SCHSIZE#c.COMPETE c.SCHSIZE#c.GRADE c.CLSIZE#c.GRADE c.SCHSIZE#c.STUBEHA c.TEACHBEHA#c.CLSIZE c.STUBEHA#c.STUBEHA c.STUBEHA#c.SMINS c.COMPETE#c.ESCS c.GRADE#c.GRADE"

logit High $x [pweight=W_FSTUWT], vce(cluster CNTSCHID) nolog
predict pscore1, pr

sum pscore1

scalar cal = r(sd)*0.25 // caliper = 1/4 of standard deviation of the ps
forvalues i=1(1)10 {
  psmatch2 High, pscore(pscore1) outcome(PV`i'SCIE) common neighbor(1) caliper(`=scalar(cal)') 
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



******Within-country PSM


clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "EMOSUPS COMPETE female SCHSIZE STUBEHA TEACHSUP EDUSHORT ESCS GRADE ICTRES SMINS c.STUBEHA#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SCHSIZE#c.SCHSIZE c.EDUSHORT#c.STUBEHA c.SMINS#c.SCHSIZE  c.ESCS#c.EMOSUPS c.TEACHSUP#c.STUBEHA c.ESCS#c.ESCS c.GRADE#c.COMPETE c.GRADE#c.EMOSUPS c.SCHSIZE#c.EMOSUPS c.TEACHSUP#c.COMPETE c.ICTRES#c.SCHSIZE c.ESCS#c.SCHSIZE c.ICTRES#c.ESCS c.EDUSHORT#c.EMOSUPS c.ESCS#c.TEACHSUP"
//Grade*Grade deleted so that the logit model convergent

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
pstest $x if weight1!=. , treated(High) mweight(weight1) raw graph
sum support1 if support1==1

clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS female GRADE SMINS STUBEHA EMOSUPS COMPETE TEACHSUP CLSIZE TEACHBEHA SCHSIZE c.SCHSIZE#c.CLSIZE c.ESCS#c.ESCS c.SCHSIZE#c.SMINS c.SCHSIZE#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SMINS#c.SMINS c.SCHSIZE#c.female c.TEACHBEHA#c.STUBEHA c.TEACHSUP#c.STUBEHA c.COMPETE#c.EMOSUPS c.CLSIZE#c.ESCS c.GRADE#c.ESCS c.SCHSIZE#c.ESCS c.TEACHSUP#c.TEACHSUP c.SCHSIZE#c.COMPETE c.SCHSIZE#c.GRADE c.CLSIZE#c.GRADE c.SCHSIZE#c.STUBEHA c.TEACHBEHA#c.CLSIZE c.STUBEHA#c.STUBEHA c.STUBEHA#c.SMINS c.COMPETE#c.ESCS c.GRADE#c.GRADE"


*gen a variable for the selected size of students in each school
bysort CNTRYID: egen slectsize=count(CNTRYID)

*gen a variable that indexes the students within each school
by CNTRYID: generate i = _n


* summarizing the hierarchy
codebook slectsize if i == 1


* balance before matching

egen c = group(CNTRYID)       
levels c, local(cluster)

gen Kcal=.
qui foreach j of local cluster {
  logit High $x [pweight=W_FSTUWT] if c==`j', vce(cluster CNTSCHID) nolog
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
	psmatch2 High if  (c==`j'), pscore(pscore`j') outcome(PV`i'SCIE) common neighbor(1) caliper(`=scalar(kkcal_`j')') 
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
pstest $x if weight1!=. , treated(High) mweight(weight1) raw graph
sum support1 if support1==1