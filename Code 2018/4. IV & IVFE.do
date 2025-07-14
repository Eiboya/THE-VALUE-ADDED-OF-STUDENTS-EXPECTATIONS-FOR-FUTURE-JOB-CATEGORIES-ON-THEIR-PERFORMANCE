clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"


*  - HavePlan

** 2SLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
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

** FE-2SLS with covariates
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
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

ivreg2 PV2SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) first savefirst

reg HavePlan guide1 $x  [pw=W_FSTUWT] , cluster(CNTSCHID)

ivreghdfe PV2SCIE (HavePlan=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)  absorb(CNTRYID) first savefirst

reghdfe HavePlan guide1 $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)

reg High guide1 $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
reghdfe High guide1 $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)

ivreg2 PV2SCIE (High=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) first savefirst

ivreghdfe PV2SCIE (High=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)  absorb(CNTRYID) first savefirst


*  - High


** 2SLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (High=guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
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


** FE-2SLS with covariates
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (High= guide1) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

sum mean_treatment
gen std=(var1treatment+var2treatment)^(1/2)
sum std
display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)


display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2treatment