clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"


*  - HavePlan

** 2SLS with covariates
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


ivreg2 PV1SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) first savefirst

// ------------------------------------------------------------------------------
//              |               Robust
//     HavePlan |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//       guide2 |   .0603156   .0136968     4.40   0.000     .0334699    .0871613
//         ESCS |  -.0033018   .0028028    -1.18   0.239    -.0087953    .0021918

reg HavePlan guide2 $x  [pw=W_FSTUWT] , cluster(CNTSCHID)

ivreghdfe PV1SCIE (HavePlan=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)  absorb(CNTRYID) first savefirst

// ------------------------------------------------------------------------------
//              |               Robust
//     HavePlan |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//       guide2 |   .0301031   .0161357     1.87   0.062     -.001523    .0617292
//         ESCS |  -.0029726   .0028214    -1.05   0.292    -.0085027    .0025574

		
reghdfe HavePlan guide2 $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)

reg High guide2 $x  [pw=W_FSTUWT] , cluster(CNTSCHID)
reghdfe High guide2 $x  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
ivreg2 PV1SCIE (High=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID) first savefirst
ivreghdfe PV1SCIE (High=guide2) $x  [pw=W_FSTUWT] , cluster(CNTSCHID)  absorb(CNTRYID) first savefirst


** FE-2SLS with covariates
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





*  - High


** 2SLS with covariates
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

** FE-2SLS with covariates
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