clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"


gen HKG=1 if CNTRYID==344
replace HKG=0 if HKG==.

gen DEU=1 if CNTRYID==276
replace DEU=0 if DEU==.

** OLS with interactions
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x c.HavePlan#c.HKG c.HavePlan#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen Treat_HKG`i'=_b[c.HavePlan#c.HKG]
  gen SDTreat_HKG`i'=_se[c.HavePlan#c.HKG]
  gen Treat_DEU`i'=_b[c.HavePlan#c.DEU]
  gen SDTreat_DEU`i'=_se[c.HavePlan#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)


display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.HavePlan#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.HavePlan#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.HavePlan#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons


** FE with interactions
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x c.HavePlan#c.HKG c.HavePlan#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen Treat_HKG`i'=_b[c.HavePlan#c.HKG]
  gen SDTreat_HKG`i'=_se[c.HavePlan#c.HKG]
  gen Treat_DEU`i'=_b[c.HavePlan#c.DEU]
  gen SDTreat_DEU`i'=_se[c.HavePlan#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)


display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.HavePlan#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.HavePlan#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.HavePlan#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons



** OLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (HavePlan=guide1) $x c.HavePlan#c.HKG c.HavePlan#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen Treat_HKG`i'=_b[c.HavePlan#c.HKG]
  gen SDTreat_HKG`i'=_se[c.HavePlan#c.HKG]
  gen Treat_DEU`i'=_b[c.HavePlan#c.DEU]
  gen SDTreat_DEU`i'=_se[c.HavePlan#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

sum mean_treatment
display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.HavePlan#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.HavePlan#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.HavePlan#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

***FE with interactions
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (HavePlan=guide1) $x c.HavePlan#c.HKG c.HavePlan#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen Treat_HKG`i'=_b[c.HavePlan#c.HKG]
  gen SDTreat_HKG`i'=_se[c.HavePlan#c.HKG]
  gen Treat_DEU`i'=_b[c.HavePlan#c.DEU]
  gen SDTreat_DEU`i'=_se[c.HavePlan#c.DEU]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)


sum mean_treatment
display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.HKG coefficient: " %9.5f mean_Treat_HKG  //207.78868
display "c.HavePlan#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.HavePlan#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.HavePlan#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)


display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2Treat_DEU





** OLS with interactions
forvalues i=1(1)10 {
  reg PV`i'SCIE High $x c.High#c.HKG c.High#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen Treat_HKG`i'=_b[c.High#c.HKG]
  gen SDTreat_HKG`i'=_se[c.High#c.HKG]
  gen Treat_DEU`i'=_b[c.High#c.DEU]
  gen SDTreat_DEU`i'=_se[c.High#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)


display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.High#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.High#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.High#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons


** FE with interactions
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE High $x c.High#c.HKG c.High#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen Treat_HKG`i'=_b[c.High#c.HKG]
  gen SDTreat_HKG`i'=_se[c.High#c.HKG]
  gen Treat_DEU`i'=_b[c.High#c.DEU]
  gen SDTreat_DEU`i'=_se[c.High#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)


display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.High#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.High#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.High#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons


** OLS with covariates
forvalues i=1(1)10 {
  ivreg2 PV`i'SCIE (High=guide1) $x c.High#c.HKG c.High#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen Treat_HKG`i'=_b[c.High#c.HKG]
  gen SDTreat_HKG`i'=_se[c.High#c.HKG]
  gen Treat_DEU`i'=_b[c.High#c.DEU]
  gen SDTreat_DEU`i'=_se[c.High#c.DEU]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

sum mean_treatment
display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.High#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.High#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.High#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

***FE with interactions
forvalues i=1(1)10 {
  ivreghdfe PV`i'SCIE (High=guide1) $x c.High#c.HKG c.High#c.DEU [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen Treat_HKG`i'=_b[c.High#c.HKG]
  gen SDTreat_HKG`i'=_se[c.High#c.HKG]
  gen Treat_DEU`i'=_b[c.High#c.DEU]
  gen SDTreat_DEU`i'=_se[c.High#c.DEU]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_Treat_HKG=(Treat_HKG1+Treat_HKG2+Treat_HKG3+Treat_HKG4+Treat_HKG5+Treat_HKG6+Treat_HKG7+Treat_HKG8+Treat_HKG9+Treat_HKG10)/10
gen var1Treat_HKG=(SDTreat_HKG1^2+SDTreat_HKG2^2+SDTreat_HKG3^2+SDTreat_HKG4^2+SDTreat_HKG5^2+SDTreat_HKG6^2+SDTreat_HKG7^2+SDTreat_HKG8^2+SDTreat_HKG9^2+SDTreat_HKG10^2)/10
gen var2Treat_HKG=(1+(1/10))*(1/9)*((Treat_HKG1-mean_Treat_HKG)^2+(Treat_HKG2-mean_Treat_HKG)^2+(Treat_HKG3-mean_Treat_HKG)^2+(Treat_HKG4-mean_Treat_HKG)^2+(Treat_HKG5-mean_Treat_HKG)^2+(Treat_HKG6-mean_Treat_HKG)^2+(Treat_HKG7-mean_Treat_HKG)^2+(Treat_HKG8-mean_Treat_HKG)^2+(Treat_HKG9-mean_Treat_HKG)^2+(Treat_HKG10-mean_Treat_HKG)^2)

gen mean_Treat_DEU=(Treat_DEU1+Treat_DEU2+Treat_DEU3+Treat_DEU4+Treat_DEU5+Treat_DEU6+Treat_DEU7+Treat_DEU8+Treat_DEU9+Treat_DEU10)/10
gen var1Treat_DEU=(SDTreat_DEU1^2+SDTreat_DEU2^2+SDTreat_DEU3^2+SDTreat_DEU4^2+SDTreat_DEU5^2+SDTreat_DEU6^2+SDTreat_DEU7^2+SDTreat_DEU8^2+SDTreat_DEU9^2+SDTreat_DEU10^2)/10
gen var2Treat_DEU=(1+(1/10))*(1/9)*((Treat_DEU1-mean_Treat_DEU)^2+(Treat_DEU2-mean_Treat_DEU)^2+(Treat_DEU3-mean_Treat_DEU)^2+(Treat_DEU4-mean_Treat_DEU)^2+(Treat_DEU5-mean_Treat_DEU)^2+(Treat_DEU6-mean_Treat_DEU)^2+(Treat_DEU7-mean_Treat_DEU)^2+(Treat_DEU8-mean_Treat_DEU)^2+(Treat_DEU9-mean_Treat_DEU)^2+(Treat_DEU10-mean_Treat_DEU)^2)


sum mean_treatment
display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.HKG coefficient: " %9.5f mean_Treat_HKG
display "c.High#c.HKG std.err: " %9.5f (var1Treat_HKG+var2Treat_HKG)^(1/2)

display "c.High#c.DEU coefficient: " %9.5f mean_Treat_DEU
display "c.High#c.DEU std.err: " %9.5f (var1Treat_DEU+var2Treat_DEU)^(1/2)


display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2Treat_DEU