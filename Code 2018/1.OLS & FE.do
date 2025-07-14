clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

//3532 clusters in 2018

******************
*  - HavePlan    *
******************

** OLS without covariates
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan [pw=W_FSTUWT] , cluster(CNTSCHID)
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


** OLS with interactions
forvalues i=1(1)10 {
  reg PV`i'SCIE HavePlan $x c.HavePlan#c.CLSIZE c.HavePlan#c.STUBEHA c.HavePlan#c.TEACHBEHA c.HavePlan#c.SCHSIZE c.HavePlan#c.EDUSHORT [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen interaction1_`i'=_b[c.HavePlan#c.CLSIZE]
  gen SDinteraction1_`i'=_se[c.HavePlan#c.CLSIZE]
  gen interaction2_`i'=_b[c.HavePlan#c.STUBEHA]
  gen SDinteraction2_`i'=_se[c.HavePlan#c.STUBEHA]
  gen interaction3_`i'=_b[c.HavePlan#c.TEACHBEHA]
  gen SDinteraction3_`i'=_se[c.HavePlan#c.TEACHBEHA]
  gen interaction4_`i'=_b[c.HavePlan#c.SCHSIZE]
  gen SDinteraction4_`i'=_se[c.HavePlan#c.SCHSIZE]
  gen interaction5_`i'=_b[c.HavePlan#c.EDUSHORT]
  gen SDinteraction5_`i'=_se[c.HavePlan#c.EDUSHORT]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_interaction1_=(interaction1_1+interaction1_2+interaction1_3+interaction1_4+interaction1_5+interaction1_6+interaction1_7+interaction1_8+interaction1_9+interaction1_10)/10
gen var1interaction1_=(SDinteraction1_1^2+SDinteraction1_2^2+SDinteraction1_3^2+SDinteraction1_4^2+SDinteraction1_5^2+SDinteraction1_6^2+SDinteraction1_7^2+SDinteraction1_8^2+SDinteraction1_9^2+SDinteraction1_10^2)/10
gen var2interaction1_=(1+(1/10))*(1/9)*((interaction1_1-mean_interaction1_)^2+(interaction1_2-mean_interaction1_)^2+(interaction1_3-mean_interaction1_)^2+(interaction1_4-mean_interaction1_)^2+(interaction1_5-mean_interaction1_)^2+(interaction1_6-mean_interaction1_)^2+(interaction1_7-mean_interaction1_)^2+(interaction1_8-mean_interaction1_)^2+(interaction1_9-mean_interaction1_)^2+(interaction1_10-mean_interaction1_)^2)

gen mean_interaction2_=(interaction2_1+interaction2_2+interaction2_3+interaction2_4+interaction2_5+interaction2_6+interaction2_7+interaction2_8+interaction2_9+interaction2_10)/10
gen var1interaction2_=(SDinteraction2_1^2+SDinteraction2_2^2+SDinteraction2_3^2+SDinteraction2_4^2+SDinteraction2_5^2+SDinteraction2_6^2+SDinteraction2_7^2+SDinteraction2_8^2+SDinteraction2_9^2+SDinteraction2_10^2)/10
gen var2interaction2_=(1+(1/10))*(1/9)*((interaction2_1-mean_interaction2_)^2+(interaction2_2-mean_interaction2_)^2+(interaction2_3-mean_interaction2_)^2+(interaction2_4-mean_interaction2_)^2+(interaction2_5-mean_interaction2_)^2+(interaction2_6-mean_interaction2_)^2+(interaction2_7-mean_interaction2_)^2+(interaction2_8-mean_interaction2_)^2+(interaction2_9-mean_interaction2_)^2+(interaction2_10-mean_interaction2_)^2)

gen mean_interaction3_=(interaction3_1+interaction3_2+interaction3_3+interaction3_4+interaction3_5+interaction3_6+interaction3_7+interaction3_8+interaction3_9+interaction3_10)/10
gen var1interaction3_=(SDinteraction3_1^2+SDinteraction3_2^2+SDinteraction3_3^2+SDinteraction3_4^2+SDinteraction3_5^2+SDinteraction3_6^2+SDinteraction3_7^2+SDinteraction3_8^2+SDinteraction3_9^2+SDinteraction3_10^2)/10
gen var2interaction3_=(1+(1/10))*(1/9)*((interaction3_1-mean_interaction3_)^2+(interaction3_2-mean_interaction3_)^2+(interaction3_3-mean_interaction3_)^2+(interaction3_4-mean_interaction3_)^2+(interaction3_5-mean_interaction3_)^2+(interaction3_6-mean_interaction3_)^2+(interaction3_7-mean_interaction3_)^2+(interaction3_8-mean_interaction3_)^2+(interaction3_9-mean_interaction3_)^2+(interaction3_10-mean_interaction3_)^2)

gen mean_interaction4_=(interaction4_1+interaction4_2+interaction4_3+interaction4_4+interaction4_5+interaction4_6+interaction4_7+interaction4_8+interaction4_9+interaction4_10)/10
gen var1interaction4_=(SDinteraction4_1^2+SDinteraction4_2^2+SDinteraction4_3^2+SDinteraction4_4^2+SDinteraction4_5^2+SDinteraction4_6^2+SDinteraction4_7^2+SDinteraction4_8^2+SDinteraction4_9^2+SDinteraction4_10^2)/10
gen var2interaction4_=(1+(1/10))*(1/9)*((interaction4_1-mean_interaction4_)^2+(interaction4_2-mean_interaction4_)^2+(interaction4_3-mean_interaction4_)^2+(interaction4_4-mean_interaction4_)^2+(interaction4_5-mean_interaction4_)^2+(interaction4_6-mean_interaction4_)^2+(interaction4_7-mean_interaction4_)^2+(interaction4_8-mean_interaction4_)^2+(interaction4_9-mean_interaction4_)^2+(interaction4_10-mean_interaction4_)^2)

gen mean_interaction5_=(interaction5_1+interaction5_2+interaction5_3+interaction5_4+interaction5_5+interaction5_6+interaction5_7+interaction5_8+interaction5_9+interaction5_10)/10
gen var1interaction5_=(SDinteraction5_1^2+SDinteraction5_2^2+SDinteraction5_3^2+SDinteraction5_4^2+SDinteraction5_5^2+SDinteraction5_6^2+SDinteraction5_7^2+SDinteraction5_8^2+SDinteraction5_9^2+SDinteraction5_10^2)/10
gen var2interaction5_=(1+(1/10))*(1/9)*((interaction5_1-mean_interaction5_)^2+(interaction5_2-mean_interaction5_)^2+(interaction5_3-mean_interaction5_)^2+(interaction5_4-mean_interaction5_)^2+(interaction5_5-mean_interaction5_)^2+(interaction5_6-mean_interaction5_)^2+(interaction5_7-mean_interaction5_)^2+(interaction5_8-mean_interaction5_)^2+(interaction5_9-mean_interaction5_)^2+(interaction5_10-mean_interaction5_)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.CLSIZE coefficient: " %9.5f mean_interaction1_
display "c.HavePlan#c.CLSIZE std.err: " %9.5f (var1interaction1_+var2interaction1_)^(1/2)

display "c.HavePlan#c.STUBEHA coefficient: " %9.5f mean_interaction2_
display "c.HavePlan#c.STUBEHA std.err: " %9.5f (var1interaction2_+var2interaction2_)^(1/2)

display "c.HavePlan#c.TEACHBEHA coefficient: " %9.5f mean_interaction3_
display "c.HavePlan#c.TEACHBEHA std.err: " %9.5f (var1interaction3_+var2interaction3_)^(1/2)

display "c.HavePlan#c.SCHSIZE  coefficient: " %9.5f mean_interaction4_
display "c.HavePlan#c.SCHSIZE  std.err: " %9.5f (var1interaction4_+var2interaction4_)^(1/2)

display "c.HavePlan#c.EDUSHORT coefficient: " %9.5f mean_interaction5_
display "c.HavePlan#c.EDUSHORT std.err: " %9.5f (var1interaction5_+var2interaction5_)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons


** FE with interactions
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE HavePlan $x c.HavePlan#c.CLSIZE c.HavePlan#c.STUBEHA c.HavePlan#c.TEACHBEHA c.HavePlan#c.SCHSIZE c.HavePlan#c.EDUSHORT  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[HavePlan]
  gen SDtreatment`i'=_se[HavePlan]
  gen interaction1_`i'=_b[c.HavePlan#c.CLSIZE]
  gen SDinteraction1_`i'=_se[c.HavePlan#c.CLSIZE]
  gen interaction2_`i'=_b[c.HavePlan#c.STUBEHA]
  gen SDinteraction2_`i'=_se[c.HavePlan#c.STUBEHA]
  gen interaction3_`i'=_b[c.HavePlan#c.TEACHBEHA]
  gen SDinteraction3_`i'=_se[c.HavePlan#c.TEACHBEHA]
  gen interaction4_`i'=_b[c.HavePlan#c.SCHSIZE]
  gen SDinteraction4_`i'=_se[c.HavePlan#c.SCHSIZE]
  gen interaction5_`i'=_b[c.HavePlan#c.EDUSHORT]
  gen SDinteraction5_`i'=_se[c.HavePlan#c.EDUSHORT]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_interaction1_=(interaction1_1+interaction1_2+interaction1_3+interaction1_4+interaction1_5+interaction1_6+interaction1_7+interaction1_8+interaction1_9+interaction1_10)/10
gen var1interaction1_=(SDinteraction1_1^2+SDinteraction1_2^2+SDinteraction1_3^2+SDinteraction1_4^2+SDinteraction1_5^2+SDinteraction1_6^2+SDinteraction1_7^2+SDinteraction1_8^2+SDinteraction1_9^2+SDinteraction1_10^2)/10
gen var2interaction1_=(1+(1/10))*(1/9)*((interaction1_1-mean_interaction1_)^2+(interaction1_2-mean_interaction1_)^2+(interaction1_3-mean_interaction1_)^2+(interaction1_4-mean_interaction1_)^2+(interaction1_5-mean_interaction1_)^2+(interaction1_6-mean_interaction1_)^2+(interaction1_7-mean_interaction1_)^2+(interaction1_8-mean_interaction1_)^2+(interaction1_9-mean_interaction1_)^2+(interaction1_10-mean_interaction1_)^2)

gen mean_interaction2_=(interaction2_1+interaction2_2+interaction2_3+interaction2_4+interaction2_5+interaction2_6+interaction2_7+interaction2_8+interaction2_9+interaction2_10)/10
gen var1interaction2_=(SDinteraction2_1^2+SDinteraction2_2^2+SDinteraction2_3^2+SDinteraction2_4^2+SDinteraction2_5^2+SDinteraction2_6^2+SDinteraction2_7^2+SDinteraction2_8^2+SDinteraction2_9^2+SDinteraction2_10^2)/10
gen var2interaction2_=(1+(1/10))*(1/9)*((interaction2_1-mean_interaction2_)^2+(interaction2_2-mean_interaction2_)^2+(interaction2_3-mean_interaction2_)^2+(interaction2_4-mean_interaction2_)^2+(interaction2_5-mean_interaction2_)^2+(interaction2_6-mean_interaction2_)^2+(interaction2_7-mean_interaction2_)^2+(interaction2_8-mean_interaction2_)^2+(interaction2_9-mean_interaction2_)^2+(interaction2_10-mean_interaction2_)^2)

gen mean_interaction3_=(interaction3_1+interaction3_2+interaction3_3+interaction3_4+interaction3_5+interaction3_6+interaction3_7+interaction3_8+interaction3_9+interaction3_10)/10
gen var1interaction3_=(SDinteraction3_1^2+SDinteraction3_2^2+SDinteraction3_3^2+SDinteraction3_4^2+SDinteraction3_5^2+SDinteraction3_6^2+SDinteraction3_7^2+SDinteraction3_8^2+SDinteraction3_9^2+SDinteraction3_10^2)/10
gen var2interaction3_=(1+(1/10))*(1/9)*((interaction3_1-mean_interaction3_)^2+(interaction3_2-mean_interaction3_)^2+(interaction3_3-mean_interaction3_)^2+(interaction3_4-mean_interaction3_)^2+(interaction3_5-mean_interaction3_)^2+(interaction3_6-mean_interaction3_)^2+(interaction3_7-mean_interaction3_)^2+(interaction3_8-mean_interaction3_)^2+(interaction3_9-mean_interaction3_)^2+(interaction3_10-mean_interaction3_)^2)

gen mean_interaction4_=(interaction4_1+interaction4_2+interaction4_3+interaction4_4+interaction4_5+interaction4_6+interaction4_7+interaction4_8+interaction4_9+interaction4_10)/10
gen var1interaction4_=(SDinteraction4_1^2+SDinteraction4_2^2+SDinteraction4_3^2+SDinteraction4_4^2+SDinteraction4_5^2+SDinteraction4_6^2+SDinteraction4_7^2+SDinteraction4_8^2+SDinteraction4_9^2+SDinteraction4_10^2)/10
gen var2interaction4_=(1+(1/10))*(1/9)*((interaction4_1-mean_interaction4_)^2+(interaction4_2-mean_interaction4_)^2+(interaction4_3-mean_interaction4_)^2+(interaction4_4-mean_interaction4_)^2+(interaction4_5-mean_interaction4_)^2+(interaction4_6-mean_interaction4_)^2+(interaction4_7-mean_interaction4_)^2+(interaction4_8-mean_interaction4_)^2+(interaction4_9-mean_interaction4_)^2+(interaction4_10-mean_interaction4_)^2)

gen mean_interaction5_=(interaction5_1+interaction5_2+interaction5_3+interaction5_4+interaction5_5+interaction5_6+interaction5_7+interaction5_8+interaction5_9+interaction5_10)/10
gen var1interaction5_=(SDinteraction5_1^2+SDinteraction5_2^2+SDinteraction5_3^2+SDinteraction5_4^2+SDinteraction5_5^2+SDinteraction5_6^2+SDinteraction5_7^2+SDinteraction5_8^2+SDinteraction5_9^2+SDinteraction5_10^2)/10
gen var2interaction5_=(1+(1/10))*(1/9)*((interaction5_1-mean_interaction5_)^2+(interaction5_2-mean_interaction5_)^2+(interaction5_3-mean_interaction5_)^2+(interaction5_4-mean_interaction5_)^2+(interaction5_5-mean_interaction5_)^2+(interaction5_6-mean_interaction5_)^2+(interaction5_7-mean_interaction5_)^2+(interaction5_8-mean_interaction5_)^2+(interaction5_9-mean_interaction5_)^2+(interaction5_10-mean_interaction5_)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "HavePlan coefficient: " %9.5f mean_treatment
display "HavePlan std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.HavePlan#c.CLSIZE coefficient: " %9.5f mean_interaction1_
display "c.HavePlan#c.CLSIZE std.err: " %9.5f (var1interaction1_+var2interaction1_)^(1/2)

display "c.HavePlan#c.STUBEHA coefficient: " %9.5f mean_interaction2_
display "c.HavePlan#c.STUBEHA std.err: " %9.5f (var1interaction2_+var2interaction2_)^(1/2)

display "c.HavePlan#c.TEACHBEHA coefficient: " %9.5f mean_interaction3_
display "c.HavePlan#c.TEACHBEHA std.err: " %9.5f (var1interaction3_+var2interaction3_)^(1/2)

display "c.HavePlan#c.SCHSIZE  coefficient: " %9.5f mean_interaction4_
display "c.HavePlan#c.SCHSIZE  std.err: " %9.5f (var1interaction4_+var2interaction4_)^(1/2)

display "c.HavePlan#c.EDUSHORT coefficient: " %9.5f mean_interaction5_
display "c.HavePlan#c.EDUSHORT std.err: " %9.5f (var1interaction5_+var2interaction5_)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons





******************
*    - High      *
******************

** OLS without covariates
forvalues i=1(1)10 {
  reg PV`i'SCIE High [pw=W_FSTUWT] , cluster(CNTSCHID)
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


** OLS with interactions
forvalues i=1(1)10 {
  reg PV`i'SCIE High $x c.High#c.CLSIZE c.High#c.STUBEHA c.High#c.TEACHBEHA c.High#c.SCHSIZE c.High#c.EDUSHORT [pw=W_FSTUWT] , cluster(CNTSCHID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen interaction1_`i'=_b[c.High#c.CLSIZE]
  gen SDinteraction1_`i'=_se[c.High#c.CLSIZE]
  gen interaction2_`i'=_b[c.High#c.STUBEHA]
  gen SDinteraction2_`i'=_se[c.High#c.STUBEHA]
  gen interaction3_`i'=_b[c.High#c.TEACHBEHA]
  gen SDinteraction3_`i'=_se[c.High#c.TEACHBEHA]
  gen interaction4_`i'=_b[c.High#c.SCHSIZE]
  gen SDinteraction4_`i'=_se[c.High#c.SCHSIZE]
  gen interaction5_`i'=_b[c.High#c.EDUSHORT]
  gen SDinteraction5_`i'=_se[c.High#c.EDUSHORT]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_interaction1_=(interaction1_1+interaction1_2+interaction1_3+interaction1_4+interaction1_5+interaction1_6+interaction1_7+interaction1_8+interaction1_9+interaction1_10)/10
gen var1interaction1_=(SDinteraction1_1^2+SDinteraction1_2^2+SDinteraction1_3^2+SDinteraction1_4^2+SDinteraction1_5^2+SDinteraction1_6^2+SDinteraction1_7^2+SDinteraction1_8^2+SDinteraction1_9^2+SDinteraction1_10^2)/10
gen var2interaction1_=(1+(1/10))*(1/9)*((interaction1_1-mean_interaction1_)^2+(interaction1_2-mean_interaction1_)^2+(interaction1_3-mean_interaction1_)^2+(interaction1_4-mean_interaction1_)^2+(interaction1_5-mean_interaction1_)^2+(interaction1_6-mean_interaction1_)^2+(interaction1_7-mean_interaction1_)^2+(interaction1_8-mean_interaction1_)^2+(interaction1_9-mean_interaction1_)^2+(interaction1_10-mean_interaction1_)^2)

gen mean_interaction2_=(interaction2_1+interaction2_2+interaction2_3+interaction2_4+interaction2_5+interaction2_6+interaction2_7+interaction2_8+interaction2_9+interaction2_10)/10
gen var1interaction2_=(SDinteraction2_1^2+SDinteraction2_2^2+SDinteraction2_3^2+SDinteraction2_4^2+SDinteraction2_5^2+SDinteraction2_6^2+SDinteraction2_7^2+SDinteraction2_8^2+SDinteraction2_9^2+SDinteraction2_10^2)/10
gen var2interaction2_=(1+(1/10))*(1/9)*((interaction2_1-mean_interaction2_)^2+(interaction2_2-mean_interaction2_)^2+(interaction2_3-mean_interaction2_)^2+(interaction2_4-mean_interaction2_)^2+(interaction2_5-mean_interaction2_)^2+(interaction2_6-mean_interaction2_)^2+(interaction2_7-mean_interaction2_)^2+(interaction2_8-mean_interaction2_)^2+(interaction2_9-mean_interaction2_)^2+(interaction2_10-mean_interaction2_)^2)

gen mean_interaction3_=(interaction3_1+interaction3_2+interaction3_3+interaction3_4+interaction3_5+interaction3_6+interaction3_7+interaction3_8+interaction3_9+interaction3_10)/10
gen var1interaction3_=(SDinteraction3_1^2+SDinteraction3_2^2+SDinteraction3_3^2+SDinteraction3_4^2+SDinteraction3_5^2+SDinteraction3_6^2+SDinteraction3_7^2+SDinteraction3_8^2+SDinteraction3_9^2+SDinteraction3_10^2)/10
gen var2interaction3_=(1+(1/10))*(1/9)*((interaction3_1-mean_interaction3_)^2+(interaction3_2-mean_interaction3_)^2+(interaction3_3-mean_interaction3_)^2+(interaction3_4-mean_interaction3_)^2+(interaction3_5-mean_interaction3_)^2+(interaction3_6-mean_interaction3_)^2+(interaction3_7-mean_interaction3_)^2+(interaction3_8-mean_interaction3_)^2+(interaction3_9-mean_interaction3_)^2+(interaction3_10-mean_interaction3_)^2)

gen mean_interaction4_=(interaction4_1+interaction4_2+interaction4_3+interaction4_4+interaction4_5+interaction4_6+interaction4_7+interaction4_8+interaction4_9+interaction4_10)/10
gen var1interaction4_=(SDinteraction4_1^2+SDinteraction4_2^2+SDinteraction4_3^2+SDinteraction4_4^2+SDinteraction4_5^2+SDinteraction4_6^2+SDinteraction4_7^2+SDinteraction4_8^2+SDinteraction4_9^2+SDinteraction4_10^2)/10
gen var2interaction4_=(1+(1/10))*(1/9)*((interaction4_1-mean_interaction4_)^2+(interaction4_2-mean_interaction4_)^2+(interaction4_3-mean_interaction4_)^2+(interaction4_4-mean_interaction4_)^2+(interaction4_5-mean_interaction4_)^2+(interaction4_6-mean_interaction4_)^2+(interaction4_7-mean_interaction4_)^2+(interaction4_8-mean_interaction4_)^2+(interaction4_9-mean_interaction4_)^2+(interaction4_10-mean_interaction4_)^2)

gen mean_interaction5_=(interaction5_1+interaction5_2+interaction5_3+interaction5_4+interaction5_5+interaction5_6+interaction5_7+interaction5_8+interaction5_9+interaction5_10)/10
gen var1interaction5_=(SDinteraction5_1^2+SDinteraction5_2^2+SDinteraction5_3^2+SDinteraction5_4^2+SDinteraction5_5^2+SDinteraction5_6^2+SDinteraction5_7^2+SDinteraction5_8^2+SDinteraction5_9^2+SDinteraction5_10^2)/10
gen var2interaction5_=(1+(1/10))*(1/9)*((interaction5_1-mean_interaction5_)^2+(interaction5_2-mean_interaction5_)^2+(interaction5_3-mean_interaction5_)^2+(interaction5_4-mean_interaction5_)^2+(interaction5_5-mean_interaction5_)^2+(interaction5_6-mean_interaction5_)^2+(interaction5_7-mean_interaction5_)^2+(interaction5_8-mean_interaction5_)^2+(interaction5_9-mean_interaction5_)^2+(interaction5_10-mean_interaction5_)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.CLSIZE coefficient: " %9.5f mean_interaction1_
display "c.High#c.CLSIZE std.err: " %9.5f (var1interaction1_+var2interaction1_)^(1/2)

display "c.High#c.STUBEHA coefficient: " %9.5f mean_interaction2_
display "c.High#c.STUBEHA std.err: " %9.5f (var1interaction2_+var2interaction2_)^(1/2)

display "c.High#c.TEACHBEHA coefficient: " %9.5f mean_interaction3_
display "c.High#c.TEACHBEHA std.err: " %9.5f (var1interaction3_+var2interaction3_)^(1/2)

display "c.High#c.SCHSIZE  coefficient: " %9.5f mean_interaction4_
display "c.High#c.SCHSIZE  std.err: " %9.5f (var1interaction4_+var2interaction4_)^(1/2)

display "c.High#c.EDUSHORT coefficient: " %9.5f mean_interaction5_
display "c.High#c.EDUSHORT std.err: " %9.5f (var1interaction5_+var2interaction5_)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons

** FE with interactions
forvalues i=1(1)10 {
  reghdfe PV`i'SCIE High $x c.High#c.CLSIZE c.High#c.STUBEHA c.High#c.TEACHBEHA c.High#c.SCHSIZE c.High#c.EDUSHORT  [pw=W_FSTUWT] , cluster(CNTSCHID) absorb(CNTRYID)
  gen treatment`i'=_b[High]
  gen SDtreatment`i'=_se[High]
  gen interaction1_`i'=_b[c.High#c.CLSIZE]
  gen SDinteraction1_`i'=_se[c.High#c.CLSIZE]
  gen interaction2_`i'=_b[c.High#c.STUBEHA]
  gen SDinteraction2_`i'=_se[c.High#c.STUBEHA]
  gen interaction3_`i'=_b[c.High#c.TEACHBEHA]
  gen SDinteraction3_`i'=_se[c.High#c.TEACHBEHA]
  gen interaction4_`i'=_b[c.High#c.SCHSIZE]
  gen SDinteraction4_`i'=_se[c.High#c.SCHSIZE]
  gen interaction5_`i'=_b[c.High#c.EDUSHORT]
  gen SDinteraction5_`i'=_se[c.High#c.EDUSHORT]
  gen cons`i'=_b[_cons]
  gen SDcons`i'=_se[_cons]
  gen r2_`i'=e(r2)
  }
gen mean_treatment=(treatment1+treatment2+treatment3+treatment4+treatment5+treatment6+treatment7+treatment8+treatment9+treatment10)/10
gen var1treatment=(SDtreatment1^2+SDtreatment2^2+SDtreatment3^2+SDtreatment4^2+SDtreatment5^2+SDtreatment6^2+SDtreatment7^2+SDtreatment8^2+SDtreatment9^2+SDtreatment10^2)/10
gen var2treatment=(1+(1/10))*(1/9)*((treatment1-mean_treatment)^2+(treatment2-mean_treatment)^2+(treatment3-mean_treatment)^2+(treatment4-mean_treatment)^2+(treatment5-mean_treatment)^2+(treatment6-mean_treatment)^2+(treatment7-mean_treatment)^2+(treatment8-mean_treatment)^2+(treatment9-mean_treatment)^2+(treatment10-mean_treatment)^2)

gen mean_interaction1_=(interaction1_1+interaction1_2+interaction1_3+interaction1_4+interaction1_5+interaction1_6+interaction1_7+interaction1_8+interaction1_9+interaction1_10)/10
gen var1interaction1_=(SDinteraction1_1^2+SDinteraction1_2^2+SDinteraction1_3^2+SDinteraction1_4^2+SDinteraction1_5^2+SDinteraction1_6^2+SDinteraction1_7^2+SDinteraction1_8^2+SDinteraction1_9^2+SDinteraction1_10^2)/10
gen var2interaction1_=(1+(1/10))*(1/9)*((interaction1_1-mean_interaction1_)^2+(interaction1_2-mean_interaction1_)^2+(interaction1_3-mean_interaction1_)^2+(interaction1_4-mean_interaction1_)^2+(interaction1_5-mean_interaction1_)^2+(interaction1_6-mean_interaction1_)^2+(interaction1_7-mean_interaction1_)^2+(interaction1_8-mean_interaction1_)^2+(interaction1_9-mean_interaction1_)^2+(interaction1_10-mean_interaction1_)^2)

gen mean_interaction2_=(interaction2_1+interaction2_2+interaction2_3+interaction2_4+interaction2_5+interaction2_6+interaction2_7+interaction2_8+interaction2_9+interaction2_10)/10
gen var1interaction2_=(SDinteraction2_1^2+SDinteraction2_2^2+SDinteraction2_3^2+SDinteraction2_4^2+SDinteraction2_5^2+SDinteraction2_6^2+SDinteraction2_7^2+SDinteraction2_8^2+SDinteraction2_9^2+SDinteraction2_10^2)/10
gen var2interaction2_=(1+(1/10))*(1/9)*((interaction2_1-mean_interaction2_)^2+(interaction2_2-mean_interaction2_)^2+(interaction2_3-mean_interaction2_)^2+(interaction2_4-mean_interaction2_)^2+(interaction2_5-mean_interaction2_)^2+(interaction2_6-mean_interaction2_)^2+(interaction2_7-mean_interaction2_)^2+(interaction2_8-mean_interaction2_)^2+(interaction2_9-mean_interaction2_)^2+(interaction2_10-mean_interaction2_)^2)

gen mean_interaction3_=(interaction3_1+interaction3_2+interaction3_3+interaction3_4+interaction3_5+interaction3_6+interaction3_7+interaction3_8+interaction3_9+interaction3_10)/10
gen var1interaction3_=(SDinteraction3_1^2+SDinteraction3_2^2+SDinteraction3_3^2+SDinteraction3_4^2+SDinteraction3_5^2+SDinteraction3_6^2+SDinteraction3_7^2+SDinteraction3_8^2+SDinteraction3_9^2+SDinteraction3_10^2)/10
gen var2interaction3_=(1+(1/10))*(1/9)*((interaction3_1-mean_interaction3_)^2+(interaction3_2-mean_interaction3_)^2+(interaction3_3-mean_interaction3_)^2+(interaction3_4-mean_interaction3_)^2+(interaction3_5-mean_interaction3_)^2+(interaction3_6-mean_interaction3_)^2+(interaction3_7-mean_interaction3_)^2+(interaction3_8-mean_interaction3_)^2+(interaction3_9-mean_interaction3_)^2+(interaction3_10-mean_interaction3_)^2)

gen mean_interaction4_=(interaction4_1+interaction4_2+interaction4_3+interaction4_4+interaction4_5+interaction4_6+interaction4_7+interaction4_8+interaction4_9+interaction4_10)/10
gen var1interaction4_=(SDinteraction4_1^2+SDinteraction4_2^2+SDinteraction4_3^2+SDinteraction4_4^2+SDinteraction4_5^2+SDinteraction4_6^2+SDinteraction4_7^2+SDinteraction4_8^2+SDinteraction4_9^2+SDinteraction4_10^2)/10
gen var2interaction4_=(1+(1/10))*(1/9)*((interaction4_1-mean_interaction4_)^2+(interaction4_2-mean_interaction4_)^2+(interaction4_3-mean_interaction4_)^2+(interaction4_4-mean_interaction4_)^2+(interaction4_5-mean_interaction4_)^2+(interaction4_6-mean_interaction4_)^2+(interaction4_7-mean_interaction4_)^2+(interaction4_8-mean_interaction4_)^2+(interaction4_9-mean_interaction4_)^2+(interaction4_10-mean_interaction4_)^2)

gen mean_interaction5_=(interaction5_1+interaction5_2+interaction5_3+interaction5_4+interaction5_5+interaction5_6+interaction5_7+interaction5_8+interaction5_9+interaction5_10)/10
gen var1interaction5_=(SDinteraction5_1^2+SDinteraction5_2^2+SDinteraction5_3^2+SDinteraction5_4^2+SDinteraction5_5^2+SDinteraction5_6^2+SDinteraction5_7^2+SDinteraction5_8^2+SDinteraction5_9^2+SDinteraction5_10^2)/10
gen var2interaction5_=(1+(1/10))*(1/9)*((interaction5_1-mean_interaction5_)^2+(interaction5_2-mean_interaction5_)^2+(interaction5_3-mean_interaction5_)^2+(interaction5_4-mean_interaction5_)^2+(interaction5_5-mean_interaction5_)^2+(interaction5_6-mean_interaction5_)^2+(interaction5_7-mean_interaction5_)^2+(interaction5_8-mean_interaction5_)^2+(interaction5_9-mean_interaction5_)^2+(interaction5_10-mean_interaction5_)^2)

gen mean_cons=(cons1+cons2+cons3+cons4+cons5+cons6+cons7+cons8+cons9+cons10)/10
gen var1cons=(SDcons1^2+SDcons2^2+SDcons3^2+SDcons4^2+SDcons5^2+SDcons6^2+SDcons7^2+SDcons8^2+SDcons9^2+SDcons10^2)/10
gen var2cons=(1+(1/10))*(1/9)*((cons1-mean_cons)^2+(cons2-mean_cons)^2+(cons3-mean_cons)^2+(cons4-mean_cons)^2+(cons5-mean_cons)^2+(cons6-mean_cons)^2+(cons7-mean_cons)^2+(cons8-mean_cons)^2+(cons9-mean_cons)^2+(cons10-mean_cons)^2)

display "High coefficient: " %9.5f mean_treatment
display "High std.err: " %9.5f (var1treatment+var2treatment)^(1/2)

display "c.High#c.CLSIZE coefficient: " %9.5f mean_interaction1_
display "c.High#c.CLSIZE std.err: " %9.5f (var1interaction1_+var2interaction1_)^(1/2)

display "c.High#c.STUBEHA coefficient: " %9.5f mean_interaction2_
display "c.High#c.STUBEHA std.err: " %9.5f (var1interaction2_+var2interaction2_)^(1/2)

display "c.High#c.TEACHBEHA coefficient: " %9.5f mean_interaction3_
display "c.High#c.TEACHBEHA std.err: " %9.5f (var1interaction3_+var2interaction3_)^(1/2)

display "c.High#c.SCHSIZE  coefficient: " %9.5f mean_interaction4_
display "c.High#c.SCHSIZE  std.err: " %9.5f (var1interaction4_+var2interaction4_)^(1/2)

display "c.High#c.EDUSHORT coefficient: " %9.5f mean_interaction5_
display "c.High#c.EDUSHORT std.err: " %9.5f (var1interaction5_+var2interaction5_)^(1/2)

display "Cons coefficient: " %9.5f mean_cons
display "Cons std.err: " %9.5f (var1cons+var2cons)^(1/2)

display "r2:" %9.5f (r2_1+r2_2+r2_3+r2_4+r2_5+r2_6+r2_7+r2_8+r2_9+r2_10)/10

drop treatment1-var2cons