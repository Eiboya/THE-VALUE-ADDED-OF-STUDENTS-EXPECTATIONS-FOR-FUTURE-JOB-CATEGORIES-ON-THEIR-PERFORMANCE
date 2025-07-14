
//Please download school and student data from the PISA 2015 and 2018 databases first.

clear

preserve
import sas SC001Q01TA SCHLTYPE CNTSCHID CLSIZE SCHSIZE STUBEHA TEACHBEHA EDUSHORT using "D:\zhdata\tatat\cy6_ms_cmb_sch_qqq.sas7bdat"
save school2015.dta,replace


import sas CNTRYID CNT CNTSCHID CNTSTUID ST004D01T SMINS PV1SCIE PV2SCIE PV3SCIE PV4SCIE PV5SCIE PV6SCIE PV7SCIE PV8SCIE PV9SCIE PV10SCIE OCOD3 OCOD1 OCOD2 ANXTEST MOTIVAT EMOSUPS ESCS GRADE TEACHSUP ICTRES INSTSCIE PA003Q08NA PA032Q03TA W_FSTUWT JOYSCIE  INTBRSCI DISCLISCI IBTEACH SCIEACT TDTEACH using "D:\zhdata\tatat\cy6_ms_cmb_stu_qqq.sas7bdat"
save stu2015.dta,replace

use stu2015.dta,clear
merge m:1 CNTSCHID using school2015.dta
drop _merge
save WORLDstu2015, replace

preserve
clear
import sas CNTRYID CNT CNTSCHID TC045Q12NA TC045Q12NB using "D:\zhdata\tatat\cy6_ms_cmb_tch_qqq.sas7bdat"
bysort CNTSCHID: egen slectsize=count(CNTSCHID)
by CNTSCHID: generate i = _n
gen guide1 = .
gen guide2 = .

* summarizing the hierarchy
codebook slectsize if i == 1
egen c = group(CNTSCHID)       
levels c, local(cluster)

qui foreach j of local cluster {
  sum TC045Q12NA if c==`j'
  replace guide1=r(mean) if c==`j'
  sum TC045Q12NB if c==`j'
  replace guide2=r(mean) if c==`j'
}
keep CNTRYID CNT CNTSCHID guide1 guide2
save d22015.dta,replace
use  d22015.dta,clear
bysort CNTSCHID: egen slectsize=count(CNTSCHID)
by CNTSCHID: generate i = _n
keep if i==1
save d22015.dta,replace



use WORLDstu2015.dta,clear
merge m:1 CNTSCHID using d22015.dta
drop _merge
drop i
drop slectsize
save WORLDstudent2015.dta, replace


clear

preserve
import sas SC001Q01TA SCHLTYPE CNTSCHID CLSIZE SCHSIZE STUBEHA TEACHBEHA EDUSHORT using "D:\zhdata\tatat\cy07_msu_sch_qqq.sas7bdat"
save school2018.dta,replace

import sas CNTRYID CNT CNTSCHID CNTSTUID ST004D01T SMINS PV1SCIE PV2SCIE PV3SCIE PV4SCIE PV5SCIE PV6SCIE PV7SCIE PV8SCIE PV9SCIE PV10SCIE OCOD3 OCOD1 OCOD2 COMPETE EMOSUPS ESCS GRADE TEACHSUP ICTRES INSTSCIE PA003Q08NA PA032Q03TA W_FSTUWT JOYSCIE  INTBRSCI DISCLISCI IBTEACH SCIEACT TDTEACH using "D:\zhdata\tatat\cy07_msu_stu_qqq.sas7bdat"
save stu2018.dta,replace

use stu2018.dta,clear
merge m:1 CNTSCHID using school2018.dta
drop _merge
save WORLDstu2018, replace

preserve
clear
import sas CNTRYID CNT CNTSCHID TC045Q12NA TC045Q12NB using "D:\zhdata\tatat\cy07_msu_tch_qqq.sas7bdat"
bysort CNTSCHID: egen slectsize=count(CNTSCHID)
by CNTSCHID: generate i = _n
gen guide1 = .
gen guide2 = .

* summarizing the hierarchy
codebook slectsize if i == 1
egen c = group(CNTSCHID)       
levels c, local(cluster)

qui foreach j of local cluster {
  sum TC045Q12NA if c==`j'
  replace guide1=r(mean) if c==`j'
  sum TC045Q12NB if c==`j'
  replace guide2=r(mean) if c==`j'
}
keep CNTRYID CNT CNTSCHID guide1 guide2
save d22018.dta,replace
use  d22018.dta,clear
bysort CNTSCHID: egen slectsize=count(CNTSCHID)
by CNTSCHID: generate i = _n
keep if i==1
save d22018.dta,replace



use WORLDstu2018.dta,clear
merge m:1 CNTSCHID using d22018.dta
drop _merge
drop i
drop slectsize
save WORLDstudent2018.dta, replace

