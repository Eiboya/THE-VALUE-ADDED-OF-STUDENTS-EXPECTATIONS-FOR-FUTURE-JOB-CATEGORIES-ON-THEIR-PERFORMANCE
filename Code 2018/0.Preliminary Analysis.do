clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2018.dta"

global x "ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

sum ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 [aw=W_FSTUWT]

sum science [aw=W_FSTUWT] if HavePlan==0
sum science [aw=W_FSTUWT] if HavePlan==1
sum science [aw=W_FSTUWT] if High==0
sum science [aw=W_FSTUWT] if High==1

///////////////////////////////////////////////////////////////////////////////
/////////////////////PART I : Preliminary analysis ////////////////////////////
///////////////////////////////////////////////////////////////////////////////

** Blancing Test for Treatment: HavePlan

summarize science if HavePlan==1
summarize science if HavePlan==0

mat y = J(17,4,.)	
local k=1
foreach var of varlist ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 {

sum `var' [aw=W_FSTUWT] if HavePlan==1
mat y[`k',1] = r(mean)
sum `var' [aw=W_FSTUWT] if HavePlan==0
mat y[`k',2] = r(mean)
reg `var' HavePlan [pw=W_FSTUWT], r 
test HavePlan==0
mat y[`k', 3]=r(p)
reg `var' HavePlan [pw=W_FSTUWT], vce(cluster CNTSCHID) 
test HavePlan==0
mat y[`k', 4]=r(p)

local k=`k'+1
}

count if HavePlan==1
mat y[17,1]=r(N)
count if HavePlan==0
mat y[17,2]=r(N)

mat rownames y = ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 "Sample Size"
mat colnames y = "HavePlan" "Don't HavePlan""P-value without clustering" "P-value with clustering"
mat2txt, matrix(y) saving("C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\balanceHavePlan2018withweights.xls") replace


** Blancing Test for Treatment: High

summarize science if High==1
summarize science if High==0

mat y = J(17,4,.)	
local k=1
foreach var of varlist ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 {

sum `var' [aw=W_FSTUWT] if High==1
mat y[`k',1] = r(mean)
sum `var' [aw=W_FSTUWT] if High==0
mat y[`k',2] = r(mean)
reg `var' High [pw=W_FSTUWT], r 
test High==0
mat y[`k', 3]=r(p)
reg `var' High [pw=W_FSTUWT], vce(cluster CNTSCHID) 
test High==0
mat y[`k', 4]=r(p)

local k=`k'+1
}

count if High==1
mat y[17,1]=r(N)
count if High==0
mat y[17,2]=r(N)

mat rownames y = ESCS SMINS COMPETE TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 "Sample Size"
mat colnames y = "High" "Not High""P-value without clustering" "P-value with clustering"
mat2txt, matrix(y) saving("C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\balanceHigh2018withweights.xls") replace

/*

*** ==PART I. Selection of covariates for matching - HavePlan==
psestimate HavePlan, totry(ESCS SMINS COMPETE TEACHSUP EMOSUPS female  GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT)

// Final model is: EMOSUPS COMPETE female SCHSIZE STUBEHA TEACHSUP EDUSHORT ESCS GRADE ICTRES SMINS c.STUBEHA#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SCHSIZE#c.SCHSIZE c.EDUSHORT#c.STUBEHA c.SMINS#c.SCHSIZE c.GRADE#c.GRADE c.ESCS#c.EMOSUPS c.TEACHSUP#c.STUBEHA c.ESCS#c.ESCS c.GRADE#c.COMPETE c.GRADE#c.EMOSUPS c.SCHSIZE#c.EMOSUPS c.TEACHSUP#c.COMPETE c.ICTRES#c.SCHSIZE c.ESCS#c.SCHSIZE c.ICTRES#c.ESCS c.EDUSHORT#c.EMOSUPS c.ESCS#c.TEACHSUP



psestimate High, totry(ESCS SMINS COMPETE TEACHSUP EMOSUPS female  GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT)

// Final model is: ESCS female GRADE SMINS STUBEHA EMOSUPS COMPETE TEACHSUP CLSIZE TEACHBEHA SCHSIZE c.SCHSIZE#c.CLSIZE c.ESCS#c.ESCS c.SCHSIZE#c.SMINS c.SCHSIZE#c.SCHSIZE c.EMOSUPS#c.EMOSUPS c.SMINS#c.SMINS c.SCHSIZE#c.female c.TEACHBEHA#c.STUBEHA c.TEACHSUP#c.STUBEHA c.COMPETE#c.EMOSUPS c.CLSIZE#c.ESCS c.GRADE#c.ESCS c.SCHSIZE#c.ESCS c.TEACHSUP#c.TEACHSUP c.SCHSIZE#c.COMPETE c.SCHSIZE#c.GRADE c.CLSIZE#c.GRADE c.SCHSIZE#c.STUBEHA c.TEACHBEHA#c.CLSIZE c.STUBEHA#c.STUBEHA c.STUBEHA#c.SMINS c.COMPETE#c.ESCS c.GRADE#c.GRADE






