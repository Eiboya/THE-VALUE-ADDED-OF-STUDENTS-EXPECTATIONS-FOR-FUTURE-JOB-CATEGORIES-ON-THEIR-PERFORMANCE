clear
use "C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\WORLDstudent2015.dta"

global x "ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT"

sum ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 [aw=W_FSTUWT]

sum science [aw=W_FSTUWT] if HavePlan==0
sum science [aw=W_FSTUWT] if HavePlan==1
sum science [aw=W_FSTUWT] if High==0
sum science [aw=W_FSTUWT] if High==1

///////////////////////////////////////////////////////////////////////////////
/////////////////////PART I : Preliminary analysis ////////////////////////////
///////////////////////////////////////////////////////////////////////////////

/// A table of summary statistics///

** Blancing Test for Treatment: HavePlan

summarize science if HavePlan==1
summarize science if HavePlan==0

mat y = J(17,4,.)	
local k=1
foreach var of varlist ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 {

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

mat rownames y = ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 "Sample Size"
mat colnames y = "HavePlan" "Don't HavePlan""P-value without clustering" "P-value with clustering"
mat2txt, matrix(y) saving("C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\balanceHavePlan2015withweights.xls") replace


** Blancing Test for Treatment: High

summarize science if High==1
summarize science if High==0

mat y = J(17,4,.)	
local k=1
foreach var of varlist ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 {

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

mat rownames y = ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT guide1 guide2 "Sample Size"
mat colnames y = "High" "Not High""P-value without clustering" "P-value with clustering"
mat2txt, matrix(y) saving("C:\Users\dhxht\Desktop\Zhihan Zhang - Master 2 Thesis\balanceHigh2015withweights.xls") replace

/*

*** ==PART I. Selection of covariates for matching - HavePlan==
psestimate HavePlan, totry(ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female  GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT)

// Final model is: MOTIVAT CLSIZE SCHSIZE ICTRES STUBEHA EMOSUPS SMINS EDUSHORT ANXTEST TEACHSUP female ESCS c.SCHSIZE#c.CLSIZE c.ESCS#c.ESCS c.ANXTEST#c.ANXTEST c.ESCS#c.female c.ESCS#c.MOTIVAT c.STUBEHA#c.STUBEHA c.EDUSHORT#c.ICTRES c.ESCS#c.ICTRES c.female#c.EMOSUPS c.female#c.EDUSHORT c.ICTRES#c.CLSIZE c.female#c.MOTIVAT c.CLSIZE#c.CLSIZE c.ESCS#c.EMOSUPS c.EMOSUPS#c.STUBEHA c.TEACHSUP#c.SCHSIZE c.STUBEHA#c.SCHSIZE c.SCHSIZE#c.SCHSIZE c.TEACHSUP#c.CLSIZE 



psestimate High, totry(ESCS SMINS ANXTEST MOTIVAT TEACHSUP EMOSUPS female  GRADE ICTRES CLSIZE STUBEHA TEACHBEHA SCHSIZE EDUSHORT)

// Final model is: MOTIVAT ESCS female SMINS CLSIZE GRADE TEACHSUP ICTRES EDUSHORT EMOSUPS STUBEHA TEACHBEHA ANXTEST c.STUBEHA#c.ESCS c.female#c.ESCS c.ESCS#c.ESCS c.SMINS#c.SMINS c.STUBEHA#c.CLSIZE c.STUBEHA#c.female c.ANXTEST#c.ANXTEST c.GRADE#c.SMINS c.ANXTEST#c.female c.GRADE#c.ESCS c.CLSIZE#c.MOTIVAT c.EDUSHORT#c.EDUSHORT c.EMOSUPS#c.female c.CLSIZE#c.female c.TEACHSUP#c.female c.EDUSHORT#c.ICTRES c.STUBEHA#c.STUBEHA c.TEACHBEHA#c.MOTIVAT c.EMOSUPS#c.EMOSUPS c.TEACHSUP#c.TEACHSUP c.TEACHBEHA#c.female c.CLSIZE#c.CLSIZE c.ANXTEST#c.CLSIZE c.TEACHBEHA#c.SMINS c.EMOSUPS#c.CLSIZE c.ANXTEST#c.STUBEHA c.TEACHSUP#c.MOTIVAT c.EDUSHORT#c.GRADE c.STUBEHA#c.EDUSHORT c.EDUSHORT#c.CLSIZE c.TEACHBEHA#c.TEACHBEHA





