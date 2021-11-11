* This program calculates bias-adjusted estimates and SEs for each assessor and takes medians to pool across assessors.
* Adjusted results are obtained first with allowance for additive biases only, and then with allowance for additive and proportional biases.
* Performs bias-adjusted meta-analyses


clear
set more off

* Merge proportional and additive bias data files
use propbias.dta

sort study assessor
merge study assessor using addbias.dta
keep if assessor == 1

drop _merge assessor

* Calculate additive only and fully bias-adjusted estimates and variances for each assessor
 
gen estadjall=(estlogor-addimn-propimn*addemn)/(propimn*propemn)

gen varadjall=(((propimn^2)+propivar)*(propevar*(estadjall^2)+addevar) + propivar*((propemn*estadjall+addemn)^2) + addivar + varlogor)/((propimn*propemn)^2)

* Bias-adjusted results obtained as medians (across assessors) of bias-adjusted estimates and SEs, for each study
keep study estlogor varlogor estadjall* varadjall*

gen seadjall=sqrt(varadjall)
lab var estadjall "Bias-adjusted estimate (all bias)"
lab var seadjall "Bias-adjusted SE (all bias)"

* Perform bias-adjusted meta-analyses

* Unadjusted meta-analysis
gen selogor=sqrt(varlogor)
metan estlogor selogor, random
* Meta-analysis adjusted for all bias
metan estadjall seadjall, random

save biasadj.dta, replace