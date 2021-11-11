* This program calculates bias-adjusted estimates and SEs for each assessor and takes medians to pool across assessors.
* Adjusted results are obtained first with allowance for additive biases only, and then with allowance for additive and proportional biases.
* Performs bias-adjusted meta-analyses


clear
set more off

* Merge proportional and additive bias data files
use propbias.dta
sort study assessor
merge study assessor using addbias.dta
drop _merge
reshape wide add* prop*, i(study) j(assessor)



* Calculate additive only and fully bias-adjusted estimates and variances for each assessor
forvalues i=1/1 {
   gen estadjadd`i'=estlogor-addimn`i'-addemn`i'
   gen varadjadd`i'=varlogor+addivar`i'+addevar`i'
   gen estadjall`i'=(estlogor-addimn`i'-propimn`i'*addemn`i')/(propimn`i'*propemn`i')
   gen varadjall`i'=(((propimn`i'^2)+propivar`i')*(propevar`i'*(estadjall`i'^2)+addevar`i') + propivar`i'*((propemn`i'*estadjall`i'+addemn`i')^2) + addivar`i' + varlogor)/((propimn`i'*propemn`i')^2)
}

* Bias-adjusted results obtained as medians (across assessors) of bias-adjusted estimates and SEs, for each study
keep study estlogor varlogor estadjadd* varadjadd* estadjall* varadjall*
reshape long estadjadd varadjadd estadjall varadjall, i(study) j(assessor) string
sort study assessor
gen seadjadd=sqrt(varadjadd)
gen seadjall=sqrt(varadjall)
egen estadjaddm=median(estadjadd), by(study)
egen seadjaddm=median(seadjadd), by(study)
egen estadjallm=median(estadjall), by(study)
egen seadjallm=median(seadjall), by(study)
reshape wide estadjadd varadjadd seadjadd estadjall varadjall seadjall, i(study) j(assessor) string

lab var estadjaddm "Bias-adjusted estimate (additive bias only)"
lab var seadjaddm "Bias-adjusted SE (additive bias only)"
lab var estadjallm "Bias-adjusted estimate (all bias)"
lab var seadjallm "Bias-adjusted SE (all bias)"

* Perform bias-adjusted meta-analyses

* Unadjusted meta-analysis
gen selogor=sqrt(varlogor)
metan estlogor selogor, random
* Meta-analysis adjusted for additive bias
metan estadjaddm seadjaddm, random
* Meta-analysis adjusted for all bias
metan estadjallm seadjallm, random

save biasadj.dta, replace