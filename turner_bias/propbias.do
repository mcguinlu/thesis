* This program calculates means and variances for internal and external proportional biases, using elicited bias ranges.
* A symmetric distribution is fitted to logs of ranges for proportional biases, ignoring bounds (see section 6.2 of paper).


clear
set more off

* Read in elicited bias ranges
insheet using propbiasdata_example.txt, tab names

* Create numeric, labelled copies of text fields
egen studyid=group(study)
lab def studylab 1 Study1 2 Study2 3 Study3 4 Study4 5 Study5 6 Study6 7 Study7 8 Study8
lab val studyid studylab
egen assessid=group(assessor)
lab def assesslab 1 A 2 B 3 C 4 D
lab val assessid assesslab
drop study assessor
rename studyid study
rename assessid assessor


* Reshape the data set from long format to wide (one row for each study)
reshape long lo hi, i(study assessor) j(result) string
drop if study==.
lab def result 8 "per" 7 "att" 6 "det" 5 "pop" 4 "dose" 3 "tim" 2 "del" 1 "out"
gen external=(result=="pop"|result=="dose"|result=="tim"|result=="del"|result=="out")
encode result, gen(resultnum) label(result)


* Fit symmetric distribution to biases on log scale, ignoring bounds
gen logbiasmean=(log(lo)+log(hi))/2
gen logbiasvar=((log(hi)-log(lo))/2)^2


* Combine proportional biases for each study and each assessor, assuming lognormality for product of biases, for internal and external biases separately
egen sumlogmn=sum(logbiasmean), by(study assessor external)
egen sumlogvr=sum(logbiasvar), by(study assessor external)
gen pooledmn=exp(sumlogmn+sumlogvr/2)
gen pooledvr=exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1)


* Save data set containing elicited means and variances for proportional biases
collapse (mean) sumlogmn sumlogvr pooledmn pooledvr, by(study assessor external)
reshape wide sumlogmn sumlogvr pooledmn pooledvr, i(study assessor) j(external)
rename pooledmn0 propimn
rename pooledvr0 propivar
rename pooledmn1 propemn
rename pooledvr1 propevar
keep study assessor prop*
sort study assessor
save propbias.dta, replace


