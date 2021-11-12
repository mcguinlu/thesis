* This program calculates means and variances for internal and external additive biases, using elicited bias ranges.
* Unadjusted study results are calculated from observed study data.


clear
set more off

* Read in elicited bias ranges
insheet using additivebiasdata_example.txt, tab names

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
qui reshape wide sela selb pera perb atta attb deta detb otha othb popa popb inta intb cona conb outa outb, i(study) j(assessor)

* Input observed study data and calculate unadjusted logOR, variance and 95% confidence limits
input r1 n1 r0 n0
60 212 68 180
167 485 190 491
371 1140 427 1153
106 335 151 360
680 2240 857 2169
162 559 237 552
101 342 135 374
161 503 287 757
gen estlogor=log((r1*(n0-r0))/(r0*(n1-r1)))
gen varlogor=(1/r1)+(1/(n1-r1))+(1/r0)+(1/(n0-r0))
gen lounadj=estlogor-invnorm(0.975)*sqrt(varlogor)
gen hiunadj=estlogor+invnorm(0.975)*sqrt(varlogor)
order study r1 n1 r0 n0 estlogor varlogor


* Calculate bias mean and sd from range limits, for each additive bias and each assessor
foreach bias in sel per att det oth pop int con out {
   forvalues i = 1/4 {
      gen est`bias'`i'=(log(`bias'a`i')+log(`bias'b`i'))/2
      gen sd`bias'`i'=(log(`bias'b`i')-log(`bias'a`i'))/2
      gen var`bias'`i'=sd`bias'`i'^2
   }
}

* Calculate mean and variance of total internal additive bias and total external additive bias, for each assessor
forvalues i = 1/4 {
   egen addimn`i'=rsum(estsel`i' estper`i' estatt`i' estdet`i' estoth`i')
   egen addemn`i'=rsum(estpop`i' estint`i' estcon`i' estout`i')
   egen addivar`i'=rsum(varsel`i' varper`i' varatt`i' vardet`i' varoth`i')
   egen addevar`i'=rsum(varpop`i' varint`i' varcon`i' varout`i')
}

* Save data set containing unadjusted study results together with elicited means and variances for additive biases
keep study estlogor varlogor add*
reshape long addimn addemn addivar addevar, i(study) j(assessor)
sort study assessor
save addbias.dta, replace

