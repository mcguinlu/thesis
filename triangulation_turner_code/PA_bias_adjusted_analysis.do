******************************************************************************
* Stata do file to implement the analysis described by Thompson S et al, 2010*
*                                                                            *
* Do file requires 4 text files of data, also available on website           *
*                                                                            *
* Written for Stata SE Version 11.0                                          *
*                                                                            *
******************************************************************************

******************************************************************************
* ESTIMATING THE MEAN AND VARIANCE OF THE BIASES FOR EACH STUDY AND ASSESSOR *
******************************************************************************

/*
	INTERNAL BIASES - ADDITIVE
*/

insheet using "PA_internal_additive.txt", clear

/*
	Apply Fisher transformation to the upper and lower limits for each bias
*/

foreach var of varlist sela-othb {
	replace `var'=0.5*log((1+`var')/(1-`var'))
}

/*
	Calculate mean and variance of each bias
*/	

foreach bias in sel con exp att out oth {
	gen estbias_`bias'=(`bias'a+`bias'b)/2 
	gen varbias_`bias'=((`bias'b-`bias'a)/2)^2
}

/*
	Sum means and variances across all the additive biases, and save these in a dataset
*/	

egen int_add=rsum(estbias_sel estbias_con estbias_exp estbias_att estbias_out estbias_oth)
egen int_add_var=rsum(varbias_sel varbias_con varbias_exp varbias_att varbias_out varbias_oth)

keep study assessor int_add int_add_var
sort study assessor
save int_add, replace

/*
	INTERNAL BIASES - PROPORTIONAL
*/

insheet using "PA_internal_proportional.txt", clear

/*
	Calculate mean and variance of each log bias
*/	

foreach bias in sel con exp att out oth {
	gen estlogbias_`bias'=(log(`bias'a)+log(`bias'b))/2 
	gen varlogbias_`bias'=((log(`bias'b)-log(`bias'a))/2)^2
}

/*
	Sum means and variances of log biases across all the proportional biases, transform back to original scale, and save these in a dataset
*/	

egen sumestlogbias=rsum(estlogbias_sel estlogbias_con estlogbias_exp estlogbias_att estlogbias_out estlogbias_oth)
egen sumvarlogbias=rsum(varlogbias_sel varlogbias_con varlogbias_exp varlogbias_att varlogbias_out varlogbias_oth)

gen int_prop=exp(sumestlogbias+sumvarlogbias/2)
gen int_prop_var=exp(2*sumestlogbias+sumvarlogbias)*(exp(sumvarlogbias)-1)

keep study assessor int_prop int_prop_var
sort study assessor
save int_prop, replace

/*
	EXTERNAL BIASES - ADDITIVE
*/

insheet using "PA_external_additive.txt", clear

/*
	Apply Fisher transformation to the upper and lower limits for each bias
*/

foreach var of varlist popa-timb {
	replace `var'=0.5*log((1+`var')/(1-`var'))
}

/*
	Calculate mean and variance of each bias
*/	

foreach bias in pop exp out tim {
	gen estbias_`bias'=(`bias'a+`bias'b)/2 
	gen varbias_`bias'=((`bias'b-`bias'a)/2)^2
}

/*
	Sum means and variances across all the additive biases, and save these in a dataset
*/	

egen ext_add=rsum(estbias_pop estbias_exp estbias_out estbias_tim)
egen ext_add_var=rsum(varbias_pop varbias_exp varbias_out varbias_tim)

keep study assessor ext_add ext_add_var
sort study assessor
save ext_add, replace

/*
	EXTERNAL BIASES - PROPORTIONAL
*/

insheet using "PA_external_proportional.txt", clear

/*
	Calculate mean and variance of each log bias
*/	

foreach bias in pop exp out tim {
	gen estlogbias_`bias'=(log(`bias'a)+log(`bias'b))/2 
	gen varlogbias_`bias'=((log(`bias'b)-log(`bias'a))/2)^2
}

/*
	Sum means and variances of log biases across all the proportional biases, transform back to original scale, and save these in a dataset
*/	

egen sumestlogbias=rsum(estlogbias_pop estlogbias_exp estlogbias_out estlogbias_tim)
egen sumvarlogbias=rsum(varlogbias_pop varlogbias_exp varlogbias_out varlogbias_tim)

gen ext_prop=exp(sumestlogbias+sumvarlogbias/2)
gen ext_prop_var=exp(2*sumestlogbias+sumvarlogbias)*(exp(sumvarlogbias)-1)

keep study assessor ext_prop ext_prop_var
sort study assessor
save ext_prop, replace

******************************************************************************
* USING THE ESTIMATED BIASES TO ADJUST THE STUDY RESULTS                     *
******************************************************************************

/*
	Input Fisher transformed correlations (g) and standard errors from each study
*/

clear
input str20 study g seg
"Johnson" 0.00 0.09
"Moore" -0.21 0.10
"Figueroa-Colon" -0.34 0.17
"DeLany" -0.19 0.09
"Treuth" 0.16 0.11
"Salbe" 0.26 0.09
end

/*
	Merge the mean and variance of the internal additive and proportional biases for each study and assessor
*/

merge 1:m study using int_add
drop _merge

sort study assessor
merge 1:1 study assessor using int_prop
drop _merge

/*
	Estimate an internal bias adjusted estimate of g and its standard error for each study
*/

gen g_adj_int = (g-int_add)/int_prop
gen se_g_adj_int = sqrt((1/int_prop)^2*((seg^2)+int_prop_var*(g_adj_int^2)+int_add_var))

/*
	For each study calculate median of adjusted estimate and standard error across assessors
*/

collapse (median) g seg g_adj_int se_g_adj_int, by(study)

sort study

/*
	Merge the mean and variance of the external additive and proportional biases for each study and assessor
*/

merge 1:m study using ext_add
drop _merge

sort study assessor
merge 1:1 study assessor using ext_prop
drop _merge

/*
	Adjust the internal bias adjusted estimates of g and its standard error for each study additionally for the external biases
*/

gen g_adj_full = (g_adj_int-ext_add)/ext_prop
gen se_g_adj_full = sqrt((1/ext_prop)^2*((se_g_adj_int^2)+((g_adj_full^2)*ext_prop_var)))

/*
	For each study calculate median of adjusted estimate and standard error across assessors
*/

collapse (median) g seg g_adj_int se_g_adj_int g_adj_full se_g_adj_full, by(study)

sort study
save pa_full, replace

/*
	Use random effects meta-analysis to combine the bias adjusted results across studies
*/

metan g_adj_full se_g_adj_full, random nodraw

/*
	Transform combined result (which is on the Fisher transformed scale) back to the original correlation scale
*/

local meta_r_adj_full=(exp(2*r(ES))-1)/(exp(2*r(ES))+1)
local meta_r_adj_full_low=(exp(2*r(ci_low))-1)/(exp(2*r(ci_low))+1)
local meta_r_adj_full_upp=(exp(2*r(ci_upp))-1)/(exp(2*r(ci_upp))+1)

di "Bias adjusted correlation (original scale) is " in wh %5.3f `meta_r_adj_full'
di "with 95% confidence interval from " in wh %5.3f `meta_r_adj_full_low' in ye " to " in wh %5.3f `meta_r_adj_full_upp'

 