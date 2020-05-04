/*
sortino_sharpe.do

We are trying to estimate Sharpe and Sortino ratios by firm year using the CRSP
monthly data file. Right now we are estimating each ratio twice, once using
the value weighted market return (variable vwretd) as Rf and once using 0 as Rf.

Sortino = (Realized Return - Target Return)/(StDev of Returns Below Target)
	https://en.wikipedia.org/wiki/Sortino_ratio
Sharpe =  (Realized Return - Benchmark)/(StDev of Excess Return, i.e. sqrt(var(R-Rf)))
	https://en.wikipedia.org/wiki/Sharpe_ratio
	
*/

clear all
cap log close
pause on

global repo "C:\Users\lmostrom\Documents\GitHub\abnormal_returns"

cap cd "C:\Users\lmostrom\Dropbox\"

use permno date ret vwretd dlstcd using "CRSP.dta", clear
	duplicates drop
	keep if inlist(dlstcd, ., 100)
	
	lab var ret "Holding Period Return"
	lab var vwretd "Value-Weighted Market Return"
	gen rf0 = 0
		lab var rf0 "Stand-in for Rf Rate of 0"
	destring ret, replace force // contains letters for missing returns
		drop if ret == .
	gen year = int(date/10000)
	gen month = int(mod(date, 10000)/100)
	gen datem = ym(year, month)
		format datem %tm
	
xtset permno datem

local benchmark_list "rf0 vwretd"

gen ret_plus1 = ret+1
bys permno year: egen min_mon = min(datem)
gen cum_ret = ret_plus1 if datem == min_mon
bys permno year: replace cum_ret = ret_plus1*cum_ret[_n-1] if datem > min_mon
replace cum_ret = cum_ret-1

local collapse_lasts ""
local collapse_sds ""

foreach bchmk of local benchmark_list {
	gen exc_`bchmk' = ret - `bchmk'
	
	gen `bchmk'_plus1 = `bchmk' + 1
	gen cum_`bchmk' = `bchmk'_plus1 if datem == min_mon
	bys permno year: replace cum_`bchmk' = `bchmk'_plus1*cum_`bchmk'[_n-1] if datem > min_mon
	replace cum_`bchmk' = cum_`bchmk'-1
	
	if "`bchmk'" == "rf0" assert cum_`bchmk' == 0

	bys permno year: egen dr_`bchmk' = sd(exc_`bchmk') if exc_`bchmk' < 0
	bys permno year: ereplace dr_`bchmk' = max(dr_`bchmk') // replace missings 
	
	local collapse_lasts "`collapse_lasts' cum_`bchmk' dr_`bchmk'"
	local collapse_sds "`collapse_sds' sd_exc_`bchmk' = exc_`bchmk'"
}
		
#delimit ;
sort permno datem;
collapse (last) cum_ret `collapse_lasts'
		 (sd) `collapse_sds',
	by(permno year) fast;
#delimit cr


foreach bchmk of local benchmark_list {
	gen sharpe_`bchmk' = (cum_ret-cum_`bchmk')/sd_exc_`bchmk'
	gen sortino_`bchmk' = (cum_ret-cum_`bchmk')/dr_`bchmk'
}

keep permno year sharpe* sortino*
order permno year sharpe* sortino*

save "Abnormal_Returns/sortino_sharpe.dta", replace


