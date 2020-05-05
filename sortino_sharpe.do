/*
sortino_sharpe.do

We are trying to estimate Sharpe and Sortino ratios by firm year using the CRSP
monthly data file. Right now we are estimating each ratio twice, once using
the value weighted market return (variable vwretd) as Rf and once using 0 as Rf.

Sortino = (Realized Return - Target Return)/(StDev of min(0,Excess Return))
	https://en.wikipedia.org/wiki/Sortino_ratio
	http://www.redrockcapital.com/Sortino__A__Sharper__Ratio_Red_Rock_Capital.pdf
Sharpe =  (Realized Return - Benchmark)/(StDev of Excess Return, i.e. sqrt(var(R-Rf)))
	https://en.wikipedia.org/wiki/Sharpe_ratio
	
*/

clear all
cap log close
pause on

local crsp_ratios 0
local pfs 1

global repo "C:\Users\lmostrom\Documents\GitHub\abnormal_returns"

cap cd "C:\Users\lmostrom\Dropbox\"

local benchmark_list "rf0 vwretd"

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
	
tempfile crsp
save `crsp', replace
	
*-----------------------
if `crsp_ratios' == 1 {
*-----------------------

xtset permno datem


local collapse_sds ""
local collapse_arith ""
local collapse_redrock ""

foreach bchmk of local benchmark_list {
	gen exc_`bchmk' = ret - `bchmk'
	gen sq_exc_`bchmk' = exc_`bchmk'^2
	
	* Calculate downside risk by averaging (min(0, exc_*))^2, per Red Rock Capital
	* http://www.redrockcapital.com/Sortino__A__Sharper__Ratio_Red_Rock_Capital.pdf
	gen redrock_adj_exc_`bchmk' = min(0, exc_`bchmk')
	gen redrock_adj_sq_exc_`bchmk' = redrock_adj_exc_`bchmk'^2

	* For Rolling Window Ratios
	tssmooth ma redrock_meansq_w36_exc_`bchmk' = redrock_adj_sq_exc_`bchmk', w(35 1 0)
	tssmooth ma arith_mean_w36_exc_`bchmk' = exc_`bchmk', w(35 1 0)
	tssmooth ma meansq_w36_exc_`bchmk' = sq_exc_`bchmk', w(35 1 0)
	
	* For Annual Ratios
	local collapse_sds "`collapse_sds' sd_exc_`bchmk' = exc_`bchmk'"
	local collapse_arith "`collapse_arith' arith_mean_exc_`bchmk' = exc_`bchmk'"
	local collapse_redrock "`collapse_redrock' redrock_meansq_exc_`bchmk' = redrock_adj_sq_exc_`bchmk'"
}
	
foreach bchmk of local benchmark_list {
	gen sharpe_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(meansq_w36_exc_`bchmk')
	gen sortino_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(redrock_meansq_w36_exc_`bchmk')
}
	
preserve // ----- Compute Annual Ratios ----------------------------------------
	#delimit ;
	sort permno datem;
	collapse (mean) `collapse_arith' `collapse_redrock'
			 (sd) `collapse_sds',
		by(permno year) fast;
	#delimit cr


	foreach bchmk of local benchmark_list {
		gen sharpeA_`bchmk' = (arith_mean_exc_`bchmk')/sd_exc_`bchmk'
		gen sortinoA_`bchmk' = (arith_mean_exc_`bchmk')/sqrt(redrock_meansq_exc_`bchmk')
	}

	tempfile annual
	save `annual', replace
restore // ---------------------------------------------------------------------

merge m:1 permno year using `annual', assert(1 3) nogen keepus(sharpeA* sortinoA*)

keep permno year sharpe* sortino*
order permno year sharpe* sortino*

save "Abnormal_Returns/sortino_sharpe.dta", replace
*----------------------
} // end `crsp_ratios'
*----------------------

*------------------
if `pfs' == 1 {
*------------------
use "Abnormal_Returns/portfolio ratios.dta", clear

reshape long p, i(permno year month) j(pf)
	ren p in_out
	replace pf = pf*10 + in_out
	drop in_out

merge m:1 permno year month using `crsp', keep(3) keepus(datem ret `benchmark_list')

collapse (mean) ret (last) `benchmark_list', by(pf year datem) fast

xtset pf datem

local collapse_sds ""
local collapse_arith ""
local collapse_redrock ""

foreach bchmk of local benchmark_list {
	gen exc_`bchmk' = ret - `bchmk'
	gen sq_exc_`bchmk' = exc_`bchmk'^2
	
	* Calculate downside risk by averaging (min(0, exc_*))^2, per Red Rock Capital
	* http://www.redrockcapital.com/Sortino__A__Sharper__Ratio_Red_Rock_Capital.pdf
	gen redrock_adj_exc_`bchmk' = min(0, exc_`bchmk')
	gen redrock_adj_sq_exc_`bchmk' = redrock_adj_exc_`bchmk'^2

	* For Rolling Window Ratios
	tssmooth ma redrock_meansq_w36_exc_`bchmk' = redrock_adj_sq_exc_`bchmk', w(35 1 0)
	tssmooth ma arith_mean_w36_exc_`bchmk' = exc_`bchmk', w(35 1 0)
	tssmooth ma meansq_w36_exc_`bchmk' = sq_exc_`bchmk', w(35 1 0)
	
	* For Annual Ratios
	local collapse_sds "`collapse_sds' sd_exc_`bchmk' = exc_`bchmk'"
	local collapse_arith "`collapse_arith' arith_mean_exc_`bchmk' = exc_`bchmk'"
	local collapse_redrock "`collapse_redrock' redrock_meansq_exc_`bchmk' = redrock_adj_sq_exc_`bchmk'"
}
	
foreach bchmk of local benchmark_list {
	gen sharpe_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(meansq_w36_exc_`bchmk')
	gen sortino_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(redrock_meansq_w36_exc_`bchmk')
}
	
preserve // ----- Compute Annual Ratios ----------------------------------------
	#delimit ;
	sort pf datem;
	collapse (mean) `collapse_arith' `collapse_redrock'
			 (sd) `collapse_sds',
		by(pf year) fast;
	#delimit cr


	foreach bchmk of local benchmark_list {
		gen sharpeA_`bchmk' = (arith_mean_exc_`bchmk')/sd_exc_`bchmk'
		gen sortinoA_`bchmk' = (arith_mean_exc_`bchmk')/sqrt(redrock_meansq_exc_`bchmk')
	}

	tempfile annual
	save `annual', replace
restore // ---------------------------------------------------------------------

merge m:1 pf year using `annual', assert(1 3) nogen keepus(sharpeA* sortinoA*)

keep pf datem year sharpe* sortino*

gen in_out = mod(pf, 10)
replace pf = int(pf/10)

order pf in_out datem year sharpe* sortino*

save "Abnormal_Returns/sortino_sharpe_pfs.dta", replace
*------------------
} // end `pfs'
*------------------

