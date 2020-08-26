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

local crsp_ratios 1
local pfs 0
local longshort 0

global repo "C:\Users\lmostrom\Documents\GitHub\abnormal_returns"

cap cd "C:\Users\lmostrom\Dropbox\"

local benchmark_list "rf0 rf vwretd"

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
		
	merge m:1 datem using "FamaFrench.dta", keep(1 3) nogen keepus(rf mktrf smb hml umd ps_vwf)
	
tempfile crsp
save `crsp', replace
	
*-----------------------
if `crsp_ratios' == 1 {
*-----------------------

xtset permno datem


local collapse_sds ""
local collapse_arith ""
local collapse_redrock ""
local collapse_upside ""

foreach bchmk of local benchmark_list {
	gen exc_`bchmk' = ret - `bchmk'
	gen sq_exc_`bchmk' = exc_`bchmk'^2
	
	* Calculate downside risk by averaging (min(0, exc_*))^2, per Red Rock Capital
	* http://www.redrockcapital.com/Sortino__A__Sharper__Ratio_Red_Rock_Capital.pdf
	gen redrock_adj_exc_`bchmk' = min(0, exc_`bchmk')
	gen redrock_adj_sq_exc_`bchmk' = redrock_adj_exc_`bchmk'^2

	* Calculate upside deviation analogously by averaging (max(0, exc_*))^2
	gen upside_adj_exc_`bchmk' = max(0, exc_`bchmk')
	gen upside_adj_sq_exc_`bchmk' = upside_adj_exc_`bchmk'^2

	* For Rolling Window Ratios
	tssmooth ma redrock_meansq_w36_exc_`bchmk' = redrock_adj_sq_exc_`bchmk', w(35 1 0)
	tssmooth ma upside_dev_w36_`bchmk' = upside_adj_sq_exc_`bchmk', w(35 1 0)
	tssmooth ma arith_mean_w36_exc_`bchmk' = exc_`bchmk', w(35 1 0)
	tssmooth ma meansq_w36_exc_`bchmk' = sq_exc_`bchmk', w(35 1 0)
	
	* For Annual Ratios
	local collapse_sds "`collapse_sds' sd_exc_`bchmk' = exc_`bchmk'"
	local collapse_arith "`collapse_arith' arith_mean_exc_`bchmk' = exc_`bchmk'"
	local collapse_redrock "`collapse_redrock' redrock_meansq_exc_`bchmk' = redrock_adj_sq_exc_`bchmk'"
	local collapse_upside "`collapse_upside' upside_meansq_exc_`bchmk' = upside_adj_sq_exc_`bchmk'"
}
	
foreach bchmk of local benchmark_list {
	gen information_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(meansq_w36_exc_`bchmk')
		if "`bchmk'" == "rf" ren information_roll36_`bchmk' sharpe_roll36_`bchmk'
	gen sortino_roll36_`bchmk' = (arith_mean_w36_exc_`bchmk')/sqrt(redrock_meansq_w36_exc_`bchmk')
}

bys permno (datem): gen n_w36 = min(_n, 36)
	
preserve // ----- Compute Annual Ratios ----------------------------------------
	#delimit ;
	sort permno datem;
	collapse (count) nA = ret
			 (mean) `collapse_arith' `collapse_redrock' `collapse_upside'
			 (sd) `collapse_sds',
		by(permno year) fast;
	#delimit cr

	foreach bchmk of local benchmark_list {
		gen informationA_`bchmk' = (arith_mean_exc_`bchmk')/sd_exc_`bchmk'
			if "`bchmk'" == "rf" ren informationA_`bchmk' sharpeA_`bchmk'
		gen sortinoA_`bchmk' = (arith_mean_exc_`bchmk')/sqrt(redrock_meansq_exc_`bchmk')

		ren upside_meansq_exc_`bchmk' upside_devA_`bchmk'
			lab var upside_devA_`bchmk' "Mean annual upside deviation, benchmark = `bchmk'"
	}

	tempfile annual
	save `annual', replace
restore // ---------------------------------------------------------------------

merge m:1 permno year using `annual', assert(1 3) nogen ///
			keepus(sharpeA* informationA* sortinoA* upside_devA* nA)

gen flag_w36 = n_w36 < 36
gen flagA = nA < 12

keep permno datem year month n_w36 nA sharpe* information* sortino* upside_dev* flag*
order permno datem year month n_w36 nA sharpe* information* sortino* upside_dev* flag*

save "Abnormal_Returns/sortino_sharpe.dta", replace
*----------------------
} // end `crsp_ratios'
*----------------------

*------------------
if `pfs' == 1 {
*------------------
use "Abnormal_Returns/portfolio ratios.dta", clear
drop portfolio
reshape long p, i(permno year month) j(pf)
	ren p in_out
	replace pf = pf*10 + in_out
	drop in_out

merge m:1 permno year month using `crsp', keep(3) keepus(datem ret `benchmark_list')

local means ""
local MSEs ""
foreach bchmk of local benchmark_list {
	gen exc_`bchmk' = ret - `bchmk'
	gen sq_exc_`bchmk' = exc_`bchmk'^2
	
	* Calculate downside risk by averaging (min(0, exc_*))^2, per Red Rock Capital
	* http://www.redrockcapital.com/Sortino__A__Sharper__Ratio_Red_Rock_Capital.pdf
	gen redrock_adj_exc_`bchmk' = min(0, exc_`bchmk')
	gen redrock_adj_sq_exc_`bchmk' = redrock_adj_exc_`bchmk'^2

	* Calculate upside deviation analogously by averaging (max(0, exc_*))^2
	gen upside_adj_exc_`bchmk' = max(0, exc_`bchmk')
	gen upside_adj_sq_exc_`bchmk' = upside_adj_exc_`bchmk'^2
	
	local means "`means' exc_`bchmk' redrock_adj_exc_`bchmk' upside_adj_exc_`bchmk'"
	local MSEs "`MSEs' sq_exc_`bchmk' redrock_adj_sq_exc_`bchmk' upside_adj_sq_exc_`bchmk'"
}

collapse (count) n = permno (mean) `means' `MSEs', by(pf year month datem) fast
	
foreach bchmk of local benchmark_list {
	gen information_`bchmk' = (exc_`bchmk')/sqrt(sq_exc_`bchmk')
		if "`bchmk'" == "rf" ren information_`bchmk' sharpe_`bchmk'
	gen sortino_`bchmk' = (exc_`bchmk')/sqrt(redrock_adj_sq_exc_`bchmk')

	ren upside_adj_sq_exc_`bchmk' upside_dev_`bchmk'
		lab var upside_dev_`bchmk' "Mean upside deviation across portfolio, benchmark = `bchmk'"
}

gen flag15 = n < 15
gen flag30 = n < 30

keep pf datem year month n sharpe* information* sortino* upside_dev* flag*

gen in_out = mod(pf, 10)
replace pf = int(pf/10)

order pf in_out datem year month n sharpe* information* sortino* upside_dev* flag*

save "Abnormal_Returns/sortino_sharpe_pfs.dta", replace
*------------------
} // end `pfs'
*------------------

*------------------
if `longshort' == 1 {
*------------------
use permno datem year month ret mktrf smb hml umd ps_vwf `benchmark_list' using `crsp', clear

merge 1:1 permno year month using "Abnormal_Returns/long short.dta", keep(3) nogen

collapse (mean) ret (last) rf mktrf smb hml umd ps_vwf, by(datem year month portfolio) fast
reshape wide ret, i(datem year month) j(portfolio)
ren ret1 ret_long
ren ret0 ret_short
gen ret_long_minus_short = ret_long - ret_short

foreach pf in "_long" "_short" "_long_minus_short" {
	foreach x in 1 3 4 5 {
		if `x' == 1 local rhs "mktrf"
		if `x' == 3 local rhs "mktrf smb hml"
		if `x' == 4 local rhs "mktrf smb hml umd"
		if `x' == 5 local rhs "mktrf smb hml umd if inrange(year, 1963, 2017)"
		
		reg ret`pf' `rhs'
			predict alpha`pf'_M`x', residuals
	}
}


save "Abnormal_Returns/long_short_alphas.dta", replace
*------------------
} // end `pfs'
*------------------