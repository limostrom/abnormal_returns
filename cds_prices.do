/*
cds_prices.do

Pulls CDS spreads (6m and 5y) by ticker and calendar month, merges these with a
	gvkey-fyear-month dataset, and collapses to the beginning and ending CDS spreads
	of the fiscal year.

*/

cap cd "C:\Users\lmostrom\Dropbox\Abnormal_Returns\CDSs\"

use "cds_01jan2017_30nov2018.dta", clear
keep if ccy == "USD"
keep date ticker spread6m spread5y

collapse (mean) spread6m spread5y, by(ticker date) fast
ren ticker tic

gen year = int(date/10000)
gen month = mod(int(date/100), 100)
gen datem = ym(year, month)
format datem %tm

sort tic date
collapse (first) spread6m spread5y, by(tic datem year)
gen hyphen = strpos(tic, "-")
replace tic = substr(tic, 1, hyphen-1) if hyphen > 0
duplicates tag tic datem, gen(dup)
drop if dup & hyphen > 0
drop dup hyphen

merge 1:1 tic datem using "../../gvkey_fyear_bydatem.dta", ///
	keep(3) nogen keepus(gvkey fyear)

sort tic datem
collapse (first) spread6m_beg = spread6m (last) spread6m_end = spread6m ///
		 (first) spread5y_beg = spread5y (last) spread5y_end = spread5y, ///
	by(tic gvkey fyear)

save "gvkey_fyear_cds_spreads.dta", replace