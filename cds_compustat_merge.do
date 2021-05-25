/*
cds_compustat_merge.do
*/
cap cd "C:/Users/lmostrom/Dropbox/Abnormal_Returns/CDSs"

use "../../Compustat-CRSP_Merged_Annual.dta", clear
assert year == int(datadate/10000)
assert month == mod(int(datadate/100), 100)
gen datem = ym(year, month)
format datem %tm

gen dot = strpos(tic, ".")
replace tic = substr(tic, 1, dot-1) if dot > 0
duplicates tag tic datem, gen(dup)
drop if dup & dot > 0
drop dup dot

tempfile compustat
save `compustat', replace

use "cds.dta", clear
drop if year == 0

assert year == int(date/10000)
gen month = mod(int(date/100), 100)
gen datem = ym(year, month)
format datem %tm

ren ticker tic
drop if tic == ""
gen hyphen = strpos(tic, "-")
replace tic = substr(tic, 1, hyphen-1) if hyphen > 0
duplicates tag tic datem, gen(dup)
drop if dup & hyphen > 0
drop dup hyphen
gen dot = strpos(tic, ".")
replace tic = substr(tic, 1, dot-1) if dot > 0
duplicates tag tic datem, gen(dup)
drop if dup & dot > 0
drop dup dot

ren date datadate

joinby tic year using `compustat', _merge(_merge) unm(master)
save "cds_compustat_merge.dta", replace

merge 1:1 tic datem using "../../gvkey_fyear_bydatem.dta", ///
	keep(1 3) nogen keepus(gvkey fyear)

	