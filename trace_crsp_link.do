/*
trace_crsp_link.do

*/

cap cd "Z:/Documents"
use bonds_bymonth, clear

tempfile trace
save `trace', replace

import delimited "Bond_CRSP_Link.csv", clear
keep cusip permno link_*

gen datem1 = ym(int(link_startdt/10000), mod(int(link_startdt/100), 100))
	format datem1 %tm
gen datem2 = ym(int(link_enddt/10000), mod(int(link_enddt/100), 100))
	format datem2 %tm

gen id = _n
drop link_startdt link_enddt
reshape long datem, i(id cusip permno) j(n)
drop n
duplicates drop
xtset id datem
tsfill
bys id: replace permno = permno[_n-1] if permno == .
bys id: replace cusip = cusip[_n-1] if cusip == ""
drop id

merge m:1 cusip datem using `trace', nogen keep(3)
save "trace_mprice_crsp_merged.dta", replace