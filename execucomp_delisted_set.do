/*
Merge delisting codes from CRSP with Execucomp
-merge Execucomp with CRSP/Compustat (annual)
-merge with CRSP delisting codes (monthly)
*/
pause on

cap cd "C:/Users/lmostrom/Dropbox/"

import delimited "CRSP_monthly_05-19.csv", clear case(lower)
save "CRSP_monthly_05-19.dta", replace
keep if dlstcd != . & !inrange(dlstcd, 100, 199)
	* http://www.crsp.org/products/documentation/delisting-codes
ren perm?? lperm??
drop date ticker comnam
isid lpermno

lab var dlpdt "Date of Delisting Payment (CRSP)"
lab var dlstcd "Delisting Code (CRSP)"
lab var dlretx "Delisting Return w/o Dividends (CRSP)"
lab var dlprc "Delisting Price (CRSP)"
lab var dlret "Delisting Return (CRSP)"
   
tempfile delistings
save `delistings', replace


use "Execucomp_AnnComp.dta", clear
keep gvkey fyear
duplicates drop
lab var gvkey "gvkey from Execucomp"
lab var fyear "Fiscal Year from Execucomp"

merge 1:m gvkey fyear using "Compustat-CRSP_Merged_Annual", ///
	nogen keep(3) keepus(lpermno cusip)

merge m:1 lpermno using `delistings', nogen keep(3)

save "Abnormal_Returns/delisted_firms_in_Execucomp.dta", replace