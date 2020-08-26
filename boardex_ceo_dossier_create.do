/*

boardex_ceo_dossier_create.do

*/
clear all
cap log close
pause on
cap cd "C:\Users\lmostrom\Dropbox\Abnormal_Returns"

use gvkey cik cusip using "../Compustat-CRSP_Merged_Annual.dta", clear
duplicates drop

isid gvkey cusip
tempfile cpu
save `cpu', replace

import excel "new to merge.xlsx", clear first case(lower)
tempfile new
save `new', replace


use "boardex_person-firm.dta", clear
replace directorname = substr(directorname,14,.) ///
		if substr(directorname,1, 13) == "Her Highness "
replace directorname = substr(directorname,20,.) ///
		if substr(directorname,1, 19) == "Her Royal Highness "
replace directorname = substr(directorname,14,.) ///
		if substr(directorname,1, 13) == "His Highness "
replace directorname = substr(directorname,15,.) ///
		if substr(directorname,1, 14) == "H. Excellency "
replace directorname = substr(directorname,7,.) ///
		if substr(directorname,1, 6) == "H. E. "
replace directorname = substr(directorname,18,.) ///
		if substr(directorname,1, 17) == "H.R.H The Prince "
replace directorname = substr(directorname,14,.) ///
		if substr(directorname,1, 13) == "The Rt. Hon. "
replace directorname = substr(directorname,10,.) ///
		if substr(directorname,1, 9) == "The Hon. "
replace directorname = substr(directorname,6,.) ///
		if substr(directorname,1, 5) == "Lord "
replace directorname = substr(directorname,10,.) ///
		if substr(directorname,1, 9) == "Baroness "
replace directorname = substr(directorname,11,.) ///
		if substr(directorname,1, 10) == "Professor "
replace directorname = substr(directorname,8,.) ///
		if substr(directorname,1, 7) == "Doctor "
replace directorname = substr(directorname,4,.) ///
		if substr(directorname,1, 3) == "Dr "
replace directorname = substr(directorname,19,.) ///
		if substr(directorname,1, 18) == "Air Chief Marshal "
replace directorname = substr(directorname,14,.) ///
		if substr(directorname,1, 13) == "Rear Admiral "
replace directorname = substr(directorname,5,.) ///
		if substr(directorname,1, 4) == "Sir "
split directorname, p(" ") gen(name) limit(3)
	replace name3 = name2 if inlist(name3, "", "Jr", "II", "III")
	replace name2 = "" if name2 == name3

tempfile boardex
save `boardex', replace
	
import excel "CEOs to merge.xlsx", clear case(lower) first

	
merge m:1 gvkey using `cpu', nogen keep(1 3) keepus(cik)
	drop if cik == .
	
append using `new'	
	
	replace exec_fullname = subinstr(exec_fullname, ",", "", .)
	split exec_fullname, p(" ") gen(name)
	replace name3 = name2 if inlist(name3,"","Jr.","Sr.","II","III","M.D.","Ph.D.")
		replace name2 = "" if name2 == name3
	replace name2 = name2 + " " + name3 if inlist(name3, "W.", "E.")
		replace name3 = name4 if inlist(name3, "W.", "E.")
	replace name3 = name2 + " " + name3 if inlist(name2,"de","Van")
		replace name2 = "" if inlist(name2,"de","Van")
	replace name3 = name3 + " " + name4 if inlist(name3,"de","Van")
		
br exec_fullname name*
pause
joinby cik name3 using `boardex'

save "boardex_ceo_for_dossier.dta", replace
export delimited "boardex_ceo_for_dossier.csv", replace
