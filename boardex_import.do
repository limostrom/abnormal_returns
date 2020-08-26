/*


*/

pause on

cap cd "C:\Users\lmostrom\Dropbox"

import delimited "Boardex/Details.csv", clear varn(1) case(lower)
	keep directorid directorname dob dod gender networksize
	order directorid directorname dob dod gender networksize
	tempfile details
	save `details', replace
	
import delimited Boardex/Company_Profiles, clear varn(1) case(lower)
	ren boardid companyid // these are the same; not sure why named different
	keep companyid isin cikcode
	duplicates drop
	drop if isin == ""
	isid companyid isin
	tempfile compid_and_isin
	save `compid_and_isin', replace

import delimited Boardex/Company_Profiles, clear varn(1) case(lower)
	ren boardid companyid // these are the same; not sure why named different
	keep companyid isin cikcode
	duplicates drop
	keep if isin == ""
	drop isin
	isid companyid
	tempfile compid_only
	save `compid_only', replace

local filelist: dir "Boardex" files "Employment_*.csv"

local first = 1
foreach file of local filelist {
	import delimited "Boardex/`file'", clear varn(1) case(lower)
	
	if `first' {
		tempfile emp
		save `emp', replace
	}
	else {
	    append using `emp'
		save `emp', replace
	}
	
	local first = 0
}

lab var ned "Non-Executive Director Indicator"
egen dir_comp_id = group(companyid directorid)
keep isin companyid directorid dir_comp_id companyname directorname ned ///
		rolename datestartrole dateendrole
	merge m:1 directorid using `details', nogen
	merge m:1 companyid isin using `compid_and_isin', keep(1 3) nogen
		ren cikcode cik
	merge m:1 companyid using `compid_only', keep(1 3) nogen
		replace cik = cikcode if cik == .
		drop cikcode
	
gen yearstart = substr(datestartrole, 1, 4) if datestartrole != "N"
gen yearend = substr(dateendrole, 1, 4) if !inlist(dateendrole, "N", "C")
		
gen birthyear = regexs(0) if regexm(dob, "[0-9][0-9][0-9][0-9]")
	destring birthyear yearstart yearend, replace

gen age = 2020-birthyear
		
order isin cik companyid directorid dir_comp_id companyname directorname ///
		age rolename gender yearstart yearend ned networksize ///
		dob birthyear dod datestartrole dateendrole 

duplicates drop
duplicates tag dir_comp_id rolename ned, gen(dup)
sort dir_comp_id rolename ned yearstart yearend

replace yearstart = yearstart[_n-1] if dup & yearstart == yearend[_n-1] ///
		& dir_comp_id == dir_comp_id[_n-1] & rolename == rolename[_n-1] ///
		& ned == ned[_n-1]
drop if dup & dir_comp_id == dir_comp_id[_n+1] & rolename == rolename[_n+1] ///
		& ned == ned[_n+1] & yearstart == yearstart[_n+1] & yearend < yearend[_n+1]
drop dup
duplicates drop cik-dateendrole, force

*isid dir_comp_id rolename ned // close enough?

save "Abnormal_Returns/boardex_person-firm.dta", replace
		
		
		
		