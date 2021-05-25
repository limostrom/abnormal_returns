/*


*/

cap cd "C:/Users/lmostrom/Dropbox/Abnormal_Returns/"

import delimited "wrds_extraction.csv", clear //--------------------------------
replace covenanttype = strtrim(covenanttype)

replace covenanttype = "Max. Debt to Tangible Net Worth" ///
	if covenanttype == "Max. Debt to Tangible Net"
replace covenanttype = "Max. Total Debt to Tangible Net Worth" ///
	if inlist(covenanttype, "Max. Total Debt (including Contingent", ///
	"Max. Total Debt (including Contingent Liabilities) to Tangible Net Worth")
replace covenanttype = "Min. Net Worth to Total Asset" ///
	if covenanttype == "Min. Net Worth to Total"
	
tempfile covenants
save `covenants', replace

keep packageid
duplicates drop
export delimited "dealscan_packageids.txt", replace

import delimited "dealscan-facilities.csv", clear //----------------------------
tempfile facilities
save `facilities', replace

import delimited "dealscan-packages.csv", clear //----------------------------
tempfile packages
save `packages', replace

joinby packageid using `covenants'
joinby packageid using `facilities'
duplicates drop


