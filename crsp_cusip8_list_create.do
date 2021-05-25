/*
crsp_cusip8_list_create.do

*/

cd "C:\Users\lmostrom\Dropbox\"

use "CRSP_monthly_05-19.dta", clear

keep cusip
duplicates drop

export delimited "CRSP_cusip8_list.txt", replace