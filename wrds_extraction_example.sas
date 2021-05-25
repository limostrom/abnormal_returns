libname deals '/wrds/tfn/sasdata/dealscan';

* Merging Company File with Package file;

proc sql;
create table packageplus as
select *
from deals.company as a, deals.package as b
where a.Companyid = b.BorrowerCompanyID ;
*quit;

*Proc print data=packageplus noobs;
*where year(dealactivedate)>1990;
*var companyid ticker packageid DealActiveDate; 
run;


* Adding Facility file;

proc sql;
create table covenantplus as
select *
from packageplus as a, deals.financialcovenant as b
where a.packageid = b.packageid ;
quit;

Proc print data=covenantplus noobs;
where year(dealactivedate)>1990;
var companyid ticker packageid dealactivedate covenanttype initialratio trend event final carryover; 
run;