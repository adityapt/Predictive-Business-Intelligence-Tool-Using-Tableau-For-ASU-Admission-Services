--checking if AppNumbers is the primary key
select count(distinct AppNumbers) from capstone_data;
--33112
select count(*) from CAPSTONE_DATA;
--33112

drop table admitted_students;

create table admitted_students nologging as 
select * from capstone_data where admit = 1;

select count(*), count(distinct AppNumbers) from admitted_students;--14322	14322

select * from all_tab_cols where user = 'ADITYA' and table_name = 'ADMITTED_STUDENTS';

select a.*
from
( 
select provincedata, sum(admit) as admitted_number,
row_number() over(order by sum(admit)desc) row_number
from ADMITTED_STUDENTS 
where provincedata not in ('Not Provided','No Province')
group by provincedata 
order by 2 desc)  a
where row_number <= 10
;
--provincedata admittednumber rank
--beijing	        1126	       1
--guangdong	      633	         2
--jiangsu	        630	         3
--sichuan	        594        	 4
--zhejiang	      543	         5
--shanghai	      434	         6
--hubei	          306	         7
--shandong	      302	         8
--municipal	      243        	 9
--henan	          185	        10

select provincedata,APPLICATIONYEAR, sum(applicationcomplete)  applied, sum(admit) admitted, sum(enrolled) enrolled
from capstone_data where provincedata in 
(
select provincedata
from
( 
select provincedata, sum(admit) as admitted_number,
row_number() over(order by sum(admit)desc) row_number
from ADMITTED_STUDENTS 
where provincedata not in ('Not Provided','No Province')
group by provincedata 
order by 2 desc)  a
where row_number <= 3
) 
--and applicationyear = 2010
group by provincedata,APPLICATIONYEAR
order by APPLICATIONYEAR, enrolled desc ;


select a.*, 
round((a.admitted*100/a.applied),2)||'%'  applyadmitperc,
round((a.enrolled*100/a.admitted),2)||'%' enrolladmitperc, 
round((a.enrolled*100/a.applied),2)||'%'  enrollapplyperc 
from (select ApplicationYear,
count(AdmittedFlag) as applied,
sum(Admit) as admitted, 
sum(Enrolled) as enrolled 
from capstone_data 
where capstone_data.APPLICATIONReceived = 1
group by ApplicationYear)  a 
order by a.ApplicationYear;

--Year  Applied Admitted Enrolled ApplyAdmitPerc EnrollAdmitPerc EnrollApplyPerc
--2009	  857	    436	      123	        50.88%	       28.21%	      14.35%
--2010	  3253	  1297	    452	        39.87%	       34.85%	      13.89%
--2011	  3862	  1701	    655	        44.04%	       38.51%	      16.96%
--2012	  4931	  2227	    804	        45.16%	       36.1%	      16.31%
--2013	  7246	  3080	    1128	      42.51%	       36.62%	      15.57%
--2014	  7337	  3338	    1316	      45.5%	         39.42%	      17.94%
--2015	  5626	  2243	    856	        39.87%	       38.16%	      15.22%

select a.*, 
round((a.admitted*100/a.applied),2)||'%'  applyadmitperc,
round((a.enrolled*100/a.admitted),2)||'%' enrolladmitperc, 
round((a.enrolled*100/a.applied),2)||'%'  enrollapplyperc 
from (select ApplicationYear,
count(AdmittedFlag) as applied,
sum(Admit) as admitted, 
sum(Enrolled) as enrolled 
from capstone_data 
where capstone_data.APPLICATIONCOMPLETE = 1
group by ApplicationYear)  a 
order by a.ApplicationYear;

--Year  Applied Admitted Enrolled ApplyAdmitPerc EnrollAdmitPerc EnrollApplyPerc
--2009	    492    436	    123	    88.62%	        28.21%	        25%
--2010	    1713	 1297	    452	    75.72%	        34.85%	        26.39%
--2011	    2280	 1701	    655	    74.61%	        38.51%	        28.73%
--2012	    3454	 2227	    804	    64.48%	        36.1%	          23.28%
--2013	    4684	 3080	    1128	  65.76%	        36.62%	        24.08%
--2014	    4349	 3338	    1316	  76.75%	        39.42%	        30.26%
--2015	    3764	 2243	    856	    59.59%	        38.16%	        22.74%


--selecting the data which contains province
select count(*) from capstone_data 
where provincedata in ('Not Provided','No Province');

select a.*, 
round((a.admitted*100/a.applied),2)||'%'  applyadmitperc,
round((a.enrolled*100/a.admitted),2)||'%' enrolladmitperc, 
round((a.enrolled*100/a.applied),2)||'%'  enrollapplyperc 
from (select provincedata,ApplicationYear,
count(AdmittedFlag) as applied,
sum(Admit) as admitted, 
sum(Enrolled) as enrolled 
from capstone_data 
where capstone_data.APPLICATIONCOMPLETE = 1
--where provincedata not in ('Not Provided','No Province')
group by provincedata, ApplicationYear)  a 
order by a.ApplicationYear;


select a.* 
from 
(select provincedata, ApplicationYear,count(AdmittedFlag) as applied,
sum(Admit) as admitted, 
sum(Enrolled) as enrolled ,
row_number() over(partition by ApplicationYear order by count(AdmittedFlag) desc) rank_number
 from capstone_data
where provincedata not in ('Not Provided','No Province')
and APPLICATIONCOMPLETE = 1
group by provincedata, ApplicationYear
order by 2,6
)a
where rank_number <=3;

select count(*) from capstone_data where admit = 0 and AdmitDate is null ;
--16596

select count(*) from capstone_data where admit = 1 and enrolled = 1 and AdmitDate is null ;
--2
select to_date(AdmitDate,'MM-DD-YY HH24:MI:SS') from capstone_data;


create table capstone_data_with_days nologging as
select a.*,
case 
when admit = 1 then to_date(AdmitDate,'MM-DD-YY HH24:MI:SS') - to_date(ApplicationDate,'MM-DD-YY HH24:MI:SS')
when admit = 0 and AdmitDate is null then 0
else to_date(AdmitDate,'MM-DD-YY HH24:MI:SS') - to_date(ApplicationDate,'MM-DD-YY HH24:MI:SS') end  acceptorrejecttime
from capstone_data a;

select * from &a where AdmitDate is null and admit = 1 ;

select APPNUMBERS, admit, enrolled, AdmitDate from capstone_data_with_days where acceptorrejecttime is null;
--2729916	1	1 (null)
--2610135	1	1 (null)

select distinct applicationDate, to_date(ApplicationDate,'MM-DD-YY HH24:MI:SS') from CAPSTONE_DATA_WITH_DAYS;

select provincedata,ApplicationYear, 
round(avg(acceptorrejecttime),2) 
from capstone_data_with_days
where admit = 1
group by provincedata, ApplicationYear
order by 2;

select 
distinct AdmittedFlag, 
tempeEnrolledFlag, 
AsuEnrolledFlag,
WestEnrolledFlag, 
PolytechEnrolledFlag, 
DowntownEnrolledFlag,
CompletedApplicationFlag
ApplicationComplete
from CAPSTONE_DATA_WITH_DAYS 
where enrolled = 1
order by 1,2,3,4,5,6,7;
-- A student can take courses from two to three campuses

select distinct NSCSCHOOL from CAPSTONE_DATA_WITH_DAYS where enrolled = 1;
--null 
-- as expected as students who have enrolled into ASU have their NSC as nothing

select * from 
(
select  
NSCSCHOOL, 
CensusAppCollegeShortDescr, 
count(CensusAppCollegeShortDescr) college_count,
row_number() over(partition by CensusAppCollegeShortDescr order by count(CensusAppCollegeShortDescr) desc) as rank_number
from CAPSTONE_DATA_WITH_DAYS 
where admit =1 and enrolled = 0
and NSCSCHOOL is not null
group by  NSCSCHOOL, CensusAppCollegeShortDescr
) a
where rank_number <=3
;
--top 3 colleges for the specified courses


select   CensusAppCollegeShortDescr, sum(enrolled) 
from CAPSTONE_DATA_WITH_DAYS 
where admit =1 
group by  CensusAppCollegeShortDescr
order by 2 desc;
-- asu is famous in this order

--Business	2069
--Engineering	1339
--Liberal Arts and Sciences	1059
--Design and the Arts	318
--Technology and Innovation	154
--Letters and Sciences	116
--University College	53
--Health Solutions	47
--Teachers College	46
--Public Programs	46
--New College	24
--Public Service & Community Solution	16
--Nursing and Health	12
--Nursing and Health Innovation	11
--Nutrition and Health Promotion	6
--Journalism	6
--Global Management	6
--Sustainability	6


define a = 'CAPSTONE_DATA_WITH_DAYS';

select distinct applicationreceived from &a; 

select distinct acceptorrejecttime from &a where ApplicationYear = 2015 order by 1;
select provincedata, sum(enrolled)  from CAPSTONE_DATA_WITH_DAYS 
group by provincedata
order by 1;

select column_name from all_tab_cols where table_name = 'CAPSTONE_DATA_WITH_DAYS' order by 1;
select count(NSCSCHOOL), avg(acceptorrejecttime) from CAPSTONE_DATA_WITH_DAYS where ApplicationYear = 2010;

-----------------------------------------------------------------------------------
--3/17/2016 just checking the columns randomly
-----------------------------------------------------------------------------------
-- people who didnt enroll but paid the deposit money before getting admit
select distinct AppNUmbers, DEPOSITDATE,AdmitDate from CAPSTONE_DATA_WITH_DAYS where enrolled =0
and to_date(AdmitDate,'MM-DD-YY HH24:MI:SS') > to_date(DepositDate,'MM-DD-YY HH24:MI:SS') ;

--1922553	2/18/2013 0:00	3/19/2013 0:00
--2032341	7/3/2013 0:00	8/24/2013 0:00
--2486331	12/14/2013 0:00	10/1/2014 0:00
--2839869	4/16/2014 0:00	5/15/2015 0:00
--2007963	5/28/2013 0:00	5/29/2013 0:00
--1521261	8/23/2011 0:00	11/23/2011 0:00
--2851983	6/21/2014 0:00	5/23/2015 0:00
--2768532	3/24/2013 0:00	6/30/2015 0:00
--2617437	5/2/2013 0:00	7/4/2015 0:00
--2412867	4/1/2013 0:00	6/21/2014 0:00

-- people who didnt enroll but paid the deposit money before getting admit
select distinct AppNUmbers, DEPOSITDATE,AdmitDate from CAPSTONE_DATA_WITH_DAYS where enrolled =1
and to_date(AdmitDate,'MM-DD-YY HH24:MI:SS') > to_date(DepositDate,'MM-DD-YY HH24:MI:SS')
;

--1625616	5/4/2012  0:00	5/11/2012 0:00
--2376288	5/29/2013 0:00	3/22/2014 0:00
--1907634	4/22/2012 0:00	12/26/2012 0:00
--1924971	4/22/2013 0:00	8/15/2013 0:00
--2319819	6/21/2013 0:00	1/19/2014 0:00
--1846620	4/29/2013 0:00	5/13/2013 0:00

select count(CONDITIONALADMIT) from CAPSTONE_DATA_WITH_DAYS where enrolled = 1;
--5334
select distinct CENSUSAPPCOLLEGESHORTDESCR ,ApplicationYear, count(CONDITIONALADMIT) from 
CAPSTONE_DATA_WITH_DAYS 
where enrolled =1
group by CENSUSAPPCOLLEGESHORTDESCR,ApplicationYear
order by 1,2;

---------------------------------------------------------------------------------------------

-- 3/18/2016 looking into the data and modifying the datatypes as rewuired

--------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------
-- checking the columntypes that have been uploaded into the database
-------------------------------------------------------------------------------------------------------

select column_name, data_type  from all_tab_cols where table_name =  'CAPSTONE_DATA_WITH_DAYS' order by 2;

--CUMULATIVETRANSFERGPA
--HIGHSCHOOLGPA
--HIGHSCHOOLABORGPA
--HIGHSCHOOLRANKPERCENT
--SATVERBALEXAMSCORE
--SATQUANTITATIVEEXAMSCORE
--ACTCOMPOSITEEXAMSCORE


select CUMULATIVETRANSFERGPA from CAPSTONE_DATA_WITH_DAYS;

--=----------------------------------------------------------------
-- correcting the datatype of all the numebric columns
-------------------------------------------------------------------
--drop table capstone_data_3182016;
create table capstone_data_3182016 as select * from CAPSTONE_DATA_WITH_DAYS;
--updatibg the column types
update  capstone_data_3182016  
set 
CUMULATIVETRANSFERGPA = 
(case 
when CUMULATIVETRANSFERGPA = 'NA' then null
else to_number(CUMULATIVETRANSFERGPA) end),
HIGHSCHOOLGPA = 
(case 
when HIGHSCHOOLGPA = 'NA' then null
else to_number(HIGHSCHOOLGPA) end),
HIGHSCHOOLABORGPA = 
(case 
when HIGHSCHOOLABORGPA = 'NA' then null
else to_number(HIGHSCHOOLABORGPA) end),
HIGHSCHOOLRANKPERCENT = 
(case 
when HIGHSCHOOLRANKPERCENT = 'NA' then null
else to_number(HIGHSCHOOLRANKPERCENT) end),
SATVERBALEXAMSCORE = 
(case 
when SATVERBALEXAMSCORE = 'NA' then null
else to_number(SATVERBALEXAMSCORE) end),
SATQUANTITATIVEEXAMSCORE = 
(case 
when SATQUANTITATIVEEXAMSCORE = 'NA' then null
else to_number(SATQUANTITATIVEEXAMSCORE) end),
ACTCOMPOSITEEXAMSCORE = 
(case 
when ACTCOMPOSITEEXAMSCORE = 'NA' then null
else to_number(ACTCOMPOSITEEXAMSCORE) end)
;
--33,112 rows updated.
commit;

select avg(CUMULATIVETRANSFERGPA), avg(ACTCOMPOSITEEXAMSCORE), avg(SATQUANTITATIVEEXAMSCORE), avg(SATVERBALEXAMSCORE),
avg(HIGHSCHOOLRANKPERCENT), avg(HIGHSCHOOLABORGPA), avg(HIGHSCHOOLGPA) from 
capstone_data_3182016
where admit = 1 and enrolled = 1;
--it works

select column_name , data_type from all_tab_cols where lower(table_name) = 'capstone_data_3182016' order by 2;
-- discrepency ! cumulativetransfergpa is still a character

update capstone_data_3182016 set cumulativetransfergpa = to_number(cumulativetransfergpa) ;
commit;
--33,112 rows updated.
-- not working for this shit

-- altering table
alter table capstone_data_3182016 add  (cumulativetransfergpa1 number);
select distinct cumulativetransfergpa1 from capstone_data_3182016;
-- there is the new column in this

update capstone_data_3182016 set cumulativetransfergpa1 = cumulativetransfergpa;
commit;
select distinct cumulativetransfergpa1 from capstone_data_3182016;

select column_name , data_type from all_tab_cols where lower(table_name) = 'capstone_data_3182016' order by 2;

--checking  for errors

select distinct cumulativetransfergpa1, cumulativetransfergpa from capstone_data_3182016;
select * from capstone_data_3182016 where cumulativetransfergpa1 != cumulativetransfergpa;
select cumulativetransfergpa1, cumulativetransfergpa from capstone_data_3182016 where cumulativetransfergpa1 = cumulativetransfergpa;
select count(*) from capstone_data_3182016 where cumulativetransfergpa1 is not null;--4169
select count(*) from capstone_data_3182016;

-- deleting the redundant cumulativetransfergpa column from the newly created table
alter table  capstone_data_3182016 drop column cumulativetransfergpa; 
--altered

select cumulativetransfergpa from capstone_data_3182016;
--not present

alter table capstone_data_3182016 rename column cumulativetransfergpa1 to cumulativetransfergpa;

select cumulativetransfergpa from capstone_data_3182016;
select cumulativetransfergpa1 from capstone_data_3182016;

describe capstone_data_3182016;