use "C:\Users\David\Desktop\Unifi\SECONDO ANNO\Longitudinal data analysis\Progetto\ESS9e03_1.stata\ESS9e03_1.dta",clear
append using "C:\Users\David\Desktop\Unifi\SECONDO ANNO\Longitudinal data analysis\Progetto\ESS3e03_7.stata\ESS3e03_7.dta", generate(newv) nolabel

gen id=_n
sum id

fre yrbrn
drop if yrbrn==.a | yrbrn==.b | yrbrn==.c | yrbrn==.d

fre eduyrs
drop if eduyrs==.b | eduyrs==.c | eduyrs==.d

fre pdempyr

drop if pdempyr==.b
drop if pdempyr==.c
drop if pdempyr==.d

fre lvpntyr
drop if lvpntyr==1111


fre yrbrn
* Birth cohorts: <1930; 1930-59; 1960-75; 1976-
gen cohort=1 if yrbrn<1930
replace cohort=2 if yrbrn>=1930 & yrbrn<=1959
replace cohort=3 if yrbrn>=1960 & yrbrn<=1975
replace cohort=4 if yrbrn>=1976 & yrbrn<=1986
replace cohort=5 if yrbrn>=1987 & yrbrn~=.

**creo evento LEFT

fre lvpntyr
fre lvptnyr
drop if lvpntyr==.b | lvpntyr==.c | lvpntyr==.d 
drop if lvptnyr==.b | lvptnyr==.c | lvptnyr==.d 

fre lvpntyr
fre lvptnyr


gen left=.
replace left=0 if lvpntyr==0 & lvptnyr==.a
replace left=1 if lvpntyr!=0 & lvpntyr<lvptnyr 
replace left=2 if lvpntyr!=0 & lvpntyr==lvptnyr
label define left 0 "no left home" 1 "other reason" 2 "cohabitation"
label values left left 
*left indica l'evento che determina l'uscita dalla funzione di sopravvivenza

fre left

drop if left==.

**creo la DURATA
gen durhome=.
replace durhome= 2006-yrbrn if (left==0 & essround==3)
replace durhome= 2018-yrbrn if (left==0 & essround==9)

replace durhome= lvpntyr-yrbrn if left==1
replace durhome= lvptnyr-yrbrn if left==2


fre durhome


*durhome indica il tempo in cui l'individuo rimane a casa


list if durhome==0
drop if durhome<0


*divido i paesi per area geografica
*fre cntry
encode cntry, gen(country)
fre country

recode country (5 10 18 25=1 "South Europe") (1 4 7=2 "German Speaking") (2 12 13 16 17 22=3 "North Europe") (3 6 9 14 15 19 20 21 22 24 26 27 29 30 31=4 "East Europe") (8 11 20 23 28=5 "Scandinavian Countries"), gen (europeanarea) 
fre europeanarea


gen SCANSUD=.
replace SCANSUD=0 if europeanarea==1
replace SCANSUD=1 if europeanarea==5

gen ITASWE=.
replace ITASWE=0 if country==18
replace ITASWE=1 if country==28

*si può fare italia (18) vs svezia (28)

gen eduF=.
replace eduF=1 if (edulvlfb==0 | edulvlfb==113 | edulvlfb==129)
replace eduF=2 if (edulvlfb==212 | edulvlfb==213 | edulvlfb==221 | edulvlfb==222 | edulvlfb==223 | edulvlfb==229)
replace eduF=3 if (edulvlfb==311 | edulvlfb==313 | edulvlfb==321 | edulvlfb==322 | edulvlfb==323)
replace eduF=4 if (edulvlfb==412 | edulvlfb==413 | edulvlfb==421 | edulvlfb==422 | edulvlfb==423)
replace eduF=5 if (edulvlfb==510 | edulvlfb==520 | edulvlfb==610 | edulvlfb==620 | edulvlfb==710 | edulvlfb==800)


gen eduM=.
replace eduM=1 if (edulvlmb==0 | edulvlmb==113 | edulvlmb==129)
replace eduM=2 if (edulvlmb==212 | edulvlmb==213 | edulvlmb==221 | edulvlmb==222 | edulvlmb==223 | edulvlmb==229)
replace eduM=3 if (edulvlmb==311 | edulvlmb==313 | edulvlmb==321 | edulvlmb==322 | edulvlmb==323)
replace eduM=4 if (edulvlmb==412 | edulvlmb==413 | edulvlmb==421 | edulvlmb==422 | edulvlmb==423)
replace eduM=5 if (edulvlmb==510 | edulvlmb==520 | edulvlmb==610 | edulvlmb==620 | edulvlmb==710 | edulvlmb==800)



gen famorig=.
replace famorig=max(edulvlfa,edulvlma) if essround==3
replace famorig=max(eduF,eduM) if essround==9
fre famorig

drop if famorig==55
drop if (edulvlfb==555 | edulvlmb==555)

gen famOrig=.
replace famOrig=1 if famorig==1
replace famOrig=2 if famorig==2 | famorig==3 | famorig==4
replace famOrig=3 if famorig==5

fre famOrig


label define famOrig 1 "low" 2 "medium" 3 "high"
label values famOrig famOrig

fre famOrig

drop if famOrig==.
*******************************************************************************

*STSET PER USCITA DI CASA DOVUTA A MOTIVI DIVERSI DALLA CONVIVENZA (LEFT=1)


stset durhome, origin(time 15) failure(left==1)  id(id) exit(time 35)

sts graph
sts graph, hazard

sts graph, by(gndr)
sts graph, by(gndr) hazard

sts graph, by(famOrig)
sts graph, by(famOrig) hazard
***slide emp strat

*sts graph, by(cntry)
sts graph, by(europeanarea) 
sts graph, by(europeanarea) hazard
sts graph, by(SCANSUD) 
sts graph, by(SCANSUD) hazard
sts graph, by(ITASWE)
sts graph, by(ITASWE) hazard

*********************************************************

**italia
preserve
drop if country!=18
sts graph, by(famOrig)
*sts graph, by(famOrig) hazard
restore

**svezia
preserve
drop if country!=28
sts graph, by(famOrig)
*sts graph, by(famOrig) hazard
restore



*paesi scandinavi
preserve
drop if europeanarea!=5
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*sud europa
preserve
drop if europeanarea!=1
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*german speaking
preserve
drop if europeanarea!=2
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*nord
preserve
drop if europeanarea!=3
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*est
preserve
drop if europeanarea!=4
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore
*********************************

****

sts graph, by(cohort)
sts graph, by(cohort) hazard

****************************************************************
*CREO VARIABILI ANTECEDENTI E RILEVANTI DI CONTROLLO
*TIME VARYING

gen Edu=.
replace Edu=1 if (edulvlb==0 | edulvlb==113 | edulvlb==129)
replace Edu=2 if (edulvlb==212 | edulvlb==213 | edulvlb==221 | edulvlb==222 | edulvlb==223 | edulvlb==229)
replace Edu=3 if (edulvlb==311 | edulvlb==313 | edulvlb==321 | edulvlb==322 | edulvlb==323)
replace Edu=4 if (edulvlb==412 | edulvlb==413 | edulvlb==421 | edulvlb==422 | edulvlb==423)
replace Edu=5 if (edulvlb==510 | edulvlb==520 | edulvlb==610 | edulvlb==620 | edulvlb==710 | edulvlb==800)


**ISTRUZIONE

**eduyrs è la durata
fre eduyrs

stsplit educ, at(0) after(time=eduyrs)
stptime, by(educ)

tab eduyrs
sum eduyrs

br idno educ

fre educ

gen eduClass=0
replace eduClass=1 if educ==0 & (edulvla==1 | Edu==1)
replace eduClass=2 if educ==0 & (edulvla==2 | edulvla==3 | edulvla==4 | Edu==2 | Edu==3 | Edu==4)
replace eduClass=3 if educ==0 & (edulvla==5 | Edu==5)

label define eduClass 0"still studying" 1"low" 2"medium" 3"high", add
label values eduClass eduClass

stptime, by(eduClass)

fre eduClass

*educ=-1 se il soggetto sta sempre studiando
*eduClass=0 se il soggetto sta sempre studiando

*encode eduClass, gen(EC)
*recode EC (0=0 "Still studying") (1=1 "ISCED 0-1") (2=2 "ISCED 2") (3=3 "ISCED 3") (4=4 "ISCED 4") (5=5 "ISCED 5-6")

br idno educ eduClass eduyrs

**LAVORO

tab pdempyr
tab evpdemp
**si vede che i "not applicable" sono quelli che non hanno mai lavorato


gen age_job=(pdempyr-yrbrn-14) if evpdemp==1
replace age_job=(2006-yrbrn-14) if evpdemp==2

drop if age_job<0
drop if age_job==.
fre age_job

stsplit empl, at(0) after(time=age_job)
tab empl


recode empl -1=0 0=1 
label define empl 0"no" 1"yes", add
label values empl empl


stptime, by(empl)




sts graph, by(eduClass)
sts graph, by(eduClass) hazard

sts graph, by(empl)
sts graph, by(empl) hazard


*STSET PER USCITA DI CASA DOVUTA ALLA CONVIVENZA (LEFT=2)

stset durhome, origin(time 15) failure(left==2)  id(id) exit(time 35)

sts graph
sts graph, hazard

sts graph, by(gndr)
sts graph, by(gndr) hazard
sts graph, by(famOrig)
sts graph, by(famOrig) hazard


*sts graph, by(cntry)
sts graph, by(europeanarea) 
sts graph, by(europeanarea) hazard
sts graph, by(SCANSUD) 
sts graph, by(SCANSUD) hazard
*sts graph, by(ESPSWE)
sts graph, by(ITASWE) hazard

************************************************************
*paesi scandinavi
preserve
drop if europeanarea!=5
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*sud europa
preserve
drop if europeanarea!=1
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*german speaking
preserve
drop if europeanarea!=2
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*nord
preserve
drop if europeanarea!=3
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore

*est
preserve
drop if europeanarea!=4
sts graph, by(famOrig)
sts graph, by(famOrig) hazard
restore
*************************************************************

sts graph, by(cohort)
sts graph, by(cohort) hazard



sts graph, by(eduClass)
sts graph, by(eduClass) hazard

sts graph, by(empl)
sts graph, by(empl) hazard








*SURVIVAL 

ssc install stcompet, replace

stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc=ci, compet1(1)
gen cuminc1=cuminc if left==1
gen cuminc2=cuminc if left==2
twoway (line cuminc1 _t if left==1, sort) (line cuminc2 _t if left==2, sort) 

gen surv1=1-cuminc1 if left==1
gen surv2=1-cuminc2 if left==2
twoway (line surv1 _t if left==1, sort) (line surv2 _t if left==2, sort)




preserve
drop if europeanarea!=5
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc5=ci, compet1(1)
gen cuminc15=cuminc5 if left==1
gen cuminc25=cuminc5 if left==2
twoway (line cuminc15 _t if left==1, sort) (line cuminc25 _t if left==2, sort) 

gen surv15=1-cuminc15 if left==1
gen surv25=1-cuminc25 if left==2
twoway (line surv15 _t if left==1, sort) (line surv25 _t if left==2, sort)
restore
**SCANDINAVI


preserve
drop if europeanarea!=1
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc1_=ci, compet1(1)
gen cuminc11=cuminc1_ if left==1
gen cuminc21=cuminc1_ if left==2
twoway (line cuminc11 _t if left==1, sort) (line cuminc21 _t if left==2, sort) 

gen surv11=1-cuminc11 if left==1
gen surv21=1-cuminc21 if left==2
twoway (line surv11 _t if left==1, sort) (line surv21 _t if left==2, sort)
restore
*SUD



preserve
drop if europeanarea!=2
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc1_=ci, compet1(1)
gen cuminc11=cuminc1_ if left==1
gen cuminc21=cuminc1_ if left==2
twoway (line cuminc11 _t if left==1, sort) (line cuminc21 _t if left==2, sort) 

gen surv11=1-cuminc11 if left==1
gen surv21=1-cuminc21 if left==2
twoway (line surv11 _t if left==1, sort) (line surv21 _t if left==2, sort)
restore
*GERMAN SPEAKING

preserve
drop if europeanarea!=3
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc1_=ci, compet1(1)
gen cuminc11=cuminc1_ if left==1
gen cuminc21=cuminc1_ if left==2
twoway (line cuminc11 _t if left==1, sort) (line cuminc21 _t if left==2, sort) 

gen surv11=1-cuminc11 if left==1
gen surv21=1-cuminc21 if left==2
twoway (line surv11 _t if left==1, sort) (line surv21 _t if left==2, sort)
restore
*NORTHERN EUROPE

preserve
drop if europeanarea!=4
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc1_=ci, compet1(1)
gen cuminc11=cuminc1_ if left==1
gen cuminc21=cuminc1_ if left==2
twoway (line cuminc11 _t if left==1, sort) (line cuminc21 _t if left==2, sort) 

gen surv11=1-cuminc11 if left==1
gen surv21=1-cuminc21 if left==2
twoway (line surv11 _t if left==1, sort) (line surv21 _t if left==2, sort)
restore
*EAST EUROPE





preserve
drop if country!=18
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc18=ci, compet1(1)
gen cuminc118=cuminc18 if left==1
gen cuminc218=cuminc18 if left==2
twoway (line cuminc118 _t if left==1, sort) (line cuminc218 _t if left==2, sort) 

gen surv118=1-cuminc118 if left==1
gen surv218=1-cuminc218 if left==2
twoway (line surv118 _t if left==1, sort) (line surv218 _t if left==2, sort)
restore
*italia





preserve
drop if country!=28
stset durhome, origin(time 15) failure(left==2) exit(time 35) id(id)
stcompet cuminc28=ci, compet1(1)
gen cuminc128=cuminc28 if left==1
gen cuminc228=cuminc28 if left==2
twoway (line cuminc128 _t if left==1, sort) (line cuminc228 _t if left==2, sort) 

gen surv128=1-cuminc128 if left==1
gen surv228=1-cuminc228 if left==2
twoway (line surv128 _t if left==1, sort) (line surv228 _t if left==2, sort)
restore
*svezia

















stset durhome,  origin(time 15) failure(left) exit(time 35)

tab left

browse id left _st _d _t _t0 in 1/7

* Duplicate the cases (based on number of competing events)
expand 2

sort id
browse id left _st _d _t _t0 in 1/14

* Generate a failure type identifier
bysort id: gen type=_n

browse id _d type left in 1/14

* Create a new failure indicator (censoring variable)
gen _dnew=1 if type==left
replace _dnew=0 if type!=left
replace _dnew=0 if _d==0

browse id _d _dnew type left in 1/14

* Replace the old indicator with the new
replace _d=_dnew

browse id _d _dnew type left in 1/14

drop if type>2

***************
**MODELLI

**le covariate sono:
**famorig
*gndr
*cohort
*europeanarea
*eduClass
*empl

streg i.type, d(llogistic) time
streg i.type i.famOrig, d(llogistic) time

streg i.type##i.famOrig, d(llogistic) time


stcurve, survival at1(type=1 famOrig=1) at2(type=1 famOrig=2) at3(type=1 famOrig=3)
stcurve, hazard at1(type=1 famOrig=1) at2(type=1 famOrig=2) at3(type=1 famOrig=3) 

stcurve, survival at1(type=2 famOrig=1) at2(type=2 famOrig=2) at3(type=2 famOrig=3)
stcurve, hazard at1(type=2 famOrig=1) at2(type=2 famOrig=2) at3(type=2 famOrig=3) 



streg i.type##i.famOrig i.type##i.gndr, d(llogistic) time

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort, d(llogistic) time

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.europeanarea, d(llogistic) time

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.europeanarea i.type##ib1.eduClass, d(llogistic) time
**perchè famorig è non monotono??????



streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.europeanarea i.type##i.empl, d(llogistic) time


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.europeanarea i.type##ib1.eduClass i.type##i.empl, d(llogistic) time

**DEFINITIVO

**non vengono :(


stcurve, hazard at1(type=1 famOrig=1 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) at2(type=1 famOrig=2 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) at3(type=1 famOrig=3 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) 


stcurve, hazard at1(type=2 famOrig=1 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) at2(type=2 famOrig=2 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) at3(type=2 famOrig=3 gndr=1 cohort=2 europeanarea=3 eduClass=2 empl=1) 



*******modelli per aree europee (solo famOrig)

streg i.type##i.famOrig, d(llogistic) time, if europeanarea==1

streg i.type##i.famOrig, d(llogistic) time, if europeanarea==2

streg i.type##i.famOrig, d(llogistic) time, if europeanarea==3

streg i.type##i.famOrig, d(llogistic) time, if europeanarea==4


streg i.type##i.famOrig, d(llogistic) time, if europeanarea==5



*******modelli completi per aree europee

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##ib1.eduClass i.type##i.empl, d(llogistic) time, if europeanarea==1

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##ib1.eduClass i.type##i.empl, d(llogistic) time, if europeanarea==2

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##ib1.eduClass i.type##i.empl, d(llogistic) time, if europeanarea==3

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##ib1.eduClass i.type##i.empl, d(llogistic) time, if europeanarea==4

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##ib1.eduClass i.type##i.empl, d(llogistic) time, if europeanarea==5


fre country



***NON CONSIDERO EDUCLASS

streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==18
*italia



streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==28
*svezia


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==10
*spagna


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==13
*gran bretagna


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==15
*hungheria


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##i.empl, d(llogistic) time, if country==7
*germania


**CONSIDERO EDUCLASS


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==18
*italia



streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==28
*svezia


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==10
*spagna


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==13
*gran bretagna


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==15
*hungheria


streg i.type##i.famOrig i.type##i.gndr i.type##i.cohort i.type##eduClass i.type##i.empl, d(llogistic) time, if country==7
*germania



















