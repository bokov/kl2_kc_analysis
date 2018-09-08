#' ---
#' title: "Kidney Cancer Data Exploration (KL2 Aim 2)"
#' author: "Alex F. Bokov"
#' date: "08/09/2018"
#' ---
#' 
#+ init, echo=FALSE, include=FALSE, message=FALSE
# if running in test-mode, uncomment the line below
#options(gitstamp_prod=F);
.junk<-capture.output(source('global.R',echo=F));
.depends <- 'data.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- knitr::current_input();
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
.loadedobjects <- tload(.depdata);
knitr::opts_chunk$set(echo = F,warning = F,message=F);
# Set default arguments for some functions
panderOptions('table.split.table',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','left');
.args_default_v <- formals(v);
formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));
#' ### Overview
#' 
#' This is not (yet) a manuscript. We are still at the data cleaning/alignment
#' stage and it is far too early to draw conclusions. Rather, this is a
#' regularly updated progress report that I am sharing with you to keep you in
#' the loop on my work and/or because you are also working on NAACCR, i2b2, Epic,
#' or Sunrise and this might be useful to you or you might wish to offer advice.
#' 
#' So far, only de-identified data has been used to generate these results and
#' any dates or `patient_num` values you see here are also de-identified, though
#' the time intervals between events are not distorted.
#' 
#' At this time the analysis of de-identified data is under Dr. Michalek's 
#' exempt project IRB number HSC20170563N. I have been given guidelines under 
#' which we can share the de-identified data with UTHSCSA collaborators. If you 
#' would like a copy of the data, please email me and I will get back to you 
#' with further instructions and any additional information I might need from 
#' you for our records. The following versions of the dataset are available:
#' 
#' * Raw: the data as it literally exists when I input it into my scripts.
#' * Lightly curated: the main dataset as it is after my scripts are done processing it
#' * Moderately curated: the dataset pared down to just the columns and rows
#'   currently being used for analysis
#'   
#' Dr. Murphy, if you are interested in a copy of the data, I'll talk to my 
#' local mentors and IRB about the best way to do that. It's probably time we 
#' start talking about what approvals in general will be necessary for the full 
#' project. In case you are wondering, I am doing parts of Aim 2 ahead of Aim 1 
#' because it will help me identify the need for any additional recurring 
#' data-transformation rules to incorporate into DataFinisher all at once. I 
#' will switch to Aim 1, the i2b2 plugin, once I hit a natural pausing-point on 
#' Aim 2.
#' 
#' ### Questions for mentors and other domain experts:
#' 
#' * Question: What are the main problems with the NAACCR stage and grade information that
#'   I will need to clean up?
#' * Question: What is the typical time that elapses between diagnosis and 
#'   surgery?
#'     * Answer (RR): 2-4 weeks, try to avoid more than 4
#' * Question: Is it possible for surgery to happen on the same day as the 
#'   diagnosis? How common is that?
#'     * Answer (RR): Fairly common, if NAACCR diagnosis based on pathology 
#'       rather than clinical examination, which is usually technically a renal 
#'       mass, not a cancer. Might want to use imaging result date as the date 
#'       of diagnosis if it isn't already being used as such.
#' * Question: What would be the threshold on the lag to surgery until we must 
#'   conclude that there is an error in that record? E.g. is four years too 
#'   long?
#'     * Answer (RR): No, there are a few local cases that took over a decade to 
#'       get to surgery for various reasons (e.g. indolent tumor, or contact 
#'       lost with patient).
#' * Question: What fraction of KC patients undergo surgery?
#'     * Answer (RR): Around 15%
#' * How would one distinguish the chart of a patient who is was diagnosed for 
#'   the first time with a kidney tumor from that of a patient experiencing a 
#'   relapse... (_need to reach out to Grace_)
#'     * ...in Epic?
#'     * ...in Sunrise?
#' * Where in the chart would one positively establish the date of the patient's 
#'   first nephrectomy...
#'     * ...in Epic?
#'     * ...in Sunrise?
#' * Is there some additional data source that the UTHealth NAACCR registrar
#'   consults?
#'   
#' ### Questions to answer empirically:
#' 
#' * Question: Are NAACCR-EMR linkages now correct?
#'     * Motivation: For Sub-Aim 2a, I will be looking for possible mediators 
#'       of disparity, many of which will come from data outside NAACCR, linked
#'       via i2b2. For this reason I need to establish that NAACCR patients are
#'       linked to the correct records in the rest of i2b2.
#'     * Answer: [Yes](#consistency-checks) because [dates of birth](#how-well-do-birthdates-match-between-naaccr-and-the-emr),
#'       [sexes](#how-well-does-sex-match-up-between-the-emrs-and-naaccr), 
#'       [races](#how-well-does-race-match-up-between-the-emrs-and-naaccr),
#'       and [Hispanic ethnicity](#how-well-does-hispanic-ethnicity-match-up-between-the-emrs-and-naaccr)
#'       do not exhibit a greater degree of mismatch between NAACCR records and
#'       EMR records than would be expected from routine data entry errors at
#'       the source. Furthermore, the mismatches do not seem to correlate with
#'       each other.
#' * Question: Which elements in the raw data to use as our highest priority 
#'   analytic variables (dates of diagnosis, surgery, recurrence, and death as 
#'   well as ethnicity)
#'     * Motivation: For the main Aim 2, I am trying to determine whether there
#'       is an outcomes disparity associated with Hispanic ethnicity. There 
#'       needs to be a way to quickly validate it against data independent of 
#'       UTHealth. With the local data we cannot conclude anything at all about
#'       prevalence or incidence in the general population because we lack a
#'       comparator group for that and this is not part of my project. Instead, 
#'       I am testing the existence in outcome disparities among patients 
#'       already diagnosed with kidney cancer and those who have undergone 
#'       surgery for kidney cancer. Here local results _can_ be compared to 
#'       de-identified NAACCR regional and national data. Therefore I need to 
#'       establish the minimum set of NAACCR-only variables needed to replicate
#'       this analysis. If possible it would also be good to find corresponding
#'       EMR data elements so that incomplete NAACCR records can be back-filled
#'       with EMR data from i2b2.
#'     * Answer: Cannot back-fill missing NAACCR values from EMR without chart
#'       review and interviewing registrar but within NAACCR the following 
#'       have emerged as the main variables:
#'         1. [Diagnosis](#initial-diagnosis) = `n_ddiag` ([NAACCR `0390 Date of Diagnosis`](http://datadictionary.naaccr.org/default.aspx?c=10#390)
#'         , no others)
#'         2. [Surgery](#surgery-conclusion) = `n_dsurg` ([NAACCR `1200 RX Date--Surgery`](http://datadictionary.naaccr.org/default.aspx?c=10#1200)
#'            surgery, no others so far but may incorporate information from 
#'            additional variables after next data update)
#'         3. Recurrence and prior occurrence: _in progress_
#'         4. Death (TODO)
#' * Question: Which records to exclude due to likely errors in the source data? 
#'   E.g. surgery precedes diagnosis, recurrence precedes surgery (for some 
#'   analysis) death precedes diagnosis or surgery
#'       * Answer: Currently excluding as incomplete any record lacking either 
#'         an `n_ddiag` event or both of `n_kcancer` and `n_seer_kcancer` events.
#'         May soon start excluding the few patients with V/Z or surgical 
#'         history codes indicating missing kidney prior to first NAACCR 
#'         diagnosis.
# A list of valid patients can be found in the 'kcpatients.naaccr'
#'   
#' ### Outline
#' 
#' * [Consistency-Checks](#consistency-checks)
#' * [Cohort Characterization](#cohort-characterization)
#' * [Which EMR and NAACCR variables are reliable event indicators?](#which-emr-and-naaccr-variables-are-reliable-event-indicators)
#' * [Descriptive Plots (Preliminary)](#descriptive-plots-preliminary)
#' * Appendices
#'     1. [Example of stage/grade data](#appendix-i-example-of-stagegrade-data)
#'     2. [Next steps](#appendix-ii-next-steps)
#'     3. [Supplementary tables](#appendix-iii-supplementary-tables)
#'     4. [Audit trail](#appendix-iv-audit-trail)
#' 
#' ## Consistency-Checks
#' 
#' ### How well do birthdates match between NAACCR and the EMR?
#' 
#+ create_xdat,cache=TRUE
# To understand what the below code does, see the comments for the very similar
# pattern in 'data.R' in the neighborhood lines 148-191 as of 8/19/2018
# using 'union()' instead of 'c()' here to avoid cumulative growth if script is
# re-run by hand
l_tte <- union(l_tte,c('e_death','n_vtstat'));
xdat1 <- sapply(l_tte
                ,function(xx) substitute(if(any(ii==0)) age_at_visit_days[ii==0] 
                                         else NA,env=list(ii=as.name(xx)))) %>% 
  c(list(.data=select(subset(dat1,eval(subs_criteria$naaccr_complete))
                      ,c('age_at_visit_days',l_tte))),.) %>% 
  do.call(summarize,.);
#' There are `r nrow(subset(xdat1,is.na(n_dob)))` patients with
#' complete NAACCR records by current criteria but no NAACCR birthdate (here 
#' referred to as `n_dob`). Interestingly there are a few `n_dob` birthdates for 
#' patients who do _not_ have an `n_ddiag` (by informal inspection). There were
#' a total of `r length(kcpatients.bad_dob)` patients with a mismatch between 
#' their NAACCR and EMR birthdates, and __of the patients with complete records 
#' by current criteria, `r length(kcpatients.naaccr_bad_dob)` have a mismatch 
#' between their NAACCR and EMR birthdates__ . Below is a summary of the 
#' distribution of their `birth_date` variable minus their NAACCR date of birth
#' (in years):
dat0[!is.na(dat0[[cstatic_n_dob]]) & 
       dat0$patient_num%in%kcpatients.naaccr_bad_dob
     ,c('birth_date',cstatic_n_dob)] %>% 
  apply(2,as.Date) %>% apply(1,diff) %>% `/`(365.25) %>% summary %>% pander();
#' 
#' The `r length(kcpatients.naaccr_bad_dob)` patients with otherwise complete 
#' records but mismatched birth dates vary by huge amounts from the EMR versions
#' of their respective birth dates. However, as can be seen in [supplementary 
#' tables at the end of this document](#how-well-do-demographic-variables-match-up-for-just-the-patients-with) 
#' the `r length(kcpatients.bad_dob)` total patients with DOB mismatches are not 
#' particularly enriched for other mismatches I have tested so far which is more 
#' consistent with isolated errors in those respective variables rather than 
#' some subset of patients continuing to have their NAACCR and EMR records 
#' incorrectly linked.
#' 
#' ### How well does sex match up between the EMRs and NAACCR?
#' 
#' Columns represent NAACCR, rows represent EMR. Whole dataset, not filtered for
#' record completeness.
with(dat2,table(sex_cd,n_sex,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));

#' ### How well does race match up between the EMRs and NAACCR?
#' 
#' Columns represent NAACCR, rows represent EMR. Whole dataset, not filtered for
#' record completeness.
with(dat2,table(race_cd,a_n_race,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:4,1:4)));

#' ### How well does Hispanic ethnicity match up between the EMRs and NAACCR?
#' 
#' This time columns represent EMR and rows represent NAACCR. Whole dataset, not 
#' filtered for record completeness.
with(dat2,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                              ,'Unknown'='Non_Hispanic'
                              ,.default='Hispanic')
                ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));
#' More detailed ethnicity breakdown...
#' 
#' Again columns represent EMR and rows represent NAACCR. Whole dataset, not 
#' filtered for record completeness.
with(dat2,table(n_hisp,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>%
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=as.matrix(expand.grid(1:6,1:2)));

#' ## Cohort Characterization
#'
#' Summary of all the variables in the combined i2b2/NAACCR set. `Tumor_Free`
#' means no recurrence, `Tumor` means recurrence, and `Unknown` means unknown.
#' `No KC in NAACCR` means there is an EMR diagnosis of kidney cancer and there
#' may in some cases also be a _record_ for that patient in NAACCR but that 
#' record does not show the patient's site of occurence being kidney.
#' 
#' Note: the below variables are subject to change as the validity criteria and
#' creation of analytic variables from multiple columns of raw data evolve.
#' 
#+ TableOne, cache=TRUE
dat2[,unique(c('patient_num',v(c_analytic),'n_cstatus','e_death'
        ,'a_n_race','a_n_dm','a_e_dm','a_e_kc','n_kcancer'))] %>% 
  mutate(n_cstatus=ifelse(!patient_num%in%kcpatients.naaccr
                          ,'No KC in NAACCR',as.character(n_cstatus)) %>%
         factor(levels=c(levels(n_cstatus),'No KC in NAACCR'))
         ,age_at_visit_days=age_at_visit_days/365.25
         ,n_vtstat=n_vtstat!=-1
         ,s_death=s_death!=-1
         ,e_death=e_death!=-1
         #,n_kcancer=n_kcancer>=0
         ) %>%
  rename(`Age at Last Contact`=age_at_visit_days
         ,`Sex, i2b2`=sex_cd
         ,`Sex, Registry`=n_sex
         ,`Language, i2b2`=language_cd
         ,`Hispanic, i2b2`=e_hisp
         ,`Hispanic, Registry`=n_hisp
         ,`Race, i2b2`=race_cd
         ,`Race, Registry`=a_n_race
         ,`Marital Status, Registry`=n_marital
         ,`Deceased, Registry`=n_vtstat
         ,`Deceased, SSN`=s_death
         ,`Deceased, EMR`=e_death
         ,`Insurance, Registry`=n_payer
         ,`Diabetes, Registry`=a_n_dm
         ,`Diabetes, i2b2`=a_e_dm
         #,`Kidney Cancer, Registry`=n_kcancer
         ,`Kidney Cancer, i2b2`=a_e_kc
         ,BMI=e_bmi) %>% select(-patient_num) %>%
  select(sort(names(.))) %>% 
  CreateTableOne(vars = setdiff(names(.),'n_cstatus'),strata='n_cstatus',data = .,includeNA = T,test = F) %>% 
  print(printToggle=F) %>% 
  set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
                    ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
  gsub('0 \\( 0.0\\)','0',.) %>% 
  pander(emphasize.rownames=F);

#' ## Which EMR and NAACCR variables are reliable event indicators?
#' 
#' We need the following variables for starters. For most or all of these 
#' events, both data sources have multiple variables some or all of which could 
#' be indicators. We will likely need to merge groups of synonymous variables 
#' into one analytic variable each for NAACCR and for the EMR. This is to
#' mitigate for missing data. We can then do the same analysis on the same 
#' patients using NAACCR-only variables and EMR-only variables confirm that
#' they agree. If we can either show agreement or find and resolve the causes
#' of discrepancy this will permit other sites, which have not necessarily 
#' merged NAACCR and EMR data, to replicate our analysis. It will also allow us
#' to compare our results to national or Texas NAACCR data-sets which of course
#' are not linked to EMR data.
#' 
#' However, there will be even fewer missing observations and a richer choice of 
#' predictor variables if we work on a combined NAACCR and EMR dataset. 
#' Therefore for each of the below we will also need a third analytic variable 
#' combining NAACCR and EMR information. 
#' 
#' Our standard way of indexing time in this study is `age_at_visit_days`. 
#' The main table `dat1` will be collapsed into one row per patient, and the 
#' value for each of the above columns will be replaced with the age in days 
#' when that event was recorded (if any, otherwise `NA`). This table will be 
#' called `xdat1`. 
#' 
#' ### Initial diagnosis 
#' 
#' The `c_kcdiag` group of columns in `dct0`.
#' 
#' * NAACCR: `n_ddiag`. The other two-- the date accompanying the SEER site and
#'   the date accompanying the NAACCR primary site-- are not date fields in
#'   NAACCR, so whatever `start_date` they are getting assigned must be from our
#'   ETL process, not NAACCR and that is the code I will need to review. There is
#'   data element 443, [Date Conclusive
#'   DX](http://datadictionary.naaccr.org/default.aspx?c=10#443) but that is never
#'   recorded in our NAACCR. All other NAACCR data elements containing the word
#'   'date' seem to be retired or related to later events, not initial diagnosis.
#'   Whatever the case, there are only `r nrow(subset(xdat1,is.na(n_ddiag)&!is.na(n_kcancer)))`
#'   patients with a missing date of diagnosis but non-missing dates for the SEER
#'   site variable, so within the range of reasonable error at the NAACCR end.
#'   __Therefore `n_ddiag` (date of initial diagnosis) is the only NAACCR 
#'   variable on which we can rely for onset.__
#' * EMR: First occurence of any ICD9/10 code for kidney cancer. Naively, I had
#'   hoped that the first ICD9/10 code for kidney cancer would closely track the
#'   date for the `n_ddiag`. Unfortunately, as can be seen from the below table,
#'   for the `r sum(!is.na(xdat1$n_ddiag))` patients who have non-missing `n_ddiag` values, the first ICD9 
#'   and first ICD10 code most often occurs after initial diagnosis, sometimes 
#'   before the date of diagnosis, and coinciding with the date of diagnosis 
#'   rarest of all. By inspection I found that several of the ICD9/10 first 
#'   observed dates lead or trail the `n_ddiag` by multiple years! **Therefore, 
#'   one or both of the following steps are needed before EMR data can be relied 
#'   on at all for establishing date of onset** :
#'     * Meeting with CTRC NAACCR registrar to see how she obtains her dates of 
#'       onset
#'     * Chart review of a sample of NAACCR patients to understand what information
#'       visible in Epic sets them apart from non kidney cancer patients.
#'     * Chart review of the 60-100 patients with ICD9/10 codes for kidney 
#'       cancer that seemingly pre-date their `n_ddiag`.
#+ xdat1_icdtimes,cache=TRUE
# select the diagnosis-related variables
xdat1[,v(c_kcdiag)] %>% 
  # subtract from each column the 'n_ddiag' value if present
  # divide by one year and remove patients with missing 'n_ddiag'
  `-`(.,with(.,n_ddiag)) %>% `/`(365.25) %>% subset(!is.na(n_ddiag)) %>% 
  with({
    # in the scope of the resulting data frame create a throwaway function to 
    # break up each variable into before, roughly coincident with, or after 
    # 'n_ddiag'
    ff<-function(xx) cut(xx,breaks=c(-Inf,-7/365.25,7/365.25,Inf)
                         ,labels=c('before','+/- 2 weeks','after'),include = T); 
    # use this function to make our table
    table(ICD9=ff(e_kc_i9),ICD10=ff(e_kc_i10),useNA = 'if')}) %>% addmargins() %>% 
  # format for viewing
  pander();
#' ### Surgery
#' 
#' The `c_nephx` group of columns
#' 
#' * NAACCR: in addition to `1200 RX Date--Surgery` (in this script
#'   shortened to `n_dsurg`) and `3180 RX Date--Surgical Disch` (shortened to 
#'   `n_dsdisc` the following possibly relevant fields are available in our 
#'   local NAACCR and will be evaluated after the next data-pull:
#'      * `1260 Date of Initial RX--SEER`
#'      * `1270 Date of 1st Crs RX--CoC`
#'      * `3170 RX Date--Most Defin Surg`
#' * EMR: First occurrence of any ICD9/10 code for acquired absence of 
#'   kidney; or first occurence of surgical history of nephrectomy
#+ xdat1_surg, cache=TRUE
# make each date of surgery proxy relative to date of diagnosis
xdat1_surg <- (xdat1[,c(v(c_nephx),'n_drecur')] - xdat1$n_ddiag) %>% 
  # keep only the ones that have a date of diagnosis and sort by NAACCR 
  # surgery date, then convert to weeks.
  subset(!is.na(xdat1$n_ddiag)) %>% arrange(n_dsurg) %>% '/'(7);
#+ xdat1_surg_summary, cache=TRUE
# make a summary table for the 'c_nephx' candidate surgery proxy variables
xdat1_surg_summary <- summary(xdat1_surg) %>% 
  # extract the rownames from the arcane way that summary() returns them
  # getting rid of extra whitespace and then strip them out from the values
  set_rownames(.,trimws(gsub(':.*$','',`[`(.,,1)))) %>% gsub('^.*:','',.) %>% 
  # convert to numeric transposing as a (good) side-effect and set the rownames
  apply(1,as.numeric) %>% set_rownames(colnames(xdat1_surg)) %>% 
  # convert to data.frame without 'fixing' the column names.
  data.frame(check.names = F);
# list of variables that can first occur before 'n_ddiag'
t_priorcond <- paste(rownames(subset(xdat1_surg_summary,Min.<0)),collapse=', ');
#' As can be seen in the table below, the variables `r t_priorcond` _sometimes_ 
#' precede `n_ddiag` by many weeks. However, they _usually_ follow `n_ddiag` by 
#' more weeks than the two NAACCR variables `n_dsdisc` and `n_dsurg`. Those two 
#' NAACCR variables never occur before `n_ddiag` and usually occur within 2-8 
#' weeks.
#' 
#' As can be seen from the `NA's` column, the inactive ICD9/10 V/Z codes for
#' acquired absence of kidney are disqualified because they are very rare in 
#' addition to being even more divergent from the `n_ddiag` than the 
#' non-inactive codes.
pander(xdat1_surg_summary);
#' It's understandable if the Epic EMR lags behind NAACCR (because as an 
#' outpatient system, it's probably recording just the visits after the original
#' surgery, and perhaps we are not yet importing the actual surgery events from 
#' Sunrise EMR). But for the V or Z or surgical history codes that precede 
#' `n_ddiag`, it could mean that those NAACCR cases are not first-time 
#' occurrences. How big of a problem is this?
#' 
mutate_all(xdat1[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-xdat1$n_ddiag,breaks=c(-Inf,-.00001,.00001,Inf)
                             ,lab=c('before','same-day','after'),include=T)) %>%
  sapply(table,useNA='always') %>% t %>% pander();
#' Not too bad. Though we cannot trust the ICD9/10 codes as replacements for
#' missing surgery dates, there are few enough of them preceding diagnosis that
#' we can remove them as source data errors without ruining the sample size.
#' 
# 
# Here is a more general table, comparing every possible recurrence event or 
# surgery event to the NAACCR surgery variable, to clean up and uncomment later
# mutate_all(xdat1[,c(v(c_nephx),v(c_recur))]
#            # break each column 
#            ,function(xx) cut(xx-xdat1$n_dsurg,breaks=c(-Inf,-.00001,.00001,Inf)
#                              ,lab=c('before','same-day','after'),include=T)) %>% 
#   cbind(.,TOTAL=apply(.,1,function(xx) factor(
#     ifelse(any(xx=='before',na.rm=T),'before'
#            ,ifelse(any(xx=='same-day',na.rm=T),'same-day'
#                    ,ifelse(any(xx=='after',na.rm=T),'after',NA)))
#     ,levels=c('before','same-day','after')))) %>% 
#   sapply(table,useNA='always') %>% t %>% pander();
#' 
#' Now, as far as the two NAACCR variables go, does `n_dsdisc` (date of 
#' discharge) contribute anything more than `n_dsurg`? There are 
#' `r nrow(subset(xdat1,is.na(n_dsurg)&!is.na(n_dsdisc)))` non-missing values 
#' of `n_dsdisc` when `n_dsurg` is missing. As can be seen from the plot below
#' where `n_dsdisc` are the red dashed lines and `n_dsurg` are the black lines,
#' both relative to date of diagnosis, `n_dsdisc` either coincides with `n_dsurg`
#' or lags by multiple weeks, as might be expected of a discharge date (what is 
#' the plausible threshold on time from surgery to discharge?).
plot(xdat1_surg$n_dsurg,type='l',ylab='Weeks After Diagnosis'
     ,xlab='Patients, sorted by time from diagnosis to surgery'
     ,main='Time from diagnosis to surgery (black)\n or discharge (red)');
lines(xdat1_surg$n_dsdisc,col='red',lty=2);
#' 
#' ##### Surgery Conclusion
#' 
#' The sole variable on which we can rely for date of surgery is `n_dsurg`, 
#' though this might get supplemented by additional NAACCR variables in the next 
#' data-pull. However, we can rely on `r t_priorcond` for excluding possibly 
#' invalid records if any of them occur prior to `n_ddiag`.
#' 
#' 
#' ### Re-occurrence
#' 
#' The current available variables are: `n_cstatus` which corresponds to [`1770 Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1770)
#' ~~hopefully with `start_date` set by the ETL to [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772)
#' (need to double-check that it is)~~ and `n_drecur`, [`1860 Recurrence Date--1st `](http://datadictionary.naaccr.org/default.aspx?c=10#1860).
#' UPDATE: Our site is on NAACCR v16, not v18, and we do not have [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772).
#' According to the v16 standard, instead the [`1750 Date of Last Contact`](http://datadictionary.naaccr.org/default.aspx?c=10#1750)
#' should be used.
#' 
#' ~~It looks like it would also be useful in the next data
#' pull to include [`1880 Recurrence Type--1st`](http://datadictionary.naaccr.org/default.aspx?c=10#1880) 
#' which our NAACCR does use.~~ Done.
#' 
#' In the below plot, the white line is the time of `n_ddiag`. The black line is 
#' the time from `n_ddiag` until `n_dsurg`. The red line is `n_lc` (last contact).
#' The blue line is the minimum of several recurrence dates. Most of the time
#' the recurrence date seems to be bounded by the last contact (`n_lc`) and 
#' surgery `n_dsurg` dates. It is almost always bounded below by the diagnosis
#' date `n_ddiag`.
#+ event_plot_diag2surg,cache=TRUE
.eventplot01 <-event_plot(subset(xdat1,!patient_num %in% kcpatients.naaccr_dupe)
                          ,'n_dsurg','n_lc',start_event = 'n_ddiag'
                          ,ltys = c(1,1));
abline(h=0,col='white');
lines(do.call(pmin,c(.eventplot01[,v(c_recur,.eventplot01)],na.rm=T)),col='blue'
      ,lty=3);
#' Here's something wierd though-- the date of first contact `n_fc` (red) is 
#' almost always between last contact `n_lc` (black) and diagnosis `n_ddiag` 
#' (white), though diagnosis is usually on a biopsy sample and that's why it's 
#' dated as during or after surgery we thought. If first contact is some kind of 
#' event after first diagnosis, what is it?
#+ event_plot_diag2lc,cache=TRUE
.eventplot02 <-event_plot(subset(xdat1,!patient_num %in% kcpatients.naaccr_dupe)
                          ,'n_lc','n_fc',start_event = 'n_ddiag'
                          ,ltys = c(1,1));
abline(h=0,col='white');
#' Surgery `n_dsurg` seems to happen in significant amounts both before and 
#' after first contact `n_fc`.
#'  
#' ### Death
#'    
#' ### Whether or not the patient is Hispanic
#' 
#' A similar process needs to be done for Hispanic ethnicity, but as an ordinary 
#' static variable rather than time-to-event. I think I'll do two variables: one 
#' that is true if we are very sure the patient is Hispanic, and the other one 
#' that is true if we aren't certain the patient is _not_ Hispanic. In both 
#' cases, there will also be `Unknown` bins for where all variables are 
#' unanimous on the patient's Hispanic status being unknown.
#' 
#' Basically two variables because there are the two ends of the spectrum for
#' resolving disagreement about a binary variable between multiple sources.
#' 
#' 
#' ## Descriptive Plots (Preliminary)
#' 
#' To avoid bias/overfitting all descriptive data and visualizations below that 
#' relate the predictor variable to the outcome are done using a randomly 
#' selected subset of the records (N=`r length(pat_samples$train)`).
#' 
#+ surv_surg,cache=TRUE
# subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$diag_surg)) %>% 
#   summarise(age=age_at_visit_days[a_tdiag==0]
#             ,a_tdiag=last(a_tdiag),a_csurg=last(a_csurg)
#             ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
#   survfit(Surv(a_tdiag,a_csurg)~hisp,.) %>% 
#   autoplot(mark.time=T,xlab='Days Since Diagnosis',ylab='% No Surgery Yet'
#            ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2);
# subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$diag_surg)) %>% 
#   summarise_all(function(xx) {browser();last(na.omit(xx))}) %>%
#   survfit(Surv(a_tdiag,a_csurg)~1,.) %>% 
#   autoplot(mark.time=T,xlim=c(0,2000)
#            ,xlab='Days Since Diagnosis',ylab='% Not Undergone Surgery');
subset(dat1
       # subset the patient histories to on and after the day of diagnosis
       # (according to NAACCR) and on or before the day of surgery (==0) or
       # last follow-up (<0), and also limit the patients to ones randomly 
       # assigned to the training set so we're not "peeking at the answers"
       ,n_ddiag>=0&n_dsurg<=0&patient_num %in% pat_samples$train)[
         # sorry to about the syntactically valid but awkward line break, we 
         # are selecting only the columns from the subsetted data frame that
         # we will need for this survival curve
         ,c('patient_num','n_ddiag','n_dsurg','e_hisp','n_hisp')] %>% 
  # take the last non-missing event from each column
  summarise_all(function(xx) {
    if(is.logical(xx)) any(xx) else (last(na.omit(xx)))}) %>% 
  # convert time to weeks, and truncate on last followup period
  mutate(n_ddiag=n_ddiag/7,n_ddiag=pmin(n_ddiag,52.179*2)
         # we're setting the follow-up time to one year, and censoring any 
         # surgeries that happened more than a year from the initial diagnosis
         # (or that never happened, n_dsurg!=0)
         ,cen=n_ddiag<52.179&n_dsurg==0
         # simplifying the 'n_hisp' variable
         ,n_hisp=!n_hisp%in%c('Non_Hispanic','Unknown')) %>% 
  # fitting a survival curve
  survfit(Surv(n_ddiag,n_ddiag<=52.179*2&n_dsurg==0)~n_hisp,.) %>% 
  # generating a plot for the survival curve
  autoplot(mark.time=T
           ,xlab='Weeks Since Diagnosis',ylab='% Not Undergone Surgery'
           ,main='Time from Diagnosis to Surgery') + 
  # cleaning up the legend for this plot
  guides(colour=guide_legend('Hispanic'),fill=guide_legend('Hispanic'));
#' 
#' So far it seems there is no great difference in the raw lag between Hispanic
#' and non-Hispanic patients but with the major caveat that I have not yet 
#' finalized the Hispanic variable and this does not account for other 
#' covariates like age and stage.
#' 
#+ surv_recur,cache=TRUE
subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_recur)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_crecur=last(a_crecur)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_crecur)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving in Remission'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1)
           ,main='Time from Surgery to Recurrence') +
  guides(colour=guide_legend('Hispanic'),fill=guide_legend('Hispanic'));
#' 
#' Does recurrence-free survival after surgery differ between hispanic and non 
#' hispanic patients?
#' 
#'  
#'
#+ surv_death,cache=TRUE
subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_death)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_cdeath=last(a_cdeath)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_cdeath)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1)
           ,main='Survival After Surgery') + 
  guides(colour=guide_legend('Hispanic'),fill=guide_legend('Hispanic'));
#' 
#' Does survival after surgery (insofar that it is reliably represented in the
#' records) differ between hispanic and non-hispanic patients?
#' 
# subset(dat1,eval(subs_criteria$surg_death)) %>% 
#   summarise_all(function(xx) last(na.omit(xx))) %>%
#   survfit(Surv(a_tsurg,a_cdeath)~1,.) %>% 
#   autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving');

#' ---
#' 
#' ## Appendix I: Example of stage/grade data
#' 
#' (proof of feasibility)
#'
subset(dat2[,c('patient_num',v(c_tnm,NA))],patient_num %in% kcpatients.naaccr) %>% 
  na.omit() %>% 
  setNames(c('patient_num'
             ,submulti(v(c_tnm,NA)
                       ,cbind(v(c_tnm,NA),v(c_tnm,NA,retcol = 'colname_long'))))) %>% 
  # head(5) %>% 
  # show a sampling of rows and columns that fits on the page and remove the
  # the extra quotation marks
  `[`(1:15,1:8) %>% apply(2,function(xx) gsub('["]','',xx)) %>% 
  pander();

#' ---
#' 
#' ## Appendix II: Next steps
#' 
#' * TODO: Prior to doing the above `tte()` put in a safeguard to make
#'         sure all the `c_tte` variables are `TRUE/FALSE` only. They
#'         are right now as it happens, but nothing enforces that.
#' * TODO: Create combined (if applicable) variables for each of the following:
#'     * ~~Initial diagnosis~~
#'     * ~~Surgery~~ _pending additional variables from next data pull_
#'     * Re-ocurrence
#'     * _Last follow-up ?_
#'     * Death
#'     * Strict Hispanic designator
#'     * Lenient Hispanic designator
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' * TODO: Create unified Hispanic indicators
#' * TODO: Create access/quality variables including: number of visits per year, 
#'         number of lab tests and imaging orders per visit, time spent with 
#'         provider per visit
#' * TODO: Resume effort to link Mays Center historic trial records from IDEAS 
#'         to get information about enrollment in adjuvant trials
#' * TODO: Start validating and using additional 2a variables already in current 
#'         data
#'     * `[CN101] OPIOID ANALGESICS` (EMR)
#'     * `[CN103] NON-OPIOID ANALGESICS` (EMR)
#'     * `0250 Birthplace` (NAACCR possibly EMR)
#'     * Language (NAACCR and EMR)
#'     * smoking and alcohol (EMR)
#'     * Diabetes (NAACCR and EMR)
#'     * Family history (EMR)
#'     * Labs (EMR) including:  hemoglobin A1c, HDL, VLDL
#'     * Vitals (EMR) including: systolic and diastolic blood pressure, BMI
#'     * income (Census)
#'     * Miperamine, other anti-depressants
#'     * Should use [`0580 Date of 1st Contact`](http://datadictionary.naaccr.org/default.aspx?c=10#580)
#'       as the diagnosis date if earlier than `n_ddiag`!
#'     * Surgery fields:
#'         * [`1260 Date of Initial RX--SEER`](http://datadictionary.naaccr.org/default.aspx?c=10#1260)
#'         * [`1270 Date of 1st Crs RX--CoC`](http://datadictionary.naaccr.org/default.aspx?c=10#1270)
#'         * [`3170 RX Date--Most Defin Surg`](http://datadictionary.naaccr.org/default.aspx?c=10#3170)
#'     * Recurrence: [`1880 Recurrence Type--1st`](http://datadictionary.naaccr.org/default.aspx?c=10#1880) 
#' * TODO: In next re-run of query...
#'     * Follow up re additional patient linkages, more recent NAACCR data
#'     * education (Census, not ready, ETL needs fixing)
#' * DONE: ~~Verify that the [ETL](http://www.hostedredmine.com/issues/719444#note-11) 
#'         gets `start_date` for `1770 Cancer Status` from 
#'         [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1770)~~
#'         _in NAACCR v16 it doesn't need to_
#' * DONE: ~~tableOne~~
#' * DONE: ~~Create time-since-first-diagnosis variable~~
#' * DONE: ~~Create a special TTE variable from the main i2b2 age at death~~
#' * DONE: ~~Matrices of pairwise differences between all TTE variables~~
#' * DONE: ~~Create TTE variable for death (several raw variables)~~
#' * DONE: ~~Create TTE variable for recurrence~~
#' * DONE: ~~Create TTE variable for surgery date~~
#' * DONE: ~~Plot time from diagnosis to surgery, hisp vs non~~
#'     * ~~First need to confirm interpretation of outcome variable~~
#' * DONE: ~~Apply the `tte()` function to all variable in `c_tte`~~
#' * DONE: ~~Create censoring variable for surgery~~
#' * DONE: ~~Create censoring variable for recurrence/death~~
#' * DONE: ~~Map cancer status variable (didn't turn out to be useful)~~
#' * DONE: ~~Create unified comorbidity variable for:~~
#'     * DONE ~~Diabetes~~
#' * DONE: ~~Mappings for other numcode variables~~
#' * DONE: ~~Re-run query with additional variables (_query completed_):~~
#'     * ~~EMR codes for secondary tumors~~
#'     * ~~median household income, 2016 and 2013~~
#'     * ~~HbA1c~~
#'     * ~~Family history of diabetes and cancer~~
#' 
#' ---
#' 
#' ## Appendix III: Supplementary tables
#' 
#' ### How well do demographic variables match up for just the patients with mismatched birthdates?
#' 
#' #### Sex
#' 
#+ dat2_bad_dob, cache=TRUE
dat2_bad_dob <- subset(dat2,patient_num %in% kcpatients.bad_dob);

#' Columns represent NAACCR, rows represent EMR. Only DOB mismatched patients.
with(dat2_bad_dob,table(sex_cd,n_sex,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));

#' #### Race
#' 
#' Columns represent NAACCR, rows represent EMR. Only DOB mismatched patients.
with(dat2_bad_dob,table(race_cd,a_n_race,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=as.matrix(expand.grid(1:4,1:4)));

#' #### Hispanic ethnicity
#' 
#' This time columns represent EMR and rows represent NAACCR. Only DOB 
#' mismatched patients.
with(dat2_bad_dob,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                              ,'Unknown'='Non_Hispanic'
                              ,.default='Hispanic')
                ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));
#'
#' #### Nephrectomy according to EMR preceding diagnosis according to NAACCR
#' 
#' Only complete NAACCR records with mismatched DOBs.
#' 
#' Looks like the `r length(kcpatients.naaccr_bad_dob)` DOB-mismatched patients 
#' otherwise meeting completeness criteria for kidney cancer records do not 
#' coincide with the set of patients seeming to have nephrectomies prior to 
#' their NAACCR diagnoses.
xdat1_bad_dob <- subset(xdat1,patient_num %in% kcpatients.bad_dob);
mutate_all(xdat1_bad_dob[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-xdat1_bad_dob$n_ddiag
                             ,breaks=c(-Inf,-.00001,.00001,Inf)
                             ,lab=c('before','same-day','after'),include=T)) %>%
  sapply(table,useNA='always') %>% t %>% pander();
#' ### What is the coverage of valid records in each data source.
#' 
#' How many patients are in NAACCR, the EMR, both, neither, or have a diagnosis
#' prior to first available record?
consort_table[with(consort_table,order(PreExisting,decreasing = T)),] %>% 
  mutate(`N Cumulative`=rev(cumsum(rev(N)))) %>% pander();
#' _This has been temporarily moved from the main section pending finalization
#' of the recurrence variables. For now, the only ones we can be sure of 
#' [as indicators of a pre-existing condition](#surgery-conclusion) as exclusion
#' criteria for possibly invalid records are `r t_priorcond` if they occur 
#' prior to `n_ddiag` and those will exclude far fewer records than suggested 
#' by this table_ .
#' 
#' ### Which variables are near-synonymous?
#' 
#' Some variables will, despite what they sound like will be clearly unrelated 
#' to each other. Others will be in high pairwise agreement when both are
#' non-missing. The ones in between need to be investigated further to determine
#' whether they are more informative than no information at all, whether they
#' can be cleaned up, and whether there is a bias (i.e. one variable will 
#' consistently lag another variable). 
#' 
#' In the `data.R` script we will convert all the event variables to a time to
#' event (tte) form. The above variables plus a few that are dates which aren't 
#' currently known to correlate with any of the events of interest, but doesn't 
#' hurt to check. The overall approach will be:
#'  
#' 1. Take for each patient the first visit where the variable is TRUE, 
#'    non-missing, or in some cases meets some other criteria.
#' 2. Center the `age_at_visit_days` variable on that visit, so for that patient
#'    it is `0` on the visit, a negative integer prior to the visit, and a 
#'    positive integer after. It will be seen later that this will help make
#'    survival analysis easier when we get to it. For patients where an event is
#'    never observed, these numbers will be shifted to that the value at the
#'    last visit is `-1`, _not_ `0`. This is so that we can easily distinguish 
#'    patients where the event never occurred.
#' 
#' Then we will be ready to probe the degree of agreement and size of lags 
#' between these variables.
#' 
#' We will then obtain diagonal matrices of various pairwise comparisons of
#' the timing of events. Not only the ones believed to reflect the same event, 
#' but all of them. This is so that we can do an overall sanity check on the 
#' relationships between  groups of variables. For example, if the supposed 
#' dates of surgery are in good agreement with each other, but they often happen 
#' after the supposed date of reoccurence, then that would be a problem we need 
#' to resolve before proceeding further. The below heatmap indicates the 
#' fraction of the column events that occurred before or at the same time as the
#' row events.
#+ medians_heatmap,cache=TRUE,fig.width=10,fig.height=10
xdat1.gteq<-outer(xdat1[,-1],xdat1[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) mean(aa>bb,na.rm = T),xx,yy));
xdat1.meds<-outer(xdat1[,-1],xdat1[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(aa-bb,.5,na.rm = T),xx,yy));
xdat1.mabs<-outer(xdat1[,-1],xdat1[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(abs(aa-bb),.5,na.rm = T),xx,yy));
xdat1.mabx<-outer(xdat1[,-1],xdat1[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(abs(aa-bb),na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
xdat1.maxs<-outer(xdat1[,-1],xdat1[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
xdat1.mins<-outer(xdat1[,-1],xdat1[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-min(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));

# We need to exclude the 'n_dob' variable because it otherwise screws up the
# scaling. Also excluding all variables that have fewer than 10 non-null 
# observations
.xdat1.keep <- !colnames(xdat1.gteq) %in% 
  c(names(xdat1)[colSums(!is.na(xdat1))<10],'n_dob');
# This is to distinguish missing values from 0 values in a heatmap! No other way
# to do that!!
#layout(matrix(1,nrow=2,ncol=2));
par(bg='gray'); #,mfrow=1:2,mfcol=1:2);
heatmap(xdat1.gteq[.xdat1.keep,.xdat1.keep],symm = T,na.rm = T,margins=c(10,10)
        ,col=colorRampPalette(c('pink','red','darkred'))(2000));
# ,col=color.palette(c('darkred','red','pink','white','lightblue','blue'
#                      ,'darkblue'),n.steps=c(3,200,2,2,200,3))(2000));

#' 
#' A lot to unpack here! We can already see that some variables are in close
#' agreement. Another early conclusion from this is that it isn't looking good 
#' for EMR events lining up with NAACCR events... they seem to lag behind NAACCR 
#' dates, especially diagnoses and surgical history. Might need to see if there
#' is something in the EMR that captures date of surgery (especially in Sunrise)
#' and chart review to see why the KC diagnosis codes lag behind NAACCR
#' diagnosis date.
#' 
#' Closer visualization of individual groups of variables can be accomplished by 
#' subsetting from this master table.
#' 
#' In addition to medians, we might also generate tables of the 5th and 95th 
#' percentiles of the differences as well as medians of the absolute values of
#' the differences. The former are for identifying directional trends and the
#' latter are to distinguish variables that track each other from variables that
#' are uncorrelated but their difference is unbiased in one direction versus 
#' another.
#' 
#' However, most of this shotgun approach is now superseded by the more focused 
#' investigation in the [initial diagnosis](#initial-diagnosis) and 
#' [surgery](#surgery-conclusion) sections in the main document above. This is
#' just for historic reference.
#' 
#' ---
#' 
#' ## Appendix IV: Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
