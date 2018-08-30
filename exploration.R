#' ---
#' title: "Kidney Cancer Data Exploration"
#' author: "Alex F. Bokov"
#' date: "08/09/2018"
#' ---
#' 
#+ echo=FALSE, inlcude=FALSE, message=FALSE
# if running in test-mode, uncomment the line below
options(gitstamp_prod=F);
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
#' Set default arguments for some functions
.args_default_v <- formals(v);
formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));
knitr::opts_chunk$set(echo = F,warning = F,message=F);
#' ### Questions for Domain Expert
#' 
#' * How would one distinguish the chart of a patient who is was diagnosed for 
#'   the first time with a kidney tumor from that of a patient experiencing a 
#'   relapse...
#'     * ...in Epic?
#'     * ...in Sunrise?
#' * Where in the chart would one positively establish the date of the patient's 
#'   first nephrectomy...
#'     * ...in Epic?
#'     * ...in Sunrise?
#' * Is there some additional data source that the UTHealth NAACCR registrar
#'   consults?
#' 
#' ### Consistency-Checks
#' 
#' How well does sex match up between the EMRs and NAACCR?
with(dat2,table(sex_cd,n_sex,useNA = 'ifany')) %>% addmargins() %>% 
  pander(split.tables=600,justify='right'
         ,emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));

#' How well does race match up between the EMRs and NAACCR?
with(dat2,table(race_cd,a_n_race,useNA = 'ifany')) %>% addmargins() %>% 
  pander(split.tables=600,justify='right'
         ,emphasize.strong.cells=as.matrix(expand.grid(1:4,1:4)));

#' How well does Hispanic ethnicity match up between the EMRs and NAACCR?
with(dat2,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                              ,'Unknown'='Non_Hispanic'
                              ,.default='Hispanic')
                ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(justify='right'
                          ,emphasize.strong.cells=as.matrix(expand.grid(1:2,1:2)));
#' More detailed ethnicity breakdown...
with(dat2,table(n_hisp,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>%
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(justify='right'
                          ,emphasize.strong.cells=as.matrix(expand.grid(1:6,1:2)));

#' ### Cohort Characterization
#' How many patients are in NAACCR, the EMR, both, neither, or have a diagnosis
#' prior to first available record?
# rbind(consort_table
#       ,summarise(consort_table
#                  ,NAACCR='',EMR='',PreExisting='',N=sum(N))) %>% pander;
consort_table[with(consort_table,order(PreExisting,decreasing = T)),] %>% 
  mutate(`N Cumulative`=rev(cumsum(rev(N)))) %>% pander(justify='right');
#'
#' Summary of all the variables in the combined i2b2/NAACCR set
#+ TableOne
dat2[,c(v(c_analytic),'n_cstatus'
        ,'a_n_race','a_n_dm','a_e_dm','a_e_kc')] %>% 
  mutate(n_cstatus=ifelse(is.na(n_cstatus),'Not in NAACCR',as.character(n_cstatus)) %>%
         factor(levels=c(levels(n_cstatus),'Not in NAACCR'))
         ,age_at_visit_days=age_at_visit_days/365.25) %>%
  rename(`Age at Last Contact`=age_at_visit_days
         ,`Sex, i2b2`=sex_cd
         ,`Sex, Registry`=n_sex
         ,`Language, i2b2`=language_cd
         ,`Hispanic, i2b2`=e_hisp
         ,`Hispanic, Registry`=n_hisp
         ,`Race, i2b2`=race_cd
         ,`Race, Registry`=a_n_race
         ,`Marital Status, Registry`=n_marital
         ,`Vital Status, Registry`=n_vtstat
         ,`Deceased, SSN`=s_death
         ,`Insurance, Registry`=n_payer
         ,`Diabetes, Registry`=a_n_dm
         ,`Diabetes, i2b2`=a_e_dm
         ,`Kidney Cancer, Registry`=n_kcancer
         ,`Kidney Cancer, i2b2`=a_e_kc
         ,BMI=e_bmi) %>%
  CreateTableOne(vars = setdiff(names(.),'n_cstatus'),strata='n_cstatus',data = .,includeNA = T,test = F) %>% 
  print(printToggle=F) %>% 
  set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
                    ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
  pander(split.table=600,justify='lrrrr',emphasize.rownames=F);

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
#' ### Choosing Event Variables
#' 
#' Our standard way of indexing time in this study is `age_at_visit_days`. 
#' The main table `dat1` will be collapsed into one row per patient, and the 
#' value for each of the above columns will be replaced with the age in days 
#' when that event was recorded (if any, otherwise `NA`). This table will be 
#' called `xdat1`. 
#+ create_xdat,cache=TRUE
# To understand what the below code does, see the comments for the very similar
# pattern in 'data.R' in the neighborhood lines 148-191 as of 8/19/2018
# using 'union()' instead of 'c()' here to avoid cumulative growth if script is
# re-run by hand
l_tte <- union(l_tte,c('e_death','n_vtstat'));
xdat1 <- sapply(l_tte
                ,function(xx) substitute(if(any(ii==0)) age_at_visit_days[ii==0] 
                                         else NA,env=list(ii=as.name(xx)))) %>% 
  c(list(.data=select(subset(dat1,!eval(subs_criteria$prior_cancer))
                      ,c('age_at_visit_days',l_tte))),.) %>% 
  do.call(summarize,.) %>% `[`(-1);
#' 
#' #### Initial diagnosis 
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
                         ,labels=c('before','+/- 2 weeks','after')); 
    # use this function to make our table
    table(ICD9=ff(e_kc_i9),ICD10=ff(e_kc_i10),useNA = 'if')}) %>% addmargins() %>% 
  # format for viewing
  pander(justify='right');
#' #### Surgery
#' 
#' The `c_nephx` group of columns
#' 
#' * NAACCR: in addition to `1200 RX Date--Surgery` (in this script
#'   shortened to `n_dsurg`) and `3180 RX Date--Surgical Disch` the following 
#'   possibly relevant fields are available in our local NAACCR and will be 
#'   evaluated after the next data-pull:
#'      * `1260 Date of Initial RX--SEER`
#'      * `1270 Date of 1st Crs RX--CoC`
#'      * `3170 RX Date--Most Defin Surg`
#' * EMR: First occurrence of any ICD9/10 code for acquired absence of 
#'   kidney; or first occurence of surgical history of nephrectomy
#'  
#' #### Re-ocurrence
#' 
#' #### Death
#'    
#' #### Whether or not the patient is Hispanic
#' 
#' A similar process needs to be done for Hispanic ethnicity, but as an ordinary 
#' static variable rather than time-to-event. I think I'll do two variables: one 
#' that is true if we are very sure the patient is Hispanic, and the other one 
#' that is true if we aren't certain the patient is _not_ Hispanic. In both 
#' cases, there will also be an `Unknown` bins for where all variables are 
#' unanimous on the patient's Hispanic status being unknown.
#' 
#' Basically two variables because there are the two ends of the spectrum for
#' resolving disagreement about a binary variable between multiple sources.
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
#' We will then obtain a diagonal matrix of median differences between each pair 
#' of variables. Not only the ones believed to reflect the same event, but all 
#' of them. This is so that we can do an overall sanity check on the 
#' relationships between  groups of variables. For example, if the supposed 
#' dates of surgery are in good agreement with each other, but they often happen 
#' after the supposed date of reoccurence, then that would be a problem we need 
#' to resolve before proceeding further. 
#+ medians_heatmap,cache=TRUE,fig.width=10,fig.height=10
xdat1.gteq<-outer(xdat1,xdat1,FUN = function(xx,yy)
  mapply(function(aa,bb) mean(aa>bb,na.rm = T),xx,yy));
xdat1.meds<-outer(xdat1,xdat1,FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(aa-bb,.5,na.rm = T),xx,yy));
xdat1.mabs<-outer(xdat1,xdat1,FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(abs(aa-bb),.5,na.rm = T),xx,yy));
xdat1.mabx<-outer(xdat1,xdat1,FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(abs(aa-bb),na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
xdat1.maxs<-outer(xdat1,xdat1,FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
xdat1.mins<-outer(xdat1,xdat1,FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-min(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));

# We need to exclude the 'n_dob' variable because it otherwise screws up the
# scaling
.xdat1.keep <- colnames(xdat1)!='n_dob';
# This is to distinguish missing values from 0 values in a heatmap! No other way
# to do that!!
#layout(matrix(1,nrow=2,ncol=2));
par(bg='gray'); #,mfrow=1:2,mfcol=1:2);
heatmap(xdat1.gteq[.xdat1.keep,.xdat1.keep],symm = T,na.rm = F,margins=c(10,10)
        ,col=colorRampPalette(c('pink','red','darkred'))(2000));
        # ,col=color.palette(c('darkred','red','pink','white','lightblue','blue'
        #                      ,'darkblue'),n.steps=c(3,200,2,2,200,3))(2000));

#' _RED indicates row-event occurred after column-event and BLUE indicates that
#' row-event occurred before column-event._
#' 
#' A lot to unpack here! We can already see that some variables are in close
#' agreement (but these are just medians, this needs to be confirmed
#' on just those groups of variables by checking whether they EVER differ when
#' when both are present... if they never differ, we can treat them as 
#' synonymous in casese where one or the other is missing assuming these 
#' conclusions are on a reasonably large sample size). Another early conclusion
#' from this is that it isn't looking good for EMR events lining up with NAACCR 
#' events out of the box... they seem to lag behind NAACCR dates, especially 
#' diagnoses and (not surprisingly) surgical history. Might need to see if there
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
#' 
#' ## Descriptive Plots (Preliminary)

# subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$diag_surg)) %>% 
#   summarise(age=age_at_visit_days[a_tdiag==0]
#             ,a_tdiag=last(a_tdiag),a_csurg=last(a_csurg)
#             ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
#   survfit(Surv(a_tdiag,a_csurg)~hisp,.) %>% 
#   autoplot(mark.time=T,xlab='Days Since Diagnosis',ylab='% No Surgery Yet'
#            ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2);
subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$diag_surg)) %>% 
  summarise_all(function(xx) last(na.omit(xx))) %>%
  survfit(Surv(a_tdiag,a_csurg)~1,.) %>% 
  autoplot(mark.time=T,xlim=c(0,2000)
           ,xlab='Days Since Diagnosis',ylab='% Not Undergone Surgery');
#' What is the overall response range for the lag from diagnosis to surgery?

subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_recur)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_crecur=last(a_crecur)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_crecur)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving in Remission'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1));
#' Does recurrence-free survival after surgery differ between hispanic and non 
#' hispanic patients?

subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_death)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_cdeath=last(a_cdeath)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_cdeath)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1));
#' Does survival after surgery (insofar that it is reliably represented in the
#' records) differ between hispanic and non-hispanic patients?

# subset(dat1,eval(subs_criteria$surg_death)) %>% 
#   summarise_all(function(xx) last(na.omit(xx))) %>%
#   survfit(Surv(a_tsurg,a_cdeath)~1,.) %>% 
#   autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving');

#' ### Example of stage/grade data
#' 
#' (proof of feasibility)
#'
subset(dat2[,c('patient_num',v(c_tnm,NA))],patient_num %in% kcpatients.naaccr) %>% 
  na.omit() %>% 
  setNames(c('patient_num'
             ,submulti(v(c_tnm,NA)
                       ,cbind(v(c_tnm,NA),v(c_tnm,NA,retcol = 'colname_long'))))) %>% 
  head(10) %>% pander(split.tables=1000);

#' 
#' ### Next steps
#' 
#' * DONE: ~~tableOne~~
#' * DONE: ~~Create time-since-first-diagnosis variable~~
#' * DONE: ~~Create TTE variable for death (several raw variables)~~
#' * DONE: ~~Create TTE variable for recurrence~~
#' * DONE: ~~Create TTE variable for surgery date~~
#' * TODO: Plot time from diagnosis to surgery, hisp vs non
#'     * _First need to confirm interpretation of outcome variable_
#' * DONE: ~~Apply the `tte()` function to all variable in `c_tte`~~
#' * TODO: Prior to doing the above `tte()` put in a safeguard to make
#'         sure all the `c_tte` variables are `TRUE/FALSE` only. They
#'         are right now as it happens, but nothing enforces that.
#' * DONE: ~~Create a special TTE variable from the main i2b2 age at death~~
#' * DONE: ~~Matrices of pairwise differences between all TTE variables~~
#' * TODO: Create combined variables for each of the following:
#'     * Initial diagnosis
#'     * Surgery
#'     * Re-ocurrence
#'     * _Last follow-up ?_
#'     * Death
#'     * Strict Hispanic designator
#'     * Lenient Hispanic designator
#' * DONE: ~~Create censoring variable for surgery~~
#' * DONE: ~~Create censoring variable for recurrence/death~~
#' * TODO: Create unified Hispanic indicator
#' * DONE: ~~Map cancer status variable (didn't turn out to be useful)~~
#' * DONE: ~~Create unified comorbidity variable for:~~
#'     * DONE ~~Diabetes~~
#' * DONE: ~~Mappings for other numcode variables~~
#' * TODO: Follow up re additional patient linkages, more recent NAACCR data
#' * TODO: In next re-run of query...
#'     * Include 3170 Date of Most Definitive Surgical Resection of the Primary 
#'       Site aka 'RX Date--Most Defin Surg'
#'     * Miperamine, other anti-depressants
#' * DONE: ~~Re-run query with additional variables (_query completed_):~~
#'     * ~~EMR codes for secondary tumors~~
#'     * ~~median household income, 2016 and 2013~~
#'     * ~~HbA1c~~
#'     * ~~Family history of diabetes and cancer~~
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' 
#' ### Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
