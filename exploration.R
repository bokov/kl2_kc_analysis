#' ---
#' title: "Kidney Cancer Data Exploration"
#' author: "Alex F. Bokov"
#' date: "08/09/2018"
#' ---
#' 
#+ echo=FALSE, inlcude=FALSE, message=FALSE
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
dat2[,c(v(c_analytic,retcol = 'varname'),'n_cstatus'
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
#' * *Events*
#'     * Initial diagnosis (the `c_kcdiag` group of columns in `dct0`)
#'         * NAACCR: 
#'         * EMR: First occurence of any ICD9/10 code for kidney cancer
#'     * Surgery (the `c_nephx` group of columns)
#'         * NAACCR:
#'         * EMR: First occurrence of any ICD9/10 code for acquired absence of 
#'           kidney; first occurence of surgical history of nephrectomy; first
#'     * Re-ocurrence
#'     * Death
#' * *Whether or not the patient is Hispanic*. A similar process needs to be
#'   done for Hispanic ethnicity, but not as an ordinary static variable rather 
#'   than time-to-event. I think I'll do two variables: one that is true if
#'   we are very sure the patient is Hispanic, and the other one that is true if
#'   we aren't certain the patient is _not_ Hispanic. In both cases, there will
#'   also be an `Unknown` bins for where all variables are unanimous on the 
#'   patient's Hispanic status being unknown. Basically two variables because
#'   there are the two ends of the spectrum for resolving disagreement about a
#'   binary variable between multiple sources.
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
#' Our standard way of indexing time in this study is `age_at_visit_days`. 
#' `dat1` will be collapsed into one row per patient, and the value for each of
#' the above columns will be replaced with the age in days when that event 
#' was recorded (if any, otherwise `NA`). We will then obtain a diagonal
#' matrix of median differences between each pair of variables. Not only the 
#' ones believed to reflect the same event, but all of them. This is so that we 
#' can do an overall sanity check on the relationships between  groups of 
#' variables. For example, if the supposed dates of surgery are in good 
#' agreement with each other, but they often happen after the supposed date of 
#' reoccurence, then that would be a problem we need to resolve before 
#' proceeding further. Closer visualization of individual groups of variables 
#' can be accomplished by subsetting from this master table.
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
subset(dat2[,c('patient_num',v(c_tnm))],patient_num %in% kcpatients.naaccr) %>% 
  na.omit() %>% 
  setNames(c('patient_num'
             ,submulti(v(c_tnm)
                       ,cbind(v(c_tnm),v(c_tnm,retcol = 'colname_long'))))) %>% 
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
#'     * TODO: Apply the `tte()` function to all variable in `c_tte`
#'     * TODO: Create a special TTE variable from the main i2b2 age at death
#'     * TODO: Matrices of pairwise differences between all TTE variables
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
#' * DONE: ~~Re-run query with additional variables (_query completed_):~~
#'     * ~~EMR codes for secondary tumors~~
#'     * ~~median household income, 2016 and 2013~~
#'     * ~~HbA1c~~
#'     * ~~Family history of diabetes and cancer~~
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' 
#' ### Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
