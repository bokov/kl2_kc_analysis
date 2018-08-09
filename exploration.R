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
knitr::opts_chunk$set(echo = FALSE,warning = FALSE);
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

#' ### Descriptive Plots (Preliminary)

#' What is the overall response range for the lag from diagnosis to surgery?
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

#' What is the overall response range recurrence-free survival after surgery?
subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_recur)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_crecur=last(a_crecur)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_crecur)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving in Remission'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1));

#' What is the overall response range for survival after surgery?
subset(dat1,patient_num %in% pat_samples$train & eval(subs_criteria$surg_death)) %>% 
  summarise(age=age_at_visit_days[a_tsurg==0]
            ,a_tsurg=last(a_tsurg),a_cdeath=last(a_cdeath)
            ,hisp=!all(na.omit(n_hisp)%in%c('Non_Hispanic','Unknown'))) %>%
  survfit(Surv(a_tsurg,a_cdeath)~hisp,.) %>% 
  autoplot(mark.time=T,xlab='Days Since Surgery',ylab='% Surviving'
           ,xlim=c(0,2000),conf.int.alpha=0.1,surv.size=2,ylim=c(.55,1));
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
#' * DONE: ~~Create censoring variable for surgery~~
#' * DONE: ~~Create censoring variable for recurrence/death~~
#' * TODO: Create unified Hispanic indicator
#' * DONE: ~~Map cancer status variable (didn't turn out to be useful)~~
#' * DONE: ~~Create unified comorbidity variable for:~~
#'     * DONE ~~Diabetes~~
#' * DONE: ~~Mappings for other numcode variables~~
#' * TODO: Follow up re additional patient linkages, more recent NAACCR data
#' * TODO: Re-run query with additional variables (_query completed_):
#'     * EMR codes for secondary tumors
#'     * median household income, 2016 and 2013
#'     * HbA1c
#'     * Family history of diabetes and cancer
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
#' 
#' ### Audit trail
walktrail()[,-5] %>% pander(split.tables=600);
