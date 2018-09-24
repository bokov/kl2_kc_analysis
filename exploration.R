#' ---
#' title: "Kidney Cancer Data Exploration"
#' subtitle: "KL2 Aim 2"
#' author: 
#' - "Alex F. Bokov^[UT Health San Antonio]"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' tags: [data characterization, preliminary, NAACCR, urology, cancer]
#' abstract: |
#'   Minimal necessary NAACCR variables chosen and 
#'   process documented for preparing them for analysis, as well as 
#'   supplementing some of them with additional data from EMR if available.
#' css: production.css
#' fig_caption: yes
#' linkReferences: true
#' nameInLink: true
#' tblLabels: "roman"
#' tblPrefix: ["table","tables"]
#' output:
#'  html_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#'  word_document:
#'   reference_docx: 'nt_styletemplate.docx'
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#'  pdf_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#' ---
#' 
#+ init, echo=FALSE, include=FALSE, message=FALSE
# init -------------------------------------------------------------------------
# if running in test-mode, uncomment the line below
options(gitstamp_prod=F);
.junk<-capture.output(source('global.R',echo=F));
.depends <- c('dictionary.R','data.R');
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- knitr::current_input();
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
.loadedobjects <- c();
for(ii in seq_along(.depends)) {
  if(!file.exists(.depdata[ii])) system(sprintf('R -e "source(\'%s\')"'
                                                ,.depends[ii]));
  .loadedobjects <- union(.loadedobjects,tload(.depdata[ii]));
}
knitr::opts_chunk$set(echo = F,warning = F,message=F,fig.scap=NA,fig.lp='');
# if a text string named FOO is created prior to a named chunk also named FOO
# then specifying opts.label='fig_opts' in the options for that chunk will use
# that string as the caption
knitr::opts_template$set(
  fig_opts=alist(fig.cap=get0(knitr::opts_current$get("label"))
                 ,results='asis'));
# Set default arguments for some functions
panderOptions('table.split.table',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','left');
.args_default_v <- formals(v);
# default arguments for getting lists of column names
formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));
# defaults for 'fancy span' string transformation of variable names 
# IN THE MAIN SECTION ONLY!! The retfun should be return for inline use and cat
# for use generating asis chunks.
.args_default_fs <- formals(fs);
formals(fs)[c('url','fs_reg','retfun')] <- alist(str,'fs_reg',return);
formals(fs)$template <- fstmplts$link_colnamelong;

# We don't yet explicitly reference patient_num outside the news block, so I'm 
# priming the fs_reg option with it here manually
options(fs_reg='patient_num');
# note_toc ---------------------------------------------------------------------
#' ###### TOC
#+ news_toc,results='asis'
.news <- c("
**Note:** This is not (yet) a manuscript. We are still at the data cleaning/alignment
stage and it is far too early to draw conclusions. Rather, this is a
regularly updated progress report that I am sharing with you to keep you in
the loop on my work and/or because you are also working on NAACCR, i2b2, Epic,
or Sunrise because I value your perspective and perhaps my results might be 
useful to your own work.\\
\\
So far, only de-identified data has been used to generate these results any 
dates or [`patient_num`](#patient%5Fnum) values you see here are also de-identified (with size
of time intervals preserved).\\
\\
This portion of the study is under Dr. Michalek's exempt project IRB number 
HSC20170563N. If you are a UT Health researcher who would like a copy of the 
data, please email me and I will get back to you with further instructions and 
any additional information I might need from you for our records.\\
\\
Dr. Murphy, if you are interested in a copy of the data, I will talk to my
local mentors and IRB about the best way to do that. It's probably time we start 
talking about what approvals in general will be necessary for the full project. 
I am doing these parts of Aim 2 ahead of Aim 1 to help me identify the need for 
additional data-transformations to incorporate into DataFinisher and will switch 
to the i2b2 plugin (Aim 1) once I hit a natural pausing-point on Aim 2."
);

.toc <- rep_len(NA,length(.news));
.toc[1] <- "
* [Consistency-Checks](#consistency-checks)
* [Cohort Characterization](#cohort-characterization)
* [Testing/Interpreting Variables](#which-emr-and-naaccr-variables-are-reliable-event-indicators)
* [Descriptive Plots (Preliminary)](#descriptive-plots-preliminary)
* Appendices
____1. [Example of stage/grade data](#appendix-i-example-of-stagegrade-data)
____2. [Next steps](#appendix-ii-next-steps)
____3. [Supplementary tables](#appendix-iii-supplementary-tables)
____4. [Variable descriptions](#appendix-iv-variable-descriptions)
____5. [Audit trail](#appendix-v-audit-trail)
";
.temp0 <- cbind(.news,.toc) %>% unname;
pander(.temp0,style='grid',keep.line.breaks=T,justify='left'
               ,split.cells=c(30,Inf),missing='')[1] %>% 
  gsub('_',' ',.) %>% cat;
# overview ---------------------------------------------------------------------
#' 
#' ## Overview
#' 
#' 
# questions, domain experts ----------------------------------------------------
#' ### Questions for mentors and other domain experts:
#' 
#' * Question: What are the main problems with the NAACCR stage and grade 
#'   information that I will need to clean up?
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
# questions, empirical ---------------------------------------------------------
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
#'         1. [Diagnosis](#initial-diagnosis) = `r fs('n_ddiag')`
#'         , no others)
#'         2. [Surgery](#surgery-conclusion) = `r fs('n_dsurg')`
#'            surgery, no others so far but may incorporate information from 
#'            additional variables after next data update)
#'         3. Recurrence and prior occurrence = `r fs('n_drecur')`
#'         4. Death = `r fs('a_tdeath')`
#' * Question: Which records to exclude due to likely errors in the source data? 
#'   E.g. surgery precedes diagnosis, recurrence precedes surgery (for some 
#'   analysis) death precedes diagnosis or surgery
#'       * Answer: Currently excluding as incomplete any record lacking either 
#'         an `r fs('n_ddiag')` event or both of `r fs('n_kcancer')` and 
#'         `r fs('n_seer_kcancer')` events. May soon start excluding the few 
#'         patients with V/Z or surgical history codes indicating missing kidney 
#'         prior to first NAACCR diagnosis.
# A list of valid patients can be found in the 'kcpatients.naaccr'
# crosschecks ------------------------------------------------------------------
#' ## Consistency-Checks
#' 

#' This project uses data in an i2b2 data warehouse that draws from the EMRs of
#' two health systems (UT Health and its main hospital teaching partner) as well
#' as the NAACCR class-A reports that UT Health has submitted to the Texas
#' Cancer Registry. All three data sources have already been linked to each
#' other and then stripped of identifying information by the Clinical
#' Informatics Research Division. Since this is the first at our site that makes
#' use of combined EMRs systems and NAACCR it is important to confirm that the
#' patient records have been linked correctly.
#'
#' The following data elements exist in both NAACCR and the EMR: date of birth
#' (`r fs('n_dob')` and `r fs('birth_date')`), marital status 
#' (`r fs('n_marital')` and `r fs('e_marital')`), sex (`r fs('sex_cd')` and 
#' `r fs('n_sex')`), race (`r fs('a_n_race')` and `r fs('race_cd')`), and
#' Hispanic ethnicity (`r fs('n_hisp')` and `r fs('e_hisp')`)

#' 
#' ### How well do marital statuses match between NAACCR and the EMR?
#' 
#' Columns represent NAACCR, rows represent EMR. Whole dataset, not filtered for
#' record completeness. Counts in bold are ones that agree between the two 
#' sources.
#+ marital_status
with(dat2a,table(e_marital,n_marital,useNA='if')) %>% addmargins %>%
  set_rownames(.,gsub('@','',rownames(.))) %>% 
  pander(emphasize.strong.cells=cbind(c(2:4,6:9),1:8)
         ,caption='[Table 1]{#tab01 .table_title} This is a test table caption');

#' 
#' ### How well do birthdates match between NAACCR and the EMR?
#' 
#' There are `r nrow(subset(dat3,is.na(n_dob)))` patients with
#' complete NAACCR records by current criteria but no NAACCR birthdate 
#' `r fs('n_dob')`. Interestingly there are a few `r fs('n_dob')` birthdates for 
#' patients who do _not_ have an `r fs('n_ddiag')` (by informal inspection). 
#' There were a total of `r length(kcpatients.bad_dob)` patients with a mismatch 
#' between  their NAACCR and EMR birthdates, and __of the patients with complete 
#' records by current criteria, `r length(kcpatients.naaccr_bad_dob)` have a mismatch 
#' between their NAACCR and EMR birthdates__ . Below is a summary of the 
#' distribution of their `r fs('birth_date')` variable minus `r fs('n_dob')`:
dat0[!is.na(dat0[[cstatic_n_dob]]) & 
       dat0$patient_num%in%kcpatients.naaccr_bad_dob
     ,c('birth_date',cstatic_n_dob)] %>% 
  apply(2,as.Date) %>% apply(1,diff) %>% `/`(365.25) %>% summary %>% pander();
#' 
#' The `r length(kcpatients.naaccr_bad_dob)` patients with otherwise complete 
#' records but mismatched birth dates vary by huge amounts from the EMR versions
#' of their respective birth dates. However, as can be seen in [supplementary tables at the end of this document](#how-well-do-demographic-variables-match-up-for-just-the-patients-with) 
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
with(dat2a,table(sex_cd,n_sex,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=cbind(1:2,1:2));

#' ### How well does race match up between the EMRs and NAACCR?
#' 
#' Columns represent NAACCR, rows represent EMR. Whole dataset, not filtered for
#' record completeness. Bolded values are those which agree between sources.
with(dat2a,table(race_cd,a_n_race,useNA = 'ifany')) %>% addmargins() %>% 
  pander(emphasize.strong.cells=cbind(1:6,1:6));

#' ### How well does Hispanic ethnicity match up between the EMRs and NAACCR?
#' 
#' This time columns represent EMR and rows represent NAACCR. Whole dataset, not 
#' filtered for record completeness.
with(dat2a,table(recode_factor(n_hisp,'Non_Hispanic'='Non_Hispanic'
                              ,'Unknown'='Non_Hispanic'
                              ,.default='Hispanic')
                ,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>% 
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=cbind(1:2,1:2));
#' More detailed ethnicity breakdown...
#' 
#' Again columns represent EMR and rows represent NAACCR. Whole dataset, not 
#' filtered for record completeness.
with(dat2a,table(n_hisp,ifelse(e_hisp,'Hispanic','Non_Hispanic'),useNA='if')) %>%
  `[`(,c('Non_Hispanic','Hispanic')) %>%
  addmargins() %>% pander(emphasize.strong.cells=cbind(1:7,c(1,2,2,2,2,2,2)));

# tableone ---------------------------------------------------------------------
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
#+ TableOne, cache=FALSE
dat2a[,unique(c('patient_num',v(c_analytic),'n_cstatus','e_death'
        ,'a_n_race','a_n_dm','a_e_dm','a_e_kc','n_kcancer','a_n_recur'
        ,'a_hsp_naaccr'))] %>% 
  mutate(
    a_n_recur=ifelse(!patient_num %in% kcpatients.naaccr | a_n_recur==''
                     ,'NONE',as.character(a_n_recur)) %>% 
      # changing the order of the levels so the NONE ends up on the right side
      factor(.,levels=c(setdiff(unique(.),'NONE'),'NONE')) %>% 
      recode(NONE='Not in NAACCR')
    # n_cstatus=ifelse(!patient_num%in%kcpatients.naaccr
    #                  ,'No KC in NAACCR',as.character(n_cstatus)) %>%
    #   factor(levels=c(levels(n_cstatus),'No KC in NAACCR')),
    ,n_vtstat=n_vtstat<=age_at_visit_days
    ,s_death=s_death<=age_at_visit_days
    ,e_death=e_death<=age_at_visit_days
    ,a_tdeath=a_tdeath<=age_at_visit_days
    ,a_tdiag=a_tdiag<=age_at_visit_days
    ,a_trecur=a_trecur<=age_at_visit_days
    ,a_tsurg=a_tsurg<=age_at_visit_days
    ,age_at_visit_days=age_at_visit_days/365.25
    #,n_kcancer=n_kcancer>=0
    ) %>% assign('.t1input',.,envir=.GlobalEnv) %>%
  rename(`Age at Last Contact, combined`=age_at_visit_days
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
         ,`Kidney Cancer, Registry`=n_kcancer
         ,`Kidney Cancer, i2b2`=a_e_kc
         ,BMI=e_bmi) %>% select(-patient_num) %>%
  select(sort(names(.))) %>% 
  CreateTableOne(vars = setdiff(names(.),'a_n_recur'),strata='a_n_recur'
                 ,data = .,includeNA = T,test = F) %>% 
  print(printToggle=F) %>% 
  set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
                    ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
  set_rownames(gsub('[ ]?=[ ]?|[ ]?TRUE[ ]?',' ',rownames(.))) %>%
  gsub('0[ ]?\\([ ]+0\\.0\\)','0',.) %>% 
  pander(emphasize.rownames=F);
# With above just a matter of finding a place to put the code below and then
# cleaning it up a little (including restricting it only to predictor vars that
# come from NAACCR)
# .t1input %>% CreateTableOne(vars=setdiff(names(.)
#                                          ,c('a_hsp_naaccr','patient_num'))
#                             ,strata='a_hsp_naaccr',data=.,includeNA = T ) %>%
#   print(printToggle=F,missing=T) %>% `[`(-5) %>% 
#   set_rownames(gsub('^([A-Za-z].*)$','**\\1**'
#                     ,gsub('   ','&nbsp;&nbsp;',rownames(.)))) %>%
#   set_rownames(gsub('[ ]?=[ ]?|[ ]?TRUE[ ]?',' ',rownames(.))) %>%
#   gsub('0[ ]?\\([ ]+0\\.0\\)','0',.) %>% 
#   pander(emphasize.rownames=F);

# event indicators -------------------------------------------------------------
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
#' Our standard way of indexing time in this study is `r fs('age_at_visit_days')`. 
#' The main table `dat1` will be collapsed into one row per patient, and the 
#' value for each of the above columns will be replaced with the age in days 
#' when that event was recorded (if any, otherwise `NA`). This table will be 
#' called `dat3`. 
#' 
# diagnosis ====================================================================
#' ### Initial diagnosis 
#' 
#' The `c_kcdiag` group of columns in `dct0`.
#' 
#' * NAACCR: `r fs('n_ddiag')`. The other two-- the date accompanying the SEER 
#'   site and the date accompanying the NAACCR primary site-- are not date 
#'   fields in NAACCR, so whatever `r fs('start_date')` they are getting 
#'   assigned must be from our ETL process, not NAACCR and that is the code I 
#'   will need to review. There is data element 443, [Date Conclusive
#'   DX](http://datadictionary.naaccr.org/default.aspx?c=10#443) but that is never
#'   recorded in our NAACCR. All other NAACCR data elements containing the word
#'   'date' seem to be retired or related to later events, not initial diagnosis.
#'   Whatever the case, there are only `r nrow(subset(dat3,is.na(n_ddiag)))`
#'   patients with a missing date of diagnosis but non-missing dates for the 
#'   SEER site variable, so within the range of reasonable error at the NAACCR 
#'   end. __Therefore `r fs('n_ddiag')` is the only NAACCR variable on which we 
#'   can rely for onset.__
#' * EMR: First occurence of any ICD9/10 code for kidney cancer. Naively, I had
#'   hoped that the first ICD9/10 code for kidney cancer would closely track the
#'   date for the `r fs('n_ddiag')`. Unfortunately, as can be seen from the 
#'   below table, for the `r sum(!is.na(dat3$n_ddiag))` patients who have 
#'   non-missing `r fs('n_ddiag')` values, the first ICD9 and first ICD10 code most 
#'   often occurs after initial diagnosis, sometimes before the date of 
#'   diagnosis, and coinciding with the date of diagnosis rarest of all. By 
#'   inspection I found that several of the ICD9/10 first observed dates lead or 
#'   trail the `r fs('n_ddiag')` by multiple years! **Therefore, one or both of 
#'   the following steps are needed before EMR data can be relied on at all for 
#'   establishing date of onset** :
#'     * Meeting with CTRC NAACCR registrar to see how she obtains her dates of 
#'       onset
#'     * Chart review of a sample of NAACCR patients to understand what 
#'       information visible in Epic sets them apart from non kidney cancer 
#'       patients.
#'     * Chart review of the 60-100 patients with ICD9/10 codes for kidney 
#'       cancer that seemingly pre-date their `r fs('n_ddiag')`.
#+ dat3_icdtimes,cache=TRUE
# select the diagnosis-related variables
dat3[,v(c_kcdiag)] %>% 
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
#+ diag_plot_cp
.diag_plot <- paste0('Here is a plot centered on ',fs('n_ddiag')
,"(blue horizontal line at 0) with black lines indicating ICD10 codes for 
primary kidney cancer from the EMR and dashed red lines indicating ICD9 codes. 
The dashed horizontal blue lines indicate +- 3 months from ",fs('n_ddiag'),'.');
#+ .diag_plot, opts.label='fig_opts'
par(xaxt='n');
#.eplot_diag <- mutate(dat3,icd=pmin(e_kc_i10,e_kc_i9,na.rm=T)) %>%
.ev_diag_plot <- event_plot(dat3,'e_kc_i10',tunit='months',type='s'
                          ,ylab='Months since NAACCR Date of Diagnosis'
                          ,xlab='Patients, sorted by time to first ICD10 code\n\n\n'
                          ,main='Time from Diagnosis to First ICD9/10 Code');
abline(h=c(-3,0,3),lty=c(2,1,2),col='blue');
points(.ev_diag_plot$e_kc_i9,col='#ff000050',pch='-');
cat('{#fig:diag_plot}');
#+ .diag_plot_summ
.eplot_diag_summ <- summary(with(.ev_diag_plot
                                 ,cut(pmin(e_kc_i10,e_kc_i9,na.rm=T)
                                      ,c(-Inf,-3,-.001,.001,3,Inf))));
#' 
#' From this we can conclude that for most patients 
#' (`r sum(.eplot_diag_summ[2:4])`), the first EMR code is recorded 
#' within 3 months of first diagnosis as recorded by NAACCR. Of those with a 
#' larger time difference, the majority (`r .eplot_diag_summ[5]`) have their 
#' first EMR code occur _after_ first NAACCR diagnosis. Only 
#' `r .eplot_diag_summ[1]` patients have ICD9/10 diagnoses that precede their
#' NAACCR diagnoses by more than 3 months. And additional `r .eplot_diag_summ[2]`
#' patients have first EMR diagnoses that precede NAACCR diagnosis by less than
#' three months. These might need to be eliminated from the sample on the 
#' grounds of not being first occurrences of kidney cancer. However, we cannot 
#' back-fill missing NAACCR records or NAACCR records lacking a diagnosis date 
#' because there is too frequently a difference between the the two sources, and 
#' the EMR records are currently biased toward later dates.
#' 
# surgery ======================================================================
#' ### Surgery
#' 
# The `c_nephx` group of columns
#' 
#' * NAACCR: 
#'      * In addition to `r fs('n_dsurg')`) the following possibly relevant 
#'        fields are available in our local NAACCR:
#'          * `r fs('n_rx1260')`
#'          * `r fs('n_rx1270')`
#'          * `r fs('n_rx3170')`
#'      * Here are the questions raised:
#'          * Do they agree with `r fs('n_dsurg')` sufficiently that missing 
#'            `r fs('n_dsurg')` can be backfilled from some or all of them?
#'          * Under what circumstances can they be interpreted as surgery dates 
#'            rather dates for something else?
#'          * How accurate is `r fs('n_surgreason')` in distinguishing surgical 
#'            cases from non-surgical cases as per EMR records?
#' * EMR: First occurrence of any ICD9/10 code for acquired absence of 
#'   kidney; or first occurence of surgical history of nephrectomy. How much do 
#'   they agree with NAACCR?
#+ dat3_surg, cache=TRUE
# make each date of surgery proxy relative to date of diagnosis
dat3_surg <- (dat3[,c(v(c_nephx),'n_drecur')] - dat3$n_ddiag) %>% 
  # keep only the ones that have a date of diagnosis and sort by NAACCR 
  # surgery date, then convert to weeks.
  subset(!is.na(dat3$n_ddiag)) %>% arrange(n_dsurg) %>% '/'(7);
# make a summary table for the 'c_nephx' candidate surgery proxy variables
dat3_surg_summary <- summary(dat3_surg) %>% 
  # extract the rownames from the arcane way that summary() returns them
  # getting rid of extra whitespace and then strip them out from the values
  set_rownames(.,trimws(gsub(':.*$','',`[`(.,,1)))) %>% gsub('^.*:','',.) %>% 
  # convert to numeric transposing as a (good) side-effect and set the rownames
  apply(1,as.numeric) %>% set_rownames(colnames(dat3_surg)) %>% 
  # convert to data.frame without 'fixing' the column names.
  data.frame(check.names = F);
# list of variables that can first occur before 'n_ddiag'
#t_priorcond <- paste(rownames(subset(dat3_surg_summary,Min.<0)),collapse=', ');
t_priorcond <- subset(dat3_surg_summary,Min.<0) %>% rownames %>% 
  sapply(fs,template=fstmplts$link_varname,retfun=return) %>% 
  knitr::combine_words();
#' As can be seen in the table below, the variables `r t_priorcond` _sometimes_ 
#' precede `r fs('n_ddiag')` by many weeks. However, they _usually_ follow 
#' `r fs('n_ddiag')` by more weeks than the two NAACCR variables 
#' `r fs('n_dsdisc')` and `r fs('n_dsurg')`. Those two NAACCR variables never occur 
#' before `r fs('n_ddiag')` and usually occur within 2-8 weeks after it.
#' 
#' As can be seen from the `NA's` column, the inactive ICD9/10 V/Z codes for
#' acquired absence of kidney are disqualified because they are very rare in 
#' addition to being even more divergent from the `r fs('n_ddiag')` than the 
#' non-inactive codes.
pander(dat3_surg_summary);
#' It's understandable if the Epic EMR lags behind NAACCR (because as an 
#' outpatient system, it's probably recording just the visits after the original
#' surgery, and perhaps we are not yet importing the actual surgery events from 
#' Sunrise EMR). But for the V or Z or surgical history codes that precede 
#' `r fs('n_ddiag')`, it could mean that those NAACCR cases are not first-time 
#' occurrences. How big of a problem is this?
#+ before_sameday_after_00,cache=TRUE
mutate_all(dat3[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-dat3$n_ddiag,breaks=c(-Inf,-.00001,.00001,Inf)
                             ,lab=c('before','same-day','after'),include=T)) %>%
  sapply(table,useNA='always') %>% t %>% pander();
#' Not too bad. Though we cannot trust the ICD9/10 codes as replacements for
#' missing surgery dates, there are few enough of them preceding diagnosis that
#' we can remove them as source data errors without ruining the sample size.
#' 
# 
# Here is a more general table, comparing every possible recurrence event or 
# surgery event to the NAACCR surgery variable, to clean up and uncomment later
# mutate_all(dat3[,c(v(c_nephx),v(c_recur))]
#            # break each column 
#            ,function(xx) cut(xx-dat3$n_dsurg,breaks=c(-Inf,-.00001,.00001,Inf)
#                              ,lab=c('before','same-day','after'),include=T)) %>% 
#   cbind(.,TOTAL=apply(.,1,function(xx) factor(
#     ifelse(any(xx=='before',na.rm=T),'before'
#            ,ifelse(any(xx=='same-day',na.rm=T),'same-day'
#                    ,ifelse(any(xx=='after',na.rm=T),'after',NA)))
#     ,levels=c('before','same-day','after')))) %>% 
#   sapply(table,useNA='always') %>% t %>% pander();
#' 
# Now, as far as the two NAACCR variables go, does `n_dsdisc` (date of 
# discharge) contribute anything more than `r fs('n_dsurg')`? There are 
# `r nrow(subset(dat3,is.na(n_dsurg)&!is.na(n_dsdisc)))` non-missing values 
# of `n_dsdisc` when `r fs('n_dsurg')` is missing. As can be seen from the plot below
# where `n_dsdisc` are the red dashed lines and `r fs('n_dsurg')` are the black lines,
# both relative to date of diagnosis, `n_dsdisc` either coincides with `r fs('n_dsurg')`
# or lags by multiple weeks, as might be expected of a discharge date (what is 
# the plausible threshold on time from surgery to discharge?).
# 
#' Below is a plot of all patients sorted by `r fs('n_dsurg')` (black line). 
#' On the same axis is `r fs('n_rx3170')` (red line) which is almost  identical 
#' to `r fs('n_dsurg')` except for a small number of cases where it occurs later 
#' than `r fs('n_dsurg')`. Never earlier. The purple lines indicate for each
#' patient the earliest EMR code implying that a surgery had taken place 
#' (acquired absence of kidney ICD V/Z codes or surgical history of nephrectomy).
#+ plot_dat3_surg,cache=TRUE
par(xaxt='n');
.eplot_surg0 <- subset(dat3,patient_num %in% 
                        kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
.eplot_surg0$icd <- apply(.eplot_surg0[,v(c_nephx,dat3)[1:5]],1,min,na.rm=T);
.eplot_surg0$icd[is.infinite(.eplot_surg0$icd)]<-NA;
lines(.eplot_surg0$icd,type='s',col='#FF00FF50');
with(.eplot_surg0,abline(v=which(icd<n_dsurg),col='#FFFF0030',lwd=4));
#' In the above plot the `r sum(with(.eplot_surg0,icd<n_dsurg),na.rm=T)` 
#' patients for which one or more EMR codes are recorded prior to 
#' `r fs('n_dsurg')` are highlighted in yellow.
#' 
#' In the below plot the `r fs('n_rx1270')` (green) and `r fs('n_rx1260')` 
#' (cyan) events are superimposed over time until `r fs('n_dsurg')` from above 
#' (but EMR codes for nephrectomy are omitted on this one). The 
#' `r fs('n_rx1270')` and `r fs('n_rx1260')` variables trend toward occurring 
#' earlier than `r fs('n_dsurg')`.
par(xaxt='n');
.eplot_surg0 <- subset(dat3,patient_num %in% 
                         kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
lines(.eplot_surg0$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg0$n_rx1260,col='#00FFFF60',type='s');
#' Furthermore, it can be seen from an equivalent plot but for patients who do 
#' _not_ have a `r fs('n_surgreason')` code equal to `Surgery Performed` there 
#' are many `r fs('n_rx1270')` and `r fs('n_rx1260')` events, but only a small 
#' number of `r fs('n_dsurg')` (black) and `r fs('n_rx3170')` (red). The 
#' `r fs('n_dsurg')` and `r fs('n_rx3170')` that do occur track each other 
#' perfectly. Together with NAACCR data dictionary's description this suggests 
#' that `r fs('n_rx3170')` is the legitimate principal surgery date in close 
#' agreement with `r fs('n_dsurg')`, so perhaps missing `r fs('n_rx3170')` 
#' values can be filled in from `r fs('n_dsurg')`. However `r fs('n_rx1270')` 
#' and `r fs('n_rx1260')` seem like non-primary surgeries or other events 
par(xaxt='n');
.eplot_surg1 <- subset(dat3,!patient_num %in% 
                         kcpatients_surgreason$`Surgery Performed`) %>% 
  mutate(nrx=pmin(n_rx3170,n_rx1270,n_rx1260)) %>% 
  event_plot('n_dsurg','n_rx3170',tunit='months',type='s',ltys = c(1,1)
             ,main='Time from Diagnosis to Surgery'
             ,ylab='Months Since Diagnosis'
             ,xlab='Patients, sorted by time to surgery'
             ,xlim=c(0,length(kcpatients_surgreason$`Surgery Performed`))
             ,ylim=c(-10,60));
abline(h=0,col='blue');
.eplot_surg1$icd <- apply(.eplot_surg1[,v(c_nephx,dat3)[1:5]],1,min,na.rm=T);
.eplot_surg1$icd[is.infinite(.eplot_surg1$icd)]<-NA;
lines(.eplot_surg1$icd,type='s',col='#FF00FF50');
with(.eplot_surg1,abline(v=which(icd<n_dsurg),col='#FFFF0030',lwd=4));
lines(.eplot_surg1$n_rx1270,col='#00FF0060',type='s');
lines(.eplot_surg1$n_rx1260,col='#00FFFF60',type='s');
#' Here is a table of every NAACCR surgery event variable versus the 
#' `r fs('n_surgreason')` variable:
lapply(v(c_nephx,dat2a)[6:9],function(ii) 
  table(dat2a$n_surgreason,dat2a[[ii]]<=dat2a$age_at_visit_days) %>% 
    set_colnames(.,paste0(ii,' = ',colnames(.)))) %>% do.call(cbind,.) %>% pander;

#' ##### Surgery Conclusion
#' 
#' As of now the sole variables on which I can rely for date of surgery are 
#' `r fs('n_rx3170')` supplemented by `r fs('n_dsurg')`, and the small number of cases where EMR 
#' codes imply surgery prior to diagnosis will be excluded. For the purposes of
#' determining whether there is a difference in the time from diagnosis to 
#' surgery I could also create an alternative 'naive' variable that is simply
#' the earliest of all possible surgery events for each patient. For the time
#' elapsed from surgery to death or recurrence, I will use the first (`r fs('n_rx3170')`
#' and `r fs('n_dsurg')`) variable as above with the additional criterion that only 
#' cases where the `r fs('n_surgreason')` is `Surgery Performed` be included.
#' 
#' TODO: Might need to rework `t_priorcond`
#' 
# re-occurrence ================================================================
#' ### Re-occurrence
#' 
#' The current available variables are: `r fs('n_cstatus')` which corresponds to [`1770 Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1770)
#' ~~hopefully with `r fs('start_date')` set by the ETL to [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772)
#' (need to double-check that it is)~~ and `r fs('n_drecur')`, [`1860 Recurrence Date--1st `](http://datadictionary.naaccr.org/default.aspx?c=10#1860).
#' UPDATE: Our site is on NAACCR v16, not v18, and we do not have [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1772).
#' According to the v16 standard, instead the [`1750 Date of Last Contact`](http://datadictionary.naaccr.org/default.aspx?c=10#1750)
#' should be used.
#' 
#' ~~It looks like it would also be useful in the next data
#' pull to include [`1880 Recurrence Type--1st`](http://datadictionary.naaccr.org/default.aspx?c=10#1880) 
#' which our NAACCR does use.~~ Done.
#' 
#' Now we can reconcile the `r fs('n_cstatus')` and `r fs('n_rectype')` variables. We can see 
#' below that almost all `r fs('n_cstatus')` `Tumor_Free` patients also have a 
#' `Disease-free` in their `r fs('n_rectype')` column, the `Tumor` ones have a 
#' variety of values, and the `Unknown` ones are also mostly `Unknown if recurred or was ever gone`.
subset(dat2a,!patient_num %in% kcpatients.naaccr_dupe) %>% droplevels() %>%
     with(table(n_rectype,n_cstatus)) %>% pander;
#' This suggest the following rules for binning them:
#' 
#' * `r fs('n_rectype')` is `Disease-free` (disease free)
#' * `r fs('n_rectype')` is `Never disease-free` (never disease free)
#' * `r fs('n_rectype')` raw code includes 70 then assume never diease free
#' * `r fs('n_rectype')` is `Unknown if recurred or was ever gone` (unknown)
#' * Otherwise, (recurred)
#
t_recur_drecur <- with(dat2a
                       ,table(a_n_recur
                              ,`Has recurrence date`=n_drecur<=age_at_visit_days
                              ,useNA='if'));
#' Here is the condensed version after having followed the above rules. Looks 
#' like the only ones who have a `r fs('n_drecur')` (recurrence date) are the ones which
#' also have a `Recurred` status for `r fs('a_n_recur')` (with `r t_recur_drecur['Recurred','FALSE']`
#' missing an `r fs('n_drecur')`). The only exception is `r t_recur_drecur['Never disease-free','TRUE']`
#' `Never diease-free` patient that had an `r fs('n_drecur')`.
t_recur_drecur %>% set_colnames(.,paste0('Recur Date=',colnames(.))) %>% 
  pander(emphasize.strong.cells=cbind(2:5,c(1,1,2,1)));
#' This explains why  `r fs('n_drecur')` values are relatively rare in the data-- they 
#' are specific to actual recurrences which are not a majority of the cases. 
#' This is a good from the standpoint of data consistency. Now we need to see to 
#' what extent the EMR codes agree with this. In the below plot, the black line
#' represents months elapsed between surgery and the first occurence of an EMR 
#' code for secondary tumors, if any. The horizontal red line segments indicate 
#' individual NAACCR dates of recurrence, `r fs('n_drecur')`. The blue horizontal line 
#' is the date of surgery. Patients whose status (`r fs('n_rectype')`) is `Disease-free`
#' are highlighted in green, `Never disease-free` in yellow, and `Recurred` in 
#' red.
#' 
par(xaxt='n');
.eplot_recur0 <-subset(dat3,patient_num %in% 
                         setdiff(kcpatients_surgreason$`Surgery Performed`
                                 ,kcpatients.naaccr_dupe)) %>% 
  mutate(.,rec=na_if(apply((.)[,v(c_recur)[-15]],1,min,na.rm=T),Inf)) %>% 
  event_plot('rec',start_event = 'n_dsurg',type='s',ltys=c(1,1)
             ,main='Time from Surgery to Recurrence'
             ,ylab='Months Since Surgery'
             ,xlab='Patients, sorted by time to first mets according to EMR'
             ,tunit = 'month');
abline(h=c(-3,0,3),lty=c(3,1,3),col='blue');
# Highlight patients with recurrence
with(.eplot_recur0,abline(v=which(patient_num %in% kcpatients_rectype$Recurred)
                          ,col='#FF000020',lwd=2));
# ...never disease-free
with(.eplot_recur0,abline(v=which(patient_num %in% 
                                    kcpatients_rectype$`Never disease-free`)
                          ,col='#FFFF0060',lwd=2));
# ... and disease-free
with(.eplot_recur0,abline(v=which(patient_num %in% 
                                    kcpatients_rectype$`Disease-free`)
                          ,col='#00FF0020',lwd=2));
points(.eplot_recur0$n_drecur,col='red',pch='-',cex=2);
#' The green highlights are _mostly_ where one would expect, but why are there
#' `r nrow(subset(.eplot_recur0,patient_num %in% kcpatients_rectype[['Disease-free']] & !is.na(rec)))`
#' patients on the left side of the plot that have EMR codes for secondary 
#' tumors? Also, there are `r nrow(subset(.eplot_recur0,rec<0))` patients with 
#' metastatic tumor codes earlier than `r fs('n_dsurg')` and of those 
#' `r nrow(subset(.eplot_recur0,rec< -3))` occur more than 3 months prior to 
#' `r fs('n_dsurg')`. Did they present with secondary tumors to begin with but remained 
#' disease free after surgery? Removing the `_inactive` versions of the 
#' secondary tumor codes does not make the left-side green patients go away.
#' 
#' 
# Not ready for production code to plot ranked events...
# baz<- subset(dat3,!patient_num %in% kcpatients.naaccr_dupe)[,c('patient_num',v(c_kcdiag,dat3),v(c_nephx,dat3),v(c_recur,dat3),v(c_death,dat3),'n_lc')]
# baz[,-1] <- t(apply( baz[,-1],1,rank,na.last='keep'))
# baz<-arrange(baz,n_ddiag)
# plot(baz$n_ddiag,type='l',ylim=c(0,21))
# lines(baz$n_fc,col='red')
# lines(baz$n_lc,col='black')
# lines(baz$n_dsurg,col='green',lty=2)
# lines(baz$n_dsurg,col='green',lwd=2)
# lines(baz$n_drecur,col='blue',lwd=2)
# for(ii in v(c_death,baz)) lines(baz[[ii]],col='orange',lwd=2)
# for(ii in v(c_kcdiag,baz)[1:2]) lines(baz[[ii]],lty=3,col='pink')
# for(ii in v(c_recur,baz)[-15]) lines(baz[[ii]],col='cyan')
# for(ii in v(c_death,baz)) lines(baz[[ii]],col='orange',lwd=2)
#'  
# death ========================================================================
#' ### Death
#' 
#' Below are plotted times of death (for patients that have them) relative to 
#' date of diagnosis `r fs('n_ddiag')` (horizontal blue line). The four data sources are
#' `r fs('e_death')` the EMR death date (![](resources/pinktriangle.png){width=10})
#' , `r fs('s_death')` the social security death date (![](resources/blueinvtriangle.png){width=10})
#' , `r fs('e_dscdeath')` the EMR hospital discharge death date (![](resources/greencross.png){width=10})
#' , and `n_vtstat` the NAACCR death date (![](resources/browncircle.png){width=10}).
#' 
#' When more than one source has a death date, they are in agreement. To be 
#' fair, the agreement between `r fs('e_death')`, `r fs('e_dscdeath')`, and `r fs('s_death')` is 
#' probably due to our i2b2 ETL already merging `r fs('e_dscdeath')` and `r fs('s_death')` into 
#' `r fs('e_death')`. But it is also encouraging that none of them seem (by visual 
#' inspection) to occur prior to the date of last contact in NAACCR. That 
#' suggests I can simply take the mininum of available death dates to fill in 
#' data for patients that NAACCR is not aware are deceased. It also means that 
#' the ETL's coverage of vital status can be further improved by using the 
#' NAACCR vital status and last contact variables in combination.
.eplot_death <- event_plot(dat3,'n_lc',start_event = 'n_ddiag'
                           ,main='Time from Diagnosis to Death (if any)'
                           ,ylab='Months Since Diagnosis'
                           ,xlab='Patients, sorted by last contact date'
                           ,tunit = 'mon',ltys = 0,type='s');
points(.eplot_death$e_death,pch=2,col='#FF00FF70',lwd=2); # \triangle
points(.eplot_death$s_death,pch=6,col='#00999970',lwd=3); # \nabla (not making this up!)
points(.eplot_death$e_dscdeath,pch=3,col='#00FF0090',lwd=2); # +
points(.eplot_death$e_disdeath,pch=3,col='#00FF0090',lwd=2);
points(.eplot_death$n_vtstat,col='brown',cex=1.5,lwd=0.5); # \bigcirc
abline(h=0,col='blue');
.xch_vtstat_lc<-subset(dat2a
                       ,n_vtstat==n_lc&n_lc!=age_at_visit_days+1)$patient_num;
.xch_vtstat_lc_death = intersect(kcpatients.naaccr_death,.xch_vtstat_lc);
if(length(.xch_vtstat_lc_death)!=length(.xch_vtstat_lc)){
  stop('.xch_vtstat_lc check failed')};
#' Here are some crosschecks on the NAACCR-only death indicator `r fs('n_vtstat')`.
#' Overall there are `r length(kcpatients.naaccr_death)` patients that according
#' to `r fs('n_vtstat')` are deceased. For all `r length(.xch_vtstat_lc)` of these
#' patients, _and only for them_, the condition also holds that `r fs('n_vtstat')` 
#' is equal to `r fs('n_lc')` but `r fs('n_lc')` happens before or on `r fs('age_at_visit_days')`.
#' If something is coded as happening _after_ `r fs('age_at_visit_days')` then 
#' because of how the data is summarized in the `dat2` section of the `data.R` 
#' script it means that the event never happened. If `r fs('n_lc')` never 
#' happened it means that patient has evidence of kidney cancer in the EMR data 
#' but no accompanying NAACCR record. In short, we are filtering for existence 
#' of NAACCR records. That, in turn, means that `r fs('n_vtstat')` `<=` `r fs('n_lc')`
#' is a valid censoring criteria (censored if false) provided that the input
#' data is filtered to include only patients with NAACCR records (for other 
#' patients, both `r fs('n_vtstat')` and `r fs('age_at_visit_days')` should be 
#' interpreted as missing).
# hispanic ethnicity ===========================================================
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
#' Here are the variables to process:
#' 
#' * `r fs('language_cd')` is an i2b2 PATIENT_DIMENSION variable that is simplified by 
#'   `data.R` and `levels_map.csv`
#'     * Hispanic : `Spanish`
#'     * non-Hispanic: `Other`
#'     * Unknown: `English` or `Unknown` or NA
#' * `r fs('e_lng')` is an i2b2 OBSERVATION_FACT variable currently in the raw form that
#'   DataFinisher uses for complex variables lacking a specific rule. Below are 
#'   regexp patterns for a non case-sensitive match.
#'     * Hispanic: ` ^.*spanish.*$` ELSE
#'     * Unknown: ` ^.*(english|sign language|unknown).*$` or NA ELSE
#'     * non-Hispanic: anything not caught by the above two filters
#' * `r fs('n_hisp')` is the [`0190 Spanish/Hispanic Origin`](http://datadictionary.naaccr.org/default.aspx?c=10#190)
#'   variable from NAACCR. Slightly processed by `data.R` and `levels_map.csv`
#'     * non-Hispanic: `Non_Hispanic`
#'     * Unknown: `Unknown`
#'     * Hispanic: any other value
#' * `r fs('e_hisp')` is the indicator variable for Hispanic ethnicity from i2b2 
#'   OBSERVATION_FACT.
#'     * Hispanic: `TRUE`
#'     * Unknown: `FALSE`
#' * `r fs('e_eth')` is the whole ethnicity variable from i2b2 OBSERVATION_FACT and
#'   suprprisingly it is not in full agreement with `r fs('e_hisp')`
#'     * Hispanic: `hispanic`
#'     * Unknown: `other`,`unknown`,`unknown/othe`,`i choose not`,`@`
#'     * non-Hispanic: `arab-amer`,`non-hispanic`
#'     
#' The strict Hispanic variable.
#' 
#' * Hispanic if ALL non-missing values of `r fs('n_hisp')`, `r fs('e_hisp')`, and `r fs('e_eth')` are 
#'   unanimous for `Hispanic`
#' * non-Hispanic if ALL non-missing values of `r fs('n_hisp')` and `r fs('e_eth')` are 
#'   unanimous for `non-Hispanic` (note that `r fs('e_hisp')` not included here) and
#'   neither `r fs('e_lng')` nor `r fs('language_cd')` vote for `Hispanic`
#' * Unknown if any other result.
#' 
#' The lenient Hispanic variable.
#' 
#' * Hispanic if ANY non-missing values of `r fs('language_cd')`, `r fs('e_lng')`, `r fs('n_hisp')`,
#'   `r fs('e_hisp')`, and `r fs('e_eth')` have value `Hispanic`
#' * Unknown if ALL non-missing values of `r fs('language_cd')`, `r fs('e_lng')`, `r fs('n_hisp')`,
#'   `r fs('e_hisp')`, and `r fs('e_eth')` are unanimous for `Unknown` 
#' * non-Hispanic if any other result
#' 
# descriptive plots ------------------------------------------------------------
#' ## Descriptive Plots (Preliminary)
#' 
#' To avoid bias/overfitting all descriptive data and visualizations below that 
#' relate the predictor variable to the outcome are done using a randomly 
#' selected subset of the records (N=`r length(intersect(pat_samples$train,kcpatients.naaccr))`).
#' The below results are still preliminary because, among other things, they 
#' have not been normalized for covariates including age and stage at diagnosis.
#+ surv_surg,fig.cap='No great short-term difference between Hispanic and non-Hispanic patients. In the longer term a greater fraction of Hispanic patients eventually undergo surgery.'
(.survfit_plot0 <- survfit_wrapper(dat2a,'a_tsurg',censrvars = c()
                                  ,startvars = 'a_tdiag'
                                  ,predvars = 'a_hsp_naaccr'
                                  ,default.censrvars = 'n_lc'
                                  # Reduce(intersect,list(...)) is how one can
                                  # do intersect with >2 vectors
                                  ,subs = a_tdiag <= n_lc & patient_num %in% 
                                    Reduce(intersect,list(kcpatients.naaccr
                                                          ,pat_samples$train))
                                  ,followup = 365.25*3,scale=7,unit='Weeks'
                                  ,main='Time from diagnosis to surgery'
                                  ,ylab='Fraction not undergone surgery'
                                  ,xlab='Weeks since diagnosis'
                                  ,plotadd = list(
                                    guides(colour=guide_legend('Hispanic')
                                           ,fill=guide_legend('Hispanic'))))
 )$plot;
# Now plot two different scenarios on the same axes, the original and enhanced 
# by EMR variables. Strict
# plot(.survfit_plot0$fit,col=c('red','blue'),mark.time = T,xlim=c(-1,150)
#      ,lwd=2,main='Time from Diagnosis to Surgery',ylab='Not Undergone Surgery'
#      ,xlab='Weeks Since Diagnosis');
# lines(update(.survfit_plot0,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_strict')$fit
#       ,col=c('#0000ff40','#00ff0040','#ff000040'),lwd=4,mark.time=T);
# lines(update(.survfit_plot0,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_broad')$fit
#       ,col=c('#ff000040','#0000ff40'),lwd=4,lty=2,mark.time=T);
#' 
#' What is the risk of relapse for patients after nephrectomy?
#' 
#+ surv_recur,cache=TRUE,fig.cap='No difference in recurrence risk observed with recurrence and surgery variables as currently prepared.'
(.survfit_plot1 <- update(.survfit_plot0,eventvars='a_trecur'
                          ,startvars='a_tsurg'
                          # turns out there needs to be a requirement that
                          # the startvars be no larger than the censrvars
                          # (their respective pmins actually, but right now 
                          # there) is just one of each.
                          ,subs = a_tsurg<=n_lc & patient_num %in% 
                            Reduce(intersect,list(kcpatients.naaccr
                                                  ,kcpatients.surg
                                                  ,pat_samples$train))
                          ,main='Time from surgery to recurrence'
                          ,ylab='Fraction recurrence-free'
                          ,xlab='Weeks since surgery'))$plot;
# plot(.survfit_plot1$fit,col=c('red','blue'),mark.time = T,xlim=c(-1,150)
#      ,lwd=2,main='Time from surgery to recurrence',ylab='Fraction recurrence-free'
#      ,xlab='Weeks since surgery',ylim=c(.7,1));
# lines(update(.survfit_plot1,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_strict')$fit
#       ,col=c('#ff000040','#00ff0040','#0000ff40'),lwd=4,mark.time=T);
# lines(update(.survfit_plot1,default.censrvars=c('a_tdeath','age_at_visit_days')
#              ,predvars='a_hsp_broad')$fit
#       ,col=c('#ff000040','#0000ff40'),lwd=4,lty=2,mark.time=T);
#' 
#' What is the mortality risk for patients after nephrectomy?
#' 
#+ surv_death,cache=TRUE,fig.cap='No strong difference in mortality risk observed with vital status and surgery variables as currently prepared.'
(.survfit_plot2 <- update(.survfit_plot1,eventvars='n_vtstat'
                          ,main='Time from surgery to death'
                          ,ylab='Fraction alive'))$plot;
#' 
#' How much difference does it make to supplement this with EMR data?
#' 
#+ surv_death_EMR,cache=TRUE,fig.cap='When additional vital status, ethnicity, and last-visit information from EMR is included, there are markedly more events but still no discernible difference.'
(.survfit_plot2a <- update(
  .survfit_plot2,eventvars='a_tdeath'
  ,predvars='a_hsp_broad'
  ,default.censrvars='age_at_visit_days'
  ,main='Time from surgery to death supplemented with EMR data'))$plot
#' ***
# A1 stage/grade ---------------------------------------------------------------
#' ## Appendix I: Example of stage/grade data
#' 
#' (proof of feasibility)
#'
subset(dat2a[,c('patient_num',v(c_tnm,NA))],patient_num %in% kcpatients.naaccr) %>% 
  na.omit() %>% 
  setNames(c('patient_num'
             ,submulti(v(c_tnm,NA)
                       ,cbind(v(c_tnm,NA),v(c_tnm,NA,retcol = 'colname_long'))))) %>% 
  # head(5) %>% 
  # show a sampling of rows and columns that fits on the page and remove the
  # the extra quotation marks
  `[`(1:15,1:8) %>% apply(2,function(xx) gsub('["]','',xx)) %>% 
  pander();
#' ***

#  A2 next steps ---------------------------------------------------------------
formals(fs)$template <- fstmplts$link_colnamelong;
formals(fs)$retfun <- as.name('return');
#' ## Appendix II: Next steps
#' 
#' * TODO: Update and clean up the plots and tables, including labels.
#'     * [Consistency-Checks]
#'         * **Marital status, sex, race, hispanic(2):** shorten text and move to 
#'           captions.
#'         * Write motivation and summary.
#'     * [Testing/Interpreting Variables](#which-emr-and-naaccr-variables-are-reliable-event-indicators)
#'         * Write motivation, intro, summary. Incorporate edits.
#'         * [Initial diagnosis], [Surgery], [Re-occurrence], [Death]
#'             * Move plots to the top of each
#'             * Shorten text and move to captions.
#'             * For each plot state what the conclusions are.
#'             * [Surgery]: turn the outline at the beginning into a more 
#'               concise paragraph.
#'     * [Hispanic variable recoding](#whether-or-not-the-patient-is-hispanic):
#'       turn into paragraphs, think about moving to variable glossary.
#'     * [Descriptive Plots (Preliminary)]
#'         * Move them to right after the overview.
#'         * Write intro mentioning that these are the relationships of interest
#'           among the four main variables.
#'         * Expand why there are two versions of the survival plot
#' * TODO: Update and streamline the narrative.
#'     * Intro
#'     * Motivation
#'     * Summary of results
#'     * Summary of next steps
#'     * Move questions to after the [Descriptive Plots (Preliminary)] but 
#'       before the [Consistency-Checks], and place the answered questions at
#'       the bottom. [Domain expert questions](#questions-for-mentors-and-other-domain-experts) 
#'       still go ahead of [empirical questions](#questions-to-answer-empirically).
#' * TODO: Remove the crossed-off stuff in [Appendix III: Supplementary tables] '
#'         but note someplace what was removed and why.
#' * TODO: Organize the inclusion/exclusion criteria into a single named list
#' * TODO: Overhaul the existing TableOne in [Cohort Characterization] -- use 
#'         data dictionary for renaming instead of _ad-hoc_ .
#' * TODO: Migrate everything that uses `dat2` and `dat3` to using `dat2a`.
#' * TODO: Create a TableOne for `r fs('a_hsp_naaccr')` (that specific one 
#'         because then the conclusions can be directly applied to TCR data) to 
#'         find possible confounding variables. Age, perhaps? Income? 
#' * TODO: Fill in more of the variable descriptions in 
#'         [Appendix IV: Variable descriptions]
#' * TODO: Prior to doing the above `tte()` put in a safeguard to make
#'         sure all the `c_tte` variables are `TRUE/FALSE` only. They
#'         are right now as it happens, but nothing enforces that.
#' * TODO: Clean up TNM variables, in consultation with domain expert (Peter?)
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
#'     * DONE: ~~Should use [`0580 Date of 1st Contact`](http://datadictionary.naaccr.org/default.aspx?c=10#580)
#'       as the diagnosis date if earlier than `r fs('n_ddiag')`!~~ _Actually, evidence 
#'       that it's neither a diagnosis date nor a first contact. Not known what
#'       it is._
#'     * DONE: ~~Surgery fields:~~
#'         * ~~[`1260 Date of Initial RX--SEER`](http://datadictionary.naaccr.org/default.aspx?c=10#1260)~~
#'         * ~~[`1270 Date of 1st Crs RX--CoC`](http://datadictionary.naaccr.org/default.aspx?c=10#1270)~~
#'         * ~~[`3170 RX Date--Most Defin Surg`](http://datadictionary.naaccr.org/default.aspx?c=10#3170)~~
#'     * DONE: ~~Recurrence: [`1880 Recurrence Type--1st`](http://datadictionary.naaccr.org/default.aspx?c=10#1880)~~
#' * TODO: In a future re-run of query...
#'     * Follow up re additional patient linkages, more recent NAACCR data
#'     * education (Census, not ready, ETL needs fixing)
#' * TODO: Separate script-level calls to `instrequire()` to reduce the number 
#'         of libraries that get loaded unnecessarily.
#' * TODO: Create a light version of `data.R.rdata` that has only the minimal 
#'         necessary stuff for, e.g. `exploration.R`
#' * DONE: ~~Create combined (if applicable) variables for each of the following:~~
#'     * ~~Initial diagnosis~~ `r fs('a_tdiag')`, `r fs('a_cdiag')`
#'     * ~~Surgery~~ `r fs('a_tsurg')`, `r fs('a_csurg')`
#'     * ~~Re-ocurrence~~ `r fs('a_trecur')`, `r fs('a_crecur')`
#'     * ~~_Last follow-up ?_~~ 
#'     * ~~Death~~ `r fs('a_tdeath')`, `r fs('a_cdeath')`
#'     * ~~Strict Hispanic designator~~ `r fs('a_hsp_strict')`
#'     * ~~Lenient Hispanic designator~~ `r fs('a_hsp_broad')`
#'     * ~~NAACCR-only Hispanic designator~~ `r fs('a_hsp_naaccr')`
#' * DONE: ~~Verify that the [ETL](http://www.hostedredmine.com/issues/719444#note-11) 
#'         gets `r fs('start_date')` for `1770 Cancer Status` from 
#'         [`1772 Date of Last Cancer Status`](http://datadictionary.naaccr.org/default.aspx?c=10#1770)~~
#'         _in NAACCR v16 it cannot/doesn't need to_
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
#' ***
#' 
# A3 supplementary tables ------------------------------------------------------
#' ## Appendix III: Supplementary tables
#' 
#' ### How well do demographic variables match up for just the patients with mismatched birthdates?
#' 
#' #### Sex
#' 
#+ dat2_bad_dob, cache=TRUE
dat2_bad_dob <- subset(dat2a,patient_num %in% kcpatients.bad_dob);

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
dat3_bad_dob <- subset(dat3,patient_num %in% kcpatients.bad_dob);
mutate_all(dat3_bad_dob[,v(c_nephx)]
           # break each column 
           ,function(xx) cut(xx-dat3_bad_dob$n_ddiag
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
#' prior to `r fs('n_ddiag')` and those will exclude far fewer records than suggested 
#' by this table_ .
#' 
#' ### What is going on with the first contact variable?
#' Wierd observation-- the date of first contact `r fs('n_fc')` (red) is 
#' almost always between last contact `r fs('n_lc')` (black) and diagnosis `r fs('n_ddiag')` 
#' (blue), though diagnosis is usually on a biopsy sample and that's why it's 
#' dated as during or after surgery we thought. If first contact is some kind of 
#' event after first diagnosis, what is it?
#+ event_plot_diag2lc,cache=TRUE
.eplot_fc <-event_plot(subset(dat3,!patient_num %in% kcpatients.naaccr_dupe)
                       ,'n_lc','n_fc',start_event = 'n_ddiag'
                       ,main='Time from Diagnosis to Last Contact'
                       ,ylab='Months Since Diagnosis'
                       ,xlab='Patients, sorted by Last Contact'
                       ,tunit = 'mon'
                       ,ltys = c(1,1));
abline(h=0,col='blue');
#' 
#' Surgery `r fs('n_dsurg')` seems to happen in significant amounts both before 
#' and after first contact `r fs('n_fc')`.
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
#' 2. Center the `r fs('age_at_visit_days')` variable on that visit, so for that 
#'    patient it is `0` on the visit, a negative integer prior to the visit, and 
#'    a positive integer after. It will be seen later that this will help make
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
dat3.gteq<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) mean(aa>bb,na.rm = T),xx,yy));
dat3.meds<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(aa-bb,.5,na.rm = T),xx,yy));
dat3.mabs<-outer(dat3[,-1],dat3[,-1],FUN = function(xx,yy)
  mapply(function(aa,bb) quantile(abs(aa-bb),.5,na.rm = T),xx,yy));
dat3.mabx<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(abs(aa-bb),na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
dat3.maxs<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-max(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));
dat3.mins<-outer(dat3[,-1],dat3[,-1],FUN=function(xx,yy)
  mapply(function(aa,bb) {
    oo<-min(aa-bb,na.rm=T);
    if(is.infinite(oo)) return(NA) else return(oo)},xx,yy));

# We need to exclude the 'n_dob' variable because it otherwise screws up the
# scaling. Also excluding all variables that have fewer than 10 non-null 
# observations
.dat3.keep <- !colnames(dat3.gteq) %in% 
  c(names(dat3)[colSums(!is.na(dat3))<10],'n_dob');
# This is to distinguish missing values from 0 values in a heatmap! No other way
# to do that!!
#layout(matrix(1,nrow=2,ncol=2));
par(bg='gray'); #,mfrow=1:2,mfcol=1:2);
heatmap(dat3.gteq[.dat3.keep,.dat3.keep],symm = T,na.rm = T,margins=c(10,10)
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
#' ***
#' 
# A4 variables -----------------------------------------------------------------
#'
#' ## Appendix IV: Variable descriptions
#' 
#+ progfootnotes, results='asis'
# set new template for creating the internal link VALUES
# formals(fs)[c('url','template','retfun')] <- alist(paste0('#',str)
#                                           ,'[%1$s]: %2$s "%4$s"\n',cat);
formals(fs)[c('url','retfun')] <- alist(paste0('#',str),cat);
formals(fs)$template <- fstmplts$linkref;
.junk <- subset(dct0,varname %in% getOption('fs_reg')
                ,select = c('varname','colname_long')) %>% 
  apply(1,function(xx) fs(xx[1],tooltip=xx[2]));
#' Here are descriptions of the variables referenced in this document.
#+ readablefootnotes, results='asis'
# set new template for creating the internal link TARGETS
cat('***\n');
.junk <- subset(dct0,varname %in% getOption('fs_reg')
                ,select = c('varname','colname_long','chartname','comment'
                            ,'col_url')) %>% 
  apply(1,function(xx) cat(
    '######',xx[1],'\n\n',na.omit(xx[2:1])[1],':\n\n  ~ '
    ,ifelse(length(na.omit(xx[2:4]))>0
            ,iconv(paste(na.omit(xx[2:4]),collapse='; '),to='UTF-8',sub=''),'')
    ,ifelse(is.na(xx[5]),'',paste('\n\n  ~ Link:',xx[5])),'\n\n***\n'));
# A5 audit ---------------------------------------------------------------------
#' ## Appendix V: Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
#+ results='hide'
c()