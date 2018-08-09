#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
source('global.R');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
debug <- 1;
#' Create custom synonyms for 'TRUE' if needed
l_truthy_default <- eval(formals(truthy.default)$truewords);
#l_truthy_default <- c("Yes", l_truthy_default);
l_missing <- c(NA,'Unknown','unknown','UNKNOWN');

#' Initialize the column specification for parsing the input data
dat0spec <- tread(inputdata,spec_csv,na=c('(null)',''),guess_max=5000);
#' Force the `patient_num` column to be numeric rather than integer, to avoid
#' missing values due to `patient_num` being too large
dat0spec$cols[['patient_num']] <- col_number();
#' Read the data 
dat0 <- tread(inputdata,read_csv,na=c('(null)',''),col_type=dat0spec);
colnames(dat0) <- tolower(colnames(dat0));

#' Read in the data dictionary
#dct_stage <- 0;
names(dat0)[1:8] %>% tibble(colname=.,colname_long=.,rule='demographics') %>% 
  rbind(tread(dctfile_raw,read_csv,na = '')) -> dct0;
dct0$colname <- tolower(dct0$colname);
dct0 <- subset(dct0,dct0$colname %in% names(dat0));

#' debug
if(debug>0) if(!identical(names(dat0),dct0$colname)) 
  stop('Mismatch between dct0$colname and actual colnames');
#' end debug
dct0$class <- lapply(dat0[,dct0$colname],class) %>% sapply(head,1);
dct0$colsuffix <- gsub('^v[0-9]{3}','',dct0$colname);

#' debug
if(debug>0) .dct0bak <- dct0;
#' end debug
dct0 <- left_join(dct0,tread(dctfile_tpl,read_csv,na='')
                  ,by=c('colsuffix','colname_long'));
#' debug
if(debug>0){
  if(nrow(dct0)!=nrow(.dct0bak)) 
    stop('Number of rows changed in dct0 after join');
  if(!identical(dct0$colname,.dct0bak$colname)) 
    stop('colname values changed in dct0 after join');
}
#' end debug
dct0$c_all <- TRUE;

#' Load the NAACCR manual code mappings
levels_map <- tread('levels_map.csv',read_csv,na='');

#' Create copy of original dataset
dat1 <- group_by(dat0,patient_num);
#' Bulk-transform the NA/non-NA columns to FALSE/TRUE ones
for(ii in v(c_natf)) dat1[[ii]] <- !is.na(dat1[[ii]]);

#' Rename columns that will be individually referenced later on so that they
#' always have the same name regardless of the data version
names(dat1) <- submulti(names(dat1)
                        ,searchrep=as.matrix(na.omit(dct0[,c('colname','varname')]))
                        ,method='startsends');
#' Mass relabel/reorder factor variables.
for(ii in v(c_sortlabels,retcol='varname')){
  dat1[[ii]] <- factorclean(dat1[[ii]]
                            ,spec_mapper = levels_map,var=ii,droplevels = T)};
#' Convert NAACCR codes to readable labels where available
# for(ii in intersect(names(dat1),levels_map$varname)){
#   dat1[[ii]] <- gsub('"','',dat1[[ii]]) %>% 
#     submulti(subset(levels_map,varname==ii)[,c('code','label')])};
#' Convert NAACCR race codes
dat1$a_n_race <- interaction(dat1[,v(c_naaccr_race)],drop = T) %>% 
  # clean up the non-informative-if-trailing codes
  gsub('."88"|."99"','',.) %>% factorclean(spec_mapper = levels_map
                                           ,var = '_rc',droplevels=T);
#' Unified NAACCR diabetes comorbidity
dat1$a_n_dm <- apply(dat1[,v(c_naaccr_comorb)],1,function(xx) any(grepl('"250',xx))); 
#' Find the patients which had active kidney cancer (rather than starting with 
#' pre-existing)... first pass
kcpatients.emr <- subset(dat1,e_kc_i10|e_kc_i9)$patient_num %>% unique;
kcpatients.naaccr <- subset(dat1,n_kcancer)$patient_num %>% unique;
#' create the raw time-to-event (tte) and censoring (cte) variables
#' along with making a_n_race and a_n_dm time invariant
dat1 <- mutate(dat1
               # historic diagnoses... if they occur prior to non-historic, be suspicious
               ,a_n_race=paste(unique(na.omit(a_n_race)),collapse=',')
               ,a_n_dm=any(a_n_dm)
               ,a_e_dm=e_dm_i9|e_dm_i10
               ,a_e_kc=e_kc_i9|e_kc_i10
               ,a_thdiag=tte(age_at_visit_days,e_kc_i10_i|e_kc_i9_i)
               ,a_tdiag=tte(age_at_visit_days
                            # only count n_ddiag when it's recorded as a cancer case
                            ,(patient_num %in% kcpatients.naaccr & n_ddiag)|
                              e_kc_i9|e_kc_i10)
               ,a_trecur=tte(age_at_visit_days,n_drecur)
               ,a_tsurg=tte(age_at_visit_days,n_dsurg)
               ,a_tdeath=tte(age_at_visit_days
                             # EMR death
                             ,isTRUE(age_at_death_days==age_at_visit_days)|
                               # SSN death
                               s_death|
                               # NAACCR death
                               isTRUE(n_vtstat=="Dead")
                             )
               ,a_cdiag=cte(a_tdiag)
               ,a_crecur=cte(a_trecur)
               ,a_csurg=cte(a_tsurg)
               ,a_cdeath=cte(a_tdeath)
               );
#' A hack to restore NAs to NAACCR race designation and turn some character
#' columns into factors with same order of levels as their i2b2 counterparts
dat1$a_n_race <- with(dat1,ifelse(a_n_race=='',NA,a_n_race)) %>% 
  factor(levels=levels(dat1$race_cd));
#dat1$sex_cd <- factor(dat1$sex_cd,levels=levels(dat1$n_sex));

kcpatients.pre_existing <- subset(dat1,a_thdiag>=0&a_tdiag<0)$patient_num %>% unique;

cohorts <- data.frame(patient_num=unique(dat1$patient_num)) %>% 
  mutate( NAACCR=patient_num %in% kcpatients.naaccr
         ,EMR=patient_num %in% kcpatients.emr
         ,PreExisting=patient_num %in% kcpatients.pre_existing
         ,combo=interaction(NAACCR,EMR,PreExisting)) %>% group_by(combo);

consort_table <- summarise(cohorts,NAACCR=any(NAACCR),EMR=any(EMR)
                           ,PreExisting=any(PreExisting)
                           ,N=length(patient_num))[,-1];
#' Creating training/testing/validation samples
#' 
#' As long as the seed is the same, all random values will be generated the same
#' reproducibly.
tseed(project_seed);
#' Randomly to training, testing, or validation sets
pat_samples <- split(dat1$patient_num,sample(c('train','test','val')
                                         ,size=nrow(dat1),rep=T));
#' ## Transform certain columns
# 
#' Make sex/gender a factor
# 
#' TODO: do this dynamically via new c_ group for all columns that are safe to
#' directly convert to factors
#dat1$gender <- factor(dat1$gender);

#' ## Create binned versions of certain numeric vars.
# 
#' (commented out until we can put a c_num2bin or something into dct0)
# dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
#   qii <- c(0,quantile(ii,c(.25,.5,.75),na.rm=T),Inf);
#   cut(ii,breaks = qii);
# })

#' ## Create response variables
#
#' ## Transform Rows
#
#' ## Sort the rows 
#
#' Creating an object to use as the lookup argument for `mapnames()``
#' TODO: change the NSQIP_NAMES column name to a more generic one
#dat1namelookup <- with(dct0,setNames(dataset_column_names
#                                     ,ifelse(is.na(NSQIP_NAMES)
#                                             ,dataset_column_names
#                                             ,NSQIP_NAMES)));
#' ### Make several subsets of dat1 all at once
#
#' for later use to make multiple versions of the same table and 
#' multiple versions of the same graph,
#' ### Create a version of the dataset that only has each patient's 1st encounter
#' 
dat2 <- group_by(dat1,patient_num) %>% 
  summarise_all(function(xx) if(is.logical(xx)) any(xx) else last(na.omit(xx)));

#' Each name is a legal variable name for that subset, the value
#' assigned to it is an R expression that can be evaluated in the
#' scope of `dat1` and will return a `TRUE`/`FALSE` vector
subs_criteria <- alist(
   # from diagnosis to surgery
    diag_surg = a_tdiag>=0 & a_tsurg<=0 & patient_num %in% kcpatients.naaccr
   # from surgery to recurrence
   ,surg_recur = a_tsurg>=0 & a_trecur<=0 & patient_num%in%kcpatients.naaccr
   # from surgery to death
   ,surg_death = a_tsurg>=0 & a_tdeath<=0 & patient_num%in%kcpatients.naaccr
   # from surgery to recurrence or death
   ,surg_drecur = a_tsurg>=0 & pmax(a_trecur,a_tdeath,na.rm = T)<=0 & patient_num%in%kcpatients.naaccr
);

#' Creates a hierarchy of lists containing various subsets of interest
sbs0 <- sapply(list(all=dat1,index=dat2),function(xx) do.call(ssply,c(list(dat=xx),subs_criteria[-1])),simplify=F);
#' Subsetting by the earlier randomly assigned train and test groups
sbs0$train <- lapply(sbs0$all,subset,patient_num%in%pat_samples$train);
sbs0$test <- lapply(sbs0$all,subset,patient_num%in%pat_samples$test);

#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
tsave(file=paste0(.currentscript,'.rdata'),list=ls());
