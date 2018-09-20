#' ---
#' title: "Kidney Cancer Data Processing"
#' author: "Alex F. Bokov"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
# init -------------------------------------------------------------------------
source('global.R');
.depends <- 'dictionary.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
.loadedobjects <- tload(.depdata);
knitr::opts_chunk$set(echo = F,warning = F,message=F);
#' Default args
formals(v)$dat <- as.name('dat1');
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
#' Create custom synonyms for 'TRUE' if needed
l_truthy_default <- eval(formals(truthy.default)$truewords);
l_missing <- c(NA,'Unknown','unknown','UNKNOWN');
# dat1 organize codes ----------------------------------------------------------
#' Load the NAACCR manual code mappings
levels_map <- tread(levels_map_file,read_csv,na='');

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
#' Clean up the pseudo-JSON around this variable
dat1$e_marital <- gsub('^\\{\"cc\":\"DEM\\|MARITAL:','',dat1$e_marital) %>%
  gsub('\",\"ix.*$','',.) %>% factor;
#' Simplified recurrence type
#' RECURRENCE VARIABLE
dat1$a_n_recur <- factor(dat1$n_rectype);
levels(dat1$a_n_recur)[!levels(dat1$a_n_recur) %in% 
                         c('Unknown if recurred or was ever gone'
                           ,'Never disease-free','Disease-free'
                           ,grep('Ambig_',levels(dat1$a_n_recur)
                                 ,val=T))]<-'Recurred';
#' Convert NAACCR race codes
dat1$a_n_race <- interaction(dat1[,v(c_naaccr_race)],drop = T) %>% 
  # clean up the non-informative-if-trailing codes
  gsub('."88"|."99"','',.) %>% factorclean(spec_mapper = levels_map
                                           ,var = '_rc',droplevels=T);
#' Unified NAACCR diabetes comorbidity
dat1$a_n_dm <- apply(dat1[,v(c_naaccr_comorb)],1,function(xx) any(grepl('"250',xx))); 
# kcpatients subsets -----------------------------------------------------------
#' Find the patients which had active kidney cancer (rather than starting with 
#' pre-existing)... first pass
kcpatients.emr <- subset(dat1,e_kc_i10|e_kc_i9)$patient_num %>% unique;
#' Patients that are recorded in NAACCR as having kidney cancer and a diagnosis 
#' date.
kcpatients.naaccr <- subset(dat1,(n_seer_kcancer|n_kcancer) & 
                              n_ddiag)$patient_num %>% unique;
kcpatients.naaccr_bad_dob <- intersect(kcpatients.naaccr,kcpatients.bad_dob);
#' create the raw time-to-event (tte) and censoring (cte) variables
#' along with making a_n_race and a_n_dm time invariant
# dat1 more analytical variables  ----------------------------------------------
dat1 <- mutate(dat1
               # the c() and paste() kind of screw up factors, making
               # extra code necessary down the line to restore them
               # There should be some simpler way to resolve multiple
               # non-identical values for the same patient. Or at least
               # a way to check to see which variables even have this 
               # problem in the first place
               ,n_sex=paste(c(unique(na.omit(n_sex)),NA)[1],collapse=',')
               ,a_n_race=paste(unique(na.omit(a_n_race)),collapse=',')
               ,a_n_recur=paste(unique(na.omit(a_n_recur)),collapse=',') %>%
                 # this is to insure that anything Disease-free gets paired 
                 # with wins out over dieasese-free
                 gsub('^Disease-free,','',.) %>% gsub(',Disease-free$','',.) %>%
                 # temporary hack so that it's more visible later that I did
                 # this rather than silently hiding it in levels_map.csv
                 gsub('Ambig_...0','Never disease-free',.)
               ,a_n_dm=any(a_n_dm)
               ,a_e_dm=e_dm_i9|e_dm_i10
               ,a_e_kc=e_kc_i9|e_kc_i10
               # THE DIAGNOSIS EVENT (PURE NAACCR)
               ,a_tdiag=tte(age_at_visit_days
                            # only count n_ddiag when it's recorded as a cancer case
                            ,(patient_num %in% kcpatients.naaccr & n_ddiag)  #|
                            #e_kc_i9|e_kc_i10)
               )
               # If we take at face value the very first occurrence of a kidney 
               # cancer diagnosis regardless of whether it is in NAACCR or in 
               # the EMR
               ,a_naive_tdiag=tte(age_at_visit_days
                                  ,e_kc_i10_i|e_kc_i9_i|
                                    (patient_num %in% kcpatients.naaccr & 
                                       n_ddiag))
               # THE RECURRENCE EVENT (PURE NAACCR)
               # look below the time-to-event section for the naive recurrence
               # event
               ,a_trecur=tte(age_at_visit_days,n_drecur)
               # THE SURGERY EVENT (PURE NAACCR)
               ,a_tsurg=tte(age_at_visit_days
                            ,if(any(n_rx3170,na.rm=T)) n_rx3170 else n_dsurg)
               # the old surgery event, for comparison purposes, will be removed 
               # eventually
               ,a_tsurg_bak=tte(age_at_visit_days,n_dsurg)
               # look below the time-to-event section for the naive surgery 
               # event
               # THE DEATH EVENT (COMBINED)
               # the pure NAACCR event is n_vtstat-- right now it's a factor
               # but in the time-to-event section of this script (below) it
               # gets converted to a tte just like these here.
               # TODO: double-check these components, a_tdeath is coming out as
               # if it is missing the contribution of =n_vtstat
               # ...might be because isTRUE returns a scalar value... not needed
               # anyway, let's see if with these changes 'a_tdeath' is in better
               # agreement with n_vtstat
               ,a_tdeath=tte(age_at_visit_days
                             # EMR death
                             #,isTRUE(age_at_death_days==age_at_visit_days)|
                             ,age_at_death_days==age_at_visit_days|
                               # death on discharge
                               e_dscdeath|
                               # SSN death
                               s_death|
                               # NAACCR death
                               #isTRUE(n_vtstat=="Dead")
                               n_vtstat=='Dead'
                             )
               # THE DIAGNOSIS CENSORING VARIABLE
               ,a_cdiag=cte(a_tdiag)
               # (naive version)
               ,a_naive_cdiag=cte(a_naive_tdiag)
               # THE RECURRENCE CENSORING VARIABLE
               ,a_crecur=cte(a_trecur)
               # THE SURGERY CENSORING VARIABLE
               ,a_csurg=cte(a_tsurg)
               # THE DEATH CENSORING VARIABLE
               ,a_cdeath=cte(a_tdeath)
               );

# time-to-event variables ------------------------------------------------------
#' ### Mass-converting variables to time-to-event form
#' 
#' Warning: this gets really into the daRk aRts of R here but the alternative is
#' a whole lot more code that accomplishes the same thing. Either way it's going
#' to be hard for intermediate R programmers to grok, but at least the concise
#' version is going to be easier for me to maintain, so I'm going with that one.
#' Sorry. I'll try to document the especially magical pieces as I go.
#' 
#' TODO-- refactor the NATF thing above so that we can intersect those with 
#' these to insure that all tte variables meet the assumption that they are 
#' TRUE/FALSE
#' 
# First, get the variable names that we designated in the data dictionary as
# needing to be time-to-event. Note that some of the variables have been 
# renamed, so that's why we have two v() expressions: the first for variables
# we haven't bothered to rename (yet?) and the second for variables we expect to
# have to refer to a lot accross multiple data refreshes, so we created 
# persistent names. Note the use of optional arguments. The second argument to
# v() is a named object, whose names v() uses to avoid returning any column
# names which do not exist our current data. The retcol argument is what column
# to return. Normally it's the 'colname' column in the data dictionary, but now
# we are returning the 'varname' column which is what some variables have been
# renamed TO.
# Note that we retain a copy of the vector created in the first expression for
# later use.
#dat1 <- (l_tte<-c(v(c_tte,dat1),v(c_tte,dat1,retcol = 'varname'))) %>%
dat1 <- (l_tte<-v(c_tte,dat1,retcol = c('colname','varname'))) %>%
# Now we are going to create an unevaluated expression that operates on each
  # of these in turn. 
  sapply(function(xx) substitute(tte(age_at_visit_days,ii)
                                 # the vector we created in the above step of 
                                 # this pipeline is of type character and we 
                                 # turn each of them into an unevaluated bit of
                                 # code by using the as.name() function. Now it
                                 # is of a data type that is compatible with 
                                 # getting inserted into the unevaluated 
                                 # call to tte() instead of the placeholder ii.
                                 # We pass it to substitute() inside a list that
                                 # via the optional env variable.
                                 # Notice that we don't need simplify=F for 
                                 # sapply() in this case because the output will 
                                 # always be a list, R believes that calls 
                                 # cannot be simplified to vectors in the first 
                                 # place.
                                 ,env=list(ii=as.name(xx)))) %>% 
  # So at this stage in the pipeline we have a list of unevaluated expressions. 
  # None of them will be valid in the .GlobalEnv context but they are valid 
  # in the context of dat1... if we could only break out the list into 
  # individual arguments to put into the '...' part of mutate(.data,...) ...
  # Or, we can construct the entire desired list of arguments to mutate by
  # prepending dat1. Notice that we wrap dat1 in a list this is to keep it as 
  # one object-- otherwise, since data frames are lists, when you concatenate 
  # a data.frame to another list you end up with one big list composed of
  # the data.frame's columns in addition to whatever was in the first list.
  # Also, adding on to the end one more new variable to be created, 'e_death'
  # which for technical reasons has to be treated differently from all the
  # others but is the death event as recorded by the 'PATIENT_DIMENSION' table
  # in i2b2. It's the EMR death record as opposed to the SSN one.
  c(list(.data=dat1),.
    ,alist( e_death=tte(age_at_visit_days
                       ,age_at_visit_days==age_at_death_days)
            # also here is 'n_vtstat', the NAACCR vital status... hopefully also
            # with its visit date set to the date of death-- if so that will 
            # save a few steps
           ,n_vtstat=tte(age_at_visit_days,n_vtstat=='Dead')
           ,n_cvtstat=cte(n_vtstat)
           )) %>% 
  # Instead we have a list with one more item at hte beginning than it had 
  # before. That item is dat1, and everything else is an unevaluated expression 
  # tha can be evaluated inside dat1. That makes it a valid set of arguments for
  # mutate. R doesn't allow you to explode a list out into separate variables 
  # like Python does (at least not very gracefully/robustly). But it does offer
  # the do.call function, which takes as its first argument a function, and as
  # its second a list that will become the arguments with which that function
  # gets invoked. 
  do.call(mutate,.);
  # We achieved our objective: now all our time-to-event variables 
  # are, instead of TRUE/FALSE integers showing how many days are until the
  # first occurences of their respetive events, 0 at those events if they happen
  # and positive numbers for as long as we have visits for after those events.
#' The the larger a TTE variable's value, the closer to the event it is or the 
#' longer ago the event took place. So to pick the first of several events we
#' need to take their parallel maximum.
#' 
#' Naive surgery variable-- earliest surgery of any kind accross all sources. No
#' attempts at sanity checks.
dat1$a_naive_tsurg <- dat1[,v(c_nephx)] %>% do.call(pmax,.);
#' Naive recurrence variable-- earliest secondary tumor of any kind, accross all
#' sources. No attempts at sanity checks.
dat1$a_naive_trecur <- dat1[,v(c_recur)] %>% do.call(pmax,.);
#' Their censoring variables
dat1[,c('a_naive_csurg','a_naive_crecur')] <- transmute(
  dat1,a_naive_csurg=cte(a_naive_tsurg),a_naive_crecur=cte(a_naive_tsurg)) %>%
  `[`(,-1);
#' 
# more analytic variable tweaks ------------------------------------------------
#' Below is a hack to restore NAs to NAACCR race designation and turn some 
#' character columns into factors with same order of levels as their i2b2 
#' counterparts
dat1$a_n_race <- with(dat1,ifelse(a_n_race=='',NA,a_n_race)) %>% 
  factor(levels=levels(dat1$race_cd));
#dat1$sex_cd <- factor(dat1$sex_cd,levels=levels(dat1$n_sex));
dat1$n_sex <- factor(dat1$n_sex,levels=c('1','2'),labels=c('m','f'));
# hispanic strict and lenient --------------------------------------------------
formals(adjudicate_levels)$levs <- list(Hispanic='Hispanic',`non-Hispanic`='non-Hispanic');
formals(adjudicate_levels)$DEFAULT <- 'Unknown';
.tmp_hspvar <- transmute(
  dat1
  ,temp_e_eth=adjudicate_levels(
    e_eth,levs=list(Hispanic='hispanic'
                    ,`non-Hispanic`=c('arab-amer','non-hispanic')))
  ,temp_e_hisp=if(any(e_hisp)) 'Hispanic' else NA
  ,temp_n_hisp=recode(n_hisp,Non_Hispanic='non-Hispanic',Unknown='Unknown'
                      ,.default='Hispanic') %>% adjudicate_levels
  ,temp_language_cd=recode(language_cd,Spanish='Hispanic',Other='non-Hispanic'
                           ,.default='Unknown') %>% adjudicate_levels
  ,temp_e_lng=gsub('^.*spanish.*$','Hispanic',e_lng,ignore.case = T) %>% 
    gsub('^.*(english|sign language|unknown).*$','Unknown',.,ignore.case = T) %>% 
    ifelse((.)%in%c('Hispanic','Unknown',NA),.,'non-Hispanic') %>% 
    adjudicate_levels
  );

dat1[,c('a_hsp_broad','a_hsp_strict')] <- apply(
  .tmp_hspvar[,-1],1,function(xx) c(
    broad=if(any(xx=='Hispanic',na.rm=T)) {
      'Hispanic' } else if(all(xx=='Unknown',na.rm=T)) 'Unknown' else {
        'non-Hispanic'}
    ,strict=if(all(xx[1:3]=='Hispanic',na.rm=T)&&!any(xx[4:5]=='non-Hispanic',na.rm=T)) {
      'Hispanic' } else if(all(xx[c(1,3)]=='non-Hispanic',na.rm=T)&&!any(xx[4:5]=='Hispanic',na.rm=T)){
        'non-Hispanic'} else 'Unknown')) %>% t %>% data.frame;
dat1$a_hsp_naaccr <- .tmp_hspvar$temp_n_hisp;

kcpatients.pre_existing <- subset(dat1,a_naive_tdiag>=0&a_tdiag<0)$patient_num %>% unique;

cohorts <- data.frame(patient_num=unique(dat1$patient_num)) %>% 
  mutate( NAACCR=patient_num %in% kcpatients.naaccr
         ,EMR=patient_num %in% kcpatients.emr
         ,PreExisting=patient_num %in% kcpatients.pre_existing
         ,combo=interaction(NAACCR,EMR,PreExisting)) %>% group_by(combo);

consort_table <- summarise(cohorts,NAACCR=any(NAACCR),EMR=any(EMR)
                           ,PreExisting=any(PreExisting)
                           ,N=length(patient_num))[,-1];
# training/testing/validation samples ------------------------------------------
#' Creating training/testing/validation samples
#' 
#' As long as the seed is the same, all random values will be generated the same
#' reproducibly.
tseed(project_seed);
#' Randomly to training, testing, or validation sets
pat_samples <- unique(dat1$patient_num) %>% 
  split(.,sample(c('train','test','val'),size=length(.),rep=T));
#' ## Create binned versions of certain numeric vars.
# 
#' (commented out until we can put a c_num2bin or something into dct0)
# dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
#   qii <- c(0,quantile(ii,c(.25,.5,.75),na.rm=T),Inf);
#   cut(ii,breaks = qii);
# })

# dat2, one-per-patient --------------------------------------------------------
#' ### Create a version of the dataset that only has each patient's 1st encounter
#' 
#' 
dat2a <- mutate_at(dat1,v(c_istte)
                   ,.funs=funs(ifelse(any((.)==0,na.rm=T)
                                      ,(age_at_visit_days)[(.)==0]
                                      # That +1 is important! We need events to
                                      # be distinguishable from censored events
                                      # by their temporal relationship with last
                                      # observations. By setting censored events
                                      # to one _more_ than final observation,
                                      # then time <= lastobs will select events.
                                      , max(age_at_visit_days)+1))) %>%
  summarise_all(function(xx){
    if(is.logical(xx)) any(xx) else (last(na.omit(xx)))});
#' This is the original dat2 that, after testing, will be replaced by the above
#' dat2a that will enable more flexible creation of survival curves.
dat2 <- summarise_all(dat1,function(xx) {
  if(is.logical(xx)) any(xx) else last(na.omit(xx))});
#' Column names for non-tte variables shared by dat2a and dat2
.nontte<-setdiff(intersect(names(dat2a),names(dat2)),v(c_tte));
#' Test to confirm that the non-tte columns of `dat2a` and `dat2` are identical
#' In all rows of .data2a_data2_eq the Total column is the sum of the `dat2` 
#' NA values and values where `dat2` is equal to `dat2a`... if this column is 
#' invariant and equal to the total number of rows that means the only cases 
#' where the values differ is where the `dat2` one is NA
.dat2a_dat2_eq <- mapply(function(aa,bb) {
  eq<-sum(aa==bb,na.rm=T);
  c(Equal=eq,`dat2 missing`=sum(is.na(bb)),Total=eq+sum(is.na(bb)))}
  ,dat2a[,.nontte],dat2[,.nontte]) %>% t;
# dat3, timevars ---------------------------------------------------------------
#+ create_xdat
# To understand what the below code does, see the comments for the very similar
# pattern in 'data.R' in the neighborhood lines 148-191 as of 8/19/2018
# using 'union()' instead of 'c()' here to avoid cumulative growth if script is
# re-run by hand
l_tte<-union(l_tte,c('e_death','n_vtstat'));
dat3 <- sapply(l_tte
                ,function(xx) substitute(if(any(ii==0)) age_at_visit_days[ii==0] 
                                         else NA,env=list(ii=as.name(xx)))) %>% 
  c(list(.data=select(subset(dat1,patient_num %in% kcpatients.naaccr)
                      ,c('age_at_visit_days',l_tte))),.) %>% 
  do.call(summarize,.);
#' Test to confirm that all l_tte variables are the same between dat3 and the 
#' equivalent subset of dat2a. Again, `Total` always being the same and equal to
#' the number of rows in `dat3` means the TTE columns have been correctly 
#' transformed.
.dat2a_dat3_eq <- mapply(function(aa,bb) {
  eq<-sum(aa==bb,na.rm=T);
  c(Equal=eq,`dat3 missing`=sum(is.na(bb)),Total=eq+sum(is.na(bb)))}
  ,subset(dat2a,patient_num %in% kcpatients.naaccr)[,l_tte],dat3[,l_tte]) %>% t
# subs_criteria, multiple subsets ----------------------------------------------
#' ### Some splits based on patient-level variables
#' 
#' Patients split by reason for (no) surgery. This used to be done on `dat1` and
#' gave slightly different results, probably due to multiple entries per patient
#' but the `dat2` versions seem like they are slightly better because they trend
#' toward more surgeries and fewer unknowns-- which is what one might expect
#' over time.
# kcpatients_surgreason <- split(dat1,dat1$n_surgreason) %>% 
#   lapply(function(xx) unique(xx$patient_num));
kcpatients_surgreason <- split(dat2,dat2$n_surgreason) %>% 
  lapply(pull,'patient_num');
#' Patients split by recurrence type
kcpatients_rectype <- split(dat2,dat2$a_n_recur) %>%
  lapply(pull,'patient_num');
#' Patients with a surgery record in NAACCR
kcpatients.surg <- unique(subset(dat1,a_tsurg==0)$patient_num);
#' Patients with a death recorded in NAACCR
kcpatients.naaccr_death <- unique(subset(dat1,n_vtstat==0));
#' ### Make several subsets of dat1 all at once
#
#' for later use to make multiple versions of the same table and 
#' multiple versions of the same graph. Each name is a legal variable name for 
#' that subset, the value assigned to it is an R expression that can be 
#' evaluated in the scope of `dat1` and will return a `TRUE`/`FALSE` vector
subs_criteria <- alist(
   # from diagnosis to surgery
    diag_surg = a_tdiag>=0 & a_tsurg<=0 #& patient_num %in% kcpatients.naaccr
   # from surgery to recurrence
   ,surg_recur = a_tsurg>=0 & a_trecur<=0 #& patient_num%in%kcpatients.naaccr
   # from surgery to death
   ,surg_death = a_tsurg>=0 & a_tdeath<=0 #& patient_num%in%kcpatients.naaccr
   # from surgery to recurrence or death
   ,surg_drecur = a_tsurg>=0 & pmax(a_trecur,a_tdeath,na.rm = T)<=0 & patient_num%in%kcpatients.naaccr
);
# evidence of cancer prior to first diagnosis in NAACCR
subs_criteria$prior_cancer <- comp_iijj(v(c_preexist,dat1
                                          ,retcol=c('colname','varname'))
                                        ,v(c_kcdiag,dat1
                                           ,retcol=c('colname','varname')));

# create complete versions of subsets according to current criteria
for(ii in names(subs_criteria)) {
  subs_criteria[[paste0(ii,'_complete')]] <- substitute(
    ii&jj,env=c( ii=subs_criteria[[ii]]
                ,jj=substitute(patient_num%in%kcpatients.naaccr)))};
# standalone completeness criterion
subs_criteria$naaccr_complete <- substitute(patient_num %in% kcpatients.naaccr);

#' Creates a hierarchy of lists containing various subsets of interest. But so 
#' far not really needed, commenting out for shorter load times.
#sbs0 <- sapply(list(all=dat1,index=dat2),function(xx) do.call(ssply,c(list(dat=xx),subs_criteria[-1])),simplify=F);
#' Subsetting by the earlier randomly assigned train and test groups
#sbs0$train <- lapply(sbs0$all,subset,patient_num%in%pat_samples$train);
#sbs0$test <- lapply(sbs0$all,subset,patient_num%in%pat_samples$test);

# save out ---------------------------------------------------------------------
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
