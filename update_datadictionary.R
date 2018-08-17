#' ---
#' title: "Update Data Dictionary With New Variables"
#' author: "Alex F. Bokov"
#' date: "08/16/2018"
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
knitr::opts_chunk$set(echo = F,warning = F,message=F);
#' ### What this file does.
#' 
#' This file will likely be run by hand from time to time, to keep `dctfile_tpl`
#' (current value: `r dctfile_tpl`) in sync with newly-added variables. The 
#' `dctfile_tpl` will not error on new variables that it doesn't know about but
#' it also does not supply any values for those variables (e.g. name-mappings,
#' comments, the `c_` grouping columns...) and for new variables all those fields
#' get populated with `NA`. The purpose of this script is to save manual effort
#' by filling in some of those `NA` s automatically and save out a union of all 
#' the new rows and all the old rows.
#' 
#' ### Merge old rows and new
#' 
#' Read the current `dctfile_tpl`
tpl_dct <- tread(dctfile_tpl,read_csv,na='');
#' Take the same columns in the same order from the dynamically generated `dct0`
#' already created in `run.R` by now
new_dct <- dct0[,names(tpl_dct)];
#' Add the rows from the original `dctfile_tpl` that are no longer in the new
#' data.(because in some future dataset they might again get included)
new_dct <- rbind(new_dct,subset(tpl_dct
                                ,!colsuffix%in%dct0$colsuffix | 
                                  !colname_long%in%dct0$colname_long));
#' To think about later: should I be dropping entries from the dynamically 
#' generated `dct0` in the first place? Will it break anything to keep them?
#' 
#' #### Automatically set some values.
if('c_analytic' %in% names(new_dct)){
  new_dct$c_analytic[grepl('_(info|unit)$',new_dct$colsuffix)]<-F;
  new_dct$c_analytic[new_dct$colsuffix == '_rc' & 
                       grepl('^016[0-9] Race '
                             ,new_dct$colname_long)]<-F;
  new_dct$c_analytic[new_dct$colsuffix == '_cmrbd_cmplctn' & 
                       grepl('^31[0-9]{2} Comorbid/Complication '
                             ,new_dct$colname_long)] <- F;
  }
if('c_naaccr_race' %in% names(new_dct)){
  new_dct$c_naaccr_race <- new_dct$colsuffix == '_rc' & 
    grepl('^016[0-9] Race ',new_dct$colname_long);}
if('c_naaccr_comorb' %in% names(new_dct)){
  new_dct$c_naaccr_comorb <- new_dct$colsuffix == '_cmrbd_cmplctn' & 
    grepl('^31[0-9]{2} Comorbid/Complication ',new_dct$colname_long);}
if('c_natf' %in% names(new_dct)){
  new_dct[new_dct$colsuffix %in% 
            subset(dct0,rule %in% c('diag','tvcdate'))$colsuffix,'c_natf'] <- T;
  new_dct$c_natf[lapply(dat1,na.omit) %>% lapply(unique) %>% lapply(length) %>% 
                   sapply(function(xx) xx==1) %>% 
                   (function(xx) xx & !grepl('_(info|unit)$',names(xx))) %>% 
                   `[`(seq_len(nrow(dct0)),.)]<- T;
  }
if('c_tnm' %in% names(new_dct)){
  new_dct$c_tnm <- grepl(' TNM | AJCC-',new_dct$colname_long);}
#' ### Now write it out.
#'
paste(file_path_sans_ext(dctfile_tpl),'backup',file_ext(dctfile_tpl),sep='.') %>%
  file.rename(dctfile_tpl,to=.);
write_csv(new_dct,dctfile_tpl,na='');