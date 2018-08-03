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

#' ## Load data if it exists 
#' 
#' (useful later, right now don't bother saving sessions)
#'if(session %in% list.files()) load(session);
dat0 <- tread(inputdata,read_tsv,na=c('(null)',''));

#' Read in the data dictionary
dct0 <- tread(dctfile,read_csv,na = '');
colnames(dat0) <- tolower(colnames(dat0));
#' If you want to use some other set of columns as indices that is
#' specific to the version of the data you are using, declare
#' a different `alist` in your config.R, but it's better to not
#' mess with this.
if(!exists('l_indices')) l_indices <- alist(
  pnum=patient_num,vnum=encounter_num
);
 
#' Create copy of original dataset
dat1 <- do.call(mutate,c(list(dat0),l_indices));

#' Create custom synonyms for 'TRUE' if needed
l_truthy_default <- eval(formals(truthy.default)$truewords);
#l_truthy_default <- c("Yes", l_truthy_default);
l_missing <- c(NA,'Unknown','unknown','UNKNOWN');

#' Creating training/testing/validation samples
#' 
#' As long as the seed is the same, all random values will be generated the same
#' reproducibly.
tseed(project_seed);
#' Randomly to training, testing, or validation sets
pat_samples <- split(dat1$pnum,sample(c('train','test','val')
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
dat2 <- group_by(dat1,pnum) %>% summarise_all(first);

#' Each name is a legal variable name for that subset, the value
#' assigned to it is an R expression that can be evaluated in the
#' scope of `dat1` and will return a `TRUE`/`FALSE` vector
subs_criteria <- alist(
   SUBSETNAME00=0==1
  ,SUBSETNAME01=0==1
);

#' Creates a hierarchy of lists containing various subsets of interest
sbs0 <- sapply(list(all=dat1,index=dat2),function(xx) do.call(ssply,c(list(dat=xx),subs_criteria[-1])),simplify=F);
#' Subsetting by the earlier randomly assigned train and test groups
sbs0$train <- lapply(sbs0$all,subset,pnum%in%pat_samples$train);
sbs0$test <- lapply(sbs0$all,subset,pnum%in%pat_samples$test);

#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
tsave(file=paste0(.currentscript,'.rdata'),list=ls());
