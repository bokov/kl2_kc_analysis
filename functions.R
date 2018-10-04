# small utils ------------------------------------------------------------------

#' This function takes a list of package named, loads them if they are
#' available, otherwise attempts to install each one and then again 
#' attempts to load it.
instrequire <- function(pkglist
                        ,quietly=TRUE
                        ,repos=getOption('repos','https://cran.rstudio.com/')){
  pkgs_installed <- sapply(pkglist,require,character.only=T);
  if(length(pkgs_needed <- names(pkgs_installed[!pkgs_installed]))>0){
    install.packages(pkgs_needed,repos=repos,dependencies = T);
    pkgs_final <- sapply(pkgs_needed,require,character.only=T,quietly=quietly);
    if(!all(pkgs_final)){
      stop(c('the following required packages could not be installed:\n'
             ,paste0(names(pkgs_final[!pkgs_final]),collapse = ', ')));
    }
  };
}


#' takes input and returns it with attributes set
with_attrs<-function(xx,rep.attrs=list(),app.attrs=list()){
  attrs <- attributes(xx); if(is.null(attrs)) attrs<-list();
  for(ii in names(rep.attrs)) attrs[[ii]] <- rep.attrs[[ii]];
  for(ii in names(app.attrs)) attrs[[ii]] <- c(attrs[[ii]],app.attrs[[ii]]);
  attributes(xx) <- attrs;
  xx;
}

#' takes input and returns it with a comment attribute
cm <- with_cm <- function(xx,comment=NULL,append=T
                          ,transf=stringr::str_squish){
  if(!is.null(transf)) comment <- transf(comment);
  if(append) with_attrs(xx,app.attrs=list(comment=comment)) else {
    with_attrs(xx,rep.attrs=list(comment=comment));
  }
}

#' Take an object name \code{obj}, check to see if it  exists in environment \code{env}
#' and if it does not, run \code{expression} \code{EXPR} and assign its result to \code{obj}
#'
#' @param obj   A \code{character} string (required) naming the variable in env
#'   to which the results of running \code{EXPR} will be assigned.
#' @param EXPR  An \code{expression} to execute and assign to the object named
#'   by \code{obj} if it doesn't already exist.
#' @param env   An \code{environment} to check for the existence of \code{obj}
#'   and where to create it if it doesn't exist.
#'
#' @return Does not return anything.
#'
#' @examples `checkrun('dat3',{group_by(dat1,idn_mrn) %>% summarise_all(first)});`
checkrun <- function(obj,EXPR,env=as.environment(-1)){
  env<-env;
  if(length(ls(env,all=T,pattern=obj))==0){
    browser();
  }
}

getCall.list <- getCall.data.frame <- getCall.gg <- function(xx) {attr(xx,'call')};

# why not update calls?
update.call <- function(xx,...){
  dots <- list(...);
  for(ii in names(dots)) xx[[ii]] <- dots[[ii]];
  xx;
}

#' Delete all the junk in your environment, for testing
clearenv <- function(env=.GlobalEnv) {rm(list=setdiff(ls(all=T,envir=env),'clearenv'),envir=env);}

#' Stack a vector to form a matrix with repeating rows, with optional 
#' column names and transformation
#'
#' @param  vv    A vector which will become the row of a matrix
#' @param  nr    Number of (identical) rows this matrix will contain
#' @param  trans An optional function that can take a matrix as its 
#'              sole argument. Useful functions include `as.data.frame()`
#'              `as_tibble()` and `as.table()`
#' @return A matrix, unless the function specified in the `trans` argument
#'         causes the output to be something else.
#' @export 
#'
#' @examples 
#' vec2m(1:10,5);
#' vec2m(1:10,5,tr=data.frame);
#' vec2m(setNames(1:12,month.name),3);
vec2m <- function(vv,nr=1,trans=identity) {
  return(trans(matrix(as.matrix(c(vv)),nrow=nr,ncol=length(vv),byrow=T
                      ,dimnames=list(NULL,names(vv)))));
}

#' returns call with ALL arguments specified, both the defaults and those
#' explicitly provided by the user
fullargs <- function(syspar=sys.parent(),env=parent.frame(2L),expand.dots=TRUE){
  fn <- sys.function(syspar);
  frm <- formals(fn);
  cll <- match.call(fn,sys.call(syspar),expand.dots = expand.dots,envir = env);
  defaults <- setdiff(names(frm),c(names(cll),'...'));
  for(ii in defaults) cll[[ii]] <- frm[[ii]];
  return(cll);
}

#' Take a set of objects coercible to matrices and perform sprintf on them while
#' preserving their dimensions (obtained from the first argument of ...)
mprintf <- function(fmt,...,flattenmethod=1){
  dots <- list(...);
  out<-dots[[1]];
  # if factors not converted to characters, those cells will come out as NA
  if(is.factor(out)) out <- as.character(out) else if(is.list(out)){
    for(ii in seq_along(out)) if(is.factor(out[[ii]])) {
      out[[ii]]<-as.character(out[[ii]])}}
  if(is.null(nrow(out))) {
    warning('Converting output to matrix. Might be errors.');
    outnames<-names(out);
    out <- t(matrix(out));
    try(colnames(out)<-outnames);
  }
  for(ii in seq_along(dots)) dots[[ii]] <- c(if(flattenmethod==1) {
    unlist(dots[[ii]])} else if(flattenmethod==2){
      sapply(dots[[ii]],as.character)});
  vals <- do.call(sprintf,c(fmt,dots));
  for(ii in seq_len(nrow(out))) for(jj in seq_len(ncol(out))) {
    out[ii,jj] <-matrix(vals,nrow=nrow(out))[ii,jj]};
  out;
  }


# renaming and remapping  ------------------------------------------------------
#' A function to re-order and/or rename the levels of a factor or 
#' vector with optional cleanup.
#'
#' @param xx            a vector... if not a factor will be converted to 
#'                      one
#' @param lookuptable   matrix-like objects where the first column will
#'                      be what to rename FROM and the second, what to
#'                      rename TO. If there is only one column or if it's
#'                      not matrix-like, the levels will be set to these
#'                      values in the order they occur in `lookuptable`.
#'                      If there are duplicated values in the first column
#'                      only the first one gets used, with a warning.
#'                      Duplicate values in the second column are allowed 
#'                      and are a feature.
#' @param reorder       Whether or not to change the order of the factor
#'                      levels to match those in `lookuptable`. True by
#'                      default (logical). By default is `TRUE`, is set to
#'                      `FALSE` will try to preserve the original order of
#'                      the levels.
#' @param unmatched     Scalar value. If equal to -1 and `reorder` is `TRUE` 
#'                      then unmatched original levels are prepended to the 
#'                      matched ones in their original order of occurence. If 
#'                      equal to 1, then appended in their original order of 
#'                      occurrence. If some other value, then they are all 
#'                      binned in one level of that name. The (empty) new ones 
#'                      always go to the end.
#' @param droplevels    Drop unused levels from the output factor (logical)
#' @param case          Option to normalize case (`'asis'`` leaves it as it was)
#'                      before attempting to match to `lookuptable`. One value 
#'                      only
#' @param mapnato       If the original levels contain `NA`s, what they should 
#'                      be instead. They stay `NA` by default.
#' @param remove        Vector of strings to remove from all level names (can
#'                      be regexps) before trying to match to `lookuptable`.
#' @param otherfun      A user-specified function to make further changes to
#'                      the original level names before trying to match and
#'                      replace them. Should expect to get the output from
#'                      `levels(xx)` as its only argument.
#' @param spec_mapper   A constrained lookup table specification that includes
#'                      a column named `varname` in addition to the two columns
#'                      that will become `from` and `to`. This is for extra
#'                      convenience in projects that use such a table. If you
#'                      aren't working on a project that already follows this
#'                      convention, you should ignore this parameter.
#' @param var           What value should be in the `varname` column of 
#'                      `spec_mapper`
#' @param fromto        Which columns in the `spec_mapper` should become the 
#'                      `from` and `to` columns, respectively. Vector of length
#'                      2, is `c('code','label')` by default.
factorclean <- function(xx,lookuptable,reorder=T,unmatched=1
                        ,droplevels=F,case=c('asis','lower','upper')
                        ,mapnato=NA,remove='"',otherfun=identity
                        ,spec_mapper,var,fromto=c('code','label')){
  if(!is.factor(xx)) xx <- factor(xx);
  lvls <- levels(xx);
  lvls <- switch (match.arg(case)
                   ,asis=identity,lower=tolower,upper=toupper)(lvls) %>% 
    submulti(cbind(remove,'')) %>% otherfun;
  levels(xx) <- lvls;
  # Check to see if spec_mapper available.
  if(!missing(spec_mapper)&&!missing(var)){
    lookuptable <- subset(spec_mapper,varname==var)[,fromto];
  }
  # The assumption is that if you're passing just a vector or something like
  # it, then you want to leave the level names as-is and just want to change
  # the ordering
  if(is.null(ncol(lookuptable))) lookuptable <- cbind(lookuptable,lookuptable);
  # can never be too sure what kind of things with rows and columns are getting 
  # passed, so coerce this to a plain old vanilla data.frame
  lookuptable <- data.frame(lookuptable[,1:2]) %>% setNames(c('from','to'));
  if(length(unique(lookuptable[,1]))!=nrow(lookuptable)) {
    lookuptable <- lookuptable[match(unique(lookuptable$from),lookuptable$from),];
    warning("You have duplicate values in the first (or only) column of your 'lookuptable' argument. Only the first instances of each will be kept");
  }
  if(reorder){
    extras <- data.frame(from=lvls,to=NA,stringsAsFactors = F) %>%
      subset(!from %in% lookuptable$from);
    lookupfinal <- if(unmatched==-1) rbind(extras,lookuptable) else {
      rbind(lookuptable,extras)};
  } else {
    lookupfinal <- left_join(data.frame(from=lvls,stringsAsFactors = F)
                             ,lookuptable,by='from') %>% 
      rbind(subset(lookuptable,!from%in%lvls));
  }
  # if the 'unmatched' parameter has the special value of -1 or 1, leave the 
  # original names for the unmatched levels. Otherwise assign them to the bin
  # this parameter specifies
  lookupfinal$to <- with(lookupfinal,if(unmatched %in% c(-1,1)){
    ifelse(is.na(to),from,to)} else {ifelse(is.na(to),unmatched,to)});
  # Warning: not yet tested on xx's that are already factors and have an NA
  # level
  lookupfinal$to <- with(lookupfinal,ifelse(is.na(from),mapnato,to));
  out <- factor(xx,levels=lookupfinal$from);
  levels(out) <- lookupfinal$to;
  if(droplevels) droplevels(out) else out;
}


#' into the specified existing or new level. That level is listed last
#' in the resulting factor.
#' @param xx A \code{vector} (required).
#' @param topn \code{numeric} for the top how many levels to keep (optional, default =4)
#' @param binto \code{character} which new or existing level to dump the other values in
cl_bintail <- function(xx,topn=4,binto='other'){
  if(!is.factor(xx)) xx <- factor(xx);
  counts <- sort(table(xx),decreasing = T);
  if(is.numeric(binto)) binto <- names(counts)[binto];
  keep <- setdiff(names(counts)[1:min(length(counts),topn)],binto);
  droplevels(
    factor(
      ifelse(
        xx %in% keep, as.character(xx), binto
        ),levels=c(keep,binto)));
}

#' A sketch for a possible future function that converts stargazer tables into
#' a universal markdown pipe format
starkable <- function(xx,firstrowisdata=T,row.names=F,taildrop=1
                      ,hrow=2
                      ,sgoutput=F,kboutput=T,searchrep=matrix(c('V1',''),ncol=2)
                      ,...){
  output <- if(!sgoutput) capture.output else identity;
  formals(htmltab)$header=hrow;
  output(out <- stargazer(xx,type = 'html')  %>%
           htmltab %>% `[`(firstrowisdata,,drop=F) %>%
           kable(format = 'markdown',row.names=row.names,...) %>%
           head(length(.)-taildrop) %>% paste0('\n') %>%
           submulti(searchrep = searchrep));
  if(kboutput) cat(out,'\n');
  invisible(out);
}

#' Take a character vector and perform multiple search-replace 
#' operations on it.
#' @param xx A \code{vector} of type \code{character} (required)
#' @param searchrep A \code{matrix} with two columns of type \code{character} (required). The left column is the pattern and the right, the replacement.
#' @param method One of 'partial','full', or 'exact'. Controls whether to replace only the matching regexp, replace the entire value that contains a matching regexp, or replace the entire value if it's an exact match.
submulti <- function(xx,searchrep
                     ,method=c('partial','full','exact'
                               ,'starts','ends','startsends')){
  # if no method is specified by the user, this makes it take the first value
  # if a method is only partially written out, this completes it, and if the
  # method doesn't match any of the valid possibilities this gives an informativ
  # error message
  method<-match.arg(method);
  # rr is a sequence of integers spanning the rows of the searchrep matrix
  rr <- 1:nrow(searchrep);
  # oo will hold the result that this function will return
  oo <- xx;
  switch(method
         ,partial = {for(ii in rr)
           oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo)}
         ,full =    {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo)]<-searchrep[ii,2]}
         ,exact = {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,fixed=T)]<-searchrep[ii,2]}
           #oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo,fixed = T)}
         ,starts = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1]),searchrep[ii,2],oo)}
         ,ends = {for(ii in rr)
           oo <- gsub(paste0(searchrep[ii,1],'$'),searchrep[ii,2],oo)}
         ,startsends = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1],'$'),searchrep[ii,2],oo)}
  );
  oo;
}

#' Take a data.frame or character vector and a vector of grep targets and return
#' the values that match (for data.frame, column names that match). If no 
#' patterns given just returns the names
#' @param xx A \code{data.frame} or character vector (required)
#' @param patterns A character vector of regexp targets to be OR-ed
grepor <- function(xx,patterns='.') {
  if(is.list(xx)) xx <-names(xx);
  grep(paste0(patterns,collapse='|'),xx,val=T);
}

#' Usage: `xx<-mapnames(xx,lookup)` where lookup is a named character vector
#' the names of the elements in the character vector are what you are renaming
#' things TO and the values are what needs to be matched, i.e. what renaming things
#' FROM. If you set namesonly=T then it just returns the names, not the original
#' object.
mapnames<-function(xx,lookup,namesonly=F,...){
  xnames <- names(xx);
  idxmatch <- na.omit(match(xnames,lookup));
  newnames <- names(lookup)[idxmatch];
  if(namesonly) return(newnames);
  names(xx)[xnames %in% lookup] <- newnames;
  xx;
}

#' Example of using R methods dispatch
#' 
#' The actual usage is: `truthy(foo)` and `truthy()` itself figures
#' out which method to actually dispatch.
truthy <- function(xx,...) UseMethod('truthy');
truthy.logical <- function(xx,...) xx;
truthy.factor <- function(xx,...) truthy.default(as.character(xx),...);
truthy.numeric <- function(xx,...) xx>0;
truthy.default <- function(xx,truewords=c('TRUE','true','Yes','T','Y','yes','y')
                           ,...) xx %in% truewords;
truthy.data.frame <- function(xx,...) as.data.frame(lapply(xx,truthy,...));

# table utilities --------------------------------------------------------------
#' Take a data.frame and create a PL/SQL table definition for it, ready to paste into DBVis and such
#' @param xx A \code{data.frame} (required)
sql_create_table <- function(xx,tname,sqltemplate='CREATE TABLE %s (%s);'){
  mysearchrep <- cbind(c('integer','character','numeric','Date','logical','factor')
    ,c('NUMBER(32)','VARCHAR2(nn)','NUMBER(32)','DATE','NUMBER(1)','VARCHAR2(nn)'));
  if(missing(tname)) tname <- as.character(substitute(xx));
  thenames <- sapply(xx, class);
  thenames <- submulti(thenames, mysearchrep, 'exact');
  strlengthres<-apply(nsqip,2,function(xx){max(str_length(xx))});
  thenames <- apply(cbind(round(strlengthres*1.5), thenames),1,function(xx){gsub('nn',xx[1],xx[2])});
  thenames <- paste(' ',names(thenames), as.character(thenames), sep=' ', collapse= ',\n');
  thenames <- gsub('.', '_', thenames, fixed=TRUE);
  thenames <- gsub('_{1,}','_',thenames);
  thenames <- gsub('_ ',' ',thenames);
  thenames <- sprintf(sqltemplate, tname, thenames);
  cat(thenames);
  invisible(thenames);
  # TODO: collect the column names and classes as we did in console
  # TODO: use the above and submulti() to convert R classes to SQL data types
  # TODO: paste() and/or sprintf() to create the string that will be the output
}

#' Perform `mutate()` on a subset of rows without breaking the pipeline but with
#' a number of safety checks. Partly based on code by [G. Grothendieck](https://stackoverflow.com/users/516548/g-grothendieck)
#' on [StackOverflow](https://stackoverflow.com/a/34096575). Not very efficient,
#' the purpose is to reilably avoid unintended consequences when modifying
#' data.frame like objects without aborting the overall process (hence warnings
#' and unaltered returns instead of hard errors)
#'
#' @param .data        A `data.frame` like object
#' @param .condition   A condition for selecting rows to mutate, to be 
#'                     to be evaluated in the scope of .data
#' @param ...          Arbitrary arguments passed to `mutate()`
#' @param .namevals    Name-value list (can be alist) prepended to `...` to 
#'                     to make it easier to use this function programatically
#' @param UNIQUE       Whether to enforce unique rows. If TRUE (default) then
#'                     detection of new duplicate rows as a result of operation
#'                     will cause a warning and the return of the original 
#'                     `.data` unchanged.
#' @param NONEWCOLS    Whether to ban creation of new columns. If TRUE (default)
#'                     will cause a warning and skipping of any columns that 
#'                     don't exist in `.data`. Otherwise will create any 
#'                     additional columns that are specified.
#' @param NOMULTI      Whether to ban updates of multiple rows at a time. If 
#'                     TRUE (default) will give a warning and return `.data` 
#'                     unchanged if `.condition` matches more than one row.
#' @param NOCOERCE     Whether to ban changing the data-types of rows. If TRUE
#'                     (default) will give a warning and return `.data` 
#'                     unchanged if otherwise some columns would have been 
#'                     converted from their original data types.
#' @param NEWROWS      Whether to permit appending of a new row instead of 
#'                     updating and existing one if `.condition` is not met. If
#'                     TRUE (default) appends a new row and gives a message 
#'                     saying so. Otherwise gives a warning and returns `.data`
#'                     unchanged.
#' @param DEFAULT      What value to insert into cells for which a value is not
#'                     specified (`NA` by default).
#' @param .envir       No need to set manually, used for evaluation of 
#'                     `.condition`
#'
#' @return  A modified version of `.data` potentially with one additional row
#'          or one or more existing rows altered. If safeguard conditions 
#'          triggered, the original `.data` with a warning.
#' @export
#'
#' @examples 
#' 
# # modifying an existing row
# mutate_rows(iris,Species=='setosa'&Petal.Width<0.2
# ,Petal.Length=1.5,Petal.Width=0.2,NOMULTI=FALSE,UNIQUE=FALSE)
# 
# # Appending a new row
# mutate_rows(iris,F,Petal.Length=1.5,Petal.Width=0.2,NOMULTI=FALSE)
#' 
mutate_rows <- function(.data,.condition=FALSE,...,.namevals=list(),UNIQUE=TRUE
                        ,NONEWCOLS=TRUE,NOMULTI=TRUE,NOCOERCE=TRUE,NEWROWS=TRUE
                        ,DEFAULT=NA,.envir=parent.frame()){
  # create combined name-val list from .namevals and ...
  innames <- setdiff(names(invals <- c(...,.namevals)),'');
  if(length(innames)!=length(invals)) {
    invals <- invals[innames];
    warning('You specified either unnamed values which were all ignored or duplicate names, of which only the first one was used.');
  }
  # handle new columns
  if(length(indiff<-setdiff(innames,names(.data)))>0){
    if(NONEWCOLS) {
      invals[indiff] <- NULL; innames <- names(invals);
      warning(sprintf(
        "You specified columns %s that don't yet exist and they will be ignored. If you want them to be created you should set NONEWCOLS to FALSE"
        ,paste0(indiff,collapse=', ')
        ));
    } else {
      warning('New columns added');
      .data[,indiff] <- DEFAULT;}
  }
  .condition <- eval(eval(substitute(.condition), .data, .envir),.data,.envir);
  .condition <- .condition & !is.na(.condition);
  matches <- sum(.condition);
  #matches<-sum(.condition <- eval(eval(substitute(.condition), .data, .envir)
  #                                ,.data,.envir),na.rm=TRUE);
  # if 0 matches, check NEWROWS
  if(matches==0){
    if(NEWROWS) {
      mrows <- subset(.data,FALSE); mrows[1,]<-DEFAULT;
      mrows[,innames] <- invals[innames];
      out <- rbind(.data,mrows);
      if(UNIQUE && nrow(unique(out)) < nrow(unique(.data))+1){
        warning("Insertion of duplicate row detected, so nothing was changed. To allow duplicate rows, set UNIQUE=FALSE");
        return(.data);
      }
      message('Adding a new row.');
    } else {
      warning("No existing rows matched your criteria and adding new rows disallowed, so nothing was changed. To add new rows, set NEWROWS=TRUE");
      return(.data);}} else {
        # if >1 matches, check NOMULTI
        if(matches>1 && NOMULTI){
          warning("More than one row matched your criteria, so nothing was changed. To allow multiple row updates, set NOMULTI=FALSE");
          return(.data);}
        out <- .data;
        out[.condition,] <- do.call(dplyr::mutate
                                    ,c(list(.data=.data[.condition,]),invals));
        # check for non-uniqueness
        if(UNIQUE && nrow(unique(out)) < nrow(unique(.data))){
          warning("Creation of duplicate row detected, so nothing was changed. To allow duplicate rows, set UNIQUE=FALSE");
          return(.data);
        }
      }
  # check for coercion 
  if(NOCOERCE && !all(mapply(function(aa,bb) all.equal(class(aa),class(bb))
                             ,.data,out))){
    warning("Performing this update would have changed the data types of some columns, so nothing was changed. To allow changing data types, set NOCOERCE=TRUE");
    return(.data);
  }
  return(out);
}



#' Take a vector of times, and an expression to evaluate in the calling context
#' and return a vector of times until the first occurence of the expression
#' @param time 
#' @param expr 
#' @param ... 
tte <- function(time,expr,...){
  event <- min(which(expr));
  # The +1 needed so that a series of followups where an 
  # event never happens is always negative while a series
  # of followups when an event is observed has a 0 in it
  if(is.infinite(event)) etime <- max(time)+1 else {
    etime <- time[event];}
  out <- time - etime;
}


#' Code the output of `tte()` such that before an event observations get `0`
#' during the (first) event observations get `1` and subsequent get `2`
#' 
#' By default it shifts the result by 1, producing a vector that can directly be
#' used as the `event` in a `Surv(time,event)` type expression assuming you trim
#' out the >1 values. To change the amount of shift, use the optional `shift`
#' argument. To do something other than shifting by that amount, use the 
#' optional `fn` variable which takes as its argument a function that can 
#' operate on exactly two arguments.
#' 
cte <- function(...,shift=1,fn=`+`) {fn(sign(pmax(...,na.rm=T)),shift)};


#' Take two vectors of names `ii` and `jj`, perform an aggregating 
#' function on each vector respectively and then perform a comparison function 
#' `fcmp` on the two aggregation results.
#' 
#' If the optional `env` is provided, returns a result vector 
#' evaluated in the environment of env, otherwise return an unevaluated call.
#'
#' @param ii    A vector of object or variable names (required)
#' @param jj    A vector of object or variable names (required)
#' @param env   An optional list-like object containing above-named
#'              objects.
#' @param fii   The name (character) of a function that can take all the objects
#'              represented in `ii` as its argument-list... 'pmax' by default.
#' @param fjj   The name (character) of a function that can take all the objects
#'              represented in `jj` as its argument-list. Set to `fii` by
#'              default.
#' @param fcmp  The name (character) of a function that will take the outputs of
#'              functions named in `fii` and `fjj` as its two arguments. By 
#'              default it's '<'.
#' @param optii A named list of optional arguments to pass to `fii`. By default
#'              it only name-value pair is `na.rm=T`
#' @param optjj A named list of optional arguments to pass to `fii`. By default
#'              it's set to `optjj`
#'
#' @return A call generated using the above arguments, unless `env`  is
#'         given in which case the result of evaluating that call in the context
#'         of `env`
#' @export
#'
#' @examples comp_iijj(v(c_natf,dat1,retcol=c('colname','varname'))
#'                    ,v(c_kcdiag,dat1,retcol=c('colname','varname')),dat1);
#' 
comp_iijj <- function(ii,jj,env,fii='pmax',fjj=fii,fcmp='>'
                      ,optii=list(na.rm=T),optjj=optii){
  ii <- lapply(ii,as.name); jj <- lapply(jj,as.name);
  callii <- do.call(call,c(fii,ii,optii),quote=T);
  calljj <- do.call(call,c(fjj,jj,optjj),quote=T);
  out <- do.call(call,c(fcmp,callii,calljj),quote=T);
  if(!missing(env)) out <- eval(out,envir = env);
  return(out);
}

#' Returns a vector of column names that contain data elements of a particular type
#' as specified by the user: "integer","POSIXct" "POSIXt", "numeric", "character", 
#' "factor" and "logical". 
vartype <- function(dat, ctype) {
  xx <- unlist(sapply(dat, class));
  idx <- which(xx %in% ctype);
  res <- names(xx)[idx];
  return(res)
}

#' This function can be called from `stat_summary()` as the
#' `fun.data=` argument. It will cause group counts to be 
#' over-printed on a `geom_boxplot()` (probably other similar
#' plots too) if `stat_summary()` is added to it.
n_fun <- function(xx) data.frame(y=mean(quantile(xx,c(.5,.75))),label=as.character(length(xx)));

#' take a list of subset criteria and return a list of data.frames
ssply<-function(dat,...) sapply(sys.call()[-(1:2)],function(ii) subset(dat,eval(ii)),simplify=F);

#' save a named list of tables, including names
savetablelist <- function(lst,fileprefix,filesuffix=paste0(format(Sys.Date(),'%m-%d-%Y-%H_%M_%S'),'.tsv')
                          ,filepath='./',outfile=paste0(filepath,fileprefix,filesuffix)
                          ,sep='\t',row.names=F
                          ,singletabname=as.character(substitute(lst))
                          ,replaceifexists=T,...) {
  if(is.data.frame(lst)) lst<-setNames(list(lst),singletabname);
  if(replaceifexists&&file.exists(outfile)) file.remove(outfile);
  for(ii in names(lst)){
    write(ii,file=outfile,append=T);
    write.table(lst[[ii]],outfile,sep=sep,row.names=row.names,append=T);
  }
}

#' function for creating a table summarizing the difference between two numeric
#' variables-- how many greater than, how many less than, by how much (median),
#' how many of each are missing, etc.
#' 
#' Example: 
# e_table.default(round(foo,2),round(bar,2)) %>% 
# mapply(function(aa,bb) ifelse(is.na(aa),'-',bb(aa))
# ,.,list(identity,function(xx) sprintf('(%2.1f%%)',xx*100),identity)
# ,SIMPLIFY=F) %>% c(fmt='%s %s\n%s',.) %>% do.call(mprintf,.) %>% 
# pander(style='multiline',keep.line.breaks=T,split.tables=600)
#'
#' @param xx 
#' @param yy 
#' @param xxnames 
#' @param breaks 
#' @param autocenter 
#' @param include.lowest 
#' @param right 
#' @param dig.lab 
#' @param diffn 
#' @param sapplyfn 
#' @param ... 
e_table.default <- function(xx,yy,xxnames=NA,breaks=c(),autocenter=T
                            ,include.lowest=T,right=T,dig.lab=3L
                            ,xlabel=gsub('[^a-zA-Z._]',''
                                         ,as.character(substitute(xx)))
                            ,diffn=`-`,sapplyfn=median,...){
  # get differences, however they might be defined
  vals <- diffn(xx,yy);
  if(length(xlabel)>1) xlabel <- gsub('^\\.','',paste(xlabel,collapse='.'));
  # set the interval for 'no difference'. Smaller than the smallest difference
  epsilon <- if(autocenter) {
    # if these are integer values just use fractions, they will never get that
    # low
    if(all(vals==floor(vals),na.rm=T)) c(-.5,.5) else {
      # otherwise use half the smallest difference and if they are all 0 then
      # use half the smallest representable value? (question: if all non 
      # missing are equal, what does it matter what I use? why not use something
      # non wierd, like also -0.5,0.5? ...to consider later)
      c(-1,1)*min(c(.Machine$double.eps,abs(vals[vals!=0])/2),na.rm=T)}
    } else c();
  renametable <- cbind(
     match=c('\\)|\\(|\\]|\\[','^-Inf,','([-0-9.]{1,}), Inf$',',','0 to 0'
             ,'\\\n0')
    ,replace=c('','Below\\\n','Above\\\n\\1',' to ','same',''));
  # didn't take long for the brittle thing to break
  if(!is.null(epsilon)) {
    renametable <- rbind(c(paste0('[-]?',format(max(epsilon),digits=dig.lab))
                           ,'0'),renametable)};
  # generate the breaks to use with cut(). Order doesn't matter.
  breaks <- c(-Inf,breaks,epsilon,Inf);
  # create the factor for various bins of less-than, greater-than, as well as 
  # equal-to if autocenter was specified
  cuts <- try(cut(vals,breaks=breaks,include.lowest = include.lowest,right=right
              ,dig.lab=dig.lab));
  if(class(cuts)[1]=='try-error') browser();
  # rename the comparison factor levels to something more readable
  levels(cuts) <- submulti(levels(cuts),renametable);
  # this is brittle right here... be prepared to encounter a case where this 
  # doesn't work and use it to develop a more general solution
  levels(cuts)[levels(cuts)=='0 to 0']<-'same';
  # the factor for comparing missingness
  cutsna <- interaction(is.na(xx),is.na(yy));
  # rename the levels, they should always be the same order I think
  levels(cutsna) <- c('Neither\\\nmissing','Left\\\nmissing','Right\\\nmissing'
                      ,'Both\\\nmissing');
  # create the counts and proportions
  pdiff<-prop.table(cdiff <- table(cuts)); pna<-prop.table(cna <- table(cutsna));
  # get the medians (or whatever the sapplyfn is)
  mdiff<-sapply(split(vals,cuts),sapplyfn,na.rm=T);
  mna <- sapply(split(vals,cutsna),sapplyfn,na.rm=T);
  out <- sapply(list(count=c(cdiff,cna),prop=c(pdiff,pna),stat=c(mdiff,mna))
                ,rbind,simplify=F);
  for(ii in seq_along(out)) rownames(out[[ii]]) <- xlabel;
  class(out) <- 'e_table';
  out;
}

#' Warning! For the data.frame method of e_table, it might be important to 
#' put the equality bin into the breaks argument and set autocenter=F if any of
#' the values are non-integers. Otherwise might be inconsistent break-points for
#' different rows in the result. Not sure if and when that would be a practical
#' problem, though. Needs more thought.
e_table.data.frame <- e_table.list <- function(xx,yy,xxnames,...){
  # the 'out' object created here is a matrix of lists of vectors
  if(! yy %in% names(xx)) stop(sprintf('Data does not have element "%s"',yy));
  xxn <- intersect(xxnames,names(xx));
  if(length(setdiff(xxnames,xxn)>0)) warning('Not all xx columns exist');
  out <- sapply(xx[xxnames],e_table.default,yy=xx[[yy]],...);
  # now we merge them into three tables of identical dimension named count, 
  # prop, and stat, with one row for every xx
  # 
  out <- sapply(rownames(out),function(ii) {
    oo<-do.call(rbind,out[ii,]);rownames(oo)<-colnames(out);oo},simplify=F);
  class(out) <- 'e_table';
  out;
}

print.e_table <- function(xx,fmt,cfn=identity,pfn=function(xx) 100*xx
                          ,sfn=identity,usepander=exists('pander')
                          ,nobreaks=!usepander,panderstyle='multiline'
                          ,keep.line.breaks=!nobreaks
                          ,...){
  if(missing(fmt)) {
    fmt <- paste('%3s (%4.1f %%)','%4.1f'
                 ,sep=if(!nobreaks) "\\\n" else ' ');}
  #message(fmt);
  out <- with(xx,mprintf(fmt,cfn(count),pfn(prop)
                         ,sfn(stat)));
  if(nobreaks) colnames(out) <- gsub('\\\n',' ',colnames(out));
  # There is something wierd going on with pander being called within a pipeline
  # or, like in this case, within another function. Turns out you have to cat()
  # the first element of whatever it is pander returns (just if it's running
  # in a knitr environment or something?). Going to try to include the cat()
  # hack here and see if it doesn't break output for interactive sessions.
  if(usepander) cat(pander(out,style=panderstyle
                           ,keep.line.breaks=keep.line.breaks,...)[1]) else {
                             print(as.data.frame(out
                                                 ,check.names=F
                                                 ,stringsAsFactors=F))};
}

`[.e_table` <- function(dat,...,drop=FALSE){
  dots <- as.list(match.call(expand.dots=T)[-(1:2)]);
  rows <- if(identical(as.character(dots[[1]]),'')) TRUE else {
    eval(dots[[1]],envir=dat)};
  cols <- if(identical(as.character(dots[[2]]),'')) TRUE else {
    eval(dots[[2]],envir=dat)};
  out <- lapply(dat,`[`,rows,cols,drop=drop);
  attributes(out) <- attributes(dat);
  out;
}

rownames.e_table <- function(dat,...){rownames(dat[[1]])};
colnames.e_table <- function(dat,...){colnames(dat[[1]])};
dimnames.e_table <- function(dat,...){dimnames(dat[[1]])};
dim.e_table <- function(dat,...){dim(dat[[1]])};
nrow.e_table <- function(dat,...){nrow(dat[[1]])};
ncol.e_table <- function(dat,...){ncol(dat[[1]])};

`colnames<-.e_table` <- function(dat,value,...){
  out <- lapply(dat,`colnames<-`,value);
  attributes(out) <- attributes(dat);
  out;
}

`rownames<-.e_table` <- function(dat,value,...){
  out <- lapply(dat,`rownames<-`,value);
  attributes(out) <- attributes(dat);
  out;
}

e_table <- function(xx,yy,...) UseMethod('e_table');

# string hacking ---------------------------------------------------------------
#' Fancy Span (or any other special formatting of strings)
#' 
fs <- function(str,text=str,url=paste0('#',gsub('[^_a-z]','-',tolower(str)))
               ,tooltip='',class='fl'
               # %1 = text, %2 = url, %3 = class, %4 = tooltip
               # TODO: %5 = which position 'str' occupies in fs_reg if 
               #       applicable and if not found, append 'str'
               ,template='[%1$s]: %2$s "%4$s"\n'
               # Turns out that the below template will generate links, but they
               # only render properly for HTML output because pandoc doesn't 
               # interpret them. However, if we use the markdown implicit link
               # format (https://pandoc.org/MANUAL.html#reference-links) we 
               # don't have to wrap links in anything, but we _can_ use fs()
               # with the new template default above to generate a block of 
               # link info all at once in the end. No longer a point in using
               # the fs_reg feature for this case, the missing links will be
               # easy to spot in the output hopefully
               #,template="<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>"
               ,dct=dct0,col_tooltip='colname_long',col_class='',col_url=''
               ,col_text='',match_col='varname',fs_reg=NULL
               ,retfun=cat
               #,fs_reg='fs_reg'
               ,...){
  # if a data dictionary is specified use that instead of the default values 
  # for arguments where the user has not explicitly provided values (if there
  # is no data dictionary or if the data dictionary doesn't have those columns,
  # fall back on the default values)
  if(is.data.frame(dct) && 
     match_col %in% names(dct) &&
     !all(is.na(dctinfo <- dct[which(dct[[match_col]]==str)[1],]))){
    if(missing(tooltip) && 
       length(dct_tooltip<-na.omit(dctinfo[[col_tooltip]]))==1) {
      tooltip <- dct_tooltip;}
    if(missing(text) && 
       length(dct_text<-na.omit(dctinfo[[col_text]]))==1) {
      text <- dct_text;}
    if(missing(url) && 
       length(dct_url<-na.omit(dctinfo[[col_url]]))==1) {
      url <- dct_url;}
    if(missing(class) && 
       length(dct_class<-na.omit(dctinfo[[col_class]]))==1) {
      class <- dct_class;}
  }
  out <- sprintf(template,text,url,class,tooltip,...);
  # register each unique str called by fs in a global option specified by 
  # fs_register
  if(!is.null(fs_reg)) {
    dbg<-try(do.call(options,setNames(list(union(getOption(fs_reg),str))
                                      ,fs_reg)));
    if(is(dbg,'try-error')) browser();
    }
  retfun(out);
}


#' ## Functions for RMarkdown and ggplot2
#' 
#' Return a commit hash (for inclusion in reports for example) after first making
#' sure all changes are committed and pushed
#' TODO: instead of auto-committing, error if uncommited changes, needs to be 
#' a deliberate process, otherwise we have tons of meaningless auto-commit
#' messages that will make future maintenance harder
#gitstamp <- function(production=getOption('gitstamp.production',T),branch=F
#                     ,whichfile='') {
#  br<- if(branch) system("git rev-parse --abbrev-ref HEAD",intern=T) else NULL;
#  if(production){
#    if(length(gitdiff<-system("git update-index --refresh && git diff-index HEAD --",intern = T))!=0){
#      stop(sprintf(
#      "\ngit message: %s\n\nYou have uncommitted changes. Please do 'git commit' and then try again."
#      ,gitdiff));};
#    hash<-system(sprintf("git push && git log --pretty=format:'%%h' -n 1 -- %s"
#                         ,whichfile),intern=T);
#    if(length(hash)==0) stop('
#The specified file is not part of a git repository. You need to do (for example):
#
#git add NAMEOFTHISSCRIPT.R && git commit -a -m "New script"
#
#...and then try again.');
#    return(c(br,hash));
#  } else return(c(br,'TEST_OUTPUT_DO_NOT_USE'));
#}

# data dictionary --------------------------------------------------------------
#' Thin wrapper for dct_mod, see below
dct_append <- function(varname_,c_lists=c(),...,NONEWCOLS=TRUE,NOCOERCE=TRUE
                       ,file=dctfile_tpl
                       ,readfun=function(xx) tread(xx,read_csv,na='')
                       ,writefun=function(xx) write_csv(xx,file,na='')){
  condition<-substitute(varname==varname_);
  #namevals <- list(varname=varname_,...);
  return(dct_mod(varname_=varname_,c_lists=c_lists,varname=varname_,...
                 ,UNIQUE=TRUE,NONEWCOLS=NONEWCOLS,NOCOERCE=NOCOERCE,NOMULTI=TRUE
                 ,NEWROWS=TRUE,file=file,readfun=readfun,writefun=writefun));
}

#' TODO: document this function
dct_mod <- function(colsuffix_,colname_long_,varname_,condition=TRUE,c_lists=c()
                    ,...,UNIQUE=TRUE,NONEWCOLS=TRUE,NOMULTI=TRUE,NOCOERCE=TRUE
                    ,NEWROWS=FALSE,file=dctfile_tpl
                    ,readfun=function(xx) tread(xx,read_csv,na='')
                    ,writefun=function(xx) write_csv(xx,file,na='')){
  # build row condition
  if(!missing(colsuffix_)) {
    condition <- substitute(condition & colsuffix==colsuffix_);}
  if(!missing(colname_long_)) {
    condition <- substitute(condition & colname_long==colname_long_);}
  if(!missing(varname_)) {
    condition <- substitute(condition & varname==varname_);}
  # build c_lists
  lc <-length(c_lists <- grep('^c_',c_lists,val=T));
  namevals <- if(lc>0) setNames(as.list(rep_len(TRUE,lc)),c_lists) else list();
  namevals <- c(namevals,...);
  # read dctfile
  dct <- readfun(file);
  # invoke mutate_rows
  dctnew <- mutate_rows(dct,.condition=condition,.namevals=namevals
                        ,UNIQUE=UNIQUE,NONEWCOLS=NONEWCOLS,NOMULTI=NOMULTI
                        ,NOCOERCE=NOCOERCE,NEWROWS=NEWROWS);
  # if update succeeded, write out and also return modified version, backing
  # up the original
  if(!identical(dct,dctnew)){
    file.rename(file,paste(file_path_sans_ext(file),'dctbackup',file_ext(file),sep='.'));
    writefun(dctnew);
    return(dctnew);
    # otherwise return the unmodified version and do nothing else
  } else return(dct);
}

#' DONE: A df.update function that works like one of these 
#'       https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
#'       ...but with the option to add a brand new row if no existing are found
#'       (thus maybe making df.insert not so useful after all)
#' DONE: A wrapper for that one and external files, equivalent to the one for 
#'       df.insert()
#' DONE? :A function for rebuilding and reloading a data dictionary. Called once
#'       in data.R, but more importantly to be able to reload it during an 
#'       interactive session without having to re-run all of data.R
#' TODO: ...so that I can finally add the a_* created columns to the data
#'       dictionary in a clean manner
#' TODO: ...so that I can subset a_e_death and a_n_death and the to-be-created
#'       additional a_n_* variables for surgery.
#' TODO: ...so that I can have a tidy and understandable set of plots 
#'       demonstrating the behavior of the various tte variables.

#' Returns a list of column names from the data dictionary for which the column
#' named in the first argument is true. The first arg can be either a string or 
#' a name. The second must be a data.frame
#'
#' @param var        Either a string or a name, of a column in `dictionary`
#' @param dat        An optional data.frame, to constrain which rows of the 
#'                   'dictionary' object get used
#' @param retcol     Which column to return-- by default the same as used for 'matchcol'
#' @param dictionary A 'data.frame' that is used as a data dictionary. It must at 
#'                   minimum contain a column of column-names for the dataset for
#'                   which it is a data dictionary ('matchcol') and one or more 
#'                   columns each representing a _group_ of columns in the dataset, 
#'                   such that a TRUE or T value means the column whose name is 
#'                   the value of 'matchcol' is the name of a column in the data
#'                   that belongs to the group defined by the grouping column.
#'                   These grouping columns are what the argument 'var' is
#'                   supposed to refer to. We will use the convention that grouping
#'                   column names begin with 'c_' but this convention is not 
#'                   (currently) enforced programmatically.
v <- function(var,dat
              # DONE: let retcol take a vector argument
              ,retcol
              ,dictionary=dct0
              ,asname=F) {
  # convenience function: if forgot what column names are available, call with
  # no arguments and they will be listed
  if(missing(var)) return(names(dictionary));
  # support both standard or non-standard evaluation
  var<-as.character(substitute(var));
  # TODO: Think about what to do when nothing matches... not necessarily an error
  #       condition, might just be something to warn about and move on.
  out <- unique(as.vector(na.omit(unlist(dictionary[dictionary[[var]],retcol]))));
  # Why did I do the above in such a complicated way below? 
  #out<-dictionary[dictionary[[var]],retcol][[1]];
  # if something other than matchcol is returned, give it a name to make it 
  # easier to align with column names in the data
  #if(retcol != matchcol){
  #  out<-setNames(out,dictionary[dictionary[[var]],matchcol][[1]]);
  #}
  
  # 'na.omit()' needed because we allow the 'dictionary' object to have NAs instead
  # of FALSEs. 'c()' needed to strip na.omit metadata, so the output is a plain
  # old vector
  #out <- c(na.omit(out));
  # if a 'dat' argument is given, restrict the output so that only results having
  # having values found in the colnames of 'dat' are returned.
  #if(!missing(dat)) out <- out[out%in%colnames(dat)];
  if(!is(try(cnames<-colnames(dat),silent = T),'try-error')&&length(cnames)>0) {
    out <- out[out%in%cnames];}
  if(asname) out <- lapply(out,as.name);
  #return(unname(out));
  return(out);
  }

#' 
rebuild_dct <- function(dat=dat0,rawdct=dctfile_raw,tpldct=dctfile_tpl,debuglev=0
                        ,tread_fun=read_csv,na=''){
  out <- names(dat)[1:8] %>% 
    tibble(colname=.,colname_long=.,rule='demographics') %>% 
    rbind(tread(rawdct,tread_fun,na = na));
  if(length(na.omit(out$colname))!=length(unique(na.omit(out$colname)))){
    stop('Invalid data dictionary! Duplicate values in colname column');}
  out$colname <- tolower(out$colname);
  #dct0 <- subset(dct0,dct0$colname %in% names(dat0));
  shared <- intersect(names(dat),out$colname);
  out[out$colname %in% shared,'class'] <- lapply(dat0[,shared],class) %>% sapply(head,1);
  out$colsuffix <- gsub('^v[0-9]{3}','',out$colname);
  if(debug>0) .outbak <- out;
  # end debug
  out <- left_join(out,tpl<-tread(tpldct,tread_fun,na=na)
                   ,by=c('colsuffix','colname_long'));
  # debug
  if(debug>0){
    if(nrow(out)!=nrow(.outbak)) 
      stop('Number of rows changed in dct0 after join');
    if(!identical(out$colname,.outbak$colname)) 
      stop('colname values changed in dct0 after join');
  }
  # find the dynamic named vars in tpl
  outappend <- subset(tpl,!varname %in% out$varname);
  # make sure same columns exist
  outappend[,setdiff(names(out),names(outappend))] <- NA;
  out <- rbind(out,outappend[,names(out)]);
  # end debug
  out$c_all <- TRUE;
  return(out);
}

# project specific -------------------------------------------------------------
#' ## Functions specifically for Kidney Cancer project
#' 
#' TODO: write a roxygen skel for this one
event_plot <- function(data,reference_event,secondary_event=NA
                       ,sort_by=reference_event,start_event='n_ddiag'
                       ,index='patient_num'
                       ,vars=setdiff(names(data)[sapply(data,is.numeric)]
                                      ,c(index,start_event))
                       ,main=sprintf('Time from %s to %s',start_event
                                     ,reference_event)
                       ,xlab=sprintf('Patients, sorted by %s',reference_event)
                       ,tunit=c('days','weeks','months','years')
                       ,lwds=c(1,1)
                       ,ylim=NA,xlim=NA,subset=TRUE,ylab=NA
                       ,frame.plot=F
                       ,type='l',cols=c('black','red'),ltys=1:2,log=''
                       ){
  # subset the data
  data <- subset(data,eval(subset));
  conv = switch(tunit<-match.arg(tunit)
                ,days=1,weeks=7,months=365.25/12,years=365.25);
  # subtract start_event and convert to time unit
  for(ii in vars) data[[ii]] <- (data[[ii]] - data[[start_event]])/conv;
  # sort by indicated column, if any
  if(!is.na(sort_by)) data <- arrange_(data,.dots=sort_by);
  # set ylab if unspecified
  if(is.na(ylab)) ylab <- sprintf('Time since %s, %s',start_event,tunit);
  # set ylim if unspecified
  if(is.na(ylim)) ylim <- range(data[,na.omit(c(reference_event
                                                ,secondary_event))],na.rm=T);
  if(is.na(xlim)) xlim <- c(0,nrow(data));
  plot(data[[reference_event]],type=type,ylim=ylim,main=main,xlab=xlab
       ,ylab=ylab,frame.plot=frame.plot,col=cols[1],lty=ltys[1]);
  if(!is.na(secondary_event)) lines(data[[secondary_event]],col=cols[2]
                                    ,type=type,lty=ltys[2]);
  return(data);
}

# take a vector with possibly missing or varying values, and standardize to one
# of several pre-defined values in order of priority
adjudicate_levels <- function(xx,levs=list(),...,DEFAULT=NA,MISSING=NA){
  xx <- unique(na.omit(xx));
  # If there are no values, return the default value
  if(length(xx)==0) return(MISSING);
  # otherwise, step through the levs list of values and return the first matched
  # note that it's okay for levs to contain name-value pairs like FOO='FOO'
  ll <- names(levs);
  while(length(ll)>0){
    if(xx %in% levs[[ll[1]]]) return(ll[1]) else (ll<-ll[-1]);
  }
  if(is.na(DEFAULT)) return(tail(xx,1)) else return(DEFAULT);
}

#' Wrapper for taking data in the form of age-at-event, subtracting a starting
#' event, truncating on one or more ending events, and generating a survival
#' curve while outputting a surv object.
survfit_wrapper <- function(dat,eventvars,censrvars,startvars,predvars='1'
                            ,formula=NA
                            ,default.censrvars=c('age_at_visit_days')
                            ,subs=patient_num %in% kcpatients.naaccr 
                            ,thrunique=5,thrsmsize=20,drop_pred=NA
                            ,eventfun=pmin,censrfun=pmin,startfun=pmin
                            # function that takes the (internally created) 
                            # 'event' and 'censr' variables and returns a T/F 
                            # vector to use as the censoring variable 'cc' in 
                            # the actual survfit formula
                            ,eventcensrfun=`<=`
                            ,followup=Inf,scale=1,unit=NA
                            ,plotfun=autoplot
                            ,plotargs=list(mark.time=T,conf.int.alpha=0.1
                                           ,surv.size=2)
                            ,main=NA,xlab=NA,ylab=NA
                            ,numeric_pred_breaks=3
                            ,plotadd=list(
                              guides(colour=guide_legend('')
                                     ,fill=guide_legend('')))
                            ,survfun=survfit,survargs=list()
                            # set to empty list in order to use fs() defaults
                            # set to NA or any other atomic value to disable
                            ,fsargs=alist(retfun=return
                                          ,col_tooltip = 'chartname'
                                          ,template="%4$s"),...){
  # get the name of the data object
  datname <- deparse(substitute(dat));
  # take the subset of dat that has relevant/valid records
  sbs <- substitute(subs);
  # No idea why subset(dat,subs) gives "object 'FOO' not found" when subs 
  # references column FOO which is in dat but this roundabout way works....
  dat <- dat[with(dat,eval(sbs)),];
  # collect the names of the tte vars and static vars
  # tte vars
  tvars0 <- na.omit(unique(c(eventvars,censrvars,default.censrvars,startvars)));
  tvars1 <- intersect(tvars0,names(dat));
  if(length(tvars0)!=length(tvars1)) warning(sprintf(
    'The variables %s were not found in %s'
    ,paste0(setdiff(tvars0,tvars1),collapse=', '),datname));
  # use startfun to make startvar out of startvars
  start <- do.call(startfun,c(dat[,intersect(startvars,tvars1)],na.rm=T));
  # subract start from eventvars and censrvars, we are now done with that var
  dat[,tvars1] <- dat[,tvars1] - start;
  # use eventfun/censfun (with followup) to create eventvar and censrvar
  event <- do.call(eventfun,c(dat[,intersect(eventvars,tvars1)],na.rm=T));
  censr <- pmin(
    do.call(censrfun
            ,as.list(dat[,intersect(c(censrvars,default.censrvars),tvars1)]))
    ,followup,na.rm=T);
  # create tt as pmin() of eventvar and censrvar and then scale
  # create cc as tt <= censor
  dat$cc <- eventcensrfun(event,censr);
  # NOTE: does it break anything for this to be pmin(event,followup)/scale?
  #       because it does seem to break stuff to have the whole competing risk
  #       (i.e. censr) be part of the pmin statment...  no, we need event to 
  #       truncate at censoring.
  dat$tt <- pmin(event,censr)/scale;
  # TODO: if they are numeric, bin them instead
  # remove the too-sparse levels of variables
  drop_pred <- na.omit(drop_pred);
  # if(length(unique(na.omit(ii)))<thrunique)
  for(ii in predvars) {
    if(!is.na(numeric_pred_breaks) && numeric_pred_breaks>0 && 
              is.numeric(dat[[ii]])){
      dat[[ii]] <- cut(dat[[ii]]
                       ,quantile(dat[[ii]]
                                 ,c(0,seq_len(numeric_pred_breaks))/
                                   numeric_pred_breaks,na.rm=T)
                       ,include.lowest = T)}
    iitab <- table(dat[[ii]]);
    dat[[ii]][dat[[ii]] %in% c(names(iitab)[iitab<thrsmsize],drop_pred)] <- NA;
  }
  # TODO: sanity-check predvars to make sure they exist 
  if(all(is.na(formula))) formula <- as.formula(paste0('Surv(tt,cc)~'
                                                       ,paste0(predvars
                                                               ,collapse='+')));
  # fit survfun
  sfit <- do.call(survfun,c(formula,list(dat),survargs));
  lstartvars <- startvars; leventvars <- eventvars;
  if(length(find('fs'))>0 && is.list(eval(fsargs))){
    lstartvars <- sapply(startvars,function(xx) do.call(fs,c(xx,fsargs)));
    lstartvars[str_length(lstartvars)==0] <- startvars;
    leventvars <- sapply(eventvars,function(xx) do.call(fs,c(xx,fsargs)));
    leventvars[str_length(leventvars)==0] <- eventvars;
  }
  if(any(c('...','main') %in% names(formals(plotfun)))) {
    if(is.na(main)){main <- sprintf('Time from %s to %s'
                                    ,paste0(lstartvars,collapse='/')
                                    ,paste0(leventvars,collapse='/'));}
    plotargs[['main']] <- main;}
  if(any(c('...','xlab') %in% names(formals(plotfun)))){
    if(is.na(xlab)){xlab <- 'Time';
    if(!is.na(unit)) xlab <- paste0('Time',' (',unit,')');}
    plotargs[['xlab']] <- xlab;} 
  if(any(c('...','ylab') %in% names(formals(plotfun)))){
    if(is.na(ylab)){
      ylab <- sprintf('Fraction without %s',paste0(leventvars,collapse='/'));}
    plotargs[['ylab']] <- ylab;}
  # create plot object
  splot <- do.call(plotfun,c(list(sfit),plotargs));
  # add plotadd to plot object
  if(!all(is.na(plotadd))) for(ii in plotadd) splot <- splot + ii;
  # return plot object and optionally surv object along with call
  out <- list(fit=sfit,plot=splot);
  attr(out,'call') <- match.call();
  attr(out,'fullcall') <- fullargs();
  invisible(out);
}

# complex task specific --------------------------------------------------------

#' Return a tableone object formatted just the way we like it
#' @param xx     A \code{data.frame} (required).
#' @param vars   Vector of variable names to pass to \code{CreatTableOne} (optional)
#' @param ...    Named expressions from which to create the strata variable
#'               for \code{CreatTableOne} (only tested for one variable)
stratatable <- function(xx,vars=NULL,...){
  nmx <- names(xx);
  xx <- transform(xx,...);
  mystrata <- setdiff(names(xx),nmx);
  res <- tableone::CreateTableOne(data=xx, vars= vars, strata=mystrata) %>% 
    print;
  # removed trimws() from below pipeline prior to as.numeric, seems not to be 
  # needed
  res[,'p'] %>%  gsub("<","", x=.) %>% as.numeric %>% p.adjust %>% 
    cbind(res[,1:2],padj=.) -> res;
  return(res);
}

survAUC<-function(Surv.rsp,Surv.rsp.new,lp,lpnew,times,time,lp0=0
                  ,nonstdSurv=c('OXS','Nagelk','XO')
                  ,...,FUNS){
  if(is.null(info<-getOption('survAUCinfo'))){
    # create a lookup table for these functions if one doesn't exist
    info<-sapply(ls(package:survAUC),get) %>% lapply(formals) %>%
      lapply(sapply,class) %>% lapply(bind_rows) %>% bind_rows(.id='fun');
    options(survAUCinfo=info);
  }
  if(!missing(FUNS)) info <- subset(info,fun %in% FUNS);
  args <- list();
  for(ii in names(match.call())[-1]) args[[ii]] <- get(ii);
  # create a value for 'time' if it's missing but there are 'times'
  if(!'time' %in% names(args) && 'times' %in% names(args)) args$time <- max(eval(args$times));
  # make sure lp0 is the right length (seriously people? You're going to make me
  # do this in R of all languages?). BTW, Surv.rsp.new not an error-- for all
  # functions requiring lp0, there is also that departure from standard names
  # and for those three Surv.rsp.new becomes Surv.rsp
  if(missing(lp0)&&!missing(Surv.rsp.new)) args$lp0 <- rep_len(0,nrow(args$Surv.rsp.new));
  # which arguments are we missing on our current invokation?
  if(!'lp'%in%names(args)) browser();
  # find the arguments that each function can accept
  allowedargs<-setNames(apply(info[,-1],1,function(xx) names(info)[-1][!is.na(xx)])
                        ,unlist(info[,1]));
  # find the names of the non-optional arguments for each function
  nonoptargs<- sapply(info$fun,function(ii) subset(info,fun==ii),simplify=F) %>% 
    lapply(function(jj) na.omit(names(jj)[jj=='name']));
  # based on the above, here are the functions we can run with the data we have
  # for the ones that are FALSE we will return NAs
  canrun <- lapply(nonoptargs,setdiff,names(args)) %>% sapply(length) == 0;
  # and filter it down to just the ones that are valid arguments for functions 
  # that can be run
  args2use <- sapply(names(canrun)
                     ,function(xx) if(canrun[xx])
                       args[intersect(allowedargs[[xx]],names(args))] else NA
                     ,simplify=F);
  # correct the inconsistent argument names in the functions names by the 
  # nonstdSurv variable
  for(ii in intersect(nonstdSurv,names(canrun[canrun]))) {
    args2use[[ii]][c('Surv.rsp','lp')] <- args[c('Surv.rsp.new','lpnew')];
  }
  # return output!
  invisible(sapply(names(args2use),function(xx) if(is.na(args2use[[xx]])) NA else {
    try(do.call(xx,args2use[[xx]]))}));
}

# prepare data for TABSIE Shiny app
mktabsie <- function(data,subsets=list(Full=T),pw
                     ,vars
                     ,filepath='.'
                     ,filename='survSave.rdata'
                     ,serverTitle='TABSIE'
                     ,serverStatement=bquote(h4("Welcome to TABSIE"))){
  serverData <- sapply(subsets,function(ii) subset(data[,vars],eval(ii)),simplify=F);
  serverDataDic <- names(serverData);
  serverHash <- digest::digest(pw,algo='sha512',ascii=TRUE);
  save(serverStatement,serverData,serverDataDic,serverTitle,serverHash
       ,file=paste0(filepath,'/',filename));
}

#' Plots in the style we've been doing (continuous y, discrete x and optionally z)
#' 
#' Instead of creating new tables for 'All', just set xx=T
#' 
#' To suppress plotting of a legend (but still use fill) set fill.name=NA
#' Likewise, to suppress printing y-axis labels make yy.name=NA
#' To merely omit printing a name, set the name in question to ''
autoboxplot <- function(pdata, xx, yy, zz, subset=T
                        , type=c('box','violin')
                        , title=sprintf('%s vs %s\n by %s',xx,yy,zz)
                        , xx.name=if(xx==TRUE) 'All' else xx, xx.breaks=if(xx==TRUE) xx else unique(pdata[[xx]])
                        , xx.labels=if(xx==TRUE) '' else xx.breaks
                        , yy.name=yy, yy.labels
                        , fill.name, fill.breaks, fill.labels
                        , counts=T
                        ,...){
  subset <- substitute(subset);
  plot_type <- switch(match.arg(type)
                      ,box=geom_boxplot(coef=100)
                      ,violin=geom_violin());
  pdata <- subset(pdata,subset=eval(subset));
  if(!missing(zz)){
    if(missing(fill.name)) fill.name <- zz;
    if(missing(fill.breaks)) fill.breaks <- unique(pdata[[zz]]);
    if(missing(fill.labels)) fill.labels <- fill.breaks;
    fill <- if(is.atomic(fill.name)&&is.na(fill.name)) scale_fill_discrete(guide=F) else {
      scale_fill_discrete(name=fill.name,breaks=fill.breaks,labels=fill.labels);
    }
  }
  if(missing(yy.labels)){
    yy.labels <- if(is.na(yy.name)) scale_y_continuous(name='',labels=NULL) else {
      scale_y_continuous(name=yy.name,labels = comma);
    };
  };
  out <- ggplot(data = pdata, aes_string(x = xx, y = yy)) + plot_type + yy.labels +
    scale_x_discrete(name=xx.name,breaks=xx.breaks,labels=xx.labels) +
    labs(title=title)
  if(!missing(zz)) out <- out + aes_string(fill=zz) + fill;
  if(counts){
    ccrds<-ggplot_build(out)$layout$panel_ranges[[1]];
    ann.label <- if(xx==TRUE && missing(zz)) nrow(pdata) else if(xx==TRUE){
      paste0(table(pdata[,zz]),collapse=' \t ') } else if(missing(zz)){
        paste0(table(pdata[,xx]),collapse=' \t ') } else {
          apply(table(pdata[,c(xx,zz)]),1,function(ii) paste0(ii[!ii%in%list(0,'')],collapse=' \t '));
        }
    #ann.label <- gsub('\\b0\\b','',ann.label);
    out <- out + annotate('text',x=ccrds$x.major_source,y=ccrds$y.range[1]
                          ,label=ann.label[ccrds$x.major_source]);
  }
  attr(out,'call') <- match.call();
  out;
}

#' From ... https://menugget.blogspot.com/2011/11/define-color-steps-for-colorramppalette.html#more
#' Manually shape the intervals of a color gradient
color.palette <- function(steps, n.steps.between=NULL, ...){
  if(is.null(n.steps.between)) n.steps.between <- rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps <- cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB <- matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] <- col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] <- vals
    }
  }
  
  new.steps <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(new.steps, ...)
  return(pal)
}


#' From ... http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) print(plots[[1]]) else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} 

# deprecated -------------------------------------------------------------------
#' Possibly obsolete?
countfrac <- function(xx,outcomes
                      # set to TOTAL in order to do _only_ the total
                      ,groupcols='rai_range',sortby=groupcols
                      # set to 'none' if don't want to sort
                      ,dir=c('desc','asc','none')
                      ,summfns=c(n=sum,frac=mean)
                      # set to '' to disable
                      ,totalrow='Total'){
  # callback, so this function can call itself even if it's renamed
  # or nameless, for purposes of rerunning to get the totals row
  if(totalrow!='') thisfn <- sys.function();
  oo <- xx;
  # is the function being invoked for the purpose of calculating a total row?
  if(doingtotalrow <- tolower(trimws(groupcols[1]))=='total'){
    oo[[groupcols]] <- totalrow;
  };
  oo <- group_by_(oo,groupcols);
  # we coerce everything to logical, whatever it might have been
  # but leave alone columns other than those specified in the 'outcomes' variable
  oo[,outcomes] <- mutate_all(oo[,outcomes],truthy);
  # Two different summary tables, the second one applies the same set of
  # functions to everything, and the first one is just a count that doesn't 
  # rely on any one specific column. So we do them separately and then cbind()
  oo <- cbind(summarise(oo,bin_n=n()),summarise_all(oo[,c(groupcols,outcomes)],summfns)[,-1]) %>%
    # creating cumul_count, as in the current code
    mutate(cumul_count=rev(cumsum(rev(bin_n))));
  # sort, if desired, just as in the current code
  if(!doingtotalrow) oo <- switch(match.arg(dir)
                                  ,none=oo
                                  ,desc=arrange_(oo,sprintf('desc(%s)',groupcols))
                                  ,asc=arrange_(oo,groupcols));
  # now we insure that the column order is the same as the order of the groupcols
  # argument-- first the grouping column name, then the 'bin_n' (used to be called
  # rai_n, but this isn't specific to RAI, could group by anithing, so renamed)
  # cumul_count...
  oo <- oo[,c(groupcols,'bin_n','cumul_count'
              # ...and then this: it pastes the suffixes as obtained from the names 
              # of the summfns argument but orders them in the same way as they were
              # given, rather than first the first summary function and then the second
              ,c(t(outer(outcomes,names(summfns),paste,sep='_'))))];
  if(!doingtotalrow&&totalrow!='') { 
    tot <- thisfn(xx,outcomes,groupcols=totalrow,summfns=summfns,totalrow='');
    names(tot)<-names(oo);
    tot[[groupcols]] <- totalrow;
    oo <- rbind(oo,tot);
  }
  attr(oo,'call') <- match.call();
  oo;
}

#'.This function will create a variable summary table that will provide 
#' the name of the variable, tells whether the variable is discrete or
#' continuous, Tukey's 5 number summary for continuousvariables, and
#' the number of observations for each category for discrete variables.
#' All the user need for this function is the data frame (df) and the
#' function will do the rest.
#' 
#' However, this is commented out because it's here just as a reminder 
#' for when this functionality is again needed and this function gets 
#' rewritten properly.
# variable_summary <- function(df){
#   #collecting the column names from the data frame:
#   all_names <- names(df)
#   #passing the column names to an 'sapply' function:
#   theresult <- sapply(all_names, summary_vector, df)
#   #reformatting the table:
#   theresult <- as.data.frame(t(theresult))
#   colnames(theresult) <-c('VariableName', 'VariableType', 'TotalCounts', 'NumberOfNAs', 'VariableDescription')
#   return(theresult)
# }
