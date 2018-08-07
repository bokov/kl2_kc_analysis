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

#' Take a data.frame or character vector and a vector of grep targets and return
#' the values that match (for data.frame, column names that match). If no 
#' patterns given just returns the names
#' @param xx A \code{data.frame} or character vector (required)
#' @param patterns A character vector of regexp targets to be OR-ed
grepor <- function(xx,patterns='.') {
  if(is.list(xx)) xx <-names(xx);
  grep(paste0(patterns,collapse='|'),xx,val=T);
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
cte <- function(...){
  event <- sign(pmax(...,na.rm=T));
  ifelse(event<0,0,ifelse(event==0,1,2))
}

#' Delete all the junk in your environment, for testing
clearenv <- function(env=.GlobalEnv) rm(list=setdiff(ls(all=T,envir=env),'clearenv'),envir=env);

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

getCall.data.frame <- getCall.gg <- function(xx) attr(xx,'call');


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

#' Returns a vector of column names that contain data elements of a particular type
#' as specified by the user: "integer","POSIXct" "POSIXt", "numeric", "character", 
#' "factor" and "logical". 
vartype <- function(dat, ctype) {
  xx <- unlist(sapply(dat, class));
  idx <- which(xx %in% ctype);
  res <- names(xx)[idx];
  return(res)
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

#' Return a tableone object formatted just the way we like it
#' @param xx     A \code{data.frame} (required).
#' @param vars   Vector of variable names to pass to \code{CreatTableOne} (optional)
#' @param ...    Named expressions from which to create the strata variable
#'               for \code{CreatTableOne} (only tested for one variable)
stratatable <- function(xx,vars=NULL,...){
  nmx <- names(xx);
  xx <- transform(xx,...);
  mystrata <- setdiff(names(xx),nmx);
  res <- tableone::CreateTableOne(data=xx, vars= vars, strata=mystrata) %>% print;
  res[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
    as.numeric() %>% p.adjust() %>% cbind(res[,1:2],padj=.) -> res;
  return(res);
}



#' Returns a list of column names from the data dictionary for which the column
#' named in the first argument is true. The first arg can be either a string or 
#' a name. The second must be a data.frame
#'
#' @param var        Either a string or a name, of a column in `dictionary`
#' @param dat        An optional data.frame, to constrain which rows of the 
#'                   'dictionary' object get used
#' @param matchcol   Optional column that maps the rows of 'dictionary' to the rows
#'                   of a 'data.frame' of interest
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
              ,matchcol='colname'
              ,retcol=matchcol
              ,dictionary=dct0) {
  # convenience function: if forgot what column names are available, call with
  # no arguments and they will be listed
  if(missing(var)) return(names(dictionary));
  # support both standard or non-standard evaluation
  var<-as.character(substitute(var));
  # if a 'dat' argument is given, restrict the output so that only results having
  # having values found in the colnames of 'dat' are returned.
  if(!missing(dat)) dictionary <- dictionary[dictionary[[matchcol]]%in%colnames(dat),];
  # TODO: Think about what to do when nothing matches... not necessarily an error
  #       condition, might just be something to warn about and move on.
  out<-dictionary[dictionary[[var]],retcol][[1]];
  # if something other than matchcol is returned, give it a name to make it 
  # easier to align with column names in the data
  if(retcol != matchcol){
    out<-setNames(out,dictionary[dictionary[[var]],matchcol][[1]]);
  }
  # 'na.omit()' needed because we allows the 'dictionary' object to have NAs instead
  # of FALSEs. 'c()' needed to strip na.omit metadata, so the output is a plain
  # old vector
  return(c(na.omit(out)));
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

