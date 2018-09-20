#' ---
#' title: "R functions for having an audit trail"
#' author: "Wilson, Bokov"
#' date: "03/21/2018"
#' ---
#' 
#' This is going to get called via getOption('trail',tinit())
#' The fields of trail are:
#' time = timestamp
#' type = type of information (file, rdata, info, gitstamp, seed, etc)
#' name = the name of that specific item, should be unique
#' value = Almost anything. Non atomic values should be assigned for example like this: 
#'   `trail[nrow(trail),'value'] <- list(list(mtcars))`
#'   For items of type 'file' the path will be stored here, for 'seed', the random seed, for 'gitstamp'
#'   the current branch.
#' hash = For 'file' and 'rdata' the md5 checksum of the file, for 'gitstamp' the commit hash
#' trail = For 'rdata', if a trail object exists, dump it into this field. And it can have its own trail objects
#'   so we have an arbitrarily long "family tree" of ancestor objects!
#' 
#' TODO: (maybe) find a way to pass 'non-production' status to scripts that
#'       get sourced by R to satisfy dependencies... or perhaps non-production
#'       should not be that convenient, perhaps it should be manually turned
#'       on and off for each file and it's better not to inherit it.
#' TODO: function to write out .trail as a JSON or XML file (to accompany 
#'       non-rdata saveouts)
#' TODO: modify tread() to check for the existance of a trail flat-file as above
#'       and read it if there is one. Then, treat it the same way
#'       that a .trail from a loaded rdata file.
#' TODO: modify tread() to see if the file being read is part of the repo and 
#'       document that fact, perhaps by the type being 'versioned-file' instead
#'       of 'file'
#' TODO: a twrite() function that is the inverse of tread() and creates an 
#'       accompanying flat-file of trail.
#' DONE: [priority] function to recursively print a .trail (to screen or to 
#'       pander)
#' DONE: wrap the repeating pattern of whichrecord, trail[whichrecord,], etc.
#'       into a function.
#' TODO: have gitstamp only do the non-production message if the non-production
#'       argument is TRUE _and_ the file isn't checked in
#' TODO: if tscript finds that the specified script is not part of the git repo
#'       then record the type as 'unversioned-script' rather than 'script'
#'       and save an md5 hash instead of a git-provided hash
#' TODO: [priority] ~~possibly rename tscript() to tself() and~~ have a separate 
#'       tsource() function specifically to wrap source
#' TODO: [priority] check if a MYSTERY_FILE with a matching hash already exists 
#'       and if it does, reuse the name
#'       
#' The below is an example of how a script can find out its own file-name 
#' (except if it is being interactively run)
currentscript <- parent.frame(2)$ofile;
if(is.null(currentscript)) currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';

#' 
#' `fingerprint()` is supposed to be a more flexible and robust way to get 
#' reproducibility info about a file, and allows the same syntax to be used on
#' any file whether it is tracked or not, and even if the file doesn't actually
#' exist. In the future I plan to rewrite `gitstamp()`, `tload()`, and `tread()`
#' to all use `fingerprint()` internally but as of right now they are independent
#' from `fingerprint()`
#' 
#' It returns a list with elements that include... 
#' 
#' * `in.repo`: whether or not the current working directory is a git repo
#' * `repo`: URL of the git repo, NA if `in.repo` is false
#' * `branch`: currently checked-out branch
#' * `branch.on.origin`: whether the currently checked-out branch has already 
#'   been pushed to origin... because that's not a given and we don't want this
#'   scenario preventing reproduceability from taking us by surprise when we
#'   least expect it
#' * `last.branch.commit`: the hash of whatever is currently checked out
#' * `uncommitted.changes`: whether or not there are uncommited changes to 
#'   any tracked files
#' * `file.tracked`: if the optional `filename` argument is used, whether or not
#'   that file is being tracked by git.
#' * `file`: the value of the optional `filename` argument.
#' * `file.hash.or.last.commit`: if `file.tracked` then the hash for the last
#'   commit THAT INCLUDED CHANGES TO `file` (which is not necessarily the
#'   same thing as `last.branch.commit` if the file has not been changed during
#'   the most recent commit). Otherwise, it's the `md5sum()` of the file. If the
#'   file doesn't exist, this value is `NA`.
#' * `timestamp`: when `fingerprint()` ran (doesn't tell you anything about the
#'   file or the repo themselves).
fingerprint <- function(filename=NA) {
  # in case something passes an empty filename somehow
  filename <- c(filename,NA)[1];
  # are we in a repo?
  inrepo <- suppressWarnings(
    isTRUE(
      'true'==system("git rev-parse --is-inside-work-tree"
                     ,intern=T,ignore.stderr=T)));
  # initialize the fhs to be NULL
  frp <- fhs <- hsh <- brn <- brn.ogn <- ogn <- NA;
  if(inrepo){
    frp <- filename %in% system('git ls-files',intern = T);
    if(frp) suppressWarnings(
      fhs <- c(system(paste0('git log -n 1 --pretty="format:%h" -- ',filename)
                      ,intern=T),NA)[1]);
    # what hash are we on?
    hsh <- system("git log --pretty=format:'%h' -n 1",intern=T);
    # what branch are we on? NOTE: don't know yet what happens in wierd cases,
    # e.g. detached head, or stash or whatever... might return something 
    # unexpected and error in a subsequent command, be aware
    brn <- system("git rev-parse --abbrev-ref HEAD",intern=T);
    # origin (i.e. what upstream repo does this project use for pushes/pulls)
    ogn <- system('git config --get remote.origin.url',intern=T);
    # does this branch exist on origin?
    brn.ogn <- any(grepl(paste0('/',brn,'$'),system("git branch --remote",intern=T)));
    # are there uncommitted changes?
    needupd <- suppressWarnings(
      system("git update-index --refresh && git diff-index HEAD --",intern = T));
    
  }
  # if for whatever reason git hash not available for a file, try to calculate 
  # an md5sum(). If file doesn't exist (including NA) then this outputs 'NA'
  if(is.na(fhs)) fhs <- unname(tools::md5sum(c(na.omit(filename),'')[1]));
  return(list(
    in.repo=inrepo,repo=ogn,branch=brn,branch.on.origin=brn.ogn
    ,last.branch.commit=hsh
    ,uncommited.changes=if(inrepo) length(needupd)>0 else NA
    ,file.tracked=frp,file=filename,file.hash.or.last.commit=fhs
    ,timestamp=Sys.time()));
}



#' Return a commit hash (for inclusion in reports for example) after first making
#' sure all changes are committed and pushed
gitstamp <- function(production=T,branch=T) {
  br<- if(branch) system("git rev-parse --abbrev-ref HEAD",intern=T) else NULL;
  if(production){
    if(length(gitdiff<-system("git update-index --refresh && git diff-index HEAD --",intern = T))!=0) stop(sprintf(
      "\ngit message: %s\n\nYou have uncommitted changes. Please do 'git commit' and then try again."
      ,gitdiff));
    c(br,system("git push && git log --pretty=format:'%h' -n 1",intern=T));
  } else return(c(br,'TEST_OUTPUT_DO_NOT_USE'));
}

# this creates a trail object if there isn't already one and either way 
# returns a trail object
tinit <- function(trail=getOption('trail'),...){
  if(is.null(trail)) {
    trail<-data.frame(sequence=sprintf('%04d',1),time=Sys.time(),type='info',name='sessionInfo',value=NA,hash=NA,trail=NA,stringsAsFactors=F); 
    trail$value <- list(sessionInfo());
    options(trail=trail);
  }
  return(trail);
}

#' The function that updates the trail object with a new row of data
tupdate <- function(type=NA,name=NA,value=NA,hash=NA,time=Sys.time()
                    ,trail=getOption('trail',tinit()),whichrecord=nrow(trail)+1
                    ,parent.trail=NA){
  seq <- sprintf('%04d',whichrecord);
  trail[whichrecord,] <- c(sequence=seq,time=NA,type=type,name=name,value=NA
                           ,hash=hash,trail=NA);
  trail[whichrecord,'time'] <- time;
  if(!is.atomic(value)||length(value)>1) value <- list(value);
  trail[whichrecord,'value'] <- value;
  if(!is.atomic(parent.trail)||length(parent.trail)>1) {
    parent.trail <- list(list(parent.trail));
  }
  trail[whichrecord,'trail'] <- parent.trail;
  options(trail=trail);
  invisible(trail);
}

#' Recursively collect nested trail dataframes and rbind them all together
#' with a sequence column which will sort them in a way that preserves their
#' hierarchical relationship
walktrail <- function(trail=tinit(),prepend='',seqcol=names(trail)[1]
                      ,nestingcol=tail(names(trail),1),sep='.'){
  fn <- sys.function();
  trail[[seqcol]] <- paste0(prepend,trail[[seqcol]]);
  out <- trail[,setdiff(names(trail),nestingcol)];
  for(ii in nested <- which(sapply(trail[[nestingcol]],is.data.frame))){
    out<-rbind(out,fn(trail[[nestingcol]][ii][[1]]
                      ,prepend=paste0(trail[[seqcol]][ii],sep)
                      ,seqcol = seqcol,nestingcol = nestingcol));
  }
  class(out) <- c('walktrail',class(out));
  return(out);
}

print.walktrail <- function(xx
                            ,sub=c('sequence','time','type','name_value','hash')
                            ,...){
  sub <- sub[sub %in% c('name_value',names(xx))];
  mutate(xx
         ,name_value=paste(name
                           ,name_value=ifelse(type=='info','...',value),sep='='))[,sub];
}

summary.walktrail <- print.walktrail;

# script registering itself... adds a gitstamp and its own name to trail
tself <- function(scriptname=parent.frame(2)$ofile
                  ,production=getOption('gitstamp_prod',T)){
  if(is.null(scriptname)) scriptname <- 'INTERACTIVE_SESSION';
  gs <- gitstamp(production=production,branch=T);
  tupdate('this_script',name=scriptname,value=gs[1],hash=gs[2]);
}

#' setting and recording the random seed
tseed <- function(seed,...){
  seedname <- deparse(match.call()$seed);
  set.seed(seed,...);
  tupdate('seed',name=seedname,value=seed);
}

#' loading an rdata file and checking whether it has trail, to include in the 
#' the current trail as a nested data.frame
tload <- function(file,envir=parent.frame()
                  ,verbose=FALSE,trailobj='.trail'){
  if(trailobj %in% ls(envir,all=T)) stop(sprintf('
The object %s already exists, perhaps due to one of the trail-related functions 
crashing. Please try again in clean environment.',trailobj));
  filename <- deparse(match.call()$file);
  filehash <- tools::md5sum(file);
  out<-load(file,envir,verbose);
  if(trailobj %in% ls(envir,all=T)){
    ptrail <- envir[[trailobj]];
    rm(list=trailobj,envir=envir);
  } else ptrail <- 'NO_TRAIL_FOUND';
  tupdate('rdata',name=sprintf('%s = "%s"',filename,file)
          ,value=file,hash=filehash,parent.trail = ptrail);
  return(out);
}

#' wrapper for most read functions, and records the file and its hash in trail.
#' Will eventually also check for accompanying flat-file trail files in JSON
#' format.
tread <- function(file,readfun,...){
  filename <- deparse(match.call()$file);
  filehash <- tools::md5sum(file);
  loaded <- readfun(file,...);
  tupdate('file',name=sprintf('%s = "%s"',filename,file)
          ,value=file,hash=filehash);
  return(loaded);
}

#' Wrapper for save(). Pulls trail out of options as a data.frame, saves the
#' data.frame along with whatever was originally going to be saved, and then 
#' deletes it. Logs the save to itself before saving.
tsave <- function(...,list=character(),envir=parent.frame(),trailobj='.trail'
                  ,verbose=TRUE){
  # add another sessionInfo() entry to trail
  tupdate('info',name='sessionInfo',value=sessionInfo());
  val <- deparse(match.call());
  # update with the actual save entry and put the trail object in the environment
  envir[[trailobj]] <- tupdate('save',name='save',value=val);
  # save with the args as given
  if(verbose) message('tsave is saving to file...');
  savetime<-system.time(save(...,list=c(trailobj,list),envir=envir));
  if(verbose) message(sprintf('...tsave is done saving. Time elapsed: %f')
                      ,savetime[3]);
  # remove the trailobj
  rm(list = trailobj,envir = envir);
}

#' A script for testing these functions;
#
# source('./trailR.R');
# currentscript <- parent.frame(2)$ofile;
# if(is.null(currentscript)) currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
# tself(scriptname=currentscript);
# tsave(mtcars,file='junktestsave.rdata');

