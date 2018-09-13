#' ---
#' title: "RMD Sandbox"
#' author: "Alex F. Bokov"
#' date: "09/13/2018"
#' css: production.css
#' ---
#' 
#' Note: in the YAML header, the `reference_docx` argument seems to get ignored
#' if it's at the top level.
#' 
#+ init, echo=FALSE, include=FALSE, message=FALSE
# init -------------------------------------------------------------------------
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
# a hack pending until we can separate the light and heavy parts of data.R 
# somehow
source('functions.R');
knitr::opts_chunk$set(echo = F,warning = F,message=F);
# Set default arguments for some functions
panderOptions('table.split.table',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','left');
.args_default_v <- formals(v);
formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));
# overview ---------------------------------------------------------------------
#' # Overview
#' 
#' This is a script for testing how how various RMarkdown features work without 
#' the overhead of running one of the main scripts.
#' 
#' # Headers 
#'
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
#' ## H2
#' 
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
#' ### H3
#' 
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
#' #### H4
#' 
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
#' ##### H5
#' 
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
#' ###### H6
#' 
#+ results='asis' 
cat(paste0(stri_rand_lipsum(3),collapse='\n\n'));
# links ------------------------------------------------------------------------
#' # Embedding URLs
#' 
#' What happens if I embed explicit HTML... `r "<a href='https://rpubs.com/bokov/kidneycancer#consistency-checks'>This Is A Link</a>"`
#' like that? If it's a char then it works, but note-- anchors to specific
#' sections of RPubs documents do not work because of their iframe thing 
#' presumably.
#' 
#' Does R embedding `r 123` even work? Hello, `r "R"` can you hear me?
#' 
#' Now let's try `sprintf`. Here comes a `r sprintf("<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>",'link with mousover text','https://rpubs.com/bokov/kidneycancer','fl','this is the tooltip')`, did you see it?
#' 
#' Trying out the new `fs()` function, here it is... `r fs('s_death',url='#appendix-iv-audit-trail',template="<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>")`
#' ...did it work?
#' 
#' Kind of, at least for HTML. For word, link not showing up. How about an 
#' [explicit](#appendix-iv-audit-trail 'This is a tooltip' 'What is this?') link will that work?
#' 
#' Explicit link works, why doesn't this? How about an `r "[explicit_02](#appendix-iv-audit-trail 'This is a tooltip!')"`
#' link?
#' 
#' Another link format [link format 01][format01_link]
#' 
#' And another [link format 02][]
#' 
#' Cool! We can add tooltips vin the footnotes. More efficient. But can we 
#' [generate][] the footnote [block][] programmatically?
#' 
#' Success!!!
#' 
#' ## Footnotes
#' 
#' [format01_link]: #embedding-urls "Tooltip format 01"
#' [link format 02]: #embedding-urls "Tooltip format 02"
#'
# programmatic_footnotes -------------------------------------------------------
#+ fnotes, results='asis'
#fs('programatically','#embedding-urls','Tooltip for cat',retfun = cat);
#cat('\n');
fs('block',url='#embedding-urls',tooltip='Tooltip for return',retfun = cat);
fs('nonexistant_footnote',url='#dunno',tooltip='You do not see me',retfun=cat);
cat('[generate]: #overview "Programmatic footnote!"\n\n');

#' 
#' 
# audit ------------------------------------------------------------------------
#' ## Appendix IV: Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
