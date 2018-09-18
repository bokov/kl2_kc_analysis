#' ---
#' title: "RMD Sandbox"
#' author: "Alex F. Bokov"
#' date: "09/13/2018"
#' css: production.css
#' toc: true
#' ---
#' 
#+ echo=FALSE
#' Note: in the YAML header, the `reference_docx` argument seems to get ignored
#' if it's at the top level.
#' 
#+ init_chunk, echo=FALSE, include=FALSE, message=FALSE
# init -------------------------------------------------------------------------
#' Things that do not work in YAML header.
#' 
#' Ignored:
# variant: markdown+fenced_divs
#' Error: `pandoc: Unknown extension: fenced_divs`
# md_document:
#   variant: markdown+fenced_divs
#' Works but Rstudio builds the first item on the list, does not prompt, 
#' annoying
# output:
#  html_document:
#    keep_md: yes
#  word_document:
#    keep_md: yes
#
#+ init, echo=FALSE, include=FALSE, message=FALSE
# if running in test-mode, uncomment the line below
options(gitstamp_prod=F);
options(markdown.extensions=c(markdown::markdownExtensions(),'fenced_divs'));
.junk<-capture.output(source('global.R',echo=F));
.depends <- 'dictionary.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- knitr::current_input();
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
project_seed <- 20180803;
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"'
                                          ,gsub('\\.light','',.depends)));
.loadedobjects <- tload(.depdata);
# a hack pending until we can separate the light and heavy parts of data.R 
# somehow
source('functions.R');
knitr::opts_chunk$set(echo = F,warning = F,message=F);
rmarkdown::pandoc_toc_args(TRUE,toc_depth=3);
# Set default arguments for some functions
panderOptions('table.split.table',Inf);
panderOptions('missing','-');
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','left');
.args_default_v <- formals(v);
#formals(v)[c('dat','retcol')]<-alist(dat1,c('colname','varname'));
# overview ---------------------------------------------------------------------
#' # Overview
#' 
#' This is a script for testing how how various RMarkdown features work without 
#' the overhead of running one of the main scripts.
#' 
#' 
# definitions and divs ---------------------------------------------------------
#' 
#' [Here is](#fooid1) a link to the first span below and here is a link to the 
#' [second one](#fooid2). Another link to [the second][ref_fooid2] one using the ref
#' link syntax.
#' 
#' Pandoc definitions, here we go.
#' 
#' Term 1
#'   ~ Definition 1 `r stri_rand_lipsum(1)`
#'
#' Term 2 
#'   ~ Definition 2a `r stri_rand_lipsum(1)`
#'   ~ Definition 2b `r stri_rand_lipsum(1)`
#' 
#' Term 3
#'   ~   Indented paragraph `r stri_rand_lipsum(1)`
#'   ~   Another indented paragraph `r stri_rand_lipsum(1)`
#'   
#' Term 4
#' 
#' : Definition 4a `r stri_rand_lipsum(1)`
#' 
#' : Definition 4b `r stri_rand_lipsum(1)`
#'   
#'   
#' Fenced divs...
#' 
#' ::::: {#special .sidebar}
#' Here is a paragraph.
#'   
#' And another.
#' :::::
#' 
#' Okay, that didn't work. Another approach: spans.
#' 
#' This is [a span] without any particular link. This is [another span]{#fooid1 .barclass}
#' that has a class and an ID. This is a [span with custom style]{custom-style="foostyle"}
#' and [this is]{#fooid2 .barclass custom-style="foostyle"} a span with both.
#' 
#' Spans work for class and Word style but only work as anchors in HTML, not in 
#' Word. I wonder if I can make a header have a custom style.
#' 
#' ###### Hi, I'm an annoying header {#annoying .annoyingclass custom-style="foostyle"}
#' 
#' Stuff under an annoying header.
#' 
# list_tables ------------------------------------------------------------------
#' # Lists in tables
#' 
#' Credit: https://stackoverflow.com/a/19953345
#+ tablewlist0, results='asis'
cat('
+---------------+---------------+--------------------+
| Fruit         | Price         | Advantages         |
+===============+===============+====================+
| Bananas       | $1.34         | - built-in wrapper |
|               |               |     - sub-item 1 |
|               |               |     - sub-item 2 |
|               |               | - bright color     |
+---------------+---------------+--------------------+
| Oranges       | $2.10         | - cures scurvy     |
|               |               | - tasty            |
+---------------+---------------+--------------------+
    ');
#' Now let's try the actual table I want to do...
#+ tablewlist1, results='asis'
cat('
+---------------+--------------------+
| Bananas Lorem ipsum dolor sit amet, augue vulputate venenatis sed a turpis sociis, blandit enim in magnis non. Varius ornare, ultricies libero mi, parturient, faucibus volutpat lobortis. Eu sed egestas|                    |
|               | - built-in wrapper |
|               |     - sub-item 1   |
|               |     - sub-item 2   |
|               | - bright color     |
+---------------+--------------------+
    ');
#' Nope, not quite. How about this, though...
#+ tablewlist2a, results='hide'
#.temp <- cbind(stri_rand_lipsum(1),"- built-in wrapper\n____- sub item 1\n____- sub-item 2\n- bright color\n") %>% 
#  pander_return(style='grid',keep.line.breaks=T,justify=' left') %>% 
#  gsub('_',' ',.);
#save(screwedup,file='screwedup.rdata');
#' Maybe inline? `cat(.temp,sep='\r\n');` ...did that work?
#' Nope
#+ tablewlist2b, results='asis'
cat("+--------------------------------+--------------------+",'\n');
cat("| Lorem ipsum dolor sit amet,    | - built-in wrapper |",'\n');
cat("+--------------------------------+--------------------+",'\n');
cat('\n\n');
#substr(.temp[1],8,str_length(screwedup[1])-1) %>% cat;
#cat('\n\nWith gsub\n');
#substr(.temp[1],8,str_length(.temp[1])-1) %>% gsub('\\\\n','\n',.) %>% cat;
#for(ii in .temp) cat(ii);
#' No. This?
#+ tablewlist3, results='asis'
cat('
+--------------------------------+--------------------+
| Lorem ipsum dolor sit amet,    | - built-in wrapper |
| tincidunt sem lorem eleifend   |     - sub item 1   |
| enim. Luctus tellus est in     |     - sub-item 2   |
| ante et. Magna habitant non.   | - bright color     |
| Quam nec sociosqu eu,          |                    |
| inceptos, in, suspendisse      |                    |
| elementum. Placerat nostra     |                    |
| curae a feugiat curae, in hac  |                    |
| et. In in ligula netus justo   |                    |
| porta purus. Netus ultricies   |                    |
| elit vulputate efficitur       |                    |
| fusce, sociosqu arcu in a      |                    |
| elit. Nulla vestibulum a.      |                    |
| Tempus ligula mauris molestie  |                    |
| integer primis vitae. Mauris   |                    |
| et nibh, amet gravida ornare   |                    |
| dui. Dolor donec diam tellus   |                    |
| sociosqu ipsum dapibus vel     |                    |
| vitae felis. Vitae eros sociis |                    |
| ac eros platea. Ac non erat    |                    |
| lectus pulvinar eu nulla a     |                    |
| egestas ut sed. Massa torquent |                    |
| nam imperdiet tincidunt, at    |                    |
| nullam.                        |                    |
+--------------------------------+--------------------+
    ');
#' 
#' The one above is a copy-paste of the console output of the programmatically 
#' generated one prior to it. But why does this one render and that one not?
#' 
#+ tablewlist4, results='asis', cache=FALSE
.temp0 <- cbind(stri_rand_lipsum(1),"- built-in wrapper\n____- sub item 1\n____- sub-item 2\n- bright color\n");
.temp2<-capture.output(.temp1<-pander(.temp0,style='grid',keep.line.breaks=T,justify='left') %>% gsub('_',' ',.));
cat('\nTemp1:\n\n');
cat(.temp1[1]);
cat('\n\n\n\nTemp2:\n\n');
cat(.temp2);
#' Temp1 wins! Let's try it in a cleaned up final state...
#+ tablewlist5, results='asis'
note <- c("
This is not (yet) a manuscript. We are still at the data cleaning/alignment
stage and it is far too early to draw conclusions. Rather, this is a
regularly updated progress report that I am sharing with you to keep you in
the loop on my work and/or because you are also working on NAACCR, i2b2, Epic,
or Sunrise and this might be useful to you or you might wish to offer advice.\\
\\
So far, only de-identified data has been used to generate these results and
any dates or `patient_num` values you see here are also de-identified, though
the time intervals between events are not distorted.\\
\\
At this time the analysis of de-identified data is under Dr. Michalek's
exempt project IRB number HSC20170563N. I have been given guidelines under
which we can share the de-identified data with UTHSCSA collaborators. If you
would like a copy of the data, please email me and I will get back to you
with further instructions and any additional information I might need from
you for our records. The following versions of the dataset are available:"
          ,"
- Raw: the data as it literally exists when I input it into my scripts.
- Lightly curated: the main dataset as it is after my scripts are done processing it
- Moderately curated: the dataset pared down to just the columns and rows
  currently being used for analysis"
          ,"
Dr. Murphy, if you are interested in a copy of the data, I'll talk to my
local mentors and IRB about the best way to do that. It's probably time we
start talking about what approvals in general will be necessary for the full
project. In case you are wondering, I am doing parts of Aim 2 ahead of Aim 1
because it will help me identify the need for any additional recurring  data-transformation rules to incorporate into DataFinisher all at once. I  will switch to Aim 1, the i2b2 plugin, once I hit a natural pausing-point on Aim 2."
);

toc <- rep_len(NA,length(note));
toc[1] <- "
- [Overview](#overview)
- Experimentation
____- [Lists in Tables](#lists-in-tables)
____- [Headers](#headers)
____- [Links](#embedding-urls)
________- [Footnotes](#footnotes)
- [Audit Trail](#appendix-i-audit-trail)
";
.temp0 <- cbind(note,toc) %>% unname;
.temp1<-pander(.temp0,style='grid',keep.line.breaks=T,justify='left'
               ,split.cells=c(30,Inf),missing='')[1] %>% 
  gsub('_',' ',.);
cat('\n\n***\n\nThe Real Table\n\n')
cat(.temp1);
#'
# headers ----------------------------------------------------------------------
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
#' [ref_fooid2]: #fooid2
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
#' ## Appendix I: Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
