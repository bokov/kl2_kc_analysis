#' ---
#' title: "RMD Sandbox"
#' author: "Alex F. Bokov"
#' date: "09/13/2018"
#' css: production.css
#' toc: true
#' output:
#'  html_document:
#'   keep_md: true
#'   css: production.css
#'   pandoc_args: [
#'     "--filter", "pandoc-crossref"
#'   ]
#'  word_document:
#'   reference_docx: 'nt_styletemplate.docx'
#'   keep_md: true
#'   pandoc_args: [
#'     "--filter", "pandoc-crossref"
#'   ]
#'  pdf_document:
#'   keep_md: true
#' ---
#' 
#+ init, echo=FALSE, include=FALSE, message=FALSE
# initialize -------------------------------------------------------------------
# if running in test-mode, uncomment the line below
options(gitstamp_prod=F);
.junk<-capture.output(source('global.R',echo=F));
# set the font
default_font <- 'Times New Roman';
.depends <- 'dictionary.R';
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
knitr::opts_chunk$set(echo = F,warning = F,message=F
                      ,dev.args=list(family=default_font));
knitr::opts_template$set(
  fig_opts=alist(fig.cap=get0(knitr::opts_current$get("label"))));
#rmarkdown::pandoc_toc_args(TRUE,toc_depth=3);
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
#' 
#' 
#' This is a script for testing how how various RMarkdown features work without 
#' the overhead of running one of the main scripts.
# header_notes -----------------------------------------------------------------
#' # Header notes
#' Stuff that does not work at the top level of the YAML header (i.e. outside 
#' individual formats):
#' 
#' * `pandoc_args`
#' * `keep_md`
#' * `reference_docx`
#' 
#' 
# params ----------------------------------------------------------------------
#' # Params & Metadata
#' 
#' Trying to print out the current YAML `r thisenv<-environment(); save.image('foo.rdata'); 'params'`...
cat('Here goes...\n\n');
if(exists('params')) pander(params) else cat('No params\n');
if(exists('input')) pander(input) else cat('No input\n');
if(exists('thisenv')) pander(ls(thisenv)) else cat('No thisenv\n');
#' ## knitr options
#' 
#' `opts_template`
#+ opts_template
pander(knitr::opts_template$get());
#' `opts_current`
pander(knitr::opts_current$get());
#' `opts_knit`
pander(knitr::opts_knit$get());
#' `opts_hook`
#+ opts_hook
baz<-try(pander(knitr::opts_hooks$get()));
#' `opts_chunk`
#+ opts_chunk
pander(knitr::opts_chunk$get());

# fenced_divs ------------------------------------------------------------------
#' # Fenced divs
#' 
#' ::::: {.sidebar #fig:fenced custom-style="Image Caption"}
#' Here is a paragraph.
#' 
#' Lorem ipsum dolor sit amet, tincidunt sem lorem eleifend enim. Luctus tellus
#' est in ante et. Magna habitant non. So, the last paragraph gets treated as a 
#' caption?
#' 
#' And another.
#' 
#' The last paragraph is the caption? But it isn't wrapped in figcaption or
#' caption tags. And starting with a leading `:` does casues the text above it 
#' to be bold but does not turn it into a caption.
#' 
#' :::::
#' 
#' Can I wrap a code chunk in it?
#' 
#' ::::: {.sidebar #fig:codeplot0 custom-style="Image Caption"}
#+ codeplot0,results='asis'
#,fig.cap='This is going to be the figure caption.'
cat('Hi, this is yet another caption attempt.');
plot(rnorm(10),rnorm(10),type='b',main='Random Plot 0a');

#' :::::
#' 
#' ~~No, that doesn't work.~~ Works, just needed to skip a space before the 
#' opening fence. Maybe the fences need to be in the code?
#' 
#+ codeplot1,id='#fig:codeplot1a',results='asis',caption='This is going to be another figure caption.'
cat("\n\n::::: {.sidebar #fig:codeplot1 fig.caption='asdfasd sadfafds' caption='No I am the caption' custom-style='Image Caption'}\n\n")
plot(rnorm(10),rnorm(10),type='b',main='Random Plot 1');
#cat("\n\n: The real caption. \n{#fig:codeplot1}\n\n");
cat("\n Third caption attempt")
cat("\n\n:::::\n\n");
#'
#' 
#' ::::: {#fig:codeplot2 custom-style="Image Caption"}
#+ codeplot2,results='asis'
plot(rnorm(10),rnorm(10),type='b',main='Random Plot 2b');
cat('\n\nOverall caption');
#' :::::
#' 
#' Can I just use a plain old H7?
#' 
#' ####### codeplot3 {#fig:codeplot3 custom-style="Image Caption"}
#+ codeplot3,results='asis'
plot(rnorm(10),rnorm(10),type='b',main='Random Plot 3');
cat('\n\nCaption for random plot 3');
#' 
# crossref ---------------------------------------------------------------------
#' 
#' # Referencing tables and figures.
#' 
#' Trying to cite [@Tbl:footab] . Did it work? Yes! Also see the top-level YAML 
#' headers. How about multiple tables, such as [@tbl:bananas;@tbl:footab]. Can
#' also reference one of the tables without a prefix (i.e. just [-@tbl:footab])
#' or with a one-off custom prefix like [TABLE @tbl:footab].
#' 
#' Note: http://lierdakil.github.io/pandoc-crossref/ was very helpful in sorting
#' all this out.
#' 
#' Now referencing [@fig:mtcars]. Link doesn't work in Word. How about if we 
#' pretend that a div is a figure, such as [@fig:fenced]?
#' 
#' Well, the link works, but in that one the target doesn't get proper caption
#' styling. But what about [@fig:codeplot0]? Or [@fig:codeplot1].
#' 
#' Referencing the one that seems to be linking to a div rather than a figure
#' [@fig:codeplot2]. On the other hand the one that is an H7 is here 
#' [@fig:codeplot3]. There is a target created for it, but its link is broken.
#' 
#' **...and the winner for docx compatibility is [@fig:codeplot2]**. Though all 
#' of these [@fig:codeplot0;@fig:codeplot1;@fig:fenced] do work, the captions 
#' are jacked in various ways.
#' 
#' # Referencing figures
#' 
#' Writing the caption which should be invisible...
.iris_plot <- "This is a plot of 
iris data. {#fig:iris}"
#' Now the figure...
#+ .iris_plot, opts.label='fig_opts'
plot(iris);
#' Does not work! Reference not created.
.mtcars_plot <- "This is a plot of mtcars, no fancy stuff in the caption"
#' How about this?
#+ .mtcars_plot, opts.label='fig_opts', results='asis'
plot(mtcars[,3:5]);
cat('{#fig:mtcars}\n');

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

: Bananas and oranges caption. {#tbl:bananas}');
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
cat("\n: Foo bar baz {#tbl:footab}");
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
#' ####### H7
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
#' ## Appendix I: Audit trail
walktrail()[,-5] %>% pander(split.tables=600,,justify='left');
