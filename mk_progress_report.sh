#! /bin/bash
input=${1:-"exploration.R"};
output=${2:-"NONE"};
styletemplate=${3:-"nt_styletemplate.docx"};
if [ "$output" == "NONE" ];
    then output=$(echo $input|sed s/exploration/progress/g|sed "s/\.R$\|\.Rmd$/.docx/g");
    fi;
#pandoc -s -S --reference-docx $styletemplate $input -o $output;
R -e "library(rmarkdown);render('$input',output_file = '$output', output_format = word_document(reference_docx = '$styletemplate',keep_md=T));render('$input',output_file='$output',output_format=html_document(keep_md=T));";

