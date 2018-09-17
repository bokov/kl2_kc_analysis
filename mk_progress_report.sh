#! /bin/bash
input=${1:-"exploration.docx"};
output=${2:-"NONE"};
styletemplate=${3:-"nt_styletemplate.docx"};
if [ "$output" == "NONE" ];
    then output=$(echo $input|sed s/exploration/progress/g|sed s/.docx/_report.docx/g);
    fi;
pandoc -s -S --reference-docx $styletemplate $input -o $output;

