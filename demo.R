


# Load three instructions.
source("studyui.R")

debug(cloze)
#debug(instruction2html)

cloze("
Complete o exemplo: 


438 = 4 &times; 10 ^ {:NUMERICAL:=2:0} + 3 &times; 10 ^ {:NUMERICAL:=1:0} + 8 &times; 10 ^ {:NUMERICAL:=0:0}


* 4 {:multichoice_s:=centenas~unidades~dezenas}
* 3 {:multichoice_s:=dezenas~centenas~unidades}
* 8 {:multichoice_s:=unidades~dezenas~centenas}

")

