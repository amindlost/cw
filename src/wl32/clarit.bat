tasm /zi/m/w2/dCLARION wlprog >err.log
tasm /zi/m/w2/dCLARION wlfeed >>err.log
tasm /zi/m/w2/dCLARION wlparse >>err.log
tasm /zi/m/w2/dCLARION wlp1mod >>err.log
tasm /zi/m/w2/dCLARION wlp1lib >>err.log
tasm /zi/m/w2/dCLARION wlp2mod >>err.log
tasm /zi/m/w2/dCLARION wlp2lib >>err.log
tasm /zi/m/w2/dCLARION wltable >>err.log
tasm /zi/m/w2/dCLARION wlp1res >>err.log
tlink /m/3 @wl32clar.rsp >>err.log
if errorlevel 1 goto bypass
cw /d wl32clar
goto end
:bypass
type err.log
:end
