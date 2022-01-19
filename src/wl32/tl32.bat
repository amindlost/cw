tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM wlprog >err.log
tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM wlfeed >>err.log
tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM  wlparse >>err.log
tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM  wlp1mod >>err.log
tasm /zi/m/w2/dDLLSUPPORT  wlp1lib >>err.log
tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM wlp2mod >>err.log
tasm /zi/m/w2/dDLLSUPPORT  wlp2lib >>err.log
tasm /zi/m/w2/dDLLSUPPORT /dWATCOM_ASM wltable >>err.log
tasm /zi/m/w2/dDLLSUPPORT  wlp1res >>err.log
tlink /m/3 @wl32.rsp >>err.log
rem wl32 /m /sy @wl32.rsp >>err.log
if errorlevel 1 goto bypass
cw /d wl32
rem erase wl32.exe
rem erase wl32.map
rem erase wl32.sym
rem ren wl32temp.* wl32.*
goto end
:bypass
type err.log
:end
