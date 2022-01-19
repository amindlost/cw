@echo off
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlprog >err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlfeed >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlparse >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlp1mod >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlp1lib >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlp2mod >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlp2lib >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wltable >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlclip >>err.log
tasm /zi/m/w2/dCLIPPER /dSYMBOLPACK /dDLLSUPPORT wlp1res >>err.log
tlink /3/m @wl32clip.rsp >>err.log
if errorlevel 1 goto bypass
cw /d wl32clip
goto end
:bypass
type err.log
:end
