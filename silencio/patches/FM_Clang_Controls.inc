gi_FM_Clang_sine ftgen 0,0,4096,10,1 ;A SINE WAVE. USED BY THE LFOs.
gi_FM_Clang_detuning ftgen 0,0,128,21,1,1 ; random array used for fixing unique detune values for each note
gi_FM_Clang_DryMap ftgen 0,0,4096,7,1,2048,1,2048,0 ; dry mixer control mapping
gi_FM_Clang_WetMap ftgen 0,0,4096,7,0,2048,1,2048,1 ; wet mixer control mapping
gaSendL,gaSendR init 0 ; initialise variable used for sending audio between instruments
gi_FM_Clang_Imp ftgen 0,0,4097,9,0.5,1,0 ; shape for the hammer inpulse
gi_FM_Clang_stereo ftgen 0,0,256,21,1,1 ; random array used for fixing unique stereo values for each note
gi_FM_Clang_detune ftgen 0,0,256,21,1,1 ; random array used for fixing unique detune values for each note
gi_FM_Clang_scale ftgen 0,0,128,-7,-1,128,1

instr FM_Clang_Controls 
iporttime = 0.05
gk_FM_Clang_mod chnget "mod"
gk_FM_Clang_mod port gk_FM_Clang_mod,iporttime
gk_FM_Clang_NdxVel chnget "NdxVel"
gk_FM_Clang_Dur chnget "Dur"
gk_FM_Clang_FiltFund chnget "FiltFund"
gk_FM_Clang_NdxCurve chnget "NdxCurve"
gk_FM_Clang_LPF chnget "LPF"
gk_FM_Clang_ModDep chnget "ModDep"
gk_FM_Clang_ModRte chnget "ModRte"
gk_FM_Clang_ModRteKyb chnget "ModRteKyb"
gk_FM_Clang_ModMix chnget "ModMix"
gk_FM_Clang_Sustain chnget "Sustain"
gk_FM_Clang_CarKyb chnget "CarKyb"
gk_FM_Clang_StWidth chnget "StWidth"
gk_FM_Clang_StWidth init 0.01
gk_FM_Clang_StMix chnget "StMix"
gk_FM_Clang_DryWet chnget "RvbDryWet"
gk_FM_Clang_Dry table gk_FM_Clang_DryWet,gi_FM_Clang_DryMap,1 ; map dry/wet control
gk_FM_Clang_Wet table gk_FM_Clang_DryWet,gi_FM_Clang_WetMap,1 ;
gk_FM_Clang_RvbSize chnget "RvbSize"
gk_FM_Clang_Detune chnget "Detune"
gk_FM_Clang_DtnMix chnget "DtnMix"
gk_FM_Clang_Amp chnget "Amp"
gk_FM_Clang_level chnget "gk_FM_Clang_level"
gk_FM_Clang_Amp port gk_FM_Clang_Amp,iporttime
gk_FM_Clang_AttTim chnget "AttTim"
gk_FM_Clang_AttVel chnget "AttVel"
gk_FM_Clang_NseAmp chnget "NseAmp"
gk_FM_Clang_NseBW chnget "NseBW"
gk_FM_Clang_NseFllw chnget "NseFllw"
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
