gk_FM_Clang_Preset init 1
instr FM_Clang_preset
gk_FM_Clang_preset chnget "gk_FM_Clang_preset"
gk_FM_Clang_preset_ int gk_FM_Clang_preset
ktrig changed gk_FM_Clang_preset_
#define SET_PRESET(N'Amp'AttTim'AttVel'Dur'Sustain'mod'NdxVel'NdxCurve'CarKyb'DtnMix'Detune'LPF'FiltFund'NseAmp'NseBW'NseFllw'ModMix'ModDep'ModRte'ModRteKyb'StMix'StWidth'RvbDryWet'RvbSize)
#
if i(gk_FM_Clang_preset_)==$N then
printks2 "Setting preset %d\n", $N
chnset $Amp ,"Amp"
chnset $AttTim ,"AttTim"
chnset $AttVel ,"AttVel"
chnset $Dur ,"Dur"
chnset $Sustain ,"Sustain"
chnset $mod ,"mod"
chnset $NdxVel ,"NdxVel"
chnset $NdxCurve ,"NdxCurve"
chnset $CarKyb ,"CarKyb"
chnset $DtnMix ,"DtnMix"
chnset $Detune ,"Detune"
chnset $LPF ,"LPF"
chnset $FiltFund ,"FiltFund"
chnset $NseAmp ,"NseAmp"
chnset $NseBW ,"NseBW"
chnset $NseFllw ,"NseFllw"
chnset $ModMix ,"ModMix"
chnset $ModDep ,"ModDep"
chnset $ModRte ,"ModRte"
chnset $ModRteKyb,"ModRteKyb"
chnset $StMix ,"StMix"
chnset $StWidth ,"StWidth"
chnset $RvbDryWet,"RvbDryWet"
chnset $RvbSize ,"RvbSize"
endif
#
if ktrig==1 then
reinit SEND_PRESET
endif
SEND_PRESET:
; N'Amp'AttTim'AttVel'Dur'Sustain'mod 'NdxVel'NdxCurve'CarKyb'DtnMix'Detune'LPF'FiltFund'NseAmp'NseBW'NseFllw'ModMix'ModDep'ModRte'ModRteKyb'StMix'StWidth'RvbDryWet'RvbSize)
$SET_PRESET(1'0.5'0 '0 '12 '1 '2.29'1 '16 '0 '1 '1.5 '16 '1 '1000 '0.001'0 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
$SET_PRESET(2'0.8'0 '0 '1.7'1 '5.04'0.55 '100 '0 '0 '0 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.1 '0.55 )
$SET_PRESET(3'0.5'0.1 '0.5 '3.8'1 '1.00'2 '28 '0 '1 '15 '16 '0 '500 '0.0077'1 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
$SET_PRESET(4'0.5'0 '0.006 '24 '1 '3.19'1 '15 '0 '0.47 '2.5 '16 '1 '0 '0.001'0 '1 '0.1 '0.33 '1.7 '0.5 '0.01 '0.3 '0.55 )
$SET_PRESET(5'0.8'0 '1 '6.6'0 '1.12'1 '13 '-2.68 '1 '25 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '1 '0.02 '0.172 '0.21 )
$SET_PRESET(6'3.25'0.1 '1 '1.28'0 '2.55'0.55 '35 '-2.3 '0 '1.5 '16 '1 '2000 '0.0025'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.68 '0.93 )
$SET_PRESET(7'4 '0.1 '1 '2.39'1 '2.55'0.55 '35 '-2.3 '0 '1.5 '16 '1 '16 '0.1262'0 '0 '0.1 '3 '0 '0 '0.01 '1.00 '0 )
$SET_PRESET(8'1 '0 '0 '14.25'1 '6.97'0.55 '35 '-2.3 '0.13 '1.5 '4.78'1 '16 '0.0326'0 '1 '0.1 '3 '0 '0 '0.01 '0.6 '0 )
$SET_PRESET(9'1.65'0 '0 '1.7'1 '1.20'0.55 '100 '0 '0 '0 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.1 '0.55 )
$SET_PRESET(10'2' 0 '0 '1 '1 '2.15'0.55 '100 '0 '0.21 '12 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.35 '0.37 )
$SET_PRESET(11'2 '0.1 '1 '3.44'1 '3.75'0.55 '100 '0 '0.21 '25 '16 '0 '0 '0.001'1 '0 '0.1 '3 '0 '0.5 '0.05 '0 '0.37 )
$SET_PRESET(12'2 '0.1 '1 '1.45'1 '2.66'1.00 '16 '0 '0.21 '25 '16 '0 '0 '0.001'1 '0 '0.1 '3 '0 '0.5 '0.05 '0.6 '0.37 )
$SET_PRESET(13'1.9'0 '0 '24 '1 '2.29'1.00 '23.25 '-4 '1 '1.5 '16 '1 '151 '0.0178'1 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
