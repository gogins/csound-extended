opcode FM_Clang_veloc, i, iii
; *p->r = *p->ilo + csound->curip->m_veloc*(*p->ihi - *p->ilo) * dv127;
ivel, ilow, ihigh xin
xout ilow + ivel * (ihigh - ilow) * 0.007874
endop
; N'Amp'AttTim'AttVel'Dur'Sustain'mod 'NdxVel'NdxCurve'CarKyb'DtnMix'Detune'LPF'FiltFund'NseAmp'NseBW'NseFllw'ModMix'ModDep'ModRte'ModRteKyb'StMix'StWidth'RvbDryWet'RvbSize)
; $SET_PRESET(1'0.5'0 '0 '12 '1 '2.29'1 '16 '0 '1 '1.5 '16 '1 '1000 '0.001'0 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
; $SET_PRESET(2'0.8'0 '0 '1.7'1 '5.04'0.55 '100 '0 '0 '0 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.1 '0.55 )
; $SET_PRESET(3'0.5'0.1 '0.5 '3.8'1 '1.00'2 '28 '0 '1 '15 '16 '0 '500 '0.0077'1 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
; $SET_PRESET(4'0.5'0 '0.006 '24 '1 '3.19'1 '15 '0 '0.47 '2.5 '16 '1 '0 '0.001'0 '1 '0.1 '0.33 '1.7 '0.5 '0.01 '0.3 '0.55 )
; $SET_PRESET(5'0.8'0 '1 '6.6'0 '1.12'1 '13 '-2.68 '1 '25 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '1 '0.02 '0.172 '0.21 )
; $SET_PRESET(6'3.25'0.1 '1 '1.28'0 '2.55'0.55 '35 '-2.3 '0 '1.5 '16 '1 '2000 '0.0025'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.68 '0.93 )
; $SET_PRESET(7'4 '0.1 '1 '2.39'1 '2.55'0.55 '35 '-2.3 '0 '1.5 '16 '1 '16 '0.1262'0 '0 '0.1 '3 '0 '0 '0.01 '1.00 '0 )
;             N'Amp 'AttTim'AttVel'Dur   'Sustain'mod 'NdxVel'NdxCurve'CarKyb'DtnMix'Detune'LPF      'FiltFund'NseAmp'NseBW'NseFllw'ModMix'ModDep'ModRte'ModRteKyb'StMix'StWidth'RvbDryWet'RvbSize)
; $SET_PRESET(8'1   '0     '0     '14.25 '1      '6.97'0.55  '35      '-2.3  '0.13  '1.5   '4.78 '1  '16      '0.0326'0    '1      '0.1   '3     '0     '0        '0.01 '0.6    '0 )
; $SET_PRESET(9'1.65'0     '0     '1.7   '1      '1.20'0.55  '100     '0     '0     '0     '3.8  '0  '0       '0.001 '0    '0      '0.1   '3     '0     '0.5      '0.01 '0.1    '0.55 )
; $SET_PRESET(10'2' 0 '0 '1 '1 '2.15'0.55 '100 '0 '0.21 '12 '3.8'0 '0 '0.001'0 '0 '0.1 '3 '0 '0.5 '0.01 '0.35 '0.37 )
; $SET_PRESET(11'2 '0.1 '1 '3.44'1 '3.75'0.55 '100 '0 '0.21 '25 '16 '0 '0 '0.001'1 '0 '0.1 '3 '0 '0.5 '0.05 '0 '0.37 )
; $SET_PRESET(12'2 '0.1 '1 '1.45'1 '2.66'1.00 '16 '0 '0.21 '25 '16 '0 '0 '0.001'1 '0 '0.1 '3 '0 '0.5 '0.05 '0.6 '0.37 )
; $SET_PRESET(13'1.9'0 '0 '24 '1 '2.29'1.00 '23.25 '-4 '1 '1.5 '16 '1 '151 '0.0178'1 '1 '0.1 '3 '0 '0.5 '0.01 '0.3 '0.55 )
gi_FM_Clang_sine ftgen 0,0,4096,10,1 ;A SINE WAVE. USED BY THE LFOs.
gi_FM_Clang_detuning ftgen 0,0,128,21,1,1 ; random array used for fixing unique detune values for each note
gi_FM_Clang_DryMap ftgen 0,0,4096,7,1,2048,1,2048,0 ; dry mixer control mapping
gi_FM_Clang_WetMap ftgen 0,0,4096,7,0,2048,1,2048,1 ; wet mixer control mappinggaSendL,gaSendR init 0 ; initialise variable used for sending audio between instruments
gi_FM_Clang_Imp ftgen 0,0,4097,9,0.5,1,0 ; shape for the hammer inpulse
gi_FM_Clang_stereo ftgen 0,0,256,21,1,1 ; random array used for fixing unique stereo values for each note
gi_FM_Clang_detune ftgen 0,0,256,21,1,1 ; random array used for fixing unique detune values for each note
gi_FM_Clang_scale ftgen 0,0,128,-7,-1,128,1

gk_FM_Clang_Amp chnexport "gk_FM_Clang_Amp", 3 ; 1
gk_FM_Clang_AttTim chnexport "gk_FM_Clang_AttTim", 3 ; 0
gk_FM_Clang_AttVel chnexport "gk_FM_Clang_AttVel", 3 ; 0
gk_FM_Clang_Dur chnexport "gk_FM_Clang_Dur", 3 ; 1.7
gk_FM_Clang_Sustain chnexport "gk_FM_Clang_Sustain", 3 ; 1
gk_FM_Clang_mod chnexport "gk_FM_Clang_mod", 3 ; 1.2
gk_FM_Clang_NdxVel chnexport "gk_FM_Clang_NdxVel", 3 ; .55
gk_FM_Clang_NdxCurve chnexport "gk_FM_Clang_NdxCurve", 3 ; 100
gk_FM_Clang_CarKyb chnexport "gk_FM_Clang_CarKyb", 3 ; 0
gk_FM_Clang_DtnMix chnexport "gk_FM_Clang_DtnMix", 3 ; 0
gk_FM_Clang_Detune chnexport "gk_FM_Clang_Detune", 3 ; 0
gk_FM_Clang_LPF chnexport "gk_FM_Clang_LPF", 3 ; 3.
gk_FM_Clang_NseAmp chnexport "gk_FM_Clang_NseAmp", 3 ; 0
gk_FM_Clang_NseBW chnexport "gk_FM_Clang_NseBW", 3 ; .001
gk_FM_Clang_NseFllw chnexport "gk_FM_Clang_NseFllw", 3 ; 0
gk_FM_Clang_ModMix chnexport "gk_FM_Clang_ModMix", 3 ; 0
gk_FM_Clang_ModDep chnexport "gk_FM_Clang_ModDep", 3 ; .1
gk_FM_Clang_ModRte chnexport "gk_FM_Clang_ModRte", 3 ; .3
gk_FM_Clang_ModRteKyb chnexport "gk_FM_Clang_ModRteKyb", 3 ; 0
gk_FM_Clang_StMix chnexport "gk_FM_Clang_StMix", 3 ; .5
gk_FM_Clang_StWidth chnexport "gk_FM_Clang_StWidth", 3 ; 0.01
gk_FM_Clang_DryWet chnexport "gk_FM_Clang_DryWet", 3 ; .01
gk_FM_Clang_RvbSize chnexport "gk_FM_Clang_RvbSize", 3 ; .55
gk_FM_Clang_FiltFund chnexport "gk_FM_Clang_FiltFund", 3 ; 0
gk_FM_Clang_level chnexport "gk_FM_Clang_level", 3 ; 0

gk_FM_Clang_Amp init 1
gk_FM_Clang_AttTim init 0
gk_FM_Clang_AttVel init 0
gk_FM_Clang_Dur init 1.7
gk_FM_Clang_Sustain init 1
gk_FM_Clang_mod init 1.2
gk_FM_Clang_NdxVel init .55
gk_FM_Clang_NdxCurve init 100
gk_FM_Clang_CarKyb init 0
gk_FM_Clang_DtnMix init 0
gk_FM_Clang_Detune init 0
gk_FM_Clang_LPF init 3.
gk_FM_Clang_NseAmp init 0
gk_FM_Clang_NseBW init .001
gk_FM_Clang_NseFllw init 0
gk_FM_Clang_ModMix init 0
gk_FM_Clang_ModDep init .1
gk_FM_Clang_ModRte init .3
gk_FM_Clang_ModRteKyb init 0
gk_FM_Clang_StMix init .5
gk_FM_Clang_StWidth init 0.01
gk_FM_Clang_DryWet init .01
gk_FM_Clang_RvbSize init .55
gk_FM_Clang_FiltFund init 0
gk_FM_Clang_level init 0

instr FM_Clang ; Sound producing instrument. Triggered by MIDI notes or score notes.
//////////////////////////////////////////////
// Original by Iain McCurdy.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 80 - 15 - 12 
i_normalization = ampdb(-i_overall_amps) / 8
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FM_Clang_level)
gk_FM_Clang_Dry table gk_FM_Clang_DryWet,gi_FM_Clang_DryMap,1 ; map dry/wet control
gk_FM_Clang_Wet table gk_FM_Clang_DryWet,gi_FM_Clang_WetMap,1 ;
; Keyboard scaling value. Note '0' maps to a value of -1, note 64 maps to a value of zero and note 127 maps to a value of 1.
inum = i_midi_key
iKybdScl table inum,gi_FM_Clang_scale
; Amplitude Envelope
idur = i(gk_FM_Clang_Dur)
iAmpCurve = -8
iRelTim = (i(gk_FM_Clang_Sustain)==1?idur:0.1)
;iAttVel veloc i(gk_FM_Clang_AttVel)*0.05,0
iAttVel FM_Clang_veloc i_midi_velocity, i(gk_FM_Clang_AttVel)*0.05,0
iAttTim = iAttVel + i(gk_FM_Clang_AttTim)
if iAttTim==0 then
aAEnv transegr 1,idur,iAmpCurve,0, iRelTim,iAmpCurve,0
else
aAEnv transegr 0,iAttTim,iAmpCurve, 1,idur,iAmpCurve,0, iRelTim,iAmpCurve,0
endif
; Key velocity to amplitude
;iAVel veloc 0,0.1
iAVel FM_Clang_veloc i_midi_velocity, 0, 0.1
; FM index
iNdx veloc 0,i(gk_FM_Clang_NdxVel) ; Key velocity to FM index
iNdx FM_Clang_veloc i_midi_velocity, 0,i(gk_FM_Clang_NdxVel) ; Key velocity to FM index
kIEnv transeg iNdx*octave(-iKybdScl*4),idur,-i(gk_FM_Clang_NdxCurve),0 ; Index envelope. Amplitude influenced by 'Index' widget control and keyboard scaling (index reduced for higher notes, increased for lower notes)
; Carrier frequency keyboard scaling
icar = 1 * octave(iKybdScl*i(gk_FM_Clang_CarKyb))
; Carrier ratio modulation
iModRte table inum,gi_FM_Clang_scale
iModRte = octave(iModRte*i(gk_FM_Clang_ModRteKyb))
kDepEnv transeg 1,idur,-4,0
amod lfo gk_FM_Clang_ModDep*0.1*kDepEnv, gk_FM_Clang_ModRte * iModRte, 0
; Noise bandpass filter cutoff as it respond to the 'Carrier Follow' switch
kcf = gk_FM_Clang_NseFllw==1?i_frequency*gk_FM_Clang_mod:i_frequency
; Noise amplitude keyboard following
kNseAmp = octave(iKybdScl*-4)*gk_FM_Clang_NseAmp
; Main FM algorithm
if gk_FM_Clang_NseAmp>0 then ; If noise amplitude is greater than zero...
aNse gauss kNseAmp ; ... generate some gaussian noise
aNse butbp aNse,kcf,kcf*gk_FM_Clang_NseBW ; bandpass filter the noise
aNse butbp aNse,kcf,kcf*gk_FM_Clang_NseBW ; and again
else
aNse = 0 ; Otherwise no noise signal!
endif
idetune table inum,gi_FM_Clang_detune ; Read a detuning value from a random table
asig foscil iAVel, i_frequency*cent(-gk_FM_Clang_Detune*idetune), icar + aNse, gk_FM_Clang_mod, kIEnv, gi_FM_Clang_sine ; FM pair
aModSig foscil iAVel, i_frequency*cent(-gk_FM_Clang_Detune*idetune), icar + amod+ aNse, gk_FM_Clang_mod, kIEnv, gi_FM_Clang_sine ; FM pair with modulation
asig = asig + (aModSig * gk_FM_Clang_ModMix) ; Mix the two FM pairs.
; Auxilliary FM algorithm (used for detuning)
if gk_FM_Clang_DtnMix>0&&gk_FM_Clang_Detune>0 then
if gk_FM_Clang_NseAmp>0 then
aNse gauss kNseAmp
aNse butbp aNse,kcf,kcf*gk_FM_Clang_NseBW
aNse butbp aNse,kcf,kcf*gk_FM_Clang_NseBW
else
aNse = 0
endif
idetune table inum+128,gi_FM_Clang_detune ; Read a detuning value from a random table
asig2 foscil iAVel, i_frequency*cent(gk_FM_Clang_Detune*idetune), icar +aNse, gk_FM_Clang_mod, kIEnv, gi_FM_Clang_sine ; Detuning applied to fundemental
aModSig2 foscil iAVel, i_frequency*cent(gk_FM_Clang_Detune*idetune), icar+amod+aNse, gk_FM_Clang_mod, kIEnv, gi_FM_Clang_sine
asig2 = asig2 + (aModSig2 * gk_FM_Clang_ModMix)
asig = asig + asig2*gk_FM_Clang_DtnMix
endif
; Filter Fundemental
if i(gk_FM_Clang_FiltFund)==1 then ; If filter fundemental switch is active
asig butbr asig,i_frequency*icar,i_frequency*icar*0.3 ; Filter out fundemental
asig butbr asig,i_frequency*icar,i_frequency*icar*0.3 ; and again to make sure it is all gone
endif
; Lowpass Filter
if i(gk_FM_Clang_LPF)<32 then ; If lowpass filter is less than maximum...
kCF limit i_frequency*gk_FM_Clang_LPF,20,20000 ; ...create the cutoff value in CPS based on fundemental and LPF ratio control. Limit to prevent out of range values.
asig clfilt asig,kCF,0,2 ; Butterworth lowpass filter
endif
; Amplitude Envelope
asig = asig * aAEnv * gk_FM_Clang_Amp; Apply amplitude envelope
; Stereo Effect
iDlyTimL table inum,gi_FM_Clang_stereo ; Read a delay time value from a random table based on note played
iDlyTimR table inum+128,gi_FM_Clang_stereo ; Read a different delay time value from a random table based on note played
if gk_FM_Clang_StWidth>0&&gk_FM_Clang_StMix>0 then ; If 'width' is above zero and 'mix' is above zero...
aL vdelay asig, iDlyTimL*gk_FM_Clang_StWidth*1000, 0.2*1000 ; ...left channel delay
aR vdelay asig, iDlyTimR*gk_FM_Clang_StWidth*1000, 0.2*1000 ; right channel delay
aL ntrpol asig, aL, gk_FM_Clang_StMix ; Mix delayed and dry signal
aR ntrpol asig, aR, gk_FM_Clang_StMix ;
aleft = aL;*gk_FM_Clang_Dry
aright = aR;*gk_FM_Clang_Dry
else
aleft = asig;*gk_FM_Clang_Dry
aright = asig;*gk_FM_Clang_Dry
;gaSendL = gaSendL + asig ; Send to reverb
;gaSendR = gaSendR + asig
endif
aleft1 = aleft; * ampdb(i_midi_velocity)
aright1 = aright; * ampdb(i_midi_velocity)
a_signal = (aleft1 + aright1)
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 2.18

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
kleft = k(a_out_left)
kright = k(a_out_right)
; printks "FM_Clang       L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_FM_Clang_level
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
