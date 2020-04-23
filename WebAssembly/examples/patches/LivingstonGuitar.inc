gk_LivingstonGuitar_midi_dynamic_range init 127
gk_LivingstonGuitar_level init 0
instr LivingstonGuitar
; Author: Jeff Livingston
; Adapted by: Michael Gogins
;
; The model takes pluck position, and pickup position (in % of string length), and generates
; a pluck excitation signal, representing the string displacement. The pluck consists
; of a forward and backward traveling displacement wave, which are recirculated thru two
; separate delay lines, to simulate the one dimensional string waveguide, with
; fixed ends.
;
; Losses due to internal friction of the string, and with air, as well as
; losses due to the mechanical impedance of the string terminations are simulated by
; low pass filtering the signal inside the feedback loops.
; Delay line outputs at the bridge termination are summed and fed into an IIR filter
; modeled to simulate the lowest two vibrational modes (resonances) of the guitar body.
; The theory implies that force due to string displacement, which is equivalent to
; displacement velocity times bridge mechanical impedance, is the input to the guitar
; body resonator model. Here we have modified the transfer fuction representing the bridge
; mech impedance, to become the string displacement to bridge input force transfer function.
; The output of the resulting filter represents the displacement of the guitar's top plate,
; and sound hole, since thier respective displacement with be propotional to input force.
; (based on a simplified model, viewing the top plate as a force driven spring).
;
; The effects of pluck hardness, and contact with frets during pluck release,
; have been modeled by injecting noise into the initial pluck, proportional to initial
; string displacement.
;
; Note on pluck shape: Starting with a triangular displacment, I found a decent sounding
; initial pluck shape after some trial and error. This pluck shape, which is a linear
; ramp, with steep fall off, doesn't necessarily agree with the pluck string models I've
; studied. I found that initial pluck shape significantly affects the realism of the
; sound output, but I the treatment of this topic in musical acoustics literature seems
; rather limited as far as I've encountered.
;
; Original pfields
; p1    p2   p3   p4   p5   p6     p7     p8      p9       p10        p11  p12  p13
; in    st   dur  amp  pch  plklen fbfac  pkupPos pluckPos brightness vibf vibd vibdel
; i01.1 0.25 0.75 5000 7.07 .75    0.9980 .0     .24       1          0    0    0
; inst st dur amp pch plksize(%) fbfac pickupPos(%) pluckPos(%)
; Initializations
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_LivingstonGuitar_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 118
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_LivingstonGuitar_level)
isine ftgenonce 0, 0, 65537, 10, 1
// Protect against too short delay times.
i_minimum_delay_seconds = ksmps / sr
afwav init 0
abkwav init 0
abkdout init 0
afwdout init 0
ip4 init i_amplitude
ipch pchmidinn i_midi_key
ip5 init ipch
ip6 init .75
ip7 init .9980
ip8 init 0
ip9 init .24
ip10 init 1
ip11 init 0
ip12 init 0
ip13 init 0
ip14 init 0
isustain_ = p3
iattack_ = .001
irelease_ = .01
xtratim isustain_ + iattack_ + irelease_
adeclick linsegr 0, iattack_, 1, isustain_, 1, irelease_, 0
iEstr = 1/cpspch(6.04)
ifqc = cpspch(ip5)
idlt = 1/(ifqc) ;note:delay time=2x length of string (time to traverse it)
idlt max i_minimum_delay_seconds, idlt
ipluck = .5*idlt * ip6 * ifqc/cpspch(8.02)
ifbfac = ip7 ;feedback factor
ibrightness = ip10*exp(ip6*log(2))/2 ;(exponentialy scaled) additive noise to add hi freq content
ivibRate = ip11 ;vibrato rate
ivibDepth pow 2,ip12/12
ivibDepth = idlt-1/(ivibDepth*ifqc) ;vibrato depth, +,- ivibDepth semitones
ivibStDly = p13 ;vibrato start delay (secs)
; termination impedance model
if0 = 10000 ;cutoff freq of LPF due to mech. impedance at the nut (2kHz-10kHz)
iA0 = ip7 ; damping parameter of nut impedance
ialpha = cos(2*3.14159265*if0*1/sr)
ia0 = .3*iA0/(2*(1-ialpha)) ; FIR LPF model of nut impedance, H(z)=a0+a1z^-1+a0z^-2
ia1 = iA0-2*ia0
; NOTE each filter pass adds a sampling period delay,so subtract 1/sr from tap time to compensate
; determine (in crude fashion) which string is being played
; icurStr = (ifqc > cpspch(6.04) ? 2 : 1)
; icurStr = (ifqc > cpspch(6.09) ? 3 : icurStr)
; icurStr = (ifqc > cpspch(7.02) ? 4 : icurStr)
; icurStr = (ifqc > cpspch(7.07) ? 5 : icurStr)
; icurStr = (ifqc > cpspch(7.11) ? 6 : icurStr)
ipupos = ip8*idlt/2; pick up position (in % of low E string length)
ippos = ip9*idlt/2; pluck position (in % of low E string length)
isegF = 1/sr
isegF2 = ipluck
iplkdelF = (ipluck/2 > ippos ? 0 : ippos-ipluck/2)
isegB = 1/sr
isegB2 = ipluck
iplkdelB = (ipluck/2 > idlt/2-ippos ? 0 : idlt/2-ippos-ipluck/2)
; EXCITATION SIGNAL GENERATION
; the two excitation signals are fed into the fwd delay represent the 1st and 2nd
; reflections off of the left boundary, and two accelerations fed into the bkwd delay
; represent the the 1st and 2nd reflections off of the right boundary.
; Likewise for the backward traveling acceleration waves, only they encouter the
; terminationsin the opposite order.
ipw = 1;
ipamp = p4*ipluck;4/ipluck
aenvstrf linseg 0,isegF,-ipamp/2,isegF2,0
adel1 delayr idlt
aenvstrf1 deltapi iplkdelF ;initial forward traveling wave (pluck to bridge)
aenvstrf2 deltapi iplkdelB+idlt/2 ;first forward traveling reflection (nut to bridge)
delayw aenvstrf
; inject noise for attack time string fret contact, and pre pluck vibrations against pick
anoiz rand ibrightness
aenvstrf1 = aenvstrf1 + anoiz*aenvstrf1
aenvstrf2 = aenvstrf2 + anoiz*aenvstrf2
; filter to account for losses along loop path
aenvstrf2 filter2 aenvstrf2, 3, 0, ia0,ia1,ia0
; combine into one signal (flip refl wave's phase)
aenvstrf = aenvstrf1-aenvstrf2
; initial backward excitation wave
aenvstrb linseg 0,isegB,-ipamp/2,isegB2,0
adel2 delayr idlt
aenvstrb1 deltapi iplkdelB ; initial bdwd traveling wave (pluck to nut)
aenvstrb2 deltapi idlt/2+iplkdelF ;first forward traveling reflection (nut to bridge)
delayw aenvstrb
; initial bdwd traveling wave (pluck to nut)
; aenvstrb1 delay aenvstrb, iplkdelB
; first bkwd traveling reflection (bridge to nut)
; aenvstrb2 delay aenvstrb, idlt/2+iplkdelF
; inject noise
aenvstrb1 = aenvstrb1 + anoiz*aenvstrb1
aenvstrb2 = aenvstrb2 + anoiz*aenvstrb2
; filter to account for losses along loop path
aenvstrb2 filter2 aenvstrb2, 3, 0, ia0,ia1,ia0
; combine into one signal (flip refl wave's phase)
aenvstrb = aenvstrb1-aenvstrb2
; low pass to band limit initial accel signals to be < 1/2 the sampling freq
ainputf tone aenvstrf,sr*.9/2
ainputb tone aenvstrb,sr*.9/2
; additional lowpass filtering for pluck shaping\
; Note, it would be more efficient to combine stages into a single filter
ainputf tone ainputf,sr*.9/2
ainputb tone ainputb,sr*.9/2
; Vibrato generator
avib poscil ivibDepth, ivibRate,isine
ivibStDly max i_minimum_delay_seconds, ivibStDly
avibdl delayr ivibStDly*1.1+.001
avibrato deltapi ivibStDly
delayw avib
; Dual Delay line,
; NOTE: delay length longer than needed by a bit so that the output at t=idlt will be interpolated properly
; fwd delay line
afd delayr (idlt+ivibDepth)*1.1 ;forward traveling wave delay line
afwav deltapi ipupos ;output tap point for fwd traveling wave
afwdout deltapi idlt-1/sr + avibrato ;output at end of fwd delay (left string boundary)
afwdout filter2 afwdout, 3, 0, ia0,ia1,ia0 ;lpf/attn due to reflection impedance
delayw ainputf + afwdout*ifbfac*ifbfac
; bkwd delay line
abkwd delayr (idlt+ivibDepth)*1.1 ;backward trav wave delay line
abkwav deltapi idlt/2-ipupos ;output tap point for bkwd traveling wave
; abkterm deltapi idlt/2 ;output at the left boundary
abkdout deltapi idlt -1/sr + avibrato ;output at end of bkwd delay (right string boundary)
abkdout filter2 abkdout, 3, 0, ia0,ia1,ia0
delayw ainputb + abkdout*ifbfac*ifbfac
; resonant body filter model, from Cuzzucoli and Lombardo
; IIR filter derived via bilinear transform method
; the theoretical resonances resulting from circuit model should be:
; resonance due to the air volume + soundhole = 110 Hz (strongest)
; resonance due to the top plate = 220 Hz
; resonance due to the inclusion of the back plate = 400 Hz (weakest)
aresbod filter2 (afwdout + abkdout), 5,4, .000000000005398681501844749,.00000000000001421085471520200,-.00000000001076383426834582,-00000000000001110223024625157,.000000000005392353230604385,-3.990098622573566,5.974971737738533,-3.979630684599723,.9947612723736902
a_signal = 1500*(afwav+abkwav+aresbod*.000000000000000000003)
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
