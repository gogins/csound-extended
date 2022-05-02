gk_FluidAudio_level chnexport "gk_FluidAudio_level", 3 ; 0
gi_FluidAudio_attack chnexport "gi_FluidAudio_attack", 3 ; 0.003
gi_FluidAudio_release chnexport "gi_FluidAudio_release", 3 ; 0.01
gS_FluidAudio_soundfont_name chnexport "gS_FluidAudio_soundfont_name", 3 ; "sf_GMbank.sf2"
gi_FluidAudio_channel chnexport "gi_FluidAudio_channel", 3 ; 0
gi_FluidAudio_bank chnexport "gi_FluidAudio_bank", 3 ; 0
gi_FluidAudio_preset chnexport "gi_FluidAudio_preset", 3 ;
gk_FluidAudio_midi_dynamic_range chnexport "gk_FluidAudio_midi_dynamic_range", 3 ; 127

gk_FluidAudio_level init 0
gi_FluidAudio_attack init 0.003
gi_FluidAudio_release init 0.01
gi_FluidAudio_engine init 0
gS_FluidAudio_soundfont_name init "sf_GMbank.sf2"
gi_FluidAudio_soundfont init 0
gi_FluidAudio_channel init 0
gi_FluidAudio_bank init 0
gi_FluidAudio_preset init 0
gi_FluidAudio_initialized init 0
gk_FluidAudio_midi_dynamic_range init 127

instr FluidAudio
//////////////////////////////////////////////
// Initialization and audio output for 
// SoundFonts with Fluidsynth.
// Author: Michael Gogins
//////////////////////////////////////////////
if gi_FluidAudio_initialized == 0 then
  gi_FluidAudio_initialized = 1
  // ienginenum fluidEngine [iReverbEnabled] [, iChorusEnabled] [,iNumChannels] [, iPolyphony] 
  gi_FluidAudio_engine fluidEngine 0, 0, nchnls
  // isfnum fluidLoad soundfont, ienginenum[, ilistpresets]
  gi_FluidAudio_soundfont fluidLoad gS_FluidAudio_soundfont_name, gi_FluidAudio_engine, 1
  // fluidProgramSelect ienginenum, ichannelnum, isfnum, ibanknum, ipresetnum
  fluidProgramSelect gi_FluidAudio_engine, gi_FluidAudio_channel, gi_FluidAudio_soundfont, gi_FluidAudio_bank, gi_FluidAudio_preset
endif
i_instrument = p1
i_time = p2
//  If p3 is greater than 0, the performance is score-driven. To prevent 
//  a click at release time, we set the sustain segment to p3, and 
//  extend p3 so that the release will complete before the instance is 
//  deleted.
if p3 > 0 then
  i_sustain = p3
endif
//  If p3 is less than 0, the performance is MIDI-driven or otherwise 
//  real-time. To enable the envelope to work, we set the sustain segment to a 
//  large value, and count on the release segment of the envelope to be 
//  triggered by the note off, and thus to complete without a click. However, 
//  if the d opcode is used to turn the instance off, there will be a click 
//  instead of a smooth release.
if p3 < 0 then
  i_sustain = 1000
endif
p3 = gi_FluidAudio_attack + i_sustain + gi_FluidAudio_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_FluidAudio_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
//  Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
//  AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FluidAudio_level)
a_out_left, a_out_right fluidOut gi_FluidAudio_engine
a_signal = (a_out_left + a_out_right) * .5
a_declicking linsegr 0, gi_FluidAudio_attack, 1, i_sustain, 1, gi_FluidAudio_release, 0
a_signal = a_signal * a_declicking

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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
