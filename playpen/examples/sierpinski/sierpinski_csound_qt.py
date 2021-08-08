'''
This piece demonstrates the use of PySide6 to create a Csound piece with a 
graphical user interface created in Qt Designer.

There is a strict naming pattern:

1.  piece.py must have a UI defined in piece.ui. This is what the SciTE custom 
    command creates and edits.
2.  Some boilerplate code is used to load and realize the UI.
3.  The user must define Python functions as slots that are connected to 
    the signal fields of the PySide6 widgets.
4.  The piece must observe a distinction between performance controls that 
    must all be handled in a single slot, and the controls that start and 
    stop the performance, which 
'''

# The actual piece follows below and is wired into the UI.



###############################################################################
"""
A musical hommage to the Sierpinski triangle using a recursive process to
generate a self-similar melodies based on a set of tones representing the
"sides" of a triangle.  The duration of each note is the process duration
divided by the number of intervals in the melody. Thus, the entire melody
in the next level will occupy the same mount of time as one tone in the
current level. When the process starts running it outputs each note in the
melody transposed to the current tone. If levels is greater then 1 then the
process sprouts recursive copies of itself for each note in the melody
transposed up trans intervals. The value for levels is decremented by 1,
which will cause the recursive process to stop when the value reaches 0.

"""
import musx
from musx import Score, Note, Seq, MidiFile, keynum
import CsoundThreaded
import traceback

global values_for_channels

try:
    print("Csound was found.")
    csound
except:
    print("Csound was not found, creating new Csound.")
    csound = CsoundThreaded.CsoundThread()
print("Csound address: {}".format(csound.GetCsound()))

def sierpinski(score, tone, shape, trans, levels, dur, amp):
    """
    Generates a melodic shape based on successive transpositions (levels) of
    itself. 
    
    Parameters
    ----------
    score : Score
        The musical score to add events to.
    tone : keynum
        The melodic tone on which to base the melody for the current level.
    shape : list
        A list of intervals defining the melodic shape. 
    levels : int
        The number of levels the melody should be reproduced on. 
    dur : int | float
        The duration of the process.
    amp : float
        The amplitude of the process.
    """
    num = len(shape)
    for i in shape:
        k = tone + i
        # play current tone in melody
        n = Note(time=score.now, duration=dur, pitch=min(k,127), amplitude=amp, instrument=0)
        score.add(n)
        if (levels > 1):
            # sprout melody on tone at next level
            score.compose(sierpinski(score, (k + trans), shape,
                        trans, levels - 1, dur / num,  amp))
        yield dur

# It's good practice to add any metadata such as tempo, midi instrument
# assignments, micro tuning, etc. to track 0 in your midi file.
track0 = MidiFile.metatrack()
# Track 1 will hold the composition.
track1 = Seq()
# Create a scheduler and give it t1 as its output object.
score = Score(out=track1)

# Create the composition. Specify levels and melody length with care!
# The number of events that are generateed is exponentially related to
# the length of the melody and the number of levels. For example the
# first compose() generates 120 events, the second 726, and the third 2728!
#score.compose(sierpinski(score, keynum('a0'), [0, 7, 5], 12, 4, 3, .5))
#score.compose(sierpinski(score, keynum('a0'), [0, 7, 5], 8, 5, 7, .5))
score.compose(sierpinski(score, musx.keynum('a0'), [0, -1, 3, 11], 12, 5, 24, .5))
#score.compose(sierpinski(score, 24., [0, -1, 2, 13], 12, 5, 24, .5))

# Write the tracks to a midi file in the current directory.
file = MidiFile("sierpinski.mid", [track0, track1]).write()
print(f"Wrote '{file.pathname}'.")

# To automatially play demos use setmidiplayer() and playfile().
# Example:
#     setmidiplayer("fluidsynth -iq -g1 /usr/local/sf/MuseScore_General.sf2")
#     playfile(file.pathname)  

# Now the Csound stuff.

orc  = '''

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1

gi_ampmidicurve_dynamic_range init .375
gi_ampmidicurve_exponent init 5

prealloc "Harpsichord", 20

connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

alwayson "ReverbSC"
alwayson "MasterOutput"

gk_overlap init .0125

gk_Harpsichord_level init 0
gk_Harpsichord_pan init .3
gi_Harpsichord_release init .3
gk_Harpsichord_pick init .275
gk_Harpsichord_reflection init .75
gi_Harpsichord_pluck init .5
gk_Harpsichord_level chnexport "gk_Harpsichord_level", 3
gk_Harpsichord_pan chnexport "gk_Harpsichord_pan", 3
gi_Harpsichord_release chnexport "gi_Harpsichord_release", 3
gk_Harpsichord_pick chnexport "gk_Harpsichord_pick", 3
gk_Harpsichord_reflection chnexport "gk_Harpsichord_reflection", 3
gk_Harpsichord_pluck chnexport "gi_Harpsichord_pluck", 3
giharptable ftgen 0, 0, 65536, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
i_space_left_to_right rnd .7
i_space_left_to_right += .3
k_space_bottom_to_top = p8
i_phase = p9
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
k_amplitude = 1
// ares wgpluck2 iplk, kamp, icps, kpick, krefl
apluck wgpluck2 gi_Harpsichord_pluck, k_amplitude, iHz, gk_Harpsichord_pick, gk_Harpsichord_pick
// pluck pluck 1, kHz, iHz, 0, 1
aharp poscil 1, kHz, giharptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2)
i_attack = .002
i_sustain = p3
i_release = gi_Harpsichord_release
p3 = i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, gk_Harpsichord_pan
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
; printks "Harpsichord      %9.4f   %9.4f\\n", 0.5, a_out_left, a_out_right
prints "Harpsichord    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p1/6, active(p1)
kpbend    pchbend   2
printks2 "pchbend %9.4f\\n", kpbend
kmodw     midictrl  1
printks2 "kmodw   %9.4f\\n", kmodw
kctl6     midictrl  6
printks2 "kctl6   %9.4f\\n", kctl6
kctl4     midictrl  4
printks2 "kctl4   %9.4f\\n", kctl4
kctl5     midictrl  5
printks2 "kctl5   %9.4f\\n", kctl5
kafter    aftouch   1
printks2 "kafter  %9.4f\\n", kafter
endin

gk_Reverb_feedback init 0.75
gi_Reverb_delay_modulation init 0.05
gk_Reverb_frequency_cutoff init 15000
gk_Reverb_feedback chnexport "gk_Reverb_feedback", 3
instr ReverbSC
aleftout init 0
arightout init 0
aleft inleta "inleft"
aright inleta "inright"
; aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]
aleftout, arightout reverbsc aleft, aright, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
outleta "outleft", aleftout
outleta "outright", arightout
prints "ReverbSC       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_MasterOutput_level init -15
gS_MasterOutput_filename init ""
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
iamp init 1
iattack init .01
idecay init 10
isustain = 2400 - (iattack + idecay)
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain * aenvelope, aright * k_gain * aenvelope
; We want something that will play on my phone.
i_amplitude_adjustment = ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto has_filename
goto non_has_filename
has_filename:
prints sprintf("Output filename: %s\\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
non_has_filename:
prints "MasterOutput   i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p1/6, active(p1)
kstatus, kchan, kdata1, kdata2 midiin
;printf "          midi in s %4d c %4d %4d %4d\\n", kdata2, kstatus, kchan, kdata1, kdata2
endin

'''    

def on_play(state):
    print("Play button was clicked: state: {}...".format(state))
    # The "f 0" statement prevents an abrupt cutoff.
    sco = "f 0 105\n" + musx.to_csound_score(file)
    print(sco)

    csound.SetOption("-+msg_color=0")
    csound.SetOption("-d")
    csound.SetOption("-m195")
    csound.SetOption("-f")
    # Change this for your actual audio configuration, try "aplay -l" to see what they are.
    csound.SetOption("-odac:plughw:2,0")
    # Can also be a soundfile.
    # csound.setOption("-otest.wav")
    csound.CompileOrc(orc)
    csound.Start()
    #~ for channel, value in values_for_channels.items():
        #~ print(channel, value)
        #~ csound.SetControlChannel(channel, value)
    csound.ReadScore(sco)
    csound.Perform()
    # Scsound.Join()
    
def on_stop(state):
    csound.Stop()
    csound.Join()
    
def on_save(state):
    pass

def on_restore(state):
    pass
    
def on_controller_changed(value, **kwargs):
    print(value)
    print(kwargs)
    

# Boilerplate code for generating and running the working UI
"""
Saves the current values of all Csound control channel widgets in 
piece.ui.channels.
"""
def save_ui():
    global widgets_for_channels
    global values_for_channels
    log_print(piece_filepath)
    try:
        ui_filepath = get_ui_filepath()
        log_print("ui_filepath: {}".format(ui_filepath))
        log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
        for channel, widget in widgets_for_channels.items():
            channel_value = get_control_value(widget)
            values_for_channels[channel] = channel_value
            log_print("channel: {} value: {}".format(widget.get_name(), channel_value))
        ui_channels_filepath_ = get_ui_channels_filepath()
        with open(ui_channels_filepath_, "w") as file:
            file.write(json.dumps(values_for_channels))
    except:
        log_exception("Failed to save UI.")
        
import os
import sys
from PySide6.QtUiTools import QUiLoader
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QObject, QFile, QIODevice, SIGNAL, SLOT

piece_filepath = os.path.splitext(sys.argv[0])[0]
print("piece_filepath: {}".format(piece_filepath))
piece_ui_filepath = piece_filepath + ".ui"
print("piece_ui_filepath: {}".format(piece_ui_filepath))

application = QApplication(sys.argv)
ui_file = QFile(piece_ui_filepath)
if not ui_file.open(QIODevice.ReadOnly):
    print(f"Cannot open {ui_file_name}: {ui_file.errorString()}")
    sys.exit(-1)
ui_loader = QUiLoader()
main_window = ui_loader.load(ui_file)
print("main_window: {} {}".format(main_window, type(main_window)))
ui_file.close()
if not main_window:
    print(loader.errorString())
    sys.exit(-1)
    
controllers_for_names = {}
values_for_names = {}

class Controller(QObject):
    def __init__(self, widget, channel_name, csound):
        QObject.__init__(self)
        self.widget = widget
        self.channel_name = channel_name
        self.csound = csound
        controllers_for_names[self.channel_name] = self
    def on_change(self, value):
        value = value / 100.
        self.csound.SetControlChannel(self.channel_name, value)
    def get_value(self):
        return self.widget.getValue()
    def set_value(self, value):
        self.widget.setValue(value)
       
def connect_channel(widget, channel_name, csound):
    controller = Controller(widget, channel_name, csound)
    widget.valueChanged.connect(controller.on_change)
                      
main_window.show()
    
# End of boilerplate code. Signals are connected to slots below.

main_window.play_button.clicked.connect(on_play)
main_window.stop_button.clicked.connect(on_stop)
main_window.save_button.clicked.connect(on_stop)
main_window.restore_button.clicked.connect(on_restore)
connect_channel(main_window.gk_Reverb_feedback, "gk_Reverb_feedback", csound)

# Actually run the piece.

result = application.exec()
sys.exit(result)



