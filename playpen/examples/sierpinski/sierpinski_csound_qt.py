'''
This piece demonstrates the use of PySide6 to create a Csound piece with a 
graphical user interface created in Qt Designer.

There is a strict naming pattern:

1.  piece.py must have a UI defined in piece.ui. This is what the SciTE custom 
    command creates and edits.
2.  Some boilerplate code is used to load and realize the UI.
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
import ctcsound
import traceback

csound = ctcsound.Csound()

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
score.compose(sierpinski(score, keynum('a0'), [0, 7, 5], 8, 5, 7, .5))
#score.compose(sierpinski(score, musx.keynum('a0'), [0, -1, 3, 11], 12, 5, 24, .5))
#score.compose(sierpinski(score, 24., [0, -1, 2, 13], 12, 5, 24, .5))

# Write the tracks to a midi file in the current directory.
midi_file = MidiFile("sierpinski.mid", [track0, track1]).write()
print(f"Wrote '{midi_file.pathname}'.")

# To automatially play demos use setmidiplayer() and playfile().
# Example:
#     setmidiplayer("fluidsynth -iq -g1 /usr/local/sf/MuseScore_General.sf2")
#     playfile(midi_file.pathname)  

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

gk_Harpsichord_level chnexport "gk_Harpsichord_level", 3
gk_Harpsichord_pan chnexport "gk_Harpsichord_pan", 3
gi_Harpsichord_release chnexport "gi_Harpsichord_release", 3
gk_Harpsichord_pick chnexport "gk_Harpsichord_pick", 3
gk_Harpsichord_reflection chnexport "gk_Harpsichord_reflection", 3
gi_Harpsichord_pluck chnexport "gi_Harpsichord_pluck", 3
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

gk_Reverb_frequency_cutoff init 15000
gk_Reverb_feedback chnexport "gk_Reverb_feedback", 3
gi_Reverb_delay_modulation chnexport "gi_Reverb_delay_modulation", 3
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
printks2 "MasterOutput: gk_MasterOutput_level %9.4f\\n", gk_MasterOutput_level
endin

'''    

# Boilerplate code for generating and running the user interface from a 
# Qt UI file.

audio_output = "dac:plughw:1,0"
metadata_author = "Michael Gogins"
metadata_publisher = "Irreducible Productions"
metadata_year = "2021"
metadata_notes = "electroacoustic music"
metadata_license= "ASCAP"
soundfile_editor = "audacity"
channels_for_names = {}
channel_values_for_names = {}

import json
import os
import sys
import time
from PySide6.QtUiTools import QUiLoader
from PySide6.QtWidgets import QApplication, QAbstractSlider, QCheckBox, QAbstractButton, QLineEdit, QComboBox
from PySide6.QtCore import QObject, QFile, QIODevice, QEventLoop, QCoreApplication, QThread

piece_filepath = os.path.splitext(sys.argv[0])[0]
print("piece_filepath: {}".format(piece_filepath))
piece_ui_filepath = piece_filepath + ".ui"
print("piece_ui_filepath: {}".format(piece_ui_filepath))
piece_ui_channels_filepath = piece_ui_filepath + ".channels"
print("piece_ui_channels_filepath: {}".format(piece_ui_channels_filepath))
piece_output_soundfile = piece_filepath + ".wav"
print("piece_output_soundfile: {}".format(piece_output_soundfile))

def create_csd_text(options, license, orc, sco):
    csd_text = '''
<CsoundSynthesizer>
<CsOptions>
{}
</CsOptions>
<CsLicense>
{}
</CsLicense>
<CsInstruments>
{}
</CsInstruments>
<CsScore>
{}
</CsScore>
</CsoundSynthesizer>
'''.format(options, license, orc, sco)
    return csd_text

def csound_play():
    try:
        print("csound_play...")
        global csound
        csound_stop()
        del csound
        csound = ctcsound.Csound()
        sco = musx.to_csound_score(midi_file)
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + audio_output, "", orc, sco)
        with open("saved.csd", "w") as file:
            file.write(csd_text)
        print("Csound address: {}".format(csound.csound()))
        csound.compileCsdText(csd_text)
        csound.start()
        csound_restore_channels()
        while csound.performBuffer() == False:
            application.processEvents(QEventLoop.AllEvents)
        print("Finished performing...")
        print("csound_play.")
    except:
        traceback.print_exc()
        
def csound_render():
    try:
        print("csound_render...")
        global sound
        csound_stop()
        sco = musx.to_csound_score(midi_file)
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + piece_output_soundfile, "", orc, sco)
        csound.compileCsdText(csd_text)
        csound.start()
        csound_restore_channels()
        while csound.PprformBuffer() == False:
            application.processEvents(QEventLoop.AllEvents)
        print("Finished performing...")
        post_process()
        print("csound_render.")
    except:
        traceback.print_exc()
    
def csound_stop():
    try:
        print("csound_stop...")
        global csound
        csound.stop()
        time.sleep(1)
        print("Stopped...")
        csound.reset()
        time.sleep(1)
        print("Finished reset...")
        print("csound_stop.")
    except:
        traceback.print_exc()
    
def csound_save_channels():
    try:
        print("csound_save_channels...")      
        with open(piece_ui_channels_filepath, "w") as file:
            file.write(json.dumps(channel_values_for_names))
    except:
        traceback.print_exc()
  
def csound_restore_channels():
    try:
        print("csound_restore_channels...")
        if os.path.exists(piece_ui_channels_filepath) == True:
            with open(piece_ui_channels_filepath, "r") as file:
                text = file.read()
                channel_values_for_names = json.loads(text)
                for name, value in channel_values_for_names.items():
                    if name in channels_for_names:
                        channel = channels_for_names[name]
                        if channel:
                            # print("Setting channel: {} to value: {}.".format(name, value))
                            channel.set_value(value)
    except:
        traceback.print_exc()
    
def post_process():
    try:
        print(piece_filepath)
        cwd = os.getcwd()
        print('cwd:                    ' + cwd)
        author = metadata_author
        year = metadata_year #'2021'
        license = metadata_license #'ASCAP'
        publisher = metadata_publisher #'Irreducible Productions, ASCAP'
        notes = metadata_notes #'Electroacoustic Music'

        directory, basename = os.path.split(piece_filepath)
        rootname = os.path.splitext(basename)[0].split('.')[0]
        soundfile_name = rootname + ".wav"
        title = rootname.replace("-", " ").replace("_", " ")
        label = '{} -- {}'.format(author, title).replace(" ", "_")
        master_filename = '{}.normalized.wav'.format(label)
        spectrogram_filename = '%s.png' % label
        cd_quality_filename = '%s.cd.wav' % label
        mp3_filename = '%s.mp3' % label
        mp4_filename = '%s.mp4' % label
        flac_filename = '%s.flac' % label
        print('Basename:               ' + basename)
        print('Original soundfile:     ' + soundfile_name)
        print('Author:                 ' + author)
        print('Title:                  ' + title)
        print('Year:                   ' + year)
        str_copyright          = 'Copyright %s by %s' % (year, author)
        print('Copyright:              ' + str_copyright)
        print('Licence:                ' + license)
        print('Publisher:              ' + publisher)
        print('Notes:                  ' + notes)
        print('Master filename:        ' + master_filename)
        print('Spectrogram filename:   ' + spectrogram_filename)
        print('CD quality filename:    ' + cd_quality_filename)
        print('MP3 filename:           ' + mp3_filename)
        print('MP4 filename:           ' + mp4_filename)
        print('FLAC filename:          ' + flac_filename)
        bext_description       = notes
        bext_originator        = author
        bext_orig_ref          = basename
        #bext_umid              = xxx
        #bext_orig_date         = xxx
        #bext_orig_time         = xxx
        #bext_coding_hist       = xxx
        #bext_time_ref          = xxx
        str_comment            = notes
        str_title              = title
        str_artist             = author
        str_date               = year
        str_license            = license
        sox_normalize_command = '''sox -S "%s" "%s" gain -n -3''' % (soundfile_name, master_filename + 'untagged.wav')
        print('sox_normalize command:  ' + sox_normalize_command)
        os.system(sox_normalize_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, master_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        sox_spectrogram_command = '''sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (master_filename, spectrogram_filename, label, str_copyright + ' (%s' % publisher)
        print('sox_spectrogram_command:' + sox_spectrogram_command)
        os.system(sox_spectrogram_command)
        sox_cd_command = '''sox -S "%s" -b 16 -r 44100 "%s"''' % (master_filename, cd_quality_filename + 'untagged.wav')
        print('sox_cd_command:         ' + sox_cd_command)
        os.system(sox_cd_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, cd_quality_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        mp3_command = '''lame --add-id3v2 --tt "%s" --ta "%s" --ty "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (title, "Michael Gogins", year, notes, "Electroacoustic", master_filename, mp3_filename)
        print('mp3_command:            ' + mp3_command)
        os.system(mp3_command)
        sox_flac_command = '''sox -S "%s" "%s"''' % (master_filename, flac_filename)
        print('sox_flac_command:       ' + sox_flac_command)
        os.system(sox_flac_command)
        mp4_command = '''%s -r 1 -i "%s" -i "%s" -codec:a aac -strict -2 -b:a 384k -c:v libx264 -b:v 500k "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), os.path.join(cwd, mp4_filename))
        mp4_metadata =  '-metadata title="%s" ' % title
        mp4_metadata += '-metadata date="%s" ' % year
        mp4_metadata += '-metadata genre="%s" ' % notes
        mp4_metadata += '-metadata copyright="%s" ' % str_copyright
        mp4_metadata += '-metadata composer="%s" ' % author
        mp4_metadata += '-metadata artist="%s" ' % author
        mp4_metadata += '-metadata publisher="%s" ' % publisher
        mp4_command = '''"%s" -y -loop 1 -framerate 2 -i "%s" -i "%s" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" %s "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), mp4_metadata, os.path.join(cwd, mp4_filename))
        mp4_command = mp4_command.replace('\\', '/')
        print('mp4_command:            ' + mp4_command)
        os.system(mp4_command)
        os.system('rm *wavuntagged.wav')
        os.system('{} {}'.format(soundfile_editor, master_filename))
        print("")
    except:
        traceback.print_exc()
    
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

'''
Each instance of this class encapsulates one Csound control channel along with  
the Qt Widget that manages it. The methods of this class should have cases for 
handling different types of Csound channels (strings or numbers), and 
different types of Qt Widgets.

The Qt Widget types and corresponding signals handled here are (from less to 
more abstract):

1. AbstractSlider and subclasses: valueChanged.
3. QCheckBox: stateChanged.
4. QAbstractButton: pressed, released.
5. QLineEdit: editingFinished.
6. QComboBox: currentTextChanged.

This list could of course be extended.

The Qt Widget values are normalized with a range of 100 and may need to be 
rescaled in the Csound code.
'''
class Channel(QObject):
    def __init__(self, widget, channel_name):
        QObject.__init__(self)
        self.widget = widget
        self.channel_name = channel_name
        channels_for_names[self.channel_name] = self
    def on_change(self, value):
        global csound
        print("on_change: {}: {}".format(self.channel_name, value))
        if isinstance(self.widget, QAbstractSlider) == True:
            value = value / 100.
            csound.setControlChannel(self.channel_name, value)
            channel_values_for_names[self.channel_name] = value
            return;
    def get_value(self):
        global csound
        if isinstance(self.widget, QAbstractSlider) == True:
            value = self.widget.getValue()
            value = value * 100.
            csound.setControlChannel(self.channel_name, value)
            channel_values_for_names[self.channel_name] = value
            return;
    def set_value(self, value):
        self.widget.setValue(value * 100)
       
def connect_channel(widget, channel_name):
    if channel_name.startswith("g") != True:
        print("Not connecting {} because {} is not a Csound control channel.".format(widget, channel_name))
        return
    if isinstance(widget, QAbstractSlider) == True:
        channel = Channel(widget, channel_name)
        widget.valueChanged.connect(channel.on_change)
        return
        
main_window.show()
    
main_window.actionPlay.triggered.connect(csound_play)
main_window.actionRender.triggered.connect(csound_render)
main_window.actionStop.triggered.connect(csound_stop)
main_window.actionSave.triggered.connect(csound_save_channels)
main_window.actionRestore.triggered.connect(csound_restore_channels)

# End of boilerplate code. The following code should configure the Csound 
# control channels. Each Csound control channel has one widget, one signal, 
# and one slot.

connect_channel(main_window.gk_MasterOutput_level, "gk_MasterOutput_level")
connect_channel(main_window.gk_Reverb_feedback, "gk_Reverb_feedback")
connect_channel(main_window.gi_Reverb_delay_modulation, "gi_Reverb_delay_modulation")
connect_channel(main_window.gk_Harpsichord_level, "gk_Harpsichord_level")
connect_channel(main_window.gk_Harpsichord_pan, "gk_Harpsichord_pan")
connect_channel(main_window.gi_Harpsichord_pluck, "gi_Harpsichord_pluck")
connect_channel(main_window.gk_Harpsichord_pick, "gk_Harpsichord_pick")
connect_channel(main_window.gk_Harpsichord_reflection, "gk_Harpsichord_reflection")
connect_channel(main_window.gi_Harpsichord_release, "gi_Harpsichord_release")

# Actually run the piece.

result = application.exec()
csound_stop()
sys.exit(result)



