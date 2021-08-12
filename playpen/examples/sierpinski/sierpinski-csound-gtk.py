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

sco = musx.to_csound_score(midi_file)

#####################################################################
# TEMPLATE CODE BEGINS
# Assumptions: 
# 1. Csound orchestra is in orc string.
# 2. Csound score is in sco string.
# 3. All suitable widgets in main_window have exactly the IDs and 
#    names of Csound control channels, which are chnexport in the 
#    Csound orchestra. All such nanes and ids begin with 'gk', "gi', 
#    or 'gS'.
#####################################################################

import inspect
import json
import logging
import os
import sys
import traceback
import warnings

warnings.filterwarnings("ignore")
logging.getLogger().setLevel(logging.DEBUG)

def log_print(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    #~ # Dump the message + the name of this function to the log.
    logging.debug("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))
    
def log_exception(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    # Dump the message + the name of this function to the log.
    logging.exception("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))

import ctcsound
csound = ctcsound.Csound()
csound_is_performing = False
log_print("Global Csound instance: {} CSOUND *: 0x{:x}.".format(csound, int(csound.csound())))
    
import gi
gi.require_version('Gdk', '3.0')
from gi.repository import Gdk
from gi.repository import GObject
from gi.repository import GLib

# Read user settings.
settings = GLib.KeyFile.new()
home_directory = os.environ["HOME"]
playpen_ini_filepath = os.path.join(home_directory, "playpen.ini")
GLib.KeyFile.load_from_file(settings, playpen_ini_filepath, GLib.KeyFileFlags.NONE)
metadata_author = settings.get_value("metadata", "author")
metadata_publisher = settings.get_value("metadata", "publisher")
metadata_year = settings.get_value("metadata", "year")
metadata_notes = settings.get_value("metadata", "notes")
metadata_license=settings.get_value("metadata", "license")
csound_audio_output = settings.get_value("csound", "audio-output")
print("csound_audio_output: " + csound_audio_output)
soundfile_editor=settings.get_value("playpen", "soundfile-editor")
gnome_theme=settings.get_value("playpen", "gnome-theme")
editor_scheme = settings.get_value("playpen", "editor-scheme")

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk 

# Override some global Gnome settings with playpen.ini values.
gnome_settings = Gtk.Settings.get_default()
gnome_settings.set_property("gtk-theme-name", gnome_theme)

piece_filepath = sys.argv[0]
piece_basename = os.path.splitext(piece_filepath)[0]
ui_filepath = piece_basename + ".ui"
ui_channels_filepath = ui_filepath + ".channels"
output_soundfile_filepath = piece_filepath + ".wav"
log_print("piece_filepath:            {}".format(piece_filepath))
log_print("ui_filepath:               {}".format(ui_filepath))
log_print("ui_channels_filepath:      {}".format(ui_channels_filepath))
log_print("output_soundfile_filepath: {}".format(output_soundfile_filepath))
widgets_for_channels = dict()
values_for_channels = dict()

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

def on_destroy(source):
    try:
        csound.stop()
        csound.cleanup()
        csound.reset()
        Gtk.main_quit()
    except:
        log_exception("Shutting down.")
    
def save_ui(button = None):
    try:
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("ui_channels_filepath: {}".format(ui_channels_filepath))
        log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
        for channel, widget in widgets_for_channels.items():
            channel_value = get_control_value(widget)
            values_for_channels[channel] = channel_value
            log_print("channel: {} value: {}".format(widget.get_name(), channel_value))
        with open(ui_channels_filepath, "w") as file:
            file.write(json.dumps(values_for_channels))
    except:
        log_exception("Failed to save UI.")
    
def on_play_button_clicked(button):
    try:
        global csound_is_performing
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + csound_audio_output, "", orc, sco)
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        while csound.performKsmps() == 0:
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
    except:
        print(traceback.format_exc())
        
def post_process():
    try:
        global piece_filepath
        global output_soundfile_filepath
        cwd = os.getcwd()
        print('cwd:                    ' + cwd)
        author = metadata_author #'Michael Gogins'
        year = metadata_year #'2021'
        license = metadata_license #'ASCAP'
        publisher = metadata_publisher #'Irreducible Productions, ASCAP'
        notes = metadata_notes #'Electroacoustic Music'

        directory, basename = os.path.split(piece_filepath)
        rootname = os.path.splitext(basename)[0].split('.')[0]
        soundfile_name = output_soundfile_filepath
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
        os.system('del *wavuntagged.wav')
        os.system('{} {}'.format(soundfile_editor, master_filename))
        print("")
    except:
        print(traceback.format_exc())
        

def on_render_button_clicked(button):
    try:
        global piece_filepath
        global values_for_channels
        global csound_is_performing
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + output_soundfile_filepath, "", orc, sco)
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        while csound.performBuffer() == 0:
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
        csound.stop()
        csound.cleanup()
        csound.reset()
        post_process()
    except:
        print(traceback.format_exc())
        

def on_stop_button_clicked(button):
    try:
        global csound_is_performing
        csound_is_performing = False
        csound.stop()
        csound.cleanup()
        csound.reset()
        print("Csound has been stopped and reset.")
    except:
        print(traceback.format_exc())
        
def get_control_value(control):
    channel_value = 0
    if isinstance(control, Gtk.Switch):
        channel_value = control.get_state()
    elif isinstance(control, Gtk.ComboBox):
        channel_value = control.get_active_id()
    elif isinstance(control, Gtk.ToggleButton):
        channel_value = control.get_active()
    elif isinstance(control, Gtk.Scale):
        channel_value = control.get_value()
        log_print("control: {} value: {}".format(control.get_name(), channel_value))
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        channel_value = control.get_text()
    return channel_value
    
def set_control_value(control, value):
    log_print("control: {} value: {}".format(control.get_name(), value))
    if isinstance(control, Gtk.Switch):
        control.set_state(value)
    elif isinstance(control, Gtk.ComboBox):
        control.set_active_id(value)
    elif isinstance(control, Gtk.ToggleButton):
        control.set_active(value)
    elif isinstance(control, Gtk.Scale):
        control.set_value(value)
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        control.set_text(value)
         
# Please note, the order of conditions matters; some subclasses do 
# not handle superclass signals.

def on_control_change(control, data=-1 ,user_data=None):
    try:
        global values_for_channels
        global csound_is_performing
        global csound
        channel_name = control.get_name()
        channel_value = get_control_value(control)
        log_print("channel: {} value: {}".format(channel_name, channel_value))
        # Prevent premature definition of control channels.
        if csound_is_performing == False:
            pass
        else:
            if isinstance(control, Gtk.ToggleButton):
                log_print("ToggleButton:  setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.ComboBox):
                channel_value = control.get_active_id()
                log_print("Combo box:     SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Button):
                channel_value = float(data)
                log_print("Button:        setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.MenuItem):
                channel_value = data
                log_print("MenuItem:      setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Scale):
                log_print("Scale:         setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            #~ elif isinstance(control, Gtk.SpinButton):
                #~ channel_value = control.get_value()
                #~ csound.SetControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Editable):
                channel_value = control.get_text()
                log_print("Editable:      SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
        values_for_channels[channel_name] = channel_value
    except:
        print(traceback.format_exc())
        
'''
For only those widgets and those signals that are used here to control Csound 
performances using the Csound control channels, connect the on_control_changed 
signal to its callback. Also, associate the actual widget with its name and 
its current value.
'''
def connect_controls(container):
    global widgets_for_channels
    global values_for_channels
    for child in container.get_children():
        channel_name = child.get_name()
        # Valid channels start with gk, gi, or gS.
        if channel_name[:2] not in ["gk", "gi", "gS"]:
            pass #log_print("  {} is not a Csound control channel, skipping...".format(channel_name))
        else:
            channel_value = get_control_value(child)
            if isinstance(child, Gtk.ComboBox):
                child.connect("changed", on_control_change, 1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Button):
                child.connect("pressed", on_control_change, 1.)
                child.connect("released", on_control_change, 0.)            
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.MenuItem):
                child.connect("select", on_control_change, 1.)  
                child.connect("deselect", on_control_change, 0.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Scale):
                handler_id = child.connect("value-changed", on_control_change)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.ScaleButton):
                child.connect("value-changed", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Switch):
                child.connect("state-set", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Editable):
                child.connect("activate", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.SpinButton):
                child.connect("value-changed", on_control_change, -1)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Container):
                connect_controls(child)                

def load_ui(source=None):
    try:
        global ui_filepath
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("Loading UI: {}".format(ui_filepath))
        if os.path.exists(ui_filepath) == True:
            with open(ui_filepath, "r") as file:
                ui_text = file.read()
            result = builder.add_from_string(ui_text)
            main_window = builder.get_object("main_window")
            log_print("main_window: {}".format(main_window))
            log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
            if os.path.exists(ui_channels_filepath) == True:
                with open(ui_channels_filepath, "r") as file:
                    text = file.read()
                    values_for_channels = json.loads(text)
                    for channel, value in values_for_channels.items():
                        if channel in widgets_for_channels:
                            widget = widgets_for_channels[channel]
                            if widget:
                                set_control_value(widget, value)
        else:
            log_print("UI file not found, not defining controls.")
    except:
        log_print("Error: failed to load user-defined controls layout.")
        print(traceback.format_exc())
  
builder = Gtk.Builder()
builder.add_from_file(ui_filepath)
main_window = builder.get_object("main_window")
main_window.connect("destroy", on_destroy)
save_button = builder.get_object("save_button")
save_button.connect("clicked", save_ui)
restore_button = builder.get_object("restore_button")
restore_button.connect("clicked", load_ui)
play_button = builder.get_object("play_button")
play_button.connect("clicked", on_play_button_clicked)
render_button = builder.get_object("render_button")
render_button.connect("clicked", on_render_button_clicked)
stop_button = builder.get_object("stop_button")
stop_button.connect("clicked", on_stop_button_clicked)
level_slider = builder.get_object("gk_MasterOutput_level")
connect_controls(main_window.get_child())
main_window.show_all() 
load_ui()
Gtk.main()
