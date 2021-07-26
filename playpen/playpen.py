#!/usr/bin/python3
# -*- coding: utf-8 -*-
import concurrent.futures
import contextlib
import datetime
import inspect
import logging
import markdown
import math
import os.path
import pathlib
import pdb
import random
import string
import subprocess
import sys
import time
import traceback
import warnings
import xml.etree.ElementTree as ElementTree

# Comment this out during development?

warnings.filterwarnings("ignore")
logging.getLogger().setLevel(logging.DEBUG)

def autolog(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    # Dump the message + the name of this function to the log.
    logging.debug("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))
    
def autoexception(message):
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


'''
A few strict naming conventions greatly simplify the code.

Each piece is always a single text file of code; it could be piece.csd, 
piece.py, or piece.html. The piece should specify a real-time audio output 
device and, if needed, an audio input and MIDI input and output. For rendering 
to a soundfile, the playpen program will write to piece.wav and post-process 
that file to produce piece.normalized.wav, piece.mp3, piece.flac, and piece.mp4;
piece.normalized.wav will then be opened for inspection in Audacity.

All user-defined Python graphical user interfaces are stored in a ui file 
with the same filename, i.e. my_piece.py goes with my_piece.ui, and the 
widget tree thus defined is re-parented to the controls_layout widget. The 
user need write no code for handling UI events; the convention is that all UI 
events from the user-defined controls are handled in a single function that 
dispatches all widget event values to a Csound control channel with the same 
name as the widget. 

If you want to write a piece that combines different languages, e.g. a Csound 
orchestra with a Python score generator with HTML for display or control, then 
write a Python piece that embeds the Csound code and the HTML code as variables 
containing multiple line strings, and then call functions in the playpen 
program to set up the GUI and control the piece. All functions defined in the 
code below are in the scope of a Python piece.
'''

import gi
gi.require_version('Gdk', '3.0')
from gi.repository import Gdk
from gi.repository import GObject
from gi.repository import GLib

# Obtain user settings.
settings = GLib.KeyFile.new()
GLib.KeyFile.load_from_file(settings, "playpen.ini", GLib.KeyFileFlags.NONE)
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
gi.require_version("GtkSource", "3.0")
from gi.repository import GtkSource
code_editor = GtkSource.View()

# Override some global Gnome settings with playpen.ini values.
gnome_settings = Gtk.Settings.get_default()
gnome_settings.set_property("gtk-theme-name", gnome_theme)

gi.require_version("WebKit2", "4.0")
from gi.repository import WebKit2
gi.require_version("JavaScriptCore", "4.0")
from gi.repository import JavaScriptCore
gi.require_version("GtkSource", "3.0")
from gi.repository import GtkSource

# Create a global instance of native Csound. For pure Csound and Python 
# pieces, this instance is a singleton.

import CsoundThreaded
csound = CsoundThreaded.CsoundThread()
autolog("Global Csound instance: {} CSOUND *: 0x{:x}.".format(csound, int(csound.GetCsound())))
        
title = "Playpen"
autolog("title: {}".format(title))

builder = Gtk.Builder()
builder.add_from_file("playpen.ui")

# These are the only global variables. As for the actual piece, it is always 
# read from the editor, which is effectively global. Note to maintainers:
# always declare these global in functions that use them.

global piece_filepath
global piece_ui_dom
global widgets_for_channel_names

piece_filepath = ""
piece_ui_dom = None
widgets_for_channel_names = dict()

# An empty, version-free ui template string.
# In ui, the GtkGrid can be replaced by any other container, 
# but it must have the id "user_controls_layout".

ui_controls_template = '''<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <requires lib="gtk+" version="3.20"/>
  <object class="GtkGrid" id="user_controls_layout">
    <property id="name">user_controls_layout</property>
    <property name="name">user_controls_layout</property>
    <property name="visible">True</property>
    <property name="can_focus">False</property>
  </object>
</interface>
'''

def piece_is_csound():
    if piece_filepath.lower().endswith('.csd'):
        return True
    else:
        return False

def piece_is_python():
    if piece_filepath.lower().endswith('.py'):
        return True
    else:
        return False
    
def piece_is_html():
    if piece_filepath.lower().endswith('.html'):
        return True
    else:
        return False
    
def on_new_button_clicked(button):
    global piece_filepath
    autolog("Creating new piece...")
    try:
        file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
            parent=None, 
            action=Gtk.FileChooserAction.SAVE)
        file_chooser_dialog.add_buttons(
        Gtk.STOCK_CANCEL,
        Gtk.ResponseType.CANCEL,
        Gtk.STOCK_SAVE,
        Gtk.ResponseType.OK)
        file_chooser_dialog.run()
        piece_filepath = file_chooser_dialog.get_filename()
        ui_filepath = get_ui_filepath()
        with open(ui_filepath, 'w') as file:
            file.write(ui_controls_template)
        autolog(piece_filepath)
        code_editor.get_buffer().set_text("")
    except:
        print(traceback.format_exc())
    
def load_piece():
    global piece_filepath
    autolog(piece_filepath)
    try:
        with open(piece_filepath, "r") as file:
            piece_code = file.read()
            language = language_manager.guess_language(piece_filepath)
            if language is not None:
                code_editor.get_buffer().set_language(language)
            code_editor.get_buffer().set_text(piece_code)
        if piece_is_csound():
            load_ui()
        if piece_is_python():
            load_ui()
        if piece_is_html():
            piece_uri = pathlib.Path(piece_filepath).resolve().parent.as_uri()
            autolog("load_piece: uri: {}".format(piece_uri))
            webview.load_html(get_piece_code(), piece_uri)            
        main_window.set_title(piece_filepath)
    except:
        print(traceback.format_exc())

"""
Saves the ui for the piece, not from the ui text, but from the ui controls, 
updating the ui DOM with the _current_ values of the user controls. Each 
control's name must be the same as its id, and each control must have a 
unique id.
"""
def save_ui():
    global piece_ui_dom
    global widgets_for_channel_names
    autolog(piece_filepath)
    autolog("piece_ui_dom:\n{}".format(ElementTree.tostring(piece_ui_dom.getroot(), encoding="unicode")))
    try:
        ui_filepath = get_ui_filepath()
        autolog("ui_filepath: {}".format(ui_filepath))
        for channel, value in widgets_for_channel_names.items():
            autolog("channel: {} value: {}".format(channel, value))
            id_xpath = "//object[@id='{}']".format(channel)
            autolog("id_xpath: {}:".format(id_xpath))
            elements = piece_ui_dom.findall(id_xpath)
            for element in elements:
                autolog("  id element: {}".format(element))
            name_xpath = "//property[@name='{}']".format(channel)
            autolog("name_xpath: {}:".format(name_xpath))
            elements = piece_ui_dom.findall(name_xpath)
            for element in elements:
                autolog("  name element: {}".format(element))
        piece_ui_dom.write(ui_filepath)
    except:
        autoexception("Failed to save ui.")
        
def get_control_value(control):
    channel_value = 0
    if isinstance(control, Gtk.ToggleButton):
        channel_value = control.get_active()
    elif isinstance(control, Gtk.Scale):
        channel_value = control.get_value()
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        channel_value = control.get_text()
    return channel_value
         
'''
For only those widgets and those signals that are used here to control Csound 
performances using the Csound control channels, connect the on_control_changed 
signal to its callback. Also, associate the actual widget with its name and 
its current value.
'''
def connect_controls(container, on_control_changed_):
    global widgets_for_channel_names
    widgets_for_channel_names.clear()
    children = container.get_children()
    for child in children:
        channel_name = child.get_name()
        if isinstance(child, Gtk.Button):
            autolog(child.get_name())
            child.connect("pressed", on_control_changed_, 1.)
            child.connect("released", on_control_changed_, 0.)            
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.MenuItem):
            autolog(child.get_name())
            child.connect("select", on_control_changed_, 1.)
            child.connect("deselect", on_control_changed_, 0.)
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.Scale):
            autolog(child.get_name())
            child.connect("value-changed", on_control_changed_, -1.)
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.Switch):
            autolog(child.get_name())
            child.connect("state-set", on_control_changed_, -1.)
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.Editable):
            autolog(child.get_name())
            child.connect("activate", on_control_changed_, -1.)
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.SpinButton):
            autolog(child.get_name())
            child.connect("value-changed", on_control_changed_, -1)
            widgets_for_channel_names[channel_name] = (get_control_value(child), child)
        if isinstance(child, Gtk.Container):
            connect_controls(child, on_control_changed_)

# Please note, the order of conditions matters; some subclasses do 
# not handle superclass signals.

def on_control_change(control, data, user=None):
    global widgets_for_channel_names
    try:
        channel_name = control.get_name()
        channel_value = get_control_value(control)
        if isinstance(control, Gtk.ToggleButton):
             csound.SetControlChannel(channel_name, float(channel_value))
        elif isinstance(control, Gtk.Button):
            channel_value = data
            csound.SetControlChannel(channel_name, float(data))
        elif isinstance(control, Gtk.MenuItem):
            channel_value = data
            csound.SetControlChannel(channel_name, float(channel_value))
        elif isinstance(control, Gtk.Scale):
            csound.SetControlChannel(channel_name, float(channel_value))
        #~ elif isinstance(control, Gtk.SpinButton):
            #~ channel_value = control.get_value()
            #~ csound.SetControlChannel(channel_name, channel_value)
        elif isinstance(control, Gtk.Editable):
            channel_value = control.get_text()
            csound.SetStringChannel(channel_name, channel_value)
        widgets_for_channel_names[channel_name] = (channel_value, control)
        autolog("on_control_change:\n\tchannel: {}\n\ttype: {} (widget: {} data: {} value: {})".format(channel_name, type(channel_value), control, data, channel_value))
    except:
        print(traceback.format_exc())

def get_ui_filepath():   
    global piece_filepath
    pathlib_ = pathlib.PurePath(piece_filepath)
    ui_filepath = str(pathlib_.with_suffix(".ui"))
    # print("ui_filepath: {}".format(ui_filepath))
    return ui_filepath

def load_ui():
    global piece_filepath
    global piece_ui_dom
    autolog(piece_filepath)
    ui_filepath = get_ui_filepath()
    if os.path.exists(ui_filepath) == True:
        try:
            piece_ui_dom = ElementTree.parse(ui_filepath)
            piece_ui_dom.write(ui_filepath + ".xml")
            ui_text = ElementTree.tostring(piece_ui_dom.getroot(), encoding="unicode")
            result = builder.add_from_string(ui_text)
            user_controls_layout = builder.get_object("user_controls_layout")
            autolog("user_controls_layout: {}".format(user_controls_layout))
            children = controls_layout.get_children()
            for child in children:
                if child.get_name() == "user_controls_layout":
                    controls_layout.remove(child)
            controls_layout.add(user_controls_layout)
            connect_controls(controls_layout, on_control_change)
        except:
            autolog("Error: failed to load user-defined controls layout.")
            print(traceback.format_exc())
    else:
        autolog("ui file not found, not defining controls.")

def on_open_button_clicked(button):
    global piece_filepath
    autolog("Opening a file...")
    try:
        file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
            parent=None, 
            action=Gtk.FileChooserAction.OPEN)
        file_chooser_dialog.add_buttons(
        Gtk.STOCK_CANCEL,
        Gtk.ResponseType.CANCEL,
        Gtk.STOCK_OPEN,
        Gtk.ResponseType.OK)
        file_chooser_dialog.run()
        piece_filepath = file_chooser_dialog.get_filename()
        load_piece()
        file_chooser_dialog.close()
    except:
        print(traceback.format_exc())
        
def get_piece_code():
    buffer = code_editor.get_buffer()
    piece_code = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    return piece_code
   
def save_piece():
    global piece_filepath
    autolog(piece_filepath)
    try:
        with open(piece_filepath, "w") as file:
            file.write(get_piece_code())
        # Don't do this by default, require the user to save the ui;
        # guards against unintential erasure of custom values.
        # save_ui()
    except:
        print(traceback.format_exc())
        
def on_save_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        save_piece()
    except:
        print(traceback.format_exc())
    
def on_save_as_button_clicked(button):
    global piece_filepath
    autolog("saving %s as...".format(piece_fileplath))
    try:
        file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
            parent=None, 
            action=Gtk.FileChooserAction.SAVE)
        file_chooser_dialog.add_buttons(
        Gtk.STOCK_CANCEL,
        Gtk.ResponseType.CANCEL,
        Gtk.STOCK_SAVE,
        Gtk.ResponseType.OK)
        file_chooser_dialog.run()
        piece_filepath = file_chooser_dialog.get_filename()
        save_piece()
        save_ui()
    except:
        print(traceback.format_exc())
        
def on_play_audio_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        save_piece()
        load_ui()
        if piece_is_python():
            # Only globals are passed, because otherwise recursive functions 
            # defined and invoked in the piece will not work. See:
            # https://stackoverflow.com/questions/871887/using-exec-with-recursive-functions
            exec(get_piece_code(), globals())
        if piece_is_csound():
            # Change output target here by patching the csound options.
            csd = patch_csound_options(get_piece_code(), output="realtime")
            csound.CompileCsdText(csd)
            csound.Start()
            csound.Perform()
    except:
        autoexception("")
        
def on_render_soundfile_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        save_piece()
        load_ui()
        if piece_is_csound():
            csd = patch_csound_options(get_piece_code(), output="soundfile")
            csound.CompileCsdText(csd)
            csound.Start()
            # Keep the UI responsive during performance.
            while csound.PerformBuffer() == 0:
                Gtk.main_iteration_do(False)
            csound.Stop()
            csound.Join()
            csound.Cleanup()
            csound.Reset()
            post_process()
    except:
        print(traceback.format_exc())
        
def on_stop_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        csound.Stop()
        csound.Join()
        csound.Cleanup()
        csound.Reset()
        print("Csound has been stopped and reset.")
    except:
        print(traceback.format_exc())
        
def post_process():
    global piece_filepath
    autolog(piece_filepath)
    try:
        cwd = os.getcwd()
        print('cwd:                    ' + cwd)
        author = metadata_author #'Michael Gogins'
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
        os.system('del *wavuntagged.wav')
        os.system('{} {}'.format(soundfile_editor, master_filename))
        print("")
    except:
        print(traceback.format_exc())
        
def patch_csound_options(csd, output="soundfile"):
    '''
    -odac --output
    -iadc --input
    -M --midi-device
    -Q
    '''
    autolog("output: " + output)
    options_start_index = csd.find("<CsOptions>") + len("<CsOptions>")
    options_end_index =  csd.find("</CsOptions>") 
    csd_top = csd[0:options_start_index]
    # Remove spaces between flags and values, so that flag and value are one token.
    csd_options = csd[options_start_index:options_end_index]
    csd_options = csd_options.replace(" -o ", " -o")
    csd_options = csd_options.replace(" --output ", " -output")
    autolog("csound_options: {}".format(csd_options))
    csd_bottom = csd[options_end_index:-1]
    csd_options_tokens = csd_options.split()
    for i in range(len(csd_options_tokens)):
        token = csd_options_tokens[i]
        autolog("token: {}".format(token))
        if token.startswith("-o"):
            if output == "soundfile":
                directory, basename = os.path.split(piece_filepath)
                rootname = os.path.splitext(basename)[0].split('.')[0]
                output_soundfile = rootname + ".wav"
                print("output_soundfile: " + output_soundfile)
                token = "-o" + output_soundfile
                print(type(token))
                print("new token: " + token)
                print("boo")
                csd_options_tokens[i] = token
            else:
                print("csound_audio_output: " + csound_audio_output)
                token = "-o" + csound_audio_output
                print("new token: " + token)
                csd_options_tokens[i] = token
    csd_options = " ".join(csd_options_tokens)
    patched_csd = "".join([csd_top, "\n", csd_options, "\n", csd_bottom])
    autolog("Original csd: {}".format(csd))
    autolog("Patched csd: {}".format(csd))
    return patched_csd
        
def ui_exit_callback(future):
    global piece_filepath
    ui_filepath = get_ui_filepath()
    autolog("Finished editing {}.".format(ui_filepath))
    load_ui()
    
def on_edit_gui_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        ui_filepath = get_ui_filepath()
        if os.path.exists(ui_filepath) == False:
            with open(ui_filepath, "wt") as file:
                print("Writing {} to {}.".format(ui_filepath, ui_controls_template))
                file.write(ui_controls_template)
        print("ui_filepath: {}".format(ui_filepath))    
        pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        future_ = pool.submit(subprocess.call, "glade {}".format(ui_filepath), shell=True)
        future_.add_done_callback(ui_exit_callback)
    except:
        print(traceback.format_exc())

def on_save_gui_button_clicked(button):
    global piece_filepath
    autolog(piece_filepath)
    try:
        save_ui()
    except:
        print(traceback.format_exc())
            
def on_destroy(source):
    global piece_filepath
    autolog(piece_filepath)
    csound.Stop()
    csound.Join()
    csound.Cleanup()
    csound.Reset()
    Gtk.main_quit()
    
search_settings = GtkSource.SearchSettings()
search_context = None
    
'''
Activating the search entry (i.e. pressing [Enter]) always
begins a new search based on current settings. Subsequent 
searches and replacements continue to use these settings.
'''
def on_search_entry_activate(widget):
    try:
        print(widget)
        search_text = widget.get_text()
        print("Search for: {}".format(widget.get_text()))
        buffer = code_editor.get_buffer()
        search_insert = buffer.get_iter_at_mark(buffer.get_insert())
        search_settings.set_wrap_around(True)
        search_settings.set_case_sensitive(check_button_case_sensitive.get_active())
        search_settings.set_at_word_boundaries(check_button_whole_word.get_active())
        search_settings.set_search_text(search_text)
        search_context = GtkSource.SearchContext.new(buffer, search_settings)
        match, start, end = search_context.forward(search_insert)
        # print("match: {} start: {} end: {}".format(match, start.get_offset(), end.get_offset()))
        if match:
            buffer.place_cursor(start)
            buffer.move_mark(buffer.get_selection_bound(), end)
            code_editor.scroll_to_mark(buffer.get_insert(), 0.25, True, 0.5, 0.5)
        else:
            print("search: {} not found.".format(search_settings.get_search_text()))
            
    except:
        print(traceback.format_exc())
    
def on_search_button_clicked(widget):
    try:
        buffer = code_editor.get_buffer()
        search_insert = buffer.get_iter_at_mark(buffer.get_insert())
        search_insert.forward_chars(1)
        match, start, end = search_context.forward(search_insert)
        # print("match: {} start: {} end: {}".format(match, start.get_offset(), end.get_offset()))
        if match:
            buffer.place_cursor(start)
            buffer.move_mark(buffer.get_selection_bound(), end)
            code_editor.scroll_to_mark(buffer.get_insert(), 0.25, True, 0.5, 0.5)
        else:
            print("search: {} not found.".format(search_settings.get_search_text()))
    except:
        print(traceback.format_exc())
        
def on_replace_button_clicked(widget):
    try:
        buffer = code_editor.get_buffer()
        oldsel = buffer.get_selection_bounds()
        search_insert = buffer.get_iter_at_mark(buffer.get_insert())
        #search_insert.forward_chars(1)
        match, start, end = search_context.forward(search_insert)
        # print("match: {} start: {} end: {}".format(match, start.get_offset(), end.get_offset()))
        if match:
            buffer.place_cursor(start)
            buffer.move_mark(buffer.get_selection_bound(), end)
            code_editor.scroll_to_mark(buffer.get_insert(), 0.25, True, 0.5, 0.5)
        newsel = buffer.get_selection_bounds()
        # Only replace if there is an already-selected match at the cursor
        if (match and oldsel and oldsel[0].equal(newsel[0]) and
                oldsel[1].equal(newsel[1])):
            search_context.replace(newsel[0], newsel[1], replacement_entry.get_text(), -1)
            search_insert = buffer.get_iter_at_mark(buffer.get_insert())
            search_insert.forward_chars(1)
            match, start, end = search_context.forward(search_insert)
            # print("match: {} start: {} end: {}".format(match, start.get_offset(), end.get_offset()))
            if match:
                buffer.place_cursor(start)
                buffer.move_mark(buffer.get_selection_bound(), end)
                code_editor.scroll_to_mark(buffer.get_insert(), 0.25, True, 0.5, 0.5)
            else:
                print("replace: {} not found.".format(search_settings.get_search_text()))
    except:
        print(traceback.format_exc())

def on_replace_all_button_clicked(widget):
    try:
        buffer = code_editor.get_buffer()
        saved_insert = buffer.create_mark(
            None, buffer.get_iter_at_mark(buffer.get_insert()), True)
        search_context.replace_all(replacement_entry.get_text(), -1)
        if not saved_insert.get_deleted():
            buffer.place_cursor(buffer.get_iter_at_mark(saved_insert))
            code_editor.scroll_to_mark(
                buffer.get_insert(), 0.25, True, 0.5, 0.5)
    except:
        print(traceback.format_exc())

def on_apply_scheme_button(widget):
    scheme = style_scheme.get_style_scheme()
    code_editor.get_buffer().set_style_scheme(scheme)
    
def on_initialize_web_extensions(web_context):
    autolog("on_initialize_web_extensions: {}".format(web_context))
    web_context.set_web_extensions_directory("/home/mkg/csound-extended/playpen/")
    user_data_ = GLib.Variant.new_uint64(int(csound.GetCsound()))
    user_data_.ref_sink()
    web_context.set_web_extensions_initialization_user_data(user_data_)

main_window = builder.get_object("main_window")
main_window.connect("destroy", on_destroy)
html_window = builder.get_object("html_window")
code_editor = builder.get_object("code_editor")
code_editor.override_color(Gtk.StateFlags.NORMAL, Gdk.RGBA(255/255, 160/255, 122/255, 1.0))
controls_layout = builder.get_object("controls_layout")
webview = WebKit2.WebView() 
# Set the directory from which to load extensions.
web_context = webview.get_context()
web_context.connect("initialize-web-extensions", on_initialize_web_extensions)
# As this program runs locally, we authorize many things.
webview_settings = webview.get_settings()
webview_settings.set_allow_file_access_from_file_urls(True)
webview_settings.set_allow_universal_access_from_file_urls(True)
webview_settings.set_enable_developer_extras(True)
webview_settings.set_enable_javascript(True)
webview_settings.set_enable_webaudio(True)
webview_settings.set_enable_webgl(True)
webview_settings.set_media_playback_requires_user_gesture(False)
webview.load_uri("http://csound.com") 
html_window.add(webview)
help_window = builder.get_object("help_window")
helpview = WebKit2.WebView() 
try:
    with open("README.md", 'r') as file:
        readme_md = file.read()
        readme_html = markdown.markdown(readme_md)
        helpview.load_html(readme_html)
except:
    traceback.print(exc())
help_window.add(helpview)
main_window.resize(4 * 800, 3 * 800)
new_button = builder.get_object("new_button")
new_button.connect("clicked", on_new_button_clicked)
open_button = builder.get_object("open_button")
open_button.connect("clicked", on_open_button_clicked)
save_button = builder.get_object("save_button")
save_button.connect("clicked", on_save_button_clicked)
save_as_button = builder.get_object("save_as_button")
save_as_button.connect("clicked", on_save_as_button_clicked)
edit_gui_button = builder.get_object("edit_gui_button")
edit_gui_button.connect("clicked", on_edit_gui_button_clicked)
save_gui_button = builder.get_object("save_gui_button")
save_gui_button.connect("clicked", on_save_gui_button_clicked)
play_audio_button = builder.get_object("play_audio_button")
play_audio_button.connect("clicked", on_play_audio_button_clicked)
render_soundfile_button = builder.get_object("render_soundfile_button")
render_soundfile_button.connect("clicked", on_render_soundfile_button_clicked)
stop_button = builder.get_object("stop_button")
stop_button.connect("clicked", on_stop_button_clicked)
search_entry = builder.get_object("search_entry")
search_entry.connect("activate", on_search_entry_activate)
replacement_entry = builder.get_object("replacement_entry")
apply_scheme_button = builder.get_object("apply_scheme_button")
apply_scheme_button.connect("activate", on_apply_scheme_button)
apply_scheme_button.connect("clicked", on_apply_scheme_button)
style_scheme = builder.get_object("style_scheme")
style_scheme_manager = GtkSource.StyleSchemeManager().get_default()
code_editor_scheme = style_scheme_manager.get_scheme(editor_scheme)
scheme_ids = style_scheme_manager.get_scheme_ids()
for scheme_id in scheme_ids:
    print("scheme_id: {}".format(scheme_id))
print("code_editor_scheme: {}".format(code_editor_scheme))
code_editor.get_buffer().set_style_scheme(code_editor_scheme)
language_manager = GtkSource.LanguageManager()
search_context = GtkSource.SearchContext()
search_settings = GtkSource.SearchSettings()
search_button = builder.get_object("search_button")
search_button.connect("clicked", on_search_button_clicked)
replace_button = builder.get_object("replace_button")
replace_button.connect("clicked", on_replace_button_clicked)
replace_all_button = builder.get_object("replace_all_button")
replace_all_button.connect("clicked", on_replace_all_button_clicked)
check_button_case_sensitive = builder.get_object("check_button_case_sensitive")
check_button_whole_word = builder.get_object("check_button_whole_word")

def perform():
    global piece_filepath
    autolog(piece_filepath)
    csound.Perform()

main_window.show_all() 

if len(sys.argv) > 1:
    piece_filepath = sys.argv[1]
    load_piece()
Gtk.main()