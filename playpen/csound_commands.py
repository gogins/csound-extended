#!/usr/bin/python3
# -*- coding: utf-8 -*-
'''
C S O U N D   C O M M A N D S
Author: Michael Gogins

This Python 3 script provides help with using Csound either from the SciTE 
text editor, or from the command line. Configuration variables for this 
script are stored in the user's home directory in a "playpen.ini" file 
with a format similar to a Windows .ini file.

Usage:

python3 csound_commands.py render-audio {csd_filepath}
    Renders the CSD to the system audio device specified in playpen.ini.
    
python3 csound_commands.py render-soundfile {csd_filepath}
    Renders the CSD to a soundfile constructed using metadata from 
    playpen.ini.
    
python3 csound_commands.py post {csd_filepath}
    Renders the CSD to a soundfile constructed using metadata from 
    playpen.ini, then post-processes the file to normalize it, save in various 
    formats, etc.
    
python3 csound_commands.py play {csd_filepath}
    Renders the CSD to a soundfile constructed using metadata from 
    playpen.ini, then post-processes the file to normalize it, save in various 
    formats, etc., then plays the soundfile using the player specified in 
    playpen.ini.
    
For post-processing, the sox and ffmpeg programs need to be installed.

For playing, the audacity program is a good choice because it can display 
soundfiles as sonograms, which can be very informative, and can export to 
various formats.

'''

import configparser
import datetime
import os
import os.path
import string
import subprocess
import sys
import time
import traceback

command = sys.argv[1]
print("command:                 {}".format(command))
cwd = os.getcwd()
print('cwd:                    ', cwd)
source_filepath = sys.argv[2]
home_directory = os.environ["HOME"]
playpen_ini_filepath = os.path.join(home_directory, "playpen.ini")
settings = configparser.ConfigParser()
settings.read_file(open(playpen_ini_filepath))
metadata_author = settings.get("metadata", "author")
metadata_publisher = settings.get("metadata", "publisher")
metadata_year = settings.get("metadata", "year")
metadata_notes = settings.get("metadata", "notes")
metadata_license = settings.get("metadata", "license")
csound_audio_output = settings.get("csound", "audio-output")
print("csound_audio_output:     " + csound_audio_output)
soundfile_editor = settings.get("playpen", "soundfile-editor")
directory, basename = os.path.split(source_filepath)
rootname, extension = os.path.splitext(basename)
title = rootname.replace("-", " ").replace("_", " ")
label = '%s -- %s' % (metadata_author, title)
label = label.replace(" ", "_")
output_filename = label + ".wav"
master_filename = '%s.normalized.wav' % label
spectrogram_filename = '%s.png' % label
cd_quality_filename = '%s.cd.wav' % label
mp3_filename = '%s.mp3' % label
mp4_filename = '%s.mp4' % label
flac_filename = '%s.flac' % label
print('Source file:            ', source_filepath)
print('Basename:               ', basename)
print('Author:                 ', metadata_author)
print('Title:                  ', title)
print('Year:                   ', metadata_year)
str_copyright          = 'Copyright %s by %s' % (metadata_year, metadata_author)
print('Copyright:              ', str_copyright)
print('Licence:                ', metadata_license)
print('Publisher:              ', metadata_publisher)
print('Notes:                  ', metadata_notes)
print('Master filename:        ', master_filename)
print('Spectrogram filename:   ', spectrogram_filename)
print('CD quality filename:    ', cd_quality_filename)
print('MP3 filename:           ', mp3_filename)
print('MP4 filename:           ', mp4_filename)
print('FLAC filename:          ', flac_filename)
bext_description       = metadata_notes
bext_originator        = metadata_author
bext_orig_ref          = basename

def render_audio():
    try:
        print("render_audio: {} to {}...".format(source_filepath, csound_audio_output))
        csound_command = "csound {} -o{}".format(source_filepath, csound_audio_output)
        print("csound command: {}".format(csound_command))
        subprocess.run(csound_command, shell=True)
    except:
        traceback.print_exc()
    finally:
        print("render_audio: {} to {}.".format(source_filepath, csound_audio_output))
        return
        
def render_soundfile():
    try:
        print("\nrender_soundfile: {} to {}...".format(source_filepath, output_filename))
        csound_command = "csound {} -o{} --simple-sorted-score={}.srt".format(source_filepath, output_filename, source_filepath)
        print("csound command: {}".format(csound_command))
        subprocess.run(csound_command, shell=True)
    except:
        traceback.print_exc()
    finally:
        print("soundfile: {} to {}.\n".format(source_filepath, output_filename))
        return
        
def post_process():
    try:
        print("post_process: {}...".format(source_filepath))
        bext_description       = metadata_notes
        bext_originator        = metadata_author
        bext_orig_ref          = basename
        #bext_umid              = xxx
        #bext_orig_date         = xxx
        #bext_orig_time         = xxx
        #bext_coding_hist       = xxx
        #bext_time_ref          = xxx
        str_comment            = metadata_notes
        str_title              = title
        str_artist             = metadata_author
        str_date               = metadata_year
        str_license            = metadata_license
        sox_normalize_command = '''sox -S "%s" "%s" gain -n -3''' % (output_filename, master_filename + 'untagged.wav')
        print('sox_normalize command:  ', sox_normalize_command)
        os.system(sox_normalize_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, master_filename)
        print('tag_wav_command:        ', tag_wav_command)
        os.system(tag_wav_command)
        sox_spectrogram_command = '''sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (master_filename, spectrogram_filename, label, str_copyright + ' (%s' % metadata_publisher)
        print('sox_spectrogram_command:', sox_spectrogram_command)
        os.system(sox_spectrogram_command)
        sox_cd_command = '''sox -S "%s" -b 16 -r 44100 "%s"''' % (master_filename, cd_quality_filename + 'untagged.wav')
        print('sox_cd_command:         ', sox_cd_command)
        os.system(sox_cd_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, cd_quality_filename)
        print('tag_wav_command:        ', tag_wav_command)
        os.system(tag_wav_command)
        mp3_command = '''lame --add-id3v2 --tt "%s" --ta "%s" --ty "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (title, "Michael Gogins", metadata_year, metadata_notes, "Electroacoustic", master_filename, mp3_filename)
        print('mp3_command:            ', mp3_command)
        os.system(mp3_command)
        sox_flac_command = '''sox -S "%s" "%s"''' % (master_filename, flac_filename)
        print('sox_flac_command:       ', sox_flac_command)
        os.system(sox_flac_command)
        mp4_command = '''%s -r 1 -i "%s" -i "%s" -codec:a aac -strict -2 -b:a 384k -c:v libx264 -b:v 500k "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), os.path.join(cwd, mp4_filename))
        mp4_metadata =  '-metadata title="%s" ' % title
        mp4_metadata += '-metadata date="%s" ' % metadata_year
        mp4_metadata += '-metadata genre="%s" ' % metadata_notes
        mp4_metadata += '-metadata copyright="%s" ' % str_copyright
        mp4_metadata += '-metadata composer="%s" ' % metadata_author
        mp4_metadata += '-metadata artist="%s" ' % metadata_author
        mp4_metadata += '-metadata publisher="%s" ' % metadata_publisher
        mp4_command = '''"%s" -y -loop 1 -framerate 2 -i "%s" -i "%s" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" %s "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), mp4_metadata, os.path.join(cwd, mp4_filename))
        mp4_command = mp4_command.replace('\\', '/')
        print('mp4_command:            ', mp4_command)
        os.system(mp4_command)
    except:
        traceback.print_exc()
    finally:
        print("post_process: {}.".format(source_filepath))
        return
    
def play():
    try:
        print("play: {}".format(source_filepath))
        command = "audacity {}".format(master_filename)
        print("playback command: {}".format(command))
        subprocess.run(command, shell=True)
    except:
        traceback.print_exc()
    finally:
        print("soundfile: {} to {}.".format(source_filepath, output_filename))
        return

if command == 'audio':
    render_audio()
if command == 'soundfile':
    render_soundfile()
if command == 'post':
    render_soundfile()
    post_process()
if command == 'play':
    render_soundfile()
    post_process()
    play()
    
print("Finished.")