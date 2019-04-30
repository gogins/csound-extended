# -*- coding: utf-8 -*-
'''
P O S T   P R O C E S S
Author: Michael Gogins

Attempts to post-process one master soundfile, which is not part of an album.

Usage:

       0               1           
python post-process.py filepath 

'''

import datetime
import os
import os.path
import string
import sys
import time
import traceback

print sys.argv

cwd = os.getcwd()
print 'cwd:                    ', cwd
filepath = sys.argv[1]
author = 'Michael Gogins'
year = '2019'
license = 'ASCAP'
publisher = 'Irreducible Productions, ASCAP'
notes = 'Electroacoustic Music'

directory, basename = os.path.split(filepath)
rootname = os.path.splitext(basename)[0].split('.')[0]
title = rootname.replace("-", " ").replace("_", " ")
label = '%s -- %s' % (author, title)
master_filename = '%s.normalized.wav' % label
spectrogram_filename = '%s.png' % label
cd_quality_filename = '%s.cd.wav' % label
mp3_filename = '%s.mp3' % label
mp4_filename = '%s.mp4' % label
flac_filename = '%s.flac' % label
print 'Original file:          ', filepath
print 'Basename:               ', basename
print 'Author:                 ', author
print 'Title:                  ', title
print 'Year:                   ', year
str_copyright          = 'Copyright %s by %s' % (year, author)
print 'Copyright:              ', str_copyright
print 'Licence:                ', license
print 'Publisher:              ', publisher
print 'Notes:                  ', notes
print 'Master filename:        ', master_filename
print 'Spectrogram filename:   ', spectrogram_filename
print 'CD quality filename:    ', cd_quality_filename
print 'MP3 filename:           ', mp3_filename
print 'MP4 filename:           ', mp4_filename
print 'FLAC filename:          ', flac_filename
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
sox_normalize_command = '''sox -S "%s" "%s" gain -n -3''' % (filepath, master_filename + 'untagged.wav')
print 'sox_normalize command:  ', sox_normalize_command
os.system(sox_normalize_command)
tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, master_filename)
print 'tag_wav_command:        ', tag_wav_command
os.system(tag_wav_command)
sox_spectrogram_command = '''sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (master_filename, spectrogram_filename, label, str_copyright + ' (%s' % publisher)
print 'sox_spectrogram_command:', sox_spectrogram_command
os.system(sox_spectrogram_command)
sox_cd_command = '''sox -S "%s" -b 16 -r 44100 "%s"''' % (master_filename, cd_quality_filename + 'untagged.wav')
print 'sox_cd_command:         ', sox_cd_command
os.system(sox_cd_command)
tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, cd_quality_filename)
print 'tag_wav_command:        ', tag_wav_command
os.system(tag_wav_command)
mp3_command = '''lame --add-id3v2 --tt "%s" --ta "%s" --ty "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (title, "Michael Gogins", year, notes, "Electroacoustic", master_filename, mp3_filename)
print 'mp3_command:            ', mp3_command
os.system(mp3_command)
sox_flac_command = '''sox -S "%s" "%s"''' % (master_filename, flac_filename)
print 'sox_flac_command:       ', sox_flac_command
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
print 'mp4_command:            ', mp4_command
os.system(mp4_command)
os.system('del *wavuntagged.wav')
os.system('audacity "%s"' % master_filename)
print

