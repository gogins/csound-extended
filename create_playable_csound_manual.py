'''
C R E A T E   P L A Y A B L E   C S O U N D   M A N U A L

Copyright (c) 2017 by Michael Gogins.
Licensed under the terms of the GNU Lesser Public License version 2

This Python script creates a modified version of the Csound Reference Manual
in which the example CSDs will play online in standard Web browsersome using 
the WebAssembly build of Csound. The target is the docs directory of the 
csound-extended repository.

Customize the source and target directories, run this Python script, and then
add the target directory to the csound-extended Git repository and push your
changes.
'''

print __doc__

import cStringIO
import glob
import os
import os.path
import shutil
import traceback

'''
The user must customize these variables. No additional configuration should be
required. The manual must already have been cloned and built in the usual way.
'''
source_home = r'''/home/mkg/manual'''
target_home = r'''/home/mkg/csound-extended/docs'''

source_html_directory = os.path.join(source_home, 'html')
source_examples_directory = os.path.join(source_html_directory, 'examples')

target_html_directory = os.path.join(target_home, 'html')
target_examples_directory = os.path.join(target_html_directory, 'examples')

print 'source_home:', source_home
print 'source_html_directory:', source_html_directory
print 'source_examples_directory:', source_examples_directory
print 'target_home:', target_home
print 'target_html_directory:', target_html_directory
print 'target_examples_directory:', target_examples_directory

play_button_template = '''<a class="ulink" href="examples/%s.csd.html" target="_top">Play</a>'''

def format_playable_example(filename, text):
    try:
        html_filename = filename
        print("Writing playable example: {}".format(html_filename))
        fout = cStringIO.StringIO()
        chunk = '''<html>
<head>
    <title>Minimal Example using Csound for WebAssembly</title>
 </head>
<body style="background-color:LightGrey;">
    <script src="CsoundAudioNode.js"></script>
    <script src="csound_loader.js"></script>
    <script>
    var handleMessage = function(message) {
        var messages_textarea = document.getElementById("console");
        var existing = messages_textarea.value;
        messages_textarea.value = existing + message;
        messages_textarea.scrollTop = messages_textarea.scrollHeight;
    }
    var onPlayClick = async function() {
        var csd = document.getElementById('csd').value;
        let csound = get_csound(handleMessage);
        if (csound == null) {
            return;
        }
        csound.CompileCsdText(csd);
        csound.Start();
        csound.Perform();
    }
    var onPlayStop = async function() {
        let csound = get_csound(handleMessage);
        csound.Stop();
    }
  </script>
<h1 style="font-family:sans-serif;">'''
        fout.write(chunk)
        fout.write(html_filename)
        chunk = '''</h1>
<p>
This should play if your Web browser has WebAssembly enabled (most do). Most examples will play unless they need to load files. The first time you click <i>Play</i>, Csound will load. After that, clicking on <i>Play</i> will actually play. You can edit and replay the code. 
</p>
<p>
<input type="button" value="Play" onclick="onPlayClick()"/>
<input type="button" value="Stop" onclick="onPlayStop()"/>
<p>
<textarea id="csd" style="width:98vw;height:45vh;font-family:monospace;background-color:#050570;color:#F0F090;">'''
        fout.write(chunk)
        # Try to catch and fix all cases...
        text = text.replace('\nsr ',     '\nsr =     48000 ; Changed for WebAssembly from: ')
        text = text.replace('\nsr=',     '\nsr =     48000 ; Changed for WebAssembly from: ')
        text = text.replace('\nkr ',     '\n; kr ; Changed for WebAssembly from: ')
        text = text.replace('\nkr=',     '\n; kr=     48000 ; Changed for WebAssembly from: ')
        text = text.replace('ksmps ',  'ksmps =  128 ; Changed for WebAssembly from: ')
        text = text.replace('ksmps=',  'ksmps =  128 ; Changed for WebAssembly from: ')
        text = text.replace('nchnls ', 'nchnls = 2 ; Changed for WebAssembly from: ')
        text = text.replace('nchnls=', 'nchnls = 2 ; Changed for WebAssembly from: ')
        text = text.replace('nchnls_i ', 'nchnls_i = 1 ; Changed for WebAssembly from: ')
        text = text.replace('nchnls_i=', 'nchnls_i = 1 ; Changed for WebAssembly from: ')
        fout.write(text)
        chunk = '''</textarea>
<textarea id="console" readonly style="width:98vw;height:40vh;font-family:monospace;background-color:DarkSlateGrey;color:LawnGreen;">
</textarea>
</body>
</html>'''
        fout.write(chunk)
        return fout.getvalue()
    except:
        print('Exception on chunk: {}'.format(chunk))
        traceback.print_exc()

print
print 'Rewriting Csound Reference Manual pages to target...'
print
source_pages = os.listdir(source_html_directory)
for filename in source_pages:
    source_pathname = os.path.join(source_html_directory, filename)
    source_basename = os.path.basename(source_pathname)
    source_filename, source_extension = os.path.splitext(source_basename)
    target_pathname = os.path.join(target_html_directory, filename)
    if (os.path.isfile(source_pathname)):
        if source_extension == '.html':
            print 'Rewriting:', source_pathname, 'to:', target_pathname
            with open(source_pathname, 'r') as source_file:
                source_page = source_file.read()
                source_page = source_page.replace("It uses the file ", "Click to play: ")
                # Imitate a button.
                source_page = source_page.replace('.csd" target="_top">', '.csd.html" target="_top" style="background-color:LightGray;color:Black;padding:.4em;text-decoration:none;font-family:sans-serif;">')
                with open(target_pathname, 'w') as target_file:
                    target_file.write(source_page)
        else:
            print 'Copying:', source_pathname, 'to:', target_pathname
            shutil.copy(source_pathname, target_pathname)
print
print 'Rewriting Csound Reference Manual examples to target...'
print
source_pages = os.listdir(source_examples_directory)
for filename in source_pages:
    source_pathname = os.path.join(source_examples_directory, filename)
    source_basename = os.path.basename(source_pathname)
    source_filename, source_extension = os.path.splitext(source_basename)
    target_pathname = os.path.join(target_examples_directory, filename) + '.html'
    if (os.path.isfile(source_pathname) and source_extension == '.csd'):
        print 'Rewriting:', source_pathname, 'to:', target_pathname
        with open(source_pathname, 'r') as source_file:
            with open(target_pathname, 'w') as target_file:
                source_page = source_file.read()
                target_page = format_playable_example(source_filename, source_page)
                target_file.write(target_page)
