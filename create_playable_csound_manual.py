'''
C R E A T E   P L A Y A B L E   C S O U N D   M A N U A L

Copyright (c) 2017 by Michael Gogins.
Licensed under the terms of the GNU Lesser Public License version 2

This Python script creates a modified version of the Csound Reference Manual
in which the example CSDs will play online in Google Chrome using the WebAssembly
build of Csound. The target is the docs directory of the csound-extended repository.

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
The usr must customize these variables. No additional configuration should be
required.
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
<body>
    <script src="CsoundAudioNode.js"></script>
    <script src="csound_loader.js"></script>
    <script>
    var handleMessage = function(message) {
        var messages_textarea = document.getElementById("console");
        var existing = messages_textarea.value;
        messages_textarea.value = existing + '\\n' + message;
        messages_textarea.scrollTop = messages_textarea.scrollHeight;
    }
    var onPlayClick = function() {
        var csd = document.getElementById('csd').value;
        csound = get_csound(handleMessage);
        if (csound == null) {
            handleMessage("Still loading, try again in a bit...");
        }
        csound.CompileCsdText(csd);
        csound.Start();
        csound.Perform();
    }
    var onPlayStop = function() {
        csound = get_csound(handleMessage);
        csound.Stop();
    }
  </script>
<h1>'''
        fout.write(chunk)
        fout.write(html_filename)
        chunk = '''</h1>
<p>
This example will play if your Web browser is a desktop version of Google Chrome with WebAssembly enabled. You can edit and replay the code. At this time, most but not all examples will run in WebAssembly.
</p>
<p>
<input type="button" value="Play" onclick="onPlayClick()"/>
<input type="button" value="Stop" onclick="onPlayStop()"/>
<p>
<textarea id="csd" style="width: 100%; height: 50%;font-size:12px;">'''
        fout.write(chunk)
        text = text.replace('nchnls ', 'nchnls = 2 ; Changed for WebAssembly output from: ')
        fout.write(text)
        chunk = '''</textarea>
<h3>Csound Messages</h3>
<textarea id="console" readonly style="width: 100%; height: 25%;font-size:12px;">
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
                source_page = source_page.replace('.csd" target="_top">', '.csd.html" target="_top"> (click here to play) ')
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
