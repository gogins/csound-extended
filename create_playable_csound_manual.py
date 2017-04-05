'''
C R E A T E   P L A Y A B L E   C S O U N D   M A N U A L

Copyright (c) 2017 by Michael Gogins.
Licensed under the terms of the GNU Lesser Public License version 2

This Python script creates a modified version of the Csound Reference Manual
in which the example CSDs will play online in Google Chrome using the PNaCl
build of Csound. The target is the gogins.github.io GitHub repository.
'''

print __doc__

import cStringIO
import glob
import os
import os.path
import shutil
import traceback

source_home = r'''D:\msys64\home\restore\manual'''
source_html_directory = os.path.join(source_home, 'html')
source_examples_directory = os.path.join(source_html_directory, 'examples')

target_home = r'''C:\Users\restore\gogins.github.io'''
target_html_directory = os.path.join(target_home, 'csound', 'html')
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
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>adsr</title
<link rel="stylesheet" type="text/css" href="csound.css" />
</head>
<h1>'''
        fout.write(chunk)
        fout.write(html_filename)
        chunk = '''</h1>
<p>
This example will play if your Web browser is a desktop version of Google Chrome with PNaCl enabled. You can edit and replay the code. At this time, most but not all examples will run in PNaCl.
</p>
<p>
<input type="button" value="Play" onclick="start_onclick()"/>
<input type="button" value="Stop" onclick="stop_onclick()"/>
<p>
<textarea id="csd" style="width: 100%; height: 50%;font-size:12px;">'''
        fout.write(chunk)
        text = text.replace('nchnls ', 'nchnls = 2 ; Changed for PNaCl output from: ')
        fout.write(text)
        chunk = '''</textarea>
<h3>Csound Messages</h3>
<textarea id="console" readonly style="width: 100%; height: 25%;font-size:12px;">
</textarea>
<script type="text/javascript" src="../../csound.js"></script>
<script>
function handleMessage(message) {
console.log(message);
var messages_textarea = document.getElementById("console");
var existing = messages_textarea.value;
messages_textarea.value = existing + message.data;
messages_textarea.scrollTop = messages_textarea.scrollHeight;
}
function sleep (time) {
return new Promise((resolve) => setTimeout(resolve, time));
}
function start_onclick() {
csound.stop();
sleep(500).then(() => {
    var csd = document.getElementById('csd').value;
    csound.compileCsdText(csd);
})
}
function stop_onclick () {
csound.stop();
}
</script>
<div id="engine"></div>
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
                source_page = source_page.replace('.csd" target="_top">', '.csd.html" target="_top">')
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

