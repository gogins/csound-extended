#!/bin/bash
echo "Updating all JavaScript dependencies that come from CDNs..."
# Please note, we do NOT want minified files if we can avoid them.
cd ../silencio/js
# <script src="https://cdn.jsdelivr.net/npm/algebrite@1/dist/algebrite.bundle-for-browser.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/algebrite@1/dist/algebrite.bundle-for-browser.js"
# <script src="https://cdn.jsdelivr.net/npm/codemirror@5/lib/codemirror.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/codemirror@5/lib/codemirror.js"
# <script src="https://cdn.jsdelivr.net/npm/dat.gui@0.7.6/build/dat.gui.min.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/dat.gui@0.7.6/build/dat.gui.min.js"
# <script src="https://cdn.jsdelivr.net/npm/jquery@3/dist/jquery.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/jquery@3/dist/jquery.js"
# <script src="https://cdn.jsdelivr.net/npm/super-table@1/jquery.super-table.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/super-table@1/jquery.super-table.js"
# <script src="https://cdn.jsdelivr.net/npm/mathjax@2/unpacked/MathJax.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/mathjax@2/unpacked/MathJax.js"
# <script src="https://cdn.jsdelivr.net/npm/mathjs@6/dist/math.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/mathjs@6/dist/math.js"
# <script src="https://cdn.jsdelivr.net/npm/@tmcw/numeric@1/dist/numeric.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/@tmcw/numeric@1/dist/numeric.js"
# <script src="https://cdn.jsdelivr.net/npm/p5@0.9.0/lib/p5.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/p5@0.9.0/lib/p5.js"
# <script src="https://cdn.jsdelivr.net/npm/sprintf-js@1/src/sprintf.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/sprintf-js@1/src/sprintf.js"
# <script src="https://cdn.jsdelivr.net/npm/three@0.107.0/build/three.js"></script>
wget --backups=1 "https://cdn.jsdelivr.net/npm/three@0.107.0/build/three.js"
# https://cdnjs.cloudflare.com/ajax/libs/tinycolor/1.4.1/tinycolor.js
wget --backups=1 https://cdnjs.cloudflare.com/ajax/libs/tinycolor/1.4.1/tinycolor.js
ls -ll *.js 
cd ../../dependencies
echo "Finished updating all JavaScript dependencies that come from CDNs."
