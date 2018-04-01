#!/bin/bash
echo "Updating JavaScript dependencies of Silencio..."
wget -r "https://cdnjs.cloudflare.com/ajax/libs/dat-gui/0.7.1/dat.gui.js" -O "js/dat.gui.js"
wget -r "https://code.jquery.com/jquery-3.3.1.js" -O "js/jquery.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/mathjs/4.0.1/math.js" -O "js/math.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js" -O "js/MathJax.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/numeric/1.2.6/numeric.js" -O "js/numeric.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/sprintf/1.1.1/sprintf.js" -O "js/sprintf.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/three.js/91/three.js" -O "js/three.js"
wget -r "https://cdnjs.cloudflare.com/ajax/libs/tinycolor/1.4.1/tinycolor.js" -O "js/tinycolor.js"
ls -ll js
echo "Finished updating JavaScript dependencies of Silencio..."
