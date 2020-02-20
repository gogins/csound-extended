#!/bin/bash
# To create a command key binding in SciTE add the following lines to the
# user properties file:
# command.go.$(file.patterns.html)=~/run_nwjs_application.sh $(FileNameExt) $(FileName) $(FileDir)
# command.go.subsystem.$(file.patterns.html)=0
# Similar customization could be done in other text editors. The purpose of this code is to
# automatically generate a NW.js application manifest for the current HTML file.
echo ps -ef | fgrep "*.exe"
killall -q -9 exe
echo Building package.json file...
read -r -d '' json_format << EOM
{
  "main": "%s",
  "name": "%s",
  "description": "HTML5 with Csound",
  "version": "0.1.0",
  "keywords": [ "Csound", "node-webkit" ],
  "nodejs": true,
  "node-remote": "http://<all-urls>/*",
  "window": {
    "title": "%s",
    "icon": "link.png",
    "toolbar": false,
    "frame": false,
    "maximized": true,
    "position": "mouse",
    "fullscreen": true
  },
  "webkit": {
    "plugin": true
  }
}
EOM
printf -v json_text "$json_format" $1 $2 $2
echo "$json_text" | tee package.json
# Change this if necessary to your nw pathname.
~/nwjs/nw --context-mixed --experimental-modules $3
