/**
 * This script attempts to load, and to use, in order of decreasing 
 * preference: 
 * (1) Injected csound (e.g. Csound for Android, CsoundQt).
 * (2) csound.node.
 * (3) Csound for WebAssembly (csound_extended).
 * To use this script, include it in your Web page and then call 
 * get_csound(csound_message_callback). The result will be a global 
 * csound object, if one is available on your system.
 */
 
 // These are globals:
csound_injected = null;
csound_node = null;
csound_web_audio = null;
csound = null;
csound_extended = {};
if (typeof csound !== 'undefined') {
    csound_injected = csound;
    console.log("Csound has already been injected into this JavaScript context.\n");
}
try {
    csound_node = require('csound.node');
    var nwgui = require('nw.gui');
    nw_window = nwgui.Window.get();
    nw_window.on('close', function() {
        console.log('Closing down...');
        this.close(true);
    });
    console.log("csound.node is available in this JavaScript context.\n");
    // Workaround for Emscripten not knowing that NW.js is a variant of Node.js.
    csound_extended["ENVIRONMENT"] = "NODE";
} catch (e) {
    console.warn(e);
}
try {
    csound_extended_module(csound_extended).then(function(module) {
        csound_extended = module;
        csound_web_audio = new csound_extended.CsoundWebAudio();
        console.log("csound_extended is available in this JavaSript contex.\n");
    });
} catch (e) {
    console.warn(e);
}
var get_csound = function(csound_message_callback) {
    if (csound_injected !== null) {
        csound = csound_injected;
        return csound_injected;
    } else if (csound_node !== null) {
        csound = csound_node;
        csound.setMessageCallback(csound_message_callback);
        return csound_node;
    } else if (csound_web_audio !== null) {
        csound = csound_web_audio;
        console.log = console.warn = csound_message_callback;
        return csound_web_audio;
    } else {
        csound = null;
        console.log("Csound has not yet been loaded, wait a bit...\n");
        return csound;
    }
}       
