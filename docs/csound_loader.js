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
csound_audio_node = null;
csound_extended = {};
if (typeof csound !== 'undefined') {
    csound_injected = csound;
    console.log("Csound has already been injected into this JavaScript context.\n");
}
csound = null;
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
    console.log(e);
}
try {
    var AudioContext = window.AudioContext || window.webkitAudioContext;
    var audioContext = new AudioContext();
    audioContext.audioWorklet.addModule('CsoundAudioProcessor.js').then(function() {
        console.log("Creating CsoundAudioNode...\n");
        csound_audio_node = new CsoundAudioNode(audioContext);
        console.log("Csound audio worklet is available in this JavaScript context.\n");
    }, function(error) {
       console.log(error);
    });
} catch (e) {
    console.log(e);
}
try {
    csound_extended_module(csound_extended).then(function(module) {
        csound_extended = module;
        csound_web_audio = new csound_extended.CsoundWebAudio();
        console.log("csound_extended is available in this JavaSript contex.\n");
    });
} catch (e) {
    console.log(e);
}
var get_csound = function(csound_message_callback) {
    if (csound_injected !== null) {
        csound = csound_injected;
        return csound_injected;
    } 
    if (csound_node !== null) {
        csound = csound_node;
        csound.setMessageCallback(csound_message_callback);
        return csound_node;
    } 
    if (csound_audio_node !== null) {
        csound = csound_audio_node;
        console.log = console.warn = csound_message_callback;
        return csound_audio_node;
    } 
    if (csound_web_audio !== null) {
        csound = csound_web_audio;
        console.log = console.warn = csound_message_callback;
        return csound_web_audio;
    } else {
        csound = null;
        alert("Csound has not yet been loaded, wait a bit...\n");
        return csound;
    }
}       
