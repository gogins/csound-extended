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

let print_;
if (typeof csound_message_callback === 'undefined') {
    print_ = console.log;
} else {
    print_ = csound_message_callback;
}

if (typeof csound !== 'undefined') {
    csound_injected = csound;
    print_("Csound has already been injected into this JavaScript context.\n");
}
csound = null;
try {
    print_("Trying to load csound.node...\n");
    csound_node = require('csound.node');
    var nwgui = require('nw.gui');
    nw_window = nwgui.Window.get();
    nw_window.on('close', function() {
        print_('Closing down...\n');
        this.close(true);
    });
    print_("csound.node is available in this JavaScript context.\n");
    // Workaround for Emscripten not knowing that NW.js is a variant of Node.js.
    csound_extended["ENVIRONMENT"] = "NODE";
} catch (e) {
    print_(e + '\n');
}
try {
    print_("Trying to load CsoundAudioNode...\n");
    var AudioContext = window.AudioContext || window.webkitAudioContext;
    var audioContext = new AudioContext();
    audioContext.audioWorklet.addModule('CsoundAudioProcessor.js').then(function() {
        print_("Creating CsoundAudioNode...\n");
        csound_audio_node = new CsoundAudioNode(audioContext);
        print_("Csound audio worklet is available in this JavaScript context.\n");
    }, function(error) {
       print_(error + '\n');
    });
} catch (e) {
    print_(e + '\n');
}
try {
    print_("Trying to load csound_extended...");
    csound_extended_module(csound_extended).then(function(module) {
        csound_extended = module;
        csound_web_audio = new csound_extended.CsoundWebAudio();
        print_("csound_extended is available in this JavaSript contex.\n");
    });
} catch (e) {
    print_(e + '\n');
}

var get_csound = function(csound_message_callback_) {
    if (csound_injected !== null) {
        csound = csound_injected;
        return csound_injected;
    } 
    if (csound_node !== null) {
        csound = csound_node;
        csound.setMessageCallback(csound_message_callback_);
        return csound_node;
    } 
    if (csound_audio_node !== null) {
        csound = csound_audio_node;
        console.log = console.warn = csound_message_callback_;
        return csound_audio_node;
    } 
    if (csound_web_audio !== null) {
        csound = csound_web_audio;
        console.log = console.warn = csound_message_callback_;
        return csound_web_audio;
    } else {
        csound = null;
        alert("Csound has not yet been loaded, wait a bit...\n");
        return csound;
    }
}       
