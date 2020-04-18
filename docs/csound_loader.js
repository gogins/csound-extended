/**
 * This script attempts to load, and to use, in order of decreasing 
 * preference: 
 * (1) Injected csound (e.g. Csound for Android or CsoundQt).
 * (2) csound.node.
 * (3) Csound for WebAssembly (CsoundAudioNode, based on AudioWorklet).
 *
 * To use this script, include it at the top of the body of your Web page and 
 * and then call get_csound(csound_message_callback). The result will be a 
 * global csound object, if one is available on your system.
 *
 * Please note, the Csound performance should (and sometimes must) be 
 * configured with sr, ksmps, nchnls, nchnls_i, and sample word format 
 * matching the host platform. 
 *
 * On Linux, PulseAudio may cause problems. It is better to disable PulseAudio
 * and use a low-level ALSA configuration.
 * 
 * On Android, csound.SetMessageCallback does not work. Instead, assign the 
 * message callback function to console.log.
 */
 
// These are globals:

csound_injected = null;
csound_node = null;
csound_audio_node = null;
csound_is_loaded = false;

var load_csound = function(csound_message_callback_) {
    if (typeof csound !== 'undefined') {
        csound_injected = csound;
        csound_is_loaded = true;
        csound_message_callback_("Csound already exists in this JavaScript context.\n");
        return;
    }
    csound = null;
    try {
        csound_message_callback_("Trying to load csound.node...\n");
        csound_node = require('csound.node');
        var nwgui = require('nw.gui');
        nw_window = nwgui.Window.get();
        nw_window.on('close', function() {
            csound_message_callback_('Closing down...\n');
            this.close(true);
        });
        csound_is_loaded = true;
        csound_message_callback_("csound.node is available in this JavaScript context.\n");
        return;
    } catch (e) {
        csound_message_callback_(e + '\n');
    }
    try {
        csound_message_callback_("Trying to load CsoundAudioNode...\n");
        var AudioContext = window.AudioContext || window.webkitAudioContext;
        var audioContext = new AudioContext();
        audioContext.audioWorklet.addModule('CsoundAudioProcessor.js').then(function() {
            csound_message_callback_("Creating CsoundAudioNode...\n");
            csound_audio_node = new CsoundAudioNode(audioContext, csound_message_callback_);
            csound_is_loaded = true;
            csound_message_callback_("CsoundAudioNode (AudioWorklet) is available in this JavaScript context.\n");
        }, function(error) {
           csound_message_callback_(error + '\n');
        });
    } catch (e) {
        csound_message_callback_(e + '\n');
    }
}

/**
 * Returns a singleton instance of Csound, if one is available in the 
 * JavaScript environment. If Csound has not been loaded, attempts are made 
 * to load it from various sources. The csound_message_callback parameter is 
 * required, but console.log can be passed. 
 */
var get_csound = function(csound_message_callback_) {
    if (csound_is_loaded === false) {
        load_csound(csound_message_callback_);
    }
    if (csound_injected != null) {
        csound = csound_injected;
        return csound_injected;
    } else if (csound_node != null) {
        csound = csound_node;
        csound.SetMessageCallback(csound_message_callback_);
       return csound_node;
    } else if (csound_audio_node != null) {
        csound = csound_audio_node;
        csound.SetMessageCallback(csound_message_callback_);
        return csound_audio_node;
     } else {
        csound_message_callback_("Csound is still loading, wait a bit...\n");
    }
}       
