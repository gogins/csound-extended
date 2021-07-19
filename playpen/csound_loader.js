/**
 * This script attempts to load, and to use, in order of decreasing 
 * preference: 
 * (1) Injected csound (e.g. Csound for Android or CsoundQt).
 * (2) csound.node.
 * (3) Csound for WebAssembly (CsoundAudioNode, based on AudioWorklet).
 * 
 * Please note, for WebAudio, code is asynchronous but is wrapped in promises 
 * using the async keyword to make calls behave synchronously; the calling 
 * code (e.g. "Play" button handlers) must therefore also be declared async. 
 * Not only that, but for other platforms handlers should also be declared 
 * async.
 *
 * To use this script, include it at the top of the body of your Web page and 
 * and then call `let csound_ = async get_csound(csound_message_callback).` 
 * The result will be a live csound_ object. Call `get_csound` in this way 
 * in every block of code that will call Csound methods.
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

var get_operating_system = function() {
    let operating_system = "unknown";
    let userAgent = navigator.userAgent || navigator.vendor || window.opera;
    console.log("userAgent: " + userAgent + "\n");
    // Windows Phone must come first because its UA also contains "Android"
    if (/windows phone/i.test(userAgent)) {
        operating_system = "Windows Phone";
        console.log("operating_system: " + operating_system + "\n");
        return operating_system;
    }
    if (/android/i.test(userAgent)) {
        operating_system = "Android";
        console.log("operating_system: " + operating_system + "\n");
        return operating_system;
    }
    // iOS detection from: http://stackoverflow.com/a/9039885/177710
    if (/iPad|iPhone|iPod/.test(userAgent) && !window.MSStream) {
        operating_system = "iOS";
        console.log("operating_system: " + operating_system + "\n");
        return operating_system;
    }
    console.log("operating_system: " + operating_system + "\n");
    return operating_system;
}

/**
 * There is an issue on Android in that csound may be undefined when the page is 
 * first rendered, and be defined only when the user plays the piece.
 */
var load_csound = async function(csound_message_callback_) {
    let operating_system = get_operating_system();
    if (operating_system === "Android" && typeof csound === 'undefined') {
        csound_message_callback("Operating system is Android, but Csound is not yet defined.\n");
        // On Android, Csound uses only stdout for messages; this becomes 
        // console.log, so we assign our "csound message callback" to 
        // console.log.
        return;
    }
    if (typeof csound !== 'undefined') {
        csound_injected = csound;
        csound_is_loaded = true;
        console.log = csound_message_callback;
        csound_message_callback_("Csound is already defined in this JavaScript context.\n");
        return;
    }
    csound = null;
    try {
        csound_message_callback_("Trying to load csound.node...\n");
        csound_node = await require('csound.node');
        var nwgui = await require('nw.gui');
        nw_window = await nwgui.Window.get();
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
        await audioContext.audioWorklet.addModule('CsoundAudioProcessor.js').then(function() {
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
var get_csound = async function(csound_message_callback_) {
    if (csound_is_loaded === false) {
        await load_csound(csound_message_callback_);
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

