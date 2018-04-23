/**
 * C S O U N D
 *
 * L I C E N S E
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 */
 
/**
 * So that print output works in all JavaScript contexts, not only 
 * browsers, we set up our own print function to handle all cases.
 */
var print = null;
if (typeof console === 'undefined') {
    print = Module.print;
} else print = function(message) {
    console.log(message);
    Module.print(message);
}

class CsoundAudioNode extends AudioWorkletNode {
    constructor(context) {
        if (typeof context === 'undefined') {
            var AudioContext = window.AudioContext || window.webkitAudioContext;
            var audioContext = new AudioContext();
        }
        context.loadModule("csound-extended.js").then(() => {
            super(context, 'CsoundAudioProcessor');
            this.is_playing = false;
            this.is_realtime = false;
            this.audioProcessNode = null;
            this.microphoneNode = null;
            this.input = null;
            this.output = null;
            // TODO: There is probably some way to 
            // make method calls that return a value cheaply 
            // be synchronous. For now, just set up the port 
            // event handler for bidirectional communication.
            this.port.onmessage = (event) => {
                console.log("CsoundAudioNode.port.onmessage: " + event);
            };
        });
    }
    Cleanup() {
        this.port.postMessage("Cleanup");
    }
    CompileCsd(filename) {
        this.port.postMessage("CompileCsd", filename);
    };
    CompileCsdText(csd) {
        this.port.postMessage("CompileCsdText", csd);
    };
    CompileOrc(orc) {
        this.port.postMessage("CompileOrc", orc);
    };
    Destroy() {
        this.port.postMessage("Destroy");
    };
    EvalCode(code) {
        this.port.postMessage("EvalCode", code);
    };
    // TODO: This and similar cheap functions with needed return values 
    // should be extended with promises to wait for a message back 
    // and thus implement a synchronous function call.
    Get0dBFS() {
        this.port.postMessage("Get0dBFS");
    };
    GetAPIVersion() {
        this.port.postMessage("GetAPIVersion");
    };
    GetControlChannel(name) {
        this.port.postMessage("GetControlChannel", name);
    };
    GetCurrentTimeSamples() {
        this.port.postMessage("GetCurrentTimeSamples");
    };
    GetEnv(name) {
        this.port.postMessage("GetEnv", name);
    };
    GetInputName() {
        this.port.postMessage("GetInputName");
    };
    GetKsmps() {
        this.port.postMessage("GetKsmps");
    };
    GetNchnls() {
        this.port.postMessage("GetNchnls");
    };
    GetNchnlsInput() {
        this.port.postMessage("GetNchnlsInput");
    };
    GetOutputName() {
        this.port.postMessage("GetOutputName");
    };
    GetScoreOffsetSeconds() {
        this.port.postMessage("GetScoreOffsetSeconds");
    };
    GetScoreTime() {
        this.port.postMessage("GetScoreTime");
    };
    GetSr() {
        this.port.postMessage("GetSr");
    };
    GetStringChannel(name) {
        this.port.postMessage("GetStringChannel", name);
    };
    GetVersion() {
        this.port.postMessage("GetVersion");
    };
    InputMessage(text) {
        this.port.postMessage("InputMessage", text);
    };
    IsPlaying() {
        this.port.postMessage("IsPlaying");
    };
    IsScorePending() {
        this.port.postMessage("IsScorePending");
    };
    Message(text) {
        this.port.postMessage("Message", text);
    };
    Perform() {
        this.port.postMessage("Perform");
    };
    ReadScore(score) {
        this.port.postMessage("ReadScore", score);
    };
    Reset() {
        this.port.postMessage("Reset");
    };
    RewindScore() {
        this.port.postMessage("RewindScore");
    };
    SetControlChannel(name, value) {
        this.port.postMessage("SetControlChannel", name, value);
    };
    SetGlobalEnv(name, value) {
        this.port.postMessage("SetGlobalEnv", name, value);
    };
    SetInput(name) {
        this.input = name;
        this.port.postMessage("SetInput", name);
    };
    SetOption(option) {
        if (option.startsWith("-odac")) {
            this.output = option.substr(2);
        } else if (option.startsWith("-iadc")) {
            this.input = option.substr(2);
        }
        this.port.postMessage("SetOption", option);
    };
    SetOutput(name, type, format) {
        this.output = name;
        this.port.postMessage("SetOutput", name, type, format);
    };
    SetScoreOffsetSeconds(seconds) {
        this.port.postMessage("SetScoreOffsetSeconds", seconds);
    };
    SetScorePending(is_pending) {
        this.port.postMessage("SetScorePending", is_pending);
    };
    SetStringChannel(name, value) {
        this.port.postMessage("SetStringChannel", name, value);
    };
    // TODO: Needs much work.
    // Wiring into Web Audio graph here in the upper half, 
    // wiring within Csound down in the lower half.
    Start() {
        var ksmps = 128;///this.csound.GetKsmps();
        bufferFrameCount = this.bufferSize;
        print("Web Audio initialized with frames per buffer: " +  bufferFrameCount + "\n");
        ///this.inputCount = inputChannelCount;
        ///this.outputCount = outputChannelCount;
        var inputChannelN = this.inputCount;
        var outputChannelN = this.outputCount;
        var zerodBFS = 1;///this.csound.Get0dBFS();
        if (input_name.startsWith("adc")) {
            window.navigator = window.navigator || {};
            navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || null;
            if (navigator.getUserMedia === null) {
                print("Audio input not supported in this context.");
            } else {
                function onSuccess(stream) {
                    this.microphoneNode = audioContext.createMediaStreamSource(stream);
                    print("Audio input initialized.\n");
                };
                function onFailure(error) {
                    this.microphoneNode = null;
                    print("Could not initialise audio input, error:" + error);
                };
                navigator.getUserMedia({
                    audio: true
                }, onSuccess, onFailure);
            }
            if (this.microphoneNode !== null) {
                if (inputChannelN >= this.microphoneNode.numberOfInputs) {
                    this.microphoneNode.connect(this);
                } else {
                    print("Csound nchnls_i does not match microphoneNode.numberOfInputs.");
                    return;
                }
            }
            this.port.postMessage("Start");
        }
        this.connect(audioContext.destination);
        this.start();
        this.is_playing = true;
    }
    Stop() {
        this.port.postMessage("Stop");
        this.is_playing = false;
        if (this.microphoneNode !== null) {
            this.microphoneNode.disconnect();
            this.microphoneNode = null;
        }
        if (this.audioProcessNode !== null) {
            this.audioProcessNode.disconnect();
            this.audioProcessNode = null;
        }
    };
    TableGet(number, index) {
        this.port.postMessage("TableGet", number, index);
    }
    TableLength(number) {
        this.port.postMessage("TableLength", number);
    }
    TableSet(number, index, value) {
        this.port.postMessage("TableSet", index, value);
    }
}

Module["CsoundAudioNode"] = CsoundAudioNode;
/**
 * In order to follow the same pattern of use as in all other Csound 
 * JavaScript environments, a global Csound object is injected into the 
 * JavaScript context, and the user is notified that Csound is ready.
 */
Module["onRuntimeInitialized"] = function() {
    csound = new Module.CsoundAudioNode();
    print("\nCsoundAudioNode has now been loaded; its functions may now be called.\n");
}



