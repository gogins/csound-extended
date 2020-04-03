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
 
// import csound_audio_processor_module from 'CsoundAudioProcessor.js';

class CsoundAudioNode extends AudioWorkletNode {
    constructor(context, options) {
        options = options || {};
        options.numberOfInputs  = 1;
        options.numberOfOutputs = 1;
        options.outputChannelCount = [context.destination.channelCount];
        super(context, 'csound-audio-processor', options);
        console.log("CsoundAudioNode constructor...\n");
        this.reset_();
        this.port.onmessage = (event) => {
            let data = event.data;
            switch(data[0]) {
                case "Start":
                    //this.start();
                    break;
                case "Message":
                    console.log(data[1]);
                    break;
            }
        }
        this.port.start();
    }
    reset_() {
        this.is_playing = false;
        this.is_realtime = false;
        this.microphoneNode = null;
        this.input = null;
        this.output = null;
    }
    Cleanup() {
        this.port.postMessage(["Cleanup"]);
    }
    CompileCsd(filename) {
        this.port.postMessage(["CompileCsd", filename]);
    };
    CompileCsdText(csd) {
        this.port.postMessage(["CompileCsdText", csd]);
    };
    CompileOrc(orc) {
        this.port.postMessage(["CompileOrc", orc]);
    };
    Destroy() {
        this.port.postMessage(["Destroy"]);
    };
    EvalCode(code) {
        this.port.postMessage(["EvalCode", code]);
    };
    Get0dBFS() {
        this.port.postMessage(["Get0dBFS"]);
    };
    GetAPIVersion() {
        this.port.postMessage(["GetAPIVersion"]);
    };
    GetControlChannel(name) {
        this.port.postMessage(["GetControlChannel", name]);
    };
    GetCurrentTimeSamples() {
        this.port.postMessage(["GetCurrentTimeSamples"]);
    };
    GetEnv(name) {
        this.port.postMessage(["GetEnv", name]);
    };
    GetInputName() {
        this.port.postMessage(["GetInputName"]);
    };
    GetKsmps() {
        this.port.postMessage(["GetKsmps"]);
    };
    GetNchnls() {
        this.port.postMessage(["GetNchnls"]);
    };
    GetNchnlsInput() {
        this.port.postMessage(["GetNchnlsInput"]);
    };
    GetOutputName() {
        this.port.postMessage(["GetOutputName"]);
    };
    GetScoreOffsetSeconds() {
        this.port.postMessage(["GetScoreOffsetSeconds"]);
    };
    GetScoreTime() {
        this.port.postMessage(["GetScoreTime"]);
    };
    GetSr() {
        this.port.postMessage(["GetSr"]);
    };
    GetStringChannel(name) {
        this.port.postMessage(["GetStringChannel", name]);
    };
    GetVersion() {
        this.port.postMessage(["GetVersion"]);
    };
    InputMessage(text) {
        this.port.postMessage(["InputMessage", text]);
    };
    IsPlaying() {
        this.port.postMessage(["IsPlaying"]);
    };
    IsScorePending() {
        this.port.postMessage(["IsScorePending"]);
    };
    Message(text) {
        this.port.postMessage(["Message", text]);
    };
    Perform() {
        this.port.postMessage(["Perform"]);
    };
    /**
     * Because AudioWorklet messages are asynchronous, a sequence 
     * of method calls cannot be guaranteed to execute in proper order. 
     * Hence, this helper.
     */
    PerformCsd(options, csd) {
        this.port.postMessage(["PerformCsd", options, csd]);
    }
    /**
     * Because AudioWorklet messages are asynchronous, a sequence 
     * of method calls cannot be guaranteed to execute in proper order. 
     * Hence, this helper.
     */
    PerformOrc(options, orc, sco) {
        this.port.postMessage(["PerformOrc", options, orc, sco]);
    }
    ReadScore(score) {
        this.port.postMessage(["ReadScore", score]);
    };
    Reset() {
        this.port.postMessage(["Reset"]);
    };
    RewindScore() {
        this.port.postMessage(["RewindScore"]);
    };
    SetControlChannel(name, value) {
        this.port.postMessage(["SetControlChannel", name, value]);
    };
    SetGlobalEnv(name, value) {
        this.port.postMessage(["SetGlobalEnv", name, value]);
    };
    SetInput(name) {
        this.input = name;
        this.port.postMessage(["SetInput", name]);
    };
    SetOption(option) {
        if (option.startsWith("-odac")) {
            this.output = option.substr(2);
        } else if (option.startsWith("-iadc")) {
            this.input = option.substr(2);
        }
        this.port.postMessage(["SetOption", option]);
    };
    SetOutput(name, type, format) {
        this.output = name;
        this.port.postMessage(["SetOutput", name, type, format]);
    };
    SetScoreOffsetSeconds(seconds) {
        this.port.postMessage(["SetScoreOffsetSeconds", seconds]);
    };
    SetScorePending(is_pending) {
        this.port.postMessage(["SetScorePending", is_pending]);
    };
    SetStringChannel(name, value) {
        this.port.postMessage(["SetStringChannel", name, value]);
    };
    // Wiring into Web Audio graph here in the upper half, 
    // wiring within Csound down in the lower half.
    Start() {
        try {
            this.Message("WebAudio frames per second:         " +  this.context.sampleRate + "\n");
            this.Message("WebAudio maximum output channels:   " +  this.context.destination.maxChannelCount + "\n");
            let onMidiEvent = function(event) {
                this.port.postMessage(["MidiEvent", event.data[0], event.data[1], event.data[2]]);
            };
            let midiSuccess = function(midiInterface) {
                let inputs = midiInterface.inputs.values();
                console.log("MIDI input initialized...\n");
                for (let input = inputs.next(); input && !input.done; input = inputs.next()) {
                    input = input.value;
                    console.log("Input: " + input.name + "\n");
                    input.onmidimessage = onMidiEvent;
                }
            };
            let midiFail = function(error) {
                console.log("MIDI failed to start, error:" + error);
            };
            if (navigator.requestMIDIAccess) {
                navigator.requestMIDIAccess().then(midiSuccess, midiFail);
            } else {
                console.log("MIDI not supported in this context.");
            }
            if (this.input !== null) {
                window.navigator = window.navigator || {};
                navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || null;
                if (navigator.getUserMedia === null) {
                    this.Message("Audio input not supported in this context.");
                } else {
                    navigator.mediaDevices.getUserMedia({audio: true}).then((stream) => {
                        this.microphoneNode = audioContext.createMediaStreamSource(stream);
                        this.Message("WebAudio input channels:            " +  this.microphoneNode.numberOfInputs + "\n");
                        if (this.microphoneNode !== null) {
                            if (inputChannelN != this.microphoneNode.numberOfInputs) {
                                this.microphoneNode.connect(this);
                                this.Message("Audio input initialized.\n");
                            } else {
                                this.Message("Csound nchnls_i does not match microphoneNode.numberOfInputs.");
                            }
                        }       
                    }).catch ((e) => {
                        this.Message("Microphone: " + e.name + ". " + e.message + "\n");
                        ///throw "Microphone: " + e.name + ". " + e.message;
                    })
                }
            }
            this.disconnect();
            this.connect(this.context.destination);
            this.port.postMessage(["Start"]);
            this.is_playing = true;
        } catch (e) {
            console.log(e);
        }
    }
    Stop() {
        this.port.postMessage(["Stop"]);
        if (this.microphoneNode !== null) {
            this.microphoneNode.stop();
            this.microphoneNode.disconnect(this);
        }
        this.disconnect();
        this.reset_();
    };
    TableGet(number, index) {
        this.port.postMessage(["TableGet", number, index]);
    }
    TableLength(number) {
        this.port.postMessage(["TableLength", number]);
    }
    TableSet(number, index, value) {
        this.port.postMessage(["TableSet", index, value]);
    }    
}




