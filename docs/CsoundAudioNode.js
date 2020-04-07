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
    resolveCompileCsdText(result) {
        console.log("[" + window.performance.now() + " resolveCompileCsdText with: " + result + ", " + this + "]\n");
        return result;
    }
    resolveCompileOrc(result) {
        return result;
    }
    resolveStop() {
    }
    resolveCleanup(result) {
        return result;
    }
    resolveReset() {
        return;
    }
    async onMessage(event) {
            let data = event.data;
            switch(data[0]) {
                case "Message":
                    console.log(data[1]);
                    break;
                // Some Csound API calls should be serializable, i.e. 
                // synchronous. These cases resolve promises from those calls.
                case "CompileCsdTextResult":
                    console.log("[" + window.performance.now() + " Received CompileCsdTextResult with: " + data[1] + ".]\n");
                    this.resolveCompileCsdText(data[1]);
                    break;
                case "CompileOrcResult":
                    console.log("[" + window.performance.now() + " Received CompileOrcResult with: " + data[1] + ".]\n");
                    this.resolveCompileOrc(data[1]);
                    break;
                case "StopResult":
                    console.log("[" + window.performance.now() + " Received StopResult.]\n");
                    this.resolveStop();
                    break;
                case "CleanupResult":
                    console.log("[" + window.performance.now() + " Received CleanupResult with: " + data[1] + ".]\n");
                    this.resolveCleanup(data[1]);
                    break;
                case "ResetResult":
                    console.log("[" + window.performance.now() + " Received ResetResult.]\n");
                    this.resolveReset();
                    break;
            };
    };
    constructor(context, options) {
        options = options || {};
        options.numberOfInputs  = 1;
        options.numberOfOutputs = 1;
        options.outputChannelCount = [context.destination.channelCount];
        super(context, 'csound-audio-processor', options);
        console.log("CsoundAudioNode constructor...\n");
        this.reset_();
        this.CompileCsdTextPromise = null;
        this.CompileOrcPromise = null;
        this.StopPromise = null;
        this.CleanupPromise = null;
        this.ResetPromise = null;
        this.port.onmessage = this.onMessage.bind(this);
        this.port.start();
    }
    reset_() {
        this.is_playing = false;
        this.is_realtime = false;
        this.microphoneNode = null;
        this.input = null;
        this.output = null;
    }
    async Cleanup() {
        console.log("[" + window.performance.now() + " Cleanup.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCleanup = resolve;
            this.port.postMessage(["Cleanup", csd]);
        });
        let result = await promise;
        console.log("[" + window.performance.now() + " Cleanup resolved with: " + result + ".]\n");
        return result;
    }
    CompileCsd(filename) {
        this.port.postMessage(["CompileCsd", filename]);
    };
    async CompileCsdText(csd) {
        console.log("[" + window.performance.now() + " CompileCsdText.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCompileCsdText = resolve;
            this.port.postMessage(["CompileCsdText", csd]);
        });
        let result = await promise;
        console.log("[" + window.performance.now() + " CompileCsdText resolved with: " + result + ".]\n");
        return result;
    };
    async CompileOrc(orc) {
        console.log("[" + window.performance.now() + " CompileOrc.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCompileOrc = resolve;
            this.port.postMessage(["CompileOrc", orc]);
        });
        let result = await promise;
        console.log("[" + window.performance.now() + " CompileOrc resolved with: " + result + ".]\n");
        return result;
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
        console.log("[" + window.performance.now() + " Perform.]\n");
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
    async Reset() {
        console.log("[" + window.performance.now() + " Reset.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveReset = resolve;
            this.port.postMessage(["Reset"]);
        });
        await promise;
        console.log("[" + window.performance.now() + " Reset resolved.]\n");
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
    // wiring within Csound down in the lower half.console.log
    Start() {
        console.log("[" + window.performance.now() + " Start.]\n");
        try {
            console.log("WebAudio frames per second:         " +  this.context.sampleRate + "\n");
            console.log("WebAudio maximum output channels:   " +  this.context.destination.maxChannelCount + "\n");
            this.connect(this.context.destination);
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
                console.log("MIDI not supported in this context.\n");
            }
            if (this.input !== null) {
                window.navigator = window.navigator || {};
                navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || null;
                if (navigator.getUserMedia === null) {
                    console.log("Audio input not supported in this context.");
                } else {
                    navigator.mediaDevices.getUserMedia({audio: true}).then((stream) => {
                        this.microphoneNode = audioContext.createMediaStreamSource(stream);
                        console.log("WebAudio input channels:            " +  this.microphoneNode.numberOfInputs + "\n");
                        if (this.microphoneNode !== null) {
                            if (inputChannelN != this.microphoneNode.numberOfInputs) {
                                this.microphoneNode.connect(this);
                                console.log("Audio input initialized.\n");
                            } else {
                                console.log("Csound nchnls_i does not match microphoneNode.numberOfInputs.\n");
                            }
                        }       
                    }).catch ((e) => {
                        console.log("Microphone: " + e.name + ". " + e.message + "\n");
                        ///throw "Microphone: " + e.name + ". " + e.message;
                    })
                }
            }
            //this.start();
            //this.disconnect();
            this.port.postMessage(["Start"]);
            this.is_playing = true;
        } catch (e) {
            console.log(e);
        }
    }
    async Stop() {
       console.log("[" + window.performance.now() + " Stop.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveStop = resolve;
            this.port.postMessage(["Stop"]);
            if (this.microphoneNode !== null) {
                this.microphoneNode.stop();
                this.microphoneNode.disconnect(this);
            }
            this.disconnect();
            this.reset_();
        });
        await promise;
        console.log("[" + window.performance.now() + " Stop resolved.]\n");
    }
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




