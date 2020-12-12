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
    resolveCleanup(result) {
        return result;
    }
    resolveCompileCsdText(result) {
        this.message_callback("[" + window.performance.now() + " resolveCompileCsdText with: " + result + ", " + this + "]\n");
        return result;
    }
    resolveCompileOrc(result) {
        return result;
    }
    resolveGetControlChannel(result) {
        return result;
    }
    resolveReadScore(result) {
        return result;
    }
    resolveReset() {
        return;
    }
    resolveStop() {
        return;
    }
    async onMessage(event) {
            let data = event.data;
            switch(data[0]) {
                case "Message":
                    if (this.message_callback != null) {
                        this.message_callback(data[1]);
                    } else {
                        console.log(data[1]);
                    }
                    break;
                // Some Csound API calls should be serializable, i.e. 
                // synchronous. These cases resolve promises (above) from those calls.
                case "CleanupResult":
                    // this.message_callback("[" + window.performance.now() + " Received CleanupResult with: " + data[1] + ".]\n");
                    this.resolveCleanup(data[1]);
                    break;
                case "CompileOrcResult":
                    // this.message_callback("[" + window.performance.now() + " Received CompileOrcResult with: " + data[1] + ".]\n");
                    this.resolveCompileOrc(data[1]);
                    break;
                case "CompileCsdTextResult":
                    // this.message_callback("[" + window.performance.now() + " Received CompileCsdTextResult with: " + data[1] + ".]\n");
                    this.resolveCompileCsdText(data[1]);
                    break;
                case "GetControlChannelResult":
                     // this.message_callback("[" + window.performance.now() + " Received GetControlChannelResult with: " + data[1] + ".]\n");
                    this.resolveGetControlChannel(data[1]);
                    break;
                case "ReadScoreResult":
                    // this.message_callback("[" + window.performance.now() + " Received ReadScoreResult with: " + data[1] + ".]\n");
                    this.resolveReadScore(data[1]);
                    break;
                case "ResetResult":
                    // this.message_callback("[" + window.performance.now() + " Received ResetResult.]\n");
                    this.resolveReset();
                    break;
                case "StopResult":
                    // this.message_callback("[" + window.performance.now() + " Received StopResult.]\n");
                    this.resolveStop();
                    break;
            };
    };
    constructor(context, message_callback_, options) {
        options = options || {};
        options.numberOfInputs  = 1;
        options.numberOfOutputs = 1;
        options.outputChannelCount = [context.destination.channelCount];
        super(context, 'csound-audio-processor', options);
        this.message_callback = message_callback_;
        this.message_callback("CsoundAudioNode constructor...\n");
        this.reset_();
        this.CompileCsdTextPromise = null;
        this.CompileOrcPromise = null;
        this.StopPromise = null;
        this.CleanupPromise = null;
        this.ReadScorePromise = null;
        this.ResetPromise = null;
        this.port.onmessage = this.onMessage.bind(this);
        this.port.start();
    }
    reset_() {
        this.is_playing = false;
        this.is_realtime = false;
        this.userMediaAudioInputNode = null;
        this.input = null;
        this.output = null;
    }
    async Cleanup() {
        // this.message_callback("[" + window.performance.now() + " Cleanup.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCleanup = resolve;
            this.port.postMessage(["Cleanup"]);
        });
        let result = await promise;
        // this.message_callback("[" + window.performance.now() + " Cleanup resolved with: " + result + ".]\n");
        return result;
    }
    CompileCsd(filename) {
        this.port.postMessage(["CompileCsd", filename]);
    };
    async CompileCsdText(csd) {
        // this.message_callback("[" + window.performance.now() + " CompileCsdText.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCompileCsdText = resolve;
            this.port.postMessage(["CompileCsdText", csd]);
        });
        let result = await promise;
        // this.message_callback("[" + window.performance.now() + " CompileCsdText resolved with: " + result + ".]\n");
        return result;
    };
    async CompileOrc(orc) {
        // this.message_callback("[" + window.performance.now() + " CompileOrc.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveCompileOrc = resolve;
            this.port.postMessage(["CompileOrc", orc]);
        });
        let result = await promise;
        // this.message_callback("[" + window.performance.now() + " CompileOrc resolved with: " + result + ".]\n");
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
        return this.port.postMessage(["GetControlChannel", name]);
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
        // this.message_callback("[" + window.performance.now() + " Perform.]\n");
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
    async ReadScore(score) {
        // this.message_callback("[" + window.performance.now() + " ReadScore.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveReadScore = resolve;
            this.port.postMessage(["ReadScore", score]);
        });
        let result = await promise;
        // this.message_callback("[" + window.performance.now() + " ReadScore resolved with: " + result + ".]\n");
        return result;
    };
    async Reset() {
        // this.message_callback("[" + window.performance.now() + " Reset.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveReset = resolve;
            this.port.postMessage(["Reset"]);
        });
        await promise;
        // this.message_callback("[" + window.performance.now() + " Reset resolved.]\n");
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
    SetMessageCallback(message_callback_) {
        this.message_callback = message_callback_;
    }
    SetOption(option) {
        if (option.startsWith("-odac")) {
            this.output = option.substr(2);
        }
        if (option.startsWith("-iadc")) {
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
    // Wiring into the Web Audio graph is up here in the upper half, 
    // wiring within Csound is down in the lower half.
    async Start() {
        // this.message_callback("[" + window.performance.now() + " Start.]\n");
        try {
            let device_list = await navigator.mediaDevices.enumerateDevices();
            var message_callback_ = this.message_callback;
            var index = 0;
            var input_connected = false;
            var print_device = function(device) {
                message_callback_("mediaDevices: " + index + " " + device.kind + ": " + device.label + "\n");
                index++;
            };     
            device_list.forEach(print_device);
            this.message_callback("WebAudio frames per second:         " +  this.context.sampleRate + "\n");
            this.message_callback("WebAudio maximum output channels:   " +  this.context.destination.maxChannelCount + "\n");
            this.connect(this.context.destination);
            if (navigator.requestMIDIAccess) {
              let midi_access = await navigator.requestMIDIAccess();
              const inputs = midi_access.inputs.values();
              var post_message = this.postMessage;
              for (let entry of midi_access.inputs) {
                  var port_ = entry[1];
                  message_callback_("MIDI port: type: " + port_.type + "  manufacturer: " + port_.manufacturer + " name: " + port_.name +
                      " version: " + port_.version + "\n");
                  // Using the MessagePort for this is probably not good enough.
                  port_.onmidimessage = function(event) {
                      post_message(["MidiEvent", event.data[0], event.data[1], event.data[2]]);
                  };
              }
              for (let entry of midi_access.outputs) {
                  var port_ = entry[1];
                  message_callback_( "MIDI port: type: " + port_.type + " manufacturer: " + port_.manufacturer + " name: " + port_.name +
                    " version: " + port_.version + "\n");
              }
            }
            // Try to obtain the Web browser audio input, if available.
            // Not to be confused with any other audio input interfaces on the 
            // computer, which are inputs in the device list above!
            try {
                this.message_callback("Trying to open browser audio input...\n")
                let stream = await navigator.mediaDevices.getUserMedia({audio: true});
                this.userMediaAudioInputNode = this.context.createMediaStreamSource(stream);
                this.message_callback("WebAudio UserMedia outputs:         " +  this.userMediaAudioInputNode.numberOfOutputs + "\n");
                this.userMediaAudioInputNode.connect(this);
                this.message_callback("Audio input initialized.\n");
            } catch (e) {
                this.message_callback(e + "\n");
            }
            this.port.postMessage(["Start"]);
            this.is_playing = true;
            this.message_callback("is_playing...\n");
        } catch (e) {
            this.message_callback(e);
        }
    }
    async Stop() {
        this.message_callback("[" + window.performance.now() + " Stop.]\n");
        let promise = new Promise((resolve, reject) => {
            // Not exactly intuitive!
            this.resolveStop = resolve;
            this.port.postMessage(["Stop"]);
            if (this.userMediaAudioInputNode !== null) {
                ///this.userMediaAudioInputNode.stop();
                this.userMediaAudioInputNode.disconnect(this);
            }
            this.disconnect();
            this.reset_();
        });
        await promise;
        this.message_callback("[" + window.performance.now() + " Stop resolved.]\n");
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




