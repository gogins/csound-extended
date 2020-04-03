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

// Workaround for Emscripten #8774.

var tempDouble;
var tempI64;
 
/**
 * So that print output works in all JavaScript contexts, not only 
 * browsers, we set up our own print function to handle all cases.
 */
var print = null;
if (typeof console === 'undefined') {
    print = Module.print;
} else {
    print = console.log;
}

var CsoundWebAudio = function() {
    this.csound = new Module.CsoundEmbind();
    this.is_playing = false;
    this.is_realtime = false;
    this.audioProcessNode = null;
    this.microphoneNode = null;
};

CsoundWebAudio.prototype.Cleanup = function() {
    this.csound.Cleanup();
};
CsoundWebAudio.prototype.cleanup = CsoundWebAudio.prototype.Cleanup;

CsoundWebAudio.prototype.CompileCsd = function(filename) {
    this.csound.CompileCsd(filename);
};
CsoundWebAudio.prototype.compileCsd = CsoundWebAudio.prototype.CompileCsd;

CsoundWebAudio.prototype.CompileCsdText = function(csd) {
    this.csound.CompileCsdText(csd);
};
CsoundWebAudio.prototype.compileCsdText = CsoundWebAudio.prototype.CompileCsdText;

CsoundWebAudio.prototype.CompileOrc = function(orc) {
    this.csound.CompileOrc(orc);
};
CsoundWebAudio.prototype.compileOrc = CsoundWebAudio.prototype.CompileOrc;

CsoundWebAudio.prototype.Destroy = function() {
    this.csound.delete();
};
CsoundWebAudio.prototype.destroy = CsoundWebAudio.prototype.Destroy;

CsoundWebAudio.prototype.EvalCode = function(code) {
    return this.csound.EvalCode(code);
};
CsoundWebAudio.prototype.evalCode = CsoundWebAudio.prototype.EvalCode;

CsoundWebAudio.prototype.Get0dBFS = function() {
    return this.csound.Get0dBFS();
};
CsoundWebAudio.prototype.get0dBFS = CsoundWebAudio.prototype.Get0dBFS;

CsoundWebAudio.prototype.GetAPIVersion = function() {
    return this.csound.GetAPIVersion();
};
CsoundWebAudio.prototype.getAPIVersion = CsoundWebAudio.prototype.GetAPIVersion;

CsoundWebAudio.prototype.GetControlChannel = function(name) {
    return this.csound.GetControlChannel(name);
};
CsoundWebAudio.prototype.getControlChannel = CsoundWebAudio.prototype.GetControlChannel;

CsoundWebAudio.prototype.GetCurrentTimeSamples = function() {
    return this.csound.GetCurrentTimeSamples();
};
CsoundWebAudio.prototype.getCurrentTimeSamples = CsoundWebAudio.prototype.GetCurrentTimeSamples;

CsoundWebAudio.prototype.GetEnv = function(name) {
    return this.csound.GetEnv(name);
};
CsoundWebAudio.prototype.getEnv = CsoundWebAudio.prototype.GetEnv;

CsoundWebAudio.prototype.GetInputName = function() {
    return this.csound.GetInputName();
};
CsoundWebAudio.prototype.getInputName = CsoundWebAudio.prototype.GetInputName;

CsoundWebAudio.prototype.GetKsmps = function() {
    return this.csound.GetKsmps();
};
CsoundWebAudio.prototype.getKsmps = CsoundWebAudio.prototype.GetKsmps;

CsoundWebAudio.prototype.GetNchnls = function() {
    return this.csound.GetNchnls();
};
CsoundWebAudio.prototype.getNchnls = CsoundWebAudio.prototype.GetNchnls;

CsoundWebAudio.prototype.GetNchnlsInput = function() {
    return this.csound.GetNchnlsInput();
};
CsoundWebAudio.prototype.getNchnlsInput = CsoundWebAudio.prototype.GetNchnlsInput;

CsoundWebAudio.prototype.GetOutputName = function() {
    return this.csound.GetOutputName();
};
CsoundWebAudio.prototype.getOutputName = CsoundWebAudio.prototype.GetOutputName;

CsoundWebAudio.prototype.GetScoreOffsetSeconds = function() {
    return this.csound.GetScoreOffsetSeconds();
};
CsoundWebAudio.prototype.getScoreOffsetSeconds = CsoundWebAudio.prototype.GetScoreOffsetSeconds;

CsoundWebAudio.prototype.GetScoreTime = function() {
    return this.csound.GetScoreTime();
};
CsoundWebAudio.prototype.getScoreTime = CsoundWebAudio.prototype.GetScoreTime;

CsoundWebAudio.prototype.GetSr = function() {
    return this.csound.GetSr();
};
CsoundWebAudio.prototype.getSr = CsoundWebAudio.prototype.GetSr;

CsoundWebAudio.prototype.GetStringChannel = function(name) {
    return this.csound.GetStringChannel(name);
};
CsoundWebAudio.prototype.getStringChannel = CsoundWebAudio.prototype.GetStringChannel;

CsoundWebAudio.prototype.GetVersion = function() {
    return this.csound.GetVersion();
};
CsoundWebAudio.prototype.getVersion = CsoundWebAudio.prototype.GetVersion;

CsoundWebAudio.prototype.InputMessage = function(text) {
    this.csound.InputMessage(text);
};
CsoundWebAudio.prototype.inputMessage = CsoundWebAudio.prototype.InputMessage;

CsoundWebAudio.prototype.IsPlaying = function() {
    return this.is_playing;
};
CsoundWebAudio.prototype.isPlaying = CsoundWebAudio.prototype.IsPlaying;

CsoundWebAudio.prototype.IsScorePending = function() {
    return this.csound.IsScorePending();
};
CsoundWebAudio.prototype.isScorePending = CsoundWebAudio.prototype.IsScorePending;

CsoundWebAudio.prototype.Message = function(text) {
    return this.csound.Message(text);
};
CsoundWebAudio.prototype.message = CsoundWebAudio.prototype.Message;

CsoundWebAudio.prototype.Perform = function() {
    if (this.is_realtime) {
        return 0;
    } else {
        return this.csound.Perform();
    }
};
CsoundWebAudio.prototype.perform = CsoundWebAudio.prototype.Perform;

CsoundWebAudio.prototype.ReadScore = function(score) {
    return this.csound.ReadScore(score);
};
CsoundWebAudio.prototype.readScore = CsoundWebAudio.prototype.ReadScore;

CsoundWebAudio.prototype.Reset = function() {
    return this.csound.Reset();
};
CsoundWebAudio.prototype.reset = CsoundWebAudio.prototype.Reset;

CsoundWebAudio.prototype.RewindScore = function() {
    return this.csound.RewindScore();
};
CsoundWebAudio.prototype.rewindScore = CsoundWebAudio.prototype.RewindScore;

CsoundWebAudio.prototype.SetControlChannel = function(name, value) {
    return this.csound.SetControlChannel(name, value);
};
CsoundWebAudio.prototype.setControlChannel = CsoundWebAudio.prototype.SetControlChannel;

CsoundWebAudio.prototype.SetGlobalEnv = function(name, value) {
    return this.csound.SetGlobalEnv(name, value);
};
CsoundWebAudio.prototype.setGlobalEnv = CsoundWebAudio.prototype.SetGlobalEnv;

CsoundWebAudio.prototype.SetInput = function(name) {
    return this.csound.SetInput(name);
};
CsoundWebAudio.prototype.setInput = CsoundWebAudio.prototype.SetInput;

CsoundWebAudio.prototype.SetMessageCallback = function(message_callback) {
    //print = message_callback;
    console.log = message_callback;
    return print;
};
CsoundWebAudio.prototype.setMessageCallback = CsoundWebAudio.prototype.SetMessageCallback;

CsoundWebAudio.prototype.SetOption = function(option) {
    return this.csound.SetOption(option);
};
CsoundWebAudio.prototype.setOption = CsoundWebAudio.prototype.SetOption;

CsoundWebAudio.prototype.SetOutput = function(name, type, format) {
    return this.csound.SetOutput(name, type, format);
};
CsoundWebAudio.prototype.setOutput = CsoundWebAudio.prototype.SetOutput;

CsoundWebAudio.prototype.SetScoreOffsetSeconds = function(seconds) {
    return this.csound.SetScoreOffsetSeconds(seconds);
};
CsoundWebAudio.prototype.setScoreOffsetSeconds = CsoundWebAudio.prototype.SetScoreOffsetSeconds;

CsoundWebAudio.prototype.SetScorePending = function(is_pending) {
    return this.csound.SetScorePending(is_pending);
};
CsoundWebAudio.prototype.setScorePending = CsoundWebAudio.prototype.SetScorePending;

CsoundWebAudio.prototype.SetStringChannel = function(name, value) {
    return this.csound.SetStringChannel(name, value);
};
CsoundWebAudio.prototype.setStringChannel = CsoundWebAudio.prototype.SetStringChannel;

CsoundWebAudio.prototype.Start = function() {
    var input_name = this.csound.GetInputName();
    var output_name = this.csound.GetOutputName();
    if (!(output_name.startsWith("dac") || input_name.startsWith("adc"))) {
        this.is_realtime = false;
        this.csound.Start();
        return 0;
    } else {
        // Create a reference to this that will be in scope in the closures of callbacks.
        var this_ = this;
        this.is_realtime = true;                        
        this.csound.SetHostImplementedAudioIO(1, 0);
        this.csound.InitializeHostMidi();
        var onMidiEvent = function(event) {
            this_.csound.MidiEventIn(event.data[0], event.data[1], event.data[2]);
        };
        var midiSuccess = function(midiInterface) {
            var inputs = midiInterface.inputs.values();
            print("MIDI input initialized...\n");
            for (var input = inputs.next(); input && !input.done; input = inputs.next()) {
                input = input.value;
                print("Input: " + input.name + "\n");
                input.onmidimessage = onMidiEvent;
            }
        };
        var midiFail = function(error) {
            print("MIDI failed to start, error:" + error);
        };
        if (navigator.requestMIDIAccess) {
            navigator.requestMIDIAccess().then(midiSuccess, midiFail);
        } else {
            print("MIDI not supported in this context.");
        }
        this.csound.Start();
        var AudioContext = window.AudioContext || window.webkitAudioContext;
        var audioContext = new AudioContext();
        var spinBuffer = this.csound.GetSpin();
        var spoutBuffer = this.csound.GetSpout();
        var ksmps = this.csound.GetKsmps();
        var inputChannelCount = this.csound.GetNchnlsInput();
        var outputChannelCount = this.csound.GetNchnls();
        this.audioProcessNode = audioContext.createScriptProcessor(0, inputChannelCount, outputChannelCount);
        bufferFrameCount = this.audioProcessNode.bufferSize;
        print("WebAudio frames per buffer:         " +  bufferFrameCount + "\n");
        print("WebAudio frames per second:         " +  audioContext.sampleRate + "\n");
        print("WebAudio maximum output channels:   " +  audioContext.destination.maxChannelCount + "\n");
        this.audioProcessNode.inputCount = inputChannelCount;
        this.audioProcessNode.outputCount = outputChannelCount;
        var inputChannelN = this.audioProcessNode.inputCount;
        var outputChannelN = this.audioProcessNode.outputCount;
        var zerodBFS = this.csound.Get0dBFS();
        if (input_name.startsWith("adc")) {
            window.navigator = window.navigator || {};
            navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || null;
            if (navigator.getUserMedia === null) {
                print("Audio input not supported in this context.");
            } else {
                navigator.mediaDevices.getUserMedia({audio: true}).then((stream) => {
                    this.microphoneNode = audioContext.createMediaStreamSource(stream);
                    print("WebAudio input channels:            " +  this.microphoneNode.numberOfInputs + "\n");
                    if (this.microphoneNode !== null) {
                        if (inputChannelN != this.microphoneNode.numberOfInputs) {
                            this.microphoneNode.connect(this.audioProcessNode);
                            print("Audio input initialized.\n");
                        } else {
                            print("Csound nchnls_i does not match microphoneNode.numberOfInputs.");
                        }
                    }       
                }).catch ((e) => {
                    print("Microphone: " + e.name + ". " + e.message + "\n");
                    ///throw "Microphone: " + e.name + ". " + e.message;
                })
            }
        }
        this.audioProcessNode.connect(audioContext.destination);
        this.is_playing = true;
        var csoundFrameI = 0;
        this.audioProcessNode.onaudioprocess = function(audioProcessEvent) {
            var inputBuffer = audioProcessEvent.inputBuffer;
            var outputBuffer = audioProcessEvent.outputBuffer;
            var hostFrameN = outputBuffer.length;
            var result = 0;
            for (var hostFrameI = 0; hostFrameI < hostFrameN; hostFrameI++) {
                for (var inputChannelI = 0; inputChannelI < inputChannelN; inputChannelI++) {
                    var inputChannelBuffer = inputBuffer.getChannelData(inputChannelI);
                    spinBuffer[(csoundFrameI * inputChannelN) + inputChannelI] = inputChannelBuffer[hostFrameI] * zerodBFS;
                }
                for (var outputChannelI = 0; outputChannelI < outputChannelN; outputChannelI++) {
                    var outputChannelBuffer = outputBuffer.getChannelData(outputChannelI);
                    outputChannelBuffer[hostFrameI] = spoutBuffer[(csoundFrameI * outputChannelN) + outputChannelI] / zerodBFS;
                    spoutBuffer[(csoundFrameI * outputChannelN) + outputChannelI] = 0.0;
                }
                csoundFrameI++
                if (csoundFrameI === ksmps) {
                    csoundFrameI = 0;
                    result = this_.csound.PerformKsmps();
                    if (result !== 0) {
                        this_.Stop();
                        this_.Reset();
                        return;
                    }
                }
            }
        };
    };
};
CsoundWebAudio.prototype.start = CsoundWebAudio.prototype.Start;

CsoundWebAudio.prototype.Stop = function() {
    this.is_playing = false;
    if (this.microphoneNode !== null) {
        this.microphoneNode.disconnect();
        this.microphoneNode = null;
    }
    if (this.audioProcessNode !== null) {
        this.audioProcessNode.disconnect();
        this.audioProcessNode = null;
    }
    this.csound.Stop();
};
CsoundWebAudio.prototype.stop = CsoundWebAudio.prototype.Stop;

CsoundWebAudio.prototype.TableGet = function(number, index) {
    return this.csound.TableGet(number, index);
};
CsoundWebAudio.prototype.tableGet = CsoundWebAudio.prototype.TableGet;

CsoundWebAudio.prototype.TableLength = function(number) {
    return this.csound.TableLength(number);
};
CsoundWebAudio.prototype.tableLength = CsoundWebAudio.prototype.TableLength;

CsoundWebAudio.prototype.TableSet = function(number, index, value) {
    return this.csound.TableSet(number, index, value);
};
CsoundWebAudio.prototype.tableSet = CsoundWebAudio.prototype.TableSet;

CsoundWebAudio.prototype.constructor = CsoundWebAudio;                  
Module["CsoundWebAudio"] = CsoundWebAudio;
/**
 * In order to follow the same pattern of use as in all other Csound 
 * JavaScript environments, a global Csound object is injected into the 
 * JavaScript context, and the user is notified that Csound is ready.
 */
Module["onRuntimeInitialized"] = function() {
    csound = new Module.CsoundWebAudio();
    print("\nCsound has now been loaded; its functions may now be called.\n");
}

