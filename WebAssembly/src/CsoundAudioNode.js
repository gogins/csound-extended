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
} else print(message) {
    console.log(message);
    Module.print(message);
}

class CsoundAudioNode extends AudioWorkletNode {
    constructor(context) {
        super(context, 'CsoundAudioProcessor');
        this.is_playing = false;
        this.is_realtime = false;
        this.audioProcessNode = null;
        this.microphoneNode = null;
        // TODO: There is probably some way to 
        // make method calls that return a value cheaply 
        // be synchronous. For now, just set up the port 
        // event handler for bidirectional communication.
        this.port.onmessage = (event) => {
            console.log("CsoundAudioNode.port.onmessage: " + event);
        }
    };
Cleanup() {
    this.port.postMessage("Cleanup");
};
cleanup = Cleanup;

CompileCsd(filename) {
    this.port.postMessage("CompileCsd", filename);
};
compileCsd = CompileCsd;

CompileCsdText(csd) {
    this.port.postMessage("CompileCsdText", csd);
};
compileCsdText = CompileCsdText;

CompileOrc(orc) {
    this.port.postMessage("CompileOrc", orc);
};
compileOrc = CompileOrc;

Destroy() {
    this.port.postMessage("Destroy");
};
destroy = Destroy;

EvalCode(code) {
    this.port.postMessage("EvalCode", code);
};
evalCode = EvalCode;

// TODO: This and similar cheap functions with needed return values 
// should be extended with promises to wait for a message back 
// and thus implement a synchronous function call.

Get0dBFS() {
    this.port.postMessage("Get0dBFS");
};
get0dBFS = Get0dBFS;

GetAPIVersion() {
    this.port.postMessage("GetAPIVersion");
};
getAPIVersion = GetAPIVersion;

GetControlChannel(name) {
    this.port.postMessage("GetControlChannel", name);
};
getControlChannel = GetControlChannel;

GetCurrentTimeSamples() {
    this.port.postMessage("GetCurrentTimeSamples");
};
getCurrentTimeSamples = GetCurrentTimeSamples;

GetEnv(name) {
    this.port.postMessage("GetEnv", name);
};
getEnv = GetEnv;

GetInputName() {
    this.port.postMessage("GetInputName");
};
getInputName = GetInputName;

GetKsmps() {
    this.port.postMessage("GetKsmps");
};
getKsmps = GetKsmps;

GetNchnls() {
    this.port.postMessage("GetNchnls");
};
getNchnls = GetNchnls;

GetNchnlsInput() {
    this.port.postMessage("GetNchnlsInput");
};
getNchnlsInput = GetNchnlsInput;

GetOutputName() {
    this.port.postMessage("GetOutputName");
};
getOutputName = GetOutputName;

GetScoreOffsetSeconds() {
    this.port.postMessage("GetScoreOffsetSeconds");
};
getScoreOffsetSeconds = GetScoreOffsetSeconds;

GetScoreTime() {
    this.port.postMessage("GetScoreTime");
};
getScoreTime = GetScoreTime;

GetSr() {
    this.port.postMessage("GetSr");
};
getSr = GetSr;

GetStringChannel(name) {
    this.port.postMessage("GetStringChannel", name);
};
getStringChannel = GetStringChannel;

GetVersion() {
    this.port.postMessage("GetVersion");
};
getVersion = GetVersion;

InputMessage(text) {
    this.port.postMessage("InputMessage", text);
};
inputMessage = InputMessage;

IsPlaying() {
    this.port.postMessage("IsPlaying");
};
isPlaying = IsPlaying;

IsScorePending() {
    this.port.postMessage("IsScorePending");
};
isScorePending = IsScorePending;

Message(text) {
    this.port.postMessage("Message", text);
};
message = Message;

Perform() {
    this.port.postMessage("Perform");
};
perform = Perform;

ReadScore(score) {
    this.port.postMessage("ReadScore", score);
};
readScore = ReadScore;

Reset() {
    this.port.postMessage("Reset");
};
reset = Reset;

RewindScore() {
    this.port.postMessage("RewindScore");
};
rewindScore = RewindScore;

SetControlChannel(name, value) {
    this.port.postMessage("SetControlChannel", name, value);
};
setControlChannel = SetControlChannel;

SetGlobalEnv(name, value) {
    this.port.postMessage("SetGlobalEnv", name, value);
};
setGlobalEnv = SetGlobalEnv;

SetInput(name) {
    this.port.postMessage("SetInput", name);
};
setInput = SetInput;

SetOption(option) {
    this.port.postMessage("SetOption", option);
};
setOption = SetOption;

SetOutput(name, type, format) {
    this.port.postMessage("SetOutput", name, type, format);
};
setOutput = SetOutput;

SetScoreOffsetSeconds(seconds) {
    this.port.postMessage("SetScoreOffsetSeconds", seconds);
};
setScoreOffsetSeconds = SetScoreOffsetSeconds;

SetScorePending(is_pending) {
    this.port.postMessage("SetScorePending", is_pending);
};
setScorePending = SetScorePending;

SetStringChannel(name, value) {
    this.port.postMessage("SetStringChannel", name, value);
};
setStringChannel = SetStringChannel;

// TODO: Needs much work.
// Wiring into Web Audio graph here in the upper half, 
// wiring within Csound down in the lower half.

Start() {
    this.port.postMessage("Start");
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
        var onMidiEvent(event) {
            this_.csound.MidiEventIn(event.data[0], event.data[1], event.data[2]);
        };
        var midiSuccess(midiInterface) {
            var inputs = midiInterface.inputs.values();
            print("MIDI input initialized...\n");
            for (var input = inputs.next(); input && !input.done; input = inputs.next()) {
                input = input.value;
                print("Input: " + input.name + "\n");
                input.onmidimessage = onMidiEvent;
            }
        };
        var midiFail(error) {
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
        print("Web Audio initialized with frames per buffer: " +  bufferFrameCount + "\n");
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
                    this.microphoneNode.connect(this.audioProcessNode);
                } else {
                    print("Csound nchnls_i does not match microphoneNode.numberOfInputs.");
                    return;
                }
            }
        }
        this.audioProcessNode.connect(audioContext.destination);
        this.is_playing = true;
        var csoundFrameI = 0;
        this.audioProcessNode.onaudioprocess(audioProcessEvent) {
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
start = Start;

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
stop = Stop;

TableGet(number, index) {
    this.port.postMessage("TableGet", number, index);
};
tableGet = TableGet;

TableLength(number) {
    this.port.postMessage("TableLength", number);
};
tableLength = TableLength;

TableSet(number, index, value) {
    this.port.postMessage("TableSet", index, value);
};
tableSet = TableSet;

constructor = CsoundAudioNode;                  
Module["CsoundAudioNode"] = CsoundAudioNode;
/**
 * In order to follow the same pattern of use as in all other Csound 
 * JavaScript environments, a global Csound object is injected into the 
 * JavaScript context, and the user is notified that Csound is ready.
 */
Module["onRuntimeInitialized"]() {
    csound = new Module.CsoundAudioNode();
    print("\nCsoundAudioNode has now been loaded; its functions may now be called.\n");
}



