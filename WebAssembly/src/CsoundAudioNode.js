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
        super(context, 'csound-audio-processor');
        this.is_playing = false;
        this.is_realtime = false;
        this.audioProcessNode = null;
        this.microphoneNode = null;
    };
Cleanup() {
    this.port.postMessage("Cleanup");
};
cleanup = Cleanup;

CompileCsd(filename) {
    this.csound.CompileCsd(filename);
};
compileCsd = CompileCsd;

CompileCsdText(csd) {
    this.csound.CompileCsdText(csd);
};
compileCsdText = CompileCsdText;

CompileOrc(orc) {
    this.csound.CompileOrc(orc);
};
compileOrc = CompileOrc;

Destroy() {
    this.csound.delete();
};
destroy = Destroy;

EvalCode(code) {
    return this.csound.EvalCode(code);
};
evalCode = EvalCode;

Get0dBFS() {
    return this.csound.Get0dBFS();
};
get0dBFS = Get0dBFS;

GetAPIVersion() {
    return this.csound.GetAPIVersion();
};
getAPIVersion = GetAPIVersion;

GetControlChannel(name) {
    return this.csound.GetControlChannel(name);
};
getControlChannel = GetControlChannel;

GetCurrentTimeSamples() {
    return this.csound.GetCurrentTimeSamples();
};
getCurrentTimeSamples = GetCurrentTimeSamples;

GetEnv(name) {
    return this.csound.GetEnv(name);
};
getEnv = GetEnv;

GetInputName() {
    return this.csound.GetInputName();
};
getInputName = GetInputName;

GetKsmps() {
    return this.csound.GetKsmps();
};
getKsmps = GetKsmps;

GetNchnls() {
    return this.csound.GetNchnls();
};
getNchnls = GetNchnls;

GetNchnlsInput() {
    return this.csound.GetNchnlsInput();
};
getNchnlsInput = GetNchnlsInput;

GetOutputName() {
    return this.csound.GetOutputName();
};
getOutputName = GetOutputName;

GetScoreOffsetSeconds() {
    return this.csound.GetScoreOffsetSeconds();
};
getScoreOffsetSeconds = GetScoreOffsetSeconds;

GetScoreTime() {
    return this.csound.GetScoreTime();
};
getScoreTime = GetScoreTime;

GetSr() {
    return this.csound.GetSr();
};
getSr = GetSr;

GetStringChannel(name) {
    return this.csound.GetStringChannel(name);
};
getStringChannel = GetStringChannel;

GetVersion() {
    return this.csound.GetVersion();
};
getVersion = GetVersion;

InputMessage(text) {
    this.csound.InputMessage(text);
};
inputMessage = InputMessage;

IsPlaying() {
    return this.is_playing;
};
isPlaying = IsPlaying;

IsScorePending() {
    return this.csound.IsScorePending();
};
isScorePending = IsScorePending;

Message(text) {
    return this.csound.Message(text);
};
message = Message;

Perform() {
    if (this.is_realtime) {
        return 0;
    } else {
        return this.csound.Perform();
    }
    return this.csound.Perform();
};
perform = Perform;

ReadScore(score) {
    return this.csound.ReadScore(score);
};
readScore = ReadScore;

Reset() {
    return this.csound.Reset();
};
reset = Reset;

RewindScore() {
    return this.csound.RewindScore();
};
rewindScore = RewindScore;

SetControlChannel(name, value) {
    return this.csound.SetControlChannel(name, value);
};
setControlChannel = SetControlChannel;

SetGlobalEnv(name, value) {
    return this.csound.SetGlobalEnv(name, value);
};
setGlobalEnv = SetGlobalEnv;

SetInput(name) {
    return this.csound.SetInput(name);
};
setInput = SetInput;

SetOption(option) {
    return this.csound.SetOption(option);
};
setOption = SetOption;

SetOutput(name, type, format) {
    return this.csound.SetOutput(name, type, format);
};
setOutput = SetOutput;

SetScoreOffsetSeconds(seconds) {
    return this.csound.SetScoreOffsetSeconds(seconds);
};
setScoreOffsetSeconds = SetScoreOffsetSeconds;

SetScorePending(is_pending) {
    return this.csound.SetScorePending(is_pending);
};
setScorePending = SetScorePending;

SetStringChannel(name, value) {
    return this.csound.SetStringChannel(name, value);
};
setStringChannel = SetStringChannel;

Start() {
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
stop = Stop;

TableGet(number, index) {
    return this.csound.TableGet(number, index);
};
tableGet = TableGet;

TableLength(number) {
    return this.csound.TableLength(number);
};
tableLength = TableLength;

TableSet(number, index, value) {
    return this.csound.TableSet(number, index, value);
};
tableSet = TableSet;

constructor = CsoundWebAudio;                  
Module["CsoundWebAudio"] = CsoundWebAudio;
/**
 * In order to follow the same pattern of use as in all other Csound 
 * JavaScript environments, a global Csound object is injected into the 
 * JavaScript context, and the user is notified that Csound is ready.
 */
Module["onRuntimeInitialized"]() {
    csound = new Module.CsoundWebAudio();
    print("\nCsound has now been loaded; its functions may now be called.\n");
}



