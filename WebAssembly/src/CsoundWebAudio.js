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

CsoundWebAudio.prototype.CompileCsd = function(filename) {
    this.csound.CompileCsd(filename);
};

CsoundWebAudio.prototype.CompileCsdText = function(csd) {
    this.csound.CompileCsdText(csd);
};

CsoundWebAudio.prototype.CompileOrc = function(orc) {
    this.csound.CompileOrc(orc);
};

CsoundWebAudio.prototype.Destroy = function() {
    this.csound.delete();
};

CsoundWebAudio.prototype.EvalCode = function(code) {
    return this.csound.EvalCode(code);
};

CsoundWebAudio.prototype.Get0dBFS = function() {
    return this.csound.Get0dBFS();
};

CsoundWebAudio.prototype.GetAPIVersion = function() {
    return this.csound.GetAPIVersion();
};

CsoundWebAudio.prototype.GetControlChannel = function(name) {
    return this.csound.GetControlChannel();
};

CsoundWebAudio.prototype.GetCurrentTimeSamples = function() {
    return this.csound.GetCurrentTimeSamples();
};

CsoundWebAudio.prototype.GetEnv = function(name) {
    return this.csound.GetEnv();
};

CsoundWebAudio.prototype.GetInputName = function() {
    return this.csound.GetInputName();
};

CsoundWebAudio.prototype.GetKsmps = function() {
    return this.csound.GetKsmps();
};

CsoundWebAudio.prototype.GetNchnls = function() {
    return this.csound.GetNchnls();
};

CsoundWebAudio.prototype.GetNchnlsInput = function() {
    return this.csound.GetNchnlsInput();
};

CsoundWebAudio.prototype.GetOutputName = function() {
    return this.csound.GetOutputName();
};

CsoundWebAudio.prototype.GetScoreOffsetSeconds = function() {
    return this.csound.GetScoreOffsetSeconds();
};

CsoundWebAudio.prototype.GetScoreTime = function() {
    return this.csound.GetScoreTime();
};

CsoundWebAudio.prototype.GetSr = function() {
    return this.csound.GetSr();
};

CsoundWebAudio.prototype.GetStringChannel = function(name) {
    return this.csound.GetStringChannel(name);
};

CsoundWebAudio.prototype.GetVersion = function() {
    return this.csound.GetVersion();
};

CsoundWebAudio.prototype.InputMessage = function(text) {
    return this.csound.InputMessage(text);
};

CsoundWebAudio.prototype.IsPlaying = function() {
    return this.is_playing;
};

CsoundWebAudio.prototype.IsScorePending = function() {
    return this.csound.IsScorePending();
};

CsoundWebAudio.prototype.Message = function(text) {
    return this.csound.Message(text);
};

CsoundWebAudio.prototype.Perform = function() {
    if (this.is_realtime) {
        return 0;
    } else {
        return this.csound.Perform();
    }
    return this.csound.Perform();
};

CsoundWebAudio.prototype.ReadScore = function(score) {
    return this.csound.ReadScore(score);
};

CsoundWebAudio.prototype.Reset = function() {
    return this.csound.Reset();
};

CsoundWebAudio.prototype.RewindScore = function() {
    return this.csound.RewindScore();
};

CsoundWebAudio.prototype.SetControlChannel = function(name, value) {
    return this.csound.SetControlChannel(name, value);
};

CsoundWebAudio.prototype.SetGlobalEnv = function(name, value) {
    return this.csound.SetGlobalEnv(name, value);
};

CsoundWebAudio.prototype.SetInput = function(name) {
    return this.csound.SetInput(name);
};

CsoundWebAudio.prototype.SetOption = function(option) {
    return this.csound.SetOption(option);
};

CsoundWebAudio.prototype.SetOutput = function(name, type, format) {
    return this.csound.SetOutput(name, type, format);
};

CsoundWebAudio.prototype.SetScoreOffsetSeconds = function(seconds) {
    return this.csound.SetScoreOffsetSeconds(seconds);
};

CsoundWebAudio.prototype.SetScorePending = function(is_pending) {
    return this.csound.SetScorePending(is_pending);
};

CsoundWebAudio.prototype.SetStringChannel = function(name, value) {
    return this.csound.SetStringChannel(name, value);
};

CsoundWebAudio.prototype.Start = function() {
    var input_name = this.csound.GetInputName();
    var output_name = this.csound.GetOutputName();
    if (!(output_name.startsWith("dac") || input_name.startsWith("adc"))) {
        this.is_realtime = false;
        this.csound.Start();
        return 0;
    } else {
        this.is_realtime = true;                        
        this.csound.SetHostImplementedAudioIO(1, 0);
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
        console.info("audioProcessNode.bufferSize (WebAudio frames per buffer): " +  bufferFrameCount);
        this.audioProcessNode.inputCount = inputChannelCount;
        this.audioProcessNode.outputCount = outputChannelCount;
        var inputChannelN = this.audioProcessNode.inputCount;
        var outputChannelN = this.audioProcessNode.outputCount;
        var zerodBFS = this.csound.Get0dBFS();
        if (input_name.startsWith("adc")) {
            window.navigator = window.navigator || {};
            navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || null;
            if (navigator.getUserMedia === null) {
                Module['print']("Audio Input not supported in this browser");
                ///audioInputCallback(false);
            } else {
                function onSuccess(stream) {
                    this.microphoneNode = audioContext.createMediaStreamSource(stream);
                    ///audioInputCallback(true);
                };
                function onFailure(error) {
                    this.microphoneNode = null;
                    ///audioInputCallback(false);
                    Module['print']("Could not initialise audio input, error:" + error);
                };
                navigator.getUserMedia({
                    audio: true
                }, onSuccess, onFailure);
            }
            if (this.microphoneNode !== null) {
                if (inputChannelN >= this.microphoneNode.numberOfInputs) {
                    this.microphoneNode.connect(this.audioProcessNode);
                } else {
                    alert("Csound nchnls_i does not match microphoneNode.numberOfInputs.");
                    return;
                }
            }
        }
        this.audioProcessNode.connect(audioContext.destination);
        this.is_playing = true;
        var csoundFrameI = 0;
        var this_ = this;
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

CsoundWebAudio.prototype.TableGet = function(number, index) {
    return this.csound.TableGet(number, index);
};

CsoundWebAudio.prototype.TableLength = function(number) {
    return this.csound.TableLength(number);
};

CsoundWebAudio.prototype.TableSet = function(number, index, value) {
    return this.csound.TableSet(number, index, value);
};

CsoundWebAudio.prototype.constructor = CsoundWebAudio;                  
Module["CsoundWebAudio"] = CsoundWebAudio;
/**
 * In order to follow the same pattern of use as in all other Csound 
 * JavaScript environments, a global Csound object is injected into the 
 * JavaScript context, and the user is notified Csound is ready.
 */
Module["onRuntimeInitialized"] = function() {
    csound = new Module.CsoundWebAudio();
    if (typeof console !== 'undefined') {
        console.log("\nCsound has now been loaded; its functions may now be called.\n");
    }
    Module.print("Csound has now been loaded; its functions may now be called.\n");
}

