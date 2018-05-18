class CsoundAudioProcessor extends AudioWorkletProcessor {
    static get parameterDescriptors() {
        return [{
            name: 'Dummy',
            defaultValue: 0.5,
            minValue: 0,
            maxValue: 1,
            automationRate: "k-rate"
        }];
    }
    constructor() {
        super();
        console.log("CsoundAudioProcessor constructor...\n");
        try {
            this.csound = new Module.CsoundEmbind();
        } catch (e) {
            console.log(e);
        }
        this.Reset();
        this.port.onmessage = this.onMessage.bind(this);
        var this_ = this;
        console.log = function(text) {
            this_.Message(text);
        };
    }
    Message(text) {
        this.port.postMessage(["Message", text]);
    }
    onMessage(event) {
        let result = null;
        let data = event.data;
        let input_name = "";
        let output_name = "";
        switch (data[0])
        {
            case "Cleanup":
                result = this.csound.Cleanup();
                break;
            case "CompileCsd":
                result = this.csound.CompileCsd(data[1]);
                break;
            case "CompileCsdText":
                result = this.csound.CompileCsdText(data[1]);
                break;
            case "CompileOrc":
                result = this.csound.CompileOrc(data[1]);
                break;
            case "Destroy":
                this.csound.Destroy();
                break;
            case "EvalCode":
                result = this.csound.EvalCode(data[1]);
                this.port.postMessage(["EvalCode", result]);
                break;
            case "Get0dBFS":
                result = this.csound.Get0dBFS();
                this.port.postMessage(["GetOdBFS", result]);
                break;
            case "GetAPIVersion":
                result = this.csound.GetAPIVersion();
                this.port.postMessage(["GetAPIVersion", result]);
                break;
            case "GetControlChannel":
                result = this.csound.GetControlChannel(data[1]);
                this.port.postMessage(["GetControlChannel", result]);
                break;
            case "GetCurrentTimeSamples":
                result = this.csound.GetCurrentTimeSamples();
                this.port.postMessage(["GetCurrentTimeSamples", result]);
                break;
            case "GetEnv":
                result = this.csound.GetEnv(data[1]);
                this.port.postMessage(["GetEnv", result]);
                break;
            case "GetInputName":
                result = this.csound.GetInputName();
                this.port.postMessage(["GetInputName", result]);
                break;
            case "GetKsmps":
                result = this.csound.GetKsmps();
                this.port.postMessage(["GetKsmps", result]);
                break;
            case "GetNchnls":
                result = this.csound.GetNchnls();
                this.port.postMessage(["GetNchnls", result]);
                break;
            case "GetNchnlsInput":
                result = this.csound.GetNchnlsInput();
                this.port.postMessage(["GetNchnlsInput", result]);
                break;
            case "GetOutputName":
                result = this.csound.GetOutputName();
                this.port.postMessage(["GetOutputName", result]);
                break;
            case "GetScoreOffsetSeconds":
                result = this.csound.GetScoreOffsetSeconds();
                this.port.postMessage(["GetScoreOffsetSeconds", result]);
                break;
            case "GetScoreTime":
                result = this.csound.GetScoreTime();
                this.port.postMessage(["GetScoreTime", result]);
                break;
            case "GetSr":
                result = this.csound.GetSr();
                this.port.postMessage(["GetSr", result]);
                break;
            case "GetStringChannel":
                result = this.csound.GetStringChannel(data[1]);
                this.port.postMessage(["GetStringChannel", result]);
                break;
            case "GetVersion":
                result = this.csound.GetVersion();
                this.port.postMessage(["GetVersion", result]);
                break;
            case "InputMessage":
                this.csound.InputMessage(data[1]);
                break;
            case "IsPlaying":
                result = this.csound.IsPlaying();
                this.port.postMessage(["IsPlaying", result]);
                break;
            case "IsScorePending":
                result = this.csound.IsScorePending();
                this.port.postMessage(["IsScorePending", result]);
                break;
            case "Message":
                this.csound.Message(data[1]);
                this.port.postMessage(["Message", result]);
                break;
            case "MidiEvent":
                this.csound.MidiEventIn(data[0], data[1], data[2]);
                break;
            case "Perform":
            {
                // This method is a dummy to preserve API compatibility 
                // across all platforms.
                let result = 0;
                this.port.postMessage(["Perform", result]);
            }
                break;
            case "PerformCsd":
            {
                let result = 0;
                let options = data[1];
                let csd = data[2];
                this.csound.CompileCsdText(csd);
                for(let i = 0; i < options.length; i++) {
                    this.csound.SetOption(options[i]);
                }
                result = this.csound.Start();
                this.port.postMessage(["PerformCsd", result]);
            }
                break;
            case "PerformOrc":
            {
                let result = 0;
                let options = data[1];
                let orc = data[2];
                this.csound.CompileOrc(orc);
                for(let i = 0; i < options.length; i++) {
                    this.csound.SetOption(options[i]);
                }
                if (data.length > 3) {
                    let sco = data[3];
                    if (typeof sco !== 'undefined') {
                        if (sco !== null) {                    
                            this.csound.ReadScore(sco);
                        }
                    }
                }
                result = this.csound.Start();
                this.port.postMessage(["PerformCsd", result]);
           }
                break;
            case "ReadScore":
                result = this.csound.ReadScore(data[1]);
                this.port.postMessage(["ReadScore", result]);
                break;
            case "Reset":
                this.csound.Reset();
                break;
            case "RewindScore":
                this.csound.RewindScore();
                break;
            case "SetControlChannel":
                this.csound.SetControlChannel(data[1], data[2]);
                break;
            case "SetGlobalEnv":
                result = this.csound.SetGlobalEnv(data[1], data[2]);
                this.port.postMessage(["SetGlobalEnv", result]);
                break;
            case "SetInput":
                this.csound.SetInput(data[1]);
                break;
            case "SetOption":
                result = this.csound.SetOption(data[1]);
                this.port.postMessage(["SetOption", result]);
                break;
            case "SetOutput":
                this.csound.SetOutput(data[1], data[2], data[3]);
                break;
            case "SetScoreOffsetSeconds":
                this.csound.SetScoreOffsetSeconds(data[1]);
                break;
            case "SetScorePending":
                this.csound.SetScorePending(data[1]);
                break;
            case "SetStringChannel":
                this.csound.SetStringChannel(data[1], data[2]);
                break;
            case "Start":
                result = this.Start();
                break;
            case "Stop":
                this.Reset();
                this.csound.Stop();
                break;
            case "TableGet":
                result = this.csound.TableGet(data[1], data[2]);
                this.port.postMessage(["TableGet", result]);
                break;
            case "TableLength":
                result = this.csound.TableLength(data[1]);
                this.port.postMessage(["TableLength", result]);
                break;
            case "TableSet":
                this.csound.TableSet(data[1], data[2]);
                break;
        };
    }
    Reset() {
        this.input_name = "";
        this.output_name = "";
        this.is_realtime = false;                        
        this.spinBuffer = null;
        this.spoutBuffer = null;;
        this.zerodBFS = 1.;                                    
        this.ksmps = 128;
        this.inputChannelN = 1;
        this.outputChannelN = 2;
        this.is_playing = false;
    }
    Start() {
        let result = 0;
        this.input_name = this.csound.GetInputName();
        this.output_name = this.csound.GetOutputName();
        if (this.output_name.startsWith("dac") || this.input_name.startsWith("adc")) {
            this.is_realtime = true;                        
            this.csound.SetHostImplementedAudioIO(1, 0);
            this.csound.InitializeHostMidi();
            result = this.csound.Start();
            this.spinBuffer = this.csound.GetSpin();
            this.spoutBuffer = this.csound.GetSpout();
            this.zerodBFS = this.csound.Get0dBFS();                                    
            this.ksmps = this.csound.GetKsmps();
            this.inputChannelN = this.csound.GetNchnlsInput();
            this.outputChannelN = this.csound.GetNchnls();
            if (this.input_name.startsWith("adc")) {
                this.has_input = true;
            } else {
                this.has_input = false;
            }
        } else {
            this.is_realtime = false;
            result = this.csound.Start();
        }
        this.is_playing = true;
        this.format_validated = false;
        return result;
    }
    process(inputs, outputs, parameters) {
        if (this.is_playing !== true) {
            return false;
        } else if (this.is_realtime === false) {
            console.log("CsoundAudioProcessor is rendering to soundfile: " + this.output_name + "\n");
            let result = 0;
            while (result === 0) {
                result = this.csound.PerformKsmps();
                if (result !== 0) {
                    this.csound.Stop();
                    this.csound.Reset();
                    console.log("CsoundAudioProcessor returns 'false'.");
                    return false;
                }
            }
        }
        /// Get the parameter values array. Here we do not use, and ignore, them.
        /// let myParamValues = parameters.myParam;
        // The processor may have multiple inputs and outputs. 
        // Each input or output may have multiple channels. 
        let inputBuffer = inputs[0];
        let outputBuffer = outputs[0];
        let inputChannel0 = inputBuffer[0];
        let outputChannel0 = outputBuffer[0];
        let inputChannelN = inputBuffer.length;
        let outputChannelN = outputBuffer.length;
        let hostFrameN = outputChannel0.length;
        // The audio stream format must match between Csound and the host.
        if (this.format_validated == false) {
            if (this.ksmps !== hostFrameN) {
                throw new RangeError("Csound ksmps doesn't match host ksmps!");
            } 
            if (this.inputChannelN > inputChannelN) {
                throw new RangeError("Csound nchnl_i doesn't match host input channel count of " + inputChannelN);
            }
            if (this.outputChannelN != outputChannelN) {
                throw new RangeError("Csound nchnls doesn't match host output channel count of " + outputChannelN);
            }
            if (this.csound.GetSr() != sampleRate) {
                throw new RangeError("Csound sampling rate doesn't match host sampling rate of " + sampleRate);
            }
            this.format_validated = true;
        }
        let csoundFrameI = 0;
        let result = 0;
        for (let hostFrameI = 0; hostFrameI < hostFrameN; hostFrameI++) {
            if (this.has_input === true) {
                for (let inputChannelI = 0; inputChannelI < inputChannelN; inputChannelI++) {
                    let inputChannelBuffer = inputBuffer[inputChannelI];
                    this.spinBuffer[(csoundFrameI * inputChannelN) + inputChannelI] = inputChannelBuffer[hostFrameI] * this.zerodBFS;
                }
            }
            for (let outputChannelI = 0; outputChannelI < outputChannelN; outputChannelI++) {
                let outputChannelBuffer = outputBuffer[outputChannelI];
                outputChannelBuffer[hostFrameI] = this.spoutBuffer[(csoundFrameI * outputChannelN) + outputChannelI] / this.zerodBFS;
                this.spoutBuffer[(csoundFrameI * outputChannelN) + outputChannelI] = 0.0;
            }
            csoundFrameI++
            if (csoundFrameI === hostFrameN) {
                csoundFrameI = 0;
                result = this.csound.PerformKsmps();
                if (result !== 0) {
                    this.csound.Stop();
                    this.csound.Reset();
                    console.log("CsoundAudioProcessor returns 'false'.");
                    return false;
                }
            }
        }
        return true;
    }
};

registerProcessor("csound-audio-processor", CsoundAudioProcessor);
console.log("Registered 'csound-audio-processor'.");
