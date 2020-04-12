// Globals to ensure that AudioWorkletProcessor.port is in scope for 
// the Csound message callback.

///var shim;
///var port_;

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
        this.port.postMessage.bind(this);
        var port_ = this.port;
        var shim = function(text) {
            port_.postMessage(["Message", text]);
        };
        this.has_audio_input = false;
        this.csound.SetMessageCallback(shim);
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
                this.port.postMessage(["CleanupResult", result]);
                break;
            case "CompileCsd":
                result = this.csound.CompileCsd(data[1]);
                break;
            case "CompileCsdText":
                result = this.csound.CompileCsdText(data[1]);
                this.port.postMessage(["CompileCsdTextResult", result]);
                break;
            case "CompileOrc":
                result = this.csound.CompileOrc(data[1]);
                this.port.postMessage(["CompileOrcResult", result]);
               break;
            case "Destroy":
                this.csound.Destroy();
                break;
            case "EvalCode":
                result = this.csound.EvalCode(data[1]);
                this.port.postMessage(["EvalCodeResult", result]);
                break;
            case "Get0dBFS":
                result = this.csound.Get0dBFS();
                this.port.postMessage(["GetOdBFSResult", result]);
                break;
            case "GetAPIVersion":
                result = this.csound.GetAPIVersion();
                this.port.postMessage(["GetAPIVersionResult", result]);
                break;
            case "GetControlChannel":
                result = this.csound.GetControlChannel(data[1]);
                this.port.postMessage(["GetControlChannelResult", result]);
                break;
            case "GetCurrentTimeSamples":
                result = this.csound.GetCurrentTimeSamples();
                this.port.postMessage(["GetCurrentTimeSamplesResult", result]);
                break;
            case "GetEnv":
                result = this.csound.GetEnv(data[1]);
                this.port.postMessage(["GetEnvResult", result]);
                break;
            case "GetInputName":
                result = this.csound.GetInputName();
                this.port.postMessage(["GetInputNameResult", result]);
                break;
            case "GetKsmps":
                result = this.csound.GetKsmps();
                this.port.postMessage(["GetKsmpsResult", result]);
                break;
            case "GetNchnls":
                result = this.csound.GetNchnls();
                this.port.postMessage(["GetNchnlsResult", result]);
                break;
            case "GetNchnlsInput":
                result = this.csound.GetNchnlsInput();
                this.port.postMessage(["GetNchnlsInputResult", result]);
                break;
            case "GetOutputName":
                result = this.csound.GetOutputName();
                this.port.postMessage(["GetOutputNameResult", result]);
                break;
            case "GetScoreOffsetSeconds":
                result = this.csound.GetScoreOffsetSeconds();
                this.port.postMessage(["GetScoreOffsetSecondsResult", result]);
                break;
            case "GetScoreTime":
                result = this.csound.GetScoreTime();
                this.port.postMessage(["GetScoreTimeResult", result]);
                break;
            case "GetSr":
                result = this.csound.GetSr();
                this.port.postMessage(["GetSrResult", result]);
                break;
            case "GetStringChannel":
                result = this.csound.GetStringChannel(data[1]);
                this.port.postMessage(["GetStringChannelResult", result]);
                break;
            case "GetVersion":
                result = this.csound.GetVersion();
                this.port.postMessage(["GetVersionResult", result]);
                break;
            case "InputMessage":
                this.csound.InputMessage(data[1]);
                break;
            case "IsPlaying":
                result = this.csound.IsPlaying();
                this.port.postMessage(["IsPlayingResult", result]);
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
                this.port.postMessage(["PerformResult", result]);
            }
                break;
            case "PerformCsd":
            {
                let result = 0;
                let options = data[1];
                let csd = data[2];
                this.csound.CompileCsdText(csd);
                //for(let i = 0; i < options.length; i++) {
                //    this.csound.SetOption(options[i]);
                //}
                result = this.Start();
                this.port.postMessage(["PerformCsdResult", result]);
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
                result = this.Start();
                this.port.postMessage(["PerformCsdResult", result]);
           }
                break;
            case "ReadScore":
                result = this.csound.ReadScore(data[1]);
                this.port.postMessage(["ReadScoreResult", result]);
                break;
            case "Reset":
                this.csound.Reset();
                this.port.postMessage(["ResetResult"]);
                break;
            case "RewindScore":
                this.csound.RewindScore();
                break;
            case "SetControlChannel":
                this.csound.SetControlChannel(data[1], data[2]);
                break;
            case "SetGlobalEnv":
                result = this.csound.SetGlobalEnv(data[1], data[2]);
                this.port.postMessage(["SetGlobalEnvResult", result]);
                break;
            case "SetInput":
                this.csound.SetInput(data[1]);
                break;
            case "SetOption":
                result = this.csound.SetOption(data[1]);
                this.port.postMessage(["SetOptionResult", result]);
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
                this.port.postMessage(["StartResult", result]);
                break;
            case "Stop":
                this.Reset();
                this.csound.Stop();
                this.port.postMessage(["StopResult"]);
                break;
            case "TableGet":
                result = this.csound.TableGet(data[1], data[2]);
                this.port.postMessage(["TableGetResult", result]);
                break;
            case "TableLength":
                result = this.csound.TableLength(data[1]);
                this.port.postMessage(["TableLengthResult", result]);
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
        this.format_validated = false;
        this.has_audio_input = false;
    }
    Start() {
        this.is_playing = true;
        this.format_validated = false;
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
            if (this.input_name.startsWith("adc")) {
                this.has_audio_input = true;  
                this.csound.Message("CsoundAudioProcessor uses audio input:          " + this.input_name + "\n");
            } else {
                this.has_audio_input = false;
            }
            this.csound.Message("CsoundAudioProcessor is rendering in real time: " + this.output_name + "\n");
        } else {
            this.is_realtime = false;
            this.csound.Message("CsoundAudioProcessor is rendering to soundfile: " + this.output_name + "\n");
            result = this.csound.Start();
        }
    }
    process(inputs, outputs, parameters) {
        if (this.is_playing === true) {
            if (this.is_realtime === false) {
                let result = 0;
                while (result === 0) {
                    result = this.csound.PerformKsmps();
                    if (result !== 0) {
                        this.is_playing = false;
                        this.csound.Stop();
                        this.csound.Cleanup();
                        this.csound.Reset();
                        this.csound.Message("CsoundAudioProcessor returns 'false'.\n");
                        return true;
                    }
                }
            } 
            // The processor may have multiple inputs and outputs. 
            // Each input or output may have multiple channels. 
            let inputBuffer = inputs[0];
            let outputBuffer = outputs[0];
            let inputChannel0 = inputBuffer[0];
            let outputChannel0 = outputBuffer[0];
            let inputChannelN = inputBuffer.length;
            let outputChannelN = outputBuffer.length;
            let hostFrameN = outputChannel0.length;
            // The audio stream buffer shape(s) must match between Csound and the host.
            if (this.format_validated == false) {
                this.csound.Message("CsoundAudioProcessor frames per quantum:        " +  hostFrameN + "\n");
                this.csound.Message("CsoundAudioProcessor output channels:           " +  outputChannelN + "\n");
                this.csound.Message("CsoundAudioProcessor input channels:            " +  inputChannelN + "\n");
                if (this.ksmps !== hostFrameN) {
                    this.csound.Message("Csound ksmps doesn't match host ksmps!\n");
                    return false;
                } 
                if (this.inputChannelN > inputChannelN) {
                    this.csound.Message("Csound nchnls_i doesn't match host input channel count of " + inputChannelN + "!\n");
                    return false;
                }
                if (this.outputChannelN != outputChannelN) {
                    this.csound.Message("Csound nchnls doesn't match host output channel count of " + outputChannelN + "!\n");
                    return false;
                }
            }
            this.format_validated = true;
            // These loops require Csound's ksmps to equal the length of the 
            // WebAudio input and output buffers (should ALWAYS be 128).
            if (this.has_audio_input === true) {
                // WebAudio spreads 1 input channel across 2 channels...
                for (let inputChannelI = 0; inputChannelI < this.inputChannelN; inputChannelI++) {
                    let inputChannelBuffer = inputBuffer[inputChannelI];
                    for (let sampleFrameI = 0; sampleFrameI < hostFrameN; sampleFrameI++) {
                        this.spinBuffer[(sampleFrameI * inputChannelN) + inputChannelI] = (inputChannelBuffer[sampleFrameI] * this.zerodBFS);
                    }
                }
            }
            let result = this.csound.PerformKsmps();
            if (result !== 0) {
                this.csound.Stop();
                this.csound.Cleanup();
                this.csound.Reset();
                this.csound.Message("Csound has finished playing.\n");
                this.is_playing = false;
            }
            for (let outputChannelI = 0; outputChannelI < outputChannelN; outputChannelI++) {
                let outputChannelBuffer = outputBuffer[outputChannelI];
                for (let sampleFrameI = 0; sampleFrameI < hostFrameN; sampleFrameI++) {
                    outputChannelBuffer[sampleFrameI] = (this.spoutBuffer[(sampleFrameI * outputChannelN) + outputChannelI] / this.zerodBFS);
                }
            }
        }
        return true;
    }
};

registerProcessor("csound-audio-processor", CsoundAudioProcessor);
console.log("Registered 'csound-audio-processor'.");
