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
        this.csound_input_channel_n = 1;
        this.csound_output_channel_n = 2;
        this.is_playing = false;
        this.format_validated = false;
        this.has_audio_input = false;
    }
    Start() {
        this.csound.Message("CsoundAudioProcessor starting...\n");
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
        this.csound.Message("CsoundAudioProcessor started.\n");
    }
    process(inputs, outputs, parameters) {
        try {
            if (this.is_playing == true) {
                // If rendering is not real-time, then all audio input and output 
                // is configured and performed within Csound.
                if (this.is_realtime == false) {
                    this.csound.Message("CsoundAudioProcessor: non-real-time performance...\n");
                    let result = 0;
                    while (result == 0) {
                        result = this.csound.PerformKsmps();
                        if (result != 0) {
                            this.is_playing = false;
                            this.csound.Stop();
                            this.csound.Cleanup();
                            this.csound.Reset();
                            this.csound.Message("CsoundAudioProcessor returns 'false'.\n");
                            return true;
                        }
                    }
                } 
                // Otherwise, all audio input and output are configured and 
                // performed in the WebAudio processor, here. Csound adapts 
                // to the WebAudio input and output configuration, except that 
                // Csound's ksmps MUST equal the length of the WebAudio input and 
                // output buffers (always 128 frames). However, the processor 
                // may have multiple inputs and outputs, and each input or output 
                // may have multiple channels. 
                //
                // Also note, Csound's buffers are in [frame, channel] order, and 
                // WebAudio's buffers are in [channel, frame] order.
                let wa_input_buffer = inputs[0];
                let wa_input_channel_n = wa_input_buffer.length;
                let wa_input_channel_0 = wa_input_buffer[0];
                let wa_input_channel_buffer;
                let csound_input_buffer = this.csound.GetSpin();            
                let csound_input_channel_n = this.csound.GetNchnlsInput();
                let wa_output_buffer = outputs[0];
                let csound_output_buffer = this.csound.GetSpout();
                let csound_output_channel_n = this.csound.GetNchnls();
                let wa_output_channel_n = wa_output_buffer.length;
                let wa_output_channel_0 = wa_output_buffer[0];
                let wa_output_channel_buffer;
                let wa_frame_n = wa_output_channel_0.length;
                let ksmps = this.csound.GetKsmps();
                let zero_dbfs = this.csound.Get0dBFS();
                if (this.format_validated == false) {
                    this.csound.Message("CsoundAudioProcessor frames per quantum:        " +  wa_frame_n + "\n");
                    this.csound.Message("CsoundAudioProcessor input channels:            " +  wa_input_channel_n + "\n");
                    this.csound.Message("CsoundAudioProcessor output channels:           " +  wa_output_channel_n + "\n");
                    if (ksmps != wa_frame_n) {
                        this.csound.Message("Error! Csound's ksmps doesn't match the WebAudio buffer length!\n");
                        return false;
                    } 
                    if (csound_input_channel_n != wa_input_channel_n) {
                        this.csound.Message("Warning! Csound nchnls_i (" + csound_input_channel_n + ") doesn't match the WebAudio input channel count (" + wa_input_channel_n + ").\n");
                    }
                    if (csound_output_channel_n != wa_output_channel_n) {
                        this.csound.Message("Warning! Csound nchnls (" + csound_output_channel_n + ") doesn't match the WebAudio output channel count (" + wa_output_channel_n + ").\n");
                    }
                    this.format_validated = true;
                    this.csound.Message("CsoundAudioProcessor format_validated: " + this.format_validated + ".\n");
                }
                this.format_validated = true;
                if (this.has_audio_input == true) {
                    let input_channel_i;
                    // If the WebAudio input channel count is less than Csound's 
                    // spin buffer count, then any extra spin buffer channels are zero-filled.
                    if (wa_input_channel_n < csound_input_channel_n) {
                        for (input_channel_i = 0; input_channel_i < wa_input_channel_n; input_channel_i++) {
                            wa_input_channel_buffer = wa_input_buffer[input_channel_i];
                            for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                                csound_input_buffer[(frame_i * csound_input_channel_n) + input_channel_i] = (wa_input_channel_buffer[frame_i] * zero_dbfs);
                            }
                        }
                        for ( ; input_channel_i < csound.input_channel_n; input_channel_i++) {
                            for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                                csound_input_buffer[(frame_i * csound_input_channel_n) + input_channel_i] = 0.;
                            }
                       }
                    // Otherwise, any extra WebAudio input channels are simply ignored.
                    } else {
                        for (input_channel_i = 0; input_channel_i < csound_input_channel_n; input_channel_i++) {
                            wa_input_channel_buffer = wa_input_buffer[input_channel_i];
                            for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                                csound_input_buffer[(frame_i * input_channel_i) + input_channel_i] = (wa_input_channel_buffer[frame_i] * zero_dbfs);
                            }
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
                let output_channel_i;
                // If the WebAudio output channel count is greater than Csound's 
                // spout channel count, then the extra WebAudio output channels 
                // are zero-filled.
                if (wa_output_channel_n > csound_output_channel_n) {
                    for (output_channel_i = 0; output_channel_i < csound_output_channel_n; ++output_channel_i) {
                        wa_output_channel_buffer = wa_output_buffer[output_channel_i];
                        for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                            wa_output_channel_buffer[frame_i] = (csound_output_buffer[(frame_i * csound_output_channel_n) + output_channel_i] / zero_dbfs);
                        }
                    }
                    for ( ; output_channel_i < wa_output_channel_n; output_channel_i++) {
                        wa_output_channel_buffer = wa_output_buffer[output_channel_i];
                        for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                            wa_output_channel_buffer[frame_i] = 0.;
                        }
                    }
                // Otherwise, any extra Csound spout buffer channels are simply ignored.
                } else {
                    for (output_channel_i = 0; output_channel_i < wa_output_channel_n; output_channel_i++) {
                        wa_output_channel_buffer = wa_output_buffer[output_channel_i];
                        for (let frame_i = 0; frame_i < wa_frame_n; frame_i++) {
                            wa_output_channel_buffer[frame_i] = (csound_output_buffer[(frame_i * wa_output_channel_n) + output_channel_i] / zero_dbfs);
                        }
                    }
                }
            }
            return true;
        } catch(e) {
            console.log(e);
        }
    }
};

registerProcessor("csound-audio-processor", CsoundAudioProcessor);
console.log("Registered 'csound-audio-processor'.");
