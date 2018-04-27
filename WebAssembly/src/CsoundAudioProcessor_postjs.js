class CsoundAudioProcessor extends AudioWorkletProcessor {
    static get parameterDescriptors() {
        return [{
            name: 'myParam',
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
        this.is_playing = false;
        this.is_realtime = false;
        this.port.onmessage = this.onMessage.bind(this);
    }
    onMessage(event) {
        var result = null;
        var data = event.data;
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
                break;
            case "Perform":
                result = this.csound.Perform();
                this.port.postMessage(["Perform", result]);
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
                {
                    var input_name = this.csound.GetInputName();
                    var output_name = this.csound.GetOutputName();
                    if (!(output_name.startsWith("dac") || input_name.startsWith("adc"))) {
                        this.is_realtime = false;
                        result = this.csound.Start();
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
                        result = this.csound.Start();
                    }
                    this.port.postMessage(["Start", result]);
                }
                break;
            case "Stop":
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
    process(inputs, outputs, parameters) {
        // The processor may have multiple inputs and outputs. 
        // Each input or output may have multiple channels. 
        let inputBuffer = inputs[0];
        let outputBuffer = outputs[0];
        let inputChannel0 = input[0];
        let outputChannel0 = output[0];
        /// Get the parameter values array. Here we do not use, and ignore, them.
        /// let myParamValues = parameters.myParam;
        var csoundFrameI = 0;
        var hostFrameN = outputChannel0.length;
        var result = 0;
        for (var hostFrameI = 0; hostFrameI < hostFrameN; hostFrameI++) {
            for (var inputChannelI = 0; inputChannelI < inputChannelN; inputChannelI++) {
                var inputChannelBuffer = inputBuffer[inputChannelI];
                spinBuffer[(csoundFrameI * inputChannelN) + inputChannelI] = inputChannelBuffer[hostFrameI] * zerodBFS;
            }
            for (var outputChannelI = 0; outputChannelI < outputChannelN; outputChannelI++) {
                var outputChannelBuffer = outputBuffer[outputChannelI];
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
                    return false;
                }
            }
        }
        return true;
    }
};

class CsoundAudioProcessorTest extends AudioWorkletProcessor {
    static get parameterDescriptors() {
        return [{
            name: 'myParam',
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
        this.is_playing = false;
    }


  process(inputs, outputs, parameters) {
    // Get the first input and output.
    let input = inputs[0];
    let output = outputs[0];
    let myParam = parameters.myParam;

    // A simple amplifier for single input and output.
    for (let channel = 0; channel < output.length; ++channel) {
      for (let i = 0; i < output[channel].length; ++i) {
        output[channel][i] = input[channel][i] * myParam[0];
      }
    }
  }
}

//export default csound_audio_processor_module;

registerProcessor("csound-audio-processor", CsoundAudioProcessor);
