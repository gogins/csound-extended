class CsoundAudioProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.csound = new Module.CsoundEmbind();
    this.is_playing = false;
    this.is_realtime = false;
    this.port.onmessage = (event) => {
        var result = null;
        switch (event.data[0])
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
                this.port.postMessage("EvalCode", result);
                break;
            case "Get0dBFS":
                result = this.csound.Get0dBFS();
                this.port.postMessage("GetOdBFS", result);
                break;
            case "GetAPIVersion":
                result = this.csound.GetAPIVersion();
                this.port.postMessage("GetAPIVersion", result);
                break;
            case "GetControlChannel":
                result = this.csound.GetControlChannel(data[1]);
                this.port.postMessage("GetControlChannel", result);
                break;
            case "GetCurrentTimeSamples":
                result = this.csound.GetCurrentTimeSamples();
                this.port.postMessage("GetCurrentTimeSamples", result);
                break;
            case "GetEnv":
                result = this.csound.GetEnv(data[1]);
                this.port.postMessage("GetEnv", result);
                break;
            case "GetInputName":
                result = this.csound.GetInputName();
                this.port.postMessage("GetInputName", result);
                break;
            case "GetKsmps":
                result = this.csound.GetKsmps();
                this.port.postMessage("GetKsmps", result);
                break;
            case "GetNchnls":
                result = this.csound.GetNchnls();
                this.port.postMessage("GetNchnls", result);
                break;
            case "GetNchnlsInput":
                result = this.csound.GetNchnlsInput();
                this.port.postMessage("GetNchnlsInput", result);
                break;
            case "GetOutputName":
                result = this.csound.GetOutputName();
                this.port.postMessage("GetOutputName", result);
                break;
            case "GetScoreOffsetSeconds":
                result = this.csound.GetScoreOffsetSeconds();
                this.port.postMessage("GetScoreOffsetSeconds", result);
                break;
            case "GetScoreTime":
                result = this.csound.GetScoreTime();
                this.port.postMessage("GetScoreTime", result);
                break;
            case "GetSr":
                result = this.csound.GetSr();
                this.port.postMessage("GetSr", result);
                break;
            case "GetStringChannel":
                result = this.csound.GetStringChannel(data[1]);
                this.port.postMessage("GetStringChannel", result);
                break;
            case "GetVersion":
                result = this.csound.GetVersion();
                this.port.postMessage("GetVersion", result);
                break;
            case "InputMessage":
                this.csound.InputMessage(data[1]);
                break;
            case "IsPlaying":
                result = this.csound.IsPlaying();
                this.port.postMessage("IsPlaying", result);
                break;
            case "IsScorePending":
                result = this.csound.IsScorePending();
                this.port.postMessage("IsScorePending", result);
                break;
            case "Message":
                this.csound.Message(data[1]);
                break;
            case "Perform":
                result = this.csound.Perform();
                this.port.postMessage("Perform", result);
                break;
            case "ReadScore":
                result = this.csound.ReadScore(data[1]);
                this.port.postMessage("ReadScore", result);
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
                this.port.postMessage("SetGlobalEnv", result);
                break;
            case "SetInput":
                this.csound.SetInput(data[1]);
                break;
            case "SetOption":
                result = this.csound.SetOption(data[1]);
                this.port.postMessage("SetOption", result);
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
                result = this.csound.Start();
                this.port.postMessage("Start", result);
                break;
            case "Stop":
                this.csound.Stop();
                break;
            case "TableGet":
                result = this.csound.TableGet(data[1], data[2]);
                this.port.postMessage("TableGet", result);
                break;
            case "TableLength":
                result = this.csound.TableLength(data[1]);
                this.port.postMessage("TableLength", result);
                break;
            case "TableSet":
                this.csound.TableSet(data[1], data[2]);
                break;
        }
    };
  }
  process(inputs, outputs, parameters) {
  }
}

registerProcessor(CsoundAudioProcessor, "CsoundAudioProcessor");