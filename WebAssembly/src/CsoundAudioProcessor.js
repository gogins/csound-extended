class CsoundAudioProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.csound = new Module.CsoundEmbind();
    this.is_playing = false;
    this.is_realtime = false;
    // event.method, event.value[]
    this.port.onmessage = (event) => {
        if (event.method === "Cleanup") {
            this.csound.Cleanup();
        }
    };
  }
  process(inputs, outputs, parameters) {
  }
}

registerProcessor(CsoundAudioProcessor, "csound-audio-processor");