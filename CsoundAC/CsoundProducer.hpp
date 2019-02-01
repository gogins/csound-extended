#include <csound/csound_threaded.hpp>
#include <string>
#include <git2.h>

namespace csound {
    
    class CsoundProducer : public CsoundThreaded {
        CsoundProducer(){};
        virtual ~CsoundProducer(){};
        virtual void GitCommit();
        virtual void TagForId3v2();
        virtual void TagForBwav();
        virtual void NormalizeOutputSoundfile();
        virtual void TranslateToMp3();
        virtual void TranslateToCdAudio();
        virtual void TranslateToMp4();
        virtual void TranslateToFlac();
        virtual void PostProcess();
        virtual void PerformAndPostProcess();
    protected:
        std::string author;
        std::string title;
        std::string license;
        std::string copyright;
        std::string publisher;
        std::string git_hash;
        std::map<std::string, std::string> additional_metadata>;
        bool do_git_commit = false;
        bool do_post_processing = true;
        std::string output_filename;
        std::string normalized_output_filename;
        std::string mp3_filename;
        std::string cd_audio_filename;
        std::string mp4_filename;
        std::string flac_filename;
    };
    
};
