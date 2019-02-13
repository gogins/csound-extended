#ifndef CSOUND_PRODUCER_HPP
#define CSOUND_PRODUCER_HPP

#include <csound/csound_threaded.hpp>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <string>

//#include <unistd.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/opt.h>

namespace csound {
    
    /** 
     * Adds metadata, performs post-processing, and translates to various 
     * soundfile formats as automatic steps in the Csound rendering of a 
     * composition. Unless the output is real-time audio, the output filename 
     * is always created from the metadata.
     */
    class CsoundProducer : public CsoundThreaded {
        public:
            CsoundProducer() {
            }
            virtual ~CsoundProducer() {
            }
            /**
             * If enabled, assumes that the code embedding this piece is within a 
             * Git repository, and commits the repository before rendering the piece 
             * to ensure a consistent history of revisions of the piece.
             */
            virtual void GitCommit() {
                if (do_git_commit == true) {
                    auto result = std::system("git commit -a -m \"Automatically committed before rendering.\"");
                }
            }
            /**
             * If Git commit is enabled, assumes that the code embedding this 
             * piece is within a Git repository and returns the current Git hash 
             * of HEAD, to facilitate ensuring a consistent history of revisions 
             * of the piece. Otherwise, returns an empty string.
             */
            virtual std::string GetGitCommitHash() {
                if (do_git_commit == true) {
                    char temporary_filename[0x200];
                    auto discard = std::tmpnam(temporary_filename);
                    char command[0x200];
                    std::sprintf(command, "git rev-parse --short HEAD > %s", temporary_filename);
                    auto result = std::system(command);
                    std::fstream hash_file(temporary_filename);
                    char hash[0x100];
                    hash_file.getline(hash, 0x200);
                    return hash;
                } else {
                    return "";
                }
            }
            /**
             * Returns the base used for all filenames, which is formed from 
             * author-title[-git_hash] with all spaces replaced by underscores.
             */
            virtual std::string GetFilenameBase() {
                std::string author = GetMetadata("artist");
                std::string title = GetMetadata("title");
                std::string filename_base = author + "-" + title;
                if (do_git_commit == true) {
                    filename_base.append("-");
                    filename_base.append(GetGitCommitHash());
                }
                for (int i = 0, n = filename_base.size(); i < n; ++i) {
                    if (filename_base[i] == ' ') {
                        filename_base[i] = '_';
                    }
                }
                return filename_base;
            }
            /**
             * Uses ffmpeg to translate the output soundfile of this piece to 
             * a normalized output file, an MP3 file, a CD audio file, a FLAC 
             * soundfile, and an MP4 video file suitable for posting to 
             * YouTube. All files are tagged with metadata. 
             */
            virtual void PostProcess() {
                Message("Began CsoundProducer::PostProcess()...\n");
                std::string filename_base = GetFilenameBase();
                char buffer[0x1000];
                // FFmpeg requires two passes to SET the loudness.
                // http://k.ylo.ph/2016/04/04/loudnorm.html
                const char *volumedetect_command = "ffmpeg -i %s.%s -af \"volumedetect\" -vn -sn -dn -f null /dev/null 2>&1";
                std::snprintf(buffer, 0x1000, volumedetect_command, filename_base.c_str(), output_type.c_str());
                FILE *pipe = popen(buffer, "r");
                double max_volume = 0;
                while (std::fgets(buffer, 0x1000, pipe) != nullptr) {
                    auto found = std::strstr(buffer, "max_volume: ");
                    if (found != nullptr) {
                        found = std::strstr(found, " ");
                        max_volume = std::atof(found);
                        Message("Original maximum level: %f dBFS\n", max_volume);
                        break;
                    }
                }
                auto result = pclose(pipe);
                max_volume = (max_volume + 6) * -1;
                Message("Correction: %f dB\n", max_volume);
                const char *volume_command = "ffmpeg -i %s.%s -filter:a \"volume=%fdB\" \"%s-normalized.wav\"";
                std::snprintf(buffer, 0x1000, volume_command, filename_base.c_str(), output_type.c_str(), max_volume, filename_base.c_str());
                std::printf(buffer);
                result = std::system(buffer);
                Message("Ended CsoundProducer::PostProcess().\n");
            }
            /**
             * Sets the value of a metadata tag. See:
             * https://www.ffmpeg.org/doxygen/trunk/group__metadata__api.html
             * Other and even user-defined tags may also be used.
             */
            virtual void SetMetadata(std::string tag, std::string value) {
                tags[tag] = value;
            }
            /** 
             * Returns the value of the metadata for the tag, or an empty 
             * string if the tag does not exist.
             */
            virtual std::string GetMetadata(std::string tag) const {
                if (tags.find(tag) != tags.end()) {
                    return tags.at(tag);
                } else {
                    return "";
                }
            }
            /**
             * Override to not only set but also save type and format.
             */
            virtual void SetOutput(const char *name, const char *type, const char *format) {
                output_type = type;
                output_format = format;
                Csound::SetOutput(name, type, format);
            }
            /**
             * Override to set output filename from metadata in this. Not 
             * implemented for real-time rendering.
             */        
            virtual int Start() {
                std::string output = GetOutputName();
                if (output.find("dac") != 0) {
                    output = GetFilenameBase();
                    output.append(".");
                    output.append(output_type);
                    Csound::SetOutput(output.c_str(), output_type.c_str(), output_format.c_str());
                }
                return CsoundThreaded::Start();
            }
            virtual int PerformAndPostProcessRoutine() {
                Message("Began CsoundProducer::PerformAndPostProcessRoutine()...\n");
                auto result = CsoundThreaded::PerformAndResetRoutine();
                PostProcess();
                Message("Ended CsoundProducer::PerformAndPostProcessRoutine() with %d.\n", result);
                return result;
            }
            /**
             * Like PerformAndReset, but performs post-processing, translation, 
             * and tagging after rendering, so that these things are all done 
             * in the rendering thread.
             */
            virtual int PerformAndPostProcess() {
                performance_thread = std::thread(&CsoundProducer::PerformAndPostProcessRoutine, this);
                return 0;
            }
            virtual void SetDoGitCommit(bool do_git_commit_) {
                do_git_commit = do_git_commit_; 
            }
            virtual bool GetDoGitCommit() const {
                return do_git_commit;
            }
        protected:
            bool do_git_commit = false;
            std::map<std::string, std::string> tags;        
            std::string git_hash;
            std::string output_type = "wav";
            std::string output_format = "float";
    };
    
};

#endif

