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

namespace csound {
    
    
    /**
     * Uses ffmpeg to translate a soundfile to a normalized output 
     * file, an MP3 file, a CD audio file, a FLAC soundfile, and an 
     * MP4 video file suitable for posting to YouTube. All files are 
     * tagged with metadata. This function is called automatically by 
     * PerformAndPostProcess. 
     */
    static void PostProcess(std::map<std::string, std::string> tags, std::string filename) {
        auto period_index = filename.rfind(".");
        std::string filename_base = filename.substr(0, period_index);
        std::string filename_extension = filename.substr(period_index + 1, std::string::npos);
        std::printf("Began PostProcess(%s)...\n", filename.c_str());
        std::string tag_options;
        for (auto it = tags.begin(); it != tags.end(); ++it) {
            tag_options.append(" ");
            tag_options.append("-metadata ");
            tag_options.append(it->first);
            tag_options.append("=\"");
            tag_options.append(it->second);
            tag_options.append("\" ");
        }
         char buffer[0x1000];
        // FFmpeg requires two passes to SET the loudness.
        // http://k.ylo.ph/2016/04/04/loudnorm.html
        const char *volumedetect_command = "ffmpeg -i %s.%s -af \"volumedetect\" -vn -sn -dn -f null /dev/null 2>&1";
        std::snprintf(buffer, 0x1000, volumedetect_command, filename_base.c_str(), filename_extension.c_str());
        FILE *pipe = popen(buffer, "r");
        double max_volume = 0;
        while (std::fgets(buffer, 0x1000, pipe) != nullptr) {
            auto found = std::strstr(buffer, "max_volume: ");
            if (found != nullptr) {
                found = std::strstr(found, " ");
                max_volume = std::atof(found);
                std::printf("Original maximum level: %9.4f dBFS\n", max_volume);
                break;
            }
        }
        auto result = pclose(pipe);
        max_volume = (max_volume + 6) * -1;
        std::printf("Correction: %9.4f dB\n", max_volume);
        
        const char *volume_command = "ffmpeg -y -i %s.%s -filter:a \"volume=%fdB\" -codec:a pcm_s32le -format:a flt %s \"%s-normalized.wav\"";
        std::snprintf(buffer, 0x1000, volume_command, filename_base.c_str(), filename_extension.c_str(), max_volume, tag_options.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("Volume command:      %s\n", buffer);
        result = std::system(buffer);
        
        const char *mp3_command = "ffmpeg -y -i %s-normalized.wav -acodec libmp3lame -b:a 192k -r:a 48k %s \"%s.mp3\"";
        std::snprintf(buffer, 0x1000, mp3_command, filename_base.c_str(), tag_options.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("MP3 command:         %s\n", buffer);
        result = std::system(buffer);
        
        const char *cda_command = "ffmpeg -y -i %s-normalized.wav -acodec pcm_s16le -ar 44100 -ac 2 -f wav %s \"%s.cd.wav\"";
        std::snprintf(buffer, 0x1000, cda_command, filename_base.c_str(), tag_options.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("CD audio command:    %s\n", buffer);
        result = std::system(buffer);
        
        const char *flac_command = "ffmpeg -y -i %s-normalized.wav -af aformat=s32 %s \"%s.flac\"";
        std::snprintf(buffer, 0x1000, flac_command, filename_base.c_str(), tag_options.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("FLAC command:        %s\n", buffer);
        result = std::system(buffer);
        
        const char *png_command = "ffmpeg -y -i %s.cd.wav -lavfi showspectrumpic=s=wxga:mode=separate %s.png";
        std::snprintf(buffer, 0x1000, png_command, filename_base.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("Spectrogram command: %s\n", buffer);
        result = std::system(buffer);

        const char *mp4_command = "ffmpeg -y -loop 1 -framerate 2 -i %s.png -i %s-normalized.wav -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" %s %s-unlabeled.mp4";
        std::snprintf(buffer, 0x1000, mp4_command, filename_base.c_str(), filename_base.c_str(), tag_options.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("MP4 command:         %s\n", buffer);
        result = std::system(buffer);
        
        std::string artist = tags["artist"];
        std::string title = tags["title"];
        std::string publisher = tags["publisher"];
        std::string copyright = tags["copyright"];
        std::string license = tags["license"];
        const char *label_command = "ffmpeg -y -i %s-unlabeled.mp4 -max_muxing_queue_size 9999 -vf drawtext=fontfile=OpenSans-Regular.ttf:text='%s\n%s\n%s\n%s %s':fontcolor=white:fontsize=36:alpha=.5:x=w/2-tw/2:y=h/6 -codec:a copy %s.mp4";
        std::snprintf(buffer, 0x1000, label_command, filename_base.c_str(), artist.c_str(), title.c_str(), publisher.c_str(), copyright.c_str(), license.c_str(), filename_base.c_str());
        std::printf("%s", buffer);
        std::printf("Label command:       %s\n", buffer);
        result = std::system(buffer);
        std::printf("Ended PostProcess().\n");
    }

    /** 
     * Optionally adds metadata, performs post-processing, translates to 
     * various soundfile formats as automatic steps in the Csound rendering of 
     * a composition to a soundfile. 
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
                auto output_filename = GetFilenameBase();
                output_filename.append(".");
                output_filename.append(output_type);
                PostProcess(tags, output_filename);
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

