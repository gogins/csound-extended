#ifndef CSOUND_PRODUCER_HPP
#define CSOUND_PRODUCER_HPP

#include <csound/csound_threaded.hpp>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <sox.h>
#include <string>
#include <taglib/tlist.h>
#include <taglib/fileref.h>
#include <taglib/tfile.h>
#include <taglib/tag.h>
#include <taglib/tpropertymap.h>

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
                auto result = sox_format_init();
            }
            virtual ~CsoundProducer() {
                sox_format_quit();
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
                std::string author = tags["COMPOSER"].toString().to8Bit();
                std::string title = tags["TITLE"].toString().to8Bit();
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
            virtual void SaveMetadata(std::string filepath) {
                Message("Tagging: %s\n", filepath.c_str());
                TagLib::FileRef file_ref(filepath.c_str());
                file_ref.file()->setProperties(tags);
                file_ref.file()->save();
            }
            /**
             * Uses the SoX library to normalize the amplitude of the output 
             * soundfile to -6 dBFS.
             */
            virtual void NormalizeOutputSoundfile() {
                auto input_filename = GetBaseFilename() + "." + type;
                auto input = sox_open_read(filename.c_str(), nullptr, nullptr, nullptr);
                auto output_filename = GetFilenameBase() + ".normalized." + type;
                auto output = sox_open_write(output_filename.c_str(), &input->signal, nullptr, nullptr, nullptr);
                auto effects_chain = sox_create_effects_chain(&in->encoding, &out->encoding);
            }
            virtual void TranslateToMp3() {
            }
            virtual void TranslateToCdAudio() {
            }
            virtual void TranslateToMp4() {
            }
            virtual void TranslateToFlac() {
            }
            virtual void PostProcess() {
                Message("Began CsoundProducer::PostProcess()...\n");
                std::string output = GetFilenameBase();
                output.append(".");
                output.append(output_type);
                SaveMetadata(output);

                NormalizeOutputSoundfile();
                TranslateToMp3();
                TranslateToCdAudio();
                TranslateToMp4();
                TranslateToFlac();
                Message("Ended CsoundProducer::PostProcess().\n");
            }
            /**
             * Sets the value of a metadata tag. Valid tags include the 
             * following (see https://taglib.org/api/index.html):            
             * TITLE
             * ALBUM
             * ARTIST
             * ALBUMARTIST
             * SUBTITLE
             * TRACKNUMBER
             * DISCNUMBER
             * DATE
             * ORIGINALDATE
             * GENRE
             * COMMENT
             * COMPOSER
             * LYRICIST
             * CONDUCTOR
             * REMIXER
             * PERFORMER:<XXXX>
             * ISRC
             * ASIN
             * BPM
             * COPYRIGHT
             * ENCODEDBY
             * MOOD
             * COMMENT
             * MEDIA
             * LABEL
             * CATALOGNUMBER
             * BARCODE          
             * Other and even user-defined tags may also be used.
             */
            virtual void SetMetadata(std::string tag, std::string value) {
                tags.replace(tag.c_str(), TagLib::String(value.c_str()));
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
            TagLib::PropertyMap tags;        
            std::string git_hash;
            std::string output_type = "wav";
            std::string output_format = "float";
    };
    
};

#endif

