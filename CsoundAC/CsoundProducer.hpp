#ifndef CSOUND_PRODUCER_HPP
#define CSOUND_PRODUCER_HPP

#include <csound/csound_threaded.hpp>
#include <cstdio>
#include <cstdlib>
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
     * composition.
     */
    class CsoundProducer : public CsoundThreaded {
        CsoundProducer(){
            auto result = sox_format_init();
        };
        virtual ~CsoundProducer(){
            sox_format_quit();
        };
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
                std::tmpnam(temporary_filename);
                char command[0x200];
                std::sprintf(command, "git rev-parse --short HEAD > %s", temporary_filename);
                auto result = std::system(command);
                std::fstream hash_file(temporary_filename);
                char hash[0x100];
                hash_file.getline(hash);
                return hash;
            } else {
                return "";
            }
        }
        virtual void TagForId3v2(std::string filepath) {
        }
        /**
         * Uses the SoX library to normalize the amplitude of the output 
         * soundfile to -6 dBFS.
         */
        virtual void NormalizeOutputSoundfile() {
            sox_format_t *input;
            sox_effects_chain_t *effects_chain;
            sox_effect_t *normalizer;
            sox_format_t *output;
            
            
        }
        virtual void TranslateToMp3();
        virtual void TranslateToCdAudio();
        virtual void TranslateToMp4();
        virtual void TranslateToFlac();
        virtual void PostProcess();
        virtual void PerformAndPostProcess();
        /**
         * Sets the name of the composer or other author.
         */
        virtual void SetAuthor(std::string author_) {
            author = author_;
        }
        /**
         * Returns the name of the composer or other author.
         */
        virtual std::string GetAuthor() const {
            return author;
        }
        /**
         * Sets the name of the performer or other artist. For computer music, 
         * the artist is usually the same as the author.
         */
        virtual void SetArtist(std::string artist_) {
            artist = artist_;
        }
        /**
         * Returns the name of the performer or other artist. For computer 
         * music, the artist is usually the same as the author.
         */
        virtual std::string GetArtist() const {
            return artist;
        }
        /**
         * Sets the title of the composition.
         */
        virtual void SetTitle(std::string title_) {
            title = title_;
        }
        /**
         * Returns the title of the composition.
         */
        virtual std::string GetTitle() const {
            return title;
        }
        /**
         * Sets the album of the composition.
         */
        virtual void SetAlbum(std::string album_) {
            album = album_;
        }
        /**
         * Returns the album of the composition.
         */
        virtual std::string GetAlbum() const {
            return album;
        }
        /**
         * Sets the year of the composition.
         */
        virtual void SetYear(std::string year_) {
            year = year_;
        }
        /**
         * Returns the year of the composition.
         */
        virtual std::string GetYear() const {
            return year;
        }
        /**
         * Sets the track number of the composition.
         */
        virtual void SetTrack(int track_) {
            track = track_;
        }
        /**
         * Returns the track number of the composition.
         */
        virtual int GetTrack() const {
            return track;
        }
        /**
         * Sets the license of the composition.
         */
        virtual void SetLicense(std::string license_) {
            license = license_;
        }
        /**
         * Returns the license of the composition.
         */
        virtual std::string GetLicense() const {
            return license;
        }
        /**
         * Sets the copyright of the composition.
         */
        virtual void SetCopyright(std::string copyright_) {
            copyright = copyright_;
        }
        /**
         * Returns the copyright of the composition.
         */
        virtual std::string GetCopyright() const {
            return copyright;
        }
        /**
         * Sets the publisher of the composition.
         */
        virtual void SetPublisher(std::string publisher_) {
            publisher = publisher_;
        }
        /**
         * Returns the publisher of the composition.
         */
        virtual std::string GetPublisher() const {
            return publisher_;
        }
    protected:
        bool do_git_commit = false;
        bool do_post_processing = true;
        std::string author;
        std::string artist;
        std::string title;
        std::string album;
        std::string year;
        int track;
        std::string license;
        std::string copyright;
        std::string publisher;
        std::string git_hash;
    };
    
};

#endif

