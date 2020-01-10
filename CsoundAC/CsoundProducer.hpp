#ifndef CSOUND_PRODUCER_HPP
#define CSOUND_PRODUCER_HPP

#include <Python.h>
#include <csound/csound_threaded.hpp>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <fstream>
#include <iostream>
extern "C"
{
    #include <lua.h>
    #include <lauxlib.h>
    #include <lualib.h>
}
#include <map>
#include <ecl/ecl.h>
#include <string>

namespace csound {
    
#if !defined(SWIG)
    
template<typename T> struct is_cl_object { constexpr static bool p = false; };
template<> struct is_cl_object<cl_object> { constexpr static bool p = true; };
template<typename...> struct are_cl_objects { constexpr static bool p = true; };
template<typename Head, typename... Tail> struct are_cl_objects<Head, Tail...> {
constexpr static bool p = is_cl_object<Head>::p && are_cl_objects<Tail...>::p; };
    
/**
 * This function must be called with the arc and argv from main() before any 
 * Lisp code is executed.
 */
inline void initialize_ecl(int argc, char **argv)
{
    static bool initialized_ecl = false;
    if (initialized_ecl) {
        return;
    } 
    initialized_ecl = true;
    cl_boot(argc, argv);
    atexit(cl_shutdown);
    std::printf("Initialized Embeddable Common Lisp.\n");
}

/**
 * Evaluates a _SINGLE_ Lisp form. Please note, in Embeddable Common Lisp, 
 * `(require :xxx)` and some other forms work only if they are at the top 
 * level. That may necessitate repeated calls to this function from the 
 * embedding system.
 */
inline cl_object evaluate_form(const std::string &form) {
    return cl_eval(c_string_to_object(form.c_str()));
}
    
/**
 * Translate a Lisp string to a C++ string.
 */
std::string to_std_string(cl_object lisp_object) {
    std::string result;
    size_t length = 0;
    bool is_unicode = ECL_EXTENDED_STRING_P(lisp_object);
    if (is_unicode) {
        length = lisp_object->string.dim;
        for (size_t i = 0; i < length; ++i) {
            auto c = lisp_object->string.self[i];
            result += (char) c;
        }
    } else {
        length = lisp_object->base_string.dim;
        for (size_t i = 0; i < length; ++i) {
            auto c = lisp_object->base_string.self[i];
            result += (char) c;
        }
    }
    return result;
}
    
/**
 * Creates a DEFUN abstraction in C++.
 */
template <typename... Params>
void defun(const std::string &name, cl_object fun(Params... params)) {
    static_assert(are_cl_objects<Params...>::p, "ECL Functions may only take cl_object as argument.");
    ecl_def_c_function(c_string_to_object(name.c_str()), (cl_objectfn_fixed)fun, sizeof...(Params));
}

#endif
    
    /**
     * Uses ffmpeg to translate a soundfile to a normalized output 
     * file, an MP3 file, a CD audio file, a FLAC soundfile, and an 
     * MP4 video file suitable for posting to YouTube. All files are 
     * tagged with metadata. This function is called automatically by 
     * PerformAndPostProcess. 
     */
    static void PostProcess(std::map<std::string, std::string> &tags, std::string filename) {
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
     * a composition to a soundfile. Also enables running scripts that 
     * can interact with Csound.
     */
    class CsoundProducer : public CsoundThreaded {
        public:
            CsoundProducer() {
                // Inject this class' Csound object into the runtime context 
                // of various scripting languages.
            }
            virtual ~CsoundProducer() {
            }
            virtual clock_t startTiming() {
                return clock();
            }
            virtual double stopTiming(clock_t beganAt)
            {
                clock_t endedAt = clock();
                clock_t elapsed = endedAt - beganAt;
                return double(elapsed) / double(CLOCKS_PER_SEC);
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
                // We do our own performance benchmarks.
                auto clock_started = startTiming();
                Message("Began CsoundProducer::PerformAndPostProcessRoutine...\n");
                auto result = CsoundThreaded::PerformAndResetRoutine();
                auto seconds = stopTiming(clock_started);
                Message("Rendering took %9.4f seconds.\n", seconds);
                clock_started = startTiming();
                auto output_filename = GetFilenameBase();
                output_filename.append(".");
                output_filename.append(output_type);
                //if (result == 0) {
                    Message("Post-processing output file: %s.\n", output_filename.c_str());
                    PostProcess(tags, output_filename);
                //}
                seconds = stopTiming(clock_started);
                Message("Post-processing %s took %9.4f seconds.\n", output_filename.c_str(), seconds);                
                Message("Ended CsoundProducer::PerformAndPostProcessRoutine with %d.\n", result);
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
            virtual lua_State *GetLuaState() {
                return L;
            }
            virtual void InitializeLuaJIT(lua_State *L_ = nullptr) {
                if (L != nullptr) {
                    lua_close(L);
                }
                if (L_ == nullptr) {
                    L = luaL_newstate();
                    luaL_openlibs(L);
                } else {
                    L = L_;
                }
                // Ensure that this instance of Csound is available in the 
                // LuaJIT runtime context. 
                lua_pushlightuserdata(L, csound);
                lua_setfield(L, LUA_GLOBALSINDEX, "csound");
            }
            virtual void InitializePython() {
                Py_Initialize();
                // Ensure that this instance of Csound is available in the 
                // Python runtime context. For ctcsound, the CSOUND pointer 
                // is just an int.
                char code[0x100];
                std::snprintf(code, 0x100, "csound = int(%p)", csound);
                PyRun_SimpleString(code);
            }
            virtual cl_env_ptr GetCommonLispEnvironment() {
                return common_lisp_environment;
            }
            virtual void InitializeCommonLisp(int argc, const char **argv) {
                initialize_ecl(argc, (char **)argv);
                common_lisp_environment = ecl_process_env();
                // Ensure that this instance of Csound is available in the 
                // Embeddable Common Lisp runtime context. 
                // This is done by creating a CFFI form with the address of 
                // this instance of Csound. It becomes a global pointer *csound*.
                const char *template_ = "(defparameter *csound* %ld)";
                char form[0x100];
                std::snprintf(form, 0x100, template_, csound);
                cl_object lisp_csound = evaluate_form(form);
            }
            /**
             * Runs a script in a dynamic language, e.g. to generate a score 
             * for Csound to render. This instance of Csound is exposed in the 
             * runtime context of the script as a raw pointer or handle named 
             * "csound"; the script itself must define or load a 
             * usable interface to this Csound object. RunScript is normally 
             * called after Compile* and either before or after Perform, and 
             * can be called any number of times during rendering. The 
             * available languages are "LuaJIT", "Python3.6m", and 
             * "Common Lisp".
             */
            virtual int RunScript(const std::string script, const std::string language) {
                int result = 0;
                if (language == "LuaJIT") {
                    const char *luacode = script.c_str();
                    Message("Executing (L: 0x%p) Lua code.\n", L);
                    result = luaL_dostring(L, luacode);
                    if (result == 0) {
                        //log(csound, "Result: %d\n", result);
                    } else {
                        Message("luaL_dostring failed with: %d\n%s\n", result, lua_tostring(L, -1));
                    }
                } else if (language == "Python3.6m") {
                    result = PyRun_SimpleString(script.c_str());
                    if (result == 0) {
                        //log(csound, "Result: %d\n", result);
                    } else {
                        Message("PyRun_SimpleString failed with: %d\n", result);
                    }
                } else if (language == "Common Lisp") {
                    evaluate_form(script.c_str());
                } else {
                    Message("Sorry, CsoundProducer does not support %s in this environment.\n", language.c_str());
                    return -1;
                }
                return result;
            }
        protected:
            bool do_git_commit = false;
            std::map<std::string, std::string> tags;        
            std::string git_hash;
            std::string output_type = "wav";
            std::string output_format = "float";
            lua_State *L = nullptr;
            cl_env_ptr common_lisp_environment = nullptr;
    };
    
};

#endif

