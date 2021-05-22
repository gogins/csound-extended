/*
 * C S O U N D
 *
 * L I C E N S E
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#pragma once

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
/* #if !defined(EMSCRIPTEN)
 * #include "CppSound.hpp"
 * #endif
 */
#include <string.h>
#include <string>
#include <vector>
#include <cstdarg>
#include <ctime>
%}
#else
#if !defined(EMSCRIPTEN)
#include "CppSound.hpp"
#else
#include <dlfcn.h>
#include <emscripten/emscripten.h>
#include <emscripten/val.h>
#endif
#include <string.h>
#include <string>
#include <vector>
#include <cstdarg>
#include <ctime>
#endif

namespace csound
{
class SILENCE_PUBLIC Logger
{
public:
    Logger();
    virtual ~Logger();
    virtual void write(const char *text);
};

#if !defined(EMSCRIPTEN)
typedef void (*MessageCallbackType)(CSOUND *csound, int attribute, const char *format, va_list marker);
#else
typedef emscripten::val MessageCallbackType;
#endif

// Note that static variables are wrapped as static variables inside 
// functions, and are lvalues as well as rvalues. This simplifies life with 
// header-only libraries.

inline SILENCE_PUBLIC int message_level(int verbosity) {
    static int verbosity_ = 1;
    auto prior_verbosity_ = verbosity_;
    if (verbosity != -1) {
        verbosity_ = verbosity;
    }
    return prior_verbosity_;
}

inline MessageCallbackType &message_callback() {
#if !defined(EMSCRIPTEN)
    static MessageCallbackType message_callback_ = nullptr;
#else
    static emscripten::val message_callback_ = emscripten::val::undefined();
#endif
    return message_callback_;
}

inline SILENCE_PUBLIC FILE* &log_file() {
    static FILE *logfile_ = nullptr;
    return logfile_;
}

inline SILENCE_PUBLIC void* &user_data() {
    static void *user_data_ = nullptr;
    return user_data_;
}

/**
 * Abstraction layer for a minimal set of system services.
 */
class SILENCE_PUBLIC System
{
public:
    enum Level {
        ERROR_LEVEL             = 1,
        WARNING_LEVEL           = 2,
        INFORMATION_LEVEL       = 4,
        DEBUGGING_LEVEL         = 8
    };
    /**
     *  Parses a filename into its component parts, which are returned in the arguments.
     *  On Unix and Linux, "drive" is always empty.
     */
    static void parsePathname(const std::string pathname, std::string &drive, std::string &base, std::string &file, std::string &extension);
    /**
     *  Opens a shared library; useful for loading plugins.
     */
    static int openLibrary(void **library, std::string filename);
    /**
     *  Returns the address of a symbol (function or object) in a shared library;
     *  useful for loading plugin functions.
     */
    static void *getSymbol(void *library, std::string name);
    /**
     *  Closes a shared library.
     */
    static void closeLibrary(void *library);
    /**
     *  Lists filenames in a directory;
     *  useful for locating plugins.
     */
    static std::vector<std::string> getFilenames(std::string directoryName);
    /**
     *  Lists directory names in a directory;
     *  useful for locating plugins.
     */
    static std::vector<std::string> getDirectoryNames(std::string directoryName);
    /**
     *  Creates a new thread.
     */
    static void *createThread(void (*threadRoutine)(void *threadData), void *data, int priority);
    /**
     *  Creates a thread lock.
     */
    static void *createThreadLock();
    /**
     *  Waits on a thread lock.
     * Zero timeout means infinite timeout.
     */
    static void waitThreadLock(void *lock, size_t timeoutMilliseconds = 0);
    /**
     *  Releases a thread lock.
     */
    static void notifyThreadLock(void *lock);
    /**
     *  Destroys a thread lock.
     */
    static void destroyThreadLock(void *lock);
    /**
     *  Sets message level, returns old message level.
     */
    static int setMessageLevel(int messageLevel);
    /**
     *  Yields to the next waiting thread.
     */
    static void yieldThread();
    /**
     *  Returns current system message level.
     */
    static int getMessageLevel();
    /**
     *  Sets userdata for message printing.
     */
    static void setUserdata(void *userdata);
    /**
     * Returns userdata for message printing.
     */
    static void *getUserdata();

    /**
     * Set a stream for printing messages to
     * (in addition to callback, stderr, etc.).
     */
    static void setLogfile(FILE *logfile);

    /**
     * Return the stream, if any, used for
     * printing messages to.
     */
    static FILE *getLogfile();
#if !defined(SWIG)
    #if !defined(EMSCRIPTEN)
    /**
     *  Prints a message if the ERROR_LEVEL flag is set.
     */
    static void error(CSOUND *csound, const char *format,...);
    /**
     *  Prints a message if the WARNNING_LEVEL flag is set.
     */
    static void warn(CSOUND *csound, const char *format,...);
    /**
     *  Prints a message if the INFORMATION_LEVEL flag is set.
     */
    static void inform(CSOUND *csound, const char *format,...);
    /**
     *  Prints a message if the DEBUGGING_LEVEL flag is set.
     */
    static void debug(CSOUND *csound, const char *format,...);
    /**
     *  Prints a message.
     */
    static void message(CSOUND *csound, const char *format,...);
    /**
     *  Prints a message.
     */
    static void message(CSOUND *csound, const char *format, va_list valist);
    /**
     *  Prints a message.
     */
    static void message(CSOUND *csound, int level, const char *format,...);
    /**
     *  Unconditionally prints a message. This is the lowest-level message 
     *  function that calls the message callback, if one has been set.
     */
    static void message(CSOUND *csound, int attribute, const char *format, va_list valist);
    #endif
    /**
     *  Prints a message.
     */
    static void message(const char *format, va_list valist);
#endif
    /**
     *  Prints a message if the ERROR_LEVEL flag is set.
     */
    static void error(const char *format,...);
    /**
     *  Prints a message if the WARNNING_LEVEL flag is set.
     */
    static void warn(const char *format,...);
    /**
     *  Prints a message if the INFORMATION_LEVEL flag is set.
     */
    static void inform(const char *format,...);
    /**
     *  Prints a message if the DEBUGGING_LEVEL flag is set.
     */
    static void debug(const char *format,...);
    /**
     *  Prints a message.
     */
    static void message(const char *format,...);
#if defined(EMSCRIPTEN)
    /**
     *  Unconditionally prints a message. This is the lowest-level message 
     *  function that calls the message callback, if one has been set.
     */
    static void message(int attribute, const char *format, va_list valist);
#endif
    /**
     *  Prints a message.
     */
    static void message(std::string text);
    /**
     *  Sets message callback.
     */
    static void setMessageCallback(MessageCallbackType messageCallback_);
    /**
     *  Return the message callback, or null if none.
     */
    static MessageCallbackType getMessageCallback();
    /**
     *  Execute a system command or program.
     */
    static int execute(const char *command);
    /**
     *  Open a file using the operating system shell.
     */
    static int shellOpen(const char *filename, const char *command = "open");
    /**
     *  Returns the standard filename extension for a shared library,
     *  such as "dll" or "so".
     */
    static std::string getSharedLibraryExtension();
    /**
     *  Starts timing.
     */
    static clock_t startTiming();
    /**
     *  Stop timing, and return elapsed seonds.
     */
    static double stopTiming(clock_t startedAt);
    /**
     *  Sleep the indicated number of milliseconds.
     */
    static void sleep(double milliseconds);
    /**
     *  Make some sort of noticeable sound.
     */
    static void beep();
    static void message_text(std::string text) {
        message(text.c_str());
    }
    static void debug_text(std::string text) {
        debug(text.c_str());
    }
    static void inform_text(std::string text) {
        inform(text.c_str());
    }
    static void warn_text(std::string text) {
        warn(text.c_str());
    }
    static void error_text(std::string text) {
        error(text.c_str());
    }
};

/**
 * Encapsulates a thread monitor, such as a Windows event handle.
 */
class SILENCE_PUBLIC ThreadLock
{
    void *lock;
public:
    ThreadLock();
    virtual ~ThreadLock();
    /**
     * Creates and initializes the monitor.
     * The monitor is in a non-notified or unsignaled state.
     */
    virtual void open();
    /**
     * Destroys the monitor.
     */
    virtual void close();
    /**
     * Returns whether the monitor is open.
     */
    virtual bool isOpen();
    /**
     * Waits until the monitor is notified by another thread.
     * Zero timeout means infinite timeout.
     */
    virtual void startWait(size_t timeoutMilliseconds = 0);
    /**
     * Releases one thread that is waiting on the monitor.
     */
    virtual void endWait();
};

inline SILENCE_PUBLIC Logger::Logger() {
}

inline SILENCE_PUBLIC Logger::~Logger() {
}

inline SILENCE_PUBLIC void Logger::write(const char *text) {
    fprintf(stderr, "%s", text);
}

inline SILENCE_PUBLIC ThreadLock::ThreadLock() : lock(0) {
}

inline SILENCE_PUBLIC ThreadLock::~ThreadLock() {
    close();
}

inline SILENCE_PUBLIC void ThreadLock::open() {
    lock = System::createThreadLock();
}

inline SILENCE_PUBLIC void ThreadLock::close() {
    if(lock) {
        System::destroyThreadLock(lock);
        lock = 0;
    }
}

inline SILENCE_PUBLIC bool ThreadLock::isOpen() {
    return (lock != 0);
}

inline SILENCE_PUBLIC void ThreadLock::startWait(size_t milliseconds) {
    if(lock)
    {
        System::waitThreadLock(lock, milliseconds);
    }
}

inline SILENCE_PUBLIC void ThreadLock::endWait() {
    if(lock) {
        System::notifyThreadLock(lock);
    }
}

inline SILENCE_PUBLIC void System::setUserdata(void *userdata) {
    user_data() = userdata;
}

inline SILENCE_PUBLIC void *System::getUserdata() {
    return user_data();
}

#if !defined(EMSCRIPTEN)

inline SILENCE_PUBLIC void System::error(CSOUND *csound, const char *format,...) {
    if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(csound, ERROR_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::warn(CSOUND *csound, const char *format,...) {
    if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(csound, WARNING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::inform(CSOUND *csound, const char *format,...) {
    if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(csound, INFORMATION_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::debug(CSOUND *csound, const char *format,...) {
    if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(csound, DEBUGGING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::message(CSOUND *csound, int level, const char *format,...) {
    if((level & message_level(-1)) == level) {
        va_list marker;
        va_start(marker, format);
        message(csound, level, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::message(CSOUND *csound, const char *format,...) {
    va_list marker;
    va_start(marker, format);
    message(csound, message_level(-1), format, marker);
    va_end(marker);
}

inline SILENCE_PUBLIC void System::message(CSOUND *csound, const char *format, va_list valist) {
    System::message(csound, 0, format, valist);
}

inline SILENCE_PUBLIC void System::message(CSOUND *csound, int attribute, const char *format, va_list valist) {
    if (log_file()) {
        vfprintf(log_file(), format, valist);
        fflush(log_file());
    }
    if(message_callback()) {
        message_callback()(csound, attribute, format, valist);
    }
    else {
        vfprintf(stderr, format, valist);
    }
}

inline SILENCE_PUBLIC void System::error(const char *format,...) {
    if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message((CSOUND*) user_data(), ERROR_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::warn(const char *format,...) {
    if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message((CSOUND*) user_data(), WARNING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::inform(const char *format,...) {
    if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message((CSOUND*) user_data(), INFORMATION_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::debug(const char *format,...) {
    if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message((CSOUND*) user_data(), DEBUGGING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::message(const char *format,...) {
    va_list marker;
    va_start(marker, format);
    message((CSOUND*) user_data(), message_level(-1), format, marker);
    va_end(marker);
}

inline SILENCE_PUBLIC void System::message(const char *format, va_list valist) {
    message((CSOUND*) user_data(), message_level(-1), format, valist);
}

#else

inline SILENCE_PUBLIC void System::error(const char *format,...) {
    if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(ERROR_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::warn(const char *format,...) {
    if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(WARNING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::inform(const char *format,...) {
    if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(INFORMATION_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::debug(const char *format,...) {
    if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
        va_list marker;
        va_start(marker, format);
        message(DEBUGGING_LEVEL, format, marker);
        va_end(marker);
    }
}

inline SILENCE_PUBLIC void System::message(const char *format,...) {
    va_list marker;
    va_start(marker, format);
    message(format, marker);
    va_end(marker);
}

inline SILENCE_PUBLIC void System::message(const char *format, va_list valist) {
    message(message_level(-1), format, valist);
}

inline SILENCE_PUBLIC void System::message(int error_level, const char *format, va_list valist) {
    char buffer[0x2000];
    std::vsnprintf(buffer, 0x2000, format, valist);
    if (message_callback() == emscripten::val::undefined() || message_callback() == emscripten::val::null()) {
        emscripten_log(EM_LOG_CONSOLE, buffer);
    } else {
        message_callback().call<void>(buffer);
    }
}

#endif

inline SILENCE_PUBLIC void System::message(std::string text) {
    message(text.c_str());
}

inline SILENCE_PUBLIC int System::setMessageLevel(int messageLevel_) {
     return message_level(messageLevel_);
}

inline SILENCE_PUBLIC int System::getMessageLevel() {
    return message_level(-1);
}

inline SILENCE_PUBLIC void System::setMessageCallback(MessageCallbackType messageCallback_) {
    message_callback() = messageCallback_;
}

inline SILENCE_PUBLIC MessageCallbackType System::getMessageCallback() {
    return message_callback();
}

inline SILENCE_PUBLIC clock_t System::startTiming() {
    return clock();
}

inline SILENCE_PUBLIC double System::stopTiming(clock_t beganAt) {
    clock_t endedAt = clock();
    clock_t elapsed = endedAt - beganAt;
    return double(elapsed) / double(CLOCKS_PER_SEC);
}

inline SILENCE_PUBLIC void System::yieldThread() {
}

inline SILENCE_PUBLIC int System::openLibrary(void **library, std::string filename) {
#if !defined(EMSCRIPTEN)
    return csoundOpenLibrary(library, filename.c_str());
#else
    *library = dlopen(filename.c_str(), RTLD_NOW | RTLD_GLOBAL);
    return (int) *library;
#endif
}

inline SILENCE_PUBLIC void *System::getSymbol(void *library, std::string name) {
    void *procedureAddress = 0;
#if !defined(EMSCRIPTEN)
    procedureAddress = csoundGetLibrarySymbol(library, name.c_str());
#else
    procedureAddress = dlsym(library, name.c_str());
#endif
    return procedureAddress;
}

inline SILENCE_PUBLIC void System::closeLibrary(void *library) {
#if !defined(EMSCRIPTEN)
    csoundCloseLibrary(library);
#else
    dlclose(library);
#endif
}

inline SILENCE_PUBLIC void System::setLogfile(FILE *logfile_) {
    log_file() = logfile_;
}

inline SILENCE_PUBLIC FILE *System::getLogfile() {
    return log_file();
}

#if defined(WIN32)

#include <process.h>
#include <windows.h>
#include <stdlib.h>
#include <io.h>

inline SILENCE_PUBLIC int System::execute(const char *command) {
    STARTUPINFO startupInfo;
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    PROCESS_INFORMATION processInformation;
    memset(&processInformation, 0, sizeof(processInformation));
    return CreateProcess(0,
                         const_cast<char *>(command),
                         0,
                         0,
                         0,
                         DETACHED_PROCESS,
                         0,
                         0,
                         &startupInfo,
                         &processInformation);
}

inline SILENCE_PUBLIC int System::shellOpen(const char *filename, const char *command) {
    int returnValue = 0;
    intptr_t hInstance = (intptr_t) ShellExecute(0,
                         command,
                         filename,
                         0,
                         0,
                         SW_SHOWNORMAL);
    returnValue = !(hInstance > 32);
    return returnValue;
}

inline SILENCE_PUBLIC void System::parsePathname(const std::string pathname,
                           std::string &drive,
                           std::string &base,
                           std::string &file,
                           std::string &extension) {
    char drive_[_MAX_DRIVE];
    char base_[_MAX_DIR];
    char file_[_MAX_FNAME];
    char extension_[_MAX_EXT];
    _splitpath(pathname.c_str(), drive_, base_, file_, extension_);
    drive = drive_;
    base = base_;
    file = file_;
    extension = extension_;
}

inline SILENCE_PUBLIC std::vector<std::string> System::getFilenames(std::string path) {
    std::vector<std::string> names;
    struct _finddata_t finddata;
    int intptr = _findfirst(path.c_str(), &finddata);
    if(intptr != -1)
    {
        if((finddata.attrib & _A_SUBDIR) != _A_SUBDIR)
        {
            names.push_back(finddata.name);
        }
        while(_findnext(intptr, &finddata) != -1)
        {
            if((finddata.attrib & _A_SUBDIR) != _A_SUBDIR)
            {
                names.push_back(finddata.name);
            }
        }
        _findclose(intptr);
    }
    return names;
}

inline SILENCE_PUBLIC std::vector<std::string> System::getDirectoryNames(std::string path) {
    std::vector<std::string> names;
    struct _finddata_t finddata;
    int intptr = _findfirst(path.c_str(), &finddata);
    if(intptr != -1)
    {
        if((finddata.attrib & _A_SUBDIR) == _A_SUBDIR)
        {
            if(strcmp(finddata.name, ".") == 0)
            {
            }
            else if(strcmp(finddata.name, "..") == 0)
            {
            }
            else
            {
                names.push_back(finddata.name);
            }
        }
        while(_findnext(intptr, &finddata) != -1)
        {
            if((finddata.attrib & _A_SUBDIR) == _A_SUBDIR)
            {
                if(strcmp(finddata.name, ".") == 0)
                {
                }
                else if(strcmp(finddata.name, "..") == 0)
                {
                }
                else
                {
                    names.push_back(finddata.name);
                }
            }
        }
        _findclose(intptr);
    }
    return names;
}

inline SILENCE_PUBLIC void *System::createThread(void (*threadRoutine)(void *threadData), void *data, int priority) {
    return (void *) _beginthread(threadRoutine, (unsigned int)0, data);
}

inline SILENCE_PUBLIC void *System::createThreadLock() {
    return (void *)CreateEvent(0, false, false, 0);
}

inline SILENCE_PUBLIC void System::waitThreadLock(void *lock, size_t milliseconds) {
    WaitForSingleObject((HANDLE) lock, milliseconds);
}

inline SILENCE_PUBLIC void System::notifyThreadLock(void *lock) {
    SetEvent((HANDLE) lock);
}

inline SILENCE_PUBLIC void System::destroyThreadLock(void *lock) {
    CloseHandle((HANDLE) lock);
}

inline SILENCE_PUBLIC std::string System::getSharedLibraryExtension() {
    return "dll";
}

inline SILENCE_PUBLIC void System::sleep(double milliseconds) {
    Sleep((int) (milliseconds + 0.999));
}

inline SILENCE_PUBLIC void System::beep() {
    Beep(880, 1000);
}

#elif defined(__linux__) || defined(MACOSX)

#include <dlfcn.h>
#include <dirent.h>
#include <libgen.h>

inline SILENCE_PUBLIC std::string System::getSharedLibraryExtension() {
    return "so";
}

inline SILENCE_PUBLIC size_t strlcpy(char *d, const char *s, size_t bufsize) {
    size_t len = strlen(s);
    size_t ret = len;
    if (bufsize <= 0) return 0;
    if (len >= bufsize) len = bufsize-1;
    memcpy(d, s, len);
    d[len] = '\0';
    return ret;
}

#ifndef MACOSX
inline SILENCE_PUBLIC char *basename(const char *path)
#else
inline SILENCE_PUBLIC char *basename_(const char *path)
#endif
{
    static char bname[NAME_MAX + 1];
    const char *endp, *startp;

    /* Empty or NULL string gets treated as "." */
    if (path == NULL || *path == '\0') {
        (void)strlcpy(bname, ".", sizeof bname);
        return(bname);
    }

    /* Strip trailing slashes */
    endp = path + strlen(path) - 1;
    while (endp > path && *endp == '/')
        endp--;

    /* All slashes become "/" */
    if (endp == path && *endp == '/') {
        (void)strlcpy(bname, "/", sizeof bname);
        return(bname);
    }

    /* Find the start of the base */
    startp = endp;
    while (startp > path && *(startp - 1) != '/')
        startp--;

    if ((int) (endp - startp) + 2 > (int) sizeof(bname)) {
        //errno = ENAMETOOLONG;
        return(NULL);
    }
    strlcpy(bname, startp, endp - startp + 2);
    return(bname);
}

inline SILENCE_PUBLIC char *dirname(const char *path) {
    static char bname[NAME_MAX +1];
    const char *endp;

    /* Empty or NULL string gets treated as "." */
    if (path == NULL || *path == '\0') {
        (void)strlcpy(bname, ".", sizeof(bname));
        return(bname);
    }

    /* Strip trailing slashes */
    endp = path + strlen(path) - 1;
    while (endp > path && *endp == '/')
        endp--;

    /* Find the start of the dir */
    while (endp > path && *endp != '/')
        endp--;

    /* Either the dir is "/" or there are no slashes */
    if (endp == path) {
        (void)strlcpy(bname, *endp == '/' ? "/" : ".", sizeof bname);
        return(bname);
    } else {
        do {
            endp--;
        } while (endp > path && *endp == '/');
    }

    if ((int) (endp - path) + 2 > (int) sizeof(bname)) {
        //errno = ENAMETOOLONG;
        return(NULL);
    }
    strlcpy(bname, path, endp - path + 2);
    return(bname);
}

inline SILENCE_PUBLIC int System::execute(const char *command) {
    int returnValue = fork();
    if(!returnValue)
    {
        std::vector<std::string> args;
        std::vector<char *> argv;
        std::string buffer = command;
        scatterArgs(buffer, args, argv);
        argv.push_back((char*) 0);      // argv[] should be null-terminated
        execvp(argv[0], &argv.front());
    }
    return returnValue;
}

inline SILENCE_PUBLIC int System::shellOpen(const char *filename, const char *command) {
    std::string buffer = filename;
    buffer += " ";
    buffer += command;
    return System::execute(buffer.c_str());
}

inline SILENCE_PUBLIC void System::parsePathname(const std::string pathname,
                           std::string &drive,
                           std::string &directory,
                           std::string &file,
                           std::string &extension) {
    drive.erase();
    directory.erase();
    file.erase();
    extension.erase();
    //~ char *dirTemp = Conversions::dupstr(pathname.c_str());
    char *dirTemp = strdup(pathname.c_str());
    directory = dirname(dirTemp);
    //~ char *fileTemp = Conversions::dupstr(pathname.c_str());
    char *fileTemp = strdup(pathname.c_str());
#ifndef MACOSX
    file = basename(fileTemp);
#else
    file = basename_(fileTemp);
#endif

    int periodPosition = pathname.find_last_of(".");
    if(periodPosition != -1)
    {
        extension = pathname.substr(periodPosition + 1);
    }
    free(dirTemp);
    free(fileTemp);
}

inline SILENCE_PUBLIC std::vector<std::string> System::getFilenames(std::string path) {
    std::vector<std::string> names;
    return names;
}

inline SILENCE_PUBLIC std::vector<std::string> System::getDirectoryNames(std::string path) {
    std::vector<std::string> names;
    return names;
}

inline SILENCE_PUBLIC void *System::createThread(void (*threadRoutine)(void *threadData), void *data, int priority) {
    pthread_t *pthread = new pthread_t;
    if(pthread_create(pthread,
                      0,
                      (void *(*) (void*)) threadRoutine,
                      data) == 0)
    {
        return pthread;
    }
    else
    {
        delete pthread;
        return 0;
    }
}

inline SILENCE_PUBLIC void *System::createThreadLock() {
    pthread_mutex_t *pthread_mutex = new pthread_mutex_t;
    if(pthread_mutex_init(pthread_mutex, 0) == 0)
    {
        // for consistency with Win32 version
        pthread_mutex_trylock(pthread_mutex);
        return pthread_mutex;
    }
    else
    {
        delete pthread_mutex;
        return 0;
    }
}

inline SILENCE_PUBLIC void System::waitThreadLock(void *lock, size_t milliseconds) {
    pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
    /* int returnValue = */ pthread_mutex_lock(pthread_mutex);
}

inline SILENCE_PUBLIC void System::notifyThreadLock(void *lock) {
    pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
    /* int returnValue = */ pthread_mutex_unlock(pthread_mutex);
}

inline SILENCE_PUBLIC void System::destroyThreadLock(void *lock) {
    pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
    /* int returnValue = */ pthread_mutex_destroy(pthread_mutex);
    delete pthread_mutex;
    /* pthread_mutex = 0; */
}

inline SILENCE_PUBLIC void System::sleep(double milliseconds) {
    csoundSleep((int) (milliseconds + 0.999));
}

inline SILENCE_PUBLIC void System::beep() {
    beep();
}

#endif

}
