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

#include "System.hpp"
//~ #if !defined(EMSCRIPTEN)
//~ #include "CppSound.hpp"
//~ #else
//~ #include <dlfcn.h>
//~ #include <emscripten/emscripten.h>
//~ #include <emscripten/val.h>
//~ #endif
//~ #include <string.h>
//~ #include <string>
//~ #include <vector>
//~ #include <cstdarg>
//~ #include <ctime>

namespace csound {

    SILENCE_PUBLIC Logger::Logger() {
    }

    SILENCE_PUBLIC Logger::~Logger() {
    }

    SILENCE_PUBLIC void Logger::write(const char *text) {
        fprintf(stderr, "%s", text);
    }

    SILENCE_PUBLIC ThreadLock::ThreadLock() : lock(0) {
    }

    SILENCE_PUBLIC ThreadLock::~ThreadLock() {
        close();
    }

    SILENCE_PUBLIC void ThreadLock::open() {
        lock = System::createThreadLock();
    }

    SILENCE_PUBLIC void ThreadLock::close() {
        if(lock) {
            System::destroyThreadLock(lock);
            lock = 0;
        }
    }

    SILENCE_PUBLIC bool ThreadLock::isOpen() {
        return (lock != 0);
    }

    SILENCE_PUBLIC void ThreadLock::startWait(size_t milliseconds) {
        if(lock) {
            System::waitThreadLock(lock, milliseconds);
        }
    }

    SILENCE_PUBLIC void ThreadLock::endWait() {
        if(lock) {
            System::notifyThreadLock(lock);
        }
    }

    SILENCE_PUBLIC void System::setUserdata(void *userdata) {
        user_data() = userdata;
    }

    SILENCE_PUBLIC void *System::getUserdata() {
        return user_data();
    }

#if !defined(EMSCRIPTEN)

    SILENCE_PUBLIC void System::error(CSOUND *csound, const char *format,...) {
        if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(csound, ERROR_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::warn(CSOUND *csound, const char *format,...) {
        if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(csound, WARNING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::inform(CSOUND *csound, const char *format,...) {
        if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(csound, INFORMATION_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::debug(CSOUND *csound, const char *format,...) {
        if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(csound, DEBUGGING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::message(CSOUND *csound, int level, const char *format,...) {
        if((level & message_level(-1)) == level) {
            va_list marker;
            va_start(marker, format);
            message(csound, level, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::message(CSOUND *csound, const char *format,...) {
        va_list marker;
        va_start(marker, format);
        message(csound, message_level(-1), format, marker);
        va_end(marker);
    }

    SILENCE_PUBLIC void System::message(CSOUND *csound, const char *format, va_list valist) {
        System::message(csound, 0, format, valist);
    }

    SILENCE_PUBLIC void System::message(CSOUND *csound, int attribute, const char *format, va_list valist) {
        if(log_file()) {
            vfprintf(log_file(), format, valist);
            fflush(log_file());
        }
        if(message_callback()) {
            message_callback()(csound, attribute, format, valist);
        } else {
            vfprintf(stderr, format, valist);
        }
    }

    SILENCE_PUBLIC void System::error(const char *format,...) {
        if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message((CSOUND*) user_data(), ERROR_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::warn(const char *format,...) {
        if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message((CSOUND*) user_data(), WARNING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::inform(const char *format,...) {
        if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message((CSOUND*) user_data(), INFORMATION_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::debug(const char *format,...) {
        if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message((CSOUND*) user_data(), DEBUGGING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::message(const char *format,...) {
        va_list marker;
        va_start(marker, format);
        message((CSOUND*) user_data(), message_level(-1), format, marker);
        va_end(marker);
    }

    SILENCE_PUBLIC void System::message(const char *format, va_list valist) {
        message((CSOUND*) user_data(), message_level(-1), format, valist);
    }

#else

    SILENCE_PUBLIC void System::error(const char *format,...) {
        if((ERROR_LEVEL & message_level(-1)) == ERROR_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(ERROR_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::warn(const char *format,...) {
        if((WARNING_LEVEL & message_level(-1)) == WARNING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(WARNING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::inform(const char *format,...) {
        if((INFORMATION_LEVEL & message_level(-1)) == INFORMATION_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(INFORMATION_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::debug(const char *format,...) {
        if((DEBUGGING_LEVEL & message_level(-1)) == DEBUGGING_LEVEL) {
            va_list marker;
            va_start(marker, format);
            message(DEBUGGING_LEVEL, format, marker);
            va_end(marker);
        }
    }

    SILENCE_PUBLIC void System::message(const char *format,...) {
        va_list marker;
        va_start(marker, format);
        message(format, marker);
        va_end(marker);
    }

    SILENCE_PUBLIC void System::message(const char *format, va_list valist) {
        message(message_level(-1), format, valist);
    }

    SILENCE_PUBLIC void System::message(int error_level, const char *format, va_list valist) {
        char buffer[0x2000];
        std::vsnprintf(buffer, 0x2000, format, valist);
        if(message_callback() == emscripten::val::undefined() || message_callback() == emscripten::val::null()) {
            emscripten_log(EM_LOG_CONSOLE, buffer);
        } else {
            message_callback().call<void>(buffer);
        }
    }

#endif

    SILENCE_PUBLIC void System::message(std::string text) {
        message(text.c_str());
    }

    SILENCE_PUBLIC int System::setMessageLevel(int messageLevel_) {
        return message_level(messageLevel_);
    }

    SILENCE_PUBLIC int System::getMessageLevel() {
        return message_level(-1);
    }

    SILENCE_PUBLIC void System::setMessageCallback(MessageCallbackType messageCallback_) {
        message_callback() = messageCallback_;
    }

    SILENCE_PUBLIC MessageCallbackType System::getMessageCallback() {
        return message_callback();
    }

    SILENCE_PUBLIC clock_t System::startTiming() {
        return clock();
    }

    SILENCE_PUBLIC double System::stopTiming(clock_t beganAt) {
        clock_t endedAt = clock();
        clock_t elapsed = endedAt - beganAt;
        return double(elapsed) / double(CLOCKS_PER_SEC);
    }

    SILENCE_PUBLIC void System::yieldThread() {
    }

    SILENCE_PUBLIC int System::openLibrary(void **library, std::string filename) {
#if !defined(EMSCRIPTEN)
        return csoundOpenLibrary(library, filename.c_str());
#else
        *library = dlopen(filename.c_str(), RTLD_NOW | RTLD_GLOBAL);
        return (int) *library;
#endif
    }

    SILENCE_PUBLIC void *System::getSymbol(void *library, std::string name) {
        void *procedureAddress = 0;
#if !defined(EMSCRIPTEN)
        procedureAddress = csoundGetLibrarySymbol(library, name.c_str());
#else
        procedureAddress = dlsym(library, name.c_str());
#endif
        return procedureAddress;
    }

    SILENCE_PUBLIC void System::closeLibrary(void *library) {
#if !defined(EMSCRIPTEN)
        csoundCloseLibrary(library);
#else
        dlclose(library);
#endif
    }

    SILENCE_PUBLIC void System::setLogfile(FILE *logfile_) {
        log_file() = logfile_;
    }

    SILENCE_PUBLIC FILE *System::getLogfile() {
        return log_file();
    }

#if defined(WIN32)

#include <process.h>
#include <windows.h>
#include <stdlib.h>
#include <io.h>

    SILENCE_PUBLIC int System::execute(const char *command) {
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

    SILENCE_PUBLIC int System::shellOpen(const char *filename, const char *command) {
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

    SILENCE_PUBLIC void System::parsePathname(const std::string pathname,
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

    SILENCE_PUBLIC std::vector<std::string> System::getFilenames(std::string path) {
        std::vector<std::string> names;
        struct _finddata_t finddata;
        int intptr = _findfirst(path.c_str(), &finddata);
        if(intptr != -1) {
            if((finddata.attrib & _A_SUBDIR) != _A_SUBDIR) {
                names.push_back(finddata.name);
            }
            while(_findnext(intptr, &finddata) != -1) {
                if((finddata.attrib & _A_SUBDIR) != _A_SUBDIR) {
                    names.push_back(finddata.name);
                }
            }
            _findclose(intptr);
        }
        return names;
    }

    SILENCE_PUBLIC std::vector<std::string> System::getDirectoryNames(std::string path) {
        std::vector<std::string> names;
        struct _finddata_t finddata;
        int intptr = _findfirst(path.c_str(), &finddata);
        if(intptr != -1) {
            if((finddata.attrib & _A_SUBDIR) == _A_SUBDIR) {
                if(strcmp(finddata.name, ".") == 0) {
                } else if(strcmp(finddata.name, "..") == 0) {
                } else {
                    names.push_back(finddata.name);
                }
            }
            while(_findnext(intptr, &finddata) != -1) {
                if((finddata.attrib & _A_SUBDIR) == _A_SUBDIR) {
                    if(strcmp(finddata.name, ".") == 0) {
                    } else if(strcmp(finddata.name, "..") == 0) {
                    } else {
                        names.push_back(finddata.name);
                    }
                }
            }
            _findclose(intptr);
        }
        return names;
    }

    SILENCE_PUBLIC void *System::createThread(void (*threadRoutine)(void *threadData), void *data, int priority) {
        return (void *) _beginthread(threadRoutine, (unsigned int)0, data);
    }

    SILENCE_PUBLIC void *System::createThreadLock() {
        return (void *)CreateEvent(0, false, false, 0);
    }

    SILENCE_PUBLIC void System::waitThreadLock(void *lock, size_t milliseconds) {
        WaitForSingleObject((HANDLE) lock, milliseconds);
    }

    SILENCE_PUBLIC void System::notifyThreadLock(void *lock) {
        SetEvent((HANDLE) lock);
    }

    SILENCE_PUBLIC void System::destroyThreadLock(void *lock) {
        CloseHandle((HANDLE) lock);
    }

    SILENCE_PUBLIC std::string System::getSharedLibraryExtension() {
        return "dll";
    }

    SILENCE_PUBLIC void System::sleep(double milliseconds) {
        Sleep((int)(milliseconds + 0.999));
    }

    SILENCE_PUBLIC void System::beep() {
        Beep(880, 1000);
    }

#elif defined(__linux__) || defined(__APPLE__)

#include <dlfcn.h>
#include <dirent.h>
#include <libgen.h>

    SILENCE_PUBLIC std::string System::getSharedLibraryExtension() {
        return "so";
    }

    SILENCE_PUBLIC size_t strlcpy(char *d, const char *s, size_t bufsize) {
        size_t len = strlen(s);
        size_t ret = len;
        if(bufsize <= 0) return 0;
        if(len >= bufsize) len = bufsize-1;
        memcpy(d, s, len);
        d[len] = '\0';
        return ret;
    }

#ifndef __APPLE__
    SILENCE_PUBLIC char *basename(const char *path)
#else
    SILENCE_PUBLIC char *basename_(const char *path)
#endif
    {
        static char bname[NAME_MAX + 1];
        const char *endp, *startp;

        /* Empty or NULL string gets treated as "." */
        if(path == NULL || *path == '\0') {
            (void)strlcpy(bname, ".", sizeof bname);
            return(bname);
        }

        /* Strip trailing slashes */
        endp = path + strlen(path) - 1;
        while(endp > path && *endp == '/')
            endp--;

        /* All slashes become "/" */
        if(endp == path && *endp == '/') {
            (void)strlcpy(bname, "/", sizeof bname);
            return(bname);
        }

        /* Find the start of the base */
        startp = endp;
        while(startp > path && *(startp - 1) != '/')
            startp--;

        if((int)(endp - startp) + 2 > (int) sizeof(bname)) {
            //errno = ENAMETOOLONG;
            return(NULL);
        }
        strlcpy(bname, startp, endp - startp + 2);
        return(bname);
    }

    SILENCE_PUBLIC char *dirname(const char *path) {
        static char bname[NAME_MAX +1];
        const char *endp;

        /* Empty or NULL string gets treated as "." */
        if(path == NULL || *path == '\0') {
            (void)strlcpy(bname, ".", sizeof(bname));
            return(bname);
        }

        /* Strip trailing slashes */
        endp = path + strlen(path) - 1;
        while(endp > path && *endp == '/')
            endp--;

        /* Find the start of the dir */
        while(endp > path && *endp != '/')
            endp--;

        /* Either the dir is "/" or there are no slashes */
        if(endp == path) {
            (void)strlcpy(bname, *endp == '/' ? "/" : ".", sizeof bname);
            return(bname);
        } else {
            do {
                endp--;
            } while(endp > path && *endp == '/');
        }

        if((int)(endp - path) + 2 > (int) sizeof(bname)) {
            //errno = ENAMETOOLONG;
            return(NULL);
        }
        strlcpy(bname, path, endp - path + 2);
        return(bname);
    }

    SILENCE_PUBLIC int System::execute(const char *command) {
        int returnValue = -1;
        #if defined(__APPLE__)
            System::error("System::execute not defined on macOS.\n");
        #else
            returnValue = fork();
            if(!returnValue) {
                std::vector<std::string> args;
                std::vector<char *> argv;
                std::string buffer = command;
                scatterArgs(buffer, args, argv);
                argv.push_back((char*) 0);      // argv[] should be null-terminated
                execvp(argv[0], &argv.front());
            }
        #endif
        return returnValue;
    }

    SILENCE_PUBLIC int System::shellOpen(const char *filename, const char *command) {
        std::string buffer = filename;
        buffer += " ";
        buffer += command;
        return System::execute(buffer.c_str());
    }

    SILENCE_PUBLIC void System::parsePathname(const std::string pathname,
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
#ifndef __APPLE__
        file = basename(fileTemp);
#else
        file = basename_(fileTemp);
#endif

        int periodPosition = pathname.find_last_of(".");
        if(periodPosition != -1) {
            extension = pathname.substr(periodPosition + 1);
        }
        free(dirTemp);
        free(fileTemp);
    }

    SILENCE_PUBLIC std::vector<std::string> System::getFilenames(std::string path) {
        std::vector<std::string> names;
        return names;
    }

    SILENCE_PUBLIC std::vector<std::string> System::getDirectoryNames(std::string path) {
        std::vector<std::string> names;
        return names;
    }

    SILENCE_PUBLIC void *System::createThread(void (*threadRoutine)(void *threadData), void *data, int priority) {
        pthread_t *pthread = new pthread_t;
        if(pthread_create(pthread,
                0,
                (void *(*)(void*)) threadRoutine,
                data) == 0) {
            return pthread;
        } else {
            delete pthread;
            return 0;
        }
    }

    SILENCE_PUBLIC void *System::createThreadLock() {
        pthread_mutex_t *pthread_mutex = new pthread_mutex_t;
        if(pthread_mutex_init(pthread_mutex, 0) == 0) {
            // for consistency with Win32 version
            pthread_mutex_trylock(pthread_mutex);
            return pthread_mutex;
        } else {
            delete pthread_mutex;
            return 0;
        }
    }

    SILENCE_PUBLIC void System::waitThreadLock(void *lock, size_t milliseconds) {
        pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
        /* int returnValue = */ pthread_mutex_lock(pthread_mutex);
    }

    SILENCE_PUBLIC void System::notifyThreadLock(void *lock) {
        pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
        /* int returnValue = */ pthread_mutex_unlock(pthread_mutex);
    }

    SILENCE_PUBLIC void System::destroyThreadLock(void *lock) {
        pthread_mutex_t *pthread_mutex = (pthread_mutex_t *)lock;
        /* int returnValue = */ pthread_mutex_destroy(pthread_mutex);
        delete pthread_mutex;
        /* pthread_mutex = 0; */
    }

    SILENCE_PUBLIC void System::sleep(double milliseconds) {
        csoundSleep((int)(milliseconds + 0.999));
    }

    SILENCE_PUBLIC void System::beep() {
        beep();
    }

#endif

}
