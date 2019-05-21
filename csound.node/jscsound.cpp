/**
 * \page csoundnode csound.node
 *
 * A Node.js (and io.js and NW.js) binding for Csound. This interface should
 * mirror the Csound JavaScript interface in other environments. In csound.node, 
 * Csound has already been instantiated and initialized, and is named "csound" 
 * in the user'snJavaScript context.
 *
 * See jscsound.cpp for more information.
 */
 /*
 *
 * Copyright (C) 2015 by Michael Gogins.
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

// Must do this on Windows: https://connect.microsoft.com/VisualStudio/feedback/details/811347/compiling-vc-12-0-with-has-exceptions-0-and-including-concrt-h-causes-a-compiler-error

#include <v8.h>

#include <csound.h>
#include <csound_threaded.hpp>
#include <CsoundProducer.hpp>
#include <cstdlib>
#include <ecl/ecl.h>
#include <fstream>
#include <ios>
#include <iostream>
#include <memory>
#include <node.h>
#include <string>
#include <vector>
#include <uv.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

static csound::CsoundProducer csound_;
static uv_async_t uv_csound_message_async;
static v8::Persistent<v8::Function> csound_message_callback;
static concurrent_queue<char *> csound_messages_queue;

void cleanup(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int result = csound_.Cleanup();
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Compiles the CSD file.
 */
void compileCsd(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int result = 0;
    v8::String::Utf8Value csd_path(args[0]->ToString());
    result = csound_.CompileCsd(*csd_path);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Compiles the CSD text.
 */
void compileCsdText(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int result = 0;
    v8::String::Utf8Value csd(args[0]->ToString());
    result = csound_.CompileCsdText(*csd);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Compiles the orchestra code.
 */
void compileOrc(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value orchestraCode(args[0]->ToString());
    int result = csound_.CompileOrc(*orchestraCode);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

static v8::Persistent<v8::Function, v8::CopyablePersistentTraits<v8::Function>> console_function(v8::Isolate *isolate)
{
    static v8::Persistent<v8::Function, v8::CopyablePersistentTraits<v8::Function>> function;
    static bool initialized = false;
    if (initialized == false) {
        initialized = true;
        auto code = v8::String::NewFromUtf8(isolate, "(function(arg) {\n\
            console.log(arg);\n\
        })");
        auto maybe_local_script = v8::Script::Compile(isolate->GetCurrentContext(), code);
        v8::Local<v8::Script> local_script;
        maybe_local_script.ToLocal(&local_script);
        auto maybe_local_result = local_script->Run(isolate->GetCurrentContext());
        v8::Local<v8::Value> local_result;
        maybe_local_result.ToLocal(&local_result);
        v8::Local<v8::Function> local_function = v8::Local<v8::Function>::Cast(local_result);
        function.Reset(isolate, local_function);
    }
    return function;
}

void setMessageCallback(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    csound_message_callback.Reset(isolate, v8::Handle<v8::Function>::Cast(args[0]));
}

void csoundMessageCallback_(CSOUND *csound__, int attr, const char *format, va_list valist)
{
    char buffer[0x1000];
    std::vsprintf(buffer, format, valist);
    // Actual data...
    csound_messages_queue.push(strdup(buffer));
    // ... and notification that data is ready.
    uv_async_send(&uv_csound_message_async);
}

/**
 * Evaluates the orchestra code as an expression, and returns its value
 * as a number.
 */
void evalCode(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value orchestraCode(args[0]->ToString());
    double result = csound_.EvalCode(*orchestraCode);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Returns the numerical value of the named Csound control channel.
 */
void getControlChannel(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value channelName(args[0]->ToString());
    int result = 0;
    double value = csound_.GetChannel(*channelName, &result);
    args.GetReturnValue().Set(v8::Number::New(isolate, value));
}

/**
 * Returns 1 if automatic Git commit is enabled, 0 otherwise.
 */
void getDoGitCommit(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    bool do_git_commit = csound_.GetDoGitCommit();
    args.GetReturnValue().Set(v8::Boolean::New(isolate, do_git_commit) );
}

/**
 * Returns the numerical value of the named Csound control channel.
 */
void getMetadata(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value key(args[0]->ToString());
    int result = 0;
    auto value = csound_.GetMetadata(*key);
    args.GetReturnValue().Set(v8::String::NewFromUtf8(isolate, value.c_str()));
}

/**
 * Returns the current number of sample frames per kperiod
 * in the current Csound performance.
 */
void getKsmps(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    double value = csound_.GetKsmps();
    args.GetReturnValue().Set(v8::Number::New(isolate, value));
}

/**
 * Returns the number of audio output channels
 * in the current Csound performance.
 */
void getNchnls(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    double value = csound_.GetNchnls();
    args.GetReturnValue().Set(v8::Number::New(isolate, value));
}

/**
 * Returns Csound's current sampling rate.
 */
void getSr(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    double value = csound_.GetSr();
    args.GetReturnValue().Set(v8::Number::New(isolate, value));
}

/**
 * Returns the time in seconds from the beginning of performance.
 */
void getScoreTime(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    double value = csound_.GetScoreTime();
    args.GetReturnValue().Set(v8::Number::New(isolate, value));
}

void getVersion(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int version = csoundGetVersion();
    args.GetReturnValue().Set(v8::Number::New(isolate, version));
}

/**
 * This is provided so that the developer may verify that
 * the "csound" object exists in his or her JavaScript context.
 */
void hello(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    char buffer[0x100];
    std::sprintf(buffer, "Hello, world! This is Csound 0x%p.", csound_.GetCsound());
    args.GetReturnValue().Set(v8::String::NewFromUtf8(isolate, buffer));
}

/**
 * Evaluates the string of text, which may main contain multiple lines,
 * as a Csound score for immediate performance. The score is assumed
 * to be presorted.
 */
void inputMessage(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value scoreLines(args[0]->ToString());
    csound_.InputMessage(*scoreLines);
    args.GetReturnValue().Set(v8::Number::New(isolate, 0));
}

/**
 * Returns 1 if Csound is currently playing (synthesizing score
 * events, or 0 otherwise.
 */
void isPlaying(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    bool playing = csound_.IsPlaying();
    args.GetReturnValue().Set(v8::Boolean::New(isolate, playing) );
}

void isScorePending(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    bool is_pending = csound_.IsScorePending();
    args.GetReturnValue().Set(v8::Boolean::New(isolate, is_pending));
}

/**
 * Sends text as a message to Csound, for printing if printing is enabled.
 */
void message(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value text(args[0]->ToString());
    csound_.Message(*text);
}

void on_exit()
{
    uv_close((uv_handle_t *)&uv_csound_message_async, 0);
}

/**
 * Begins performing the score and/or producing audio.
 * It is first necessary to call compileCsd(pathname) or compileOrc(text).
 * Returns the native handle of the performance thread.
 */
void perform(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int result = csound_.PerformAndReset();
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

void performAndPostProcess(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    int result = csound_.PerformAndPostProcess();
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Evaluates the string of text, which may main contain multiple lines,
 * as a Csound score for immediate performance.
 */
void readScore(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int result = 0;
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value scoreLines(args[0]->ToString());
    csound_.ReadScore(*scoreLines);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

void reset(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    csound_.Reset();
}

void rewindScore(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    csound_.RewindScore();
}

/**
 * Runs arbitrary JavaScript code in the caller's context.
 */
double run_javascript(v8::Isolate *isolate, std::string code)
{
    v8::Handle<v8::String> source = v8::String::NewFromUtf8(isolate, code.c_str());
    auto script = v8::Script::Compile(isolate->GetCurrentContext(), source);
    v8::Local<v8::Script> local_script;
    script.ToLocal(&local_script);
    auto result = local_script->Run(isolate->GetCurrentContext());
    v8::Local<v8::Value> value;
    result.ToLocal(&value);
    return value->NumberValue();
}

/**
 * Evaluates a single score event, sent as opcode and pfields,
 * relative to the current performance time. The number of pfields
 * is read from the length of the array, not from the Csound API
 * parameter.
 */
void scoreEvent(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int result = 0;
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value javascript_opcode(args[0]->ToString());
    v8::Local<v8::Array> javascript_pfields = v8::Local<v8::Array>::Cast(args[1]);
    // There are lower-level ways of doing this, but they look complex and perhaps fragile.
    std::vector<MYFLT> pfields;
    int javascript_pfields_count = javascript_pfields->Length();
    for(int i = 0; i < javascript_pfields_count; i++) {
        v8::Local<v8::Value> element = javascript_pfields->Get(i);
        pfields.push_back(element->NumberValue());
    }
    csound_.ScoreEvent((*javascript_opcode)[0], pfields.data(), pfields.size());
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Sets the numerical value of the named Csound control channel.
 */
void setControlChannel(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value channelName(args[0]->ToString());
    v8::Local<v8::Number> v8_value = v8::Local<v8::Number>::Cast(args[1]);
    double value = v8_value->NumberValue();
    csound_.SetChannel(*channelName, value);
}

void setDoGitCommit(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    bool do_git_commit = args[0]->BooleanValue();
    csound_.SetDoGitCommit(do_git_commit);
}

void setMetadata(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value key(args[0]->ToString());
    v8::String::Utf8Value value(args[1]->ToString());
    csound_.SetMetadata(*key, *value);
}

/**
 * Sets the value of one Csound option. Spaces are not permitted.
 */
void setOption(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value option(args[0]->ToString());
    int result = csound_.SetOption(*option);
    args.GetReturnValue().Set(v8::Number::New(isolate, result));
}

/**
 * Sets the output filename, type, and format.
 */
void setOutput(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    v8::String::Utf8Value filename(args[0]->ToString());
    v8::String::Utf8Value type(args[1]->ToString());
    v8::String::Utf8Value format(args[2]->ToString());
    csound_.SetOutput(*filename, *type, *format);
}

void setScorePending(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    bool is_pending = args[0]->BooleanValue();
    csound_.SetScorePending(is_pending);
}

/**
 * Starts the Csound performance.
 */
void start(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    csound_.Start();
}

/**
 * Stops any ongoing Csound performance.
 */
void stop(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    csound_.Stop();
    // Prevent the host from restarting Csound before it has finished
    // stopping.
    csound_.Join();
}

void uv_csound_message_callback(uv_async_t *handle)
{
    v8::Isolate* isolate = v8::Isolate::GetCurrent();
    v8::HandleScope scope(isolate);
    char *message;
#if defined(_MSC_VER)
    while (csound_messages_queue.try_pop(message)) {
#else
    while (csound_messages_queue.try_pop(message)) {
#endif
        v8::Local<v8::Value> receiver;
        v8::Local<v8::Value> args[] = { v8::String::NewFromUtf8(isolate, message) };
        if (csound_message_callback.IsEmpty()) {
            auto local_function = v8::Local<v8::Function>::New(isolate, console_function(isolate));
            local_function->Call(isolate->GetCurrentContext()->Global(), 1, args);
        } else {
            auto local_csound_message_callback = v8::Local<v8::Function>::New(isolate, csound_message_callback);
            local_csound_message_callback->Call(isolate->GetCurrentContext()->Global(), 1, args);
        }
        std::free(message);
    }
}

void init(v8::Handle<v8::Object> target)
{
    csound_.SetMessageCallback(csoundMessageCallback_);
    // Keep these in alphabetical order.
    NODE_SET_METHOD(target, "Cleanup", cleanup);
    NODE_SET_METHOD(target, "CompileCsd", compileCsd);
    NODE_SET_METHOD(target, "CompileCsdText", compileCsdText);
    NODE_SET_METHOD(target, "CompileOrc", compileOrc);
    NODE_SET_METHOD(target, "EvalCode", evalCode);
    NODE_SET_METHOD(target, "GetControlChannel", getControlChannel);
    NODE_SET_METHOD(target, "GetDoGitCommit", getDoGitCommit);
    NODE_SET_METHOD(target, "GetKsmps", getKsmps);
    NODE_SET_METHOD(target, "GetMetadata", getMetadata);
    NODE_SET_METHOD(target, "GetNchnls", getNchnls);
    NODE_SET_METHOD(target, "GetScoreTime", getScoreTime);
    NODE_SET_METHOD(target, "GetSr", getSr);
    NODE_SET_METHOD(target, "GetVersion", getVersion);
    NODE_SET_METHOD(target, "Hello", hello);
    NODE_SET_METHOD(target, "InputMessage", inputMessage);
    NODE_SET_METHOD(target, "IsPlaying", isPlaying);
    NODE_SET_METHOD(target, "IsScorePending", isScorePending);
    NODE_SET_METHOD(target, "Message", message);
    NODE_SET_METHOD(target, "Perform", perform);
    NODE_SET_METHOD(target, "PerformAndPostProcess", performAndPostProcess);
    NODE_SET_METHOD(target, "ReadScore", readScore);
    NODE_SET_METHOD(target, "Reset", reset);
    NODE_SET_METHOD(target, "RewindScore", rewindScore);
    NODE_SET_METHOD(target, "ScoreEvent", scoreEvent);
    NODE_SET_METHOD(target, "SetControlChannel", setControlChannel);
    NODE_SET_METHOD(target, "SetDoGitCommit", setDoGitCommit);
    NODE_SET_METHOD(target, "SetMessageCallback", setMessageCallback);
    NODE_SET_METHOD(target, "SetMetadata", setMetadata);
    NODE_SET_METHOD(target, "SetOption", setOption);
    NODE_SET_METHOD(target, "SetOutput", setOutput);
    NODE_SET_METHOD(target, "SetScorePending", setScorePending);
    NODE_SET_METHOD(target, "Start", start);
    NODE_SET_METHOD(target, "Stop", stop);
    
    NODE_SET_METHOD(target, "cleanup", cleanup);
    NODE_SET_METHOD(target, "compileCsd", compileCsd);
    NODE_SET_METHOD(target, "compileCsdText", compileCsdText);
    NODE_SET_METHOD(target, "compileOrc", compileOrc);
    NODE_SET_METHOD(target, "evalCode", evalCode);
    NODE_SET_METHOD(target, "getControlChannel", getControlChannel);
    NODE_SET_METHOD(target, "getDoGitCommit", getDoGitCommit);
    NODE_SET_METHOD(target, "getKsmps", getKsmps);
    NODE_SET_METHOD(target, "getMetadata", getMetadata);
    NODE_SET_METHOD(target, "getNchnls", getNchnls);
    NODE_SET_METHOD(target, "getScoreTime", getScoreTime);
    NODE_SET_METHOD(target, "getSr", getSr);
    NODE_SET_METHOD(target, "getVersion", getVersion);
    NODE_SET_METHOD(target, "hello", hello);
    NODE_SET_METHOD(target, "inputMessage", inputMessage);
    NODE_SET_METHOD(target, "isPlaying", isPlaying);
    NODE_SET_METHOD(target, "isScorePending", isScorePending);
    NODE_SET_METHOD(target, "message", message);
    NODE_SET_METHOD(target, "perform", perform);
    NODE_SET_METHOD(target, "performAndPostProcess", performAndPostProcess);
    NODE_SET_METHOD(target, "readScore", readScore);
    NODE_SET_METHOD(target, "reset", reset);
    NODE_SET_METHOD(target, "rewindScore", rewindScore);
    NODE_SET_METHOD(target, "scoreEvent", scoreEvent);
    NODE_SET_METHOD(target, "setDoGitCommit", setDoGitCommit);
    NODE_SET_METHOD(target, "setControlChannel", setControlChannel);
    NODE_SET_METHOD(target, "setMessageCallback", setMessageCallback);
    NODE_SET_METHOD(target, "setMetadata", setMetadata);
    NODE_SET_METHOD(target, "performAndPostProcess", performAndPostProcess);
    NODE_SET_METHOD(target, "setOption", setOption);
    NODE_SET_METHOD(target, "setOutput", setOutput);
    NODE_SET_METHOD(target, "setScorePending", setScorePending);
    NODE_SET_METHOD(target, "start", start);
    NODE_SET_METHOD(target, "stop", stop);
    
    uv_async_init(uv_default_loop(), &uv_csound_message_async, uv_csound_message_callback);
    std::atexit(&on_exit);
}

NODE_MODULE(binding, init);

#pragma GCC diagnostic pop
