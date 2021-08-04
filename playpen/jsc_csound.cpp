#if (defined(WIN32) || defined(_WIN32)) && !defined(SWIG) && !defined(_MSC_VER)
#  define SILENCE_PUBLIC __declspec(dllexport)
#elif defined(__GNUC__) && (__GNUC__ >= 4) /* && !defined(__MACH__) */
#  define SILENCE_PUBLIC        __attribute__ ( (visibility("default")) )
#else
#  define SILENCE_PUBLIC
#endif

#include <memory>
#include <string>
#include <cstring>
#include <csound/csound.h>
#include <csound/csound.hpp>
#include <csound/csound_threaded.hpp>
#include <glib-2.0/glib.h>
#include <glib-2.0/gmodule.h>
#include <libsoup-2.4/libsoup/soup.h>
#include <libsoup-2.4/libsoup/soup-address.h>
#include <gtk/gtk.h>
#include <gtk/gtk.h>
#include <webkit2/webkit-web-extension.h>

/**
* Wrap a subset of CsoundThreaded as a JavaScriptCore WebExtension. That
* should be the same as the interface of CsoundThread in csound.i here,
* wrapped by SWIG and working fine in Python.
*/

/**
* The Csound object in this JavaScript context is a singleton object, that
* if possible wraps a pointer to the playpen's global Csound instance.
*/
static std::unique_ptr<CsoundThreaded> csound_;

extern "C" {

    void destroy_notify_(gpointer data) {
        std::printf("destroy_notify: %p\n", data);
    }

    void on_gjs_csound_hello() {
        std::printf("Hello from Csound!\n");
    }

    static JSCContext *js_context = nullptr;
    static JSCClass *csound_class = nullptr;
    static JSCValue *csound_constructor = nullptr;
    static JSCValue *csound_message_callback = nullptr;
    static char browser_csound_callback_name[] = "csound_message_callback";

    static void native_csound_message_callback(CSOUND *, int attr, const char *format, va_list valist) {
        //~ char message_buffer[0x2000];
        //~ std::vsprintf(message_buffer, format, valist);
        //~ // Try invoking the actual callable. This doesn't currently work.
        //~ if (csound_message_callback != nullptr) {
        //~ JSCValue *jsc_message_buffer = jsc_value_new_string(js_context, message_buffer);
        //~ g_object_ref(jsc_message_buffer);
        //~ JSCValue *result = jsc_value_function_call(csound_message_callback,
        //~ JSC_TYPE_VALUE,
        //~ jsc_message_buffer,
        //~ G_TYPE_NONE);
        //~ }
        //~ // Try just invoking JavaScript. This doesn't currently work.
        //~ char javascript_code[0x2100];
        //~ std::sprintf(javascript_code, "%s(\"%s\");", browser_csound_callback_name, message_buffer);
        //~ std::printf("native_csound_message_callback: invoking: %s\n", javascript_code);
        //~ JSCValue *result = jsc_context_evaluate(js_context, javascript_code, -1);
        //~ std::printf("native_csound_message_callback: result: %s\n", jsc_value_to_string(result));
        std::vfprintf(stderr, format, valist);
    }

    static JSCValue *csound_constructor_callback(gpointer data) {
        std::printf("csound_constructor_callback: data: %p\n", data);
        JSCValue *new_instance = jsc_value_new_object(js_context, NULL, csound_class);
        return new_instance;
    }

    static int Csound_Cleanup(JSCValue *instance, gpointer user_data) {
        int result = csound_->Cleanup();
        std::printf("Csound_Cleanup: result: %d\n", result);
        return result;
    }

    static int Csound_CompileCsd(JSCValue *instance, char *csd_filepath, gpointer user_data) {
        // std::printf("Csound_CompileCsd: csd_filepath: %s\n", csd_filepath);
        int result = csound_->CompileCsd(csd_filepath);
        std::printf("Csound_CompileCsd: result: %d\n", result);
        return result;
    }

    static int Csound_CompileCsdText(JSCValue *instance, char *csd_text, gpointer user_data) {
        // std::printf("Csound_CompileCsdText: csd_text: %s\n", csd_text);
        int result = csound_->CompileCsdText(csd_text);
        std::printf("Csound_CompileCsdText: result: %d\n", result);
        return result;
    }

    static void Csound_CreateMessageBuffer(JSCValue *instance, int to_stdout, gpointer user_data) {
        std::printf("Csound_CreateMessageBuffer: to_stdout: %d\n", to_stdout);
        csound_->CreateMessageBuffer(to_stdout);
    }

    static int Csound_GetMessageCnt(JSCValue *instance, gpointer user_data) {
        int result = csound_->GetMessageCnt();
        std::printf("Csound_GetMessageCnt: result: %d\n", result);
        return result;
    }

    static int Csound_GetVersion(JSCValue *instance, gpointer user_data) {
        int result = csound_->GetVersion();
        std::printf("Csound_GetVersion: result: %d\n", result);
        return result;
    }

    static JSCValue *Csound_GetFirstMessage(JSCValue *instance, gpointer user_data) {
        const char *message = csound_->GetFirstMessage();
        std::printf("Csound_GetFirstMessage: message: %s\n", message);
        JSCValue *jsc_message = jsc_value_new_string(js_context, message);
        g_object_ref_sink(jsc_message);
        return jsc_message;
    }

    static void Csound_Perform(JSCValue *instance, gpointer user_data) {
        std::printf("Csound_Perform\n");
        csound_->Perform();
    }

    static void Csound_PopFirstMessage(JSCValue *instance, gpointer user_data) {
        std::printf("Csound_PopFirstMessage\n");
        csound_->PopFirstMessage();
    }

    static int Csound_Render(JSCValue *instance, char *csd_text, gpointer user_data) {
        // std::printf("Csound_Render: csd_text: %s\n", csd_text);
        int result = csound_->CompileCsdText(csd_text);
        result = result + csound_->Start();
        result = result + csound_->Perform();
        std::printf("Csound_Render: result: %d\n", result);
        return result;
    }

    static void Csound_Reset(JSCValue *instance, gpointer user_data) {
        std::printf("Csound_Reset\n");
        csound_->Reset();
    }

    static void Csound_SetControlChannel(JSCValue *instance, char *name, double value, gpointer user_data) {
        // std::printf("Csound_SetControlChannel: name: %s value: %f\n", name, value);
        csound_->SetControlChannel(name, value);
    }

    static void Csound_SetMessageCallback(JSCValue *instance, JSCValue *csound_message_callback_, gpointer user_data) {
        std::printf("Csound_SetMessageCallback: csound_message_callback: %p\n", csound_message_callback_);
        csound_message_callback = csound_message_callback_;
        g_object_ref(csound_message_callback);
        std::printf("Csound_SetMessageCallback: csound_message_callback type: %s %s\n", G_OBJECT_TYPE_NAME(csound_message_callback), jsc_value_to_string(csound_message_callback));
        csound_->SetMessageCallback(native_csound_message_callback);
    }

    static void Csound_Start(JSCValue *instance, gpointer user_data) {
        std::printf("Csound_Start\n");
        csound_->Start();
    }

    static void Csound_Stop(JSCValue *instance, gpointer user_data) {
        std::printf("Csound_Stop");
        csound_->Stop();
        csound_->Join();
    }

    /**
    * Apparently, only at this time is it possible for WebExtensions to
    * inject native code into WebKit's JavaScript context.
    */
    static void
    window_object_cleared_callback (WebKitScriptWorld *world,
                                    WebKitWebPage     *web_page,
                                    WebKitFrame       *frame,
                                    GVariant          *user_data)
    {
        js_context = webkit_frame_get_js_context_for_script_world(frame, world);
        JSCValue *js_global_object = jsc_context_get_global_object(js_context);
        csound_.reset(new CsoundThreaded());
        JSCValue *gjs_csound_hello = jsc_value_new_function(js_context,
                                     NULL,
                                     G_CALLBACK(on_gjs_csound_hello),
                                     NULL,
                                     NULL,
                                     G_TYPE_NONE,
                                     0);
        jsc_context_set_value (js_context,
                               "gjs_csound_hello",
                               gjs_csound_hello);
        // First the class must be registered.
        csound_class = jsc_context_register_class(js_context, "Csound", NULL, NULL, NULL);
        std::printf("window_object_cleared_callback: defined Csound class: %p name: %s\n",
                    csound_class, jsc_class_get_name(csound_class));
        // Then its constructor must be defined.
        csound_constructor = jsc_class_add_constructor (csound_class,
                             "Csound",
                             GCallback(csound_constructor_callback),
                             user_data,
                             destroy_notify_,
                             G_TYPE_OBJECT,
                             0);
        // jsc_class_add_method (JSCClass *jsc_class,
        // const char *name,
        // GCallback callback,
        // gpointer user_data,
        // GDestroyNotify destroy_notify,
        // GType return_type,
        // guint n_params,
        // ...);
        // NOTE: Elementary types can be returned as such.
        jsc_class_add_method(csound_class,
                             "GetVersion",
                             G_CALLBACK(Csound_GetVersion),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             0);
        jsc_class_add_method(csound_class,
                             "Cleanup",
                             G_CALLBACK(Csound_Cleanup),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             0);
        jsc_class_add_method(csound_class,
                             "CompileCsd",
                             G_CALLBACK(Csound_CompileCsd),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             1,
                             G_TYPE_STRING);
        jsc_class_add_method(csound_class,
                             "CompileCsdText",
                             G_CALLBACK(Csound_CompileCsdText),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             1,
                             G_TYPE_STRING);
        jsc_class_add_method(csound_class,
                             "CreateMessageBuffer",
                             G_CALLBACK(Csound_CreateMessageBuffer),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             1,
                             G_TYPE_INT);
        jsc_class_add_method(csound_class,
                             "GetFirstMessage",
                             G_CALLBACK(Csound_GetFirstMessage),
                             user_data,
                             NULL,
                             JSC_TYPE_VALUE,
                             0);
        jsc_class_add_method(csound_class,
                             "GetMessageCnt",
                             G_CALLBACK(Csound_GetMessageCnt),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             0);
        jsc_class_add_method(csound_class,
                             "Perform",
                             G_CALLBACK(Csound_Perform),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             0);
        jsc_class_add_method(csound_class,
                             "PopFirstMessage",
                             G_CALLBACK(Csound_PopFirstMessage),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             0);
        jsc_class_add_method(csound_class,
                             "Render",
                             G_CALLBACK(Csound_Render),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             1,
                             G_TYPE_STRING);
        jsc_class_add_method(csound_class,
                             "Reset",
                             G_CALLBACK(Csound_Reset),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             0);
        jsc_class_add_method(csound_class,
                             "SetControlChannel",
                             G_CALLBACK(Csound_SetControlChannel),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             2,
                             G_TYPE_STRING,
                             G_TYPE_DOUBLE);
        jsc_class_add_method(csound_class,
                             "SetMessageCallback",
                             G_CALLBACK(Csound_SetMessageCallback),
                             user_data,
                             NULL,
                             G_TYPE_NONE,
                             1,
                             JSC_TYPE_VALUE);
        jsc_class_add_method(csound_class,
                             "Start",
                             G_CALLBACK(Csound_Start),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             0);
        jsc_class_add_method(csound_class,
                             "Stop",
                             G_CALLBACK(Csound_Stop),
                             user_data,
                             NULL,
                             G_TYPE_INT,
                             1,
                             G_TYPE_STRING);

        // Then the constructor must be added to the window object.
        jsc_context_set_value (js_context,
                               "Csound",
                               csound_constructor);
    }

    /**
    * Loads this extension when the page is created.
    */
    void SILENCE_PUBLIC
    webkit_web_extension_initialize_with_user_data(WebKitWebExtension *extension,
            GVariant *user_data)
    {
        g_variant_ref_sink(user_data);
        std::printf("webkit_web_extension_initialize_with_user_data: %p user_data: %s (0x%lx)\n",
                    extension,
                    g_variant_print(user_data, TRUE),
                    g_variant_get_uint64(user_data));
        g_signal_connect (webkit_script_world_get_default (),
                          "window-object-cleared",
                          G_CALLBACK (window_object_cleared_callback),
                          user_data);
    }
};
