/**
 * This module wraps important parts of the Csound API as a WebKit2GTK+ Web 
 * Process Extension. This in turn enables Gtk programs that embed the WebKit2 
 * browser to call Csound using JavaScript in Web pages run by WebKit2.
 */
#include <csound/csound_threaded.hpp>
#include 
 
static CsoundThreaded &csound() {
    static CsoundThreaded csound_;
    return csound_;
}

extern "C" {

static void 
window_object_cleared_callback (WebKitScriptWorld *world, 
                                WebKitWebPage     *web_page, 
                                WebKitFrame       *frame, 
                                gpointer           user_data)
{
    JSGlobalContextRef jsContext;
    JSObjectRef        globalObject;

    jsContext = webkit_frame_get_javascript_context_for_script_world (frame, world);
    globalObject = JSContextGetGlobalObject (jsContext);

    /* Use JSC API to add the JavaScript code you want */
}

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
    g_signal_connect (webkit_script_world_get_default (), 
                      "window-object-cleared", 
                      G_CALLBACK (window_object_cleared_callback), 
                      NULL);
}

}