/**
 * \page csoundforandroid Csound for Android App
 * <p>
 * This Android app provides pretty much all of Csound, including an HTML/JavaScript interface
 * that runs HTML5 code in the <html> element of a csd file.
 * <p>
 * See CsoundAppActivity.java and csound_oboe.hpp for more information.
 */
package com.csounds.Csound6;

import android.Manifest;
import android.annotation.TargetApi;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.AssetManager;
import android.content.res.Configuration;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.media.AudioManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.preference.PreferenceManager;
import android.support.design.widget.TabLayout;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.support.v4.content.FileProvider;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.webkit.JavascriptInterface;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListAdapter;
import android.widget.ScrollView;
import android.widget.SeekBar;
import android.widget.TableLayout;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.ToggleButton;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import csnd6.CsoundCallbackWrapper;
import csnd6.CsoundOboe;

import static android.support.v4.app.ActivityCompat.OnRequestPermissionsResultCallback;


@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
@SuppressWarnings("unused")
public class CsoundAppActivity extends AppCompatActivity implements /* CsoundObjListener,
        CsoundObj.MessagePoster, */ TabLayout.OnTabSelectedListener,
        SharedPreferences.OnSharedPreferenceChangeListener,
        OnRequestPermissionsResultCallback {
    Uri templateUri = null;
    Button newButton = null;
    Button openButton = null;
    Button saveAsButton = null;
    Button editButton = null;
    ToggleButton startStopButton = null;
    MenuItem helpItem = null;
    MenuItem aboutItem = null;
    CsoundOboe csound_oboe = null;
    ///File csound_file = null;
    Uri csound_uri = null;
    Button pad = null;
    ArrayList<SeekBar> sliders = new ArrayList<SeekBar>();
    ArrayList<Button> buttons = new ArrayList<Button>();
    ArrayList<String> str = new ArrayList<String>();
    private Boolean firstLvl = true;
    private File path = new File(Environment.getExternalStorageDirectory() + "");
    private String chosenFile;
    private static final int BROWSE_DIALOG = 0xFFFFFFFF;
    private static final int ERROR_DIALOG = 0xFFFFFFF0;
    ListAdapter adapter = null;
    String errorMessage = null;
    String csdTemplate = null;
    String html5Page = null;
    URL baseUrl = null;
    PackageInfo packageInfo = null;
    // Csound environment variables managed on Android.
    static String OPCODE6DIR = null;
    static String SFDIR = null;
    static String SSDIR = null;
    static String SADIR = null;
    static String INCDIR = null;
    private String screenLayout = "2";
    protected CsoundCallbackWrapper oboe_callback_wrapper = null;
    Intent csound_service_intent = null;
    static final int OPEN_FILE_REQUEST = 8391;
    static final int SAVE_FILE_REQUEST = 8392;
    static final int NEW_FILE_REQUEST = 8393;
    private boolean is_running = false;
    protected Handler handler = new Handler();
    private TabLayout tabs = null;
    private EditText editor_tab = null;
    private TextView messageTextView = null;
    private WebView html_tab = null;
    private TableLayout widgets_tab = null;
    private ScrollView messages_tab = null;
    private WebView help_tab = null;
    private WebView portal_tab = null;
    private MenuItem renderItem = null;

    static {
        int result = 0;
        try {
            java.lang.System.loadLibrary("sndfile");
        } catch (Throwable e) {
            Log.e("Csound: ", "sndfile native code library failed to load.\n");
            Log.e("Csound: ", e.toString());
        }
        try {
            java.lang.System.loadLibrary("csoundandroid");
        } catch (Throwable e) {
            Log.e("Csound: ", "Csound: csoundandroid native code library failed to load.\n"
                    + e);
            Log.e("Csound: ", e.toString());
        }
        try {
            result = csnd6.csnd
                    .csoundInitialize(csnd6.csnd.CSOUNDINIT_NO_ATEXIT);
        } catch (Throwable e) {
            Log.e("Csound: ", "csoundInitialize failed.\n"
                    + e);
            Log.e("Csound: ", e.toString());
            // java.lang.System.exit(1);
        }
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    private static void copyFile(InputStream in, OutputStream out) throws IOException {
        byte[] buffer = new byte[1024];
        int read;
        while ((read = in.read(buffer)) != -1) {
            out.write(buffer, 0, read);
        }
    }

    public boolean isExternalStorageWritable() {
        String state = Environment.getExternalStorageState();
        return Environment.MEDIA_MOUNTED.equals(state);
    }


    // Copy the file indicated by fromAssetPath to
    // the public external storage music directory.
    // The asset directory becomes a subdirectory of the music directory.
    private File copyAsset(String fromAssetPath) {
        if (checkDangerousPermissions() == false) {
            return null;
        }
        AssetManager assetManager = getAssets();
        InputStream in = null;
        OutputStream out = null;
        File outputFile = null;
        boolean result;
        try {
            File externalStoragePublicDirectory = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_MUSIC);
            outputFile = new File(externalStoragePublicDirectory, fromAssetPath);
            //result = outputFile.mkdirs();
            String filename = outputFile.getName();
            File directory = outputFile.getParentFile();
            result = directory.mkdirs();
            outputFile.createNewFile();
            in = assetManager.open(fromAssetPath);
            out = new FileOutputStream(outputFile);
            copyFile(in, out);
            in.close();
            in = null;
            out.flush();
            out.close();
            out = null;
            return outputFile;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setEnv(String variable, String value) {
        try {
            Class<?> libcore = Class.forName("libcore.io.Libcore");
            if (libcore == null) {
                Log.w("Csound:", "Cannot find libcore.os.Libcore;");
                //                return;
            } else {
                final Object os = Class.forName("libcore.io.Libcore").getField("os").get(null);
                Method method = os.getClass().getMethod("setenv", String.class, String.class, boolean.class);
                method.invoke(os, variable, value, true);
            }
        } catch (Exception e) {
            Log.w("Csound:", Log.getStackTraceString(e));
            Log.w("Csound:", e.getMessage());
        }
    }

    private void copyRawwaves() {
        try {
            String[] files = getAssets().list("rawwaves");
            for (int i = 0; i < files.length; i++) {
                String file = files[i];
                copyAsset("rawwaves/" + file);
            }
            File externalStoragePublicDirectory = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_MUSIC);
            String rawwavePath = externalStoragePublicDirectory.toString() + "/rawwaves/";
            setEnv("RAWWAVE_PATH", rawwavePath);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.menu, menu);
        renderItem = menu.findItem(R.id.action_render);
        return true;
    }

    public void startRendering() {
        messageTextView.setText("");
        File file = new File(OPCODE6DIR);
        File[] files = file.listFiles();
        CsoundAppActivity.this
                .postMessage("Loading Csound plugins:\n");
        for (int i = 0; i < files.length; i++) {
            String pluginPath = files[i].getAbsoluteFile()
                    .toString();
            try {
                CsoundAppActivity.this.postMessage(pluginPath
                        + "\n");
                System.load(pluginPath);
            } catch (Throwable e) {
                CsoundAppActivity.this.postMessage(e.toString()
                        + "\n");
            }
        }
        // This must be set before the Csound object is created.
        csnd6.csndJNI.csoundSetGlobalEnv("OPCODE6DIR", OPCODE6DIR);
        csnd6.csndJNI.csoundSetGlobalEnv("SFDIR", SFDIR);
        csnd6.csndJNI.csoundSetGlobalEnv("SSDIR", SSDIR);
        csnd6.csndJNI.csoundSetGlobalEnv("SADIR", SADIR);
        csnd6.csndJNI.csoundSetGlobalEnv("INCDIR", INCDIR);
        String driver = PreferenceManager
                .getDefaultSharedPreferences(this).getString("audioDriver", "");
        csound_oboe = new csnd6.CsoundOboe();
        oboe_callback_wrapper = new CsoundCallbackWrapper(csound_oboe.getCsound()) {
            @Override
            public void MessageCallback(int attr, String msg) {
                Log.d("CsoundOboe:", msg);
                postMessage(msg);
            }
        };
        oboe_callback_wrapper.SetMessageCallback();
        html_tab.addJavascriptInterface(csound_oboe, "csound");
        html_tab.addJavascriptInterface(CsoundAppActivity.this, "csoundApp");
        // Csound will not be in scope of any JavaScript on the page
        // until the page is reloaded. Also, we want to show any edits
        // to the page.
        loadWebView();
        postMessageClear("Csound is starting...\n");
        // Make sure this stuff really got packaged.
        String samples[] = null;
        try {
            samples = getAssets().list("samples");
        } catch (IOException e) {
            e.printStackTrace();
        }
        if (csound_uri.toString().toLowerCase().endsWith(".csd")) {
            int result = 0;
            String code = editor_tab.getText().toString();
            String filepath = uriToFilepath(csound_uri);
            result = csound_oboe.compileCsdText(code);
            result = csound_oboe.start();
            result = csound_oboe.performAndReset();
        }
        // Make sure this is still set after starting.
        String getOPCODE6DIR = csnd6.csndJNI.csoundGetEnv(0,
                "OPCODE6DIR");
        csound_oboe.message(
                "OPCODE6DIR has been set to: " + getOPCODE6DIR
                        + "\n");
    }

    public void stopRendering() {
        csound_oboe.stop();
        postMessage("Csound has been stopped.\n");
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        File outFile = null;
        switch (item.getItemId()) {
            case R.id.action_new:
                Intent.
                Intent new_intent = new Intent(Intent.ACTION_CREATE_DOCUMENT);
                new_intent.setType("*/*");
                new_intent.addCategory(Intent.CATEGORY_OPENABLE);
                Intent new_chooser = Intent.createChooser(new_intent, "Name");
                startActivityForResult(new_chooser, NEW_FILE_REQUEST);
                return true;
            case R.id.action_open:
                Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
                intent.setType("*/*");
                intent.addCategory(Intent.CATEGORY_OPENABLE);
                Intent chooser = Intent.createChooser(intent, "Open file");
                startActivityForResult(chooser, OPEN_FILE_REQUEST);
                return true;
            case R.id.action_save:
                String text = editor_tab.getText().toString();
                try {
                    saveTextToUri(text, csound_uri);
                } catch (Exception e) {

                }
                return true;
            case R.id.action_save_as:
                Intent save_as_intent = new Intent(Intent.ACTION_CREATE_DOCUMENT);
                save_as_intent.setType("*/*");
                save_as_intent.addCategory(Intent.CATEGORY_OPENABLE);
                Intent save_as_chooser = Intent.createChooser(save_as_intent, "Save as");
                startActivityForResult(save_as_chooser, SAVE_FILE_REQUEST);
            return true;
            // TODO: Automatically save before rendering.
            case R.id.action_render:
                synchronized (this) {
                    if (is_running == false) {
                        is_running = true;
                        renderItem.setTitle("Stop");
                        startRendering();
                    } else {
                        is_running = false;
                        stopRendering();
                        renderItem.setTitle("Run");
                    }
                }
                return true;
            case R.id.itemGuide:
                File user_guide = copyAsset("Csound6_User_Guide.pdf");
                Uri uri = FileProvider.getUriForFile(getApplicationContext(), BuildConfig.APPLICATION_ID, user_guide);
                Intent guide_intent = new Intent(Intent.ACTION_VIEW, uri);
                guide_intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                guide_intent.setDataAndType(uri, "application/pdf");
                startActivity(guide_intent);
                return true;
            case R.id.itemPrivacy:
                goToUrl("http://csound.github.io/csound_for_android_privacy.html");
                return true;
            case R.id.itemSettings:
                Intent intent1 = new Intent(this, SettingsActivity.class);
                startActivity(intent1);
                return true;
            case R.id.itemTrapped: {
                outFile = copyAsset("Csound6AndroidExamples/Boulanger/trapped.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
            }
            return true;
            case R.id.itemDroneIV: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/Drone-IV.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
            }
            return true;
            case R.id.itemPartikkel: {
                outFile = copyAsset("Csound6AndroidExamples/Khosravi/partikkel.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
            }
            return true;
            case R.id.itemXanadu: {
                outFile = copyAsset("Csound6AndroidExamples/Kung/xanadu.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
                return true;
            }
            case R.id.itemModulatedDelay: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/ModulateInput.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
                return true;
            }
            case R.id.itemBuiltinChannels: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/BuiltinChannels.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
                return true;
            }
            case R.id.itemMessage: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/silencio/js/jquery.js");
                if (outFile == null) {
                    return true;
                }
                outFile = copyAsset("Csound6AndroidExamples/Gogins/Message.html");
                if (outFile != null) {
                    LoadFile(outFile);
                }
                return true;
            }
            case R.id.itemKoanI: {
                outFile = copyAsset("Csound6AndroidExamples/McCurdy/i.csd");
                if (outFile != null) {
                    LoadFile(outFile);
                }
                return true;
            }
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @JavascriptInterface
    public void postMessage(String message_) {
        postMessageClear_(message_, false);
    }

    @JavascriptInterface
    public void postMessageClear(String message_) {
        postMessageClear_(message_, true);
    }

    private StringBuilder csoundMessageStringBuilder = new StringBuilder();

    private synchronized void postMessageClear_(String message_,
                                                boolean doClear_) {
        final String message = message_;
        final boolean doClear = doClear_;
        CsoundAppActivity.this.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if (doClear == true) {
                    messageTextView.setText("");
                }
                if (message == null) {
                    return;
                }
                messageTextView.append(message);
                messages_tab.fullScroll(ScrollView.FOCUS_DOWN);
                // Send Csound messages to the WebView's console.log function.
                // This should happen when and only when a newline appears.
                for (int i = 0, n = message.length(); i < n; i++) {
                    char c = message.charAt(i);
                    if (c == '\n') {
                        String line = csoundMessageStringBuilder.toString();
                        String code = String.format("console.log(\"%s\\n\");", line);
                        html_tab.evaluateJavascript(code, null);
                        csoundMessageStringBuilder.setLength(0);
                    } else {
                        csoundMessageStringBuilder.append(c);
                    }
                }
                Log.i("Csound:", message);
            }
        });
    }

    private void goToUrl(String url) {
        Uri uriUrl = Uri.parse(url);
        Intent launchBrowser = new Intent(Intent.ACTION_VIEW, uriUrl);
        startActivity(launchBrowser);
    }

    private void displayLog() {
        try {
            Process process = Runtime.getRuntime().exec(
                    "logcat -dt 16 CsoundObj:D AndroidCsound:D *:S");
            BufferedReader bufferedReader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()));
            String line;
            postMessage("Csound system log:\n");
            while ((line = bufferedReader.readLine()) != null) {
                postMessage(line + "\n");
            }
        } catch (IOException e) {
        }
    }

    public void setTitle(String title) {
        String fullTitle = "Csound";
        if (title != null) {
            fullTitle = fullTitle + ": " + title;
        }
        super.setTitle(fullTitle);
    }

    /**
     * Loads text from a file, normally a source code file.
     *
     * @param file Filepath to the file.
     * @return Source code.
     */
    @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
    public String loadTextFile(File file) {
        String code = null;
        try {
            if (checkDangerousPermissions() == false) {
                return "";
            }
            FileReader in = new FileReader(file);
            StringBuilder contents = new StringBuilder();
            char[] buffer = new char[4096];
            int read = 0;
            do {
                contents.append(buffer, 0, read);
                read = in.read(buffer);
            } while (read >= 0);
            in.close();
            code = contents.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return code;
    }

    /**
     * Saves text to a file, normally from the code editor.
     * @param code
     * @param file
     */
    @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
    public void saveTextFile(String code, File file) {
        try {
            if (checkDangerousPermissions() == false) {
                return;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    // TODO: This works if the .html file is loaded from the filesystem, but not if it is loaded
    // as an example.
    @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
    protected void loadWebView() {
        try {
            String code = editor_tab.getText().toString();
            int start = code.indexOf("<html");
            int end = code.indexOf("</html>") + 7;
            if (!(start == -1 || end == -1)) {
                String html5_page = code.substring(start, end);
                if (html5_page.length() > 1) {
                    html_tab.setLayerType(View.LAYER_TYPE_NONE, null);
                    WebSettings settings = html_tab.getSettings();
                    // Page itself must specify utf-8 in meta tag?
                    settings.setDefaultTextEncodingName("utf-8");
                    settings.setDomStorageEnabled(true);
                    settings.setDatabaseEnabled(true);
                    settings.setBuiltInZoomControls(true);
                    settings.setDisplayZoomControls(false);
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
                        settings.setAllowFileAccessFromFileURLs(true);
                        settings.setAllowUniversalAccessFromFileURLs(true);
                    }
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
                        settings.setJavaScriptEnabled(true);
                    }
                    String filepath = uriToFilepath(csound_uri);
                    File file = new File(filepath);
                    baseUrl = file.getParentFile().toURI().toURL();
                    String baseUrlString = baseUrl.toString();
                    baseUrlString = baseUrlString.replace("file:/root/", "file:/");
                    String baseUriPath = csound_uri.getPath();
                    html_tab.addJavascriptInterface(csound_oboe, "csound");
                    html_tab.addJavascriptInterface(CsoundAppActivity.this, "csoundApp");
                    Log.d("Csound:", "csound_uri.toString(): " + csound_uri.toString());
                    Log.d("Csound:", "csound_uri.getPath(): " + csound_uri.getPath());
                    Log.d("Csound:", "baseUrlString: " + baseUrlString);
                    if (baseUrlString.endsWith("/") == false) {
                        baseUrlString = baseUrlString + "/";
                    }
                    html_tab.loadDataWithBaseURL(baseUrlString,
                            html5_page, "text/html", "utf-8", null);
                }
            } else {
                html_tab.onPause();
                html_tab.pauseTimers();
            }
            View mainLayout = findViewById(R.id.mainLayout);
            mainLayout.invalidate();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void LoadFile(File file) {
        Log.d("Csound:", file.getAbsolutePath());
        ///csound_file = file;
        String code = loadTextFile(file);
        editor_tab.setText(code);
        setTitle(file.getName());
        csound_uri = FileProvider.getUriForFile(getApplicationContext(), BuildConfig.APPLICATION_ID, file);
        loadWebView();
    }

    @Override
    protected void onStop() {
        super.onStop();
        Log.d("Csound:", "onStop...");
    }

    @Override
    protected void onDestroy() {
        Log.d("Csound:", "onDestroy...");
        super.onDestroy();
        final boolean b = stopService(csound_service_intent);
        try {
            synchronized (this) {
                if (csound_oboe != null) {
                    csound_oboe.stop();
                }
            }
        } catch (Exception e) {
            Log.e("Csound:", "Could not stop csound_oboe: \n" + e.toString());
        }
        finishAndRemoveTask();
    }

    public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        setSupportActionBar((Toolbar) findViewById(R.id.toolbar));
        Log.d("Csound:", "onCreate...");
        // Start a service that will keep this activity's process running in the background.
        csound_service_intent = new Intent(this, CsoundService.class);
        ComponentName componentName = startService(csound_service_intent);
        // We ask for the data directory in case Android changes on
        // us without warning.
        try {
            packageInfo = getPackageManager().getPackageInfo(getPackageName(),
                    0);
        } catch (NameNotFoundException e) {
            e.printStackTrace();
        }
        final AudioManager audioManager = (AudioManager) getSystemService(AUDIO_SERVICE);
        // Bug since not turned back on: audioManager.setRingerMode(AudioManager.RINGER_MODE_SILENT);
        PreferenceManager.setDefaultValues(this, R.xml.settings, false);
        OPCODE6DIR = getBaseContext().getApplicationInfo().nativeLibraryDir;
        final SharedPreferences sharedPreferences = PreferenceManager
                .getDefaultSharedPreferences(this);
        sharedPreferences.registerOnSharedPreferenceChangeListener(this);
        OPCODE6DIR = sharedPreferences.getString("OPCODE6DIR", OPCODE6DIR);
        SSDIR = packageInfo.applicationInfo.dataDir + "/samples";
        SSDIR = sharedPreferences.getString("SSDIR", SSDIR);
        SFDIR = sharedPreferences.getString("SFDIR", SFDIR);
        SADIR = sharedPreferences.getString("SADIR", SADIR);
        INCDIR = sharedPreferences.getString("INCDIR", INCDIR);
        String driver = sharedPreferences.getString("audioDriver", "");
        // Pre-load plugin opcodes, not only to ensure that Csound
        // can load them, but for easier debugging if they fail to load.
        File file = new File(OPCODE6DIR);
        File[] files = file.listFiles();
        for (int i = 0; i < files.length; i++) {
            String pluginPath = files[i].getAbsoluteFile().toString();
            try {
                System.load(pluginPath);
            } catch (Throwable e) {
                postMessage(e.toString() + "\n");
            }
        }
        copyRawwaves();
        csdTemplate = "<CsoundSynthesizer>\n" + "<CsLicense>\n"
                + "</CsLicense>\n" + "<CsOptions>\n" + "</CsOptions>\n"
                + "<CsInstruments>\n" + "</CsInstruments>\n" + "<CsScore>\n"
                + "</CsScore>\n" + "</CsoundSynthesizer>\n";
        tabs = findViewById(R.id.tab_layout);
        tabs.addOnTabSelectedListener(this);
        widgets_tab = findViewById(R.id.widgets_tab);
        widgets_tab.setVisibility(View.GONE);
        html_tab = findViewById(R.id.html_tab);
        editor_tab = findViewById(R.id.editor);
        editor_tab.setVisibility(View.VISIBLE);
/*
        editor_tab.setOnEditorActionListener(new TextView.OnEditorActionListener() {
            @Override
            public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
                if (actionId == EditorInfo.IME_ACTION_SEARCH) {
                    ///performSearch();
                    return true;
                }
                return false;
            }
        });
*/
        html_tab = findViewById(R.id.html_tab);
        html_tab.setVisibility(View.GONE);
        messages_tab = findViewById((R.id.messages_tab));
        messages_tab.setVisibility(View.GONE);
        help_tab = findViewById(R.id.help_tab);
        help_tab.setVisibility(View.GONE);
        portal_tab = findViewById(R.id.portal_tab);
        portal_tab.setVisibility(View.GONE);
        messageTextView = findViewById(R.id.messageTextView);
        sliders.add((SeekBar) findViewById(R.id.seekBar1));
        sliders.add((SeekBar) findViewById(R.id.seekBar2));
        sliders.add((SeekBar) findViewById(R.id.seekBar3));
        sliders.add((SeekBar) findViewById(R.id.seekBar4));
        sliders.add((SeekBar) findViewById(R.id.seekBar5));
        buttons.add((Button) findViewById(R.id.button1));
        buttons.add((Button) findViewById(R.id.button2));
        buttons.add((Button) findViewById(R.id.button3));
        buttons.add((Button) findViewById(R.id.button4));
        buttons.add((Button) findViewById(R.id.button5));
        pad = findViewById(R.id.pad);
        html_tab.getSettings().setJavaScriptEnabled(true);
        html_tab.getSettings().setBuiltInZoomControls(true);
        html_tab.getSettings().setDisplayZoomControls(false);
        html_tab.setWebViewClient(new CsoundWebViewClient());
        help_tab.getSettings().setJavaScriptEnabled(true);
        help_tab.getSettings().setBuiltInZoomControls(true);
        help_tab.getSettings().setDisplayZoomControls(false);
        help_tab.setWebViewClient(new CsoundWebViewClient());
        help_tab.loadUrl("https://csound.com/docs/manual/indexframes.html");
        portal_tab.getSettings().setJavaScriptEnabled(true);
        portal_tab.getSettings().setBuiltInZoomControls(true);
        portal_tab.getSettings().setDisplayZoomControls(false);
        portal_tab.setWebViewClient(new CsoundWebViewClient());
        portal_tab.loadUrl("https://csound.com/");
        // Add slider handlers.
        for (int i = 0; i < 5; i++) {
            SeekBar seekBar = sliders.get(i);
            final String channelName = "slider" + (i + 1);
            seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
                public void onStopTrackingTouch(SeekBar seekBar) {
                    // TODO Auto-generated method stub
                }

                public void onStartTrackingTouch(SeekBar seekBar) {
                    // TODO Auto-generated method stub
                }

                public void onProgressChanged(SeekBar seekBar, int progress,
                                              boolean fromUser) {
                    if (fromUser) {
                        double value = progress / (double) seekBar.getMax();
                        synchronized (this) {
                            if (csound_oboe != null) {
                                csound_oboe.setChannel(channelName, value);
                            }
                        }
                    }
                }
            });
        }
        // Add button handlers.
        for (int i = 0; i < 5; i++) {
            Button button = buttons.get(i);
            final String channelName = "butt" + (i + 1);
            button.setOnClickListener(new OnClickListener() {
                public void onClick(View v) {
                    synchronized (this) {
                        if (csound_oboe != null) {
                            csound_oboe.setChannel(channelName, 1.0);
                        }
                    }
                }
            });
        }
        // Add trackpad handler.
        pad.setOnTouchListener(new View.OnTouchListener() {
            public boolean onTouch(View v, MotionEvent event) {
                int action = event.getAction() & MotionEvent.ACTION_MASK;
                double xpos = 0;
                double ypos = 0;
                boolean selected = false;
                switch (action) {
                    case MotionEvent.ACTION_DOWN:
                    case MotionEvent.ACTION_POINTER_DOWN:
                        pad.setPressed(true);
                        selected = true;
                        break;
                    case MotionEvent.ACTION_POINTER_UP:
                    case MotionEvent.ACTION_UP:
                        selected = false;
                        pad.setPressed(false);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        break;
                }
                if (selected == true) {
                    xpos = event.getX() / v.getWidth();
                    ypos = 1. - (event.getY() / v.getHeight());
                }
                synchronized (this) {
                    if (csound_oboe != null) {
                        csound_oboe.setChannel("trackpad.x", xpos);
                        csound_oboe.setChannel("trackpad.y", ypos);
                    }
                }
                return true;
            }
        });
        // Add motion handler.
        SensorManager sensorManager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);
        List<Sensor> sensors = sensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER);
        if (sensors.size() > 0) {
            Sensor sensor = sensors.get(0);
            SensorEventListener motionListener = new SensorEventListener() {
                public void onAccuracyChanged(Sensor sensor, int accuracy) {
                    // Not used.
                }

                public void onSensorChanged(SensorEvent event) {
                    double accelerometerX = event.values[0];
                    double accelerometerY = event.values[1];
                    double accelerometerZ = event.values[2];
                    synchronized (this) {
                        if (csound_oboe != null) {
                            csound_oboe.setChannel("accelerometerX", accelerometerX);
                            csound_oboe.setChannel("accelerometerY", accelerometerY);
                            csound_oboe.setChannel("accelerometerZ", accelerometerZ);
                        }
                    }
                }
            };
            int microseconds = 1000000 / 20;
            sensorManager.registerListener(motionListener, sensor, microseconds);
        }
    }

    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        if (hasFocus) {
            hideSystemUI();
        }
    }

    private String uriToFilename(android.net.Uri uri) {
        String result = uri.getPath();
        int cut = result.lastIndexOf('/');
        if (cut != -1) {
            result = result.substring(cut + 1);
        }
        return result;
    }

    private String uriToFilepath(android.net.Uri uri) {
        String absolute_filepath = null;
        // We must distinguish between content (external) Uris and
        // internal (root) Uris.
        try {
            String uri_string = uri.toString();
            String uri_path = uri.getPath();
            final String[] split = uri_string.split(":");
            final String[] pathpart = uri_path.split(":");
            String filepath = "";
            if (pathpart.length > 1) {
                filepath = Environment.getExternalStorageDirectory() + "/" + pathpart[1];
            } else {
                filepath = uri_path;
            }
            File file = new File(filepath);
            absolute_filepath = file.getAbsolutePath();
            return absolute_filepath;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return absolute_filepath;
    }

    private void hideSystemUI() {
        // Enables regular immersive mode.
        // For "lean back" mode, remove SYSTEM_UI_FLAG_IMMERSIVE.
        // Or for "sticky immersive," replace it with SYSTEM_UI_FLAG_IMMERSIVE_STICKY
        View decorView = getWindow().getDecorView();
        decorView.setSystemUiVisibility(
                View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                        // Set the content to appear under the system bars so that the
                        // content doesn't resize when the system bars hide and show.
                        | View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                        // Hide the nav bar and status bar
                        | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_FULLSCREEN);
    }

    // Shows the system bars by removing all the flags
    // except for the ones that make the content appear under the system bars.
    private void showSystemUI() {
        View decorView = getWindow().getDecorView();
        decorView.setSystemUiVisibility(
                View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                        | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                        | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
    }

    static final int MY_REQUEST_DANGEROUS_PERMISSIONS = 2149;

    protected boolean checkDangerousPermissions() {
        if (ContextCompat.checkSelfPermission(getApplicationContext(),
                Manifest.permission.WRITE_EXTERNAL_STORAGE)
                != PackageManager.PERMISSION_GRANTED ||
                ContextCompat.checkSelfPermission(getApplicationContext(),
                        Manifest.permission.RECORD_AUDIO)
                        != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(CsoundAppActivity.this,
                    new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE,
                            Manifest.permission.RECORD_AUDIO},
                    MY_REQUEST_DANGEROUS_PERMISSIONS);
            return false;
        } else {
            return true;
        }
    }

    public void saveTextToUri(String text, Uri uri) throws java.io.FileNotFoundException,
            java.io.IOException,
            java.lang.NullPointerException {
        OutputStream outputStream = getContentResolver().openOutputStream(uri);
        outputStream.write(text.getBytes());
        outputStream.close();
    }

    public String loadTextFromUri(Uri uri) throws java.io.FileNotFoundException, java.io.IOException {
        InputStream inputStream = getContentResolver().openInputStream(uri);
        InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
        BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
        StringBuilder stringBuilder = new StringBuilder();
        for (String line; (line = bufferedReader.readLine()) != null; ) {
            stringBuilder.append(line).append('\n');
        }
        String text = stringBuilder.toString();
        return text;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode,
                                    Intent intent) {
        try {
            if (requestCode == NEW_FILE_REQUEST && intent != null) {
                csound_uri = intent.getData();
                editor_tab.setText(csdTemplate);
                saveTextToUri(csdTemplate, csound_uri);
                String title = uriToFilename(csound_uri);
                setTitle(title);
            }
            if (requestCode == SAVE_FILE_REQUEST && intent != null) {
                if (checkDangerousPermissions() == false) {
                    return;
                }
                csound_uri = intent.getData();
                String text = editor_tab.getText().toString();
                saveTextToUri(text, csound_uri);
                String title = uriToFilename(csound_uri);
                setTitle(title);
            }
            if (requestCode == OPEN_FILE_REQUEST && intent != null) {
                if (checkDangerousPermissions() == false) {
                    return;
                }
                csound_uri = intent.getData();
                String text = loadTextFromUri(csound_uri);
                editor_tab.setText(text);
                loadWebView();
                String title = uriToFilename(csound_uri);
                setTitle(title);
             }
        } catch (Exception e) {
            Log.e("Csound:", e.toString());
        }
    }

    @JavascriptInterface
    public void setControlChannel(String channelName, double value) {
        csound_oboe.setChannel(channelName, value);
    }

    @JavascriptInterface
    public double getControlChannel(String channelName) {
        double value = 0;
        value = csound_oboe.getChannel(channelName);
        return value;
    }

    @Override
    public void onTabSelected(TabLayout.Tab tab) {
        String text = tab.getText().toString();
        if (text.contentEquals("Editor")) {
            editor_tab.setVisibility(View.VISIBLE);
        } else if (text.contentEquals("Widgets")) {
            widgets_tab.setVisibility(View.VISIBLE);
        } else if (text.contentEquals("Messages")) {
            messages_tab.setVisibility(View.VISIBLE);
        } else if (text.contentEquals("HTML")) {
            html_tab.setVisibility(View.VISIBLE);
        } else if (text.contentEquals("Help")) {
            help_tab.setVisibility(View.VISIBLE);
        } else if (text.contentEquals("About")) {
            portal_tab.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onTabUnselected(TabLayout.Tab tab) {
        String text = tab.getText().toString();
        if (text.contentEquals("Editor")) {
            editor_tab.setVisibility(View.GONE);
        } else if (text.contentEquals("Widgets")) {
            widgets_tab.setVisibility(View.GONE);
        } else if (text.contentEquals("Messages")) {
            messages_tab.setVisibility(View.GONE);
        } else if (text.contentEquals("HTML")) {
            html_tab.setVisibility(View.GONE);
        } else if (text.contentEquals("Help")) {
            help_tab.setVisibility(View.GONE);
        } else if (text.contentEquals("About")) {
            portal_tab.setVisibility(View.GONE);
        }
    }

    @Override
    public void onTabReselected(TabLayout.Tab tab) {

    }

    /**
     * Enable self-contained browsers to run JavaScript and load external URLs.
     */
    private class CsoundWebViewClient extends WebViewClient {
        @Override
        public boolean shouldOverrideUrlLoading(WebView view,
                                                WebResourceRequest request) {
            return false;
        }
        @Override
        public void onReceivedError(WebView view,
                                    WebResourceRequest request,
                                    WebResourceError error) {
            String originalUrl = view.getOriginalUrl();
            (new Throwable()).printStackTrace();
            Toast.makeText(CsoundAppActivity.this,
                    "WebView error for " + request.getUrl() + " from " + originalUrl + "! " + error.getDescription(), Toast.LENGTH_LONG)
                    .show();
        }
    }
}
