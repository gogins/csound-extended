/**
 * \page csoundforandroid Csound for Android App
 *
 * This Android app provides pretty much all of Csound, including an HTML/JavaScript interface
 * that runs HTML5 code in the <html> element of a csd file.
 *
 * See CsoundAppActivity.java and csound_oboe.hpp for more information.
 */
package com.csounds.Csound6;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import android.annotation.TargetApi;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.AssetManager;
import android.content.res.Configuration;
import android.database.Cursor;
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
import android.os.ParcelFileDescriptor;
import android.preference.PreferenceManager;
import android.provider.OpenableColumns;
import android.support.v4.content.FileProvider;
import android.support.v4.provider.DocumentFile;
import android.util.Log;
import android.view.ContextThemeWrapper;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ScrollView;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.ToggleButton;


import csnd6.Csound;
import csnd6.CsoundCallbackWrapper;
import csnd6.CsoundOboe;



@TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
@SuppressWarnings("unused")
public class CsoundAppActivity extends Activity implements /* CsoundObjListener,
        CsoundObj.MessagePoster, */ SharedPreferences.OnSharedPreferenceChangeListener {
    Uri templateUri = null;
    Button newButton = null;
    Button openButton = null;
    Button saveAsButton = null;
    Button editButton = null;
    ToggleButton startStopButton = null;
    MenuItem helpItem = null;
    MenuItem aboutItem = null;
    CsoundOboe csound_oboe = null;
    File csound_file = null;
    Button pad = null;
    WebView webView = null;
    LinearLayout channelsLayout = null;
    ArrayList<SeekBar> sliders = new ArrayList<SeekBar>();
    ArrayList<Button> buttons = new ArrayList<Button>();
    ArrayList<String> str = new ArrayList<String>();
    private Boolean firstLvl = true;
    private File path = new File(Environment.getExternalStorageDirectory() + "");
    private String chosenFile;
    private static final int BROWSE_DIALOG = 0xFFFFFFFF;
    private static final int ERROR_DIALOG = 0xFFFFFFF0;
    ListAdapter adapter = null;
    protected Handler handler = new Handler();
    private TextView messageTextView = null;
    private ScrollView messageScrollView = null;
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
    WebView webview = null;
    private String screenLayout = "2";
    protected CsoundCallbackWrapper oboe_callback_wrapper = null;
    Intent csound_service_intent = null;
    static final int PICK_FILE_REQUEST = 8391;

    static {
        int result = 0;
        try {
            java.lang.System.loadLibrary("sndfile");
        } catch (Throwable e) {
            Log.e("Csound: ","sndfile native code library failed to load.\n");
            Log.e("Csound: ", e.toString());
        }
        try {
            java.lang.System.loadLibrary("csoundandroid");
        } catch (Throwable e) {
            Log.e("Csound: ","Csound: csoundandroid native code library failed to load.\n"
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

    protected void writeTemplateFile() {
        File root = Environment.getExternalStorageDirectory();
        try {
            if (root.canWrite()) {
                FileWriter filewriter = new FileWriter(csound_file);
                BufferedWriter out = new BufferedWriter(filewriter);
                out.write(csdTemplate);
                out.close();
            }
        } catch (IOException e) {
            Log.e("Csound:", "Could not write file " + e.getMessage());
        }
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
        } catch ( Exception e) {
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
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        File outFile = null;
        switch (item.getItemId()) {
            case R.id.itemGuide:
                File user_guide = copyAsset("Csound6_User_Guide.pdf");
                Uri uri = FileProvider.getUriForFile(getApplicationContext(), BuildConfig.APPLICATION_ID, user_guide);
                //Uri uri = Uri.fromFile(user_guide);
                Intent intent = new Intent(Intent.ACTION_VIEW, uri);
                intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                intent.setDataAndType(uri, "application/pdf");
                startActivity(intent);
                return true;
            case R.id.itemHelp:
                goToUrl("http://csound.github.io/docs/manual/indexframes.html");
                return true;
            case R.id.itemAbout:
                goToUrl("http://csound.github.io/");
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
                if (outFile != null){
                    OnFileChosen(outFile);
                }
            }
            return true;
            case R.id.itemDroneIV: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/Drone-IV.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
            }
            return true;
            case R.id.itemLuaScoregen: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/lua_scoregen.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
            }
            return true;
            case R.id.itemPartikkel: {
                outFile = copyAsset("Csound6AndroidExamples/Khosravi/partikkel.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
            }
            return true;
            case R.id.itemXanadu: {
                outFile = copyAsset("Csound6AndroidExamples/Kung/xanadu.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
                return true;
            }
            case R.id.itemModulatedDelay: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/ModulateInput.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
                return true;
            }
            case R.id.itemBuiltinChannels: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/BuiltinChannels.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
                return true;
            }
            case R.id.itemMessage: {
                outFile = copyAsset("Csound6AndroidExamples/Gogins/silencio/js/jquery.js");
                if (outFile == null){
                    return true;
                }
                outFile = copyAsset("Csound6AndroidExamples/Gogins/Message.html");
                if (outFile != null){
                    OnFileChosen(outFile);
                }
                return true;
            }
            case R.id.itemKoanI: {
                outFile = copyAsset("Csound6AndroidExamples/McCurdy/i.csd");
                if (outFile != null){
                    OnFileChosen(outFile);
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
                messageScrollView.fullScroll(ScrollView.FOCUS_DOWN);
                // Send Csound messages to the WebView's console.log function.
                // This should happen when and only when a newline appears.
                for (int i = 0, n = message.length(); i < n; i++){
                    char c = message.charAt(i);
                    if (c == '\n') {
                        String line = csoundMessageStringBuilder.toString();
                        String code = String.format("console.log(\"%s\\n\");", line);
                        webView.evaluateJavascript(code, null);
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
     * Opens the CSD and searches for a <html> element.
     */
    @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
    protected void parseWebLayout() {
        try {
            FileReader in = new FileReader(csound_file);
            StringBuilder contents = new StringBuilder();
            char[] buffer = new char[4096];
            int read = 0;
            do {
                contents.append(buffer, 0, read);
                read = in.read(buffer);
            } while (read >= 0);
            in.close();
            String csdText = contents.toString();
            int start = csdText.indexOf("<html");
            int end = csdText.indexOf("</html>") + 7;
            if (!(start == -1 || end == -1)) {
                html5Page = csdText.substring(start, end);
                if (html5Page.length() > 1) {
                    webView.setLayerType(View.LAYER_TYPE_NONE, null);
                    WebSettings settings = webView.getSettings();
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
                    File basePath = csound_file.getParentFile();
                    baseUrl = basePath.toURI().toURL();
                    webView.loadDataWithBaseURL(baseUrl.toString(),
                            html5Page, "text/html", "utf-8", null);
                }
            } else {
                webView.onPause();
                webView.pauseTimers();
            }
            View mainLayout = findViewById(R.id.mainLayout);
            mainLayout.invalidate();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void setScreenLayout(SharedPreferences sharedPreferences) {
        screenLayout = sharedPreferences.getString("screenLayout", "1");
        if (screenLayout.equals("1")) {
            channelsLayout.setVisibility(View.GONE);
            webView.setVisibility(View.GONE);
            messageScrollView.setVisibility(View.VISIBLE);
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR);
        } else if (screenLayout.equals("2")) {
            channelsLayout.setVisibility(View.GONE);
            webView.setVisibility(View.VISIBLE);
            messageScrollView.setVisibility(View.GONE);
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR);
        } else if (screenLayout.equals("3")) {
            channelsLayout.setVisibility(View.GONE);
            webView.setVisibility(View.VISIBLE);
            messageScrollView.setVisibility(View.VISIBLE);
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR);
        } else if (screenLayout.equals("4")) {
            channelsLayout.setVisibility(View.VISIBLE);
            webView.setVisibility(View.GONE);
            messageScrollView.setVisibility(View.GONE);
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        } else if (screenLayout.equals("5")) {
            channelsLayout.setVisibility(View.VISIBLE);
            webView.setVisibility(View.GONE);
            messageScrollView.setVisibility(View.VISIBLE);
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        }
        View mainLayout = findViewById(R.id.mainLayout);
        mainLayout.invalidate();
    }

    public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
        if (key.equals(SettingsActivity.KEY_LIST_PREFERENCE)) {
            setScreenLayout(sharedPreferences);
        }
    }

    private void OnFileChosen(File file) {
        ///setContentView(R.layout.main);
        Log.d("Csound:", file.getAbsolutePath());
        csound_file = file;
        setTitle(csound_file.getName());
        parseWebLayout();
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
            if (csound_oboe != null) {
                csound_oboe.stop();
            }
        } catch (Exception e) {
            Log.e("Csound:", "Could not stop csound_oboe: \n" + e.toString());
        }
        finishAndRemoveTask();
     }

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d("Csound:", "onCreate...");
        // Start a service that will keep this activity's process running in the background.
        csound_service_intent = new Intent(this, com.csounds.Csound6.CsoundService.class);
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
        setContentView(R.layout.main);
        newButton = findViewById(R.id.newButton);
        newButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                AlertDialog.Builder alert = new AlertDialog.Builder(
                        CsoundAppActivity.this,
                         R.style.csoundAlertDialogStyle);
                alert.setTitle("New CSD...");
                alert.setMessage("Filename:");
                final EditText input = new EditText(CsoundAppActivity.this);
                alert.setView(input);
                alert.setPositiveButton("Ok",
                        new DialogInterface.OnClickListener() {
                            public void onClick(DialogInterface dialog,
                                                int whichButton) {
                                String value = input.getText().toString();
                                File root = Environment
                                        .getExternalStorageDirectory();
                                csound_file = new File(root, value);
                                writeTemplateFile();

                                Intent intent = new Intent(Intent.ACTION_VIEW);
                                Uri uri = FileProvider.getUriForFile(getApplicationContext(), BuildConfig.APPLICATION_ID, csound_file);
                                intent.setDataAndType(uri, "text/plain");
                                intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                                intent.addFlags(Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
                                startActivity(intent);
                            }
                        });
                alert.setNegativeButton("Cancel",
                        new DialogInterface.OnClickListener() {
                            public void onClick(DialogInterface dialog,
                                                int whichButton) {
                            }
                        });
                alert.show();
            }
        });
        openButton = findViewById(R.id.openButton);
        openButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
                intent.setType("*/*");
                intent.addCategory(Intent.CATEGORY_OPENABLE);
                Intent chooser = Intent.createChooser(intent, "Open file");
                startActivityForResult(chooser, PICK_FILE_REQUEST);
            }
        });
        saveAsButton = findViewById(R.id.saveAsButton);
        saveAsButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
            }
        });
        editButton = findViewById(R.id.editButton);
        editButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                try {
                    if (csound_file != null) {
                        Intent intent = new Intent(Intent.ACTION_VIEW);
                        Uri uri = FileProvider.getUriForFile(getApplicationContext(), BuildConfig.APPLICATION_ID, csound_file);
                        intent.setDataAndType(uri, "text/plain");
                        intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                        intent.addFlags(Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
                        startActivity(intent);
                    }
                } catch (Exception e) {
                    Log.d("Csound", e.getMessage());
                }
            }
        });
        channelsLayout = findViewById(R.id.channelsLayout);
        webView = findViewById(R.id.htmlView);
        webView.setWebViewClient(new WebViewClient() {
            public void onReceivedError(WebView view, int errorCode,
                                        String description, String failingUrl) {
                Toast.makeText(CsoundAppActivity.this,
                        "WebView error! " + description, Toast.LENGTH_SHORT)
                        .show();
            }
        });
        messageTextView = findViewById(R.id.messageTextView);
        messageScrollView = findViewById(R.id.csoundMessages);
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
        startStopButton = findViewById(R.id.runButton);
        startStopButton.setOnClickListener(new OnClickListener() {
            public synchronized void onClick(View v) {
                if (csound_file == null) {
                    startStopButton.toggle();
                    return;
                }
                if (startStopButton.isChecked()) {
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
                    String driver = sharedPreferences.getString("audioDriver", "");
                        csound_oboe = new csnd6.CsoundOboe();
                        oboe_callback_wrapper = new CsoundCallbackWrapper(csound_oboe.getCsound()) {
                            @Override
                            public void MessageCallback(int attr, String msg) {
                                Log.d("CsoundOboe:", msg);
                                postMessage(msg);
                            }
                        };
                        oboe_callback_wrapper.SetMessageCallback();
                        webView.addJavascriptInterface(csound_oboe, "csound");
                        webView.addJavascriptInterface(CsoundAppActivity.this, "csoundApp");
                    // Csound will not be in scope of any JavaScript on the page
                    // until the page is reloaded. Also, we want to show any edits
                    // to the page.
                    parseWebLayout();
                    postMessageClear("Csound is starting...\n");
                    // Make sure this stuff really got packaged.
                    String samples[] = null;
                    try {
                        samples = getAssets().list("samples");
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    if (screenLayout.equals("4") || screenLayout.equals("5")) {
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
                                        csound_oboe.setChannel(channelName, value);
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
                                    csound_oboe.setChannel(channelName, 1.0);
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
                            csound_oboe.setChannel("trackpad.x", xpos);
                            csound_oboe.setChannel("trackpad.y", ypos);
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
                                    csound_oboe.setChannel("accelerometerX", accelerometerX);
                                    csound_oboe.setChannel("accelerometerY", accelerometerY);
                                    csound_oboe.setChannel("accelerometerZ", accelerometerZ);
                                }
                            };
                            int microseconds = 1000000 / 20;
                            sensorManager.registerListener(motionListener, sensor, microseconds);
                        }
                    }
                    // If the Csound file is a CSD, start Csound;
                    // otherwise, do not start Csound, and assume the
                    // file is HTML with JavaScript that will call
                    // csound_obj.perform() as in csound.node().
                    if (csound_file.toString().toLowerCase().endsWith(".csd")) {
                        int result = 0;
                        result = csound_oboe.compileCsd(csound_file.getAbsolutePath());
                        result = csound_oboe.start();
                        result = csound_oboe.performAndReset();
                    }
                    // Make sure this is still set after starting.
                    String getOPCODE6DIR = csnd6.csndJNI.csoundGetEnv(0,
                            "OPCODE6DIR");
                        csound_oboe.message(
                                "OPCODE6DIR has been set to: " + getOPCODE6DIR
                                        + "\n");
                } else {
                    csound_oboe.stop();
                    postMessage("Csound has been stopped.\n");
                }
            }
        });
        setScreenLayout(sharedPreferences);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode,
                                    Intent intent) {
        try {
            if (requestCode == R.id.newButton && intent != null) {
                csound_file = new File(intent.getData().getPath());
                setTitle(csound_file.getName());
            }
            if (requestCode == PICK_FILE_REQUEST && intent != null) {
                String filepath = null;
                Uri uri = intent.getData();
                String uri_string = uri.toString();
                String uri_path = uri.getPath();
                Log.i("Csound", "Uri: " + uri_string);
                if (uri_string.startsWith("content://")) {
                    final String[] split = uri_string.split(":");
                    final String[] pathpart = uri_path.split(":");
                    filepath = Environment.getExternalStorageDirectory() + "/"+ pathpart[1];
                }
                csound_file = new File(filepath);
                csound_file.setReadable(true);
                csound_file.setWritable(true);
                setTitle(csound_file.getName());
                parseWebLayout();
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
}
