package com.csounds.Csound6;

import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceActivity;
import android.preference.ListPreference;

public class SettingsActivity extends PreferenceActivity implements SharedPreferences.OnSharedPreferenceChangeListener{
    public static final String KEY_AUDIO_DRIVER_PREFERENCE = "audioDriver";
    public ListPreference audioDriverPreference;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.settings);
        audioDriverPreference = (ListPreference)findPreference(KEY_AUDIO_DRIVER_PREFERENCE);

    }
    @Override
    protected void onResume() {
        super.onResume();
        // Setup the initial values
        // Set up a listener whenever a key changes
        getPreferenceScreen().getSharedPreferences().registerOnSharedPreferenceChangeListener(this);
    }

    @Override
    protected void onPause() {
        super.onPause();
        // Unregister the listener whenever a key changes
        getPreferenceScreen().getSharedPreferences().unregisterOnSharedPreferenceChangeListener(this);
    }

    public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
        if (key.equals(KEY_AUDIO_DRIVER_PREFERENCE)) {
            audioDriverPreference.setSummary("Selected driver is " + audioDriverPreference.getEntry().toString());
        }
    }
}
