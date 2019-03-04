package com.irreducible_productions.silence

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.support.v7.widget.Toolbar
import kotlinx.android.synthetic.main.activity_main.*
import android.R.layout
import android.R.menu
import android.support.design.widget.TabLayout
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.webkit.WebView
import android.widget.EditText
import android.widget.TextView


class MainActivity : AppCompatActivity(), TabLayout.OnTabSelectedListener {
    private lateinit var tabs: TabLayout
    private lateinit var editor: EditText
    private lateinit var html_view: WebView
    private lateinit var messages: TextView
    private lateinit var help_view: WebView
    private lateinit var portal_view: WebView

    override fun onTabReselected(tab: TabLayout.Tab?) {
    }
    override fun onTabUnselected(tab: TabLayout.Tab?) {
        when (tab?.text) {
            "Editor" -> editor?.visibility = View.INVISIBLE
        }
    }
    override fun onTabSelected(tab: TabLayout.Tab?) {
        when (tab?.text) {
            "Editor" -> editor?.visibility = View.VISIBLE
        }
    }
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        setSupportActionBar(findViewById(R.id.toolbar))
        tabs = findViewById<TabLayout>(R.id.tab_layout)
        tabs.addOnTabSelectedListener(this)
        editor = findViewById<EditText>(R.id.editor)
        messages = findViewById<TextView>(R.id.editor)


        // Example of a call to a native method
        sample_text.text = stringFromJNI()

    }
    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        // Inflate the menu; this adds items to the action bar if it is present.
        menuInflater.inflate(R.menu.menu, menu)
        return true
    }
    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        // Handle presses on the action bar menu items
        when (item.itemId) {
            R.id.action_new -> {
                sample_text.text = "New"
                return true
            }
            R.id.action_open -> {
                sample_text.text = "Open..."
                return true
            }
            R.id.action_save-> {
                sample_text.text = "Save"
                return true
            }
            R.id.action_save_as-> {
                sample_text.text = "Save as..."
                return true
            }
            R.id.action_render-> {
                sample_text.text = "Render"
                return true
            }
            R.id.action_settings-> {
                sample_text.text = "Settings..."
                return true
            }
            R.id.action_examples -> {
                sample_text.text = "Examples..."
                return true
            }
            R.id.action_about -> {
                sample_text.text = "About..."
                return true
            }
        }
        return super.onOptionsItemSelected(item)
    }
    /**
     * A native method that is implemented by the 'native-lib' native library,
     * which is packaged with this application.
     */
    external fun stringFromJNI(): String
    companion object {
        // Used to load the 'native-lib' library on application startup.
        init {
            System.loadLibrary("native-lib")
        }
    }
}
