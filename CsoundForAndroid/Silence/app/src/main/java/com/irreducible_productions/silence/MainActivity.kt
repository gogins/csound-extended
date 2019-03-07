package com.irreducible_productions.silence

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.support.v7.widget.Toolbar
import kotlinx.android.synthetic.main.activity_main.*
import android.R.layout
import android.R.menu
import android.support.design.widget.TabLayout
import android.view.KeyEvent
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.webkit.WebResourceRequest
import android.webkit.WebView
import android.webkit.WebViewClient
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
            "Editor" -> editor.visibility = View.GONE
            "Messages" -> messages.visibility = View.GONE
            "HTML" -> html_view.visibility = View.GONE
            "Help" -> help_view.visibility = View.GONE
            "Portal" -> portal_view.visibility = View.GONE
        }
    }
    override fun onTabSelected(tab: TabLayout.Tab?) {
        when (tab?.text) {
            "Editor" -> editor.visibility = View.VISIBLE
            "Messages" -> messages.visibility = View.VISIBLE
            "HTML" -> html_view.visibility = View.VISIBLE
            "Help" -> help_view.visibility = View.VISIBLE
            "Portal" -> portal_view.visibility = View.VISIBLE
        }
    }
    private inner class InnerWebViewClient : WebViewClient() {
        override fun shouldOverrideUrlLoading(view: WebView, url: WebResourceRequest): Boolean {
            return false
        }
     }
    override fun onKeyDown(keyCode: Int, event: KeyEvent): Boolean {
        //if (keyCode == KeyEvent.KEYCODE_BACK && webview.canGoBack()) {
        //    webview.goBack()
        //    return true
        //}
        return super.onKeyDown(keyCode, event)
    }
    // We do not use Fragments because state is not shared.
    // Instead we hide and show widgets to simulate tabs.
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        setSupportActionBar(findViewById(R.id.toolbar))
        tabs = findViewById<TabLayout>(R.id.tab_layout)
        tabs.addOnTabSelectedListener(this)
        editor = findViewById<EditText>(R.id.editor)
        editor.visibility = View.VISIBLE
        messages = findViewById<TextView>(R.id.messages)
        messages.visibility = View.GONE
        html_view = findViewById<WebView>(R.id.html_view)
        html_view.visibility = View.GONE
        help_view = findViewById<WebView>(R.id.help_view)
        help_view.visibility = View.GONE
        help_view.getSettings().javaScriptEnabled = true
        help_view.getSettings().setBuiltInZoomControls(true)
        help_view.getSettings().setDisplayZoomControls(false)
        help_view.webViewClient = InnerWebViewClient()
        help_view.loadUrl("https://csound.com/docs/manual/indexframes.html")
        portal_view = findViewById<WebView>(R.id.portal_view)
        portal_view.visibility = View.GONE
        portal_view.getSettings().javaScriptEnabled = true
        portal_view.getSettings().setBuiltInZoomControls(true)
        portal_view.getSettings().setDisplayZoomControls(false)
        portal_view.webViewClient = InnerWebViewClient()
        portal_view.loadUrl("https://csound.com/")
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
                return true
            }
            R.id.action_open -> {
                return true
            }
            R.id.action_save-> {
                return true
            }
            R.id.action_save_as-> {
                return true
            }
            R.id.action_render-> {
                return true
            }
            R.id.action_settings-> {
                return true
            }
            R.id.action_examples -> {
                return true
            }
            R.id.action_about -> {
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
            //System.loadLibrary("native-lib")
        }
    }
}
