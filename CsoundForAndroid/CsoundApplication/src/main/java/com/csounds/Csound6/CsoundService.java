package com.csounds.Csound6;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class CsoundService extends Service {
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        //TODO do something useful
        return Service.START_STICKY;
    }

    @Override
    public IBinder onBind(Intent intent) {
        //TODO for communication return IBinder implementation
        return null;
    }
}
