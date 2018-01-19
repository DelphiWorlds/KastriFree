//*******************************************************
//
//           CodeGear Delphi Runtime Library
// Copyright(c) 2014-2017 Embarcadero Technologies, Inc.
//              All rights reserved
//
//*******************************************************

package com.embarcadero.services;

import android.app.Service;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.IBinder;
import android.os.Binder;
import com.embarcadero.rtl.ProxyService;

public class <%ServiceName%> extends Service {
    private final String baseLibraryName = "<%ServiceName%>"; 
    private String libraryName;

    // Binder given to clients
    public final IBinder mBinder = new LocalBinder();

    /**
     * Class used for the client Binder.  Because we know this service always
     * runs in the same process as its clients, we don't need to deal with IPC.
     */
    public class LocalBinder extends Binder {
        public long getService() {
            // Return the instance of the Delphi LocalService so clients can call public methods
            return ProxyService.getService(this, libraryName);            
        }
    }

    @Override
    public void onCreate() {
        libraryName = getApplicationInfo().nativeLibraryDir + "/lib" + baseLibraryName + ".so";
        super.onCreate();
        ProxyService.onCreate(this, libraryName);
    }

    @Override
    public void onDestroy() {
        ProxyService.onDestroy(this, libraryName);
        super.onDestroy();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return ProxyService.onStartCommand(this, libraryName, intent, flags, startId);
    }

    @Override
    public IBinder onBind(Intent intent) {
        return ProxyService.onBind(this, libraryName, intent);
    }

    @Override
    public void onRebind(Intent intent) {
        ProxyService.onRebind(this, libraryName, intent);
    }

    @Override
    public boolean onUnbind(Intent intent) {
        return ProxyService.onUnbind(this, libraryName, intent);
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        ProxyService.onConfigurationChanged(this, libraryName, newConfig);
    }

    @Override
    public void onLowMemory() {
        ProxyService.onLowMemory(this, libraryName);
    }

    @Override
    public void onTrimMemory(int level) {
        ProxyService.onTrimMemory(this, libraryName, level);
    }

}

