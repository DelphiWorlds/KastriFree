package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *         DelphiWorlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

/*
  DWMultiBroadcastReceiver can assist with starting your app after an update, or on boot. In order for the action handlers to start your app,
  you will need to add the corresponding permission, receiver and metadata items in the manifest, e.g:

  <!-- **** Required for start on boot  **** -->
  <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />

  <!-- The application tag should already exist -->
  <application android:icon="@drawable/icon" android:label="@string/app_name">
    <activity android:name=".MainMenu" android:label="@string/app_name">
      <intent-filter>
        <action android:name="android.intent.action.MAIN" />
          <category android:name="android.intent.category.LAUNCHER" />
        </intent-filter>
    </activity>

    <!-- **** This is the metadata **** -->
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE" android:value="true" />
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_START_ON_BOOT" android:value="true" />

    <!-- **** This is the BroadcastReceiver **** -->
    <receiver android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver">
      <intent-filter>
        <action android:name="android.intent.action.MY_PACKAGE_REPLACED"/>
        <action android:name="android.intent.action.BOOT_COMPLETED"/>
      </intent-filter>
    </receiver>
  </application>
*/

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.support.v4.content.LocalBroadcastManager;
import android.os.Bundle;
import android.util.Log;

public class DWMultiBroadcastReceiver extends BroadcastReceiver {

  private static final String TAG = "DWMultiBroadcastReceiver";

  private static final String KEY_RESTART_AFTER_REPLACE = "DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE"; // true or false
  private static final String KEY_START_ON_BOOT = "DWMultiBroadcastReceiver.KEY_START_ON_BOOT"; // true or false
  private static final String KEY_START_SERVICE_ON_BOOT = "DWMultiBroadcastReceiver.KEY_START_SERVICE_ON_BOOT"; // string = service name

  public static final String ACTION_SERVICE_ALARM = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_SERVICE_ALARM";

  private boolean startApp(Context context) {
    context.startActivity(context.getPackageManager().getLaunchIntentForPackage(context.getPackageName()));
    return true;
  }

  private boolean checkStartupIntent(Context context, Intent intent) {
    // Retrieve the metadata
    Bundle metaData = null;
    try {
      metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
    } catch (PackageManager.NameNotFoundException  exception) {
      return false;
    }
    Boolean handled = false;

    // This action will start the application if it has been updated, e.g. when "side-loaded".
    // Note: If your app is on Google Play, you will likely want to omit having the option in the metadata
    if (intent.getAction().equals(Intent.ACTION_MY_PACKAGE_REPLACED)) {
      if ((metaData != null) && metaData.containsKey(DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE)) {
        if (metaData.getBoolean(DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE))
          return startApp(context);
      }
    }

    // This action will start the application or service on bootup of the device
    // Note: This action also needs <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />  in the manifest    
    if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
      if ((metaData != null) && metaData.containsKey(DWMultiBroadcastReceiver.KEY_START_ON_BOOT)) {
        if (metaData.getBoolean(DWMultiBroadcastReceiver.KEY_START_ON_BOOT))
          return startApp(context);
      }
      if ((metaData != null) && metaData.containsKey(DWMultiBroadcastReceiver.KEY_START_SERVICE_ON_BOOT)) {
        String serviceName = "com.embarcadero.services." + metaData.getString(DWMultiBroadcastReceiver.KEY_START_SERVICE_ON_BOOT);
        Log.v(TAG, "Attempting to start service from boot: " + serviceName);
        Intent serviceIntent = new Intent();
        serviceIntent.setClassName(context.getPackageName(), serviceName);
        context.startService(serviceIntent);
        return true;
      }
    }

    // Starting a service from an alarm. The intent should already have the class name set in the intent
    if (intent.getAction().equals(ACTION_SERVICE_ALARM)) {
      Log.v(TAG, "Attempting to start service from alarm");
      intent.setClassName(context.getPackageName(), intent.getStringExtra("ServiceName"));
      context.startService(intent);
      return true;
    }

    return false;
  }

  public void onReceive(Context context, Intent intent) {
    Log.v(TAG, "Received intent with action: " + intent.getAction());
    if (!checkStartupIntent(context, intent))
      // Simply forward on the intent in a local broadcast
      LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
  }

}