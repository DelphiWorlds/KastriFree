package com.delphiworlds.firemonkey;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.content.Context;
import android.os.Bundle;
import android.util.Log;

/*
  AppLoaderReceiver assists with starting your app after an update, or on boot. In order for the action handlers to start your app,
  you will need to add corresponding permission, receiver and metadata items in the manifest, e.g:

  <!-- **** Required for start on boot  **** -->
  <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />

  <application android:icon="@drawable/icon" android:label="@string/app_name">
    <activity android:name=".MainMenu" android:label="@string/app_name">
      <intent-filter>
        <action android:name="android.intent.action.MAIN" />
          <category android:name="android.intent.category.LAUNCHER" />
        </intent-filter>
    </activity>

    <!-- **** This is the metadata **** -->
    <meta-data android:name="apploader.restartAfterReplace" android:value="true" />
    <meta-data android:name="apploader.startOnBoot" android:value="true" />

    <!-- **** This is the BroadcastReceiver **** -->
    <receiver android:name="com.delphiworlds.firemonkey.DWAppLoaderReceiver">
      <intent-filter>
        <action android:name="android.intent.action.MY_PACKAGE_REPLACED"/>
        <action android:name="android.intent.action.BOOT_COMPLETED"/>
      </intent-filter>
    </receiver>
  </application>

*/

public class DWAppLoaderReceiver extends BroadcastReceiver {
  private static final String KEY_RESTART_AFTER_REPLACE = "apploader.restartAfterReplace";
  private static final String KEY_START_ON_BOOT = "apploader.startOnBoot";

  @Override
  public void onReceive(Context context, Intent intent) {
    // Retrieve the metadata
    Bundle metaData = getPackageManager().getApplicationInfo(activity.getPackageName(), PackageManager.GET_META_DATA).metaData;

    // This action will start the application if it has been updated, e.g. when "side-loaded".
    // Note: If your app is on Google Play, you will likely want to omit having the option in the metadata
    if (intent.getAction().equals(Intent.ACTION_MY_PACKAGE_REPLACED)) {
      if ((metadata != null) && metaData.containsKey(DWAppLoaderReceiver.KEY_RESTART_AFTER_REPLACE) {
        if (metaData.getBoolean(DWAppLoaderReceiver.KEY_RESTART_AFTER_REPLACE))
          startApp(context);
      }
    }

    // This action will start the application on bootup of the device
    // Note: This action also needs <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />  in the manifest    
    if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
      if ((metadata != null) && metaData.containsKey(DWAppLoaderReceiver.KEY_START_ON_BOOT) {
        if (metaData.getBoolean(DWAppLoaderReceiver.KEY_START_ON_BOOT))
          startApp(context);
      }
    }
  }

  private void startApp(context) {
    context.startActivity(context.getPackageManager().getLaunchIntentForPackage(context.getPackageName()));
  }
}
