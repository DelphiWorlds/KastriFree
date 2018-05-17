package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *         DelphiWorlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

import android.content.pm.PackageManager;
import android.util.Log;
import com.embarcadero.firemonkey.FMXNativeActivity;

public class DWNativeActivity extends com.embarcadero.firemonkey.FMXNativeActivity {

  static final String TAG = "DWNativeActivity";
  public native void onRequestPermissionsResultNative(int requestCode, String[] permissions, int[] grantResults);

  @Override
  public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults)
  {
    Log.d(TAG, "onRequestPermissionsResult");
    super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    onRequestPermissionsResultNative(requestCode, permissions, grantResults);
  }
}


