package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *         DelphiWorlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.WindowManager;
import com.embarcadero.firemonkey.FMXNativeActivity;

public class DWNativeActivity extends com.embarcadero.firemonkey.FMXNativeActivity {

  static final String TAG = "DWNativeActivity";
  public native void onRequestPermissionsResultNative(int requestCode, String[] permissions, int[] grantResults);

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    if (Build.VERSION.SDK_INT >= 24)
      getWindow().addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
  }

  @Override
  public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults)
  {
    Log.d(TAG, "onRequestPermissionsResult");
    super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    onRequestPermissionsResultNative(requestCode, permissions, grantResults);
  }
}


