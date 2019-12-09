package com.delphiworlds.kastri;

import android.util.Log;
import java.util.TimerTask;

public class DWTimerTask extends TimerTask {

  private static final String TAG = "DWTimerTask";
  private DWTimerTaskDelegate mDelegate;

  public DWTimerTask(DWTimerTaskDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void run() {
    // Log.v(TAG, "DWTimerTask.run");
    mDelegate.run();
  }
}