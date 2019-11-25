package com.delphiworlds.kastri;

import android.content.Intent;
import android.net.Uri;
import android.webkit.WebChromeClient;
import android.webkit.WebChromeClient.FileChooserParams;
import android.webkit.WebView;
import android.webkit.PermissionRequest;
import android.webkit.ValueCallback;

public class DWWebChromeClient extends WebChromeClient {

  private DWWebChromeClientDelegate mDelegate;
  private ValueCallback<Uri[]> mFilePathCallback;

  public DWWebChromeClient(DWWebChromeClientDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onPermissionRequest(final PermissionRequest request) {
    request.grant(request.getResources());
  }

  // Gleaned from: https://github.com/anthonycr/Lightning-Browser/issues/253
  @Override
  public boolean onShowFileChooser(WebView webView, ValueCallback<Uri[]> filePathCallback, FileChooserParams fileChooserParams) {
    mFilePathCallback = filePathCallback;
    return mDelegate.onFileChooserIntent(fileChooserParams.createIntent());
  } 

  public void handleFileChooserResult(Intent intent, int resultCode) {
    mFilePathCallback.onReceiveValue(WebChromeClient.FileChooserParams.parseResult(resultCode, intent));
    mFilePathCallback = null;
  }
}