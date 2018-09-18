***** NOTE ******

In order for local notifications to work in this demo, you will need to create a patch for the Delphi source file:

  <BDS>\Source\rtl\common\System.Android.Notification.pas

(Where <BDS> is the root of the Delphi install, e.g: C:\Program Files (x86)\Embarcadero\Studio\19.0)

The patch file to be applied is:

  <KastriFree>\Workarounds\System.Android.Notification.10.2.3.API26.patch

(Where <KastriFree> is the root of the source files for the KastriFree library)

The tool for applying patches can be obtained from:

  http://gnuwin32.sourceforge.net/packages/patch.htm

Steps:

1. Install the Patch tool from http://gnuwin32.sourceforge.net/packages/patch.htm
2. Copy System.Android.Notification.pas from the Delphi source (described above) to the demo project folder
3. Copy System.Android.Notification.10.2.3.API26.patch from the Workarounds folder in the KastriFree library (described above) to the demo project folder
3. Open a command prompt, change to the project folder, and execute the following command:

  <PatchFolder>\Patch System.Android.Notification.pas < System.Android.Notification.10.2.3.API26.patch

  Where <PatchFolder> is where the Patch tool was installed to, e.g: C:\Program Files (x86)\Patch

This should result in a patched System.Android.Notification.pas file in the demo project folder. 
If you open the file you should note comments starting with: 

  // DW

Indicating where changes have been made. Note that the changes require units from the Workarounds folder, the API folder and the Core folder in the KastriFree library