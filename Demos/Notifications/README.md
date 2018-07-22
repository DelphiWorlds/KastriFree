# Notifications Demo

# NOTE: This code is experimental, and may or may not be removed from Kastri Free

This code demonstrates use of the Notifications feature of Kastri Free, which is designed to be a total replacement for local notifications, and perhaps remote notifications in the future.

That means that you should not have to use the NotificationCenter component in Delphi, at least for local notifications.

It is also designed to be able to cater for targeting Android API 26 or higher, and will also work only with iOS 10 or higher, so if you use it, please ensure that you set the Minimum iOS version supported value in the Linker options to 10.0

The Android implementation requires the following files, relative to the root of the Kastri Free library

\Lib\dw-multireceiver.jar

\ThirdParty\Android\support-compat-26.1.0.jar

\ThirdParty\Android\support-core-utils.26.1.0.jar

Even though this code is experimental, if you feel so inclined, please report any issues to:

  https://github.com/DelphiWorlds/KastriFree/issues

Note: Repeating notifications are untested




