object ServiceModule: TServiceModule
  OldCreateOrder = False
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 405
  Width = 462
  object NotificationCenter: TNotificationCenter
    Left = 200
    Top = 104
  end
end
