object WebModule1: TWebModule1
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 288
  Width = 519
  PixelsPerInch = 120
  object FDConnection1: TFDConnection
    Left = 296
    Top = 72
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 136
    Top = 88
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 320
    Top = 200
  end
end
