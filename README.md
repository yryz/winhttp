winhttp
=======

基于WinHTTP核心封装的HTTP库

基于WinHTTP封装的DELPHI HTTP库，小巧、高效，已经在项目中使用很久，共享出来，算是为Delphi贡献一点东西吧。

###Demo:

    var
      Http: THTTP;
      LData: TUrlEncodedData;
      sData: string;
    begin
      Http := THTTP.Create;
      LData := TUrlEncodedData.Create(True, False);
      try
        LData.Put('_input_charset', 'utf-8');
        LData.Put('suffix', 'csv');

        Http.Referer := 'https://github.com/yryz/winhttp';
        sData := Http.Get('http://yryz.net?' + LData.ToString);
        //sData := Http.Post('http://yryz.net', LData.ToString);

         // 网络问题, 忽略
        if Http.RawHeader = '' then
        begin
          OutLog('网络连接异常!');
          Exit;
        end;

        OutLogFmt('检测耗时：%dms', [Http.GetUseTime]);

        // DO....
      finally
        LData.Free;
        Http.Free;
      end;
    end;
