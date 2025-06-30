unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, FireDAC.ConsoleUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteWrapper,
  IdException;

type
  TWebModule1 = class(TWebModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    procedure HandleCadastro(Request: TWebRequest; Response: TWebResponse);
    procedure HandlePesquisa(Request: TWebRequest; Response: TWebResponse);
    procedure InitDatabase;
  public
  end;

var
  WebModule1: TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule1.InitDatabase;
var
  dbPath: string;
begin
  try
    // Garantir que a conexão esteja fechada
    FDConnection1.Connected := False;
    
    // Limpar e reconfigurar os parâmetros
    FDConnection1.Params.Clear;
    
    // Configurar o caminho do banco dinamicamente
    dbPath := ExtractFilePath(ParamStr(0)) + 'banco.db';
    
    FDConnection1.Params.Add('DriverID=SQLite');
    FDConnection1.Params.Add('Database=' + dbPath);
    FDConnection1.Params.Add('LockingMode=Normal');
    
    // Conectar
    FDConnection1.Connected := True;

    // Criar as tabelas se não existirem
    FDConnection1.ExecSQL(
      'CREATE TABLE IF NOT EXISTS pessoas (' +
      'numero INTEGER PRIMARY KEY, nome TEXT)'
    );

    FDConnection1.ExecSQL(
      'CREATE TABLE IF NOT EXISTS Cadastros (' +
      'Numero INTEGER PRIMARY KEY, Nome TEXT)'
    );
    
    // Verificar se as tabelas foram criadas
    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT name FROM sqlite_master WHERE type="table"';
    FDQuery1.Open;
    
  except
    on E: Exception do
    begin
      // Log do erro para debug
      raise Exception.Create('Erro ao inicializar banco: ' + E.Message);
    end;
  end;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  // Forçar o carregamento do driver SQLite
  FDPhysSQLiteDriverLink1.Release;
  
  InitDatabase;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  path: string;
begin
  try
    path := LowerCase(Request.PathInfo);

    if path.StartsWith('/cadastro') then
    begin
      if SameText(Request.Method, 'POST') then
        HandleCadastro(Request, Response)
      else
      begin
        Response.StatusCode := 405; // Method Not Allowed
        Response.Content := 'Método não permitido para /cadastro. Use POST.';
      end;
    end
    else if path.StartsWith('/pesquisa') then
    begin
      if SameText(Request.Method, 'GET') then
        HandlePesquisa(Request, Response)
      else
      begin
        Response.StatusCode := 405;
        Response.Content := 'Método não permitido para /pesquisa. Use GET.';
      end;
    end
    else if path.StartsWith('/') then
    begin
      // ...existing code...
      Response.Content :=
        '<html>' +
        '<head>' +
        '<title>Desafio Delphi</title>' +
        '<meta charset="UTF-8">' +
        '<style>' +
        '  body { font-family: Arial, sans-serif; margin: 40px; background-color: #f5f5f5; }' +
        '  .container { max-width: 800px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }' +
        '  h1 { color: #333; text-align: center; margin-bottom: 30px; }' +
        '  .form-section { margin-bottom: 40px; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }' +
        '  .form-section h2 { color: #555; margin-top: 0; }' +
        '  .form-group { margin-bottom: 15px; }' +
        '  label { display: block; margin-bottom: 5px; font-weight: bold; color: #666; }' +
        '  input[type="number"], input[type="text"] { width: 100%; padding: 10px; border: 1px solid #ccc; border-radius: 4px; font-size: 14px; }' +
        '  button { background-color: #007bff; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; font-size: 14px; margin-right: 10px; }' +
        '  button:hover { background-color: #0056b3; }' +
        '  .result { margin-top: 20px; padding: 15px; border-radius: 4px; }' +
        '  .success { background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724; }' +
        '  .error { background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24; }' +
        '</style>' +
        '</head>' +
        '<body>' +
        '<div class="container">' +
        '  <h1>Sistema de Cadastro de Pessoas</h1>' +
        '  ' +
        '  <!-- Formulário de Cadastro -->' +
        '  <div class="form-section">' +
        '    <h2>Cadastrar Pessoa</h2>' +
        '    <form action="/cadastro" method="post" enctype="application/x-www-form-urlencoded">' +
    '      <div class="form-group">' +
    '        <label for="numero">Número:</label>' +
    '        <input type="number" id="numero" name="numero" required>' +
    '      </div>' +
    '      <div class="form-group">' +
    '        <label for="nome">Nome:</label>' +
    '        <input type="text" id="nome" name="nome" required>' +
    '      </div>' +
        '      <button type="submit">Cadastrar</button>' +
        '    </form>' +
        '  </div>' +
        '  ' +
        '  <!-- Formulário de Pesquisa -->' +
        '  <div class="form-section">' +
        '    <h2>Pesquisar Pessoas</h2>' +
        '    <form action="/pesquisa" method="get">' +
        '      <div class="form-group">' +
        '        <label for="numeros">Números (separados por vírgula):</label>' +
        '        <input type="text" id="numeros" name="numeros" placeholder="Ex: 1,2,3" required>' +
        '      </div>' +
        '      <button type="submit">Pesquisar</button>' +
        '    </form>' +
        '  </div>' +
        '</div>' +
        '</body>' +
        '</html>';
    end
    else
    begin
      Response.StatusCode := 404;
      Response.Content := 'Endpoint desconhecido';
    end;

    Handled := True;
    
  except
    on E: EIdConnClosedGracefully do
    begin
      // Ignorar esta exceção - é normal quando o cliente fecha a conexão
      Handled := True;
    end;
    on E: EIdException do
    begin
      // Outras exceções do Indy - log mas não propagar
      Handled := True;
    end;
    on E: Exception do
    begin
      // Outras exceções - responder com erro 500
      try
        Response.StatusCode := 500;
        Response.Content := 'Erro interno do servidor';
        Handled := True;
      except
        // Se não conseguir responder, apenas marcar como handled
        Handled := True;
      end;
    end;
  end;
end;

procedure TWebModule1.HandleCadastro(Request: TWebRequest; Response: TWebResponse);
var
  numeroStr, nome: string;
  numero: Integer;
  debugInfo: string;
  content: string;
  pairs: TArray<string>;
  pair: string;
  keyValue: TArray<string>;
  key, value: string;
begin
  try
    // Debug: informações da requisição
    debugInfo := 'Method: ' + Request.Method + #13#10 +
                 'ContentType: ' + Request.ContentType + #13#10 +
                 'Content: ' + Request.Content + #13#10 +
                 'ContentLength: ' + IntToStr(Request.ContentLength) + #13#10;

    // Verificar se a conexão está ativa
    if not FDConnection1.Connected then
    begin
      try
        InitDatabase;
      except
        on E: Exception do
        begin
          Response.StatusCode := 500;
          Response.Content := '{"status":"erro","mensagem":"Erro de conexão: ' + E.Message + '"}';
          Response.ContentType := 'application/json';
          Exit;
        end;
      end;
    end;

    // Capturar dados do formulário POST
    try
      // Método 1: Tentar ContentFields primeiro
      numeroStr := Request.ContentFields.Values['numero'];
      nome := Request.ContentFields.Values['nome'];
      
      // Método 2: Se não funcionar, tentar QueryFields (para debug)
      if (numeroStr = '') and (nome = '') then
      begin
        numeroStr := Request.QueryFields.Values['numero'];
        nome := Request.QueryFields.Values['nome'];
      end;
      
      // Método 3: Parse manual do Content para form-data
      if (numeroStr = '') and (nome = '') and (Request.Content <> '') then
      begin
        content := Request.Content;
        pairs := content.Split(['&']);
        for pair in pairs do
        begin
          keyValue := pair.Split(['=']);
          if Length(keyValue) = 2 then
          begin
            key := keyValue[0];
            value := keyValue[1];
            // Decodificar URL encoding se necessário
            value := StringReplace(value, '+', ' ', [rfReplaceAll]);
            if key = 'numero' then
              numeroStr := value
            else if key = 'nome' then
              nome := value;
          end;
        end;
      end;
      
      debugInfo := debugInfo + 'numeroStr: ' + numeroStr + #13#10 + 'nome: ' + nome + #13#10;
      
    except
      on E: Exception do
      begin
        debugInfo := debugInfo + 'Erro ao capturar dados: ' + E.Message + #13#10;
      end;
    end;

    if (numeroStr = '') or (nome = '') or (not TryStrToInt(numeroStr, numero)) then
    begin
      Response.StatusCode := 400; // Bad Request
      if Request.GetFieldByName('Accept').Contains('text/html') then
      begin
        Response.ContentType := 'text/html';
        Response.Content := 
          '<html><head><title>Dados Inválidos</title>' +
          '<meta charset="UTF-8">' +
          '<style>body{font-family:Arial,sans-serif;margin:40px;text-align:center;}</style>' +
          '</head><body>' +
          '<h2 style="color:red;">Dados inválidos!</h2>' +
          '<p>Por favor, preencha corretamente o número (deve ser um número inteiro) e o nome.</p>' +
          '<details><summary>Debug Info</summary><pre>' + debugInfo + '</pre></details>' +
          '<a href="/" style="text-decoration:none;background:#007bff;color:white;padding:10px 20px;border-radius:4px;">Voltar</a>' +
          '</body></html>';
      end
      else
      begin
        Response.Content := 'Parâmetros inválidos. Debug: ' + debugInfo;
      end;
      Exit;
    end;

    try
      FDConnection1.ExecSQL('INSERT OR REPLACE INTO pessoas(numero, nome) VALUES(?, ?)',
        [numero, nome]);
      
      // Verificar se é uma requisição via navegador (Accept header)
      if Request.GetFieldByName('Accept').Contains('text/html') then
      begin
        Response.ContentType := 'text/html';
        Response.Content := 
          '<html><head><title>Cadastro Realizado</title>' +
          '<meta charset="UTF-8">' +
          '<style>body{font-family:Arial,sans-serif;margin:40px;text-align:center;}</style>' +
          '</head><body>' +
          '<h2 style="color:green;">Cadastro realizado com sucesso!</h2>' +
          '<p>Pessoa <strong>' + nome + '</strong> (número ' + IntToStr(numero) + ') foi cadastrada.</p>' +
          '<a href="/" style="text-decoration:none;background:#007bff;color:white;padding:10px 20px;border-radius:4px;">Voltar</a>' +
          '</body></html>';
      end
      else
      begin
        Response.Content := '{"status":"sucesso","mensagem":"Cadastro realizado com sucesso"}';
        Response.ContentType := 'application/json';
      end;
    except
      on E: Exception do
      begin
        Response.StatusCode := 500; // Internal Server Error
        if Request.GetFieldByName('Accept').Contains('text/html') then
        begin
          Response.ContentType := 'text/html';
          Response.Content := 
            '<html><head><title>Erro no Cadastro</title>' +
            '<meta charset="UTF-8">' +
            '<style>body{font-family:Arial,sans-serif;margin:40px;text-align:center;}</style>' +
            '</head><body>' +
            '<h2 style="color:red;">Erro no cadastro!</h2>' +
            '<p>' + E.Message + '</p>' +
            '<a href="/" style="text-decoration:none;background:#007bff;color:white;padding:10px 20px;border-radius:4px;">Voltar</a>' +
            '</body></html>';
        end
        else
        begin
          Response.Content := '{"status":"erro","mensagem":"' + E.Message + '"}';
          Response.ContentType := 'application/json';
        end;
      end;
    end;
    
  except
    on E: EIdConnClosedGracefully do
    begin
      // Ignorar - conexão fechada pelo cliente (normal)
    end;
    on E: EIdException do
    begin
      // Outras exceções do Indy - ignorar
    end;
    on E: Exception do
    begin
      // Outras exceções - tentar responder com erro
      try
        Response.StatusCode := 500;
        Response.Content := 'Erro interno do servidor: ' + E.Message;
      except
        // Se não conseguir responder, apenas ignore
      end;
    end;
  end;
end;

procedure TWebModule1.HandlePesquisa(Request: TWebRequest; Response: TWebResponse);
var
  numerosStr: string;
  lista: TArray<string>;
  i: Integer;
  numero: Integer;
  nome: string;
  resultado: TStringList;
  primeiroItem: Boolean;
begin
  try
    // Verificar se a conexão está ativa
    if not FDConnection1.Connected then
    begin
      try
        InitDatabase;
      except
        on E: Exception do
        begin
          Response.StatusCode := 500;
          Response.Content := '{"status":"erro","mensagem":"Erro de conexão: ' + E.Message + '"}';
          Response.ContentType := 'application/json';
          Exit;
        end;
      end;
    end;

  numerosStr := Request.QueryFields.Values['numeros'];
  if numerosStr = '' then
  begin
    Response.StatusCode := 400;
    if Request.GetFieldByName('Accept').Contains('text/html') then
    begin
      Response.ContentType := 'text/html';
      Response.Content := 
        '<html><head><title>Parâmetro Obrigatório</title>' +
        '<meta charset="UTF-8">' +
        '<style>body{font-family:Arial,sans-serif;margin:40px;text-align:center;}</style>' +
        '</head><body>' +
        '<h2 style="color:red;">Parâmetro obrigatório!</h2>' +
        '<p>Por favor, informe os números que deseja pesquisar.</p>' +
        '<a href="/" style="text-decoration:none;background:#007bff;color:white;padding:10px 20px;border-radius:4px;">Voltar</a>' +
        '</body></html>';
    end
    else
    begin
      Response.Content := '{"status":"erro","mensagem":"Parâmetro numeros é obrigatório"}';
      Response.ContentType := 'application/json';
    end;
    Exit;
  end;

  resultado := TStringList.Create;
  try
    // Verificar se é uma requisição via navegador
    if Request.GetFieldByName('Accept').Contains('text/html') then
    begin
      // Resposta HTML
      resultado.Add('<html><head><title>Resultado da Pesquisa</title>');
      resultado.Add('<meta charset="UTF-8">');
      resultado.Add('<style>');
      resultado.Add('body{font-family:Arial,sans-serif;margin:40px;}');
      resultado.Add('.container{max-width:800px;margin:0 auto;}');
      resultado.Add('table{width:100%;border-collapse:collapse;margin:20px 0;}');
      resultado.Add('th,td{padding:12px;text-align:left;border-bottom:1px solid #ddd;}');
      resultado.Add('th{background-color:#f2f2f2;}');
      resultado.Add('.not-found{color:#888;font-style:italic;}');
      resultado.Add('.error{color:red;}');
      resultado.Add('</style>');
      resultado.Add('</head><body><div class="container">');
      resultado.Add('<h2>Resultado da Pesquisa</h2>');
      resultado.Add('<table><thead><tr><th>Número</th><th>Nome</th></tr></thead><tbody>');
      
      lista := numerosStr.Split([',']);
      for i := 0 to High(lista) do
      begin
        if TryStrToInt(Trim(lista[i]), numero) then
        begin
          try
            FDQuery1.Close;
            FDQuery1.SQL.Text := 'SELECT nome FROM pessoas WHERE numero = :numero';
            FDQuery1.ParamByName('numero').AsInteger := numero;
            FDQuery1.Open;

            if not FDQuery1.Eof then
              nome := FDQuery1.FieldByName('nome').AsString
            else
              nome := '[não encontrado]';

            if nome = '[não encontrado]' then
              resultado.Add(Format('<tr><td>%d</td><td class="not-found">%s</td></tr>', [numero, nome]))
            else
              resultado.Add(Format('<tr><td>%d</td><td>%s</td></tr>', [numero, nome]));
              
          except
            on E: Exception do
              resultado.Add(Format('<tr><td>%d</td><td class="error">Erro: %s</td></tr>', [numero, E.Message]));
          end;
        end;
      end;
      
      resultado.Add('</tbody></table>');
      resultado.Add('<a href="/" style="text-decoration:none;background:#007bff;color:white;padding:10px 20px;border-radius:4px;">Voltar</a>');
      resultado.Add('</div></body></html>');
      
      Response.ContentType := 'text/html';
    end
    else
    begin
      // Resposta JSON
      resultado.Add('{"resultados": [');
      
      lista := numerosStr.Split([',']);
      primeiroItem := True;

      for i := 0 to High(lista) do
      begin
        if TryStrToInt(Trim(lista[i]), numero) then
        begin
          try
            FDQuery1.Close;
            FDQuery1.SQL.Text := 'SELECT nome FROM pessoas WHERE numero = :numero';
            FDQuery1.ParamByName('numero').AsInteger := numero;
            FDQuery1.Open;

            if not FDQuery1.Eof then
              nome := FDQuery1.FieldByName('nome').AsString
            else
              nome := '[não encontrado]';

            if not primeiroItem then
              resultado.Add(',');
              
            resultado.Add(Format('{"numero": %d, "nome": "%s"}',
              [numero, nome.Replace('"', '\"')]));
            
            primeiroItem := False;
          except
            on E: Exception do
            begin
              if not primeiroItem then
                resultado.Add(',');
              resultado.Add(Format('{"numero": %d, "nome": "erro: %s"}',
                [numero, E.Message.Replace('"', '\"')]));
              primeiroItem := False;
            end;
          end;
        end;
      end;
      
      resultado.Add(']}');
      Response.ContentType := 'application/json';
    end;
    
    Response.StatusCode := 200;
    if Request.GetFieldByName('Accept').Contains('text/html') then
      Response.Content := resultado.Text
    else
      Response.Content := resultado.Text.Replace(#13#10, '').Replace(#13, '').Replace(#10, '');
    
  finally
    resultado.Free;
  end;
  
  except
    on E: EIdConnClosedGracefully do
    begin
      // Ignorar - conexão fechada pelo cliente (normal)
    end;
    on E: EIdException do
    begin
      // Outras exceções do Indy - ignorar
    end;
    on E: Exception do
    begin
      // Tentar responder com erro
      try
        Response.StatusCode := 500;
        Response.Content := 'Erro interno do servidor: ' + E.Message;
      except
        // Se não conseguir responder, apenas ignore
      end;
    end;
  end;
end;

end.

