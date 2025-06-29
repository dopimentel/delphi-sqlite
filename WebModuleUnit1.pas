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
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteWrapper;

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
  path := LowerCase(Request.PathInfo);

  if path.StartsWith('/cadastro') then
  begin
    if SameText(Request.Method, 'POST') then
      HandleCadastro(Request, Response)
    else
    begin
      Response.StatusCode := 405; // Method Not Allowed
      Response.Content := 'M�todo n�o permitido para /cadastro. Use POST.';
    end;
  end
  else if path.StartsWith('/pesquisa') then
  begin
    if SameText(Request.Method, 'GET') then
      HandlePesquisa(Request, Response)
    else
    begin
      Response.StatusCode := 405;
      Response.Content := 'M�todo n�o permitido para /pesquisa. Use GET.';
    end;
  end
  else
  begin
    Response.StatusCode := 404;
    Response.Content := 'Endpoint desconhecido';
  end;

  Handled := True;
end;

procedure TWebModule1.HandleCadastro(Request: TWebRequest; Response: TWebResponse);
var
  numeroStr, nome: string;
  numero: Integer;
begin
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

  // Agora lê do corpo da requisição POST (form-data)
  numeroStr := Request.ContentFields.Values['numero'];
  nome := Request.ContentFields.Values['nome'];

  if (numeroStr = '') or (nome = '') or (not TryStrToInt(numeroStr, numero)) then
  begin
    Response.StatusCode := 400; // Bad Request
    Response.Content := 'Parâmetros inválidos. Use formulário POST com campos "numero" e "nome".';
    Exit;
  end;

  try
    FDConnection1.ExecSQL('INSERT OR REPLACE INTO pessoas(numero, nome) VALUES(?, ?)',
      [numero, nome]);
    Response.Content := '{"status":"sucesso","mensagem":"Cadastro realizado com sucesso"}';
    Response.ContentType := 'application/json';
  except
    on E: Exception do
    begin
      Response.StatusCode := 500; // Internal Server Error
      Response.Content := '{"status":"erro","mensagem":"' + E.Message + '"}';
      Response.ContentType := 'application/json';
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
    Response.Content := '{"status":"erro","mensagem":"Parâmetro numeros é obrigatório"}';
    Response.ContentType := 'application/json';
    Exit;
  end;

  resultado := TStringList.Create;
  try
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
    Response.StatusCode := 200;
    Response.Content := resultado.Text.Replace(#13#10, '').Replace(#13, '').Replace(#10, '');
    
  finally
    resultado.Free;
  end;
end;

end.

