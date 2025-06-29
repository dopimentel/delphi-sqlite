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
  FireDAC.Phys.SQLiteWrapper.Stat;

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
begin
  FDConnection1.Params.DriverID := 'SQLite';
  FDConnection1.Params.Database := ExtractFilePath(ParamStr(0)) + 'banco.db';
  FDConnection1.Connected := True;

  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS pessoas (' +
    'numero INTEGER PRIMARY KEY, nome TEXT)'
  );
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
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
  // Agora l� do corpo da requisi��o POST (form-data)
  numeroStr := Request.ContentFields.Values['numero'];
  nome := Request.ContentFields.Values['nome'];

  if (numeroStr = '') or (nome = '') or (not TryStrToInt(numeroStr, numero)) then
  begin
    Response.StatusCode := 400; // Bad Request
    Response.Content := 'Par�metros inv�lidos. Use formul�rio POST com campos "numero" e "nome".';
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
begin
  numerosStr := Request.QueryFields.Values['numeros'];
  if numerosStr = '' then
  begin
    Response.StatusCode := 400;
    Response.Content := '{"status":"erro","mensagem":"Par�metro numeros � obrigat�rio"}';
    Response.ContentType := 'application/json';
    Exit;
  end;

  resultado := TStringList.Create;
  try
    resultado.Add('{"resultados": [');

    lista := numerosStr.Split([',']);

    for i := 0 to High(lista) do
    begin
      if TryStrToInt(Trim(lista[i]), numero) then
      begin
        FDQuery1.Close;
        FDQuery1.SQL.Text := 'SELECT nome FROM pessoas WHERE numero = ?';
        FDQuery1.Params[0].AsInteger := numero;
        FDQuery1.Open;

        if not FDQuery1.Eof then
          nome := FDQuery1.FieldByName('nome').AsString
        else
          nome := '[n�o encontrado]';

        resultado.Add(Format('{"numero": %d, "nome": "%s"}',
          [numero, nome.Replace('"', '\"')]));

        if i < High(lista) then
          resultado.Add(',');
      end;
    end;
    resultado.Add(']}');
    Response.ContentType := 'application/json';
    Response.Content := resultado.Text;
  finally
    resultado.Free;
  end;
end;

end.

