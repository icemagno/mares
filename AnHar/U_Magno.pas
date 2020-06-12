unit U_Magno;

interface

uses U_ClasseRotinasGenericas, ADODB, SysUtils, ActiveX,U_ClasseAnaliseHarmonicaSis, U_ClassePrevisaoSis,
U_Tipos;

type
  TMagno = class
  private
  public
     procedure CarregaFasesLua( Ano: Word; Fuso: Smallint; ArquivoFases: string);
     procedure GeraArqAlturasDll( CodEstacao : Word; CodEqpto : Word; DataIni, DataFim, Arquivo: string  );
     procedure GeraArqConstantesDll( CodEstacao: Word; CodAnalise: integer; Arquivo: string  );
     function getConnection() :  TADOConnection;
     function RetornaZ0Analise(Conexao: TADOConnection; CodAnalise: Integer): Real;
     procedure previsaoColunas( CodEstacao: Word; vDataInicial, vDataFinal: TDateTime;
        Diretorio: string; TipoSaida : Word; ArqConst : string );
     function geraTabua(CodEstacao : word; NumEstacao:word; DirDadosTabuas:string; Ano: TAnoTabua; ArquivoConst : string): string;

  end;

implementation


function TMagno.getConnection() : TADOConnection;
var
  AdoConnection : TADOConnection;
  StringConexao : string;
  config: TextFile;
  ip,porta,usuario,senha : string;
begin
  AssignFile(config, 'config.ini');
  Reset(config);
  Readln(config, ip);
  Readln(config, porta);
  Readln(config, usuario);
  Readln(config, senha);
  CloseFile(config);
  StringConexao := 'Provider=SQLOLEDB;User ID='+usuario+';Data Source='+ip+','+porta+'/mare;Password='+senha+';Initial Catalog=mare';

  Writeln('Usando Banco de Dados em ' + ip + ':' + porta);

  CoInitialize(nil);
  AdoConnection:=TADOConnection.Create( nil );
  AdoConnection.LoginPrompt:=False;
  AdoConnection.ConnectionString:=StringConexao;
  AdoConnection.Connected:=True;

  Result := AdoConnection;
end;

function TMagno.RetornaZ0Analise(Conexao: TADOConnection; CodAnalise: Integer): Real;
var
  AdoConnection : TADOConnection;
  CRotGen: TClasseRotinasGenericas;
  Z0 : Real;
begin
  AdoConnection := getConnection();
  CRotGen := TClasseRotinasGenericas.Create();

  Z0 := CRotGen.RetornaZ0Analise(AdoConnection, CodAnalise );


  Result := Z0;
  
end;


procedure TMagno.CarregaFasesLua( Ano: Word; Fuso: Smallint; ArquivoFases: string);
var
  AdoConnection : TADOConnection;
  Prev : TClassePrevisaoSis;
begin
  AdoConnection := getConnection();
  Prev := TClassePrevisaoSis.create();
  Prev.CarregaFasesLua( AdoConnection, Ano, Fuso, ArquivoFases );
end;

function TMagno.geraTabua( CodEstacao : word; NumEstacao:word; DirDadosTabuas:string; Ano: TAnoTabua; ArquivoConst : string): string;
var
  AdoConnection : TADOConnection;
  Prev : TClassePrevisaoSis;
begin
  AdoConnection := getConnection();
  Prev := TClassePrevisaoSis.create();
  result := Prev.geraTabua( AdoConnection, CodEstacao, NumEstacao, DirDadosTabuas,Ano , ArquivoConst );
end;

procedure TMagno.GeraArqConstantesDll( CodEstacao: Word; CodAnalise: integer; Arquivo: string  );
var
  AdoConnection : TADOConnection;
  CRotGen: TClasseRotinasGenericas;
begin

  AdoConnection := getConnection();

  CRotGen := TClasseRotinasGenericas.Create();
  //CRotGen.GeraArqConstantesDll(AdoConnection, CodEstacao, CodAnalise, Arquivo);
  //CRotGen.GeraArquivoConstantesDll(AdoConnection, 1, CodAnalise, Arquivo);
  CRotGen.GeraArqConstantesPadraoDll( AdoConnection, CodEstacao, Arquivo );

end;  






// *********************************************************************
// Eh isso que me interessa
// *********************************************************************


procedure TMagno.previsaoColunas( CodEstacao: Word; vDataInicial, vDataFinal: TDateTime;
    Diretorio: string; TipoSaida : Word; ArqConst : string );
var
  Prev : TClassePrevisaoSis;
  AdoConnection : TADOConnection;
begin
  Prev := TClassePrevisaoSis.create();
  AdoConnection := getConnection();
  Prev.PrevisaoColunasOld(AdoConnection, 1, CodEstacao, vDataInicial, vDataFinal, Diretorio, TipoSaida, true, false, ArqConst);
end;


// *********************************************************************
// *********************************************************************
// *********************************************************************














procedure TMagno.GeraArqAlturasDll( CodEstacao : Word; CodEqpto : Word; DataIni, DataFim, Arquivo: string  );
var
  AdoConnection : TADOConnection;
  CRotGen: TClasseRotinasGenericas;
  //Sis : TClasseAnaliseHarmonicaSis;
begin
  AdoConnection := getConnection();

  CRotGen := TClasseRotinasGenericas.Create();
  CRotGen.GeraArqAlturasDll(AdoConnection, CodEstacao, CodEqpto, DataIni, DataFim, Arquivo);

  //Sis := TClasseAnaliseHarmonicaSis.create();
  //Sis.geraArqAlturasDll( Arquivo );

end;

end.
