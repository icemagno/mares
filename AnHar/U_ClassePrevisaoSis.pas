unit U_ClassePrevisaoSis;

interface

uses
  SysUtils, Windows, Controls, BNDO, DB, ADODB, U_Dll,
  U_ClasseRotinasGenericas, U_Tipos;

type
  TClassePrevisaoSis = class
  private
    CRotGen: TClasseRotinasGenericas;
    function formataIndiceComponente(Indice: string): string;
    function atualizaNumComponentes(Area: MinInteiroSs; Conexao: TADOConnection; CodAnalise : Integer): Word;
    procedure geraArquivoConstantesSisDll(Conexao: TADOConnection; Arquivo: string; Area: MinInteiroSs; CodEstacao: PeqInteiroSs; CodAnalise: Integer );
    function getArquivoBase(Diretorio :String; CodEstacao:word; CodAnalise, DataIni, DataFim: string) : String;
    procedure GeraCabecalhoPrevisaoHoraria(Conexao: TADOConnection; Area: MinInteiroSs; CodAnalise: Integer;
        DataIni, DataFim: TDate; Arquivo: string);
    procedure GeraCabecalhoPrevisaoMaxMin(Conexao: TADOConnection; Area: MinInteiroSs; CodAnalise: Integer;
  DataIni, DataFim: TDate; Arquivo: string);
  public
    function geraTabua(Conexao: TADOConnection; CodEstacao : PeqInteiroSs; NumEstacao:word; DirDadosTabuas:string; Ano: TAnoTabua; ArquivoConst : string): string;
    procedure CarregaFasesLua(Conexao: TADOConnection; Ano: PeqInteiroSs; Fuso: Smallint; ArquivoFases: string);

    procedure PrevisaoColunasOld(Conexao: TADOConnection; Area: MinInteiroSs; CodEstacao: PeqInteiroSs; 
  DataIni, DataFim: TDate; Diretorio: string; TipoSaida: MinInteiroSs; Cabecalho, DeletaConst: Boolean; ArqConst : string);

    procedure previsaoColunas(vDataInicial, vDataFinal: TDateTime;
      Z0: Real; ArquivoConst, ArquivoPrev: string; TipoSaida : Integer);

    procedure previsaoColunasGeral(DataIni, DataFim: TDate; Arquivo: string;
      Z0: Real; TipoSaida: MinInteiroSs; Cabecalho, DeletaConst: Boolean);
  end;

implementation


function TClassePrevisaoSis.geraTabua(Conexao: TADOConnection; CodEstacao : PeqInteiroSs; NumEstacao:word; DirDadosTabuas:string; Ano: TAnoTabua; ArquivoConst : string): string;
var
  Portos: TADOQuery;
  ArqConstDll, ArqPortoDll, ArqTabuaDll: array [1..150] of char;
  Carta: array [1..5] of char;
  Instituicao: array [1..13] of char;
  NomePorto: array [1..76] of char;
  AnoTabua: array [1..4] of char;
  ArqConst, ArqTabua, ArqPorto: string;
  CRotGen : TClasseRotinasGenericas;
begin

  CRotGen := TClasseRotinasGenericas.Create();

  ArqConst:= DirDadosTabuas + FormatFloat('00000', NumEstacao) + 'CH.tmp';
  ArqTabua:= DirDadosTabuas + FormatFloat('00000', NumEstacao) +
    FormatFloat('0000', Ano) + 'Tabua.txt';
  ArqPorto:= DirDadosTabuas + FormatFloat('00000', NumEstacao) +
    FormatFloat('0000', Ano) + 'Porto.txt';
  if (FileExists(ArqConst)) then
    SysUtils.DeleteFile(ArqConst);
  if (FileExists(ArqTabua)) then
    SysUtils.DeleteFile(ArqTabua);
  if (FileExists(ArqPorto)) then
    SysUtils.DeleteFile(ArqPorto);
  Portos:= TADOQuery.Create(nil);

  Portos.Connection:= Conexao;

  Portos.SQL.Add('select p.*, e.instituicao from mare.dbo.portos p, (select cod_estacao_maregrafica, ' +
    'instituicao from mare.dbo.estacao_maregrafica where cod_estacao_maregrafica = ' + IntToStr(CodEstacao) + ') e ' +
    'where p.cod_estacao_maregrafica = e.cod_estacao_maregrafica');
  Portos.Open;
  BNDO.strtochar(Carta, Portos.FieldByName('carta').AsString);
  BNDO.strtochar(Instituicao, Portos.FieldByName('instituicao').AsString);
  BNDO.strtochar(NomePorto, Portos.FieldByName('nome').AsString);
  Portos.Close;
  FreeAndNil(Portos);

  CRotGen.GeraArqConstantesPadraoDll(Conexao, CodEstacao, ArqConst);

  BNDO.strtochar(AnoTabua, IntToStr(Ano));
  BNDO.strtochar(ArqConstDll, ArqConst);
  BNDO.strtochar(ArqPortoDll, ArqPorto);
  BNDO.strtochar(ArqTabuaDll, ArqTabua);
  U_Dll.TABUA(ArqConstDll, ArqPortoDll, ArqTabuaDll, Carta, Instituicao, NomePorto, AnoTabua);
  //SysUtils.DeleteFile(ArqConst);
  //SysUtils.DeleteFile(ArqPorto);
  Result:= ArqTabua;
end;

procedure TClassePrevisaoSis.CarregaFasesLua(Conexao: TADOConnection; Ano: PeqInteiroSs; Fuso: Smallint;
  ArquivoFases: string);
var
  ArqFases: TextFile;
  FasesLua: TADOQuery;
begin
  AssignFile(ArqFases, ArquivoFases);
  Rewrite(ArqFases);
  FasesLua:= TADOQuery.Create(nil);
  FasesLua.Connection:= Conexao;
  FasesLua.SQL.Clear;
  FasesLua.SQL.Add('select dateadd(hh, ' + IntToStr(Fuso) + ', data_hora) data_hora, fase_lua ');
  FasesLua.SQL.Add('from mare.dbo.fases_lua_tu where datepart(yyyy, data_hora) = ' + IntToStr(Ano));
  FasesLua.Open;
  while (not (FasesLua.Eof)) do
    begin
      Writeln(ArqFases, FormatDateTime('dd/mm/yyyy', FasesLua.FieldByName('data_hora').AsDateTime) + ' ' +
        FasesLua.FieldByName('fase_lua').AsString);
      FasesLua.Next;  
    end;
  FasesLua.Close;
  CloseFile(ArqFases);
end;


procedure TClassePrevisaoSis.GeraCabecalhoPrevisaoMaxMin(Conexao: TADOConnection; Area: MinInteiroSs; CodAnalise: Integer;
  DataIni, DataFim: TDate; Arquivo: string);
var
  Arq: TextFile;
  Query: TADOQuery;
begin
  AssignFile(Arq, Trim(Arquivo));
  if (FileExists(Arquivo)) then
    Append(Arq)
  else
    Rewrite(Arq);
  Query:= TADOQuery.Create(nil);
  Query.Connection:= Conexao;
  Query.SQL.Clear;
  Query.SQL.Add('select em.num_estacao_maregrafica, em.nome_estacao_maregrafica, em.latitude, em.longitude, em.fuso, am.* ');
  Query.SQL.Add('from estacao_maregrafica em, (select cod_estacao_maregrafica, data_hora_inicio, data_hora_fim, num_componentes, z0 ');
  case (Area) of
    1: Query.SQL.Add('from vw_analise_mares ');
    2: Query.SQL.Add('from vw_analise_mares_temp ');
  else
    Query.SQL.Add('from vw_analise_mares ');
  end;
  Query.SQL.Add('where cod_analise_mares = ' + IntToStr(CodAnalise) + ') am where em.cod_estacao_maregrafica = am.cod_estacao_maregrafica');
  Query.Open;
  Writeln(Arq, 'PREVISÃO MÁXIMAS E MÍNIMAS - ' + FormatDateTime('dd/mm/yyyy', DataIni) + ' A ' + FormatDateTime('dd/mm/yyyy', DataFim));
  Writeln(Arq, 'ESTAÇÃO:' + FormatFloat('00000', StrToFloat(Query.FieldByName('num_estacao_maregrafica').AsString)));
  Writeln(Arq, 'NOME ESTAÇÃO:' + Query.FieldByName('nome_estacao_maregrafica').AsString);
  Writeln(Arq, 'LATITUDE:' + BNDO.FormataLatLonMareTela(Query.FieldByName('latitude').AsInteger, 'LT'));
  Writeln(Arq, 'LONGITUDE:' + BNDO.FormataLatLonMareTela(Query.FieldByName('longitude').AsInteger, 'LG'));
  Writeln(Arq, 'FUSO:' + BNDO.FormataFusoMareTela(Query.FieldByName('fuso').AsString));
  Writeln(Arq, 'PERÍODO DA ANÁLISE:' + FormatDateTime('dd/mm/yyyy', Query.FieldByName('data_hora_inicio').AsDateTime) +
    ' A ' + FormatDateTime('dd/mm/yyyy', Query.FieldByName('data_hora_fim').AsDateTime));
  Writeln(Arq, 'COMPONENTES:' + Query.FieldByName('num_componentes').AsString);
  Writeln(Arq, 'NÍVEL MÉDIO:' + FormatFloat('0.00', (Query.FieldByName('z0').AsFloat / 100)) + ' METROS');
  Writeln(Arq, 'ALTURAS EM METROS');
  Writeln(Arq, 'ALTURAS ACIMA DO NÍVEL DE REDUÇÃO (NR) LOCAL'); //Alteração solicitada pelo Intercâmbio - Jorge Carvalho
  Writeln(Arq, 'ESTA PREVISÃO É BASEADA EM CONSTANTES HARMÔNICAS OBTIDAS COM O PERÍODO DE ANÁLISE SUPRACITADO'); //Alteração solicitada pelo Intercâmbio - Jorge Carvalho
  Writeln(Arq, 'HORÁRIO PAPA');
  Writeln(Arq, '#');
  Query.Close;
  FreeAndNil(Query);
  CloseFile(Arq);
end;


function TClassePrevisaoSis.getArquivoBase( Diretorio :String; CodEstacao:word; CodAnalise, DataIni, DataFim: string ) : String;
begin
  //result := Diretorio + NumEstacao + CodAnalise + DataIni + DataFim;
  result := Diretorio + IntToStr( CodEstacao );
end;

function TClassePrevisaoSis.atualizaNumComponentes(Area: MinInteiroSs; Conexao: TADOConnection; CodAnalise : Integer): Word;
var
  Query: TADOQuery;
begin
  Query:= TADOQuery.Create(nil);
  Query.Connection:= Conexao;
  Query.SQL.Clear;
  Query.SQL.Add('select count(*) as cont from ');
  if (Area = 1) then
    Query.SQL.Add('mare.dbo.constantes_analise ')
  else
    Query.SQL.Add('mare.dbo.constantes_analise_temp ');
  Query.SQL.Add('where cod_analise_mares = ' + IntToStr(CodAnalise) + ' ');
  Query.SQL.Add('and cod_componente > 6');
  Query.Open;
  if (Query.IsEmpty) then
    Result:= 0
  else
    Result:= Query.FieldByName('cont').AsInteger;
  Query.Close;
  FreeAndNil(Query);
end;

function TClassePrevisaoSis.formataIndiceComponente(Indice: string): string;
var
  IndiceAux: string;
begin
  if (Length(Indice) = 0) then
    IndiceAux:= '  '
  else
    if (Length(Indice) = 1) then
      IndiceAux:= ' ' + Indice
    else
      IndiceAux:= Indice;
  Result:= IndiceAux;
end;

procedure TClassePrevisaoSis.geraArquivoConstantesSisDll(Conexao: TADOConnection; Arquivo: string; Area: MinInteiroSs; CodEstacao: PeqInteiroSs; CodAnalise: Integer);
var
  EstMaregr, AnaliseMares, Componente: TADOQuery;
  Arq: TextFile;
begin
  EstMaregr:= TADOQuery.Create(nil);
  AnaliseMares:= TADOQuery.Create(nil);
  Componente:= TADOQuery.Create(nil);
  EstMaregr.Connection:= Conexao;
  AnaliseMares.Connection:= Conexao;
  Componente.Connection:= Conexao;
  AssignFile(Arq, Arquivo);
  Rewrite(Arq);
  EstMaregr.SQL.Clear;
  EstMaregr.SQL.Add('select num_estacao_maregrafica, nome_estacao_maregrafica, latitude, longitude, fuso ');
  EstMaregr.SQL.Add('from estacao_maregrafica where cod_estacao_maregrafica = ' + IntToStr(CodEstacao) );
  AnaliseMares.SQL.Clear;
  AnaliseMares.SQL.Add('select data_hora_inicio, data_hora_fim, z0, num_componentes from ');
  if (Area = 1) then
    AnaliseMares.SQL.Add('mare.dbo.vw_analise_mares ')
  else
    AnaliseMares.SQL.Add('mare.dbo.vw_analise_mares_temp ');
  AnaliseMares.SQL.Add('where cod_analise_mares = ' + IntToStr(CodAnalise));
  EstMaregr.Open;
  AnaliseMares.Open;
  //Write(Arq, FormatFloat('000', AnaliseMares.FieldByName('num_componentes').AsFloat));
  Write(Arq, FormatFloat('000', atualizaNumComponentes(Area, Conexao, CodAnalise) ) );
  Write(Arq, FormatFloat('0000000.00', AnaliseMares.FieldByName('z0').AsFloat));
  Write(Arq, FormatDateTime('ddmmyyyy', AnaliseMares.FieldByName('data_hora_inicio').AsDateTime));
  Write(Arq, FormatDateTime('ddmmyyyy', AnaliseMares.FieldByName('data_hora_fim').AsDateTime));
  Write(Arq, FormatFloat('00000', EstMaregr.FieldByName('num_estacao_maregrafica').AsFloat));
  Write(Arq, BNDO.formata_string(EstMaregr.FieldByName('nome_estacao_maregrafica').AsString, 38));
  Write(Arq, BNDO.formata_latlon_mare(EstMaregr.FieldByName('latitude').AsInteger, 'LT'));
  Write(Arq, BNDO.formata_latlon_mare(EstMaregr.FieldByName('longitude').AsInteger, 'LG'));
  Write(Arq, EstMaregr.FieldByName('fuso').AsString);
  EstMaregr.Close;
  AnaliseMares.Close;
  Writeln(Arq);
  Componente.SQL.Clear;
  Componente.SQL.Add('select ct.cod_analise_mares, ct.g, ct.h, cp.* from ');
  if (Area = 1) then
    Componente.SQL.Add('mare.dbo.constantes_analise ct, ')
  else
    Componente.SQL.Add('mare.dbo.constantes_analise_temp ct, ');
  Componente.SQL.Add('(select * from mare.dbo.componente) as cp where cod_analise_mares = ' + IntToStr(CodAnalise));
  Componente.SQL.Add(' and ct.cod_componente = cp.cod_componente order by velocidade');
  Componente.Open;
  while not(Componente.Eof) do
    begin
      if ((Componente.FieldByName('cod_componente').AsInteger) > 6) then
        begin
          Write(Arq, Componente.FieldByName('tipo').AsString);
          Write(Arq, BNDO.formata_string(Componente.FieldByName('nome').AsString, 7));
          Write(Arq, FormatFloat('000.0000000', Componente.FieldByName('velocidade').AsFloat));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_1').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_2').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_3').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_4').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_5').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_6').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_7').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_8').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_9').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_10').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_11').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_12').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_13').AsString));
          Write(Arq, formataIndiceComponente(Componente.FieldByName('indice_14').AsString));
          Write(Arq, Formatfloat('00000.00', Componente.FieldByName('h').AsFloat));
          Write(Arq, Formatfloat('00000.00', Componente.FieldByName('g').AsFloat));
          Writeln(Arq);
        end;
      Componente.Next;
    end;
  Componente.Close;
  CloseFile(Arq);
  FreeAndNil(EstMaregr);
  FreeAndNil(AnaliseMares);
  FreeAndNil(Componente);
end;




// *********************************************************************
// Eh isso que me interessa
// *********************************************************************

procedure TClassePrevisaoSis.PrevisaoColunasOld(Conexao: TADOConnection; Area: MinInteiroSs; CodEstacao: PeqInteiroSs;
  DataIni, DataFim: TDate; Diretorio: string; TipoSaida: MinInteiroSs; Cabecalho, DeletaConst: Boolean; ArqConst : string);
{
  1 - Previsão Horária;
  2 - Previsão Máximas e Mínimas.
}
var
  ArqConstDll, ArqPrevDll: array [1..150] of Char;
  Tipo, Op, Sim: array [1..1] of Char;
  Di, Mi, Df, Mf: array [1..2] of Char;
  Ai, Af: array [1..4] of Char;
  Nivel,DataRelatorio: array [1..10] of Char;
  TArqPrev, TArqPrevSaida: TextFile;
  ArqPrev, ArqPrevAux, ArqAux: string;
  NumEstacao, DataIniArq, DataFimArq, CodAnaliseArq: string;
  AnoIni, AnoFim, AnoAux: string[4];
  VetPeriodosPrev: array of RegPeriodosPrev;
  Indice, I, DiaAux: Shortint;
  DataPrev: string[10];
  HoraPrev: string[6];
  AlturaPrev: string[5];
  CRotinasGenericas: TClasseRotinasGenericas;
  CodAnalise: Integer;
  Z0: Real;
begin
  CRotinasGenericas:= TClasseRotinasGenericas.Create;

  CodAnalise:= CRotinasGenericas.RetornaCodigoAnalisePeriodoPadrao(Conexao, CodEstacao);
  Z0 := CRotinasGenericas.RetornaZ0Analise(Conexao, CodAnalise );

  AnoIni:= FormatDateTime('yyyy', DataIni);
  AnoAux:= FormatDateTime('yyyy', DataIni);
  AnoFim:= FormatDateTime('yyyy', DataFim);
  Indice:= 1;
  SetLength(VetPeriodosPrev, Indice);
  VetPeriodosPrev[Indice-1].DataInicial:= '01/' + FormatDateTime('mm/yyyy', DataIni);
  while (AnoAux <> AnoFim) do
    begin
      VetPeriodosPrev[Indice-1].DataFinal:= '31/12/' + AnoAux;
      Indice:= Indice + 1;
      SetLength(VetPeriodosPrev, Indice);
      AnoAux:= IntToStr(StrToInt(AnoAux) + 1);
      VetPeriodosPrev[Indice-1].DataInicial:= '01/01/' + AnoAux;
    end;
  DiaAux:= 31;
  VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', DataFim);
  while not (CRotinasGenericas.ValidaDataString(VetPeriodosPrev[Indice-1].DataFinal)) do
    begin
      DiaAux:= DiaAux - 1;
      VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', DataFim);
    end;
  NumEstacao:= FormatFloat('00000', CRotinasGenericas.RetornaNumeroEstacao(Conexao, CodEstacao));
  DataIniArq:= BNDO.converte_data_arq(FormatDateTime('dd/mm/yyyy', DataIni));
  DataFimArq:= BNDO.converte_data_arq(FormatDateTime('dd/mm/yyyy', DataFim));

  CodAnaliseArq:= FormatFloat('00000', CodAnalise);

  //ArqConst:= Diretorio + NumEstacao + DataIniArq + DataFimArq + 'CH.txt';

  //CRotinasGenericas.GeraArquivoConstantesDll(Conexao, Area, CodAnalise, ArqConst);
  geraArquivoConstantesSisDll(Conexao, ArqConst, Area, CodEstacao, CodAnalise );



  BNDO.strtochar(ArqConstDll, ArqConst);
  BNDO.strtochar(Nivel, FormatFloat('0000000.00', Z0));
  BNDO.strtochar(Op, 'N');
  BNDO.strtochar(Sim, 'S');
  case (TipoSaida) of
    1: begin
         ArqPrev:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVHORCOL.txt';
         ArqPrevAux:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVHORCOL.tmp';
         if (FileExists(ArqPrev)) then
           SysUtils.DeleteFile(ArqPrev);
         if (FileExists(ArqPrevAux)) then
           SysUtils.DeleteFile(ArqPrevAux);
         if (Cabecalho) then
           //GeraCabecalhoPrevisaoHoraria(Conexao, Area, CodAnalise, DataIni, DataFim, ArqPrev);
       end;
    2: begin
         ArqPrev:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVMAXMINCOL.txt';
         ArqPrevAux:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVMAXMINCOL.tmp';
         if (FileExists(ArqPrev)) then
           SysUtils.DeleteFile(ArqPrev);
         if (FileExists(ArqPrevAux)) then
           SysUtils.DeleteFile(ArqPrevAux);
         if (Cabecalho) then
           //GeraCabecalhoPrevisaoMaxMin(Conexao, Area, CodAnalise, DataIni, DataFim, ArqPrev);
       end;
  end;

  for I:= 1 to Indice do
    begin
      BNDO.strtochar(Di, Copy(VetPeriodosPrev[I-1].DataInicial, 1, 2));
      BNDO.strtochar(Mi, Copy(VetPeriodosPrev[I-1].DataInicial, 4, 2));
      BNDO.strtochar(Ai, Copy(VetPeriodosPrev[I-1].DataInicial, 7, 4));
      BNDO.strtochar(Df, Copy(VetPeriodosPrev[I-1].DataFinal, 1, 2));
      BNDO.strtochar(Mf, Copy(VetPeriodosPrev[I-1].DataFinal, 4, 2));
      BNDO.strtochar(Af, Copy(VetPeriodosPrev[I-1].DataFinal, 7, 4));
      case (TipoSaida) of
        1: begin
             ArqAux:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVHORCOL' + IntToStr(I) + '.tmp';
             if (FileExists(ArqAux)) then
               SysUtils.DeleteFile(ArqAux);
             BNDO.strtochar(Tipo, '1');
             BNDO.strtochar(ArqPrevDll, ArqAux);
             U_Dll.PREVISAO_ALTURAS_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op);
             CRotinasGenericas.ConcatenaArquivos(ArqPrevAux, ArqPrevDll, 1, True);
           end;
        2: begin
             ArqAux:= getArquivoBase( Diretorio, CodEstacao, CodAnaliseArq, DataIniArq, DataFimArq) + 'PREVMAXMINCOL' + IntToStr(I) + '.tmp';
             if (FileExists(ArqAux)) then
               SysUtils.DeleteFile(ArqAux);
             BNDO.strtochar(Tipo, '2');
             BNDO.strtochar(ArqPrevDll, ArqAux);
             U_Dll.PREVISAO_MAXMIN_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op, Sim);
             CRotinasGenericas.ConcatenaArquivos(ArqPrevAux, ArqPrevDll, 2, True);
           end;
      end;
    end;
  if (DeletaConst) then
    SysUtils.DeleteFile(ArqConst);
  FreeAndNil(CRotinasGenericas);
  AssignFile(TArqPrevSaida, ArqPrev);
  AssignFile(TArqPrev, ArqPrevAux);
  if (FileExists(ArqPrev)) then
    Append(TArqPrevSaida)
  else
    Rewrite(TArqPrevSaida);
  Reset(TArqPrev);
  while not(Eof(TArqPrev)) do
    begin
      case (TipoSaida) of
        1: Readln(TArqPrev, DataPrev, AlturaPrev, HoraPrev);
        2: Readln(TArqPrev, DataPrev, HoraPrev, AlturaPrev);
      end;
      if ((StrToDate(Trim(DataPrev)) >= DataIni) and (StrToDate(Trim(DataPrev)) <= DataFim)) then
        Writeln(TArqPrevSaida, Trim(DataPrev) + ' ' + Trim(HoraPrev) + AlturaPrev);
    end;
  CloseFile(TArqPrevSaida);
  CloseFile(TArqPrev);
  SysUtils.DeleteFile(ArqPrevAux);
end;




// *********************************************************************
// *********************************************************************



procedure TClassePrevisaoSis.previsaoColunas(vDataInicial, vDataFinal: TDateTime; Z0: Real; ArquivoConst, ArquivoPrev: string; TipoSaida : Integer);
{
  1 - Previsão Horária;
  2 - Previsão Máximas e Mínimas.
}
var
  ArqConstDll, ArqPrevDll: array [1..150] of Char;
  Tipo, Op, Sim: array [1..1] of Char;
  Di, Mi, Df, Mf: array [1..2] of Char;
  Ai, Af: array [1..4] of Char;
  Nivel: array [1..10] of Char;
  TArqPrev, TArqPrevSaida: TextFile;
  ArqConst, ArqPrev, ArqPrevAux, ArqAux: string;
  AnoIni, AnoFim, AnoAux: string[4];
  VetPeriodosPrev: array of RegPeriodosPrev;
  Indice, I, DiaAux: Shortint;
  DataPrev: string[10];
  HoraPrev: string[6];
  AlturaPrev: string[5];
begin
  AnoIni:= FormatDateTime('yyyy', vDataInicial);
  AnoAux:= FormatDateTime('yyyy', vDataInicial);
  AnoFim:= FormatDateTime('yyyy', vDataFinal);

  Indice := 1;

  SetLength(VetPeriodosPrev, Indice);
  VetPeriodosPrev[Indice-1].DataInicial:= '01/' + FormatDateTime('mm/yyyy', vDataInicial);
  while (AnoAux <> AnoFim) do
    begin
      VetPeriodosPrev[Indice-1].DataFinal:= '31/12/' + AnoAux;
      Indice:= Indice + 1;
      SetLength(VetPeriodosPrev, Indice);
      AnoAux:= IntToStr(StrToInt(AnoAux) + 1);
      VetPeriodosPrev[Indice-1].DataInicial:= '01/01/' + AnoAux;
    end;
  DiaAux:= 31;
  VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', vDataFinal);
  while not (CRotGen.ValidaDataString(VetPeriodosPrev[Indice-1].DataFinal)) do
    begin
      DiaAux:= DiaAux - 1;
      VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', vDataFinal);
    end;
  ArqConst:= ArquivoConst;
  BNDO.strtochar(ArqConstDll, ArqConst);
  BNDO.strtochar(Nivel, FormatFloat('0000000.00', Z0));
  BNDO.strtochar(Op, 'N');
  BNDO.strtochar(Sim, 'S');
  ArqPrev:= ArquivoPrev;
  ArqPrevAux:= ArquivoPrev + '.tmp';
  if (FileExists(ArqPrev)) then
    SysUtils.DeleteFile(ArqPrev);
  if (FileExists(ArqPrevAux)) then
    SysUtils.DeleteFile(ArqPrevAux);
  for I:= 1 to Indice do
    begin
      BNDO.strtochar(Di, Copy(VetPeriodosPrev[I-1].DataInicial, 1, 2));
      BNDO.strtochar(Mi, Copy(VetPeriodosPrev[I-1].DataInicial, 4, 2));
      BNDO.strtochar(Ai, Copy(VetPeriodosPrev[I-1].DataInicial, 7, 4));
      BNDO.strtochar(Df, Copy(VetPeriodosPrev[I-1].DataFinal, 1, 2));
      BNDO.strtochar(Mf, Copy(VetPeriodosPrev[I-1].DataFinal, 4, 2));
      BNDO.strtochar(Af, Copy(VetPeriodosPrev[I-1].DataFinal, 7, 4));
      ArqAux:= 'PREV' + IntToStr(I) + '.tmp';
      if (FileExists(ArqAux)) then
        SysUtils.DeleteFile(ArqAux);
      BNDO.strtochar(Tipo, '1');
      BNDO.strtochar(ArqPrevDll, ArqAux);

      case (TipoSaida) of
        1: begin
            U_Dll.PREVISAO_ALTURAS_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op);
            CRotGen.ConcatenaArquivos(ArqPrevAux, ArqPrevDll, 1, false);
          end;
        2 : begin
            U_Dll.PREVISAO_MAXMIN_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op, Sim);
            CRotGen.ConcatenaArquivos(ArqPrevAux, ArqPrevDll, 2, false);
            end;
       end;

    end;
  AssignFile(TArqPrevSaida, ArqPrev);
  AssignFile(TArqPrev, ArqPrevAux);
  if (FileExists(ArqPrev)) then
    Append(TArqPrevSaida)
  else
    Rewrite(TArqPrevSaida);
  Reset(TArqPrev);
  while not(Eof(TArqPrev)) do
    begin
      case (TipoSaida) of
        1: begin
             Readln(TArqPrev, DataPrev, AlturaPrev, HoraPrev);
           end;
        2: begin
             Readln(TArqPrev, DataPrev, HoraPrev, AlturaPrev);
           end;
      end;

      if ((StrToDate(Trim(DataPrev)) >= vDataInicial) and (StrToDate(Trim(DataPrev)) <= vDataFinal)) then
            Writeln(TArqPrevSaida, Trim(DataPrev) + ' ' + Trim(HoraPrev) + AlturaPrev);
    end;
  CloseFile(TArqPrevSaida);
  CloseFile(TArqPrev);
  SysUtils.DeleteFile(ArqPrevAux);
end;


procedure TClassePrevisaoSis.GeraCabecalhoPrevisaoHoraria(Conexao: TADOConnection; Area: MinInteiroSs; CodAnalise: Integer;
  DataIni, DataFim: TDate; Arquivo: string);
var
  Arq: TextFile;
  Query: TADOQuery;
begin
  AssignFile(Arq, Trim(Arquivo));
  if (FileExists(Arquivo)) then
    Append(Arq)
  else
    Rewrite(Arq);
  Query:= TADOQuery.Create(nil);
  Query.Connection:= Conexao;
  Query.SQL.Clear;
  Query.SQL.Add('select em.num_estacao_maregrafica, em.nome_estacao_maregrafica, em.latitude, em.longitude, em.fuso, am.* ');
  Query.SQL.Add('from estacao_maregrafica em, (select cod_estacao_maregrafica, data_hora_inicio, data_hora_fim, num_componentes, z0 ');
  case (Area) of
    1: Query.SQL.Add('from vw_analise_mares ');
    2: Query.SQL.Add('from vw_analise_mares_temp ');
  else
    Query.SQL.Add('from vw_analise_mares ');
  end;
  Query.SQL.Add('where cod_analise_mares = ' + IntToStr(CodAnalise) + ') am where em.cod_estacao_maregrafica = am.cod_estacao_maregrafica');
  Query.Open;
  Writeln(Arq, 'PREVISÃO HORÁRIA - ' + FormatDateTime('dd/mm/yyyy', DataIni) + ' A ' + FormatDateTime('dd/mm/yyyy', DataFim));
  Writeln(Arq, 'ESTAÇÃO:' + FormatFloat('00000', StrToFloat(Query.FieldByName('num_estacao_maregrafica').AsString)));
  Writeln(Arq, 'NOME ESTAÇÃO:' + Query.FieldByName('nome_estacao_maregrafica').AsString);
  Writeln(Arq, 'LATITUDE:' + BNDO.FormataLatLonMareTela(Query.FieldByName('latitude').AsInteger, 'LT'));
  Writeln(Arq, 'LONGITUDE:' + BNDO.FormataLatLonMareTela(Query.FieldByName('longitude').AsInteger, 'LG'));
  Writeln(Arq, 'FUSO:' + BNDO.FormataFusoMareTela(Query.FieldByName('fuso').AsString));
  Writeln(Arq, 'PERÍODO DA ANÁLISE:' + FormatDateTime('dd/mm/yyyy', Query.FieldByName('data_hora_inicio').AsDateTime) +
    ' A ' + FormatDateTime('dd/mm/yyyy', Query.FieldByName('data_hora_fim').AsDateTime));
  Writeln(Arq, 'COMPONENTES:' + Query.FieldByName('num_componentes').AsString);
  Writeln(Arq, 'NÍVEL MÉDIO:' + Query.FieldByName('z0').AsString + ' CENTÍMETROS');
  Writeln(Arq, 'ALTURAS EM CENTÍMETROS');
  Writeln(Arq, 'ALTURAS ACIMA DO NÍVEL DE REDUÇÃO (NR) LOCAL'); //Alteração solicitada pelo Intercâmbio - Jorge Carvalho
  Writeln(Arq, 'ESTA PREVISÃO É BASEADA EM CONSTANTES HARMÔNICAS OBTIDAS COM O PERÍODO DE ANÁLISE SUPRACITADO'); //Alteração solicitada pelo Intercâmbio - Jorge Carvalho
  Writeln(Arq, 'HORÁRIO PAPA');
  Writeln(Arq, '#');
  Query.Close;
  FreeAndNil(Query);
  CloseFile(Arq);
end;


procedure TClassePrevisaoSis.previsaoColunasGeral(DataIni, DataFim: TDate; Arquivo: string;
  Z0: Real; TipoSaida: MinInteiroSs; Cabecalho, DeletaConst: Boolean);
{
  1 - Previsão Horária;
  2 - Previsão Máximas e Mínimas.
}
var
  ArqConstDll, ArqPrevDll: array [1..150] of Char;
  Tipo, Op, Sim: array [1..1] of Char;
  Di, Mi, Df, Mf: array [1..2] of Char;
  Ai, Af: array [1..4] of Char;
  Nivel: array [1..10] of Char;
  TArqPrev, TArqPrevSaida: TextFile;
  ArqConst, ArqPrev, ArqPrevAux: string;
  NumEstacao, DataIniArq, DataFimArq: string;
  AnoIni, AnoFim, AnoAux: string[4];
  VetPeriodosPrev: array of RegPeriodosPrev;
  Indice, I, DiaAux: Shortint;
  DataPrev: string[10];
  HoraPrev: string[6];
  AlturaPrev: string[5];
  CRotinasGenericas: TClasseRotinasGenericas;
begin
  {CRotinasGenericas:= TClasseRotinasGenericas.Create;
  AnoIni:= FormatDateTime('yyyy', DataIni);
  AnoAux:= FormatDateTime('yyyy', DataIni);
  AnoFim:= FormatDateTime('yyyy', DataFim);
  Indice:= 1;
  SetLength(VetPeriodosPrev, Indice);
  VetPeriodosPrev[Indice-1].DataInicial:= '01/' + FormatDateTime('mm/yyyy', DataIni);
  while (AnoAux <> AnoFim) do
    begin
      VetPeriodosPrev[Indice-1].DataFinal:= '31/12/' + AnoAux;
      Indice:= Indice + 1;
      SetLength(VetPeriodosPrev, Indice);
      AnoAux:= IntToStr(StrToInt(AnoAux) + 1);
      VetPeriodosPrev[Indice-1].DataInicial:= '01/01/' + AnoAux;
    end;
  DiaAux:= 31;
  VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', DataFim);
  while not (CRotinasGenericas.ValidaDataString(VetPeriodosPrev[Indice-1].DataFinal)) do
    begin
      DiaAux:= DiaAux - 1;
      VetPeriodosPrev[Indice-1].DataFinal:= FormatFloat('00', DiaAux) + '/' + FormatDateTime('mm/yyyy', DataFim);
    end;
  NumEstacao:= FormatFloat('00000', CRotinasGenericas.RetornaNumeroEstacao(DtMod.BD, CodEstacao));
  DataIniArq:= BNDO.converte_data_arq(FormatDateTime('dd/mm/yyyy', DataIni));
  DataFimArq:= BNDO.converte_data_arq(FormatDateTime('dd/mm/yyyy', DataFim));
  ArqConst:= ExtractFilePath(Arquivo) + NumEstacao + DataIniArq + DataFimArq + 'CH.txt';
  if (Origem in[2,3]) then
    geraArquivoConstantesPacDll(ArqConst)
  else
    geraArquivoConstantesSisDll(ArqConst);
  BNDO.strtochar(ArqConstDll, ArqConst);
  BNDO.strtochar(Nivel, FormatFloat('0000000.00', Z0));
  BNDO.strtochar(Op, 'N');
  BNDO.strtochar(Sim, 'S');
  ArqPrev:= Arquivo + '.tmp';
  if (FileExists(Arquivo)) then
    SysUtils.DeleteFile(Arquivo);
  if (FileExists(ArqPrev)) then
    SysUtils.DeleteFile(ArqPrev);
  if (Cabecalho) then
    begin
      //GeraCabecalhoPrevisaoHoraria(Conexao, Area, CodAnalise, DataIni, DataFim, ArqPrev);
    end;
  for I:= 1 to Indice do
    begin
      BNDO.strtochar(Di, Copy(VetPeriodosPrev[I-1].DataInicial, 1, 2));
      BNDO.strtochar(Mi, Copy(VetPeriodosPrev[I-1].DataInicial, 4, 2));
      BNDO.strtochar(Ai, Copy(VetPeriodosPrev[I-1].DataInicial, 7, 4));
      BNDO.strtochar(Df, Copy(VetPeriodosPrev[I-1].DataFinal, 1, 2));
      BNDO.strtochar(Mf, Copy(VetPeriodosPrev[I-1].DataFinal, 4, 2));
      BNDO.strtochar(Af, Copy(VetPeriodosPrev[I-1].DataFinal, 7, 4));
      ArqPrevAux:= ExtractFilePath(Arquivo) + NumEstacao + DataIniArq + DataFimArq + IntToStr(I) + '.tmp';
      if (FileExists(ArqPrevAux)) then
        SysUtils.DeleteFile(ArqPrevAux);
      case (TipoSaida) of
        1: begin
             BNDO.strtochar(Tipo, '1');
             BNDO.strtochar(ArqPrevDll, ArqPrevAux);
             U_Dll.PREVISAO_ALTURAS_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op);
             CRotinasGenericas.ConcatenaArquivos(ArqPrev, ArqPrevDll, 1, True);
           end;
        2: begin
             BNDO.strtochar(Tipo, '2');
             BNDO.strtochar(ArqPrevDll, ArqPrevAux);
             U_Dll.PREVISAO_MAXMIN_EXCEL(ArqConstDll, ArqPrevDll, Tipo, Di, Mi, Ai, Df, Mf, Af, Nivel, Op, Sim);
             CRotinasGenericas.ConcatenaArquivos(ArqPrev, ArqPrevDll, 2, True);
           end;
      end;
    end;
  if (DeletaConst) then
    SysUtils.DeleteFile(ArqConst);
  FreeAndNil(CRotinasGenericas);
  AssignFile(TArqPrevSaida, Arquivo);
  AssignFile(TArqPrev, ArqPrev);
  if (FileExists(Arquivo)) then
    Append(TArqPrevSaida)
  else
    Rewrite(TArqPrevSaida);
  Reset(TArqPrev);
  while not(Eof(TArqPrev)) do
    begin
      case (TipoSaida) of
        1: Readln(TArqPrev, DataPrev, AlturaPrev, HoraPrev);
        2: Readln(TArqPrev, DataPrev, HoraPrev, AlturaPrev);
      end;
      if ((StrToDate(Trim(DataPrev)) >= DataIni) and (StrToDate(Trim(DataPrev)) <= DataFim)) then
        Writeln(TArqPrevSaida, Trim(DataPrev) + ' ' + Trim(HoraPrev) + AlturaPrev);
    end;
  CloseFile(TArqPrevSaida);
  CloseFile(TArqPrev);
  SysUtils.DeleteFile(ArqPrev);}
end;

end.
