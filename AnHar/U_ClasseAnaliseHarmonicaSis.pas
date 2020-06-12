unit U_ClasseAnaliseHarmonicaSis;

interface

uses SysUtils, BNDO, Classes, DB, ADODB,
  U_Dll, U_Tipos, BbtFuncoes, Controls;

type
  TTipoAnalise = (tAh, tNmSis);
  TFiltroNm = (f24, f25);

type
  TClasseAnaliseHarmonicaSis = class
  private
    NumEstacao: string;
    NomeEstacao: string;
    Latitude: string;
    Longitude: string;
    DataInicial: TDate;
    DataFinal: TDate;
    Fuso: string;
    Z0: Real;
    function retornaZ0Reducao(ArqReducao: string): Real;
    procedure formataCostantes(ArquivoConst: string);
    function retornaLinhaComponente(Nome, H, G: string): string;
  public
    constructor create;
    procedure geraArqAlturasDll(Arquivo: string);
    procedure analiseHarmonica(TipoAnalise: TTipoAnalise;
      ArquivoAlt, ArquivoAh, ArquivoConst: string);
  end;

implementation

constructor TClasseAnaliseHarmonicaSis.create;
begin
//
end;

procedure TClasseAnaliseHarmonicaSis.analiseHarmonica(TipoAnalise: TTipoAnalise; ArquivoAlt, ArquivoAh, ArquivoConst: string);
var
  ArqConstantes, ArqReducao, ArqRelatorio: string;
  ArqAlt, ArqConst, ArqReduc, ArqRelat, Arq13Ciclos, Arq9Ciclos: array [1..150] of Char;
  Klm1, Cons, Niveis, Del: array [1..1] of Char;
  DataImp: array [1..10] of Char;
begin
  geraArqAlturasDll(ArquivoAlt);
  ArqConstantes:= ArquivoConst;
  ArqReducao:= 'ReducaoSondagem.tmp';
  ArqRelatorio:= ArquivoAh;
  if (FileExists(ArqRelatorio)) then
    DeleteFile(ArqRelatorio);
  if (FileExists(ArqConstantes)) then
    DeleteFile(ArqConstantes);
  if (FileExists(ArqReducao)) then
    DeleteFile(ArqReducao);
  BNDO.strtochar(ArqAlt, ArquivoAlt + '.tmp');
  BNDO.strtochar(ArqConst, ArqConstantes);
  BNDO.strtochar(ArqReduc, ArqReducao);
  BNDO.strtochar(ArqRelat, ArqRelatorio);
  BNDO.strtochar(Arq13Ciclos, 'tab_13_ciclos.dat');
  BNDO.strtochar(Arq9Ciclos, 'tab_9_ciclos.dat');
  case (TipoAnalise) of
    tAh: BNDO.strtochar(Klm1, '0');
    tNmSis: BNDO.strtochar(Klm1, '2');
  end;
  BNDO.strtochar(Cons, 'S');
  BNDO.strtochar(Niveis, 'S');
  BNDO.strtochar(Del, 'N');
  BNDO.strtochar(DataImp, FormatDateTime('dd/mm/yyyy', Now));
  U_Dll.ANALISE(ArqAlt, ArqConst, ArqReduc, ArqRelat, Arq13Ciclos, Arq9Ciclos, Klm1, Cons, Niveis, Del, DataImp);
  //DeleteFile(ArqAlt);
  //DeleteFile(ArqConstantes);
  Z0:= retornaZ0Reducao(ArqReducao);
  DeleteFile(ArquivoAlt + '.tmp');
  DeleteFile(ArqReducao);
  formataCostantes(ArquivoConst);
end;

function TClasseAnaliseHarmonicaSis.retornaZ0Reducao(ArqReducao: string): Real;
var
  ArqReduc: TextFile;
  NumEstacao: string[5];
  DataIni, DataFim: string[8];
  NivelMedio, NivelClassif, Z0, NivelReduc: string[7];
  PreSizi, PreQuadrat, BaixaSizi, BaixaQuadrat: string[7];
  Classif: string[21];
  Hora, Minuto: string[2];
begin
  AssignFile(ArqReduc, ArqReducao);
  Reset(ArqReduc);
  Readln(ArqReduc, NumEstacao, DataIni, DataFim, NivelMedio, NivelClassif, Classif,
    Z0, NivelReduc, Hora, Minuto, PreSizi, PreQuadrat, BaixaSizi, BaixaQuadrat);
  CloseFile(ArqReduc);
  NivelMedio:= Trim(NivelMedio);
  NivelClassif:= Trim(NivelClassif);
  Z0:= Trim(Z0);
  NivelReduc:= Trim(NivelReduc);
  PreSizi:= Trim(PreSizi);
  PreQuadrat:= Trim(PreQuadrat);
  BaixaSizi:= Trim(BaixaSizi);
  BaixaQuadrat:= Trim(BaixaQuadrat);
  Classif:= Trim(Classif);
  Hora:= Trim(Hora);
  Minuto:= Trim(Minuto);
  Result:= StrToFloat(Trim(Z0));
end;

procedure TClasseAnaliseHarmonicaSis.geraArqAlturasDll(Arquivo: string);
var
  ArqEntrada, ArqSaida: TextFile;
  I, Altura: Integer;
  DataIni, DataFim, Linha: string;
begin
  AssignFile(ArqEntrada, Arquivo);
  Reset(ArqEntrada);
  Readln(ArqEntrada, Linha);
  NumEstacao:= FormatFloat('00000', StrToFloat(BbtFuncoes.retornaParametroPosString(Linha, 2, ':')));
  Readln(ArqEntrada, Linha);
  NomeEstacao:= BNDO.formata_string(BbtFuncoes.retornaParametroPosString(Linha, 2, ':'), 38);
  Readln(ArqEntrada, Linha);
  Latitude:= BNDO.formata_latlon_mare(StrToInt(BbtFuncoes.retornaParametroPosString(Linha, 2, ':')), 'LT');
  Readln(ArqEntrada, Linha);
  Longitude:= BNDO.formata_latlon_mare(StrToInt(BbtFuncoes.retornaParametroPosString(Linha, 2, ':')), 'LG');
  Readln(ArqEntrada, Linha);
  DataInicial:= StrToDate(BbtFuncoes.retornaParametroPosString(Linha, 2, ':'));
  DataIni:= FormatDateTime('ddmmyyyy', DataInicial);
  Readln(ArqEntrada, Linha);
  DataFinal:= StrToDate(BbtFuncoes.retornaParametroPosString(Linha, 2, ':'));
  DataFim:= FormatDateTime('ddmmyyyy', DataFinal);
  Readln(ArqEntrada, Linha);
  Fuso:= BbtFuncoes.retornaParametroPosString(Linha, 2, ':');
  AssignFile(ArqSaida, Arquivo + '.tmp');
  Rewrite(ArqSaida);
  Writeln(ArqSaida, NumEstacao, DataIni, DataFim, NomeEstacao, Latitude, Longitude, Fuso);
  I:= 1;
  while not (Eof(ArqEntrada)) do
    begin
      Readln(ArqEntrada, Linha);
      if (I > 24) then
        begin
          I:= 1;
          Writeln(ArqSaida);
        end;
      Altura:= StrToInt(BbtFuncoes.retornaParametroPosString(Linha, 2, ';'));
      if (Altura >= 0) then
        Write(ArqSaida, FormatFloat('0000', Altura))
      else
        Write(ArqSaida, FormatFloat('000', Altura));
      I:= I + 1;
    end;
  Writeln(ArqSaida);
  CloseFile(ArqEntrada);
  CloseFile(ArqSaida);
end;

procedure TClasseAnaliseHarmonicaSis.formataCostantes(ArquivoConst: string);
var
  ArqConst, ArqSaida: TextFile;
  QtdConstantes: Integer;
  Nome: string[7];
  Veloc: string[11];
  H, G: string[8];
  LinhaCompo: string;
begin
  QtdConstantes:= BbtFuncoes.contaLinhasArquivo(ArquivoConst);
  AssignFile(ArqConst, ArquivoConst);
  AssignFile(ArqSaida, ArquivoConst + '.tmp');
  Reset(ArqConst);
  Rewrite(ArqSaida);
  Write(ArqSaida, FormatFloat('000', QtdConstantes));
  Write(ArqSaida, FormatFloat('0000000.00', Z0));
  Write(ArqSaida, FormatDateTime('ddmmyyyy', DataInicial));
  Write(ArqSaida, FormatDateTime('ddmmyyyy', DataFinal));
  Write(ArqSaida, NumEstacao);
  Write(ArqSaida, NomeEstacao);
  Write(ArqSaida, Latitude);
  Write(ArqSaida, Longitude);
  Writeln(ArqSaida, Fuso);
  while not (Eof(ArqConst)) do
    begin
      Readln(ArqConst, Nome, Veloc, H, G);
      LinhaCompo:= retornaLinhaComponente(Nome, H, G);
      if (LinhaCompo <> '') then
        Writeln(ArqSaida, LinhaCompo);
    end;
  CloseFile(ArqSaida);
  CloseFile(ArqConst);
  DeleteFile(ArquivoConst);
  RenameFile(ArquivoConst + '.tmp', ArquivoConst);
end;

function TClasseAnaliseHarmonicaSis.retornaLinhaComponente(Nome, H, G: string): string;
var
  ArqCompo: TextFile;
  Linha, LinhaSaida: string;
  Tipo: string[1];
  Nm: string[7];
  Veloc: string[11];
  Indice: array [1..14] of string[3];
  I: Integer;
  Ok: Boolean;
begin
  Ok:= False;
  AssignFile(ArqCompo, 'const.csv');
  Reset(ArqCompo);
  while ((not Eof(ArqCompo)) and (not Ok)) do
    begin
      Readln(ArqCompo, Linha);
      Tipo:= BbtFuncoes.retornaParametroPosString(Linha, 2, ';');
      Nm:= BbtFuncoes.retornaParametroPosString(Linha, 3, ';');
      Veloc:= BbtFuncoes.retornaParametroPosString(Linha, 4, ';');
      for I:= 1 to 14 do
        Indice[I]:= BbtFuncoes.retornaParametroPosString(Linha, (I+4), ';');
      if (Trim(Nome) = Trim(Nm)) then
        begin
          LinhaSaida:= Tipo;
          LinhaSaida:= LinhaSaida + BbtFuncoes.geraEspacosString(Nm, 7, True);
          LinhaSaida:= LinhaSaida + BbtFuncoes.geraEspacosString(Veloc, 11, False);
          for I:= 1 to 14 do
            LinhaSaida:= LinhaSaida + BbtFuncoes.geraEspacosString(Indice[I], 2, False);
          LinhaSaida:= LinhaSaida + BbtFuncoes.geraEspacosString(H, 8, False);
          LinhaSaida:= LinhaSaida + BbtFuncoes.geraEspacosString(G, 8, False);
          Ok:= True;
        end;
    end;
  CloseFile(ArqCompo);
  if (Ok) then
    Result:= LinhaSaida
  else
    Result:= '';
end;

end.
