{
Biblioteca de funções comuns usadas nos sistemas do BNDO.
Última modificação: 18/09/2008 por Carlos Henrique.
}

unit BNDO;

interface

uses
  SysUtils, Controls, Math, Classes;

function converte_data_mysql(data: string): string;
procedure strtochar(var arr: array of char; str: string);
function desmembra_latlon(latlon: integer; var gr, mn, seg, sinal: string;
  tipo: string): boolean;
function desmembra_latlon_mare(latlon: integer; var gr, mn, decseg, sinal: string;
  tipo: string): boolean;
function desmembra_latlon_meteo(lat, long: integer; var gr_lat, decmn_lat, sinal_lat,
  gr_long, decmn_long, sinal_long, quadr: string): boolean;
function retorna_nome_arquivo(arquivo: string): string;
function retorna_diretorio_arquivo(arquivo: string): string;
function verifica_extensao_arquivo(arquivo, extensao: string): boolean;
function substitui_caracter(texto: string; procura, substitui: char): string;
function correlacao(x, y: array of real; tam_matrizes: integer): real;
function retorna_num_linhas(arquivo: string): integer;
function formata_string(St: string; TamTotal: Integer): string;
function fator(dd, mm, aa: integer): integer;
function formata_latlon_mare(latlon: integer; tipo: string): string;
function valida_inteiro(num: string): boolean;
function calcula_dias(data_ini, data_fim: tdate): integer;
function valida_data_hora(data_hora: string): boolean;
function valida_real(num: string; separador_decimal: char): boolean;
function converte_data_br(data: string): string;
function converte_data_arq(data: string): string;
function Quadrante(Latitude, Longitude: integer): integer;
function QuadradoMarsden10(Latitude, Longitude: integer): integer;
function SubQuadradoMarsden01(Latitude, Longitude: integer): integer;
function NumeroValido(Numero: string): boolean;
function ConverteDataAnsi(Data: string): string;
function ConverteDataBr(Data: string): string;
function GeraEspacos(Tam: integer): string;
function FormataLatLonMareTela(LatLon: integer; Tipo: string): string;
procedure FormataFusoTela(Fuso: string; var Hora, Minuto, Sinal: string);
function FormataFusoMareTela(Fuso: string): string;
procedure ListarArquivos(Diretorio: string; Sub: Boolean; var Lista: TStrings);
function TemAtributo(Attr, Val: Integer): Boolean;
function Cria_LatLong_Oce(latitude,longitude:string; var graulat,minlat,seglat,graulong,minlong,
  seglong,sinal_lat,sinal_long:string) : boolean;
function IdentaDataHora(DataHora: TDateTime; QtdMinutos: Byte): TDateTime;
function RetornaQtdLinhasArquivo(Arquivo: string): Integer;
procedure ConcatenaArquivos(ArquivoConcatenado, ArquivoDados: string);

implementation

function converte_data_mysql(data: string): string;
{Função para converter uma data no formato dd/mm/yyyy para formato
padrão MySql (yyyy-mm-dd). Em caso de data inválida retorna '0'.}
var
  tam, i: integer;
begin
  try
    tam:= length(data);
    for i:= 1 to tam do
      begin
        if not (data[i] in['0'..'9', '/']) then
          begin
            converte_data_mysql:= '0';
            exit;
          end;
      end;
    strtodate(data);
    converte_data_mysql:= copy(data, 7, 4) + '-' + copy(data, 4, 2) + '-' + copy(data, 1, 2);
  except
    converte_data_mysql:= '0';
  end;
end;

procedure strtochar(var arr: array of char; str: string);
{Procedimento que transfere uma string para um vetor de caracteres.
Se o tamanho da string for maior do que o tamanho do vetor os
caracteres excedentes serão ignorados e se for menor o vetor será
completado com espaços em branco.}
var
  tam_arr, tam_str, i: integer;
begin
  tam_arr:= length(arr);
  tam_str:= length(str);
  for i:= 1 to tam_arr do
    begin
      if (i <= tam_str) then
        arr[i-1]:= str[i]
      else
        arr[i-1]:= ' ';
    end;
end;

function desmembra_latlon(latlon: integer; var gr, mn, seg, sinal: string;
  tipo: string): boolean;
{Função que recebe uma latitude ou longitude no formato GGMMSS ou
GGGMMSS e desmembra os valores nas variáveis passadas por referência (gr - Grau,
mn - Minuto, seg - Segundo e sinal - Sinal da Coordenada. Em caso de
latitude ou longitude inválida retorna 'false'. O parâmetro tipo deve ser 'LT' para
latitude ou 'LG' para longitude.'}
var
  latlon_aux, gr_aux, mn_aux, seg_aux, sinal_aux: string;
  tam, i: integer;
begin
  if ((tipo = 'LT') or (tipo = 'LG')) then
    begin
      gr_aux:= '';
      mn_aux:= '';
      seg_aux:= '';
      if (latlon < 0) then
        begin
          if (tipo = 'LT') then
            sinal_aux:= 'S'
          else
            sinal_aux:= 'W';
        end
      else
        begin
          if (tipo = 'LT') then
            sinal_aux:= 'N'
          else
            sinal_aux:= 'E';
        end;
      latlon_aux:= inttostr(abs(latlon));
      tam:= length(latlon_aux);
      for i:= tam downto 1 do
        begin
          if (length(seg_aux) < 2) then
            seg_aux:= latlon_aux[i] + seg_aux
          else
            if (length(mn_aux) < 2) then
              mn_aux:= latlon_aux[i] + mn_aux
            else
              gr_aux:= latlon_aux[i] + gr_aux;
        end;
      if (gr_aux = '') then
        gr_aux:= '0';
      if (mn_aux = '') then
        mn_aux:= '0';
      if ((tipo = 'LT') and ((strtoint(gr_aux) > 90) or (strtoint(mn_aux) > 59) or (strtoint(seg_aux) > 59))) then
        begin
          desmembra_latlon:= false;
          exit;
        end;
      if ((tipo = 'LG') and ((strtoint(gr_aux) > 180) or (strtoint(mn_aux) > 59) or (strtoint(seg_aux) > 59))) then
        begin
          desmembra_latlon:= false;
          exit;
        end;
      if (tipo = 'LT') then
        gr:= formatfloat('00', strtofloat(gr_aux))
      else
        gr:= formatfloat('000', strtofloat(gr_aux));
      mn:= formatfloat('00', strtofloat(mn_aux));
      seg:= formatfloat('00', strtofloat(seg_aux));
      sinal:= sinal_aux;
      desmembra_latlon:= true;
    end
  else
    desmembra_latlon:= false;
end;

function desmembra_latlon_mare(latlon: integer; var gr, mn, decseg, sinal: string;
  tipo: string): boolean;
{Função que recebe uma latitude ou longitude no formato GGGMMSS ou
GGMMSS e desmembra os valores nas variáveis passadas por referência (gr - Grau,
mn - Minuto, decseg - Décimo de Segundo e sinal - Sinal da coordenada. Em caso de
latitude ou longitude inválida retorna false. O parâmetro tipo deve ser 'LT para
latitude ou 'LG' para longitude.'}
var
  gr_aux, mn_aux, seg, decseg_aux, sinal_aux: string;
begin
  if (desmembra_latlon(latlon, gr_aux, mn_aux, seg, sinal_aux, tipo) = false) then
    begin
      desmembra_latlon_mare:= false;
      exit;
    end;
  decseg_aux:= floattostr(frac(strtoint(seg) /60));
  if (length(decseg_aux) > 1) then
    decseg_aux:= copy(decseg_aux, 3, 1);
  gr:= gr_aux;
  mn:= mn_aux;
  decseg:= decseg_aux;
  sinal:= sinal_aux;
  desmembra_latlon_mare:= true;
end;

function desmembra_latlon_meteo(lat, long: integer; var gr_lat, decmn_lat, sinal_lat,
  gr_long, decmn_long, sinal_long, quadr: string): boolean;
{Função que recebe uma latitude e uma longitude no formato GGGMMSS ou
GGMMSS e desmembra os valores nas respectivas variáveis passadas por referência
(gr_lat e gr_long - Grau, decmn_lat e decmn_long - Décimo de Minuto, sinal_lat e sinal_long -
Sinal da coordenada e quadr - Quadrante das coordenadas. Em caso de latitude e/ou longitude
inválida(s) a função retorna false. O parâmetro tipo deve ser 'LT' para latitude ou 'LG'
para longitude.'}
var
  gr_lat_aux, mn_lat_aux, seg_lat_aux, sinal_lat_aux, decmn_lat_aux: string;
  gr_long_aux, mn_long_aux, seg_long_aux, sinal_long_aux, decmn_long_aux: string;
  quadr_aux: string;
begin
  if (desmembra_latlon(lat, gr_lat_aux, mn_lat_aux, seg_lat_aux, sinal_lat_aux, 'LT') = false) then
    begin
      desmembra_latlon_meteo:= false;
      exit;
    end;
  if (desmembra_latlon(long, gr_long_aux, mn_long_aux, seg_long_aux, sinal_long_aux, 'LG') = false) then
    begin
      desmembra_latlon_meteo:= false;
      exit;
    end;
  if ((lat >= 0) and (long >= 0)) then
    quadr_aux:= '1';
  if ((lat >= 0) and (long < 0)) then
    quadr_aux:= '7';
  if ((lat < 0) and (long >= 0)) then
    quadr_aux:= '3';
  if ((lat < 0) and (long < 0)) then
    quadr_aux:= '5';
  decmn_lat_aux:= floattostr(frac(strtoint(mn_lat_aux) /60));
  decmn_long_aux:= floattostr(frac(strtoint(mn_long_aux) /60));
  if (length(decmn_lat_aux) > 1) then
    decmn_lat_aux:= copy(decmn_lat_aux, 3, 1);
  if (length(decmn_long_aux) > 1) then
    decmn_long_aux:= copy(decmn_long_aux, 3, 1);
  gr_lat:= gr_lat_aux;
  decmn_lat:= decmn_lat_aux;
  sinal_lat:= sinal_lat_aux;
  gr_long:= gr_long_aux;
  decmn_long:= decmn_long_aux;
  sinal_long:= sinal_long_aux;
  quadr:= quadr_aux;
  desmembra_latlon_meteo:= true;
end;

function retorna_nome_arquivo(arquivo: string): string;
{Função que retorna o nome de arquivo do caminho
completo de um arquivo.}
var
  i, cont_char: integer;
  nome_arq: string;
begin
  nome_arq:= '';
  cont_char:= length(arquivo);
  while ((arquivo[cont_char] <> '\') and (cont_char > 0)) do
    cont_char:= cont_char - 1;
  if (cont_char >= 3) then
    begin
      for i:= cont_char + 1 to length(arquivo) do
        nome_arq:= nome_arq + arquivo[i];
    end;
  retorna_nome_arquivo:= nome_arq;
end;

function retorna_diretorio_arquivo(arquivo: string): string;
{Função que retorna o diretório do caminho completo de
um arquivo.}
var
  i, cont_char: integer;
  diretorio: string;
begin
  diretorio:= '';
  cont_char:= length(arquivo);
  while ((arquivo[cont_char] <> '\') and (cont_char > 0)) do
    cont_char:= cont_char - 1;
  if (cont_char >= 3) then
    begin
      for i:= cont_char downto 1 do
        diretorio:= arquivo[i] + diretorio;
    end;
  retorna_diretorio_arquivo:= diretorio;
end;

function verifica_extensao_arquivo(arquivo, extensao: string): boolean;
{Função que verifica se o caminho de um arquivo possui a extensão desejada.
Caso não possua retorna false.}
var
  i, cont_char: integer;
  extensao_aux: string;
begin
  extensao_aux:= '';
  cont_char:= length(arquivo);
  while ((arquivo[cont_char] <> '.') and (cont_char > 0)) do
    cont_char:= cont_char - 1;
  if (cont_char >= 3) then
    begin
      for i:= cont_char + 1 to length(arquivo) do
        extensao_aux:= extensao_aux + arquivo[i];
    end;
  if (uppercase(extensao_aux) = uppercase(extensao)) then
    verifica_extensao_arquivo:= true
  else
    verifica_extensao_arquivo:= false;
end;

function substitui_caracter(texto: string; procura, substitui: char): string;
{Função para substituir um caracter dentro de uma string. Substitui
todas as ocorrências do caracter alvo.}
var
  texto_aux: string;
  i, tam_texto: integer;
begin
  texto_aux:= '';
  tam_texto:= length(texto);
  for i:= 1 to tam_texto do
    begin
      if (texto[i] = procura) then
        texto_aux:= texto_aux + substitui
      else
        texto_aux:= texto_aux + texto[i];
    end;
  substitui_caracter:= texto_aux;
end;

function correlacao(x, y: array of real; tam_matrizes: integer): real;
{Função que retorna a correlação (Pearson) das matrizes informadas.
O número de elementos das matrizes devem ser idênticos e informados
no parâmetro tam_matrizes. Se todos os valores de uma ou ambas matrizes
forem iguais a zero ou o desvio padrão destes valores for igual a zero
a função retornará -99.}
var
  i: integer;
  coef, deno, deno1, deno2: real;
  media_x, media_y: real;
  tot_x, tot_y: real;
begin
  try
    coef:= 0;
    deno1:= 0;
    deno2:= 0;
    tot_x:= 0;
    tot_y:= 0;
    for i:= 1 to tam_matrizes do
      begin
        tot_x:= tot_x + x[i-1];
        tot_y:= tot_y + y[i-1];
      end;
    media_x:= tot_x / tam_matrizes;
    media_y:= tot_y / tam_matrizes;
    for i:= 1 to tam_matrizes do
      begin
        coef:= coef + ((x[i-1] - media_x) * (y[i-1] - media_y));
        deno1:= deno1 + (power((x[i-1] - media_x), 2));
        deno2:= deno2 + (power((y[i-1] - media_y), 2));
      end;
    deno:= sqrt(deno1) * sqrt(deno2);
    correlacao:= coef / deno;
  except
    correlacao:= -99;
  end;
end;

function retorna_num_linhas(arquivo: string): integer;
{Função que retorna a quantidade de linhas de um arquivo texto.}
var
  arq: textfile;
  cont_linhas: integer;
begin
  assignfile(arq, arquivo);
  cont_linhas:= 0;
  reset(arq);
  while not (eof(arq)) do
    begin
      readln(arq);
      cont_linhas:= cont_linhas + 1;
    end;
  closefile(arq);
  retorna_num_linhas:= cont_linhas;
end;

function formata_string(St: string; TamTotal: Integer): string;
{Função que preenche com espaços em branco à direita uma string.
Informar no parâmetro tam_format o tamanho total da string (com os
espaços em branco).}
var
  StAux: string;
  I, MaxSt: Integer;
begin
  StAux:= '';
  if (Length(St) <> 0) then
    begin
      if (Length(St) <= TamTotal) then
        MaxSt:= Length(St)
      else
        MaxSt:= TamTotal;
      for I:= 1 to MaxSt do
        StAux:= StAux + St[I];
    end;
  if (Length(StAux) < TamTotal) then
    begin
      for I:= (Length(StAux)+1) to TamTotal do
        StAux:= StAux + ' ';
    end;
  Result:= StAux;
end;

function fator(dd, mm, aa: integer): integer;
var
  d, m, a, x1, x2: real;
begin
  d:= strtofloat(inttostr(dd));
  m:= strtofloat(inttostr(mm));
  a:= strtofloat(inttostr(aa));
  x1:= -int(0.7 + (1 / (1 + m)));
  a:= a + x1;
  x2:= (1 + m) - (12 * x1);
  fator:= strtoint(floattostr(int(x2 * 30.6001) + int(a * 365.25) + int(d)));
end;

function formata_latlon_mare(latlon: integer; tipo: string): string;
{Função que formata a latitude ou longitude para arquivos das rotinas
Aplha (DLL). Retorna '0' se a latitude ou a longitude for inválida.}
var
  gr, mn, decseg, sinal: string;
begin
  if (desmembra_latlon_mare(latlon, gr, mn, decseg, sinal, tipo) = false) then
    formata_latlon_mare:= '0'
  else
    formata_latlon_mare:= gr + mn + decseg + sinal;
end;

function valida_inteiro(num: string): boolean;
{Função para verificar se uma string é um número inteiro.
Retorna true se a conversão for possível.}
var
  tam_num, cont_char: integer;
  valido: boolean;
begin
  valido:= true;
  tam_num:= length(num);
  cont_char:= 1;
  while ((cont_char <= tam_num) and (valido = true)) do
    begin
      if not (num[cont_char] in['0'..'9']) then
        valido:= false;
      cont_char:= cont_char + 1;
    end;
  valida_inteiro:= valido;
end;

function calcula_dias(data_ini, data_fim: tdate): integer;
{Função que retorna o número de dias entre duas datas. Se a
data inicial for maior do que a data final então a função retorna
-1.}
var
  cont_dias: integer;
  data_aux: tdate;
begin
  if (data_ini > data_fim) then
    calcula_dias:= -1
  else
    begin
      cont_dias:= 0;
      data_aux:= data_ini;
      while (data_aux <= data_fim) do
        begin
          data_aux:= data_aux + 1;
          cont_dias:= cont_dias + 1;
        end;
      calcula_dias:= cont_dias;
    end;
end;

function valida_data_hora(data_hora: string): boolean;
{Função que valida uma data/hora. Data/hora deve estar no
formato 'dd/mm/yyyy hh:mm:ss'.}
begin
  try
    if (length(data_hora) <> 19) then
      valida_data_hora:= false
    else
      begin
        strtodatetime(data_hora);
        valida_data_hora:= true;
      end;
  except
    valida_data_hora:= false;
  end;
end;

function valida_real(num: string; separador_decimal: char): boolean;
{Função para verificar se uma string é um número real. O separador
decimal usado na string deve ser informado. Retorna true se a conversão
for possível.}
var
  num_aux: string;
begin
  try
    num_aux:= substitui_caracter(num, separador_decimal, decimalseparator);
    strtofloat(num_aux);
    valida_real:= true;
  except
    valida_real:= false;
  end;
end;

function converte_data_br(data: string): string;
{Função para converter uma data no formato yyyy-mm-dd (MySql) para
formato padrão BR (dd/mm/yyyy). Em caso de data inválida retorna '0'.}
var
  data_aux: string;
begin
  try
    data_aux:= copy(data, 9, 2) + '/' + copy(data, 6, 2) + '/' + copy(data, 1, 4);
    strtodate(data_aux);
    converte_data_br:= data_aux;
  except
    converte_data_br:= '0';
  end;
end;

function converte_data_arq(data: string): string;
{Função para converter uma data no formato 'dd/mm/yyyy' para formato
o formato 'ddmmyyyy'. Este formato é utilizado em nomes de arquivos
gerados pelo Sistema Maré (Ex. 501400101200531122005AH.graf).
Em caso de data inválida retorna '0'.}
var
  tam, i: integer;
begin
  try
    tam:= length(data);
    for i:= 1 to tam do
      begin
        if not (data[i] in['0'..'9', '/']) then
          begin
            converte_data_arq:= '0';
            exit;
          end;
      end;
    strtodate(data);
    converte_data_arq:= copy(data, 1, 2) + copy(data, 4, 2) + copy(data, 7, 4);
  except
    converte_data_arq:= '0';
  end;
end;

function QuadradoMarsden10(Latitude, Longitude: integer): integer;
var
  Lat1, Lat2, Lat3, Lat4, Lat5, Lat6: integer;
  Long1, Long2, Long3, Long4, Long5, Long6, Long7: integer;
  LatGrau, LatMin, LatSec, LatSinal, LongGrau, LongMin, LongSec, LongSinal: string;
  Quadr: integer;
  Qmd10, Qmd1: integer;
begin
  Quadr:= Quadrante(Latitude, Longitude);
  desmembra_latlon(Latitude, LatGrau, LatMin, LatSec, LatSinal, 'LT');
  desmembra_latlon(Longitude, LongGrau, LongMin, LongSec, LongSinal, 'LG');
  lat1:= strtoint(copy(latgrau, 1, 1));
  lat2:= strtoint(copy(latgrau, 2, 1));
  lat3:= strtoint(copy(latmin, 1, 1));
  lat4:= strtoint(copy(latmin, 2, 1));
  lat5:= strtoint(copy(latsec, 1, 1));
  lat6:= strtoint(copy(latsec, 2, 1));
  long1:= strtoint(copy(longgrau, 1, 1));
  long2:= strtoint(copy(longgrau, 2, 1));
  long3:= strtoint(copy(longgrau, 3, 1));
  long4:= strtoint(copy(longmin, 1, 1));
  long5:= strtoint(copy(longmin, 2, 1));
  long6:= strtoint(copy(longsec, 1, 1));
  long7:= strtoint(copy(longsec, 2, 1));
  if (Quadr = 1) then
    begin
      if ((strtoint(latgrau) >= 80) and (strtoint(latgrau) <= 90)) then
        begin
          if (long1 = 1) then
            Qmd10:= ((lat1 * 112) - long2) + 30
          else
            Qmd10:= ((lat1 * 112) - long2) + 40;
        end
      else
        begin
         if (long1 = 1) then
           Qmd10:= ((lat1 * 36) - long2) + 26
         else
           Qmd10:= ((lat1 * 36) - long2) + 36;
        end;
    end;
  if (Quadr = 3) then
    begin
      if (long1 = 1) then
        qmd10:= ((lat1 * 36) - long2) + 325
      else
        qmd10:= ((lat1 * 36) - long2) + 335;
    end;
  if (Quadr = 5) then
    begin
      if (long1 = 1) then
        qmd10:= ((lat1 * 36) + long2) + 310
      else
        qmd10:= ((lat1 * 36) + long2) + 300;
    end;
  if (Quadr = 7) then
    begin
      if ((strtoint(latgrau) >= 80) and (strtoint(latgrau) <= 90)) then
        begin
          if (long1 = 1) then
            qmd10:= ((lat1 * 112) + long2) + 15
          else
            qmd10:= ((lat1 * 112) + long2) + 5;
        end
      else
        begin
          if (long1 = 1) then
            qmd10:= ((lat1 * 36) + long2) + 11
          else
            qmd10:= ((lat1 * 36) + long2) + 1;
        end;
    end;
  Qmd1:= (lat2 * 10) + long3;
  QuadradoMarsden10:= Qmd10;
end;

function Quadrante(Latitude, Longitude: integer): integer;
begin
  if (Latitude <= 0) then
    begin
      if (Longitude <= 0)  then
	Quadrante:= 5
      else
	Quadrante:= 3;
    end
  else
    begin
      if (Longitude <= 0)  then
	Quadrante:= 7
      else
	Quadrante:= 1;
    end;
end;

function SubQuadradoMarsden01(Latitude, Longitude: integer): integer;
var
  Lat1, Lat2, Lat3, Lat4, Lat5, Lat6: integer;
  Long1, Long2, Long3, Long4, Long5, Long6, Long7: integer;
  LatGrau, LatMin, LatSec, LatSinal, LongGrau, LongMin, LongSec, LongSinal: string;
  Quadr: integer;
  Qmd10, Qmd1: integer;
begin
  Quadr:= Quadrante(Latitude, Longitude);
  Quadr:= Quadrante(Latitude, Longitude);
  desmembra_latlon(Latitude, LatGrau, LatMin, LatSec, LatSinal, 'LT');
  desmembra_latlon(Longitude, LongGrau, LongMin, LongSec, LongSinal, 'LG');
  lat1:= strtoint(copy(latgrau, 1, 1));
  lat2:= strtoint(copy(latgrau, 2, 1));
  lat3:= strtoint(copy(latmin, 1, 1));
  lat4:= strtoint(copy(latmin, 2, 1));
  lat5:= strtoint(copy(latsec, 1, 1));
  lat6:= strtoint(copy(latsec, 2, 1));
  long1:= strtoint(copy(longgrau, 1, 1));
  long2:= strtoint(copy(longgrau, 2, 1));
  long3:= strtoint(copy(longgrau, 3, 1));
  long4:= strtoint(copy(longmin, 1, 1));
  long5:= strtoint(copy(longmin, 2, 1));
  long6:= strtoint(copy(longsec, 1, 1));
  long7:= strtoint(copy(longsec, 2, 1));
  if (Quadr = 1) then
    begin
      if ((strtoint(latgrau) >= 80) and (strtoint(latgrau) <= 90)) then
        begin
          if (long1 = 1) then
            Qmd10:= ((lat1 * 112) - long2) + 30
          else
            Qmd10:= ((lat1 * 112) - long2) + 40;
        end
      else
        begin
         if (long1 = 1) then
           Qmd10:= ((lat1 * 36) - long2) + 26
         else
           Qmd10:= ((lat1 * 36) - long2) + 36;
        end;
    end;
  if (Quadr = 3) then
    begin
      if (long1 = 1) then
        qmd10:= ((lat1 * 36) - long2) + 325
      else
        qmd10:= ((lat1 * 36) - long2) + 335;
    end;
  if (Quadr = 5) then
    begin
      if (long1 = 1) then
        qmd10:= ((lat1 * 36) + long2) + 310
      else
        qmd10:= ((lat1 * 36) + long2) + 300;
    end;
  if (Quadr = 7) then
    begin
      if ((strtoint(latgrau) >= 80) and (strtoint(latgrau) <= 90)) then
        begin
          if (long1 = 1) then
            qmd10:= ((lat1 * 112) + long2) + 15
          else
            qmd10:= ((lat1 * 112) + long2) + 5;
        end
      else
        begin
          if (long1 = 1) then
            qmd10:= ((lat1 * 36) + long2) + 11
          else
            qmd10:= ((lat1 * 36) + long2) + 1;
        end;
    end;
  Qmd1:= (lat2 * 10) + long3;
  SubQuadradoMarsden01:= Qmd1;
end;

function NumeroValido(Numero: string): boolean;
{Função que verifica se uma string é um número com ponto
flutuante válido.}
var
  Tam, I, Chave: integer;
begin
  Chave:= 0;
  Tam:= Length(Numero);
  if (Tam = 0) then
    NumeroValido:= false
  else
    begin
      if ((Numero[1] = '.') or (Numero[Tam] = '.')) then
        Chave:= 1;
      I:= 1;
      while ((Chave = 0) and (I <= Tam)) do
        begin
          if not (Numero[I] in['0'..'9', '.']) then
            Chave:= 1;
          I:= I + 1;
        end;
      if (Chave = 0) then
        NumeroValido:= true
      else
        NumeroValido:= false;
    end;
end;

function ConverteDataAnsi(Data: string): string;
{Função para converter uma data no formato dd/mm/yyyy para formato
padrão ANSI (yyyy-mm-dd). Em caso de data inválida retorna '0'.}
var
  Tam, I: integer;
begin
  try
    Tam:= Length(Data);
    for I:= 1 to Tam do
      begin
        if not (Data[I] in['0'..'9', '/']) then
          begin
            ConverteDataAnsi:= '0';
            Exit;
          end;
      end;
    StrToDate(Data);
    ConverteDataAnsi:= Copy(Data, 7, 4) + '-' + Copy(Data, 4, 2) + '-' + Copy(Data, 1, 2);
  except
    ConverteDataAnsi:= '0';
  end;
end;

function ConverteDataBr(Data: string): string;
{Função para converter uma data no formato yyyy-mm-dd (ANSI) para
formato padrão BR (dd/mm/yyyy). Em caso de data inválida retorna '0'.}
var
  DataAux: string;
begin
  try
    DataAux:= Copy(Data, 9, 2) + '/' + Copy(Data, 6, 2) + '/' + Copy(Data, 1, 4);
    StrToDate(DataAux);
    ConverteDataBr:= DataAux;
  except
    ConverteDataBr:= '0';
  end;
end;

function GeraEspacos(Tam: integer): string;
var
  I: integer;
  Espacos: string;
begin
  Espacos:= '';
  if (Tam <> 0) then
    begin
      for I:= 1 to Tam do
        Espacos:= Espacos + ' ';
    end;
  GeraEspacos:= Espacos;
end;

function FormataLatLonMareTela(LatLon: integer; Tipo: string): string;
var
  Gr, Mn, DecSeg, Sinal: string;
begin
  if (desmembra_latlon_mare(LatLon, Gr, Mn, DecSeg, Sinal, Tipo) = false) then
    FormataLatLonMareTela:= '0'
  else
    FormataLatLonMareTela:= Gr + 'º ' + Mn + '''.' + DecSeg + ' ' + Sinal;
end;

function FormataFusoMareTela(Fuso: string): string;
var
  Hora, Minuto, Sinal: string;
begin
  FormataFusoTela(Fuso, Hora, Minuto, Sinal);
  FormataFusoMareTela:= Sinal + Hora + '.' + Minuto;
end;

procedure FormataFusoTela(Fuso: string; var Hora, Minuto, Sinal: string);
begin
  if ((Fuso = '') or (Length(Fuso) < 4)) then
    begin
      Hora:= '00';
      Minuto:= '0';
      Sinal:= '+';
    end
  else
    begin
      Hora:= Copy(Fuso, 2, 2);
      Minuto:= Copy(Fuso, 4, 1);
      Sinal:= Copy(Fuso, 1, 1);
    end;
end;

procedure ListarArquivos(Diretorio: string; Sub: Boolean; var Lista: TStrings);
{
Descrição: Procedimento que lista na variável passada por referência #Lista#
os nomes completos dos arquivos de um diretório informado na variável #Diretorio#.
O diretório informado deve existir. A variável #Sub#, se verdadeira, informa que todos
os subdiretórios abaixo do diretório informado deverão ser listados.
Data Criação: 27/08/2008 por Carlos Henrique
Data Modificação:
}
var
  F: TSearchRec;
  Ret: Integer;
  TempNome: string;
begin
  Ret:= FindFirst(Diretorio+'\*.*', faAnyFile, F);
  try
    while Ret = 0 do
      begin
        if TemAtributo(F.Attr, faDirectory) then
          begin
            if (F.Name <> '.') And (F.Name <> '..') then
              if Sub = True then
                begin
                  TempNome := Diretorio+'\' + F.Name;
                  ListarArquivos(TempNome, True, Lista);
                end;
          end
        else
          begin
            Lista.Add(Diretorio+'\'+F.Name);
          end;
        Ret := FindNext(F);
      end;
  finally
    FindClose(F);
  end;
end;

function TemAtributo(Attr, Val: Integer): Boolean;
{
Descrição: Função pertencente ao procedimento #ListarArquivos#.
Data Criação: 27/08/2008 por Carlos Henrique
Data Modificação:
}
begin
  TemAtributo:= Attr and Val = Val;
end;

function Cria_LatLong_Oce(latitude,longitude:string; var graulat,minlat,seglat,graulong,minlong,
  seglong,sinal_lat,sinal_long:string) : boolean;
var
ilat,ilong:integer;
decimalat,totalat:string;
decimalong,totalong:string;
//decimaldominuto:string;
minuto,decimaldominuto,decimaldominlong,posicao_lat,posicao_long,graulong_aux:string;
segundo,segundolong,minutolong:real;

begin
     totalat:='6';
     decimalat:='1';
     totalong:='7';
     decimalong:='1';

     if (latitude = ' ') or (longitude = '  ') then
      begin
        Cria_LatLong_Oce:= false;
        exit;
      end;
     if  (latitude <> '') and (Length(copy(latitude,1,6)) < 7) then
       begin
          for ilat:=1 to Length(latitude) do
              begin
                if ((latitude[ilat]<>',')and (latitude[ilat]<>#0) and (ilat <= 2)) then
                   graulat:= graulat + latitude[ilat]
                  else
                    begin
                       if ((latitude[ilat]= '.')and (latitude[ilat]<>#0)) then
                           begin
                             decimalat := inttostr(ilat);
                             decimalat := inttostr(strtoint(totalat) - strtoint(decimalat)) ;
                             decimalat:='';
                           end
                        else
                           if ((latitude[ilat]<>',')and (ilat > 2)) then
                               decimalat := decimalat+latitude[ilat];

                    end;

              end;
              while length(graulat)< 2 do
                 graulat:= '0'+graulat;
              decimalat:='.'+decimalat;
          minuto:= floattostr((strtofloat(decimalat) * 60));
         // intminutolat:=(int(minuto));
        //  decimaldominuto:=frac(minuto);
         if  (minuto <> '') and (Length(minuto) < 7) then
             begin
                for ilat:=1 to Length(minuto) do
                    begin
                      if ((minuto[ilat]<>'.')and (minuto[ilat]<>#0) and (ilat <= 2)) then
                         minlat:= minlat + minuto[ilat]
                        else
                          begin
                             if ((minuto[ilat]= ',')and (minuto[ilat]<>#0)) then
                                 begin
                                   decimaldominuto := inttostr(ilat);
                                   decimaldominuto:='';
                                 end
                              else
                                 if ((minuto[ilat]<>',')and (ilat > 2)) then
                                     decimaldominuto := decimaldominuto+minuto[ilat];

                          end;
                    end;
                    decimaldominuto:='.'+decimaldominuto;
                    while length(minlat)< 2 do
                        minlat:='0'+minlat;
             end;


             if (decimaldominuto <> '') and (decimaldominuto <> '.') then
               begin
                  segundo:= int(strtofloat(decimaldominuto) * 60);
                  seglat:=floattostr(segundo);
               end
             else
                seglat:='00';
          //   posicao_lat := 'S';
             while length(seglat)<2 do
                 seglat:='0'+seglat;
       end;
      if  (longitude <> '') and (Length(copy(longitude,1,7)) < 8) then
       begin
          for ilong:=1 to Length(longitude) do
              begin
                if ((longitude[ilong]<>',')and (longitude[ilong]<>#0) and (ilong <= 3)) then
                   graulong:= graulong + longitude[ilong]
                  else
                    begin
                       if ((longitude[ilong]= '.')and (longitude[ilong]<>#0)) then
                           begin
                             decimalong := inttostr(ilong);
                             decimalong := inttostr(strtoint(totalong) - strtoint(decimalong)) ;
                             decimalong:='';
                           end
                        else
                           if ((longitude[ilong]<>',')and (ilong > 2)) then
                               decimalong := decimalong+longitude[ilong];

                    end;

              end;
              if graulong[1] = '-' then
                 graulong_aux:= copy(graulong,2,length(graulong))
               else
                 graulong_aux:=graulong;
              while length(graulong_aux) < 3 do
                begin
                 if graulong[1] = '-' then
                    begin
                      graulong_aux:= '0'+copy(graulong,2,length(graulong));
                      graulong:= graulong[1]+graulong_aux;
                    end
                  else
                   begin
                      graulong_aux:= '0'+graulong;
                      graulong:=graulong_aux;
                   end;

                end;
              decimalong:='.'+decimalong;
          minutolong:= ((strtofloat(decimalong) * 60));
          if  (floattostr(minutolong) <> '') and (Length(floattostr(minutolong)) < 7) then
               begin
                  for ilong:=1 to Length(floattostr(minutolong)) do
                      begin
                        if ((floattostr(minutolong)[ilong]<>'.')and (floattostr(minutolong)[ilong]<>#0) and (ilong <= 2)) then
                           minlong:= minlong + floattostr(minutolong)[ilong]
                          else
                            begin
                               if ((floattostr(minutolong)[ilong]= '.')and (floattostr(minutolong)[ilong]<>#0)) then
                                   begin
                                     decimaldominlong := inttostr(ilong);
                                     decimaldominlong:='';
                                   end
                                else
                                   if ((floattostr(minutolong)[ilong]<>'.')and (ilong > 2)) then
                                       decimaldominlong := decimaldominlong+floattostr(minutolong)[ilong];

                            end;
                      end;
                      decimaldominlong:='.'+decimaldominlong;
                      while length(minlong) < 2 do
                         minlong:='0'+minlong;
               end;
               {if length(minuto) < 3     then
                   decimaldominuto:= latitudemin;
               if length(minutolong) < 3     then
                   decimaldominlong:= ' ';}

          if (decimaldominlong <> ' ') and (decimaldominlong <> '.') then
            begin
               segundolong:= int(strtofloat(decimaldominlong) * 60);
               seglong:=floattostr(segundolong);
            end
           else
              seglong:= '00';
       //   posicao_long:= 'W';
          while length(seglong)<2 do
               seglong:= '0'+seglong;
       end;
       if (posicao_lat = 'N') and (posicao_long = 'W') then
         begin
           //sinal_lat:= '+';
           sinal_long:= '-';
         end
        else
          if (posicao_lat = 'N') and (posicao_long = 'E') then
            begin
             //sinal_lat:= '+';
             // sinal_long:= '+';
            end
           else
             if (posicao_lat = 'S') and (posicao_long = 'E') then
                begin
                  sinal_lat:= '-';
                //  sinal_long:= '+';
                end
              else
                if (posicao_lat = 'S') and (posicao_long = 'W') then
                  begin
                    sinal_lat:= '-';
                    sinal_long:= '-';
                  end ;

      Cria_LatLong_Oce:=true;
end;

function IdentaDataHora(DataHora: TDateTime; QtdMinutos: Byte): TDateTime;
var
  Hora, Min, Seg: Byte;
  HoraAux, MinAux: Extended;
  QtdDias: Word;
  Data: TDate;
begin
  Data:= StrToDate(FormatDateTime('dd/mm/yyyy', DataHora));
  QtdDias:= 0;
  Hora:= StrToInt(FormatDateTime('hh', DataHora));
  Min:= StrToInt(FormatDateTime('nn', DataHora));
  Seg:= StrToInt(FormatDateTime('ss', DataHora));
  Min:= Min + QtdMinutos;
  if (Min >= 60) then
    begin
      MinAux:= (((Min / 60) - (Min mod 60)) * 60);
      Min:= StrToInt(FloatToStr(Min - (((Min / 60) - (Min mod 60)) * 60)));
      Hora:= Hora + StrToInt(FloatToStr(MinAux / 60));
    end;
  if (Hora > 23) then
    begin
      HoraAux:= (((Hora / 24) - (Hora mod 24)) * 24);
      Hora:= StrToInt(FloatToStr(Hora - (((Hora / 24) - (Hora mod 24)) * 24)));
      QtdDias:= StrToInt(FloatToStr(HoraAux / 24));
    end;
  if (QtdDias > 0) then
    Data:= Data + QtdDias;
  IdentaDataHora:= StrToDateTime(DateToStr(Data) + ' ' + IntToStr(Hora) + ':' + IntToStr(Min) + ':' + IntToStr(Seg));
end;

function RetornaQtdLinhasArquivo(Arquivo: string): Integer;
var
  Arq: TextFile;
  ContLinhas: Integer;
begin
  AssignFile(Arq, Arquivo);
  ContLinhas:= 0;
  Reset(Arq);
  while not (Eof(Arq)) do
    begin
      Readln(Arq);
      ContLinhas:= ContLinhas + 1;
    end;
  CloseFile(Arq);
  RetornaQtdLinhasArquivo:= ContLinhas;
end;

procedure ConcatenaArquivos(ArquivoConcatenado, ArquivoDados: string);
var
  ArqDados, ArqConcat: TextFile;
  Caracter: Char;
begin
  AssignFile(ArqConcat, ArquivoConcatenado);
  AssignFile(ArqDados, ArquivoDados);
  if (FileExists(ArquivoConcatenado)) then
    Append(ArqConcat)
  else
    Rewrite(ArqConcat);
  Reset(ArqDados);
  while not Eof(ArqDados) do
    begin
      Read(ArqDados, Caracter);
      Write(ArqConcat, Caracter);
    end;
  CloseFile(ArqConcat);
  CloseFile(ArqDados);
  DeleteFile(ArquivoDados);
end;

end.
