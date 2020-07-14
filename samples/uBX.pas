unit uBX;

interface

uses Classes, SysUtils, System.StrUtils,Winapi.Windows, Winapi.Messages, System.IOUtils,
    System.Math ;

function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
function CurrentPath : string;
//Bitcoin functions
//keys
function GetNewSeed : string;
function GetHDPrivKeyFromSeed (seed : string) : string;
function GetECPrivKeyFromSeed (seed : string) : string;
function GetNewHDPubKeyFromPubkeyIdx (PubKey : string; idx : integer) : string;
function GetNewHDPrivKeyFromPrivKeyIdx (PrivKey : string; idx : integer) : string;
function HDtoEC (Key : string) : string;
function GetHDPubKeyFromPrivKey (PrivKey : string) : string;
function GetAddrFromHDKey (Key : string) : string;

//hash
function Bitcoin256 (value : string) : string;
function Bitcoin160 (value : string) : string;
function SHA256 (value : string) : string;

//encode-decode
function Base16Encode (value : string) : string;
function Base16Decode (value : string) : string;
function Base64Encode (value : string) : string;
function Base64Decode (value : string) : string;
function ScriptEncode (value : string) : string;
function ScriptDecode (value : string) : string;
function AddressDecode (value : string) : string;

//network
function FetchBalance (addr : string; format: string = 'info') : string;
function FetchHistory (addr : string; format: string = 'info') : string;
function FetchTx (txID : string; format: string = 'info') : string;
function IsTxExist (txID : string; format: string = 'info') : boolean;
function FetchTxIndex (txID : string; format: string = 'info') : string;
function FetchHeight : string;

//transaction
function InputSign (PrivKey : string; Contract : string; Trx : string; idx : integer = 0; signType : string = 'all') : string;
function InputSet (Signature : string; ECPubKey : string; Trx : string; idx : integer = 0) : string;
function InputSetP2SH (slSignatures : TStringList; P2SHscript : string; Trx : string) : string;
function ValidateTx (Trx : string) : Boolean;
function SendTx (Trx : string) : Boolean;

//operation
procedure SwapByte(var strA : string; var StrB : string);
procedure AddZeroes(var StrA : string; var StrB : string; totalLength : integer);

function TxFee (InputCount : integer; OutputCount : integer) : integer;

//scripting
function ScriptToAddress (script : string) : string;
function CoinSwap2CommitScript(secret : string; cltv : integer; ECPubKey1 : string; ECPubKey2 : string) : string;
function CoinSwap3CommitScript(secret : string; cltv : integer; ECPubKey1 : string; ECPubKey2 : string) : string;
function BITACommitScript(cltv : integer; ECPubKey1 : string; ECPubKey2 : string; ECPubKey3 : string) : string;
function BITARedeemScript(ECPrivKey1 : string; ECPrivKey2 : string; unsignedTX : string; P2SHscript : string) : string;
function BITARefundScript(ECPrivKey1 : string; ECPubKey1 : string; unsignedTX : string; P2SHscript : string) : string;

function AnyScript(script : string) : string;

implementation

uses uDMod;

function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

function CurrentPath : string;
begin
  {$IFDEF ANDROID}
  Result := TPath.GetDocumentsPath;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

function GetNewSeed : string;
var cli : string;
begin
  cli := 'bx seed';
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetHDPrivKeyFromSeed (seed : string) : string;
var cli : string;
begin
  cli := 'bx hd-new ' + seed;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetECPrivKeyFromSeed (seed : string) : string;
var cli : string;
begin
  cli := 'bx ec-new ' + seed;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetNewHDPubKeyFromPubkeyIdx (PubKey : string; idx : integer) : string;
var cli : string;
begin
  cli := 'bx hd-public --index ' + IntToStr(idx) + ' ' + PubKey;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetNewHDPrivKeyFromPrivKeyIdx (PrivKey : string; idx : integer) : string;
var cli : string;
begin
  cli := 'bx hd-private --index ' + IntToStr(idx) + ' ' + PrivKey;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function HDtoEC (Key : string) : string;
var cli : string;
begin
  cli := 'bx hd-to-ec ' + Key;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetHDPubKeyFromPrivKey (PrivKey : string) : string;
var cli : string;
begin
  cli := 'bx hd-to-public ' + PrivKey;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function GetAddrFromHDKey (Key : string) : string;
var cli : string;
begin
  cli := 'bx hd-to-address ' + Key;
  Result := Trim(GetDosOutput(cli,CurrentPath));
end;

function Bitcoin256 (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  if length(value)< 8000 then
  begin
    cli := 'bx bitcoin256 ' + value;
    hasil := GetDosOutput(cli,CurrentPath);
  end
  else
  begin
    cli := 'bx bitcoin256 < tempSrc > tempDest';
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  end;
  Result := Trim(hasil);
end;

function Bitcoin160 (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  if length(value)< 8000 then
  begin
    cli := 'bx bitcoin160 ' + value;
    hasil := GetDosOutput(cli,CurrentPath);
  end
  else
  begin
    cli := 'bx bitcoin160 < tempSrc > tempDest';
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  end;
  Result := Trim(hasil);
end;

function SHA256 (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);
  if length(value)< 8000 then
  begin
    cli := 'bx sha256 ' + value;
    hasil := GetDosOutput(cli,CurrentPath);
  end
  else
  begin
    cli := 'bx sha256 < tempSrc > tempDest';
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  end;
  Result := Trim(hasil);
end;

function Base16Encode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    {if length(value)< 8000 then
    begin
      cli := 'bx base16-encode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end
    else
    begin
      cli := 'bx base16-encode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;}
    cli := 'bx base16-encode < tempSrc > tempDest';
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');

  end;
  Result := Trim(hasil);
end;

function Base16Decode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  {if length(value)< 8000 then
  begin
    cli := 'bx base16-decode ' + '"' + value + '"';
    hasil := GetDosOutput(cli,CurrentPath);
  end
  else
  begin
    cli := 'bx base16-decode < tempSrc > tempDest';
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  end;}
  cli := 'bx base16-decode < tempSrc > tempDest';
  hasil := GetDosOutput(cli,CurrentPath);
  hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  Result := Trim(hasil);
end;

function Base64Encode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  {if length(value)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx base64-encode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx base64-encode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;}
  cli := 'bx base64-encode < tempSrc > tempDest';
  hasil := GetDosOutput(cli,CurrentPath);
  hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  Result := Trim(hasil);
end;

function Base64Decode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  {if length(value)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx base64-decode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx base64-decode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;}
  cli := 'bx base64-decode < tempSrc > tempDest';
  hasil := GetDosOutput(cli,CurrentPath);
  hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
  Result := Trim(hasil);
end;


function ScriptEncode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  if length(value)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx script-encode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx script-encode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;
  Result := Trim(hasil);
end;

function ScriptDecode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  if length(value)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx script-decode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx script-decode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;
  Result := Trim(hasil);
end;

function AddressDecode (value : string) : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',value);

  if length(value)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx address-decode ' + '"' + value + '"';
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx address-decode < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;
  Result := Trim(hasil);
end;

function FetchBalance (addr : string; format: string = 'info') : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-balance ' + '-f ' + format + ' ' + addr;
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

function FetchHistory (addr : string; format: string = 'info') : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-history ' + '-f ' + format + ' ' + addr;
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
  //Result := cli;
end;

function FetchTx (txID : string; format: string = 'info') : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  //Shout('wew');
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-tx ' + '-f ' + format + ' ' + txID + ' > tempDest';
    //Shout(cli);
    hasil := GetDosOutput(cli,CurrentPath);
    hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    //Shout(hasil);
  end;
  Result := Trim(hasil);
end;

function IsTxExist (txID : string; format: string = 'info') : boolean;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-tx ' + '-f ' + format + ' ' + txID;
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  if (UpperCase(Trim(hasil)) = 'OBJECT DOES NOT EXIST') then Result := False
  else Result := True;
end;

function FetchTxIndex (txID : string; format: string = 'info') : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-tx-index ' + '-f ' + format + ' ' + txID;
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

function FetchHeight : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx fetch-height';
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

function InputSign (PrivKey : string; Contract : string; Trx : string; idx : integer = 0; signType : string = 'all') : string;
var cli : string;
    hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',Trx);

  if length(Trx)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx input-sign ' + '-i ' + IntToStr(idx) + ' -s ' + signType + ' ' + PrivKey + ' "' + Contract + '" ' + Trx;
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx input-sign ' + '-i ' + IntToStr(idx) + ' -s ' + signType + ' ' + PrivKey + ' "' + Contract + '"  < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;
  Result := Trim(hasil);
end;

function InputSet (Signature : string; ECPubKey : string; Trx : string; idx : integer = 0) : string;
var cli, endorsement, hasil : string;
    berkas : TFile;
begin
  hasil := '';
  endorsement := '[ ' + Signature + ' ] [ ' + ECPubKey + ' ]';

  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx input-set -i ' + IntToStr(idx) + ' "' + endorsement + '" ' + Trx;
    hasil := GetDosOutput(cli,CurrentPath);
    //hasil := cli;
  end;
  Result := Trim(hasil);
end;

function InputSetP2SH (slSignatures : TStringList; P2SHscript : string; Trx : string) : string;
var cli, endorsement, hasil : string;
    i : integer;
begin
  hasil := '';
  for i := 0 to slSignatures.Count - 1 do
  begin
    endorsement := endorsement + '[ ' + slSignatures[i] + ' ] ';
  end;
  endorsement := endorsement + '[ ' + P2SHscript + ' ]';


  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx input-set ' + ' "' + endorsement + '" ' + Trx;
    hasil := GetDosOutput(cli,CurrentPath);
    //hasil := cli;
  end;
  Result := Trim(hasil);
end;

function ValidateTx (Trx : string) : Boolean;
var cli, hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',Trx);

  if length(Trx)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx validate-tx ' + Trx;
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx validate-tx < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;

  if (Trim(hasil) = 'The transaction is valid.') then Result := True
    else Result := False;
end;

function SendTx (Trx : string) : Boolean;
var cli, hasil : string;
    berkas : TFile;
begin
  try
    berkas.Delete(CurrentPath + 'tempSrc');
    berkas.Delete(CurrentPath + 'tempDest');
  except
  end;
  berkas.WriteAllText(CurrentPath + 'tempSrc',Trx);

  if length(Trx)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx send-tx ' + Trx;
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx send-tx < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;

  if length(Trx)< 8000 then
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx send-tx-p2p ' + Trx;
      hasil := GetDosOutput(cli,CurrentPath);
    end;
  end
  else
  begin
    while ((hasil = '') or (Trim(hasil) = 'timed out')) do
    begin
      cli := 'bx send-tx-p2p < tempSrc > tempDest';
      hasil := GetDosOutput(cli,CurrentPath);
      hasil := berkas.ReadAllText(CurrentPath + 'tempDest');
    end;
  end;

  Result := True;

end;

procedure SwapByte(var strA : string; var StrB : string);
var buffer : string;
    panjangString : integer;
    i, indeks : integer;
begin
  //ab cd ef gh ij
  panjangString := Length(strA);
  if panjangString mod 2 > 0 then strA := '0' + strA;
  panjangString := Length(strA);

  indeks := panjangString+1;
  StrB := '';
  for i := 1 to panjangString div 2 do
  begin
    indeks := indeks - 2;
    buffer := Copy(strA, indeks,2);
    StrB := StrB + buffer;
  end;
end;

procedure AddZeroes(var StrA : string; var StrB : string; totalLength : integer);
var i : integer;
    panjangString : integer;
begin
  //ab cd ef gh ij 00 00
  panjangString := Length(strA);
  StrB := StrA;
  if (panjangString < totalLength) then
  begin
    //
    for i := 0 to (totalLength - panjangString - 1) do
    begin
      StrB := StrB + '0';
    end;
  end;
end;

function TxFee (InputCount : integer; OutputCount : integer) : integer;
var minTxFee, TxSize : integer;
begin
  //https://github.com/bitcoin/bitcoin/pull/6201
  minTxFee := 20000; // 10.000 satoshi per KB or 10 satoshi per Byte if we want the tx to be prioritized. now make it 20000
  //if more than 1 KB then pay 20 satoshi per Byte
  //http://www.coindesk.com/new-service-finds-optimum-bitcoin-transaction-fee/
  TxSize := 148 * InputCount + 34 * OutputCount + 10;
  //if (TxSize < 10000) then Result := minTxFee
  //else Result := TxSize * 20; ///////////

  //https://en.bitcoin.it/wiki/Transaction_fees. modify to make it faster
  Result := Ceil(txSize / 1000) * 20000;
end;

function ScriptToAddress (script : string) : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx script-to-address ' + '"' + script +  '"';
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

function PubKeyToPubKeyHash (PubKey : string) : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx sha256 ' + PubKey + ' | bx ripemd160';
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

function CoinSwap2CommitScript(secret : string; cltv : integer; ECPubKey1 : string; ECPubKey2 : string) : string;
var base16Secret, sha256Secret, ECPKH1, ECPKH2 : string;
    s : string;
    cltvHex : string;
    cltvHexLE : string;
begin
  //
  base16Secret := Base16Encode(secret);
  sha256Secret := SHA256(base16Secret);
  ECPKH1 := PubKeyToPubKeyHash(ECPubKey1);
  ECPKH2 := PubKeyToPubKeyHash(ECPubKey2);
  cltvHex := IntToHex(cltv,0);
  SwapByte(cltvHex, cltvHexLE);

  s := 'dup sha256 [ %s ] equal if drop dup hash160 [ %s ] equalverify checksig else [ %s ] nop2 drop dup hash160 [ %s ] equalverify checksig endif';
  s := Format(s,[sha256Secret, ECPKH2, cltvHexLE, ECPKH1]);
  Result := s;
end;

function CoinSwap3CommitScript(secret : string; cltv : integer; ECPubKey1 : string; ECPubKey2 : string) : string;
var base16Secret, sha256Secret, ECPKH1, ECPKH2 : string;
    s : string;
    cltvHex : string;
    cltvHexLE : string;
begin
  //
  base16Secret := Base16Encode(secret);
  sha256Secret := SHA256(base16Secret);
  ECPKH1 := PubKeyToPubKeyHash(ECPubKey1);
  ECPKH2 := PubKeyToPubKeyHash(ECPubKey2);
  cltvHex := IntToHex(cltv,0);
  SwapByte(cltvHex, cltvHexLE);

  s := 'if if dup sha256 [ %s ] equal drop dup hash160 [ %s ] equalverify checksig else [ %s ] nop2 drop dup hash160 [ %s ] equalverify checksig endif else 2 [ %s ] [ %s ] 2 checkmultisig endif ';
  s := Format(s,[sha256Secret, ECPKH2, cltvHexLE, ECPKH1, ECPubKey1, ECPubKey2]);
  Result := s;
end;

function BITACommitScript(cltv : integer; ECPubKey1 : string; ECPubKey2 : string; ECPubKey3 : string) : string;
var ECPKH1 : string;
    s : string;
    cltvHex : string;
    cltvHexLE : string;
begin
  //
  ECPKH1 := PubKeyToPubKeyHash(ECPubKey1);

  cltvHex := IntToHex(cltv,0);
  SwapByte(cltvHex, cltvHexLE);
  //Shout(IntToStr(cltv) + ' - ' + cltvHex + ' - ' + cltvHexLE);

  s := 'if 2 [ %s ] [ %s ] [ %s ] 3 checkmultisig else [ %s ] nop2 drop dup hash160 [ %s ] equalverify checksig endif';
  s := Format(s,[ECPubKey1, ECPubKey2, ECPubKey3, cltvHexLE, ECPKH1]);
  Result := s;
end;

function BITARedeemScript(ECPrivKey1 : string; ECPrivKey2 : string; unsignedTX : string; P2SHscript : string) : string;
var s : string;
    sig1, sig2 : string;
    signedTX : string;
    inputSetCommand : string;
    contract : string;
begin
  //
  contract := ScriptDecode(P2SHscript);

  sig1 := InputSign(ECPrivKey1,contract,unsignedTX);
  sig2 := InputSign(ECPrivKey2,contract,unsignedTX);

  inputSetCommand := ' input-set "zero [ ' + sig1 + ' ] [ ' + sig2 + ' ] 1 [ ' + P2SHscript + ' ]" ' + unsignedTX;

  signedTX := AnyScript(inputSetCommand);
  Result := signedTX;

end;

function BITARefundScript(ECPrivKey1 : string; ECPubKey1 : string; unsignedTX : string; P2SHscript : string) : string;
var s : string;
    sig1, sig2 : string;
    signedTX : string;
    inputSetCommand : string;
    contract : string;
begin
  //
  contract := ScriptDecode(P2SHscript);
  sig1 := InputSign(ECPrivKey1,contract,unsignedTX);

  inputSetCommand := ' input-set "[ ' + sig1 + ' ]  [ ' + ECPubKey1 + ' ] zero [ ' + P2SHscript + ' ]" ' + unsignedTX;

  signedTX := AnyScript(inputSetCommand);
  Result := signedTX;
end;

function AnyScript(script : string) : string;
var cli : string;
    hasil : string;
begin
  hasil := '';
  while ((hasil = '') or (Trim(hasil) = 'timed out')) do
  begin
    cli := 'bx ' + script;
    hasil := GetDosOutput(cli,CurrentPath);
  end;
  Result := Trim(hasil);
end;

end.
