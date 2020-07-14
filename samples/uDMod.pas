unit uDMod;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.ExprFuncs,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.SQLite, uBlongToast,
  FMX.Dialogs, System.IOUtils, System.UITypes, FireDAC.FMXUI.Wait,
  FireDAC.Comp.UI, FMX.Types, FMX.Controls, FireDAC.Phys.FB, uUtilitas, FMX.GridExcelIO,
  FMX.TMSGrid, FMX.Forms, FMX.TMSFindDialog, System.Types, FMX.TMSGridData,
  FMX.Objects, FMX.Graphics, FMX.Memo, System.JSON;

type
  TDMod = class(TDataModule)
    FDCKonfig: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    QryKonfig: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    StyleBook1: TStyleBook;
    DBConn: TFDConnection;
    Qry1: TFDQuery;
    DBTrans: TFDTransaction;
    SaveDlgToXLS: TSaveDialog;
    ExcelIO: TTMSFMXGridExcelIO;
    OpenDlgXLS: TOpenDialog;
    procedure DataModuleCreate(Sender: TObject);
    procedure FDCKonfigBeforeConnect(Sender: TObject);
    procedure FDCKonfigAfterConnect(Sender: TObject);
  private
    { Private declarations }
    KonfigNamaDatabase : string;
    NamaDatabase : string;
    procedure KonfigInitiateTables;
    procedure KonfigDBConnect;
    procedure AmbilKonfig;

    procedure DrawQRCode(code : string; var imgQRCode : TImage);
  public
    { Public declarations }

  end;

  TTxHistory = record
    Count : integer;
    slRhash: TStringList;
    slRheight : TStringList;
    slRindex : TStringList;
    slShash : TStringList;
    slSheight : TStringList;
    slSindex : TStringList;
    slTvalue : TStringList;
    procedure Start;
    procedure Finish;
  end;

  TTxDetail = record
    Count : integer;
    inputCount : integer;
    outputCount : integer;
    hash : string;
    lockTime : string;
    versi : string ;
    iAddress : TStringList;
    iHash : TStringList;
    iIndex : TStringList;
    iScript : TStringList;
    iSequence : TStringList;
    oAddress : TStringList;
    oScript : TStringList;
    oValue : TStringList;
    oHash : TStringList;
    oIndex : TStringList;
    hasOpReturn : Boolean;
    height : string;
    idx : string;
    OpReturn: string;
    procedure Start;
    procedure Finish;
  end;

  TTxTaint = record
    address : string;
    slAddress2 : TStringList;
    slMethod : TStringList;
    slLvl : TStringList;
    slTvalue : TStringList;
    slHash : TStringList;
    slIdx : TStringList;
    procedure Start;
    procedure Finish;
  end;

  TTxBalance = record
    address : string;
    confirmed : real;
    received : real;
    unspent : real;
  end;

  TAddress = record
    slAddress : TStringList;
    slAmount : TStringList;
  end;

var
  DMod: TDMod;
  IsConnectedToDB : boolean;
  NamaPengguna, LevelUser : string;
  isLogged : boolean;
  PathAplikasi : string;
  Konfig : TKonfig;

const
  UsernameDB : string = 'SYSDBA';
  PassDB : string = 'masterkey';

function ShowKonfirmasi(Pesan : string) : boolean;
procedure ShowError(Pesan : string);
procedure ShowInfo(Pesan : string);
procedure Shout(Pesan : string; Tipe : integer = 1);
procedure ShowDebug(Pesan : string);
function QueryDB (Qry : TFDQuery; s : string) : boolean;
function ExecuteQueryDB (Qry : TFDQuery; s : string) : boolean;
function QueryDBddl (Conn : TFDConnection; s : string) : boolean;

function DBConnect(UserName,Pass:string) : boolean;
function DBDisconnect : boolean;
function SimpanGridKeExcel(ExcelIO :TTMSFMXGridExcelIO) : string;

procedure QryToTMSGrid(Qry : TFDQuery; s : string; Grid : TTMSFMXGrid);
procedure QryToTMSGridAkumulasi(Qry : TFDQuery; s : string; Grid : TTMSFMXGrid);
procedure GridToXLS(Grid : TTMSFMXGrid);
procedure XLSToGrid(namaFile : string; Grid : TTMSFMXGrid);
function isFrameCreated(namaForm : TForm; namaFrame : string) : boolean;
function frameComponentNumber(namaForm : TForm; namaFrame : string) : integer;

//bitcoin BX
procedure GetNewKeys(var seed : string; var idx : integer; var hdPrivKey : string; var ecPrivKey : string; var hdPubKey : string; var ecPubKey : string; var address : string);
function TxOpReturn (address : string; pesan : string) : string;
function CountMsgLength (pesan : string) : integer;
function AddressToPayToAddressScript (address : string) : string;
function PayToAddress (payerAddress : string; payeeAddress : string; changeAddress : string; amount : real) : string;
function PayToAddresses (payerAddress : string; payeeAddress : TStringList; changeAddress : string; amount : TStringList) : string;

//function CoinSwapCommit: string;
//procedure GetTxHistory(mode: string; address : string; var slRhash: TStringList; var slRheight : TStringList; var slRindex : TStringList; var slShash : TStringList; var slSheight : TStringList; var slSindex : TStringList; var slTvalue : TStringList);
//procedure GetTxDetail (txID : string; var hash : string; var lockTime : string; var versi : string ; var iAddress : TStringList; var iHash : TStringList; var iIndex : TStringList; var iScript : TStringList; var iSequence : TStringList; var oAddress : TStringList; var oScript : TStringList; var oValue : TStringList; var hasOpReturn : Boolean);

procedure GetTxHistory(mode: string; address : string; var TxHistory : TTxHistory);
procedure GetTxDetail (txID : string; var TxDetail : TTxDetail);
procedure GetTxIndex (txID : string; var height : string; var idx : string);
procedure GetAddressBalance (address : string; var TxBalance : TTxBalance);
procedure GetTaint (mode: string; address : string; var TxTaint : TTxTaint; depth : integer);
function ReadMsgInTx(txID: string) : string;

//QR Code
procedure DataToQR(code : string; var imgQRCode : TImage);





implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses uBX, DelphiZXingQRCode, uFrmDebug;

{$R *.dfm}

{ TDMod }

procedure TDMod.AmbilKonfig;
var s : string;
begin
  //ambil konfigurasi
  s := 'SELECT server, tipeserver, serverpath FROM tkonfig';
  QueryDB(QryKonfig,s);
  Konfig.Server := QryKonfig.FieldByName('server').AsString;
  Konfig.TipeServer := QryKonfig.FieldByName('tipeserver').AsInteger;
  Konfig.ServerPath := QryKonfig.FieldByName('serverpath').AsString;
end;

procedure TDMod.DataModuleCreate(Sender: TObject);
begin
  NamaPengguna := '';
  LevelUser := '';
  isLogged := False;
  KonfigNamaDatabase := 'konfig.s3db';
  NamaDatabase := 'BITACLIENT.GDB';
  KonfigDBConnect;
end;

procedure TDMod.FDCKonfigAfterConnect(Sender: TObject);
begin
  KonfigInitiateTables;
end;

procedure TDMod.FDCKonfigBeforeConnect(Sender: TObject);
begin
  {$IFDEF ANDROID}
  FDCKonfig.Params.Values['Database'] := System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, KonfignamaDatabase);
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  FDCKonfig.Params.Values['Database'] := System.IOUtils.TPath.Combine(ExtractFilePath(ParamStr(0)), KonfignamaDatabase);
  {$ENDIF}
end;

procedure TDMod.KonfigDBConnect;
begin
  if FDCKonfig.Connected then
  begin
    FDCKonfig.Connected := False;
  end;

  with FDCKonfig do
  begin
    Connected := True;
  end;
end;

procedure TDMod.KonfigInitiateTables;
var s : string;
    jumlahData : integer;
begin
  // tabel t_user
  s := 'CREATE TABLE IF NOT EXISTS tkonfig (server text, tipeserver text, serverpath text)';
  QueryDBddl(FDCKonfig,s);

  //mengisi konfig
  s := 'SELECT COUNT(*) FROM tkonfig';
  if QueryDB(QryKonfig,s) then jumlahData := QryKonfig.Fields[0].AsInteger
  else jumlahData := 0;
  if jumlahData = 0 then
  begin
    //server
    s := 'INSERT INTO tkonfig (server, tipeserver, serverpath) VALUES (%s,%s,%s)';
    s := Format(s,[QuotedStr('127.0.0.1'), QuotedStr('1'), QuotedStr(ExtractFilePath(ParamStr(0)) + 'db\' + NamaDatabase)]);
    //Shout(s);
    ExecuteQueryDB(QryKonfig,s);
  end;

end;

function QueryDB (Qry : TFDQuery; s : string) : boolean;
begin
  try
    with Qry do
    begin
      Close;
      SQL.Clear;
      SQL.Add(s);
      Open;
      //Result := OpenOrExecute;
      Result := True;
    end;
  except
    on E:Exception do
    begin
      Shout(E.ClassName + ' : ' + E.Message);
      Result := False;
    end;
  end;
end;

function ExecuteQueryDB (Qry : TFDQuery; s : string) : boolean;
begin
  try
    with Qry do
    begin
      Close;
      SQL.Clear;
      SQL.Add(s);
      //Open;
      ExecSQL;
      Result := True;
    end;
  except
    on E:Exception do
    begin
      Shout(E.ClassName + ' : ' + E.Message);
      Result := False;
    end;
  end;
end;


function QueryDBddl (Conn : TFDConnection; s : string) : boolean;
var hasil : integer;
begin
  try
    with Conn do
    begin
      //hasil := ExecSQL(s);
      //if hasil > 0 then Result := True else Result := False;
      ExecSQL(s);
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function ShowKonfirmasi(Pesan : string) : boolean;
begin
  if MessageDlg(Pesan,TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbOK,TMsgDlgBtn.mbCancel],0) = mrOK then Result := True
  else Result := False;
end;

procedure ShowError(Pesan : string);
begin
  MessageDlg(Pesan,TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
end;

procedure ShowInfo(Pesan : string);
begin
  MessageDlg(Pesan,TMsgDlgType.mtInformation,[TMsgDlgBtn.mbOK],0);
end;

procedure Shout(Pesan : string; Tipe : integer = 1);
begin
{$IFDEF ANDROID}
  case Tipe of
  0 : begin
        Toast(Msg,ShortToast);
      end;
  1 : begin
        Toast(Msg,LongToast);
      end;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  ShowInfo(Pesan);

  {$ENDIF}
end;

procedure ShowDebug(Pesan : string);
var frmDebug : TfrmDebug;
begin
  frmDebug := TfrmDebug.Create(Application);
  frmDebug.mmData.Text := Pesan;
  frmDebug.Show;
end;

function DBConnect(UserName,Pass:string) : boolean;
var PesanError, DatabasePath : string;
begin
  DMod.AmbilKonfig;

  with DMod.DBConn do
  begin
    try
      Close;
      Connected := False;
    except
      ShowError('Connection Failed');
    end;
  end;

  with DMod.DBConn do
  begin
    case Konfig.TipeServer of
      //using TCP/IP the syntax is <server_name>:<filename>.
      //using NetBEUI, the syntax is: \\<server_name>\<filename>.
      //using SPX, the syntax is: <server_name>@<filename>.
      0 : DatabasePath:=Konfig.ServerPath;
      1 : DatabasePath:=Konfig.Server+':'+Konfig.ServerPath;
      //2 : DatabasePath:='\\'+Konfig.Server+'\'+Konfig.ServerPath;
      //3 : DatabasePath:=Konfig.Server+'@'+Konfig.ServerPath;
    end;
    //DatabaseName := DatabasePath;
    {User_Name=sysdba
    Password=masterkey
    Database=D:\DELPHI PROJECTS\XE6-WIN\FB1\db\MUTASI.GDB
    DriverID=FB}
    Params.Clear;
    Params.Add('User_Name=' + UserName);
    Params.Add('Password=' + Pass);
    Params.Add('Database=' + DatabasePath);
    Params.Add('DriverID=FB');
    try
      Connected := true;
      Result := True;
    except
      on E: EFDDBEngineException do
      begin
        {Connected := False;
        case E.IBErrorCode of
          335544472 : PesanError := 'Pengguna atau Password Salah!'; // salah pengguna atau password
          335544344 : PesanError := 'Database Tidak Ditemukan!';     // databasePath tidak valid
          335544721 : PesanError := 'Server Tidak Ditemukan!';       // host tidak ada
          335544352 : PesanError := 'Database dalam keadaan Read-Only!';
          else
            PesanError := 'Error tidak ditangani!'+#13+'ID: ' +inttostr(E.IBErrorCode) + #13 + 'Pesan : ' +  E.Message;


        end; }
        //ShowError(PesanError);
        raise Exception.Create('Connection Failed');
        Result := False;
      end;
    end;
    //Result := Connected;
    IsConnectedToDB := Result;
  end;
end;
function DBDisconnect : boolean;
var i : integer;
begin
  with DMod.DBConn do
  begin
    try
      DMod.DBConn.Connected := False;
      Result := True;
      IsConnectedToDB := Result;
    except
      on E: EFDDBEngineException do
      begin
        raise Exception.Create('Connection Failed');
        Result := False;
        IsConnectedToDB := Result;
      end;

    end;
  end;


end;

function SimpanGridKeExcel(ExcelIO :TTMSFMXGridExcelIO) : string;
var namaBerkas : string;
    filetoopen : PChar;
begin
  try
    //DMod.SaveDlgToXLS.InitialDir := GetMyDocDir;
    if DMod.SaveDlgToXLS.Execute then
    begin
      namaBerkas := DMod.SaveDlgToXLS.FileName;
      ExcelIO.Options.ExportCellFormats := True;
      ExcelIO.XLSExport(namaBerkas,'Exported',0,1);
    end;
  finally
    Result := namaBerkas;
  end;
end;

procedure QryToTMSGrid(Qry : TFDQuery; s : string; Grid : TTMSFMXGrid);
var i,j : integer;
begin
  //bersihkan grid
  Grid.Clear;
  Grid.RowCount := 0;
  Grid.ColumnCount := 0;

  QueryDB(Qry,s);

  //nama kolom
  Grid.RowCount := Grid.RowCount + 1;
  for i := 0 to Qry.FieldCount - 1 do
  begin
    Grid.ColumnCount := i + 1;
    Grid.Cells[i,0] := Qry.Fields[i].FieldName;
    //Qry.Fields[i].DataType
  end;

  while not Qry.Eof do
  begin
    Grid.RowCount := Grid.RowCount + 1;
    for j := 0 to Qry.FieldCount - 1 do
    begin
      Grid.Cells[j,Grid.RowCount - 1] := Qry.Fields[j].AsString;
    end;

    Qry.Next;
  end;
  Qry.Active := False;
  Qry.Close;

  Grid.Update;
end;

procedure QryToTMSGridAkumulasi(Qry : TFDQuery; s : string; Grid : TTMSFMXGrid);
var i,j : integer;
begin
  //bersihkan grid
  {Grid.Clear;
  Grid.RowCount := 0;
  Grid.ColumnCount := 0;
  }

  QueryDB(Qry,s);
  //ShowMessage(inttostr(Qry.FieldCount));

  //nama kolom
  if (Grid.RowCount = 0) then
  begin
    Grid.RowCount := Grid.RowCount + 1;
    for i := 0 to Qry.FieldCount - 1 do
    begin
      Grid.ColumnCount := i + 1;
      Grid.Cells[i,0] := Qry.Fields[i].FieldName;
      //Qry.Fields[i].DataType
    end;
  end;

  while not Qry.Eof do
  begin
    Grid.RowCount := Grid.RowCount + 1;
    for j := 0 to Qry.FieldCount - 1 do
    begin
      Grid.Cells[j,Grid.RowCount - 1] := Qry.Fields[j].AsString;
    end;

    Qry.Next;
  end;
  Qry.Active := False;
  Qry.Close;
end;

procedure GridToXLS(Grid : TTMSFMXGrid);
begin
  if DMod.SaveDlgToXLS.Execute then
  begin
    DMod.ExcelIO.Grid := Grid;
    with DMod.ExcelIO do
    begin
      XLSExport(DMod.SaveDlgToXLS.FileName, 'Exported');
    end;
  end;

end;

procedure XLSToGrid(namaFile : string; Grid : TTMSFMXGrid);
begin
  DMod.ExcelIO.Grid := Grid;
  with DMod.ExcelIO do
  begin
    XLSImport(namaFile);
  end;
end;

function isFrameCreated(namaForm : TForm; namaFrame : string) : boolean;
var i : integer;
    ada : boolean;
begin
  ada := False;
  for i := 0 to namaForm.ComponentCount - 1 do
  begin
    if namaForm.Components[i] is TFrame then
    begin
      if namaForm.Components[i].Name = namaFrame then
      ada := True;
      //namaForm.Components[i].Free;
    end;
  end;

  Result := ada;
end;

function frameComponentNumber(namaForm : TForm; namaFrame : string) : integer;
var i : integer;
    ada : integer;
begin
  ada := -1;
  for i := 0 to namaForm.ComponentCount - 1 do
  begin
    if namaForm.Components[i] is TFrame then
    begin
      if namaForm.Components[i].Name = namaFrame then
      ada := i;
      //namaForm.Components[i].Free;
    end;
  end;

  Result := ada;
end;

procedure GetNewKeys(var seed : string; var idx : integer; var hdPrivKey : string; var ecPrivKey : string; var hdPubKey : string; var ecPubKey : string; var address : string);
var hdPrivKeyTemp, ecPrivKeyTemp : string;
begin
  if seed = '' then
  begin
    seed := GetNewSeed;
    hdPrivKey := GetHDPrivKeyFromSeed(seed);
    ecPrivKey := GetECPrivKeyFromSeed(seed);
    hdPubKey := GetHDPubKeyFromPrivKey(hdPrivKey);
    ecPubKey := HDtoEC(hdPubKey);
    address := GetAddrFromHDKey(hdPubKey);
  end
  else
  begin
    hdPrivKeyTemp := GetHDPrivKeyFromSeed(seed);
    ecPrivKeyTemp := GetECPrivKeyFromSeed(seed);
    hdPrivKey := GetNewHDPrivKeyFromPrivKeyIdx(hdPrivKeyTemp, idx);
    ecPrivKey := HDtoEC(hdPrivKey);
    hdPubKey := GetHDPubKeyFromPrivKey(hdPrivKey);
    ecPubKey := HDtoEC(hdPubKey);
    address := GetAddrFromHDKey(hdPubKey);
  end;
end;

function CountMsgLength (pesan : string) : integer;
var pesanEncoded, skrip, skripEncoded, TxOutNum, MsgValue, MsgLen : string;
begin
  //base16
  pesanEncoded := Base16Encode(pesan);
  //base64
  //pesanEncoded := Base64Encode(pesan);
  skrip := 'return [ ' + pesanEncoded + ' ]';
  skripEncoded := ScriptEncode(skrip);
  TxOutNum := IntToHex(2,2);
  MsgValue := IntToHex(0,16);
  //MsgLen := LowerCase(IntToHex(Length(skripEncoded) div 2, 2));
  Result := Length(skripEncoded) div 2;
end;

function TxOpReturn (address : string; pesan : string) : string;
var pesanEncoded, skrip, skripEncoded : string;
    biayaTx, saldo, tvalue : integer;
    s : string;
    TxIn: string;
    TxInCount : integer;
    rhash, rindex : TStringList;
    TxInNum : string;
    i : integer;
    swappingByte,swappedByte : string;
    TxOutNum : string;
    MsgValue : string;
    MsgLen : string;
    TxOut : string;
    TxOutLockTime : string;
    ChgValue : string;
    ChgpubKey : string;
    ChgpubKeyHash160 : string;
    skripLength : string;
    mmData : TMemo;
    sl : TStringList;
    lockTime : string;
    protocolVersion : string;
    UnsignedTx, SignedTx : string;
    pubKey, privKey, pubKeyHash160 : string;
    skripAddress : string;
    signature : string;
begin

  saldo := 0;
  s := 'SELECT unspent FROM tbalance WHERE address = %s';
  s := Format(s,[QuotedStr(address)]);
  QueryDB(DMod.Qry1,s);
  if not Dmod.Qry1.Eof then saldo := DMod.Qry1.Fields[0].AsInteger;

  s := 'SELECT ec_pubkey, ec_privkey FROM tkey WHERE address = %s';
  s := Format(s,[QuotedStr(address)]);
  QueryDB(DMod.Qry1,s);
  if not DMod.Qry1.Eof then
  begin
    pubKey := DMod.Qry1.Fields[0].AsString;
    privKey := DMod.Qry1.Fields[1].AsString;
  end;

  pubKeyHash160:= Bitcoin160(pubKey);
  skripAddress := 'dup hash160 [ ' + pubKeyHash160 + ' ] equalverify checksig';

  //biaya transaksi dan Tx inputs
  biayaTx := TxFee(1,2);
  TxInCount := 0;

  rhash := TStringList.Create;
  rindex := TStringList.Create;

  if (saldo > biayaTx) then
  begin
    tvalue := 0;
    s := 'SELECT rhash, rindex, tvalue FROM thistory WHERE address = %s and shash = %s ORDER BY tvalue DESC';
    s := Format(s,[QuotedStr(address), QuotedStr('')]);
    QueryDB(DMod.Qry1,s);
    if not Dmod.Qry1.Eof then
    begin
      while tvalue < biayaTx do
      begin
        rhash.Add(DMod.Qry1.Fields[0].AsString);
        rindex.Add(IntToHex(DMod.Qry1.Fields[1].AsInteger,8));
        tvalue := tvalue + DMod.Qry1.Fields[2].AsInteger;
        TxInCount := TxInCount + 1;
        if (TxInCount > 1) then
        begin
          biayaTx := TxFee(TxInCount,2);
        end;
        DMod.Qry1.Next;
      end;
    end;
  end;

  //construct TxIn
  TxIn := '';
  TxInNum := IntToHex(TxInCount,2);
  TxIn := TxIn + TxInNum;
  for i := 0 to rhash.Count - 1 do
  begin
    swappingByte := rhash[i];
    SwapByte(swappingByte,swappedByte);
    TxIn := TxIn + swappedByte;
    swappingByte := rindex[i];
    SwapByte(swappingByte,swappedByte);
    TxIn := TxIn + swappedByte;
    //scriptsig length for unsigned tx
    TxIn := TxIn + '00';
    //sequence number
    TxIn := TxIn + 'ffffffff';
  end;
  rhash.Free;
  rindex.Free;

  //TxOut
  //base16
  pesanEncoded := Base16Encode(pesan);
  //base64
  //pesanEncoded := Base64Encode(pesan);
  skrip := 'return [ ' + pesanEncoded + ' ]';
  skripEncoded := ScriptEncode(skrip);
  TxOutNum := IntToHex(2,2);
  MsgValue := IntToHex(0,16);
  MsgLen := LowerCase(IntToHex(Length(skripEncoded) div 2, 2));
  //TxOut pertama : pesan
  TxOut := '';
  TxOut := TxOut + TxOutNum + MsgValue + MsgLen + skripEncoded;

  //TxOut kedua : change address
  ChgValue := IntToHex((tvalue - biayaTx),16);
  swappingByte := ChgValue;
  SwapByte(swappingByte,swappedByte);
  ChgValue := swappedByte;
  //pubkey
  mmData := TMemo.Create(DMod);
  mmData.Text := AddressDecode(address);
  if (mmData.Lines.Count > 3) then
  begin
    //Shout(mmData.Lines[3]);
    sl := TStringList.Create;
    Explode(sl,Trim(mmData.Lines[3]), ' ');
    //the result is already a hash of public key
    ChgpubKeyHash160 := sl[1];
    //Shout(sl[0] + ' - ' + sl[1]);
    //ChgpubKeyHash160:= Bitcoin160(ChgpubKey);
    //Shout(ChgpubKeyHash160);
    sl.Free;
  end
  else
  begin
    //get the EC public key of the address
    s := 'SELECT ec_pubkey FROM tkey WHERE address = %s';
    s := Format(s,[QuotedStr(address)]);
    QueryDB(DMod.Qry1,s);
    ChgpubKey := DMod.Qry1.Fields[0].AsString;
    ChgpubKeyHash160:= Bitcoin160(ChgpubKey);
  end;
  mmData.Free;
  //get the EC public key of the address
  {s := 'SELECT ec_pubkey FROM tkey WHERE address = %s';
  s := Format(s,[QuotedStr(address)]);
  QueryDB(DMod.Qry1,s);
  ChgpubKey := DMod.Qry1.Fields[0].AsString;
  ChgpubKeyHash160:= Bitcoin160(ChgpubKey); }

  skrip := 'dup hash160 [ ' + ChgpubKeyHash160 + ' ] equalverify checksig';
  skripEncoded := ScriptEncode(skrip);
  skripLength := LowerCase(IntToHex(Length(skripEncoded) div 2, 2));
  //count the number of bytes of the script

  lockTime := '00000000';

  TxOut := TxOut + ChgValue + skripLength + skripEncoded + lockTime;
  protocolVersion := '01000000';

  UnsignedTx := protocolVersion + TxIn + TxOut;

  //signing the transaction
  //function InputSign (PrivKey : string; Contract : string; Trx : string) : string;


  SignedTx := UnsignedTx;
  //SignedTx := InputSet(signature,pubKey,SignedTx,0);
  for i := 0 to TxInCount - 1 do
  begin
    signature := InputSign(privKey,skripAddress,UnsignedTx, i, 'all');
    SignedTx := InputSet(signature,pubKey,SignedTx,i);
  end;

  if (ValidateTx(SignedTx)) then Result := SignedTx
    else Result := 'Tx invalid';

end;

function AddressToPayToAddressScript (address : string) : string;
var mmData : TMemo;
    sl : TStringList;
    ChgpubKeyHash160, skrip : string;
begin
  mmData := TMemo.Create(DMod);
  mmData.Text := AddressDecode(address);
  if (mmData.Lines.Count > 3) then
  begin
    sl := TStringList.Create;
    Explode(sl,Trim(mmData.Lines[3]), ' ');
    //the result is already a hash of public key
    ChgpubKeyHash160 := sl[1];
    sl.Free;
  end
  else
  begin
    ChgpubKeyHash160 := '';
  end;
  mmData.Free;

  skrip := 'dup hash160 [ ' + ChgpubKeyHash160 + ' ] equalverify checksig';
  Result := skrip;
end;

function PayToAddress (payerAddress : string; payeeAddress : string; changeAddress : string; amount : real) : string;
var s : string;
    TxHistory : TTxHistory;
    i : integer;
    slTXID, slIdx, slSignature : TStringList;
    totalAmount, changeAmount : real;
    txFee : real;
    unsignedTX, command, inputs, outputs, signature, signedTX : string;
    TxBalance : TTxBalance;
    inputCount, outputCount : integer;
    ecPrivKey, ecPubKey : string;
begin
  //use standard txFee of 10000 satoshi but to make it faster, make it 20000
  txFee := 20000;

  //get balance
  GetAddressBalance(payerAddress,TxBalance);
  if TxBalance.unspent < (amount + txFee) then
  begin
    //
    Result := 'Insufficient Fund';
  end
  else
  begin
    //get UTXO
    TxHistory.Start;
    GetTxHistory('UNSPENT',payerAddress,TxHistory);
    slTXID := TStringList.Create;
    slIdx := TStringList.Create;
    totalAmount := 0;
    inputCount := 0;
    for i:= 0 to TxHistory.Count - 1 do
    begin
      //
      if totalAmount < (amount + txFee) then
      begin
        slTXID.Add(TxHistory.slRhash[i]);
        slIdx.Add(TxHistory.slRindex[i]);
        inputCount := inputCount + 1;
        totalAmount := totalAmount + StrToFloat(TxHistory.slTvalue[i]);
      end;
    end;
    TxHistory.Finish;

    //construct the transaction
    inputs := '';
    for i := 0 to inputCount - 1 do
    begin
      //
      inputs := inputs + ' -i ' + slTXID[i] + ':' + slIdx[i];
    end;
    changeAmount := totalAmount - amount - txFee;
    outputs := ' -o ' + payeeAddress + ':' + FloatToStr(amount);
    if changeAmount > 0 then
      outputs := outputs + ' -o ' + changeAddress + ':' + FloatToStr(changeAmount);
    command := 'tx-encode' + inputs + outputs;
    unsignedTX := AnyScript(command);

    s := 'SELECT ec_privkey, ec_pubkey FROM tkey WHERE address = %s';
    s := Format(s,[QuotedStr(payerAddress)]);
    QueryDB(DMod.Qry1,s);
    ecPrivKey := DMod.Qry1.Fields[0].AsString;
    ecPubKey := DMod.Qry1.Fields[1].AsString;

    signedTX := unsignedTX;
    for i := 0 to inputCount - 1 do
    begin
      //create signature
      signature := InputSign(ecPrivKey,AddressToPayToAddressScript(payerAddress),unsignedTX,i);
      signedTX := InputSet(signature,ecPubKey,signedTX,i);
    end;

    slTXID.Free;
    slIdx.Free;

    Result := signedTX;
  end;
end;

function PayToAddresses (payerAddress : string; payeeAddress : TStringList; changeAddress : string; amount : TStringList) : string;
var s : string;
    TxHistory : TTxHistory;
    i : integer;
    slTXID, slIdx, slSignature : TStringList;
    totalAmount, changeAmount : real;
    Fee : real;
    unsignedTX, command, inputs, outputs, signature, signedTX : string;
    TxBalance : TTxBalance;
    inputCount, outputCount : integer;
    ecPrivKey, ecPubKey : string;
    totalAmountSent : real;
begin


  //calculate total amount needed
  totalAmountSent := 0;
  for i := 0 to amount.Count-1 do
  begin
    totalAmountSent := totalAmountSent + StrToFloat(amount[i]);
  end;

  //use standard txFee of 10000 satoshi, make it 20000 to ensure the confirmation speed
  Fee := 20000;
  Fee := TxFee(1,payeeAddress.Count);
  //calculate TxFee
  //get UTXO
  TxHistory.Start;
  GetTxHistory('UNSPENT',payerAddress,TxHistory);
  slTXID := TStringList.Create;
  slIdx := TStringList.Create;
  totalAmount := 0;
  inputCount := 0;
  for i:= 0 to TxHistory.Count - 1 do
  begin
    //
    if totalAmount < (totalAmountSent + Fee) then
    begin
      slTXID.Add(TxHistory.slRhash[i]);
      slIdx.Add(TxHistory.slRindex[i]);
      inputCount := inputCount + 1;
      totalAmount := totalAmount + StrToFloat(TxHistory.slTvalue[i]);
      Fee := TxFee(slTXID.Count, payeeAddress.Count);

    end;
  end;
  TxHistory.Finish;
  slTXID.Free;
  slIdx.Free;

  //Shout('Total amount sent : ' + FloatToStr(totalAmountSent) + ' satoshi. Fee : ' + FloatToStr(Fee) );

  //get balance
  GetAddressBalance(payerAddress,TxBalance);
  if TxBalance.unspent < (totalAmountSent + Fee) then
  begin
    //
    Result := 'Insufficient Fund. Need ' + FloatToStr(totalAmountSent + Fee) + ' satoshis';
  end
  else
  begin
    //get UTXO
    TxHistory.Start;
    GetTxHistory('UNSPENT',payerAddress,TxHistory);
    slTXID := TStringList.Create;
    slIdx := TStringList.Create;
    totalAmount := 0;
    inputCount := 0;
    for i:= 0 to TxHistory.Count - 1 do
    begin
      //
      if totalAmount < (totalAmountSent + Fee) then
      begin
        slTXID.Add(TxHistory.slRhash[i]);
        slIdx.Add(TxHistory.slRindex[i]);
        inputCount := inputCount + 1;
        totalAmount := totalAmount + StrToFloat(TxHistory.slTvalue[i]);
      end;
    end;
    TxHistory.Finish;

    //construct the transaction
    //inputs
    inputs := '';
    for i := 0 to inputCount - 1 do
    begin
      //
      inputs := inputs + ' -i ' + slTXID[i] + ':' + slIdx[i];
    end;

    //outputs
    outputs := '';
    for i := 0 to payeeAddress.Count-1 do
    begin
      if i <= amount.Count-1 then
      begin
        outputs := outputs + ' -o ' + payeeAddress[i] + ':' + amount[i];
      end;
    end;

    //change
    changeAmount := totalAmount - totalAmountSent - Fee;
    if changeAmount > 0 then
      outputs := outputs + ' -o ' + changeAddress + ':' + FloatToStr(changeAmount);
    command := 'tx-encode' + inputs + outputs;

    //Shout('unsigned tx: ' + command);
    unsignedTX := AnyScript(command);

    s := 'SELECT ec_privkey, ec_pubkey FROM tkey WHERE address = %s';
    s := Format(s,[QuotedStr(payerAddress)]);
    QueryDB(DMod.Qry1,s);
    ecPrivKey := DMod.Qry1.Fields[0].AsString;
    ecPubKey := DMod.Qry1.Fields[1].AsString;

    signedTX := unsignedTX;
    //Shout('unsigned tx: ' + unsignedTX);
    for i := 0 to inputCount - 1 do
    begin
      //create signature
      signature := InputSign(ecPrivKey,AddressToPayToAddressScript(payerAddress),signedTX,i);
      signedTX := InputSet(signature,ecPubKey,signedTX,i);
    end;
    //Shout('signed tx: ' + signedTX);
    slTXID.Free;
    slIdx.Free;

    Result := signedTX;
  end;
  //
end;

//procedure GetTxHistory(mode: string; address : string; var slRhash: TStringList; var slRheight : TStringList; var slRindex : TStringList; var slShash : TStringList; var slSheight : TStringList; var slSindex : TStringList; var slTvalue : TStringList);
procedure GetTxHistory(mode: string; address : string; var TxHistory : TTxHistory);
var s : string;
    i : integer;
    sl : TStringList;
    confirmed, received, unspent : string;
    TJsonObj  : TJSONObject;
    LJsonObj  : TJSONObject;
    LJPair    : TJSONPair;
    LProducts : TJSONValue;
    LProduct  : TJSONValue;
    LItem     : TJSONValue;
    LIndex    : Integer;
    LSize     : Integer;
    rhash, rheight, rindex, shash, sheight, sindex, tvalue : string;
    id : string;
    mmData : TMemo;
begin

  //
  mmData := TMemo.Create(Application);
  mmData.Text := FetchHistory(address);
  //Shout(mmData.Text);
  //Shout('transfers ""');
  rhash := '';
  rheight := '';
  rindex := '';
  shash := '';
  sheight := '';
  sindex := '';
  tvalue := '0';
  TxHistory.Count := 0;
  if (mmData.Lines.Count > 3) then
  begin
    mmData.Text := FetchHistory(address, 'json');
    TJsonObj := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(mmData.Text),0) as TJSONObject;
    mmData.Text := TJsonObj.ToString;
    LJsonObj := TJsonObj.Get(0).JsonValue as TJSONObject;
    try
      for i := 0 to LJsonObj.Count - 1 do
      begin
        LProducts:=LJsonObj.Get(i).JsonValue;
        LSize:=TJSONArray(LProducts).Size;
        rhash := '';
        rheight := '';
        rindex := '';
        shash := '';
        sheight := '';
        sindex := '';
        tvalue := '0';
        for LIndex:=0 to LSize-1 do
        begin
          LProduct := TJSONArray(LProducts).Get(LIndex);
          LJPair   := TJSONPair(LProduct);

          if (LJPair.JsonString.Value <> 'value') then
          begin
            //shout(LJPair.JsonString.Value);
            if (LJPair.JsonString.Value = 'received') then
            begin
              for LItem in TJSONArray(LJPair.JsonValue) do
              begin
                //Shout(TJSONPair(LItem).JsonString.Value);
                if TJSONPair(LItem).JsonString.Value = 'hash' then rhash := TJSONPair(LItem).JsonValue.Value
                  else if TJSONPair(LItem).JsonString.Value = 'height' then rheight := TJSONPair(LItem).JsonValue.Value
                    else if TJSONPair(LItem).JsonString.Value = 'index' then rindex := TJSONPair(LItem).JsonValue.Value
              end;
            end
            else if (LJPair.JsonString.Value = 'spent') then
            begin
              for LItem in TJSONArray(LJPair.JsonValue) do
              begin
                //Shout(TJSONPair(LItem).JsonString.Value);
                if TJSONPair(LItem).JsonString.Value = 'hash' then shash := TJSONPair(LItem).JsonValue.Value
                  else if TJSONPair(LItem).JsonString.Value = 'height' then sheight := TJSONPair(LItem).JsonValue.Value
                    else if TJSONPair(LItem).JsonString.Value = 'index' then sindex := TJSONPair(LItem).JsonValue.Value
              end;
            end;
          end
          else
          begin
            // value
            for LProduct in TJSONArray(LProducts) do
            begin
              if (TJSONPair(LProduct).JsonValue.Value <>'') then
                if TJSONPair(LProduct).JsonString.Value = 'value' then tvalue := TJSONPair(LProduct).JsonValue.Value;
            end;
          end;
       end;
       // end of a history record
       //put in stringlist
       if (((mode = 'UNSPENT') and (shash = '')) or (mode = 'ALL') or ((mode = 'SPENT') and (shash <> ''))) then
       begin
         //
         TxHistory.Count := TxHistory.Count + 1;
         TxHistory.slRhash.Add(rhash);
         TxHistory.slRheight.Add(rheight);
         TxHistory.slRindex.Add(rindex);
         TxHistory.slShash.Add(shash);
         TxHistory.slSheight.Add(sheight);
         TxHistory.slSindex.Add(sindex);
         TxHistory.slTvalue.Add(tvalue);
       end;
      end;
    finally
       LJsonObj.Free;
    end;
  end;
  mmData.Free;
end;

//procedure GetTxDetail (txID : string; var hash : string; var lockTime : string; var versi : string ; var iAddress : TStringList; var iHash : TStringList; var iIndex : TStringList; var iScript : TStringList; var iSequence : TStringList; var oAddress : TStringList; var oScript : TStringList; var oValue : TStringList; var hasOpReturn : Boolean);
procedure GetTxDetail (txID : string; var TxDetail : TTxDetail);
var s : string;
    i : integer;
    sl : TStringList;
    confirmed, received, unspent : string;
    TJsonObj  : TJSONObject;
    LJsonObj  : TJSONObject;
    LJPair    : TJSONPair;
    LProducts : TJSONValue;
    LProduct  : TJSONValue;
    LItem     : TJSONValue;
    LDetail   : TJSONValue;
    LDetailItem : TJSONValue;
    LDetailSize : Integer;
    LDetailIndex : Integer;
    LIndex    : Integer;
    LSize     : Integer;
    rhash, rheight, rindex, shash, sheight, sindex, tvalue : string;
    thash, tlockTime, tversi, tiAddress, tiHash, tiIndex, tiScript, tiSequence, toAddress, toScript, toValue, toHash : string;
    id : string;
    mmData : TMemo;
    jumLItem : integer;
    isAddressExist : boolean;
begin

  //
  mmData := TMemo.Create(Application);
  mmData.Text := FetchTx(txID);
  //Shout(mmData.Text);
  //Shout('transfers ""');
  rhash := '';
  rheight := '';
  rindex := '';
  shash := '';
  sheight := '';
  sindex := '';
  tvalue := '0';
  TxDetail.Count := 0;
  TxDetail.inputCount := 0;
  TxDetail.outputCount := 0;
  TxDetail.hasOpReturn := False;
  if (mmData.Lines.Count > 3) then
  begin
    mmData.Text := FetchTx(txID, 'json');
    TJsonObj := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(mmData.Text),0) as TJSONObject;
    mmData.Text := TJsonObj.ToString;
    LJsonObj := TJsonObj.Get(0).JsonValue as TJSONObject;
    try
      for i := 0 to LJsonObj.Count - 1 do
      begin
        //separate the value
        //Shout(LJsonObj.Get(i).JsonString.Value);
        if (Trim(LJsonObj.Get(i).JsonString.Value) = 'hash') then
        begin
          TxDetail.hash := LJsonObj.Get(i).JsonValue.Value;
          //Shout(hash);
        end
        else
        if (Trim(LJsonObj.Get(i).JsonString.Value) = 'inputs') then
        begin
          //Shout(LJsonObj.Get(i).JsonValue.ToString);
          //get the input
          LProducts:=LJsonObj.Get(i).JsonValue;
          LSize:=TJSONArray(LProducts).Size;
          TxDetail.inputCount := LSize;
          for LIndex:=0 to LSize-1 do
          begin
            LProduct := TJSONArray(LProducts).Get(LIndex);
            LJPair   := TJSONPair(LProduct);
            //Shout(LJPair.ToString);
            //input
            for LItem in TJSONArray(LJPair.JsonValue) do
            begin
              if TJSONPair(LItem).JsonString.Value = 'address' then TxDetail.iAddress.Add(TJSONPair(LItem).JsonValue.Value)
                else if TJSONPair(LItem).JsonString.Value = 'script' then TxDetail.iScript.Add(TJSONPair(LItem).JsonValue.Value)
                  else if TJSONPair(LItem).JsonString.Value = 'sequence' then TxDetail.iSequence.Add(TJSONPair(LItem).JsonValue.Value)
                    else if TJSONPair(LItem).JsonString.Value = 'previous_output' then
                    begin
                      LDetail := TJSONArray(LItem);
                      for LDetailItem in TJSONArray(TJSONPair(LDetail).JsonValue) do
                      begin
                        if (TJSONPair(LDetailItem).JsonString.Value = 'hash') then TxDetail.iHash.Add(TJSONPair(LDetailItem).JsonValue.Value)
                          else if (TJSONPair(LDetailItem).JsonString.Value = 'index') then TxDetail.iIndex.Add(TJSONPair(LDetailItem).JsonValue.Value);

                      end;
                    end;
            end;
          end;
        end
        else
        if (Trim(LJsonObj.Get(i).JsonString.Value) = 'lock_time') then
        begin
          TxDetail.lockTime := LJsonObj.Get(i).JsonValue.Value;
        end
        else
        if (Trim(LJsonObj.Get(i).JsonString.Value) = 'outputs') then
        begin
          //get the output
          LProducts:=LJsonObj.Get(i).JsonValue;
          LSize:=TJSONArray(LProducts).Size;
          TxDetail.outputCount := LSize;
          for LIndex:=0 to LSize-1 do
          begin
            LProduct := TJSONArray(LProducts).Get(LIndex);
            LJPair   := TJSONPair(LProduct);
            //Shout(LJPair.ToString);
            //previous_output
            jumLItem := 0;
            isAddressExist := False;
            for LItem in TJSONArray(LJPair.JsonValue) do
            begin
              //Shout(TJSONPair(LItem).JsonValue.Value);
              //Shout(TJSONPair(LItem).JsonString.Value + ' - ' + TJSONPair(LItem).JsonValue.Value);
              if Trim(TJSONPair(LItem).JsonString.Value) = 'address' then
              begin
                TxDetail.oAddress.Add(Trim(TJSONPair(LItem).JsonValue.Value));
                isAddressExist := True;
                //Shout(TJSONPair(LItem).JsonString.Value + ' - ' + TJSONPair(LItem).JsonValue.Value);
                //Shout(TxDetail.oAddress[TxDetail.oAddress.Count-1]);
              end
                else if Trim(TJSONPair(LItem).JsonString.Value) = 'script' then
                begin
                  TxDetail.oScript.Add(Trim(TJSONPair(LItem).JsonValue.Value));
                  if not isAddressExist then TxDetail.OpReturn := Trim(TJSONPair(LItem).JsonValue.Value);
                end
                  else if Trim(TJSONPair(LItem).JsonString.Value) = 'value' then TxDetail.oValue.Add(Trim(TJSONPair(LItem).JsonValue.Value));
              jumLItem := jumLItem + 1;
            end;
            TxDetail.oIndex.Add(IntToStr(LIndex));
            //if op_return then only has 2 entries
            //if LSize < 3 then
            //Shout(IntToStr(jumLItem));
            //if jumLItem < 3 then
            if not isAddressExist then
            begin
              //Shout(TxDetail.oAddress[TxDetail.oAddress.Count-1]);
              //Shout('no address');
              TxDetail.oAddress.Add('');
              if (TxDetail.hasOpReturn = false) then
              begin
                TxDetail.hasOpReturn := True;
                //Shout('has op return');
              end;
            end;
            //mock data
            TxDetail.oHash.Add(TxDetail.hash);
          end;
        end
        else
        if (Trim(LJsonObj.Get(i).JsonString.Value) = 'version') then
        begin
          TxDetail.versi := LJsonObj.Get(i).JsonValue.Value;
        end;
      end;
    finally
       LJsonObj.Free;
    end;
  end;
  mmData.Free;

  GetTxIndex(TxDetail.hash,TxDetail.height, TxDetail.idx);

end;

procedure GetTxIndex (txID : string; var height : string; var idx : string);
var mmData : TMemo;
    sl : TStringList;
begin
  height := '';
  idx := '';
  mmData := TMemo.Create(Application);
  mmData.Text := FetchTxIndex(txID);
  if mmData.Lines.Count > 5 then
  begin
    //height
    sl := TStringList.Create;
    Explode(sl, Trim(mmData.Lines[3]), ' ');
    if sl.Count > 1 then
    begin
      height := sl[1];
    end;

    //idx
    sl := TStringList.Create;
    Explode(sl, Trim(mmData.Lines[4]), ' ');
    if sl.Count > 1 then
    begin
      idx := sl[1];
    end;
  end;

end;

procedure GetAddressBalance (address : string; var TxBalance : TTxBalance);
var mmData : TMemo;
    sl : TStringList;
begin
  TxBalance.address := address;
  TxBalance.confirmed := 0;
  TxBalance.received := 0;
  TxBalance.unspent := 0;
  mmData := TMemo.Create(Application);
  mmData.Text := FetchBalance(address);
  if mmData.Lines.Count > 5 then
  begin
    //confirmed
    sl := TStringList.Create;
    Explode(sl, Trim(mmData.Lines[3]), ' ');
    if sl.Count > 1 then
    begin
      TxBalance.confirmed := sl[1].ToDouble;
    end;

    //received
    sl := TStringList.Create;
    Explode(sl, Trim(mmData.Lines[4]), ' ');
    if sl.Count > 1 then
    begin
      TxBalance.received := sl[1].ToDouble;
    end;

    //unspent
    sl := TStringList.Create;
    Explode(sl, Trim(mmData.Lines[5]), ' ');
    if sl.Count > 1 then
    begin
      TxBalance.unspent := sl[1].ToDouble;
    end;
  end;
end;

procedure TDMod.DrawQRCode(code: string; var imgQRCode: TImage);
var
  QRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
  pixelColor : TAlphaColor;
  vBitMapData : TBitmapData;
  rSrc, rDest : TRectF;
  s : widestring;
  QRCodeBitmap : TBitmap;
begin
  imgQRCode.DisableInterpolation := true;
  imgQRCode.WrapMode := TImageWrapMode.iwStretch;

  QRCode := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Data := code;
    QRCode.Encoding := TQRCodeEncoding(0);
    QRCode.QuietZone := StrToIntDef('2', 4);
    QRCodeBitmap.SetSize(QRCode.Rows, QRCode.Columns);
    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
          pixelColor := TAlphaColors.Black
        else
          pixelColor := TAlphaColors.White;
        if QRCodeBitmap.Map(TMapAccess.maWrite, vBitMapData)  then
        try
          vBitMapData.SetPixel(Column, Row, pixelColor);
        finally
          QRCodeBitmap.Unmap(vBitMapData);
        end;
      end;
    end;
  finally
    QRCode.Free;
    QRCodeBitmap.Free;
  end;

  //refresh image control imgQRCode is a TImage
  imgQRCode.Bitmap.SetSize(QRCodeBitmap.Width, QRCodeBitmap.Height);
  rSrc := TRectF.Create(0, 0, QRCodeBitmap.Width, QRCodeBitmap.Height);
  rDest := TRectF.Create(0, 0, imgQRCode.Bitmap.Width, imgQRCode.Bitmap.Height);
  if imgQRCode.Bitmap.Canvas.BeginScene then
  try
    //ShowMessage(address);

    imgQRCode.Bitmap.Canvas.Clear(TAlphaColors.White);
    imgQRCode.Bitmap.Canvas.DrawBitmap(QRCodeBitmap, rSrc, rDest, 1);
  finally
    imgQRCode.Bitmap.Canvas.EndScene;
  end
end;

procedure DataToQR(code : string; var imgQRCode : TImage);
begin
  DMod.DrawQRCode(code, imgQRCode);
  DMod.DrawQRCode(code, imgQRCode);
  DMod.DrawQRCode(code, imgQRCode);
  DMod.DrawQRCode(code, imgQRCode);
end;

{ TTxDetail }

procedure TTxDetail.Start;
begin
  iAddress := TStringList.Create;
  iHash := TStringList.Create;
  iIndex := TStringList.Create;
  iScript := TStringList.Create;
  iSequence := TStringList.Create;
  oAddress := TStringList.Create;
  oScript := TStringList.Create;
  oValue := TStringList.Create;
  oHash := TStringList.Create;
  oIndex := TStringList.Create;
end;

procedure TTxDetail.Finish;
begin
  iAddress.Free;
  iHash.Free;
  iIndex.Free;
  iScript.Free;
  iSequence.Free;
  oAddress.Free;
  oScript.Free;
  oValue.Free;
  oHash.Free;
  oIndex.Free;
end;

{ TTxHistory }

procedure TTxHistory.Finish;
begin
  slRhash.Free;
  slRheight.Free;
  slRindex.Free;
  slShash.Free;
  slSheight.Free;
  slSindex.Free;
  slTvalue.Free;
end;

procedure TTxHistory.Start;
begin
  slRhash := TStringList.Create;
  slRheight := TStringList.Create;
  slRindex := TStringList.Create;
  slShash := TStringList.Create;
  slSheight := TStringList.Create;
  slSindex := TStringList.Create;
  slTvalue := TStringList.Create;
end;

{ TTxTaint }

procedure TTxTaint.Finish;
begin
  slAddress2.Free;
  slMethod.Free;
  slLvl.Free;
  slTvalue.Free;
  slHash.Free;
  slIdx.Free;
end;

procedure TTxTaint.Start;
begin
  slAddress2 := TStringList.Create;
  slMethod := TStringList.Create;
  slLvl := TStringList.Create;
  slTvalue:= TStringList.Create;
  slHash := TStringList.Create;
  slIdx := TStringList.Create;
end;

procedure GetTaint (mode: string; address : string; var TxTaint : TTxTaint; depth : integer);
var txHistory : TTxHistory;
    txDetail : TTxDetail;
    txBalance : TTxBalance;
    i, j, k, l, m, n : integer;
    totalValue : real;
    keepDigging, keepLooking : boolean;
    hash, idx : TStringList;
begin
  //
  hash := TStringList.Create;
  idx := TStringList.Create;
  TxTaint.address := address;
  GetAddressBalance(address, txBalance);
  totalValue := txBalance.received;
  //initial
  txHistory.Start;
  GetTxHistory('ALL',address,txHistory);
  n := 0;
  if (mode = 'RECEIVE') then
  begin
    for i := 0 to txHistory.Count-1 do
    begin
      //

      hash.Add(txHistory.slRhash[i]);
      idx.Add(txHistory.slRindex[i]);
      if hash.Count > 0 then keepLooking := True else keepLooking := False;
      //k := 0;
      l := 0;
      while keepLooking do
      begin
        j := hash.Count;
        for k := n to j - 1 do
        begin
          txDetail.Start;
          GetTxDetail(hash[k], txDetail);
          //process the output of certain idx
          if ((txDetail.outputCount >= idx[k].ToInteger) and (Trim(txDetail.oAddress[idx[k].ToInteger]) <> '')) then
          begin
            TxTaint.slAddress2.Add(txDetail.oAddress[StrToInt(idx[k])]);
            TxTaint.slMethod.Add(mode);
            TxTaint.slLvl.Add(IntToStr(l));
            //TxTaint.slLvl.Add(IntToStr(0));
            TxTaint.slTvalue.Add(txDetail.oValue[StrToInt(idx[k])]);
            TxTaint.slHash.Add(hash[k]);
            TxTaint.slIdx.Add(idx[k]);
            //Shout(hash[k]);
          end;
          //process the inputs, put it in the hash and idx for next round if next level is allowed
          for m := 0 to txDetail.inputCount - 1 do
          begin
            hash.Add(txDetail.iHash[m]);
            idx.Add(txDetail.iIndex[m]);
          end;
          txDetail.Finish;
        end;
        n := j;
        l := l + 1;
        if (l >= depth) then
        begin
          keepLooking := False;
          n := n + m;
        end;

      end;
    end;
  end;
  txHistory.Finish;
  hash.Free;
  idx.Free;

end;

function ReadMsgInTx(txID: string) : string;
var txDetail: TTxDetail;
    i, j: integer;
    teks: string;
    sl : TStringList;
    teksOpReturn: string;
begin
  teks := '';
  txDetail.Start;
  //Shout(FetchHeight);
  GetTxDetail(txID,txDetail);
  //Shout('wew1');
  //Shout(IntToStr(txDetail.iScript.Count));
  for i := 0 to txDetail.iScript.Count-1 do
  begin
    //Shout(txDetail.iScript[i]);
    //teks := teks + ' || ' + txDetail.iScript[i];
    //Shout('wew2');
    if txDetail.hasOpReturn then
    begin
      //Shout('wew3');
      sl := TStringList.Create;
      Explode(sl,txDetail.iScript[i],' ] [ ');
      if sl.Count > 2 then
      begin
        for j := 1 to sl.Count-2 do
        begin
          //Shout(sl[j]);
          teks := teks + sl[j];
        end;
      end;
      sl.Free;
    end;
  end;
  if txDetail.hasOpReturn then
  begin
    teksOpReturn := txDetail.OpReturn;
    teksOpReturn := StringReplace(teksOpReturn,'return [ ','',[rfReplaceAll, rfIgnoreCase]);
    teksOpReturn := StringReplace(teksOpReturn,' ]','',[rfReplaceAll, rfIgnoreCase]);
    teks := teks + teksOpReturn;
  end;

  txDetail.Finish;
  if teks <> '' then Result := Base16Decode(teks)
  else Result := '';
end;

end.
