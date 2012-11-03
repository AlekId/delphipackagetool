{ -------------------------------------------------------------------------------------}
{ An "application launcher" component for Delphi32.                                    }
{ Copyright 1996, Patrick Brisacier and Jean-Fabien Connault.  All Rights Reserved.    }
{ This component can be freely used and distributed in commercial and private          }
{ environments, provided this notice is not modified in any way.                       }
{ -------------------------------------------------------------------------------------}
{ Feel free to contact us if you have any questions, comments or suggestions at        }
{ cycocrew@aol.com                                                                     }
{ -------------------------------------------------------------------------------------}
{ Date last modified:}
{ -------------------------------------------------------------------------------------}

{ -------------------------------------------------------------------------------------}
{ TAppExec v1.12                                                                       }
{ -------------------------------------------------------------------------------------}
{ Description:                                                                         }
{   A component that allows you to execute easily applications.                        }
{ Properties:                                                                          }
{   property ChangeDir: Boolean;                                                       }
{   property ErrNo: Integer;                                                           }
{   property ExeName: String;                                                          }
{   property ExePath: String;                                                          }
{   property ExeParams: TStringList;                                                   }
{   property Wait: Boolean;                                                            }
{   property WindowState: TWindowState;                                                }
{ Procedures and functions:                                                            }
{   procedure Execute;                                                                 }
{   function GetErrorString: string;                                                   }
{                                                                                      }
{ See example contained in example.zip file for more details.                          }
{ -------------------------------------------------------------------------------------}
{ Revision History:                                                                    }
{ 1.00:  + Initial release                                                             }
{ 1.01:  + Added support for french and english languages                              }
{ 1.02:  S.Herzog Changed the mehtod Execute now it is a function instead a procedure  }
{        if no error occurred then execute returns a true else false                   }
{        I also changed the property ExeParams. It's not a string list any more. Now,  }
{        it is just a string. I also made some changes to in the execute method        }
{ 1.03:  SH added Property for Process Priority.                                       }
{ 1.04:  SH Bug fix in setexepath                                                      }
{ 1.05:  SH 21.12.2001 - added OnError Event. Cleaned up the code.                     }
{ 1.06:  SH 08.01.2002 - added property editor to the property <ExeName>.              }
{ 1.07:  SH 18.01.2002 - fix for pathname property.                                    }
{ 1.08:  SH 17.07.2002 - added compiler statements for compiling with D5/D6            }
{ 1.09:  SH 21.12.2002 - review for Delphi 7                                           }
{ 1.10:  SH 06.01.2003 - added resource file statement.                                }
{ 1.11:  SH 19.02.2003 - changed unit and class name.                                  }
{ 1.12:  SH 15.03.2003 - added version property.                                       }
{ 1.13:  SH 04.05.2005 - changes for command line in method execute.                   }
{ 1.14:  SH 08.05.2005 - rename priority types to avoid naming conflicts.              }
{ 1.15:  SH 07.06.2005 - fix for priority.                                             }
{ 1.16   SH 28.01.2006 - changes for return value of execute method.                   }
{ 1.17   SH 02.11.2007 - added property <CloseRunningProcess>.                         }
{ -------------------------------------------------------------------------------------}
unit uDPTAppExec;

interface

uses
  SysUtils,
  WinProcs,
  Forms,
  Classes;

const
cComponentVersion='1.17';
cNoError=32;

type
  TNVBProcessPrio    = (ppIdle,ppNormal,ppHigh,ppRealTime);
  TNVBErrorEvent     = procedure (Sender: TObject;_ErrorMsg:String) of object;


  TNVBAppExec = class(TComponent)
  private
    FErrNo: Integer;
    FExeName: String;
    FExePath: String;
    FExeParams:String;
    FPriority:TNVBProcessPrio;
    FWindowState: TWindowState;
    FMode: Word;
    FWait: Boolean;
    FOnError:TNVBErrorEvent;
    FVersion: string;
    FProcessInfo:TProcessInformation;
    FCloseRunningProcess: boolean;
    procedure SetWindowState(AWindowState: TWindowState);
    procedure SetExePath(AExePath: String);
    procedure SetExeName(_Name:String);
    procedure SetVersion(const Value: string);
    function ConvertPriority(_prio:TNVBProcessPrio):DWord;
    function DoError:boolean;
    procedure CloseRunningInstance;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function Execute:boolean;
    function GetErrorString: string;
    function GetProcessInfo:TProcessInformation;
  published
    property Version:string            read FVersion  write SetVersion;
    property ErrNo:Integer             read FErrNo;
    property ExeName:String            read FExeName   write SetExeName;
    property ExeParams:String          read FExeParams write FExeParams;
    property ExePath:String            read FExePath   write SetExePath;
    property Wait:Boolean              read FWait write FWait;
    property WindowState:TWindowState  read FWindowState write SetWindowState;
    property Priority:TNVBProcessPrio  read FPriority write FPriority;
    property CloseRunningProcess:boolean read FCloseRunningProcess write FCloseRunningProcess;
    property OnError:TNVBErrorEvent    read FOnError write FOnError;
  end;

const
 { French Messages }
 {MSG_ERROR_DASH_1 = 'Pas d''exécution';
 MSG_ERROR_0 = 'Système dépassé en capacité mémoire, exécutable corrompu, ou réallocations invalides';
 MSG_ERROR_2 = 'Fichier non trouvé';
 MSG_ERROR_3 = 'Chemin non trouvé';
 MSG_ERROR_5 = 'Tentative de liaison dynamique à une tâche, ou erreur de partage, ou erreur de protection réseau';
 MSG_ERROR_6 = 'Librairie nécessitant des segments de données séparés pour chaque tâche';
 MSG_ERROR_8 = 'Mémoire insuffisante pour démarrer l''application';
 MSG_ERROR_10 = 'Version de Windows incorrecte';
 MSG_ERROR_11 = 'Exécutable invalide, application non Windows, ou erreur dans l''image du fichier .EXE';
 MSG_ERROR_12 = 'Application écrite pour un système d''exploitation différent';
 MSG_ERROR_13 = 'Application écrite pour MS-DOS 4.0';
 MSG_ERROR_14 = 'Type d''exécutable inconnu';
 MSG_ERROR_15 = 'Tentative de chargement d''une application en mode réel (développée pour une version antérieure de Windows)';
 MSG_ERROR_16 = 'Tentative de chargement d''une seconde instance d''un exécutable contenant plusieurs segments de données non marqués en lecture seule';
 MSG_ERROR_19 = 'Tentative de chargement d''un exécutable compressé. Le fichier doit être décompressé avant de pouvoir être chargé';
 MSG_ERROR_20 = 'Fichier Dynamic-link library (DLL) invalide. Une des DLLs requises pour exécuter cette application est corrompue';
 MSG_ERROR_21 = 'Application nécessitant des extensions 32-bit';
 MSG_ERROR_32_AND_MORE = 'Pas d''erreur';
 }
 { English Messages }
 MSG_ERROR_DASH_1 = 'No execution';
 MSG_ERROR_0 = 'System was out of memory, executable file was corrupt, or relocations were invalid';
 MSG_ERROR_2 = 'File was not found';
 MSG_ERROR_3 = 'Path was not found';
 MSG_ERROR_5 = 'Attempt was made to dynamically link to a task, or there was a sharing or network-protection error';
 MSG_ERROR_6 = 'Library required separate data segments for each task';
 MSG_ERROR_8 = 'There was insufficient memory to start the application';
 MSG_ERROR_10 = 'Windows version was incorrect';
 MSG_ERROR_11 = 'Executable file was invalid. Either it was not a Windows application or there was an error in the .EXE image';
 MSG_ERROR_12 = 'Application was designed for a different operating system';
 MSG_ERROR_13 = 'Application was designed for MS-DOS 4.0';
 MSG_ERROR_14 = 'Type of executable file was unknown';
 MSG_ERROR_15 = 'Attempt was made to load a real-mode application (developed for an earlier version of Windows)';
 MSG_ERROR_16 = 'Attempt to load second instance of an executable containing multiple data segments not marked read-only';
 MSG_ERROR_19 = 'Attempt was made to load a compressed executable file. The file must be decompressed before it can be loaded';
 MSG_ERROR_20 = 'Dynamic-link library (DLL) file was invalid. One of the DLLs required to run this application was corrupt';
 MSG_ERROR_21 = 'Application requires 32-bit extensions';
 MSG_ERROR_32_AND_MORE = 'No error';


implementation

constructor TNVBAppExec.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersion   :=cComponentVersion;
  FExeParams :='';
  FMode      := SW_SHOWNORMAL;
  FErrNo     := -1;
  FPriority  :=ppNormal;
end;

{-----------------------------------------------------------------------------
  Procedure: TNVBAppExec.Execute
  Author:    herzogs2
  Date:      25-Jan-2002
  Arguments: None
  Result:    boolean
  Description:
  Last changed: 18.01.2002 fix for pathname property.
                04.05.2005 changes for command line
-----------------------------------------------------------------------------}
function TNVBAppExec.Execute:boolean;
var
  StartupInfo:TStartupInfo;
  IpApplicationname,lpCommandLine:string;
  lpExitCode:DWord;
begin
  { Création de la ligne de commande }
  if FExePath='' then FExePath:=ExtractFilePath(Paramstr(0));
  IpApplicationname:=FExePath+FExeName;
  CloseRunningInstance;
  lpCommandLine:='"'+IpApplicationname+ '" ' + FExeParams;
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := FMode;
  if not CreateProcess(pchar(IpApplicationname),
    pchar(lpCommandLine),          { pointer to command line string }
    nil,                           { pointer to process security attributes }
    nil,                           { pointer to thread security attributes }
    false,                         { handle inheritance flag }
    CREATE_NEW_CONSOLE or          { creation flags }
    ConvertPriority(FPriority),
    nil,                           { pointer to new environment block }
    PChar(FExePath),               { pointer to current drive and directory of the new process }
    StartupInfo,                   { pointer to STARTUPINFO }
    FProcessInfo) then result:=DoError
  else begin
    if FWait then begin
      WaitforSingleObject(FProcessInfo.hProcess,INFINITE);
      if GetExitCodeProcess(FProcessInfo.hProcess,lpExitCode) then result:=true
                                                              else result:=DoError;
    end else result:=true;
  end;
end;

function TNVBAppExec.DoError:boolean;
begin
  FErrNo := GetLastError;
  result:=(FErrNo>=cNoError);
  if (not result) and
      Assigned(FOnError) then FOnError(self,GetErrorString)
end;


procedure TNVBAppExec.SetWindowState(AWindowState: TWindowState);
const
  Mode: array[wsNormal..wsMaximized] of Word =
    (SW_SHOWNORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED);
begin
  if FWindowState = AWindowState then exit;
  FMode := Mode[AWindowState];
  FWindowState := AWindowState;
end;


procedure TNVBAppExec.SetExePath(AExePath: String);
begin
  if FExePath = AExePath then exit;
  FExePath := ExtractFilePath(AExePath);
end;


function TNVBAppExec.GetErrorString: string;
begin
  case FErrNo of
    -1:
      Result := MSG_ERROR_DASH_1;
    0:
      Result := MSG_ERROR_0;
    2:
      Result := MSG_ERROR_2;
    3:
      Result := MSG_ERROR_3;
    5:
      Result := MSG_ERROR_5;
    6:
      Result := MSG_ERROR_6;
    8:
      Result := MSG_ERROR_8;
    10:
      Result := MSG_ERROR_10;
    11:
      Result := MSG_ERROR_11;
    12:
      Result := MSG_ERROR_12;
    13:
      Result := MSG_ERROR_13;
    14:
      Result := MSG_ERROR_14;
    15:
      Result := MSG_ERROR_15;
    16:
      Result := MSG_ERROR_16;
    19:
      Result := MSG_ERROR_19;
    20:
      Result := MSG_ERROR_20;
    21:
      Result := MSG_ERROR_21;
    32..MaxInt:
      Result := MSG_ERROR_32_AND_MORE;
  end;
end;

procedure TNVBAppExec.SetExeName(_Name: String);
var
_path:String;
begin
  FExeName:=ExtractFileName(_Name);
  _path:=ExtractFilePath(_Name);
  if _path<>'' then FExePath:=_Path;
end;

procedure TNVBAppExec.SetVersion(const Value: string);
begin
  FVersion := cComponentVersion;
end;

function TNVBAppExec.ConvertPriority(_prio: TNVBProcessPrio): DWord;
begin
  result:=NORMAL_PRIORITY_CLASS;
  case _prio of
    ppNormal:  result:=NORMAL_PRIORITY_CLASS;
    ppHigh  :  result:=HIGH_PRIORITY_CLASS;
    ppIdle  :  result:=IDLE_PRIORITY_CLASS;
    ppRealtime:result:=REALTIME_PRIORITY_CLASS;
  end;
end;



function TNVBAppExec.GetProcessInfo: TProcessInformation;
begin
  result:=FProcessInfo;
end;

destructor TNVBAppExec.Destroy;
begin
  CloseRunningInstance;
  inherited;
end;

procedure TNVBAppExec.CloseRunningInstance;
var
  hProcess:hwnd;
begin
  if (FCloseRunningProcess) and (FProcessInfo.dwProcessId<>0) then begin // if this setting is true
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, True, FProcessInfo.dwProcessId); // and an instance was already started
    TerminateProcess(hProcess,13); // then close it first.
  end;
end;

end.

