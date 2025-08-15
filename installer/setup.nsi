; setup.nsi
!include "MUI2.nsh"
!include "LogicLib.nsh"

; General settings
!define NAME "Rulu"
!define APPFILE "rulu.exe"
!define VERSION "0.1.0-patch-1"
!define SLUG "${NAME} v${VERSION}"

Name "${NAME}"
OutFile "Rulu_Setup.exe"
InstallDir "$PROGRAMFILES\${NAME}"
InstallDirRegKey HKCU "Software\${NAME}" ""
RequestExecutionLevel admin

; Modern UI settings
!define MUI_ABORTWARNING
!define MUI_WELCOMEPAGE_TITLE "${SLUG} Setup"

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

; Install section
Section "Install Rulu" SEC_RULU
    SectionIn RO
    SetOutPath "$INSTDIR"
    File "app\${APPFILE}"
    WriteRegStr HKCU "Software\${NAME}" "" "$INSTDIR"
    WriteUninstaller "$INSTDIR\Uninstall.exe"

    ; Add to system PATH
    ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"
    StrCpy $1 "$INSTDIR"
    ${If} $0 != ""
        StrCpy $0 "$0;$1"
    ${Else}
        StrCpy $0 "$1"
    ${EndIf}
    WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$0"
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
SectionEnd

; Uninstaller section
Section "Uninstall"
    Delete "$INSTDIR\${APPFILE}"
    Delete "$INSTDIR\Uninstall.exe"
    RMDir "$INSTDIR"
    DeleteRegKey /ifempty HKCU "Software\${NAME}"

    ; Remove from system PATH
    ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"
    Push "$INSTDIR"
    Push "$0"
    Call un.RemoveFromPath
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
SectionEnd

; Function to remove directory from PATH
Function un.RemoveFromPath
    Exch $0 ; old path
    Exch
    Exch $1 ; dir to remove
    Push $2
    Push $3
    Push $4
    StrLen $2 "$1"
    StrCpy $3 0
    loop:
        StrCpy $4 "$0" $2 $3
        ${If} $4 == "$1"
            StrCpy $4 "$0" $3
            IntOp $3 $3 + $2
            StrCpy $0 "$4$0" -1 $3
            Goto done
        ${EndIf}
        IntOp $3 $3 + 1
        StrCpy $4 "$0" 1 $3
        ${If} $4 == ""
            Goto done
        ${EndIf}
        Goto loop
    done:
        WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$0"
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd