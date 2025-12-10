; NSIS Installer Script
; Requires NSIS 3.x - Download from https://nsis.sourceforge.io/

; from command line 
; APP_NAME
; VERSION
; PACKAGE_NAME
; WIN_EXE_DIR
; DESCRIPTION
; ICON_NAME
; INSTALLER_NAME


; Extract major and minor version from VERSION (e.g., "4.2.1")
!searchparse "${VERSION}" "" VERSIONMAJOR "." VERSIONMINOR "." VERSIONPATCH

!define COMPANYNAME "Jochen Guenzel"

; These will be set dynamically by the build script
!define HELPURL "https://github.com/jxjo/${APP_NAME}"
!define UPDATEURL "https://github.com/jxjo/${APP_NAME}"
!define ABOUTURL "https://github.com/jxjo/${APP_NAME}"

RequestExecutionLevel admin

Name "${APP_NAME} ${VERSION}"
Caption "${APP_NAME} ${VERSION} Setup"
BrandingText "${APP_NAME} Installer"
Icon "..\icons\${ICON_NAME}"
OutFile "..\dist\${INSTALLER_NAME}"
InstallDir "$PROGRAMFILES64\${APP_NAME}"
InstallDirRegKey HKLM "Software\${APP_NAME}" "Install_Dir"

; Modern UI
!include "MUI2.nsh"
!include "nsDialogs.nsh"
!include "LogicLib.nsh"

!define MUI_ICON "..\icons\${ICON_NAME}"
!define MUI_UNICON "..\icons\${ICON_NAME}"
!define MUI_HEADERIMAGE
!define MUI_ABORTWARNING

; Variables for checkboxes
Var Dialog
Var AssocDatPC2
Var CheckDatPC2

; Pages
!insertmacro MUI_PAGE_LICENSE "..\LICENSE"
!insertmacro MUI_PAGE_DIRECTORY
Page custom FileAssocPage FileAssocPageLeave
!insertmacro MUI_PAGE_INSTFILES

; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\${APP_NAME}.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Launch ${APP_NAME}"
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

; Languages
!insertmacro MUI_LANGUAGE "English"

; Version Information
VIProductVersion "${VERSION}.0"
VIAddVersionKey "ProductName" "${APP_NAME}"
VIAddVersionKey "CompanyName" "${COMPANYNAME}"
VIAddVersionKey "FileDescription" "${DESCRIPTION}"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "LegalCopyright" "© ${COMPANYNAME}"

Function FileAssocPage
  !insertmacro MUI_HEADER_TEXT "File Associations" "Choose which file types to associate with ${APP_NAME}"
  
  nsDialogs::Create 1018
  Pop $Dialog
  
  ${If} $Dialog == error
    Abort
  ${EndIf}
  
  ${NSD_CreateLabel} 0 0 100% 20u "Select the file types you want to open with ${APP_NAME}:"
  Pop $0
  
  ${NSD_CreateCheckbox} 10u 30u 100% 12u "Associate .pc2 (PlanformCreator2 project files)"
  Pop $CheckDatPC2
  ${NSD_SetState} $CheckDatPC2 $AssocDatPC2
    
  nsDialogs::Show
FunctionEnd

Function FileAssocPageLeave
  ${NSD_GetState} $CheckDatPC2 $AssocDatPC2
FunctionEnd

Section "Install"
  SetOutPath "$INSTDIR"
  
  ; Copy all files from the PyInstaller dist folder
  File /r "..\dist\${WIN_EXE_DIR}\*.*"
  
  ; Create shortcuts
  CreateDirectory "$SMPROGRAMS\${APP_NAME}"
  CreateShortcut "$SMPROGRAMS\${APP_NAME}\${APP_NAME}.lnk" "$INSTDIR\${APP_NAME}.exe" "" "$INSTDIR\${APP_NAME}.exe" 0
  CreateShortcut "$SMPROGRAMS\${APP_NAME}\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "$INSTDIR\Uninstall.exe" 0
  CreateShortcut "$DESKTOP\${APP_NAME}.lnk" "$INSTDIR\${APP_NAME}.exe" "" "$INSTDIR\${APP_NAME}.exe" 0
  
  ; Write registry keys for uninstaller
  WriteRegStr HKLM "Software\${APP_NAME}" "Install_Dir" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayName" "${APP_NAME}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayIcon" "$INSTDIR\${APP_NAME}.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "Publisher" "${COMPANYNAME}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "URLInfoAbout" "${ABOUTURL}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayVersion" "${VERSION}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "VersionMajor" ${VERSIONMAJOR}
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "VersionMinor" ${VERSIONMINOR}
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "NoRepair" 1
  
  ; File associations based on checkbox selections
  ${If} $AssocDatPC2 == ${BST_CHECKED}
    ; Register ..pc2 file extension
    WriteRegStr HKCR ".pc2" "" "${APP_NAME}.pc2file"
    WriteRegStr HKCR "${APP_NAME}.pc2file" "" "Airfoil Data File"
    WriteRegStr HKCR "${APP_NAME}.pc2file\DefaultIcon" "" "$INSTDIR\${APP_NAME}.exe,0"
    WriteRegStr HKCR "${APP_NAME}.pc2file\shell\open\command" "" '"$INSTDIR\${APP_NAME}.exe" "%1"'
  ${EndIf}
  
  
  ; Notify Windows of file association changes
  ${If} $AssocDatPC2 == ${BST_CHECKED}
    System::Call 'Shell32::SHChangeNotify(i 0x8000000, i 0, i 0, i 0)'
  ${EndIf}
  
  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
SectionEnd

Section "Uninstall"
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}"
  DeleteRegKey HKLM "Software\${APP_NAME}"

  ; Remove file associations only if they belong to us
  ReadRegStr $0 HKCR ".pc2" ""
  ${If} $0 == "${APP_NAME}.pc2file"
    DeleteRegKey HKCR ".pc2"
  ${EndIf}
  DeleteRegKey HKCR "${APP_NAME}.pc2file"
  
  ; Notify Windows of file association changes and clear icon cache
  System::Call 'Shell32::SHChangeNotify(i 0x8000000, i 0, i 0, i 0)'

  ; Remove shortcuts
  Delete "$DESKTOP\${APP_NAME}.lnk"
  Delete "$SMPROGRAMS\${APP_NAME}\*.*"
  RMDir "$SMPROGRAMS\${APP_NAME}"
  
  ; Remove files and directories
  RMDir /r "$INSTDIR"
SectionEnd