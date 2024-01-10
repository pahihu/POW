(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Resource                                    V 2.00.32         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   Definition of resources                                       *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000SEP15                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

DEFINITION Resource;


CONST
  Version      =                      "V 2.00.32";
  Module       =                      "Resource";

(* Resource Identifiers *)
  (* Icons *)
  MyIcon               =                   1;
  MainIcon             =                  11;
  
  (* Cursors *)
  Cursor02             =                   1;
  
  (* Bitmaps *)

  (* Menus *)
  IDM_Files            =               100  ;              (* Menu Files *)
  IDM_File_Open        =               IDM_Files +  2;
  IDM_File_Close       =               IDM_Files +  3;
  IDM_File_Start       =               IDM_Files + 11;
  IDM_File_Attach      =               IDM_Files + 12;
  IDM_File_Suspend     =               IDM_Files + 13;
  IDM_File_Resume      =               IDM_Files + 14;
  IDM_File_Stop        =               IDM_Files + 15;
  IDM_File_Print       =               IDM_Files + 21;
  IDM_File_Printer     =               IDM_Files + 22;
  IDM_File_Exit        =               IDM_Files + 99;

  IDM_Edit             =               200;                (* Menu Edit *)
  IDM_Edit_Search      =               IDM_Edit +  1;
  IDM_Edit_Replace     =               IDM_Edit +  2; 
  IDM_Edit_Goto        =               IDM_Edit +  3;
  IDM_Edit_Cut         =               IDM_Edit +  4;
  IDM_Edit_Paste       =               IDM_Edit +  5;
  
  IDM_View             =               300;                (* Menu View *)
  IDM_View_HexData     =               IDM_View +  1;
  IDM_View_Dump        =               IDM_View +  2; 
  IDM_View_SectionHeaders  =           IDM_View +  3;
  IDM_View_DD_Export   =               IDM_View + 11;
  IDM_View_DD_Import   =               IDM_View + 12;
  IDM_View_DD_Resource  =              IDM_View + 13;
  IDM_View_DD_Exception  =             IDM_View + 14;
  IDM_View_DD_Security  =              IDM_View + 15;
  IDM_View_DD_BaseRelocation  =        IDM_View + 16;
  IDM_View_DD_Debug    =               IDM_View + 17;
  IDM_View_DD_Copyright  =             IDM_View + 18;
  IDM_View_DD_GlobalPtr  =             IDM_View + 19;
  IDM_View_DD_TLS      =               IDM_View + 20;
  IDM_View_DD_LoadConfig   =           IDM_View + 21;
  IDM_View_DD_BoundImport  =           IDM_View + 22;
  IDM_View_DD_IAT      =               IDM_View + 23;
  IDM_View_SymbolTable  =              IDM_View +  5;
  IDM_View_LineNumbers  =              IDM_View +  6;
  IDM_View_SymbolTableGlobal  =        IDM_View +  7;
  
  IDM_Window           =               400;                (* Menu Window *)
  IDM_Window_Tile_hor  =               IDM_Window +  1;
  IDM_Window_Tile_ver  =               IDM_Window +  2;
  IDM_Window_Cascade   =               IDM_Window +  3;
  IDM_Window_Arrange   =               IDM_Window +  4;
  IDM_Window_Close     =               IDM_Window +  5;
  IDM_Window_First_Child  =            IDM_Window +  6;
  
  IDM_Admin            =               700;                (* Menu Administration *)
  IDM_Admin_Font       =               IDM_Admin +  1;
  IDM_Admin_Zero       =               IDM_Admin + 10;
  IDM_Admin_First      =               IDM_Admin + 11;
  IDM_Admin_Second     =               IDM_Admin + 12; 
  IDM_Admin_Third      =               IDM_Admin + 13;
  IDM_Admin_Fourth     =               IDM_Admin + 14; 
  
  IDM_Options          =               800;                (* Menu Option *)
  IDM_Options_Write    =               IDM_Options +  1;
  IDM_Options_Read     =               IDM_Options +  2;
  
  IDM_Help             =               900;                (* Menu Help *)
  IDM_Help_About       =               IDM_Help +  2;
  
  IDM_Debug            =              1000;
  IDM_Dump             =              1100;
  IDM_UserInterface    =              1200;
  
  (* Resource Names *)
  IDM_Main           =                "MainMenu";
  IDD_HelpAbout      =                "AboutBox";

  (* Buttons *)
  IDB_HelpAbout_Dialog               = 601;
  IDP_HelpAbout_OK                   = 609;
  
  IDB_OPEN             =               102;
  IDB_CLOSE            =               103;
  IDB_START            =               111;
  IDB_ATTACH           =               112;
  IDB_SUSPEND          =               113;
  IDB_RESUME           =               114;
  IDB_STOP             =               115;
  IDB_PRINT            =               121;
  IDB_PRINTER          =               122;
  
  IDS_FilesOpen        =                "IDS_FilesOpen";
  IDS_Selected         =                "IDS_Selected";
  
END Resource.

