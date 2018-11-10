MODULE excaliber;

IMPORT SYSTEM,WinUser,WinDef,WinBase,MainWindow,Button,SingleEdit,Panel,T:=Types;

TYPE
  (* Create a Window Type derived from MainWindowDesc*)
  myWindow=POINTER TO myWindowDesc;
  myWindowDesc = RECORD(MainWindow.MainWindowDesc)
  Button1 :Button.Button;
  Button2 :Button.Button;
  Button3 :Button.Button;
  Edit1   :SingleEdit.SingleEdit;
  Panel1  :Panel.Panel;
END;
VAR
    ThisWindow:myWindow;(* the instance of myWindow *)

(************ user defined procedures ************)
PROCEDURE OnChange();
VAR
  dummy:LONGINT;
BEGIN
   ThisWindow^.Edit1^.GetText;
   ThisWindow^.SetCaption(ThisWindow^.Edit1^.Buffer^);
END OnChange;

PROCEDURE OnClick();                          
VAR
  dummy:LONGINT;
BEGIN
    ThisWindow^.Edit1^.GetText(); (* not working removed the message handler*)
    ThisWindow^.Button1^.SetCaption(ThisWindow^.Edit1^.Buffer^);
    ThisWindow^.Button2^.SetCaption(ThisWindow^.Button1^.Caption^);
    ThisWindow^.Edit1.SetText("Hello World");
END OnClick;


PROCEDURE (p : myWindow)Paint*;
VAR
  ps:                                 WinUser.PAINTSTRUCT;
  hDC,dummy:                              LONGINT;  
BEGIN
   hDC:=WinUser.BeginPaint(p^.hWnd,ps);
    (*dummy := WinUser.SetFocus(p^.Edit1.hWnd);*)
    dummy := WinUser.EndPaint(p^.hWnd,ps);
    p^.Return(0);    
END Paint;

PROCEDURE (p:myWindow)SetFocus*;
VAR
  dummy:LONGINT;
BEGIN 
     dummy := WinUser.SetFocus(p^.hWnd);
    p^.Return(0);     
END SetFocus;

PROCEDURE (p:myWindow)MouseDown*(Keys:SET;x,y:LONGINT);
VAR
  dummy:LONGINT;
BEGIN
  IF (T.cRightButton IN Keys) & (T.cShift IN Keys) THEN  (*right button  and shift pressed*)
    dummy := WinUser.MessageBoxA(0,SYSTEM.ADR("Main Window"),SYSTEM.ADR("Here is"),0);
  END;
  
END MouseDown;
    
PROCEDURE (p:myWindow)Char*(ch:CHAR;Data:LONGINT);
(*only when window has focus on start up*)
VAR
  txt:ARRAY 2 OF CHAR;
BEGIN
  txt[0]:=ch;
  txt[1]:=0X;
  p.SetCaption(txt);
END Char;
    
PROCEDURE (p:myWindow)Init*();
VAR
  dummy:LONGINT;
BEGIN
  (* set the window class *)
  (*each main window must have its own class*)
  p^.Init^;
  p^.SetClass('Test');
  (*Create the window *)
  IF (p^.Create(10,10,220,220,
  (*style will be simpler *)WinUser.WS_OVERLAPPEDWINDOW + WinUser.WS_VISIBLE,
  (*will try a SET here   *)WinUser.COLOR_BTNFACE+1 ,
                            TRUE))             
  THEN
  (* after Create methods can be called *)
    p^.SetCaption('hello World');                            
    (* Create the panel and add to mainwindow *)
   p^.Panel1.Init;
   IF  p^.Panel1.Create(1,1,190,190,p^.hWnd,TRUE) THEN
      p^.Panel1^.SetCaption('Panel1');
      p^.Panel1^.SetDefaultFont;
      p^.Panel1^.CreateBrush; 
   END; 
    (* add button to panel *)
   p^.Button1^.Init;
   IF  p^.Button1^.Create(10,40,100,22,p^.Panel1.hWnd,TRUE,{T.ColourButton}) THEN
     p^.Button1^.OnClick:=OnClick;
     p^.Button1^.SetCaption('button1');
     p^.Button1^.SetDefaultFont;
     p^.Button1^.SetTextColour({T.cMagenta});
     p^.Button1^.CreateBrush;
   END; 
   (* add button to panel *)
   p^.Button2^.Init;
   IF  p^.Button2^.Create(10,70,100,22,p^.Panel1.hWnd,TRUE,{T.ColourButton}) THEN
(*     p^.Button2^.OnButtonClick:=OnClick;*)
     p^.Button2^.SetCaption('button2');
     p^.Button2^.SetDefaultFont;
     p^.Button2^.SetTextColour({T.cBlue});
     p^.Button2^.CreateBrush;
   END;   
   (* add button to panel *)

   p^.Button3^.Init;
   IF  p^.Button3^.Create(10,100,100,22,p^.Panel1.hWnd,TRUE,{T.ColourButton}) THEN
(*     p^.Button2^.OnButtonClick:=OnClick;*)
     p^.Button3^.SetCaption('button3');
     p^.Button3^.SetDefaultFont;
     p^.Button3^.SetTextColour({T.cBlack});
     p^.Button3^.CreateBrush;
   END;

    (* Create an editbox add to panel *)
    p^.Edit1^.Init;
    IF p^.Edit1.Create (10,10,100,20,p^.Panel1.hWnd,TRUE,{0}) THEN
      p^.Edit1^.SetDefaultFont;
      p^.Edit1^.OnChange:=OnChange;
      p^.Edit1^.SetTextColour({T.cBlue});      
      p^.Edit1^.CreateBrush; 
    END;

 
  END;
  
END Init;


PROCEDURE (p:myWindow)Destroy;
VAR
  dummy:LONGINT;
BEGIN
  (* this will be called on window closed *)
  (* call default method to destoy the main
  window,windows will then take care and send a destroy to each control
  created so there is nothing else to do! *)
  p^.Destroy^;

      dummy := WinUser.MessageBoxA(0,SYSTEM.ADR("myWindow"),SYSTEM.ADR("Destroy"),0);
END Destroy;



(*********************** Program initialised here ****************)
PROCEDURE ProgMain*;
VAR
  dummy:LONGINT;
BEGIN
  (* create the objects here *)
  NEW(ThisWindow);
  NEW(ThisWindow^.Button1);
  NEW(ThisWindow^.Button2);
  NEW(ThisWindow^.Button3);
  NEW(ThisWindow^.Edit1);
  NEW(ThisWindow^.Panel1);

    ThisWindow.Init();


    REPEAT 
        ThisWindow.Run;
    UNTIL(ThisWindow.Destroyed);
  (* NOW FREE MEMORY ALLOCATED *)  
  DISPOSE(ThisWindow.Button1);
  DISPOSE(ThisWindow.Button2);
  DISPOSE(ThisWindow.Edit1);
  DISPOSE(ThisWindow.Panel1);
  DISPOSE(ThisWindow);
      dummy := WinUser.MessageBoxA(0,SYSTEM.ADR("ProgMain"),SYSTEM.ADR("Destroy"),0);     
  (* PROGRAM WILL NOW TERMINATE HERE AND CALL ExitProcess VIA StartHlp
   to teriminate correctly *)

END ProgMain;


END excaliber.
