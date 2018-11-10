Oberon-2 Debugger

this is a new version of a debugging tool for POW.

It is used as follows:
O2Debug [/L] [/S] [filename.exe]

Functions:
File.Open/Close (dump a file)
  Displaying the DOS, NT & optional header of *.exe, *.obj, *.dll
  Displaying the section headers
  Displaying some data directories (still under construction)
  Displaying line numbers & symbol table
  
File.Start/Suspend/Resume/Stop
  Debugging a process
  start, suspend and resume the program
  shows exceptions
  stops a program

This is another raw version, there may be some crashes when using the wrong 
keys or files...

The program uses the MDI functions of Win 32 as well as the debugging and dump API.
I am very interested in your comments !!

And there are still some things to do which we can do in team.

Regards, Klaus

Be careful when recompiling the sources,
this program uses an extended version of OPAL: strings.UHexStr!!

