SymBrowser32 - Symbol file browser ( viewer ) for Pow! Oberon-2 symbol-files

Release 2005-05-25

Changes from the version that ships with Pow! :
  
    °Bug fix: PowDDE Callback procedure did not return a value, this occasionally crashed the browser
    °Bug fix: Command line parsing could cause buffer overflow
    °Bug fix: display [_APICALL] instead of [WINDOWS] for procedure variables
    
    +Enhancement: It is now possible to associate "*.Sym" files with SymBrowser32 in Windows Explorer.
        Double clicking will open a browser window if Pow! is running. (You can alsodrag & drop  or
        copy & paste "*.Sym" files on the Browser executable in Explorer windows)
    
    +Enhancement:  Improved readability: extra whitespace in parameter lists, export mark only shown for
        readonly export ("-")
        
        
Installation :
  Copy "SymBrowser32.exe" to your tools directory.
  
Usage :
  2 commandline parameters are available:
    -a : show also imported modules
    -g : display debug information
    
Examples of parameter usage in the 'Add Tool' dialog:
    
  -'Initial directory' = %o ; 'Arguments' = -g
      => A file selection dialog will open in the project directory.
        Select a sym file and a viewer will open with debug info added to the definition.
  -'Initial directory' = (empty) ; 'Arguments' = %o\%n.sym -a
      => The file currently being edited wil be browsed, and a viewer will be shown with
         information on inported modules included in the definition.


Full source code is included.

Enjoy,
Koen.
koend@yahoo.com