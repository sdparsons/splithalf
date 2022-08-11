Comments on cran checks:


1. I have moved institutions and so my email has changed


2. I received the following note:
checking compiled code ... NOTE
  Note: information on .o files for i386 is not available
  Note: information on .o files for x64 is not available
  File 'C:/Users/samdp/AppData/Local/Temp/RtmpAdYuSM/splithalf.Rcheck/splithalf/libs/i386/splithalf.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  File 'C:/Users/samdp/AppData/Local/Temp/RtmpAdYuSM/splithalf.Rcheck/splithalf/libs/x64/splithalf.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  
  Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs. The detected symbols are linked into the code but
  might come from libraries and not actually be called.
  
  See 'Writing portable packages' in the 'Writing R Extensions' manual.


I believe this is a false-positive ref https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using
