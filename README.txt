How to run test file:

1. make signature file from fs implementation file: 
	$ fsharpc --nologo --sig:.fsi simulate.fs  
	
2. make dll from signature file and implementation file: 
	$ fsharpc -a simulate.fsi simulate.fs

3. run script using dll library: 
    $ fsharpc --nologo -r simulate.dll testSimulate.fsx && mono testSimulate.exe

Command as one line:
    $ fsharpc --nologo --sig:simulate.fsi simulate.fs && fsharpc -a simulate.fsi simulate.fs && fsharpc --nologo -r simulate.dll testSimulate.fsx && mono testSimulate.exe     

