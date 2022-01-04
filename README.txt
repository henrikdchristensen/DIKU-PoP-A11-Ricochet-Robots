How to run game:

1. make signature file from fs implementation file: 
	$ fsharpc --nologo --sig:robots.fsi robots.fs  
	
2. make dll from signature file and implementation file: 
	$ fsharpc -a robots.fsi robots.fs

3. run script using dll library: 
    $ fsharpc --nologo -r robots.dll robots-game.fsx && mono robots-game.exe

Command as one line:
    $ fsharpc --nologo --sig:robots.fsi robots.fs && fsharpc -a robots.fsi robots.fs && fsharpc --nologo -r robots.dll robots-game.fsx && mono robots-game.exe     

