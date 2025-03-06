# MNH5
MNH5 project; a low profile scripting language written in Free Pascal / Lazarus

Contains:
* Shared source code
* Code for an IDE (which contains install routines in return)
* Code for a light version (console only)
* Scripts for building under Windows or Linux
* MNH scripts for testing

Previously on MNH (or: why 5?)
* Started as MNH ("**m**inimal **n**umeric **h**elper"), a small toolset for interpreting mathematical expressions, based on a substitution system
* Later versions added vectors, custom functions, etc.y

## Installation for users

* Download and unpack the build artifacts
* Linux/UNIX only: you may want to copy the executables to /usr/bin
* Start mnh.exe (Windows) or mnh (Linux)
* The application automatically "installs" itself, creating offline documentation and file associations (Windows only)
* Windows users can choose between a "portable" and a "normal" installation, which differ only by the location of the configuration files. You can change this later.

## Installation for developers
* Prerequisites:
** Lazarus compiler (https://www.lazarus-ide.org/)
** BGRA Bitmap package (https://github.com/bgrabitmap)
* clone this repository
* run make.bat or make.sh
* if this does not work out of the box you may have to modify make_config.mnh
* if it does work you may want to try make.bat test (or make.sh test)

## Usage
Just run the generated executable mnh.exe (or mnh for Linux flavours).
You should have a look at the demos (press Strg+O and look for "demos").
You may also refer to the generated documentation (accessible via the "Show in Browser" button in the Help view that opens when pressing F1).

## Used projects
* Free Pascal    (c) 1993-2015 by Florian Klaempfl and others  http://www.freepascal.org/
* Ararat Synapse (c) 1999-2003 by Lukas Gebauer                http://www.ararat.cz/synapse/
* EpikTimer      (c) 2003-2014 by Tom Lisjac                   <netdxr@gmail.com>
* TRegExpr lib   (c) 1999-2004 by Andrey V. Sorokin            http://RegExpStudio.com
* BGRABitmap                                                   https://github.com/bgrabitmap/
* Lazarus Component Library (LCL) 

## License
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

If you redistribute a modified version of this software please include a link to this Codeberg page!
