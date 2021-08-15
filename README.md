# KIM-Venture

KIM-Venture is an adventure game based on Colossal Cave written for the KIM-1.  The program and its documentation are Copyright © Robert Leedom and I make no claims to any of this material.  I have gathered everything together here for posterity.

## Installing and Running

Included here are both paper tape and HEX format files.  The HEX files can be used on a KIM-1 clone from [Coresham Tech](https://www.corshamtech.com), especially if you have their SD card system.

Load the `Venture-ZeroPage`, `Venture-Game`, and `Venture-Extra` files in any order.

Start the game by going to address `$0100` and pressing `GO`.

When you are finished, load `Venture-Scoring` and run from `$0100` to see how you did.

Note that the scoring program overwrites some page zero locations.  If you want to be able to go back to your game after seeing your score, then you will need to save page zero first, then run the scoring program, then load your saved file and also `Venture-Game` again and restart at `$0100`.

## Instructions

Included here are:

* The [Instructions](KIM-Venture Game Instructions.pdf) for playing the game.
* The [Game Manual](KIM-Venture Manual.pdf) containing information about how to load and run the game.

## Source Code

The `source` directory contains the original source code for the game with all original comments and formatting preserved.  There is one change from the original, though.  In `Game.asm` in the Main Move Loop, the original code used absolute addressing for the `INC NMBUTS` statement.  [My assembler](https://github.com/markbush/6502-assembler) uses zero page addressing when possible which would make this a 2-byte instruction instead of 3-byte, so I have added a `NOP` to preserve addresses in the output.

The following files contain the source code:

File | Content
-----|---------
ZeroPage.asm | data to be loaded into zero page
Light.asm | The LIGHT subroutine which compiles into the start of zero page
Game.asm | The actual game which loads into pages 1, 2, and 3
Extra.asm | Some support subroutines which load into the 6530 RAM
Scoring.asm | The scoring program

These source files (except `ZeroPage.asm`) are location agnostic.  This allows them to be recompiled into alternate addresses.  I have done this in order to create a combined binary allowing all parts (including scoring) to be loaded at the same time in a KIM with extended memory (or the KIM-1 clone).

The above code can be compiled using the following files:

File | Content
-----|---------
Venture-ZeroPage.asm | Combines LIGHT with the zero page data
Venture-Game.asm | Creates the actual game
Venture-Extra.asm | Creates the file loaded into 6530 RAM
Venture-Scoring.asm | Creates the scoring program

These files are necessary as the individual source files refer to locations and subroutines located in the other parts.  They can only be assembled together.  Each of the above files contains a directive causing only the relavent part to be output, enabling the separate program files to be created.
