# Mega65Lib
TRSE Mega65 Library !!! unfinished, Code will change

Attempt to write sort of standard library for the MEGA65 (https://mega65.org/) with TRSE (https://lemonspawn.com/turbo-rascal-syntax-error-expected-but-begin/)

Usage:
  Clone this repository, copy the Mega65 Folder into your project folder.
  
  !!!! Check that "Allow for local naming of function variables" is checked under menu project->Project Settings->Tab:Pascal Settings !!!!

  In the examples folder is a TRSE.D81 with all the example projects compiled for quick testing on the machine or
  the emulator.
  
  Example Code for the textio unit :
  ```
/****************************************************************/
/*	TRSE Mega65 StdLib							  													*/
/*	Example : text.ras							  													*/
/*												  																		*/
/*	Shows usage of textio unit					  											*/
/*                        						  												*/
/****************************************************************/
program text;

@use "Mega65/textio"

var 
	counter1 : integer;
	counter2 : integer;
	font : byte;
	i : byte;
	hello : String = "HELLO WORLD";
	
	newChar : Array[8] of byte = (%01111110,%11111111,%11011011,%11111111,%11000011,%11100111,%11111111,%01111110);
	
begin
	textio::Set40x25();	// no need to move the screen ram
//	textio::Set80x25();  // no need to move the screen ram
	
//	textio::Set80x50();	// ! move the screen ram
	textio::SetCharLocation($0004,$0000);		// set char location to $40000
	textio::SetScreenLocation($0005,$0000);		// new screen location $50000
	
	textio::SetScreenBackground(1,8);
	textio::ClearScreen(1,6);
	

	font := 0;
	counter1 := 65535;
	counter2 :=2;
	
	for i := 0 to 32 do
	begin
		textio::PrintChar(i,10,2,textio::COLORS[i]);
	end;
	
	textio::PrintString(#hello,20,5,0);
	// cycle endlessly through fonts
	while (1) do
	begin
		dec(counter1);
		if (counter1=0) then
		begin
			dec(counter2);
			if (counter2=0) then
			begin
				counter2 := 4;
				inc(font);
				case font of
					0: textio::SetFont(textio::FONT_A);
					1: textio::ToggleLowerCase;
					2: textio::SetFont(textio::FONT_B);
					3: textio::ToggleLowerCase;
					4: textio::SetFont(textio::FONT_C);
					5: textio::ToggleLowerCase;
				else 
				begin
					font := 0;
				end;
				textio::CharDef(2,#newChar);	// SetFont will overwrite our new char
			end;
			counter1 := 65535;
		end;
	end;
end.
```
