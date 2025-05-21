# Mega65Lib
TRSE Mega65 Library !!! unfinished, Code will change !!!

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
	
begin
//	textio::Set40x25();	// no need to move the screen ram
//	textio::Set80x25();  // no need to move the screen ram
	
	textio::Set80x50();	// ! move the screen ram
	textio::SetScreenLocation($0005,$0000);
	
	textio::SetScreenBackground(1,8);
	textio::ClearScreen(1,6);
	textio::SetCharLocation($0002,$D000);
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
				counter2 := 2;
				inc(font);
				case font of
					0: textio::SetCharLocation($0002,$D000);
					1: textio::SetCharLocation($0002,$D800);
					2: textio::SetCharLocation($0003,$D000);
					3: textio::SetCharLocation($0003,$D800);
					4: textio::SetCharLocation($0002,$9000);
					5: textio::SetCharLocation($0002,$9800);
				else 
				begin
					font := 0;
				end;
			end;
			counter1 := 65535;
		end;
	end;
end.
```
