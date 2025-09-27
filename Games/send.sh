#!/bin/bash
	if [ $? -eq 0 ]; then
		c1541 -format "disk,0" d81 "RPG.D81"
		c1541 -attach "RPG.D81" 8 -write "rpg.prg"
		c1541 -attach "RPG.D81" 8 -write "data/hud.bin"
		c1541 -attach "RPG.D81" 8 -write "data/hudc.bin"
		c1541 -attach "RPG.D81" 8 -write "data/map3.bin"
		c1541 -attach "RPG.D81" 8 -write "data/map3c.bin"
		
		#c1541 -attach "RPG.D81" 8 -write "bin/pal/aurora.bin"
		#c1541 -attach "RPG.D81" 8 -write "bin/pal/palette.bin"

		#~/Dokumente/m65tools-develo-190-7a0467-linux/m65 -l /dev/ttyUSB1 -F -r -virtuald81 "ZZT.D81"
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -F1
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/mega65_ftp -l /dev/ttyUSB1 -c "put RPG.D81" -c "mount RPG.D81" -c "quit"
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'list'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'list'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'load\"$\",8'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'list'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'list'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'list'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'load\"*\"'
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -T 'run'
	else
    echo FAIL
	fi


