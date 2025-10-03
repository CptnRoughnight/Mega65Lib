#!/bin/bash
	if [ $? -eq 0 ]; then
		c1541 -format "disk,0" d81 "PLAT.D81"

		c1541 -attach "PLAT.D81" 8 -write "platformer.prg"


		c1541 -attach "PLAT.D81" 8 -write "bin/pal/palette.bin"


		c1541 -attach "PLAT.D81" 8 -write "bin/images/tiles.bin"
		c1541 -attach "PLAT.D81" 8 -write "bin/images/tilesattrib.bin"
		c1541 -attach "PLAT.D81" 8 -write "bin/images/bck.bin"

		c1541 -attach "PLAT.D81" 8 -write "bin/maps/map1.bin"
		c1541 -attach "PLAT.D81" 8 -write "bin/maps/bckmap.bin"

		
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/m65 -l /dev/ttyUSB1 -F1
		/home/roughnight86/Dokumente/Mega65/m65tools-develo-202-553682-linux/mega65_ftp -l /dev/ttyUSB1 -c "put PLAT.D81" -c "mount PLAT.D81" -c "quit"
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


