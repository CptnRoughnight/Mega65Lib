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

		/home/roughnight86/xemu/build/bin/xmega65.native -besure -autoload -keymap de -8 "PLAT.D81"
	else
    echo FAIL
	fi


