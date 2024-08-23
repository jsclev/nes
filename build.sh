ca65 hello-world.s -g -o hello-world.o
ld65 -o hello-world.nes -C hello-world.cfg hello-world.o -m hello-world.map.txt -Ln hello-world.labels.txt --dbgfile hello-world.nes.dbg
mv hello-world.nes "/Users/john/Library/Application Support/OpenEmu/Game Library/roms/Nintendo (NES)"