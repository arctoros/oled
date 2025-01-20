echo assembled
ca65 -D arc bios.s -o arc.o &&
ld65 -C map.cfg arc.o -o a.out -Ln labels.lbl &&
rm arc.o
