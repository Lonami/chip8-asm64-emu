BINARY=main.bin

build:
	gcc main.s `sdl-config --cflags --libs` -o $(BINARY)

debug:
	gcc main.s `sdl-config --cflags --libs` -o $(BINARY) -gstabs+
	gdb -q $(BINARY)

run: build
	./$(BINARY) bin

clean:
	rm $(BINARY)
