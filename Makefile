
TARGET=solver

all: $(TARGET)

$(TARGET):
	ocamlbuild -lib str main.native
	mv main.native $(TARGET)

clean:
	ocamlbuild -clean

