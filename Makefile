
TARGET=solver

all: $(TARGET)

$(TARGET):
	ocamlbuild main.native
	mv main.native $(TARGET)

clean:
	ocamlbuild -clean

