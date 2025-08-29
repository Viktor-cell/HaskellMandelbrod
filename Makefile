i:
	ghci Main.hs -I./raylib/include/ -L./raylib/lib/ -lraylib
b:
	ghc Main.hs -I./raylib/include/ -include ./raylibWrapper.h -L./raylib/lib/ -lraylib
