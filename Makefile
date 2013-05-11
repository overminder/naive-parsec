Main : $(shell find -name "*.hs")
	ghc --make -O Main.hs
