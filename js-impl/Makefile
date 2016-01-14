OUT := parsec-compiled.js
BABEL := node_modules/.bin/babel

all : run

run : $(OUT)
	node $<

%-compiled.js : %.js $(BABEL)
	$(BABEL) --presets es2015 $< --out-file $@

$(BABEL) :
	npm install --save

clean :
	rm $(OUT)
