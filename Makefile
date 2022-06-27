FILES := $(shell find $(SOURCEDIR)src -maxdepth 1 -name '*.hs')
OUT_FILE=simplify-bkg
OUT_ZIP_FILE=flp-fun-xskuta04.zip

phony: all

# Compile
all:
	ghc ${FILES} -o ${OUT_FILE}

# Remove all files generated by compilation
clean:
	@rm ${OUT_FILE} $(SOURCEDIR)src/*.o $(SOURCEDIR)src/*.hi ${OUT_ZIP_FILE} || true

# Check if there are any hints for Haskell files
hlint:
	hlint $(FILES)

zip:
	zip -r ${OUT_ZIP_FILE} ./