DUSTDIR=Dust
ED25519DIR=$(DUSTDIR)/ed25519-donna

CFLAGS=-lcrypto

EXTERNAL=$(DUSTDIR)/Ed25519.hs $(ED25519DIR)/ed25519.c
SRC=*.hs

all: $(EXTERNAL) $(SRC)
	ghc -o main $(CFLAGS) $(EXTERNAL) $(SRC)
	rm -f $(DUSTDIR)/*.hi *.hi $(DUSTDIR)/*.o *.o

clean:
	rm -f $(DUSTDIR)/*.hi *.hi $(DUSTDIR)/*.o *.o
	rm -f main
