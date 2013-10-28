DUSTDIR=Dust
SQRLDIR=SQRL
ED25519DIR=$(DUSTDIR)/ed25519-donna

CFLAGS=-lcrypto

EXTERNAL=$(DUSTDIR)/Ed25519.hs $(ED25519DIR)/ed25519.c $(SQRLDIR)/*.hs
SRC=*.hs

all: $(EXTERNAL) $(SRC)
	ghc -o main $(CFLAGS) $(EXTERNAL) $(SRC)
	rm -f $(DUSTDIR)/*.hi $(SQRLDIR)/*.hi *.hi $(DUSTDIR)/*.o $(SQRLDIR)/*.o *.o

clean:
	rm -f $(DUSTDIR)/*.hi $(SQRLDIR)/*.hi *.hi $(DUSTDIR)/*.o $(SQRLDIR)/*.o *.o
	rm -f main
