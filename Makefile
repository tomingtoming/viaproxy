all: viaproxy.exe

viaproxy.exe: viaproxy.hs clean
	ghc -O3 -threaded -with-rtsopts="-N" viaproxy.hs

clean:
	rm -fv viaproxy.hi viaproxy.o viaproxy.exe viaproxy
