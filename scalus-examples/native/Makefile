CXX=clang
LIBSCALUS_PATH=../../scalus-core/native/target/scala-3.3.6
CXXFLAGS=-Wall -I.
LDFLAGS=-L$(LIBSCALUS_PATH)
LIBS=-lsodium -lsecp256k1 -lc++ -lscalus


all: example

example: main.c $(LIBSCALUS_PATH)/libscalus.a
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o example main.c $(LIBS)

clean:
	rm -f example

run: clean all
	./example
