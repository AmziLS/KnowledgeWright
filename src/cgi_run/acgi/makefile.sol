#debug
#C_FLAGS = -g
#release
C_FLAGS =

CC = CC

all: acgi

acgi: amzicgi.c amzisub.c $(AMZI_DEV_DIR)/lib/libamzi.so
	$(CC) -I$(AMZI_DEV_DIR)/include -L$(AMZI_DEV_DIR)/lib $(C_FLAGS) -o $(AMZI_DEV_DIR)/lsapis/cgi/acgi \
	   amzicgi.c amzisub.c -lamzi
	acmp acgi
	cp acgi.plm $(AMZI_DEV_DIR)/abin/acgi.plm

clean:
	-rm *.o

