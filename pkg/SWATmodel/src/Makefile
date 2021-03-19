DIRS	= SWAT2012
ECHO	= echo

all : swat12

swat12 : force_look
	$(ECHO) looking into swat2012: $(MAKE) $(MFLAGS)
	cd SWAT2012; $(MAKE) $(MFLAGS)

clean :
	$(ECHO) cleaning up in .
	-rm -f $(EXE) $(OBJS) $(OBJLIBS)
	-for d in $(DIRS); do (cd $$d; rm -f *~ *.o *.exe *.so *.mod); done

force_look :
	true
