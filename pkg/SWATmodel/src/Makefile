DIRS	= SWAT2012
ECHO	= echo

all : swatp

swat12 : force_look
	$(ECHO) looking into swat2012: $(MAKE) $(MFLAGS)
	cd SWAT2012; $(MAKE) $(MFLAGS)

swatp : force_look
	$(ECHO) looking into swatp: $(MAKE) $(MFLAGS)
	cd SWATP; $(MAKE) $(MFLAGS)
clean :
	$(ECHO) cleaning up in .
	-rm -f $(EXE) $(OBJS) $(OBJLIBS)
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done

force_look :
	true
