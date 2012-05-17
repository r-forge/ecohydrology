DIRS	= SWAT2005
CD	= cd
RM	= rm
#SHELL	= /bin/sh
ECHO	= echo

all : swat2005

swat2005 : force_look
	$(ECHO) looking into swat2005: $(MAKE) $(MFLAGS)
	cd SWAT2005; $(MAKE) $(MFLAGS)

clean :
	$(ECHO) cleaning up in .
	-$(RM) -f $(EXE) $(OBJS) $(OBJLIBS)
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done

force_look :
	true
