# This makefile is used to create .tgz .zip versions
# of the BCPL distribution.

PUB = /homes/mr/public_html

# Public HTML directory if not mountable on this machine
# and the shared drive is called E: (/dose on Linux).
# Remember to call ssh-add before calling make sshpub.
SSHPUB = mr10@ely.cl.cam.ac.uk:public_html

help:
	@echo
	@echo "make all          Construct files: bcpl.tgz and bcpl.zip"
	@echo "                  leaving them in distribution/"
	@echo "make sshpube      Put them in /dose and my home page using scp"
	@echo
	@echo "make allnew       Construct files: bcplnew.tgz and bcplnew.zip"
	@echo "make dosenew      Put them in my E drive"
	@echo "make sshpubenew   Put them in /dose and my home page using scp"
	@echo
	@echo "make copytoprev   Copy bcpl.tgz, bcpl.zip and FILES to" 
	@echo "                  bcplprev.tgz, bcplprev.zip and FILESPREV" 
	@echo "                  on my homepage" 
	@echo 


all:
	rm -f *~ */*~
	rm -f TGZFILES
	date >TGZDATE
	cp TGZDATE cintcode/TGZDATE
	cp cintcode/doc/README .
	(cd cintcode; make vclean)
	(cd natbcpl; make clean)
	(cd bcplprogs; make vclean)
	(cd ..; tar cvzf bcpl.tgz BCPL)
	(cd ..; rm -f bcpl.zip)
	(cd ..;  zip -rv9 bcpl.zip BCPL)
	ls -l ../bcpl.tgz ../bcpl.zip >TGZFILES

allnew:
	rm -f *~ */*~
	rm -f TGZFILESNEW
	date >TGZDATENEW
	cp TGZDATENEW cintcode/TGZDATENEW
	cp cintcode/doc/README .
	(cd cintcode; make vclean)
	(cd natbcpl; make clean)
	(cd bcplprogs; make vclean)
	(cd ..; tar cvzf bcplnew.tgz BCPL)
	(cd ..; rm -f bcplnew.zip)
	(cd ..;  zip -rv9 bcplnew.zip BCPL)
	ls -l ../bcplnew.tgz ../bcplnew.zip >TGZFILES

sshpube:	dose
	scp README TGZFILES ../bcpl.tgz ../bcpl.zip $(SSHPUB)/BCPL
	cp TGZDATE PUBDATE
	cp TGZDATE cintcode/PUBDATE
	cp TGZFILES PUBFILES
	@cat TGZFILES
	@cat TGZDATE

sshpubenew:	dosenew
	scp README TGZFILESNEW ../bcplnew.tgz ../bcplnew.zip $(SSHPUB)/BCPL
	cp TGZDATENEW PUBDATENEW
	cp TGZDATENW cintcode/PUBDATENEW
	cp TGZFILESNEW PUBFILESNEW
	@cat TGZFILESNEW
	@cat TGZDATENEW

copytoprev:
	scp $(SSHPUB)/BCPL/TGZFILES $(SSHPUB)/BCPL/TGZFILESPREV
	scp $(SSHPUB)/BCPL/bcpl.tgz $(SSHPUB)/BCPL/bcplprev.tgz
	scp $(SSHPUB)/BCPL/bcpl.zip $(SSHPUB)/BCPL/bcplprev.zip
	@echo "bcplprev.tgz bcplprev.zipand TGZFILESPREV updated"
	@echo


dose:	all
	cp ../bcpl.tgz ../bcpl.zip /dose

dosenew:	allnew
	cp ../bcplnew.tgz ../bcplnew.zip /dose
