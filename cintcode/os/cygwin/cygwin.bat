# This .bat file is obsolete. It is better add a line to .bashrc
# to invoke the script: setbcplenv

@echo off
echo Initialising Cintcode BCPL environment

SET HOME=/home/mr10

SET BCPLROOT=%HOME%/distribution/BCPL/cintcode
SET BCPLPATH=%BCPLROOT%/cin
SET BCPLHDRS=%BCPLROOT%/g
SET BCPLSCRIPTS=%BCPLROOT%/s

SET BCPL64ROOT=%HOME%/distribution/BCPL/cintcode
SET BCPL64PATH=%BCPL64ROOT%/cin64
SET BCPL64HDRS=%BCPL64ROOT%/g
SET BCPL64SCRIPTS=%BCPL64ROOT%/s64

SET POSROOT=%HOME%/distribution/Cintpos/cintpos
SET POSPATH=%POSROOT%/cin
SET POSHDRS=%POSROOT%/g
SET POSSCRIPTS=%POSROOT%/s

SET MCPLROOT=%HOME%/distribution/MCPL/mintcode
SET MCPLPATH=%MCPLROOT%/min
SET MCPLHDRS=%MCPLROOT%/g
SET MCPLSCRIPTS=%MCPLROOT%/s

SET PATH=.;%BCPLROOT%/bin;%POSROOT%/bin;%MCPLROOT%;C:\cygwin\home\mr10\bin;e:\devkitGP2X\bin

C:
chdir C:\cygwin\bin

bash --login -i
