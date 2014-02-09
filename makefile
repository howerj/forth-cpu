## Top level make file
#
#	Richard James Howe
#
# TODO:
# 	* The user should just be able to type "make" and it
# 	will create *everything* in the project.
#

DOXYFILE=doxygen.conf

all: _vhdl _doxygen

_welcome:
	@echo "H2 Computing system by Richard James Howe";

_vhdl:
	@echo "Making the VHDL simulation and bitfile for the device";
	@make -C vhd/ simulation synthesis implementation bitfile

_doxygen:
	@echo "Generating system documentation";
	@doxygen $(DOXYFILE)

clean:
	@rm -rf doc/doxy/
	@make -C vhd/ clean
