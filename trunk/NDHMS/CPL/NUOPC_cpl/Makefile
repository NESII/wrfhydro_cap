#
# --- WRF-HYDRO makefile 
# --- Requires ESMVv7+
# --- WRF-HYDRO ESMF component.
#

# ###############
# Local Variables
# ###############

HR    := ========================================
HR    := $(HR)$(HR)
COMMA := ,
DIR   := $(CURDIR)

# ###########################
# Include ESMFMKFILE fragment
# ###########################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif
include $(ESMFMKFILE)

# #########################
# Determine Build Precision
# #########################

ifeq ($(BUILD_PREC),r4)
override ESMF_F90COMPILECPPFLAGS += -DREAL4
else
override ESMF_F90COMPILECPPFLAGS += -DREAL8
endif

# ###########################
# Determine Installation Path
# ###########################

ifndef DESTDIR
DESTDIR  := $(DIR)
endif

ifndef INSTDIR
INSTDIR  := WRFHYDRO_$(shell date '+%Y-%m-%d-%H-%M-%S')
endif

INSTPATH := $(abspath $(DESTDIR)/$(INSTDIR))

# ###############
# Model Variables
# ###############

MODEL_DIR    := $(abspath $(DIR)/../..)
MODEL_LIBDIR := $(abspath $(MODEL_DIR)/lib)
MODEL_MODDIR := $(abspath $(MODEL_DIR)/mod)
MODEL_MPPDIR := $(abspath $(MODEL_DIR)/MPP)
MODEL_LIB    := $(abspath $(MODEL_LIBDIR)/libHYDRO.a)
MODEL_MK     := $(abspath $(MODEL_DIR)/Makefile.comm)
MODEL_MKINC  := $(abspath $(MODEL_DIR)/macros)
MODEL_MODS   := $(abspath $(MODEL_MPPDIR)/module_mpp_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MPPDIR)/module_cpl_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_drv.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_rt_data.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_namelist.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_io.mod)
MODEL_FILES  := $(MODEL_LIB) $(MODEL_MODS)

# #############
# Cap Variables
# #############

CAP_DIR       := $(abspath $(DIR))
CAP_LIB       := libwrfhydro_nuopc.a
CAP_MK        := wrfhydro.mk
CAP_DEP_FRONT := wrfhydro_nuopc
CAP_VERS      := version
CAP_OBJS      := WRFHydro_NUOPC_Cap.o
CAP_OBJS      += WRFHydro_NUOPC_Gluecode.o
CAP_OBJS      += beta_NUOPC_Copy.o
CAP_OBJS      += beta_NUOPC_FileRead.o
CAP_OBJS      += beta_NUOPC_Auxiliary.o
CAP_OBJS      += beta_NUOPC_Fill.o
CAP_OBJS      += beta_NUOPC_Log.o
CAP_OBJS      += beta_NUOPC_Base.o
CAP_MOD       := wrfhydro_nuopc.mod
CAP_FILES     := $(CAP_LIB) $(CAP_MOD)

# ###############################
# Include Model Makefile Fragment
# ###############################

include $(MODEL_MKINC)
ESMF_F90COMPILEPATHS    += -I$(MODEL_MODDIR)
ESMF_F90COMPILEPATHS    += -I$(MODEL_MPPDIR)
ESMF_F90COMPILECPPFLAGS += $(HYDRO_D)

ifeq ($(DEBUG),on)
ESMF_F90COMPILEOPTS     += -g -traceback
ESMF_CXXCOMPILEOPTS     += -g -traceback
ESMF_F90COMPILECPPFLAGS += -DDEBUG
ESMF_CXXCOMPILECPPFLAGS += -DDEBUG
endif


# #######################
# Primary Makefile Target
# #######################
.PHONY: nuopc nuopcinstall nuopcclean clean_cap clean_model

nuopc: $(CAP_FILES) nuopcinstall

# ############
# Dependencies
# ############

WRFHydro_NUOPC_Cap.o: WRFHydro_NUOPC_Gluecode.o \
        beta_NUOPC_Fill.o beta_NUOPC_Auxiliary.o \
        beta_NUOPC_Log.o beta_NUOPC_Base.o \
        WRFHydro_NUOPC_Macros.h
WRFHydro_NUOPC_Gluecode.o: beta_NUOPC_Copy.o \
        beta_NUOPC_Log.o beta_NUOPC_FileRead.o \
        WRFHydro_NUOPC_Macros.h \
        $(MODEL_FILES)

# ###############
# Build Model
# ###############
$(MODEL_FILES):
	@echo $(HR)
	@echo "Building Model..."
	@echo $@
	@echo
	$(call checkdir, $(MODEL_DIR))
	(cd $(MODEL_DIR); make -f Makefile.comm)
	
# ##############
# Build Settings
# ##############

.SUFFIXES: 
.SUFFIXES: .c .C .f90 .F90 .F .f

MODEL_F90COMPILEPATHS =-I$(MODEL_DIR)/$(MODEL_MPPDIR) -I$(MODEL_DIR)/$(MODEL_MODDIR)

.F:
	@echo "Must have an explicit rule for" $*
.f:
	@echo "Must have an explicit rule for" $*
.C:
	@echo "Must have an explicit rule for" $*
.c: 
	@echo "Must have an explicit rule for" $*
	
%.o : %.f90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
# #####################
# Build NUOPC Component
# #####################

$(CAP_LIB): $(MODEL_LIB) $(CAP_OBJS)
	@echo $(HR)
	@echo "Creating static library $@..."
	@echo
	ar cruT $@ $^

# -----------------------------------------------------------------------------
# Install Library, Modules, and Makefile Fragment
# -----------------------------------------------------------------------------

nuopcinstall: $(INSTPATH) \
  $(addprefix $(INSTPATH)/,$(CAP_MOD)) \
  $(addprefix $(INSTPATH)/,$(CAP_LIB)) \
  $(addprefix $(INSTPATH)/,$(CAP_MK)) \
  $(addprefix $(INSTPATH)/,$(CAP_VERS))

$(INSTPATH):
	@echo $(HR)
	@echo "Creating Installation Directory $@"
	@echo
	@mkdir -p $(INSTPATH)

$(addprefix $(INSTPATH)/,$(CAP_MOD)): $(CAP_MOD)
	@echo $(HR)
	@echo "Installing Module Files"
	@echo
	@cp $(CAP_MOD) $(INSTPATH)

$(addprefix $(INSTPATH)/,$(CAP_LIB)): $(CAP_LIB)
	@echo $(HR)
	@echo
	@echo "Installing Library File"
	@cp $(CAP_LIB) $(INSTPATH)

$(addprefix $(INSTPATH)/,$(CAP_MK)): 
	@echo $(HR)
	@echo "Generating NUOPC Makefile Fragment"
	@echo
	@echo "# ESMF self-describing build dependency makefile fragment" > $@
	@echo "" >> $@
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $@
	@echo "ESMF_DEP_INCPATH   = $(INSTPATH)" >> $@
	@echo "ESMF_DEP_CMPL_OBJS = " >> $@
	@echo "ESMF_DEP_LINK_OBJS = $(INSTPATH)/$(CAP_LIB)" >> $@
	@echo "ESMF_DEP_SHRD_PATH = " >> $@
	@echo "ESMF_DEP_SHRD_LIBS = " >> $@
	@echo $(HR)

$(addprefix $(INSTPATH)/,$(CAP_VERS)):
	@echo $(HR)
	@echo "Generating Version Information"
	@echo
	@echo "# NUOPC Cap Version" > $@
	@if [ -d .svn ]; then \
	  echo "SVN Repository" > $@; \
	  svn info . | grep URL >> $@; \
	  svn info . | grep "Last Changed Rev" >> $@; \
	fi
	@if [ -d `git rev-parse --git-dir` ]; then \
	  echo "Git Repository" > $@; \
	  git show . | grep -m 1 "commit " >> $@; \
	  git show . | grep -m 1 "Author: " >> $@; \
	  git show . | grep -m 1 "Date: " >> $@; \
	fi

# ###########
# Check Build
# ###########

define checkfile
	@if [ ! -e $(1) ]; then \
	echo "File is missing:$(1)"; \
	exit 1; fi;

endef # blank line in checkfile is required

define checkdir
	@if [ ! -d $(1) ]; then \
	echo "Directory is missing:$(1)"; \
	exit 1; fi;
endef # blank line in checkdir is required

check: check_esmf check_model check_cap

# ##################
# Check ESMF Version
# ##################

check_esmf:
	@echo $(HR)
	@echo "Checking ESMFMKFILE file..."
	@echo
	@echo "ESMFMKFILE=$(ESMFMKFILE)"
	@if [ "$(ESMF_VERSION_MAJOR)" -lt 7 ]; then \
	echo "Please use ESMF version 7+"; \
	exit 1; fi;
	@echo "ESMF Version=$(ESMF_VERSION_STRING)"
	

# ###########
# Check Model
# ###########

check_model:
	@echo $(HR)
	@echo "Checking for Model files..."
	@echo
	$(foreach FILENAME, $(MODEL_FILES), $(call checkfile, $(FILENAME)))

# #########
# Check Cap
# #########

check_cap: 
	@echo $(HR)
	@echo "Checking for WRF-Hydro NUOPC files..."
	@echo
	$(foreach FILENAME, $(CAP_FILES), $(call checkfile, $(FILENAME)))

# #########
# Clean all
# #########

nuopcclean: clean_cap clean_model

# ##########
# Clean  Cap
# ##########

clean_cap:
	@echo $(HR)
	@echo "Cleaning Cap build..."
	@echo
	rm -f $(CAP_FILES) *.mod *.o *.a $(CAP_MK) $(CAP_VERS)

# ###########
# Clean Model
# ###########

clean_model:
	@echo $(HR)
	@echo "Cleaning Model build..."
	@echo ""
	$(call checkdir, $(MODEL_DIR))
	(cd $(MODEL_DIR); make -f $(MODEL_MK) clean)

# ------------------------------------------------------------------------------
