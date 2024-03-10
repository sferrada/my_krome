"""
	KROME is a nice and friendly chemistry package for a wide range of
	astrophysical simulations. Given a chemical network (in CSV format)
	it automatically generates all the routines needed to solve the kinetic
	of the system, modelled as system of coupled Ordinary Differential
	Equations.
	It provides different options which make it unique and very flexible.
	Any suggestions and comments are welcomed. KROME is an open-source
	package, GNU-licensed, and any improvements provided by
	the users is well accepted. See disclaimer below and GNU License
	in gpl-3.0.txt.

	more details in http://kromepackage.org/
	also see https://bitbucket.org/krome/krome_stable

	Written and developed by Tommaso Grassi
	tgrassi@nbi.dk,
	Starplan Center, Copenhagen.
	Niels Bohr Institute, Copenhagen.

	and Stefano Bovino
	stefano.bovino@uni-hamburg.de
	Hamburger Sternwarte, Hamburg.

	Others (alphabetically): D.Galli, F.A.Gianturco, T.Haugboelle,
	J.Prieto, J.Ramsey, D.R.G.Schleicher, D.Seifried, E.Simoncini,
	E.Tognelli

	KROME is provided \"as it is\", without any warranty.
	The Authors assume no liability for any damages of any kind
	(direct or indirect damages, contractual or non-contractual
	damages, pecuniary or non-pecuniary damages), directly or
	indirectly derived or arising from the correct or incorrect
	usage of KROME, in any possible environment, or arising from
	the impossibility to use, fully or partially, the software,
	or any bug or malefunction.
	Such exclusion of liability expressly includes any damages
	including the loss of data of any kind (including personal data)
"""
# THIS FILE CONTAINS THE KROME CLASS
import os
import re
import glob
import shutil
import argparse
import copy
from os import listdir
from os.path import isfile, join
from kromelib import *

class krome:
	#set defaults
	solver_MF = 222
	force_rwork = useHeating = doReport = checkConserv = useFileIdx = buildCompact = useEquilibrium = False
	use_implicit_RHS = use_photons = useTabs = useDvodeF90 = useTopology = useFlux = skipDup = False
	useCoolingAtomic = useCoolingH2 = useCoolingH2GP98 = useCoolingHD = useCoolingZ = False
	useCoolingCompton = useCoolingExpansion = useShieldingDB96 = useShieldingWG11 = useShieldingR14 = False
	useCoolingCIE = useCoolingDISS = useCoolingFF = use_cooling = useCoolingDust = useCoolingCont = False
	useCoolingZCIE = useCoolingZCIENOUV = useCoolingZExtended  = useCoolingGH = False
	useCoolingCO = useCustom = useDustTabs = dustTabsCool = dustTabsH2 = dustTabsAvVariable = False
	useCoolingHCN = useCoolingOH = useCoolingH2O = False
	useReverse = useCustomCoe = useODEConstant = cleanBuild = usePlainIsotopes = useDust = usePhotoDust_3D = False
	use_thermo = useStars = useNuclearMult = useCoolingdH = useHeatingdH = useCoolingChem = False
	usePhIoniz = useHeatingCompress = useHeatingPhoto = useHeatingChem = useDecoupled = False
	useHeatingCR = useHeatingPhotoAv = useHeatingPhotoDust = useHeatingXRay = useThermoToggle = useHeatingPhotoDustNet = False
	useX = pedanticMakefile = useFakeOpacity = useConserve = useConserveE = useConserveLin = noExample = useNLEQ = False
	usePhotoOpacity = useXRay = hasSurfaceReactions = shieldHabingDust = False
	has_plot = doIndent = useTlimits = useODEthermo = safe = doJacobian = sinkCheck = recCheck = shortHead = True
	useDustGrowth = useDustSputter = useDustH2 = useDustT = useDustEvap = useDustH2const = False
	doRamses = doRamsesTH = doFlash = doEnzo = doGizmo = interfaceC = interfacePy = mergeTlimits = False
	isdry = useIERR = checkReverse = usePhotoInduced = checkThermochem = needLAPACK = useCoolFloor = False
	useComputeElectrons = useChemisorption = useSemenov = usedTdust = useSurface = useHeatingVisc = False
	useHeatingPumpH2 = reducer = useFexCustom = hasStoreOnceRates = useBroadening = False
	verbatimFilename = "reactions_verbatim.dat"
	useVerbatimFile = True
	xsecKernelFunction = "" #kernel function for interpolating xsecs
	humanFlux = True
	dustTableMode = "" #type of dust tables required
	dustTableDimension = "2D"
	typeGamma = "DEFAULT"
	test_name = "default"
	test_status = "OK"
	is_test = False
	TlimitOpLow = "GE"
	TlimitOpHigh = "LT"
	customCoeFunction = "[CUSTOM COE FUNCTION NOT SET!]"
	buildFolder = "build/"
	srcFolder = "src/"
	TminAuto = 1e99
	TmaxAuto = 0e0
	H2opacity = "" #H2 opacity model
	checkMode = "ALL" #conservation check mode (ALL | [CHARGE],[MASS]| NONE)
	RTOL = 1e-4 #default relative tolerance
	ATOL = 1e-20 #default absolute tolerance
	coolingQuench = -1e0 #if coolingQuench is negative cooling quench is not enabled, otherwise this is Tcrit
	dustArraySize = dustTypesSize = photoBins = 0
	maxord = 0 #default solver maximum order (0=automatic)
	dustTypes = []
	specs = []
	reacts = []
	constantList = []
	dummy = molec()
	coevars = dict() #variables in function coe() (krome_subs.f90)
	coolVars = dict() #variables for custom cooling (krome_cooling.f90)
	heatVars = dict() #variables for custom heating (krome_heating.f90)
	coevarsODE = dict() #variables in function fex() (krome_ode.f90)
	commonvars = [] #list of common variables
	implicit_arrays = totMetals = ""
	thermodata = dict() #thermochemistry data (nasa polynomials)
	parser = filename = ""
	deltajacMode = "RELATIVE" #increment mode: RELATIVE or ABSOLUTE
	deltajac = "1d-3" #increment (relative or absolute, see deltajacMode)
	atols = [] #custom ATOLs
	rtols = [] #custom RTOLs
	jaca = [] #unrolled sparse jacobian
	customODEs = [] #custom ODEs
	nrea = 0 #number of reactions
	nPhotoRea = 0 #number of photoreactions (for photobin array)
	dustSeed = "0d0" #default for dust seed in cm-3
	full_cool = vars_cool = ""
	coolZ_functions = []
	coolZ_rates = []
	coolZ_vars_cool = []
	coolZ_poplevelvars = [] #population levels variables
	fcn_levs = [] #list of number of cooling levels found
	coolZ_nkrates = 0
	zcoolants = [] #list of cooling read from file (flag name, e.g CII)
	Zcools = [] #list of cooling read from file (species name, e.g. C+)
	allCoolings = [] #list all coolings names from option
	allHeatings = [] #list all heatings names from option
	anytabvars = [] #variable names for the tables
	anytabfiles = [] #file name for the tables
	anytabpaths = [] #paths for the tables
	anytabsizes = [] #sizes of the tables
	coolLevels = [] #levels employed for cooling, if empty uses all
	physVariables = [] #list of the phys variables (list of [variable_name, default_value_string])
	kModifier = [] #modifier lines that will be appended after the rate calculation
	odeModifier = [] #modifier lines that will be appended after the ODE calculation
	photoPartners = dict() #dictionary of the reactants of photoreactions (key is reaction index)
	reducerVars = ["ntot", "Tgas","Zmetals"] #variables for the reducer tool interface
	columnDensityMethod = "DEFAULT"
	compiler = "ifort" #default compiler
	ramses_offset = 2 #offset in the array for ramses
	photoDustVarAv = "" #variable for visual extinction in the photoelectric heating
	photoDustVarG0 = "" #variable for normalization in the photoelectric heating
	coolFile = ["data/coolZ.dat"]
	customCoolList = [] #list of the custom cooling functions
	customHeatList = [] #list of the custom heating functions
	individualCoolingFloors = [] #list of individual floors
	iceSpeciesList = dict() #list of species on ice
	fdbase = "data/database/" #database of reaction folder for auto reactions
	thermochemistryFolder = "data/thermochemistry/"
	indexSolomon = -1 #default solomon index, -1 to trigger error
	indexH2photodissociation = -1 #default H2pd index, -1 to trigger error
	KindSingle = "real*4"
	KindDouble = "real*8"
	KindDoubleValue = "real*8"
	KindDoubleValueOptional = "real*8,optional"
	KindInteger = "integer"
	KindIntegerValue = "integer"
	KindBoolValueOptional = "logical,optional"
	KindCharacter = "character"
	BindC = ""
	version = "14.08.dev"
	codename = "Beastie Boyle"

	#########################################
	def checkPrereq(self):
		#check for argparse module
		try:
			import argparse
		except:
			print("ERROR: you need installed argparse!")
			print("You can obtain it by typing (ubuntu users):")
			print(" apt-get install python-setuptools")
			print(" easy_install argparse")
			print("")
			print("more details here:")
			print(" https://pypi.python.org/pypi/argparse")
			sys.exit()

		#check python version
		ver = sys.version_info
		aver = list(ver)
		#sver = (".".join([str(x) for x in aver[:3]]))
		hver = aver[0] * 1e4 + aver[1] * 1e2 + aver[2]
		if hver < 2e4 + 7e2:
			print("ERROR: your version of Python ("+ver+") is not supported by KROME!")
			print(" KROME needs at least Python 2.7.x!")
			sys.exit()

		#check necessary files
		fles = get_file_list() #get the list of necessary files
		for fle in fles:
			if os.path.isdir(fle):
				continue #do not check folders
			if not os.path.isfile(fle):
				print("************************************************")
				print("WARNING: the file "+fle+" is missing!")
				print("Do you want to proceed anyway?")
				print("************************************************")
				a = keyb_input("Any key to ignore q to quit... ")
				if a == "q":
					sys.exit()

	#########################################
	def init_argparser(self):

		tests = ", ".join(next(os.walk('tests'))[1])
		self.parser = argparse.ArgumentParser(description="KROME a package for astrochemistry and microphysics")
		self.parser.add_argument("-ATOL", help="set solver absolute tolerance to the float or double value ATOL, e.g. -atol 1d-40\
			Default is ATOL=1d-20, see also -RTOL and -customATOL")
		self.parser.add_argument("-compact", action="store_true", help="creates a single fortran file with all the modules instead of\
			various file with the different modules. Solver files remain stand-alone (see example make in test/MakefileCompact)")
		self.parser.add_argument("-checkConserv", action="store_true", help="check mass conservation during integration (slower)")
		self.parser.add_argument("-checkReverse", action="store_true", help="check network for reverse reactions including thermochemistry.\
			Output written in build/krome_reverse.log file.")
		self.parser.add_argument("-checkThermochem", action="store_true", help="print a warning when thermochemistry data are not found\
			for a given species.")
		self.parser.add_argument("-clean", action="store_true", help="clean all in /build (including krome_user_commons.f90 that\
			is normally kept by default) before creating new f90 files.")
		self.parser.add_argument("-columnDensityMethod", metavar="method", help="use an alternative method to \
			N=1.8e21*(n*1e-3)**(2./3.) for column density calculation (N) from number density (n). Option available JEANS,\
			which employs Jeans length (l) as N=n*l, or JEANS40, which employs the Jeans length capped at 40K (Safranek-Shrader \
                        et al. 2017).")
		#self.parser.add_argument("-compressFluxes", action="store_true", help="in the ODE fluxes are stored in a single variable")
		self.parser.add_argument("-computeElectrons", action="store_true", help="computes electrons by balancing charges instead of\
			using the differential de/dt.")
		self.parser.add_argument("-conserve", action="store_true", help="conserves the species total number and charge global\
			neutrality. Works with some limitations, please read the manual.")
		self.parser.add_argument("-conserveE", action="store_true", help="conserves the charge global neutrality only.")
		self.parser.add_argument("-conserveLin", action="store_true", help="enable hydro-code oriented function to conserve mass using\
			a mass-weighted method.")
		self.parser.add_argument("-coolFile", metavar='FILENAME', help="select the filename to be used to load external cooling. See\
			also tools/lamda2.py script for a LAMDA<->KROME converter. Default FILENAME is data/coolZ.dat, which contains\
			fine-strucutre atomic metal cooling for C,O,Si,Fe, and their first ions. It can also be a list of files comma-separated.")
		self.parser.add_argument("-cooling", metavar='TERMS', help="cooling options, TERMS can be ATOMIC, H2, HD, Z, DH, DUST, H2GP98,\
			COMPTON, EXPANSION, CIE, DISS, CI, CII, SiI, SiII, OI, OII, FeI, FeII, CHEM, CO (e.g. -\
			cooling=ATOMIC,CII,OI,FeI),Z_CIE,Z_CIENOUV,Z_EXTENDED.\
			Note that further cooling options can be added when reading cooling function from file. If you want a complete list of\
			the available cooling options type -cooling=?")
		self.parser.add_argument("-coolLevels", metavar='MAXLEV', help="use only the levels up to MAXLEV (included), e.g. -coolLevels=3\
			Note that levels are zero-based (i.e. ground state is zero).")
		self.parser.add_argument("-coolingQuench", metavar='TCRIT', help="quenches the cooling when T<TCRIT with a tanh \
		 function.")
		self.parser.add_argument("-compiler", metavar='COMPILER', help="changes the Makefile according to the selected COMPILER.")
		self.parser.add_argument("-customATOL", help="file with the list of the individual ATOLs in the form SPECIES ATOL in each line,\
			e.g. H2 1d-20, see also -ATOL", metavar="filename")
		self.parser.add_argument("-customODE", help="file with the list of custom ODEs", metavar="FILENAME")
		self.parser.add_argument("-customRTOL", help="file with the list of the individual RTOLs in the form SPECIES RTOL in each line,\
			e.g. H3+ 1d-4, see also -RTOL", metavar="filename")
		self.parser.add_argument("-dry", action="store_true", help="dry pre-compilation: does not write anything in the build direactory")
		self.parser.add_argument("-dust", help="include dust ODE using N bins for each TYPE, e.g. -dust 10,C,Si set 10 dust carbon\
			bins and 10 dust silicon dust bins. Note: requires a call to the krome_init_dust subroutine.\
			See -test=dust for an example.")
		self.parser.add_argument("-dustOptions", help="activate dust options: (GROWTH) dust growth, (SPUTTER) sputtering, (H2) molecular\
			hydrogen formation on dust, (EVAP) thermal evaporation, (T) dust temperature including CMB/radiation coupling,\
			and (dT) to use dTdust/dt differential.",\
			metavar="OPTIONS")
		self.parser.add_argument("-dustTabs", help="activate dust dust tables for: (H2) molecular\
			hydrogen formation on dust, and/or (COOL) cooling. Note that this tables depends on the environment (radiation, metallicity,\
			dust type, dust power law characteristics, ...). To change enviroment you have to indicate a mode. Details are in the folder\
			data/dust_tabs/ in the headers of the files. e.g. -dustTabs=H2,HM2012",
			metavar="OPTIONS")
		self.parser.add_argument("-dustSeed", help="set the dust seed in 1/cm3 for dust growth. Default is zero. Any F90 expression \
			is allowed for SEED.", metavar="SEED")
		self.parser.add_argument("-enzo", action="store_true", help="create patches for ENZO")
		self.parser.add_argument("-fexArgument", action="store_true", help="add ODE function (fex) as additional argument to \
			the main call to KROME")
		self.parser.add_argument("-flash", action="store_true", help="create patches for FLASH")
		self.parser.add_argument("-forceMF21", action="store_true", help="force explicit sparsity and Jacobian")
		self.parser.add_argument("-forceMF222", action="store_true", help="force internal-generated sparsity and Jacobian")
		self.parser.add_argument("-forceRWORK", help="force the size of RWORK to N", metavar="N")
		self.parser.add_argument("-gamma",help="define the adiabatic index according to OPTION that can be FULL for employing Grassi et al.\
			2011, i.e. a density dependent but temperature independent adiabatic index, VIB to keep into account the vibrational\
                        paritition function, ROT to keep into account the rotational partition function, EXACT to evaluate the\
                        adiabatic index accurately taking into account both contributions, or REDUCED to use only H2 and CO as diatomic\
			molecules (faster). Finally a custom F90 expression e.g. -gamma=\"1d0\"\
			can also be used. Default value is 5/3.",metavar="OPTION")
		self.parser.add_argument("-gizmo", action="store_true", help="create patches for Gizmo")
		self.parser.add_argument("-H2opacity", metavar="TYPE",help="use H2 opacity for H2 cooling, TYPE can be RIPAMONTI or OMUKAI")
		self.parser.add_argument("-heating", metavar='TERMS', help="heating options, TERMS can be COMPRESS, PHOTO, CHEM\
			, DH, CR, PHOTOAV,VISCOUS. If you want a complete list of the available heating options type -heating=?")
		self.parser.add_argument("-ierr", action="store_true", help="same as -useIERR")
		self.parser.add_argument("-interfaceC", action="store_true", help="create a C wrapper")
		self.parser.add_argument("-interfacePy", action="store_true", help="create a Python wrapper (and a C wrapper \
			since its a pre-requisite)")
		self.parser.add_argument("-iRHS", action="store_true", help="implicit loop-based RHS (suggested for large systems).")
		self.parser.add_argument("-lh", action="store_true", help="use long header in f90 files.")
		self.parser.add_argument("-listAutomatics", action="store_true", help="list all the automatic reactions available.")
		self.parser.add_argument("-listSWRI", action="store_true", help="list all the photo reactions available in the SWRI database.")
		self.parser.add_argument("-maxord", help="max order of the BDF solver. Default (and maximum values) is 5.")
		self.parser.add_argument("-mergeTlimits", action="store_true", help="use the same reaction index for equivalent\
			reactions (same reactants and products) that have different temperature limits")
		self.parser.add_argument("-n", help="reaction network file", metavar='FILENAME')
		self.parser.add_argument("-network", help="same as -n", metavar='FILENAME')
		self.parser.add_argument("-nochargeCheck", action="store_true", help="skip reaction charge check")
		self.parser.add_argument("-noCheck", action="store_true", help="skip reaction charge and mass check. Equivalent to\
			-nomassCheck -nochargeCheck options.")
		self.parser.add_argument("-noExample", action="store_true", help="do not write test.f90 and Makefile in the build directory")
		self.parser.add_argument("-nomassCheck", action="store_true", help="skip reaction mass check")
		self.parser.add_argument("-noRecCheck", action="store_true", help="skip recombination check (species that do not\
			 recombine with electrons).")
		self.parser.add_argument("-noSinkCheck", action="store_true", help="skip sink check (species that are only formed)")
		self.parser.add_argument("-noTlimits", action="store_true", help="ignore rate coefficient temperature limits.")
		self.parser.add_argument("-verbatimFilename", metavar='FILENAME', help="path to file with reaction names\
			(ignored if -noVerbatimFile is set). Default is `reactions_verbatim.dat`")
		self.parser.add_argument("-noVerbatimFile", action="store_true", help="do not read the file with reaction names")
		self.parser.add_argument("-nuclearMult", action="store_true", help="keep into account reactants multeplicity, and modify\
			fluxes according to this. Intended for nuclear networks.")
		self.parser.add_argument("-options", metavar="filename", help="read the options from a file instead of command line\
			(in principle you can use both). See options_example file.")
		self.parser.add_argument("-pedantic", action="store_true", help="uses a pedantic Makefile (debug purposes)")
		self.parser.add_argument("-photoDustVarAv", metavar="common_variable", help="set the name of the common variable that\
			is employed for the visual extinction Av to attenuate the photoelectric effect on the dust. It follows\
			G0*exp(-2.5*Av) where Av is the variable. The variable should be set in the network file using the token\
			@common: user_Av, or any other custom name. This option must be used togheter with -heating=PHOTODUST")
		self.parser.add_argument("-photoDustVarG0", metavar="common_variable", help="set the name of the common variable that\
			is employed for the normalization G0 to attenuate the photoelectric effect on the dust. It follows\
			G0*exp(-2.5*Av) where G0 is the variable. The variable should be set in the network file using the token\
			@common: user_G0, or any other custom name. This option must be used togheter with -heating=PHOTODUST")
		self.parser.add_argument("-project", help="build everything in a folder called build_NAME instead of building all in the\
			default build folder. It also creates a NAME.kpj file with the krome input used.",metavar="NAME")
		self.parser.add_argument("-quote", action="store_true", help="print a citation and exit")
		self.parser.add_argument("-quotelist", action="store_true", help="print all the citations and exit")
		self.parser.add_argument("-ramses", action="store_true", help="create patches for RAMSES, see also -enzo and -flash")
		self.parser.add_argument("-ramsesOffset", metavar="offset", help="add an offset to the array of the passive scalar. The\
			default is 3.")
		self.parser.add_argument("-reducer", action="store_true", help="Create the interface to the reaction reducer (experimental).\
			Variables are ntot, Tgas, and all the custom variable set with @common in the network file (i.e. user_*)")
		self.parser.add_argument("-ramsesTH", action="store_true", help="create patches for RAMSES_TH. This is a private version\
			and probably does not fix your needs.")
		self.parser.add_argument("-report", action="store_true", help="generate report file in the main call to krome as\
			KROME_ERROR_REPORT and when calling the fex as KROME_ODE_REPORT. It also stores abundances evolution in fex as \
			fort.98, and prepares a report.gps gnuplot script file to plot evolutions callable in gnuplot with load \
			'report.gps'. Warning: it slows the whole system!")
		self.parser.add_argument("-reverse", action="store_true", help="create reverse reaction from the current network\
			using NASA polynomials.")
		self.parser.add_argument("-RTOL", help="set solver relative tolerance to the float double value RTOL, e.g.\
			-RTOL 1e-5 Default is RTOL=1d-4, see also -ATOL and -customRTOL")
		self.parser.add_argument("-photoBins", metavar="NBINS", help="define the number of frequency bins for the impinging radiation.")
		self.parser.add_argument("-sh", action="store_true", help="write a shorter header in the f90 files. Now this is the default, \
			here for retrocompatibility, see option -lh.")
		self.parser.add_argument("-shielding", metavar="TYPE", help="use H2 self-shielding, TYPE can be DB96 for Draine+Bertoldi 1996,\
    		WG11 for the more accurate Wolcott+Greene 2011, R14 for the Tgas-dependent by Richings+2014")
		self.parser.add_argument("-shieldHabingDust", action="store_true", help="dust shielding for Habing flux \
			(when calculated from photobins).")
		self.parser.add_argument("-skipDevTest", action="store_true", help="exit if test under development found.")
		self.parser.add_argument("-skipDup", action="store_true", help="skip duplicate reactions")
		self.parser.add_argument("-skipJacobian", action="store_true", help="do not write Jacobian in krome_ode.f90 file. Useful\
			to reduce compilation time when Jacobian is not needed (MF=222).")
		self.parser.add_argument("-skipODEthermo", action="store_true", help="do not compute dT/dt in the ODE RHS function (fex)")
		self.parser.add_argument("-source", metavar="folder", help="use FOLDER as source directory")
		self.parser.add_argument("-stars", action="store_true", help="use star module for nuclear reactions. NOTE: krome_stars\
			module required in the Makefile")
		self.parser.add_argument("-test",help=("Create a test model in /build. TEST can be: "+tests+"."))
		self.parser.add_argument("-Tlimit", metavar="opLow,opHigh", help="set the operators for all the reaction temperature limits\
			where opLow is the operator for the first temperature value in the reaction file, and opHigh is for the second one. e.g.\
			if the T limits for a given reaction are 10. and 1d4 the option -Tlmit GE,LE will provide (Tgas>=10. AND Tgas<=1d4) as\
			the reaction range of validity. Operators opLow and opHigh must be one of the following: LE, GE, LT, GT.")
		self.parser.add_argument("-unsafe", action="store_true", help="skip to check if the build folder is empty or not")
		self.parser.add_argument("-useBroadening", action="store_true", help="use broadening (note: be careful!).")

		self.parser.add_argument("-useCoolFloor", action="store_true", help="include a cooling floor given by the Tfloor temperature.\
			note that you must define Tfloor by using the subroutine krome_set_Tfloor(your_Tfloor) before calling krome.")
		#self.parser.add_argument("-useCoolCMBFloorZ", action="store_true", help="as -useCoolCMBFloor, but for metals only.")
		self.parser.add_argument("-useCustomCoe", help="use a user-defined custom function that returns a real*8 array of size\
			NREA = number of reactions, that replaces the standard rate coefficient calculation function. Note that FUNCTION\
			must be explicitly included in krome_user_commons module.", metavar="FUNCTION")
		self.parser.add_argument("-useAutoNetwork", action="store_true", help="Use a set of instruction to build an automatic network\
			instead of a pre-made one. This option changes the behaviour of -n FILENAME into -n INSTRUCTIONS. See\
			custom.dat for an example. In this case you should use -n custom.dat -useAutoNetwork")
		self.parser.add_argument("-useDustH2const", action="store_true", help="use Jura + Gnedin\
			H2 formation on dust, needs user_clump defined. Cannot be used if  you enable the dust Options.")
		self.parser.add_argument("-useDvodeF90", action="store_true", help="use Dvode implementation in F90 (slower)")
		self.parser.add_argument("-useEquilibrium", action="store_true", help="check if the solver has reached the equilbirum.\
			If so break the solver's loop and return the values found. It is useful when the system oscillates around\
			a solution (as in some photoheating cases). To be used with caution!")
		self.parser.add_argument("-useFileIdx", action="store_true", help="use the reaction index in the reaction file instead of\
			using the automatic progressive index starting from 1. Useful with rate coefficients that depends on other\
			coefficients, e.g. k(10) = 1d-2*k(3)")
		self.parser.add_argument("-useIERR", action="store_true",help="use ierr in the interface with KROME to return errors instead\
			of stopping the exectution")
		self.parser.add_argument("-useIndividualFloor", metavar="TERMS", help="applies a floor definded by Tfloor\
			at the single cooling which are specified")
		self.parser.add_argument("-useN", action="store_true",help="use number densities as input/ouput instead of\
			 mass fractions. This is the default.")
		self.parser.add_argument("-useX", action="store_true",help="use mass fractions as input/ouput instead of number densities\
			 (1/cm3)")
		self.parser.add_argument("-useODEConstant", help="postpone an expression to each ODE. EXPRESSION must be a valid f90\
			expression (e.g. *3.d0 or +1.d-10)", metavar="EXPRESSION")
		self.parser.add_argument("-usePhIoniz", action="store_true", help="includes photochemistry (obsolete)")
		self.parser.add_argument("-usePhotoInduced", action="store_true", help="includes the photo-induced transitions in the calculation\
			of the cooling according to the choosen photon flux.")
		self.parser.add_argument("-usePhotoOpacity", action="store_true", help="computes photorates using opacity as a function of \
			the species densities and the photo cross sections, i.e. exp(-sum_i N_i*sigma_i). Column densities are computed\
			from density by using the local approximation N = 1.8e21*(n/1000)**(2/3) 1/cm2.")
		self.parser.add_argument("-usePlainIsotopes", action="store_true", help="use kA format for isotopes instead of [k]A format,\
			where k is the isotopic number and A is the atom name, e.g. krome looks for 14C instead of [14]C in the reactions file.")
		self.parser.add_argument("-useSemenov", action="store_true", help="use semenov framework for surface chemistry")
		self.parser.add_argument("-useThermoToggle", action="store_true", help="include thermal calculation control. Use\
			krome_thermo_on and krome_thermo_off to switch on/off the thermal processes (i.e. cooling and heating). Default is on.")
		self.parser.add_argument("-useTabs", action="store_true", help="use tabulated rate coefficients (free parameter: temperature)")
		self.parser.add_argument("-v", action="store_true", help="print the current version of KROME")
		self.parser.add_argument("-ver", action="store_true", help="same as -v")
		self.parser.add_argument("-version", action="store_true", help="same as -v")
		self.parser.add_argument("-xsecKernelFunction", help="use a function to scale photo cross-sections when interpolated. \
				Function has to be a function of energy, i.e. f(energy). Store it in krome_user_commons.f90 module.", \
			metavar="FUNCTION")

	######################################
	#select test name
	def select_test(self,argv):
		parser = self.parser
		args = parser.parse_args()
		all_status = ["OK","dev"]
		test_status = "OK"

		if args.test:
			self.is_test = True
		else:
			return

		#read options from file
		optionFileName = "tests/" + args.test + "/options.opt"
		#check if test folder and option file exist
		if not file_exists(optionFileName):
			print("ERROR: problem loading test "+args.test+"!")
			print(" Missing option.opt file in tests/"+args.test+"/ folder or folder not present.")
			#list available tests
			tests = (", ".join(sorted(next(os.walk('tests'))[1])))
			print(" Available tests are: " + tests)
			sys.exit()

		#read option file
		fh = open(optionFileName)
		for row in fh:
			srow = row.strip()
			#skip comments and blank lines
			if srow == "":
				continue
			if srow.startswith("#"):
				continue
			#store file name
			if srow.startswith("-n "):
				(opt, filename) = [x.strip() for x in srow.split(" ") if x!=""]
				continue
			#store test status if DEV
			if srow == "DEV":
				test_status = "dev"
				continue
			#append options to argv
			argv.append(srow)
		fh.close()

		#append extra arguments if listed
		# e.g. -dustOptions=H2 -dustOptions=GROWTH is merged in one
		argall = dict() #new argv
		#loop on arguments
		for arg in argv[1:]:
			#if arguments have values split and merge
			if "=" in arg:
				(option, value) = arg.split("=")
				#merge or create new
				if option in argall:
					argall[option] += ","+value
				else:
					argall[option] = "="+value
			else:
				#no options just add argument as key
				argall[arg] = ""

		#prepare the new argv from the dictionary
		sys.argv = [argv[0]] + [k+v for (k,v) in argall.items()]

		#check if the status of the test is valid
		if test_status not in all_status:
			sys.exit("ERROR: status "+test_status+" not recognized!")

		self.filename = filename #add the network filename
		self.test_name = args.test #copy the name of the test
		self.test_status = test_status #development status of the test

	##########################################
	def argparsing(self,argv):

		args = self.parser.parse_args() #return namespace from argv

		#use short header for f90 files
		if args.v or args.ver or args.version:
			masterfile = ".git/refs/heads/master" #name of the git master file
			print("You are using KROME "+self.version+" \""+self.codename+"\"")
			#if git master file existst grep the changeset
			if file_exists(masterfile):
				changeset = open(masterfile).read()
				print("[changeset: "+changeset[:7]+"]\n")
			print(" Bye!")
			sys.exit()

		#use custom option file (load options from a file and append to argv)
		if args.options:
			fopt = args.options.strip() #get filename
			print("Reading option -options=" + fopt)
			#check if option file exists
			if not file_exists(fopt):
				print("ERROR: custom option file \""+fopt+"\" does not exist!")
				sys.exit()

			trues = ["T","TRUE","1","Y","YES","OK","YEP","SURE"]
			falses = ["F","FALSE","0","N","NO","KO","NOPE"]
			#read from file
			fho = open(fopt,"r")
			for row in fho:
				srow = row.strip()
				if srow == "":
					continue #skip blank lines
				if srow[0] == "#":
					continue #skip comments
				if srow[:2] == "//":
					continue #skip comments
				srow = srow.split("#")[0]
				srow = srow.split("//")[0]
				srow = srow.strip()
				#replace tabs
				srow = srow.replace("\t", " ")
				#replace double spaces
				while "  " in srow:
					srow = srow.replace("  ", " ")
				if srow[0] != "-":
					srow = "-"+srow

				arow = srow.split()
				if len(arow) == 1:
					sys.argv.append(arow[0].strip())
					continue
				elif len(arow) == 2:
					if arow[1].strip().upper() in trues:
						sys.argv.append(arow[0].strip())
					elif arow[1].strip().upper() in falses:
						continue
					else:
						sys.argv.append("=".join([x.strip() for x in arow]))
				else:
					print("ERROR: problems with option line in option file "+fopt)
					print(srow)
					sys.exit()

			args = self.parser.parse_args() #return updated namespace

		#project name folder (required for dev.skip file)
		if args.project:
			self.projectName = projectName = args.project
			print("Reading option -project (name="+str(projectName)+")")
			self.buildFolder = "build_"+projectName+"/"
			fout = open(projectName+".kpj","w")
			fout.write((" ".join(argv)))
			fout.close()


		#EXIT if development test found and skipDevTest enabled
		if args.skipDevTest and self.test_status == "dev":
			fh = open(self.buildFolder+"dev.skip","w")
			fh.close()
			sys.exit("THIS IS A DEV TEST (and -skipDevTest enabled): KROME ENDS!")

		#print a warning if the test is under development
		if args.test and self.test_status == "dev":
			print("************************************************")
			print("WARNING: the test \""+self.test_name+"\" is currently")
			print(" UNDER DEVELOPMENT and its results could be")
			print(" horribly wrong. ")
			print(" Some details about the test can be found in the")
			print(" test_list file in the main KROME directory.")
			print(" Do you want to proceed?")
			print("************************************************")
			a = keyb_input("Any key to ignore q to quit... ")
			if a == "q":
				sys.exit()
			print("")


		#list arguments if test
		if args.test:
			print("This TEST is running with the following arguments:")
			for k in args.__dict__:
				arg = args.__dict__[k]
				if arg: print(" -"+k+" = "+str(arg))
			print(" -n = "+self.filename)
			print("")

		#list all the automatic reactions available from the files in the fdbase folder and exit
		if args.listAutomatics :
			os.path.isdir(self.fdbase)
			if not file_exists(self.fdbase):
				print("ERROR: database directory "+self.fdbase+" not found!")
				sys.exit()
			file_list = [f for f in listdir(self.fdbase) if isfile(join(self.fdbase,f))]
			for fname in file_list:
				fname = self.fdbase+fname
				print("retriving reactions in "+fname)
				fhauto = open(fname,"r")
				icounta = 0
				reasa = prodsa = typea = ""
				for row in fhauto:
					srow = row.strip()
					if "@type:" in srow:
						reasa = prodsa = typea = ""
					if reasa != "" and prodsa != "" and typea != "":
						icounta += 1
						print(str(icounta)+". ("+typea+") "+reasa+" -> "+prodsa)
					if "@reacts:" in srow:
						reasa = " + ".join([x.strip() for x in srow.replace("@reacts:","").split(",")])
					if"@prods:" in srow:
						prodsa = " + ".join([x.strip() for x in srow.replace("@prods:","").split(",")])
					if "@type:" in srow:
						typea = srow.replace("@type:", "").strip()
				print("")
			sys.exit()

		#list all the reactions availbale from the SWRI database files
		if args.listSWRI:
			swriPath = "data/database/swri_xsecs/"
			if not file_exists(swriPath):
				print("ERROR: database directory "+swriPath+" not found!")
				sys.exit()
			print("List of the reactions present in the SWRI datafiles (-listSWRI option):")
			file_list = [f for f in listdir(swriPath) if isfile(join(swriPath,f))]
			for fname in file_list:
				if "~" in fname: continue
				swriR = fname.replace(".dat","")
				fswri = open(swriPath+fname,"r")
				for row in fswri:
					srow = row.strip()
					if srow == "": continue
					arow = [x for x in srow.split(" ") if x!=""]
					if arow[0] == "Lambda": storeLambda = arow
				print("in "+fname)
				for branch in storeLambda[2:]:
					print(" "+swriR+" -> "
						  + " + ".join([x for x in branch.replace("+","+/E/").split("/") if x!=""]))

			sys.exit()

		#get a citation and exit
		if args.quote:
			print("KROME is a quote random generator with some utility for astrochemistry.")
			print("As requested a random citation:")
			get_quote()
			sys.exit()

		#get the list of the quotes and exit
		if args.quotelist:
			print("KROME is a quote random generator with some utility for astrochemistry.")
			print("As requested the complete list of the available citations:")
			get_quote(True)
			sys.exit()

		#save options into a file
		fopt = open("options.log","w")
		for k, v in vars(args).items():
			#if option is set add to the namespace
			if v:
				if v is True: v="" #if is exactly True write key only
				fopt.write("-"+k+" "+v+"\n") #write to file

		#you can select only one -forceMF
		if args.forceMF222 and args.forceMF21:
			die("ERROR: options -forceMF222 and -forceMF21 are mutually exclusive: choose one.")

		#get filename
		if not self.is_test and args.n: self.filename = args.n
		if not self.is_test and args.network: self.filename = args.network
		#chech if reactions file exists
		if args.n or self.is_test:
			if not os.path.isfile(self.filename):
				die("ERROR: Reaction file \""+self.filename+"\" doesn't exist!")
		else:
			die("ERROR: you must define -n FILENAME or -network FILENAME, "
				"where FILENAME is the reaction file!")

		#read the coolFile
		if args.coolFile:
			self.coolFile = args.coolFile.split(",")
			print("Reading option -coolFile (filename="+str(",".join(self.coolFile))+")")

		#read compiler name
		if args.compiler:
			self.compiler = args.compiler.strip()
			print("Reading option -compiler (COMPILER="+self.compiler+")")
			sys.exit("ERROR: option -compiler is deprecated; see wiki. "
					 "Remove this from your command line.")

		#use f90 solver
		if args.useDvodeF90:
			self.useDvodeF90 = True
			self.solver_MF = 227
			print("Reading option -useDvodeF90")

		#set implicit RHS
		if args.iRHS:
			self.use_implicit_RHS = True
			self.solver_MF = 222
			if self.useDvodeF90:
				self.solver_MF = 227
			print("Reading option -iRHS")
		#force MF=21
		if args.forceMF21:
			self.solver_MF = 21
			if self.useDvodeF90:
				self.solver_MF = 27
			print("Reading option -forceMF21")
		#force MF=222
		if args.forceMF222:
			self.solver_MF = 222
			if self.useDvodeF90:
				self.solver_MF = 227
			print("Reading option -forceMF222")

		#method for column density calculation
		if args.columnDensityMethod:
			allMethods = ["JEANS","JEANS40"]
			if args.columnDensityMethod not in allMethods:
				sys.exit("ERROR: method for -columnDensityMethod must be one of "
						 +(",".join(allMethods)))
			self.columnDensityMethod = args.columnDensityMethod

		#use Semenov framework 
		if args.useSemenov:
			self.useSemenov = True
			print("Reading option -useSemenov")


		#use rate tables
		if args.useTabs:
			self.useTabs = True
			print("Reading option -useTabs")

		#do report
		if args.report:
			self.doReport = True
			print("Reading option -report")
		#check mass conservation
		if args.checkConserv:
			self.checkConserv = True
			print("Reading option -checkConserv")
		#use reaction indexes in reaction file
		if args.useFileIdx:
			self.useFileIdx = True
			print("Reading option -useFileIdx")
		#write a single compact file krome_all.f90
		if args.compact:
			self.buildCompact = True
			print("Reading option -compact")
		#perform a clean build
		if args.clean:
			self.cleanBuild = True
			print("Reading option -clean")

		#perform a clean build
		if args.useAutoNetwork:
			self.useCustom = True
			print("Reading option -useAutoNetwork")

		#build isotopes automatically
		if args.usePlainIsotopes:
			self.usePlainIsotopes = True
			print("Reading option -usePlainIsotopes")
			#replace square brackets
			copydic = dict()
			for k, v in self.mass_dic.items():
				copydic[k.replace("[","").replace("]","")] = v
			self.mass_dic = copydic
			self.atoms = [x.replace("[","").replace("]","") for x in self.atoms]

		#compute electrons by balancing the charge
		if args.computeElectrons:
			self.useComputeElectrons = True
			print("Reading option -computeElectrons")

		#use photoionization from Verner et al. 1996 (no longer working)
		if args.usePhIoniz:
			self.usePhIoniz = True
			print("Reading option -usePhIoniz (now obsolete, you can remove it)")

		#use photoionization
		if args.usePhotoOpacity:
			self.usePhotoOpacity = True
			print("Reading option -usePhotoOpacity (now obsolete, you can remove it)")

		#use a global cooling floor
		if args.useCoolFloor:
			self.useCoolFloor = True
			if not args.cooling:
				print("ERROR: option -useCoolFloor needs at least one active cooling option. "
					  "See -cooling=")
				sys.exit()
			print("Reading option -useCoolFloor")

		#use broadening
		if args.useBroadening:
			self.useBroadening = True
			print("Reading option -useBroadening")

		#apply an individual cooling floor (SB, mod TG)
		if args.useIndividualFloor:
			myFloor = [x.strip() for x in args.useIndividualFloor.split(",")]
			allFloor = ["H2","Z_CIE","Z","ATOMIC","HD","CHEM","CO","Z_CIENOUV","Z_EXTENDED","GH"]
			for floor in myFloor:
				if floor not in allFloor:
					die("ERROR: Floor \""+floor+"\" is unknown!\nAvailable floor are: "
						+(", ".join(allFloor)))
			if self.useCoolFloor:
				die("ERROR: useCoolFloor and useIndividualFloor are mutually exclusive!")
			self.individualCoolingFloors = myFloor
			print("Reading option -useIndividualFloor ("+(",".join(myFloor))+")")

		#use photo-induced cooling transitions
		if args.usePhotoInduced:
			self.usePhotoInduced = True
			if not args.photoBins:
				print("ERROR: -usePhotoInduced requires the option -photoBins=N enabled")
				print(" where N is the number of photon bins employed.")
				sys.exit()
			print("Reading option -usePhotoInduced")

		#use equilibrium check to break loops earlier
		if args.useEquilibrium:
			self.useEquilibrium = True
			print("Reading option -useEquilibrium")

		#do not use temperature limits
		if args.noTlimits:
			self.useTlimits = False
			print("Reading option -noTlimits")

		#set the filename of the file with reaction names
		if args.verbatimFilename:
			self.verbatimFilename = args.verbatimFilename.strip()
			if len(self.verbatimFilename) > 255:
				print("ERROR: the path specified in -verbatimFilename must not exceed 255 characters.")
				sys.exit()
			print("Name of the file with reaction names: "+self.verbatimFilename)

		#do not read the file with reaction names
		if args.noVerbatimFile:
			self.useVerbatimFile = False
			print("Reading option -noVerbatimFile")

		#skip duplicated reactions
		if args.skipDup:
			self.skipDup = True
			print("Reading option -skipDup")

		#skip duplicated reactions
		if args.pedantic:
			self.pedanticMakefile = True
			print("Reading option -pedantic")
			sys.exit("ERROR: option -pedantic is deprecated; see wiki. Remove this from your command line.")

		#use reverse kinetics
		if args.reverse:
			self.useReverse = True
			print("Reading option -reverse")

		#use H2 on dust, constant rate by Jura
		if args.useDustH2const:
			self.useDustH2const = True
			print("Reading option -useDustH2const")

		#use H2opacity following
		if args.H2opacity:
			opacities = ["RIPAMONTI", "OMUKAI"]
			if args.H2opacity not in opacities:
				print("ERROR: H2opacity must be one of the following "+(", ".join(opacities))+".")
				sys.exit()
			self.H2opacity = args.H2opacity.strip()
			print("Reading option -H2opacity="+self.H2opacity)

		#determine H2shielding types
		if args.shielding:
			myShielding = [x.strip() for x in args.shielding.split(",")]
			#list of the shielding approximations
			allShielding = ["DB96","WG11","R14"]
			for shi in myShielding:
				if shi not in allShielding:
					die("ERROR: Shielding \""+shi+"\" is unknown!\nAvailable shielding are: "
						+(", ".join(allShielding)))
			if len(myShielding) > 1:
				die("ERROR: "+(", ".join(allShielding))+" are mutually exclusive!")
			self.useShieldingDB96 = ("DB96" in myShielding)
			self.useShieldingWG11 = ("WG11" in myShielding)
			self.useShieldingR14  = ("R14" in myShielding)
			self.useShielding = True
			print("Reading option -shielding (TYPE="+(",".join(myShielding))+")")

		#use dust shielding for Habing flux
		if args.shieldHabingDust:
			self.shieldHabingDust = True
			print("Reading option -shieldHabingDust")

		#use cooling dT/dt in the ODE fex
		if args.skipODEthermo:
			self.useODEthermo = False
			print("Reading option -skipODEthermo")

		#use species mass conservation (and charge)
		if args.conserve:
			self.useConserve = True
			self.useConserveE = True
			print("Reading option -conserve")

		#use species mass conservation (and charge)
		if args.conserveLin:
			self.useConserveLin = True
			self.useConserveE = True
			self.needLAPACK = True
			print("Reading option -conserveLin")

		#use species charge conservation only
		if args.conserveE:
			self.useConserveE = True
			print("Reading option -conserveE")

		#same index for equivalent reactions with different Tlimits
		if args.mergeTlimits:
			self.mergeTlimits = True
			print("Reading option -mergeTlimits")

		#use short header for f90 files
		if args.sh:
			self.shortHead = True
			print("Reading option -sh")

		#use short header for f90 files
		if args.lh:
			self.shortHead = False
			print("Reading option -lh")

		#enable thermochemistry checking
		if args.checkThermochem:
			self.checkThermochem = True
			print("Reading option -checkThermochem")

		#use IERR interface for krome
		if args.useIERR or args.ierr:
			self.useIERR = True
			print("Reading option -useIERR")

		#check if reverse reactions are present in the network
		if args.checkReverse:
			self.checkReverse = True
			print("Reading option -checkReverse")

		#do not write anything to the build directory
		if args.dry:
			self.isdry = True
			print("Reading option -dry")

		#skip reaction mass / charge check
		if (args.nomassCheck and args.nochargeCheck) or args.noCheck:
			print("Reading option -nochargeCheck")
			print("Reading option -nomassCheck")
			self.checkMode = "NONE"
		elif args.nomassCheck and not args.nochargeCheck:
			print("Reading option -nomassCheck")
			self.checkMode = "CHARGE"
		elif not args.nomassCheck and args.nochargeCheck:
			print("Reading option -nochargeCheck")
			self.checkMode = "MASS"
		elif not args.nomassCheck and not args.nochargeCheck:
			self.checkMode = "ALL"
		else:
			print("ERROR: problem with -nomassCheck and/or -nochargeCheck and/or -noCheck")
			sys.exit()

		#skip recombination check
		if args.noRecCheck:
			self.recCheck = False
			print("Reading option -noRecCheck")

		#skip sink check
		if args.noSinkCheck:
			self.sinkCheck = False
			print("Reading option -noSinkCheck")

		#use nuclear multeplicity flux/(1.+delta_ij)
		if args.nuclearMult:
			self.useNuclearMult = True
			print("Reading option -useNuclearMult")

		#include an if in the ODE for the thermal part
		if args.useThermoToggle:
			self.useThermoToggle = True

		#creates ramses patches
		if args.ramses:
			self.doRamses = True
			print("Reading option -ramses")
			if not args.compact:
				print("ERROR: the patch for RAMSES requires the -compact option!")
				sys.exit()
			if self.is_test:
				print("ERROR: -test option and -ramses are incompatible!")
				sys.exit()
			if args.useX:
				print("ERROR: the patch for RAMSES requires number densities, "
					  "please remove -useX option!")
				sys.exit()
			if args.heating:
				if "COMPR" in args.heating:
					print("ERROR: -heating=COMPRESS is intended only for one-zone "
						  "gravitational collapse!")
					sys.exit()

			if not args.customATOL and not args.ATOL:
				print("WARNING: default ATOL set to 1e-10 due to -ramses flag.")
				self.ATOL = 1e-10

		#creates ramsesTH version patches
		if args.ramsesTH :
			self.doRamsesTH = True
			print("Reading option -ramsesTH")
			if not args.compact:
				die("ERROR: the patch for RAMSES TH requires the -compact option!")
			if self.is_test:
				die("ERROR: -test option and -ramsesTH are incompatible!")
			if args.useX:
				die("ERROR: the patch for RAMSES TH requires number densities, please remove "
					"-useX option!")
			if args.heating:
				if "COMPR" in args.heating:
					die("ERROR: -heating=COMPRESS is intended only for one-zone gravitational "
						"collapse! Remove it")

			if not args.customATOL and not args.ATOL:
				print("WARNING: default ATOL set to 1e-10 due to -ramsesTH flag.")
				self.ATOL = 1e-10

		#creates flash patches
		if args.flash:
			self.doFlash = True
			print("Reading option -flash")
			if not args.compact:
				print("ERROR: the patch for FLASH requires the -compact option!")
				sys.exit()
			if self.is_test:
				print("ERROR: -test option and -flash are incompatible!")
				sys.exit()
			if args.gamma:
				typeGamma = args.gamma
				self.typeGamma = typeGamma.replace("\"","")
				if self.typeGamma != "FULL" and self.typeGamma != "DEFAULT" :
					print("ERROR: for consistency reasons so far only -gamma FULL or DEFAULT is "
						  "allowed for FLASH!")
					print("Updates follow soon, please contact D. Seifried for more details")
					sys.exit()
			if args.useX:
				print("ERROR: the patch for FLASH requires number densities, please remove "
					  "-useX option!")
				sys.exit()
			if args.heating:
				if "COMPR" in args.heating:
					print("ERROR: -heating=COMPRESS is intended only for one-zone gravitational "
						  "collapse!")
					sys.exit()

			if not args.customATOL and not args.ATOL:
				print("WARNING: default ATOL set to 1e-10 due to -flash flag.")
				self.ATOL = 1e-10


		#creates enzo patches
		if args.enzo:
			self.doEnzo = True
			print("Reading option -enzo")
			if not args.compact:
				print("ERROR: the patch for ENZO requires the -compact option!")
				sys.exit()
			if self.is_test:
				print("ERROR: -test option and -enzo are incompatible!")
				sys.exit()
			if args.useX:
				print("ERROR: the patch for ENZO requires number densities, please remove "
					  "-useX option")
				sys.exit()
			if args.heating:
				if "COMPR" in args.heating:
					print("ERROR: -heating=COMPRESS is intended only for one-zone "
						  "gravitational collapse!")
					sys.exit()
			if not args.customATOL and not args.ATOL:
				print("WARNING: default ATOL set to 1e-10 due to -enzo flag.")
				self.ATOL = 1e-10

		#creates gizmo patches
		if args.gizmo:
			self.doGizmo = True
			print("Reading option -gizmo")
			if not args.compact:
				print("ERROR: the patch for Gizmo requires the -compact option!")
				sys.exit()
			if self.is_test:
				print("ERROR: -test option and -gizmo are incompatible!")
				sys.exit()
			if not args.useX:
				print("ERROR: the patch for Gizmo requires mass fractions, please add -useX option")
				sys.exit()
			if not args.interfaceC:
				print("ERROR: the patch for Gizmo requires the C interface, please add "
					  "-interfaceC option")
				sys.exit()
			if args.heating:
				if "COMPR" in args.heating:
					print("ERROR: -heating=COMPRESS is intended only for one-zone "
						  "gravitational collapse!")
					sys.exit()
			if not args.customATOL and not args.ATOL:
				print("WARNING: default ATOL set to 1e-10 due to -enzo flag.")
				self.ATOL = 1e-10

		#creates C and Python wrappers
		if args.interfaceC or args.interfacePy :
			self.KindInteger = "integer(kind=c_int)"
			self.KindIntegerValue = "integer(kind=c_int), value"
			self.KindBoolValueOptional = "logical(kind=c_bool), optional"
			self.KindSingle = "real(kind=c_float)"
			self.KindDouble = "real(kind=c_double)"
			self.KindDoubleValue = "real(kind=c_double), value"
			self.KindDoubleValueOptional = "real(kind=c_double), value"
			self.KindCharacter = "character(kind=c_char)"
			self.BindC = "bind(C)"
			if  args.interfaceC:
				print("Reading option -interfaceC")
				self.interfaceC = True
			if args.interfacePy:
				print("Reading option -interfacePy")
				self.interfacePy = True

		#skip writing Jacobian in krome_ode.f90, allows faster compilation
		if args.skipJacobian:
			self.doJacobian = False
			print("Reading option -skipJacobian")

		#skip checking for objects in build/
		if args.unsafe:
			self.safe = False
			print("Reading option -unsafe")

		#enable stellar physics
		if args.stars:
			self.useStars = True
			print("Reading option -stars")

		#do not write test.f90 and Makefile
		if args.noExample:
			self.noExample = True
			print("Reading option -noExample")

		#set the number of photobins
		if args.photoBins:
			self.photoBins = int(args.photoBins)
			self.usePhIoniz = True
			if self.photoBins<0:
				die("ERRROR: number of frequency bins < 0!")
			print("Reading option -photoBins (NBINS="+str(self.photoBins)+")")

		#kernel for xsec interpolation
		if args.xsecKernelFunction:
			self.xsecKernelFunction = args.xsecKernelFunction.strip()
			print("Reading option -xsecKernelFunction ("+self.xsecKernelFunction+")")

		#determine Tgas limit operators
		if args.Tlimit:
			self.myTlimit = args.Tlimit
			self.myTlimit = self.myTlimit.replace("[","").replace("]","").split(",")
			self.TlimitOpHigh = self.myTlimit[1].strip().upper()
			self.TlimitOpLow = self.myTlimit[0].strip().upper()
			allOps = ["LE","LT","GE","GT"]
			if self.TlimitOpLow not in allOps or self.TlimitOpHigh not in allOps:
				die("ERROR: on -Tlimit operators must be one of the followings: "
					+ (", ".join(allOps)))
			print("Reading option -Tlimit (Low="+self.TlimitOpLow+", High="+self.TlimitOpHigh+")")

		#determine cooling types
		if args.cooling:
			myCools = args.cooling.split(",")
			myCools = [x.strip() for x in myCools]
			#list of all cooling (excluded from file)
			allCools = ["ATOMIC","H2","HD","DH","DUST","FF","H2GP98","COMPTON","EXPANSION","CIE",
						"CONT","CHEM","DISS","Z","CO","Z_CIE","Z_CIENOUV","Z_EXTENDED","GH","OH",
						"H2O", "HCN"]
			fileCools = [] #list of the cooling read from file
			#load additional coolings from file
			for fname in self.coolFile:
				partialFileCools = [] #for output purposes
				if not file_exists(fname):
					print("ERROR: file "+fname+" not found!")
					sys.exit()
				fh = open(fname, "r")
				inComment = False
				for row in fh:
					srow = row.strip()
					#skip cooments
					if srow == "": continue
					if srow[0] == "#": continue
					if srow[:1] == "//": continue
					if srow[:1] == "/*": inComment = True
					if "*/" in srow:
						inComment = False
						continue
					srow = srow.split("#")[0]
					if inComment: continue
					#look for the metal name
					if "metal:" in srow:
						metal_name = srow.split(":")[1].strip()
						mol = parser(metal_name,self.mass_dic,self.atoms,self.thermodata)
						mname = mol.coolname
						if mname in allCools:
							print("ERROR: conflict name for "+mname+", which is already present!")
							sys.exit()
						partialFileCools.append({"flag":mname,"name":metal_name})
						fileCools.append({"flag":mname,"name":metal_name}) #append to the list fo the available coolings
						allCools.append(mname) #append flag to the list of the coolants
				#write found coolants
				joinedCool = (", ".join([x["flag"] for x in partialFileCools]))
				if len(partialFileCools) > 0:
					print("Cooling "+joinedCool+" available from "+fname)
					allCools.append("FILE")

			if "?" in myCools:
				print("Available coolings are:", (", ".join(sorted(allCools))))
				sys.exit()

			#check coolant names
			for coo in myCools:
				if coo not in allCools:
					die("ERROR: Cooling \""+coo+"\" is unknown!\nAvailable coolings are: "
						+(", ".join(allCools)))

			if "ATOMIC" in myCools: self.useCoolingAtomic = True
			if "H2" in myCools: self.useCoolingH2 = True
			if "H2GP98" in myCools: self.useCoolingH2GP98 = True
			if "HD" in myCools: self.useCoolingHD = True
			if "DH" in myCools: self.useCoolingdH = True
			if "DUST" in myCools: self.useCoolingDust = True
			if "COMPTON" in myCools: self.useCoolingCompton = True
			if "EXPANSION" in myCools: self.useCoolingExpansion = True
			if "CHEM" in myCools: self.useCoolingChem = True
			if "CIE" in myCools: self.useCoolingCIE = True
			if "FF" in myCools: self.useCoolingFF = True
			if "DISS" in myCools: self.useCoolingDISS = True
			if "CONT" in myCools: self.useCoolingCont = True
			if "Z" in myCools: self.useCoolingZ = True
			if "CO" in myCools: self.useCoolingCO = True
			if "OH" in myCools: self.useCoolingOH = True
			if "H2O" in myCools: self.useCoolingH2O = True
			if "HCN" in myCools: self.useCoolingHCN = True
			if "Z_CIE" in myCools: self.useCoolingZCIE = True
			if "Z_CIENOUV" in myCools: self.useCoolingZCIENOUV = True
			if "Z_EXTENDED" in myCools:
				self.useCoolingZExtended = self.useCoolingZ = self.useCoolingZCIE = True
			if "GH" in myCools: self.useCoolingGH = True

			#loop over metals loaded from file and search for them in the cooling flags provided by the user
			for met in fileCools:
				if met["flag"] in myCools or "FILE" in myCools:
					#print "Option "+met["flag"]+" will load data from "+fname
					self.useCoolingZ = True
					self.Zcools.append(met["name"]) #append metal name to the list of the requested species
					self.zcoolants.append(met["flag"]) #append cooling name to the list of the coolants
					#add also neutral in case of ions
					if "+" in met["name"]:
						neutral_name = met["name"].replace("+", "")
						if neutral_name not in self.Zcools: self.Zcools.append(neutral_name)
					#add also neutral in case of anions
					if "-" in met["name"]:
						neutral_name = met["name"].replace("-", "")
						if neutral_name not in self.Zcools: self.Zcools.append(neutral_name)

			self.allCoolings = myCools
			if len(self.Zcools)>0:
				self.allCoolings += ["Z"]

			self.use_cooling = True
			self.hasDust = False
			for aa in argv:
				if "dust=" in aa: self.hasDust = True
			if self.useCoolingDust and not self.hasDust:
				die("ERROR: to include dust cooling you need dust (use -dust=[see help]).")
			if "CIE" in myCools and "CONT" in myCools:
				die("ERROR: CIE and CONT cooling are mutually exclusive!")
			if "CIE" in myCools and "GH" in myCools:
				die("ERROR: CIE and GH cooling are mutually exclusive!")

			self.use_thermo = True

			print("Reading option -cooling ("+(",".join(myCools))+")")

		if args.coolLevels:
			self.coolLevels = [int(x) for x in range(int(args.coolLevels)+1)]
			if int(args.coolLevels)<1:
				die("ERROR: coolLevels must be at least 1 (two levels)!")
			print("Reading option -coolLevels ("+str(self.coolLevels[0])
				  + " to "+str(self.coolLevels[-1])+")")

		#cooling quenching
		if args.coolingQuench:
			self.coolingQuench = format_double(args.coolingQuench)
			if self.coolingQuench < 0e0:
				die("ERROR: Tcrit for coolingQuench should be greater than zero!")
			print("Reading option -coolingQuench ("+str(self.coolingQuench)+")")

		#determine heating types
		if args.heating:
			myHeat = args.heating.upper().split(",")
			myHeat = [x.strip() for x in myHeat]
			self.allHeatings = myHeat
			allHeats = ["COMPRESS","PHOTO","CHEM","DH","CR","PHOTOAV","PHOTODUST",
						"PHOTODUSTNET","XRAY","VISCOUS"]
			for hea in myHeat:
				if hea not in allHeats:
					die("ERROR: Heating \""+hea+"\" is unknown!\nAvailable heatings are: "
						+(", ".join(allHeats)))

			if "COMPRESS" in myHeat: self.useHeatingCompress = True #compressional heating
			if "PHOTO" in myHeat: self.useHeatingPhoto = True #photo heating with photobins
			if "CHEM" in myHeat: self.useHeatingChem = True #chemical heating
			if "DH" in myHeat: self.useHeatingdH = True #enthalpic heating (experimental)
			if "CR" in myHeat: self.useHeatingCR = True #cosmic ray heating
			if "PHOTOAV" in myHeat: self.useHeatingPhotoAv = True #H2 photodissociation and photo-pumping
			if "PHOTODUST" in myHeat: self.useHeatingPhotoDust = True #photoelectric heating from dust
			if "PHOTODUSTNET" in myHeat: self.useHeatingPhotoDustNet = True #photoelectric heating from dust with recombination cooling
			if "XRAY" in myHeat: self.useHeatingXRay = True #heating from xray reactions rate
			if "VISCOUS" in myHeat: self.useHeatingVisc = True #heating from viscosity
			#if("H2PUMPING" in myHeat): self.useHeatingPumpH2 = True #heating from photodissociation of H2 in LW bands

			self.use_thermo = True
			if self.photoBins<=0 and self.useHeatingPhoto:
				print("ERROR: if you use photoheating you should include the number of photo-bins")
				print(" by using the option -photoBins=NBINS")
				sys.exit()

			if self.useHeatingPhotoDust and self.useHeatingPhotoDustNet:
				print("ERROR: PHOTODUST and PHOTODUSTNET options are mutually exclusive!")
				sys.exit()

			if self.photoBins<=0 and self.useHeatingPhotoDustNet:
				print("ERROR: PHOTODUSTNET option requires -photoBins=NBINS to set")
				print(" the number of photobins!")
				sys.exit()

			if "?" in myHeat:
				print("Available heatings are:", (", ".join(allHeats)))
				sys.exit()

			print("Reading option -heating ("+(",".join(myHeat))+")")

		#set variable name for Av attenuation in photoelectric effect
		if args.photoDustVarAv:
			self.photoDustVarAv = args.photoDustVarAv.strip()
			if not self.useHeatingPhotoDust:
				sys.exit("ERROR: -photoDustVarAv should be used with -heating=PHOTODUST")
			print("Reading option -photoDustVarAv (variable name: "+self.photoDustVarAv+")")

		if args.photoDustVarG0:
			self.photoDustVarG0 = args.photoDustVarG0.strip()
			if not self.useHeatingPhotoDust:
				sys.exit("ERROR: -photoDustVarG0 should be used with -heating=PHOTODUST")
			print("Reading option -photoDustVarG0 (variable name: "+self.photoDustVarG0+")")

		#use number densities instead of mass fractions (default, retrocompatibility)
		if args.useN:
			self.usex = False
			print("Reading option -useN")

		#use mass fractions instead of number densities
		if args.useX:
			self.useX = True
			print("Reading option -useX")

		#force rwork size
		if args.forceRWORK:
			self.myrwork = args.forceRWORK
			self.force_rwork = True
			print("Reading option -forceRWORK (RWORK="+str(self.myrwork)+")")

		#use custom function for coefficient instead of coe_tab()
		if args.useCustomCoe:
			self.customCoeFunction = args.useCustomCoe.replace("\"","")
			self.useCustomCoe = True
			print("Reading option -useCustomCoe (Expression="+str(self.customCoeFunction)+")")

		#use function to append after each ODE
		if args.useODEConstant:
			self.ODEConstant = args.useODEConstant
			self.useODEConstant = True
			print("Reading option -useODEConstant (Constant="+str(self.ODEConstant)+")")

		#dust
		hasDustOptions = hasDustTabs = False
		if args.dustOptions: hasDustOptions = True
		if args.dustTabs: hasDustTabs = True
		if args.dust:
			dustopt = args.dust
			adust = dustopt.split(",")
			self.useDust = True
			if len(adust) < 2:
				die("ERROR: you must specify dust size and type(s), e.g. -dust=20,C,Si")
			if self.use_implicit_RHS:
				die("ERROR: you cannot use dust AND implicit RHS: remove -iRHS option")
			self.dustArraySize = int(adust[0])
			self.dustTypes = adust[1:]
			self.dustTypesSize = len(self.dustTypes)
			print("Reading option -dust (size="+str(self.dustArraySize)+", type(s)="
				  +(",".join(self.dustTypes))+")")
			#if(not(hasDustOptions) and not(hasDustTabs)):
			#	print "ERROR: -dust flag needs to define -dustOptions=[see help] or -dustTabs=[see help])"
			#	sys.exit()

		#dust options
		dustOptions = []
		if args.dustOptions:
			allOptions = ["H2","GROWTH","SPUTTER","T","EVAP","dT"]
			if not self.useDust: die("ERROR: you need -dust=[see help] to activate -dustOptions!")
			dustopt = args.dustOptions
			dustOptions = [x.strip() for x in dustopt.split(",")]
			for opt in dustOptions:
				if opt not in allOptions: sys.exit("ERROR: option "+opt+" in -dustOptions unknown!")
			if "GROWTH" in dustOptions: self.useDustGrowth = True
			if "SPUTTER" in dustOptions: self.useDustSputter = True
			if "H2" in dustOptions: self.useDustH2 = True
			if "T" in dustOptions: self.useDustT = True
			if "EVAP" in dustOptions: self.useDustEvap = True
			if "dT" in dustOptions: self.usedTdust = True
			if self.useDustT and self.usedTdust:
				sys.exit("ERROR: options T and dT for dust are mutually exclusive!")
			print("Reading option -dustOptions (options="+(",".join(dustOptions))+")")

		#dust tabs
		if args.dustTabs:
			tabPath = "data/dust_tables/"
			tabModes = [x for x in os.listdir(tabPath) if(x.endswith("_cool.dat"))]
			tabModes = [x.replace("dust_table_","").replace("_cool.dat","") for x in tabModes]
			tabOpts = ["H2","COOL","3D","Photo3D"] #options
			allTabs = tabOpts + tabModes #all possible options

			if self.useDust: die("ERROR: -dustTabs and -dust options are not compatible!")
			dustTabs = [x.strip() for x in args.dustTabs.split(",")]
			modeFound = optFound = False

			#use additional dimension for tables (Av)
			if "3D" in dustTabs:
				self.dustTableDimension = "3D"

			if "Photo3D" in dustTabs:
				self.usePhotoDust_3D = True

			for dTab in dustTabs:
				if dTab not in allTabs:
					print("ERROR: option (or mode) "+dTab+" in -dustTabs unknown!")
					print("Available options:", tabOpts)
					print("Available modes:",tabModes)
					sys.exit()
				if dTab in tabOpts: optFound = True
				if dTab in tabModes:
					modeFound = True
					self.dustTableMode = dTab

			#error if mode not found
			if not modeFound:
				print("ERROR: you should indicate a mode when you use -dustTabs option")
				print("Available modes:", sorted(tabModes))
				sys.exit()

			#error if option not found
			if not optFound:
				print("ERROR: you should indicate options when you use -dustTabs option")
				print("Available options:", tabOpts)
				sys.exit()

			if "H2" in dustTabs: self.dustTabsH2 = True
			if "COOL" in dustTabs: self.dustTabsCool = True
			if "Av" in dustTabs: self.dustTabsAvVariable = True
			self.useDustTabs = True
			print("Reading option -dustTabs (options="+(",".join(dustTabs))+")")

		#dust seed value
		if args.dustSeed:
			if not self.useDust: die("ERROR: you need -dust=[see help] to activate dust seed!")
			self.dustSeed = args.dustSeed.strip().replace("\"","")
			print("Reading option -dustSeed (seed="+self.dustSeed+")")

		#project name folder
		if args.source:
			flist = ["krome_commons.f90", "krome_cooling.f90", "krome.f90", "krome_heating.f90"]
			flist += ["krome_photo.f90", "krome_grfuncs.f90", "krome_subs.f90", "krome_user_commons.f90", "krome_constants.f90"]
			flist += ["krome_dust.f90", "kromeF90.f90", "krome_ode.f90", "krome_reduction.f90"]
			flist += ["krome_tabs.f90", "krome_user.f90", "krome_gadiab.f90", "krome_phfuncs.f90", "krome_getphys.f90"]

			src = str(args.source)
			print("Reading option -source (name="+src+")")

			#check if folder exists
			if not os.path.exists(src):
				print("ERROR: the folder "+src+"/ doesn't exist!")
				sys.exit()
			#check if the file in flist are present in the folder
			notfound = []
			for fle in flist:
				if not file_exists(src+"/"+fle):
					notfound.append(fle)

			#if file missing write the error message
			if len(notfound) > 0:
				print("ERROR: you suggested to use the folder "+src+"/ as source")
				print(" but the following file(s) missing:")
				print(" " + (", ".join(notfound)))
				sys.exit()

			self.srcFolder = src + "/"

		# typeGamma
		if args.gamma:
			typeGamma = args.gamma
			self.typeGamma = typeGamma.replace("\"","")
			if not args.heating and not args.cooling:
				print("ERROR: you are trying to use -gamma without -cooling or -heating")
				sys.exit()
			print("Reading option -gamma (gamma="+str(self.typeGamma)+")")

		#offset for ramses
		if args.ramsesOffset:
			if not args.ramses:
				die("ERROR: if you use -ramsesOffset you must also add -ramses option!")
			self.ramses_offset = args.ramsesOffset
			print("Reading option -ramsesOffset (offset="+str(args.ramsesOffset)+")")

		#ATOL
		if args.ATOL:
			self.ATOL = args.ATOL
			print("Reading option -atol (atol="+str(self.ATOL)+")")

		#RTOL
		if args.RTOL:
			self.RTOL = args.RTOL
			print("Reading option -rtol (rtol="+str(self.RTOL)+")")

		#maxord
		if args.maxord:
			self.maxord = min(max(1,int(args.maxord)),5)
			print("Reading option -maxord (maxord="+str(self.maxord)+")")

		#reducer
		if args.reducer:
			self.reducer = True
			print("Reading option -reducer")

		#fex argument option
		if args.fexArgument:
			self.useFexCustom = True
			print("Reading option -fexArgument")

		#custom ATOLs
		if args.customATOL:
			fname = args.customATOL
			print("Reading option -customATOL (file="+fname+")")
			if not file_exists(fname):
				print("ERROR: custom ATOL file \""+fname+"\" does not exist!")
				sys.exit()
			fh = open(fname,"r")
			for row in fh:
				srow = row.strip()
				if len(srow) == 0: continue
				if srow[0] == "#": continue
				arow = [x for x in srow.split(" ") if x.strip()!=""]
				if len(arow)<2:
					print("ERROR: wrong format in custom ATOL file!")
					print(srow)
					sys.exit()
				print("ATOL: " + arow[0] + " " + arow[1])
				self.atols.append([arow[0], arow[1]])
			fh.close()

		#custom RTOLs
		if args.customRTOL:
			fname = args.customRTOL
			print("Reading option -customRTOL (file=" + fname + ")")
			if not file_exists(fname):
				print("ERROR: custom RTOL file \""+fname+"\" does not exist!")
				sys.exit()
			fh = open(fname, "r")
			for row in fh:
				srow = row.strip()
				if len(srow) == 0: continue
				if srow[0] == "#": continue
				arow = [x for x in srow.split(" ") if x.strip()!=""]
				if len(arow) < 2:
					print("ERROR: wrong format in custom RTOL file!")
					print(srow)
					sys.exit()
				print("RTOL: " + arow[0] + " " + arow[1])
				self.rtols.append([arow[0],arow[1]])
			fh.close()

		#custom ODEs
		if args.customODE:
			fname = args.customODE
			print("Reading option -customODE (file=" + fname + ")")
			if not file_exists(fname):
				print("ERROR: custom ODE file \""+fname+"\" does not exist!")
				sys.exit()
			fh = open(fname, "r")
			ivarcoe = 0
			for row in fh:
				srow = row.strip()
				if len(srow) == 0: continue
				if srow[0] == "#": continue
				#search for variables
				if "@var:" in srow:
					arow = srow.replace("@var:", "").split("=")
					if len(arow) != 2:
						print("ERROR: variable line must be @var:variable=F90_expression")
						print("found: " + srow)
						sys.exit()
					#check if the current @var is allowed
					notAllowedVars = ["k","tgas","energy_ev"]
					for nav in notAllowedVars:
						if nav.lower() == arow[0].lower():
							sys.exit("ERROR: you can't use "+nav+" as an @var variable")
					print("var: " + arow[0])
					self.coevarsODE[arow[0]] = [ivarcoe, arow[1]]
					ivarcoe += 1 #count variables to sort
					continue #SKIP: a variable line is not a reaction line
				#search for ODE
				arow = [x.strip() for x in srow.split("=")]
				if len(arow) != 2:
					print("ERROR: wrong format in custom ODE file!")
					print(srow)
					sys.exit()
				print("ODE: " + arow[0] + " " + arow[1])
				self.customODEs.append([arow[0],arow[1]])
			fh.close()

	###################################################
	def safe_check(self):
		if not self.safe or not os.path.exists(self.buildFolder): return
		if self.isdry: return
		wlk = next(os.walk(self.buildFolder))
		wlk = wlk[1] + wlk[2] #folders+files
		if len(wlk) < 1: return
		print("************************************************")
		print("WARNING: the folder "+self.buildFolder+" is not empty")
		print(" some items may be replaced. Do you want to proceed?")
		print("To avoid this message use -unsafe option.")
		print("************************************************")
		a = keyb_input("Any key to ignore q to quit... ")
		if a == "q": sys.exit()

	####################################################
	#load thermochemistry data from chemkin-formatted file
	def load_thermochemistry(self):
		nskip = 99999 #skip comments
		thermo = dict() #prepare dictionary
		fth = open("data/thermo30.dat") #open thermochemistry file
		#loop on file
		for row in fth:
			srow = row.strip()
			if len(srow) == 0: continue #skip empty lines
			arow = srow.split()
			if arow[0] == "END": break #break on END
			if arow[0] == "THERMO": #start to read data
				nskip = 1 #skip line after thermo
				continue
			#skip comments
			if nskip > 0 or srow[0] == "!":
				nskip -= 1 #reduce line to be skipped by one
				continue
			#if not number is a species
			if not is_number(row[:15]):
				spec = arow[0].strip().upper() #read species name
				Tmin = arow[len(arow)-4]
				Tmed = arow[len(arow)-2]
				Tmax = arow[len(arow)-3]
				mypoly = [Tmin, Tmed, Tmax] #first data are temperature limits
			else:
				coe = [row[i*15:(i+1)*15] for i in range(5)] #read 5 coefficients
				irow = int(row[5*15:].strip()) #read line number
				mypoly += coe #add coefficients to the coefficients list
				#last line for the given species
				if irow == 4:
					#convert coefficients to floating and skip empty values
					coef = [float(x) for x in mypoly[:17] if x.strip()!=""]
					#check the number of coefficients (3temp+14poly)
					if len(coef) != 17:
						print("ERROR: NASA polynomials!")
						print(spec)
						print(srow)
						sys.exit()
					thermo[spec] = dict()
					thermo[spec]["NASA"] = coef #append coefficients to the dictionary
		fth.close()

		with open("data/thermoNIST.dat") as thermofile:
			#loop over file line per line
			for row in thermofile:
				srow = row.strip()
				#skip newlines
				if not srow:
					continue
				#skip comments
				if srow[0] == "#":
					continue
				arow = srow.split(":")
				# convert numbers to floats
				arow = [arow[0]] + [float(x) for x in arow[1:]]
				spec = arow[0].upper()
				# Tmin = arow[1]
				# Tmax = arow[2]
				# no need for the last coefficient = H(T=298K)
				# coe = arow[3:-1]
				mypoly = arow[1:-1]

				#creat new dict for new species
				if spec not in thermo:
					thermo[spec] = dict()
				# if no NIST data for species yet, make new
				if "NIST" not in thermo[spec]:
					thermo[spec]["NIST"] = mypoly
				# else append to same species
				else:
					thermo[spec]["NIST"] += mypoly

		self.thermodata = thermo
		# print "Thermochemistry data loaded!"

	################################################
	def prepare_massdict(self):
		self.use_RHS_variable = False

		self.separator = "," #separator character

		me =  9.10938188e-28 #electron mass (g)
		mp = 1.67262158e-24 #proton mass (g)
		mn = 1.6725e-24 #neutron mass (g)
		menp = me + mp + mn
		#mass dictionary
		mass_dic = {'H':me+mp,
			'D':menp,
			'He':2.* menp,
			'Li':3.*(me+mp)+4.*mn,
			'Be':4.*(me+mp)+5.*mn,
			'B':5.* menp + mn,
			'C':6. * menp,
			'N':7. * menp,
			'O':8. * menp,
			'F':9. * menp + mn,
			'Ne':10. * menp,
			'Mg':12. * menp,
			'Na':(me+mp)*11+mn*12,
			'Al':(me+mp)*13+mn*14,
			'Si':14. * menp,
			'P':15. * menp + mn,
			'S':menp * 16,
			'Cl': menp * 17 + mn,
			'Ar':menp * 18 + 4 * mn,
			'Ti':menp * 22 + 4 * mn,
			'Fe':(me+mp) * 26 + mn * 29,
			'GRAIN0': 100*6*menp,
			'GRAIN-': 100*6*menp+me,
			'GRAIN+': 100*6*menp-me,
			'PAH': 30*6*menp,
			'PAH-': 30*6*menp+me,
			'PAH+': 30*6*menp-me,
			'O(1D)':8.*menp,
			'O(3P)':8.*menp,
			'_dust':0e0,
			'_c_dust':0e0,
			'_grain':0e0,
			'_surface':0e0,
			'_para':0e0,
			'_ortho':0e0,
			'_meta':0e0,
			'_anti':0e0,
			'_ice':0e0,
			'_total':0e0,
			'l_':0e0,
			'c_':0e0,
			'CR':0e0,
			'Q':0e0,
			'M':0e0,
			'g':0e0,
			'E':me,
			'-':me,
			'+':-me}

		#fake species FK1,FK2,...
		for i in range(10):
			mass_dic['FK'+str(i)] = 1.

		#excited levels of some molecules. add here if needed
		for i in range(9):
			mass_dic['CH2_'+str(i+1)] = 6.*menp + 2.*(me+mp)
			mass_dic['SO2_'+str(i+1)] = 16.*menp + 2.*8.*menp

		#build isotopes (including some non-esistent) as [n]A
		# with -usePlainIsotopes build as nA
		atoms_iso = ["H","He","Li","Be","B","C","N","O","F","Ne","Na","Mg","Al","Si","P",
					 "S","Cl","Ar","K","Ca","Ti","Fe","Co","Ni"]
		atoms_p = [i+1 for i in range(20)] + [22,26,27,28]
		if len(atoms_iso) != len(atoms_p):
			die("ERROR: in building isotopes the length of the atoms array and the number of protons array mismatch!")
		for aiso in atoms_iso:
			protons = atoms_p[atoms_iso.index(aiso)] #get proton numbers
			for i in range(protons,80):
				iso_name = "["+str(i)+aiso+"]"
				if self.usePlainIsotopes: iso_name = str(i) + aiso
				mass_dic[iso_name] = protons*(me+mp) + ((i-protons)*mn)

		#prepare mass dictionary
		self.mass_dic = dict([[k.upper(),v] for (k,v) in mass_dic.items()])
		#sort dictionary, longest first. note that even if it is called
		# atoms, this contains also other chemical formula parts, as GRAIN, PAH, and so on...
		self.atoms = sorted(mass_dic, key = lambda x: len(x),reverse=True)


	#################################################
	#read the reaction file
	def read_file(self):
		skipDup = self.skipDup
		filename = self.filename
		atoms = self.atoms
		mass_dic = self.mass_dic
		thermodata = self.thermodata
		spec_names = [] #string
		idx_list = [] #store reaction index in case of -useFileIdx
		pseudo_hash_list = []
		rcount = 0 #count reactions
		found_one = False #flag to control if at least one reaction has been found
		unmatch_idx = False #controls if the reaction index in the file match the sequential index
		#default position in input array line
		iidx = 0 #position of the index
		skipped_dupl = 0 #number of duplicated reaction skipped
		ireact = range(1,4) #positions of the reactants
		iprod = range(4,8) #position of the products
		iTmin = 8 #position of tmin
		iTmax = 9 #position of tmax
		irate = 10 #position of the rate in F90 style
		ivarcoe = 0 #number variable to order dictionary (dictionaries are not ordered by definition!)
		ivarCool = 0 #same as above but for custom cooling variables
		ivarHeat = 0 #same as above but for custom heating variables
		TminAuto = self.TminAuto
		TmaxAuto = self.TmaxAuto
		hasFormat = False
		format_items = 4+len(ireact)+len(iprod)
		if skipDup: fdup = open("duplicates.log","w")
		idxFound = tminFound = tmaxFound = rateFound = True
		qeffFound = False
		group = "__DEFAULT__" #default group for reactions
		specs = [] #list of species as mol objects
		reacts = [] #list of reactions as react objects
		reags = [] #list of reagents for already found
		prods = [] #list of prods for already found
		idxs = [] #list of index for already found
		noTabNextBlock = False #default for blocks of reactions
		inCRblock = False #block of CR reactions
		inPhotoBlock = False #block of photo reactions with xsection function
		inXRayBlock = False #block of xray reactions
		inReactionModifierBlock = False #block of modifier expression for the computed coefficients
		inOdeModifierBlock = False #block of modifier expression for the ODE dn/dT
		noTabBlockStored = noTabNextBlock #store the noTabNextBlock array before inPhotoBlock to restore it
		inCoolingBlock = False #block for custom cooling expression
		inHeatingBlock = False #block for custom heating expression
		inSurfaceBlock = False #block for reaction on surface
		inStoreOnceBlock = False #block that stores reactions that are constants during the solver call
		self.hasSurfaceReactions = False #true if surface reactions found
		self.use_GFE_tables = False #flag for using Gibbs free energy tables
		self.GFE_species = [] #list of species with available Gibbs free energy
		self.clusterablesPresent = [] #list the clusterable species that are present in the network

		#generate a custom reaction network and replace filename with the custom one
		if self.useCustom:
			filename = generateCustom(filename)

		print("Reading from file \""+filename+"\"...")


		#read the size of the file in lines (skip blank and comments)
		# to have a rough idea of the size
		fh = open(filename)
		line_count = 0
		allrows = []
		for row in fh:
			if row.strip() == "": continue
			if row.strip()[0] == "#": continue
			line_count += 1
			allrows.append(row.strip())
		fh.close()

		#warning if the number of lines exceed a certain limit
		if line_count > 1000:
			print("Found "+str(line_count)+" lines! It takes a while...")

		fsh_found = False #search for fsh variable for shielding if needed
		#start reading file stored in the loop above
		isComment = False #flag for comment block
		noTabNext = False #flag for use tabs for the next reaction
		nextSolomon = False #next reaction is Solomon (to store index for H2 pumping)
		nextH2photodissociation = False #next reaction is H2 photodissociation
		for line_number, row in enumerate(allrows):
			srow = row.strip() #stripped row
			if srow.strip() == "": continue #looks for blank line
			if srow[0] == "#": continue #looks for comment line
			if len(srow) > 1:
				if srow[0:2] == "//": continue #looks for comment line
				if srow[0:2] == "/*": isComment = True #start multiline comment

			#end multiline comment
			if "*/" in srow:
				isComment = False
				continue
			if isComment: continue #skip if in comment block

			#search for final expression to modify the coefficients (stop)
			if srow.lower() == "@reactionmodifier_stop" or srow.lower() == "@reactionmodifier_end":
				inReactionModifierBlock = False
				continue #SKIP (not a reaction)

			#search for final expression to modify the ODE (stop)
			if srow.lower() == "@odemodifier_stop" or srow.lower() == "@odemodifier_end":
				inOdeModifierBlock = False
				continue #SKIP (not a reaction)

			#store coefficient modifier
			if inReactionModifierBlock:
				if "@" in srow:
					print(srow)
					sys.exit("ERROR: @ expressions are not allowed inside the @reactionModifier "
							 "block!")
				self.kModifier.append(srow)
				continue #skip: modifier line is not a reaction

			#store ode modifier
			if inOdeModifierBlock:
				if "@" in srow:
					print(srow)
					sys.exit("ERROR: @ expressions are not allowed inside the @ODEModifier block!")
				self.odeModifier.append(srow)
				continue #skip: modifier line is not a reaction

			#search for group indication
			if "@group:" in srow:
				group = srow.replace("group:", "").strip().replace(" ", "_")
				if not group.isalnum(): die("ERROR: group must be alphanumeric. Found " + group)
				print("Found reactions group " + group)
				if group not in self.groups: self.groups.append(group)
				continue

			#search for custom cooling and append to the list
			if "@cooling:" in srow and inCoolingBlock:
				customCool = srow.replace("@cooling:", "").strip() #remove token
				if customCool[0] == "+":
					customCool = customCool[1:] #remove initial + sign if present
				self.customCoolList.append(customCool) #append cooling
				continue #not a reaction

			#search for custom heating and append to the list
			if "@heating:" in srow and inHeatingBlock:
				customHeat = srow.replace("@heating:", "").strip() #remove token
				if customHeat[0] == "+":
					customHeat = customHeat[1:] #remove initial + sign if present
				self.customHeatList.append(customHeat) #append heating
				continue #not a reaction

			#search for variables
			if "@var:" in srow:
				arow = srow.replace("@var:", "").split("=")
				if len(arow) != 2:
					print("ERROR: variable line must be @var:variable=F90_expression")
					print("found: " + srow)
					sys.exit()

				#look for array definition in var token
				arow[0] = coeVarArray(arow[0])

				#check if the current @var is allowed
				notAllowedVars = ["k", "tgas", "energy_ev", "n"]
				for nav in notAllowedVars:
					if nav.lower() == arow[0].split("(")[0].strip().lower():
						sys.exit("ERROR: you can't use " + nav + " as an @var variable")

				#check if the variable belongs to cooling or rate coefficient variables
				if not inCoolingBlock  and not inHeatingBlock:
					if arow[0] in self.coevars:
						print("ERROR: @var:" + arow[0] + " already defined in the network!")
						print("Around these lines in the network file:")
						lmin = max(0, line_number-2)
						lmax = min(line_number+3, len(allrows)-1)
						print(" " + "\n ".join(allrows[lmin:lmax]))
						print("Change name otherwise will be ovewritten!")
						sys.exit()
					self.coevars[arow[0]] = [ivarcoe, arow[1]]
					ivarcoe += 1 #count variables for later sorting
				elif inHeatingBlock:
					if arow[0] in self.heatVars:
						continue #skip already found variables
					self.heatVars[arow[0]] = [ivarHeat, arow[1]]
					ivarHeat += 1 #count variables for later sorting
				elif inCoolingBlock:
					if arow[0] in self.coolVars:
						continue #skip already found variables
					self.coolVars[arow[0]] = [ivarCool, arow[1]]
					ivarCool += 1 #count variables for later sorting
				continue #SKIP: a variable line is not a reaction line

			#search for common variables
			if "@common:" in srow:
				arow = srow.replace("@common:", "").split(",")
				for x in arow:
					commonvar = x.strip()
					if commonvar.split("_")[0].lower() != "user":
						print("ERROR: to avoid conflicts common variables with @common should "
							  "begin with user_")
						print(" you provided: " + commonvar)
						print(" it should be: user_" + commonvar)
						sys.exit()
					if commonvar in self.commonvars: continue #skip if already present
					self.reducerVars.append(commonvar)
					self.commonvars.append(commonvar) #add to the global array
				continue #skip: a common is not a reaction line

			#search for ghost species
			if "@ghost:" in srow:
				ghost = srow.replace("@ghost:", "").strip()
				aghost = ghost.split(",")
				for ghost in aghost:
					print("Found ghost species: " + ghost)
					mol = parser(ghost,mass_dic,atoms,self.thermodata)
					if mol.name not in spec_names:
						spec_names.append(mol.name)
						specs.append(mol)
					mol.idx = spec_names.index(mol.name) + 1
				continue #SKIP: a ghost line is not a reaction line

			#get ice species
			if "@ice:" in srow:
				ice = srow.replace("@ice:", "").strip()
				(iceName,rateType,krate) = ice.split(",", 2)
				mol = parser(iceName+"_total",mass_dic,atoms,self.thermodata)
				mol.mass = 0e0 #_total species mass is zero to avoid double counting
				if mol.name not in spec_names:
					spec_names.append(mol.name)
					specs.append(mol)
				mol.idx = spec_names.index(mol.name) + 1
				myrea = reaction()
				myrea.products = []
				myrea.reactants = []
				myrea.krate = krate
				myrea.idx = rcount
				myrea.canUseTabs = False
				myrea.hasTlimitMin = myrea.hasTlimitMax = False
				if iceName not in self.iceSpeciesList:
					self.iceSpeciesList[iceName] = dict()
				self.iceSpeciesList[iceName]["ODE"] = "0d0"
				if rateType.lower() == "freezeout":
					myrea.verbatim = iceName+" -> "+iceName+"_ice"
					self.iceSpeciesList[iceName]["reactionFreezeout"] = myrea
				elif rateType.lower() == "evaporation":
					myrea.verbatim = iceName+"_ice -> "+iceName
					self.iceSpeciesList[iceName]["reactionEvaporation"] = myrea
				else:
					sys.exit("ERROR: unknown reaction type "+rateType)
				reacts.append(myrea)
				rcount += 1
				continue

			#read surface reaction
			# e.g. @surface:RRP,H,OH,H2O,krate_2bodySi(n(:),idx_H,idx_OH,0d0,tabTdust)
			if "@surface:" in row:
				surf = srow.replace("@surface:", "").strip()
				asurf = surf.split(",")

				#read format
				fmt = list(asurf[0])

				#create reaction object
				myrea = reaction()
				myrea.products = []
				myrea.reactants = []

				enthalpy = 0e0 #init total enthalpy @ 300K, eV
				#loop on format (e.g. RRPP) to store species
				for i in range(len(fmt)):
					sp = asurf[i+1]
					#parse species
					mol = parser(sp + "_total", mass_dic, atoms, self.thermodata)
					#parse species without _total to get enthalpy
					mol2 = parser(sp, mass_dic, atoms, self.thermodata)
					kboltzmann_eV = 8.617332478e-5 #eV/K

					mol.mass = 0e0
					#add species to list if not present
					if mol.name not in spec_names:
						spec_names.append(mol.name)
						specs.append(mol)
					mol.idx = spec_names.index(mol.name) + 1

					#store species depending on format element (R or P)
					if fmt[i] == "R":
						myrea.reactants.append(mol)
						myrea.curlyR.append(False)
						enthalpy += mol2.enthalpy
					elif fmt[i] == "P":
						myrea.products.append(mol)
						myrea.curlyP.append(False)
						enthalpy -= mol2.enthalpy
					else:
						print("ERROR: unknown surface reaction format")
						print(fmt)
						sys.exit()

				#ending part is rate coefficients
				myrea.krate = (",".join(asurf[len(fmt)+1:]))
				myrea.idx = rcount
				myrea.canUseTabs = False
				myrea.hasTlimitMin = myrea.hasTlimitMax = False
				#store enthalpy in K
				myrea.enthalpyK = enthalpy/kboltzmann_eV

				#create verbatim and use _ice
				myrea.build_verbatim()
				myrea.verbatim = myrea.verbatim.replace("_total","_ice")

				#append reaction
				reacts.append(myrea)
				rcount += 1
				continue

			#search for table pragma
			if "@tabvar:" in srow:
				atab = srow.replace("@tabvar:", "").split("=")
				if len(atab) != 2:
					print("ERROR: wrong format, it should be @tabvar:varname=file,var1,var2")
					print(" You provided: " + srow.strip())
					sys.exit()
				aatab = atab[1].split(",")
				if len(aatab) != 3:
					print("ERROR: wrong format, it should be @tabvar:varname=file,var1,var2")
					print(" You provided: " + srow.strip())
					sys.exit()

				mytabvar = atab[0].strip()
				mytabpath = aatab[0].strip().replace("\"", "")
				mytabxxyy = aatab[1]+","+aatab[2]
				#updates anytab arrays
				create_tabvar(mytabvar, mytabpath, mytabxxyy, self.anytabvars, self.anytabfiles, self.anytabpaths,
					self.anytabsizes, self.coevars)
				continue #this is not a reaction line

			#search for format string
			if "@format:" in srow:
				idxFound = tminFound = tmaxFound = rateFound = qeffFound = False
				hasFormat = True #format flag
				srow = srow.replace("@format:", "") #remove
				#print "Found custom format: "+srow
				arow = srow.split(",") #split format line
				#check format (at least 4 elements)
				if len(arow) < 4:
					print("ERROR: format line must contains at least 4 elements")
					print(" idx,R,P,rate")
					print(" You provided "+str(len(arow))+" elements:")
					print(" " + srow)
					sys.exit()
				ipos = 0 #count items in the split line
				ireact = [] #reactants index array
				iprod = [] #products index array
				format_items = len(arow)
				#read format elements
				for x in arow:
					x = x.lower().strip() #lower trimmed item
					if x == "idx":
						iidx = ipos #index position
						idxFound = True
					if x == "r": ireact.append(ipos) #reactants positions
					if x == "p": iprod.append(ipos) #products positions
					if x == "tmin":
						iTmin = ipos #min temperature position
						tminFound = True
					if x == "tmax":
						iTmax = ipos #max temperature position
						tmaxFound = True
					if x == "rate" or x == "k":
						irate = ipos #rate in F90 style position
						rateFound = True
					if x == "qeff" or x == "qpp":
						iqeff = ipos
						qeffFound = True
					ipos += 1 #increase position
				#check for rate
				if not rateFound:
					print("ERROR: format must contain rate token")
					sys.exit()

				continue #SKIP format line (it is not a reaction line)

			#custom cooling block start
			if srow.lower() == "@cooling_start" or srow.lower() == "@cooling_begin":
				inCoolingBlock = True
				self.use_cooling = True
				continue #SKIP (not a reaction)

			#custom cooling block end
			if srow.lower() == "@cooling_end" or srow.lower() == "@cooling_stop":
				inCoolingBlock = False
				continue #SKIP (not a reaction)

			#custom heating block start
			if srow.lower() == "@heating_start" or srow.lower() == "@heating_begin":
				inHeatingBlock = True
				self.use_heating = True
				continue #SKIP (not a reaction)

			#custom heating block end
			if srow.lower() == "@heating_end" or srow.lower() == "@heating_stop":
				inHeatingBlock = False
				continue #SKIP (not a reaction)

			#if requested the next reaction will not uses tabs
			if srow.lower() == "@notabnext" or srow.lower() == "@notab_next":
				noTabNext = True
				continue #SKIP (not a reaction)
			#if requested the next reaction will not uses tabs for a BLOCK of reactions
			if srow.lower() == "@notab_begin" or srow.lower() == "@notab_start":
				noTabNext = noTabNextBlock = True
				continue #SKIP (not a reaction)
			#if requested the next reaction will not uses tabs for a BLOCK of reactions
			if srow.lower() == "@notab_end" or srow.lower() == "@notab_stop":
				noTabNext = noTabNextBlock = False
				continue #SKIP (not a reaction)

			#start block of reactions with constant rate during the solver call
			if srow.lower() == "@storeonce_begin" or srow.lower() == "@storeonce_start":
				inStoreOnceBlock = self.hasStoreOnceRates = True
				continue #SKIP (not a reaction)
			#end block of reactions with constant rate during the solver call
			if srow.lower() == "@storeonce_end" or srow.lower() == "@storeonce_stop":
				inStoreOnceBlock = False
				continue #SKIP (not a reaction)

			#start a CR reaction block
			if srow.lower() == "@cr_start" or srow.lower() == "@cr_begin":
				inCRblock = True
				noTabBlockStored = noTabNextBlock
				noTabNext = noTabNextBlock = True
				continue #SKIP (not a reaction)

			#start a CR reaction block
			if srow.lower() == "@cr_stop" or srow.lower() == "@cr_end":
				inCRblock = False
				noTabNext = noTabNextBlock = noTabBlockStored #restore the noTabNextBlock value before entering CR block
				continue #SKIP (not a reaction)

			#start a photo reaction block
			if srow.lower() == "@photo_start" or srow.lower() == "@photo_begin":
				if self.photoBins <= 0:
					print("ERROR: you are using "+srow.lower()+" in your reaction file")
					print(" with zero photo-bins. Use -photoBins=NBINS option.")
					sys.exit()
				inPhotoBlock = True
				noTabBlockStored = noTabNextBlock
				noTabNext = noTabNextBlock = True
				continue #SKIP (not a reaction)

			#start a photo reaction block
			if srow.lower() == "@photo_stop" or srow.lower() == "@photo_end":
				inPhotoBlock = False
				noTabNext = noTabNextBlock = noTabBlockStored #restore the noTabNextBlock value before entering inPhotoBlock
				continue #SKIP (not a reaction)

			#start an XRAY reaction block
			if srow.lower() == "@xray_start" or srow.lower() == "@xray_begin":
				inXRayBlock = True
				self.useXRay = True
				noTabBlockStored = noTabNextBlock
				noTabNext = noTabNextBlock = True
				continue #SKIP (not a reaction)

			#start an XRAY reaction block
			if srow.lower() == "@xray_stop" or srow.lower() == "@xray_end":
				inXRayBlock = False
				noTabNext = noTabNextBlock = noTabBlockStored #restore the noTabNextBlock value before entering inXRayBlock
				continue #SKIP (not a reaction)

			#search for final expression to modify the coefficients (start)
			if srow.lower() == "@reactionmodifier_start" or srow.lower() == "@reactionmodifier_begin":
				inReactionModifierBlock = True
				continue #SKIP (not a reaction)

			#search for final expression to modify the coefficients (start)
			if srow.lower() == "@odemodifier_start" or srow.lower() == "@odemodifier_begin":
				inOdeModifierBlock = True
				continue #SKIP (not a reaction)

			#search for surface chemistry reactions (start)
			if srow.lower() == "@surface_start" or srow.lower() == "@surface_begin":
				self.hasSurfaceReactions = True
				inSurfaceBlock = self.useSurface = True
				noTabBlockStored = noTabNextBlock
				noTabNext = noTabNextBlock = True
				continue #SKIP (not a reaction)

			#search for surface chemistry reactions (stop)
			if srow.lower() == "@surface_stop" or srow.lower() == "@surface_end":
				inSurfaceBlock = False
				noTabNext = noTabNextBlock = noTabBlockStored #restore the noTabNextBlock value before entering inSurfaceBlock
				continue #SKIP (not a reaction)

			#search for solomon reaction
			if srow.lower() == "@next_solomon":
				nextSolomon = True
				continue

			#search for solomon reaction
			if srow.lower() == "@next_h2photodissociation":
				nextH2photodissociation = True
				continue

			#store index if reaction is Solomon for H2 pumping
			if nextSolomon:
				self.indexSolomon = rcount + 1
				nextSolomon = False

			#store index if reaction is Solomon for H2 pumping
			if nextH2photodissociation:
				self.indexH2photodissociation = rcount + 1
				nextH2photodissociation = False

			arow = srow.split(self.separator,format_items-1) #split only N+1 elements with N separations
			arow[len(arow)-1] = arow[len(arow)-1].rsplit('!',1)[0].rsplit('#',1)[0] #strip end-of-line comments
			arow = [x.strip() for x in arow] #strip single elements
			if len(arow) != format_items:
				print("WARNING: wrong format for reaction "+str(rcount+1))
				print(srow)
				a = keyb_input("Any key to continue q to quit... ")
				if a == "q": sys.exit()
				continue #check line format (N elements, 4=idx+Tmin+Tmax+rate)
			found_one = True #flag to determine at least one reaction found
			rcount += 1 #count the total number of reaction found
			printStep = int(line_count/10)
			if line_count > 1000 and rcount % printStep == 0:
				print(str(round(rcount*1e2/line_count)) + "%")

			myrea = reaction() #create object reaction

			#use reaction index found into the file
			if self.useFileIdx:
				if not hasFormat or hasFormat and idxFound:
					reaction_idx = int(arow[0]) #index of the reaction (from file)
					if reaction_idx < 1:
						die("ERROR: reaction index must be > 0!\n Check your reaction file!")
					if reaction_idx in idx_list:
						die("ERROR: reaction index "+str(reaction_idx)+" already present!\n Check your reaction file!")
					myrea.idx = reaction_idx
					rcount = reaction_idx
			else:
				myrea.idx = rcount
				if not hasFormat or hasFormat and idxFound:
					if rcount != int(arow[0]): unmatch_idx = True

			reactants = [arow[x].strip().upper() for x in ireact]
			products = [arow[x].strip().upper() for x in iprod]

			#store reactants "curliness" before purge
			myrea.curlyR = [("{" in x) for x in reactants]

			#purge reactants name from curly brackets
			reactants = [x.replace("}","").replace("{","") for x in reactants]

			#remove empty reactants/products (i.e. dummy) and sort
			reags_clean = sorted([x for x in reactants if x.strip()!=""])
			prods_clean = sorted([x for x in products if x.strip()!=""])

			#check for identical reactions
			foundAlready = False
			for i in range(len(reags)):
				if reags_clean==reags[i] and prods_clean==prods[i] and self.mergeTlimits:
					foundAlready = True
					rcount -= 1 #decrease reaction index (since already increased few lines above)
					myrea.idx = idxs[i]
					print("already found: ("+str(idxs[i])+") "+(" + ".join(reags_clean))
						  +" -> "+(" + ".join(prods_clean)))

			#store reactants and products to find identical reactions (and different Tlimits)
			if not foundAlready:
				reags.append(reags_clean)
				prods.append(prods_clean)
				idxs.append(myrea.idx)

			opTlist = ["<",">",".LE.",".GE.",".LT.",".GT."]
			myrea.TminOp = self.TlimitOpLow
			myrea.TmaxOp = self.TlimitOpHigh
			for op in opTlist:
				if not tminFound: break
				if op in arow[iTmin]:
					arow[iTmin] = arow[iTmin].replace(op,"")
					myrea.TminOp = op.replace(">","GT").replace("<","LT").replace(".","")
					break
			for op in opTlist:
				if not tmaxFound: break
				if op in arow[iTmax]:
					arow[iTmax] = arow[iTmax].replace(op,"")
					myrea.TmaxOp = op.replace(">","GT").replace("<","LT").replace(".","")
					break

			myrea.Tmin = "2.73d0" #default min temperature
			myrea.Tmax = "1.d8" #default max temperature

			#search for reactions without Tlims (MIN)
			if tminFound:
				if arow[iTmin].strip().upper() in ["N", "NONE", "N/A", "NO", ""]:
					myrea.hasTlimitMin = False
			else:
				myrea.hasTlimitMin = False

			#search for reactions without Tlims (MAX)
			if tmaxFound:
				if arow[iTmax].strip().upper() in ["N", "NONE", "N/A", "NO", ""]:
					myrea.hasTlimitMax = False
			else:
				myrea.hasTlimitMax = False

			#store Tlimits if any
			if myrea.hasTlimitMin:
				if tminFound: myrea.Tmin = format_double(arow[iTmin]) #get Tmin
				if tminFound: TminAuto = min(float(arow[iTmin].lower().replace("d","e")), TminAuto)
			if myrea.hasTlimitMax:
				if tmaxFound: myrea.Tmax = format_double(arow[iTmax]) #get Tmax
				if tmaxFound: TmaxAuto = max(float(arow[iTmax].lower().replace("d","e")), TmaxAuto)
			#store other data
			area = arow[irate].split(":",2) #reaction can be if_condition:reaction_rate
			if len(area) == 1:
				myrea.ifrate = "" #store empty prepending if condition
				myrea.krate = arow[irate] #get reaction rate written in F90 style
			else:
				myrea.ifrate = area[0] #store prepending if condition
				myrea.krate = area[1] #get reaction rate written in F90 style
			if "krome_fshield" in myrea.krate.lower():
				fsh_found = True

			if qeffFound:
				myrea.qeff = arow[iqeff]

			#if(self.useCustomCoe): myrea.krate = "0.d0" #when custom function is used standard coefficient are set to zero
			#loop over reactants to grep molecules
			for r in reactants:
				if r.strip() == "G" and not self.use_photons: continue
				if r.strip() == "E-": r = "E"
				if r.strip() != "":
					mol = parser(r, mass_dic, atoms, thermodata)
					if mol.name not in spec_names:
						spec_names.append(mol.name)
						specs.append(mol)
					mol.idx = spec_names.index(mol.name) + 1
					myrea.reactants.append(mol) #add molecule object to reactants

			#loop over products to grep molecules
			for p in products:
				if p.strip() == "G" and not self.use_photons: continue
				if p.strip() == "E-": p = "E"
				if p.strip() != "":
					mol = parser(p,mass_dic,atoms,thermodata)
					if mol.name not in spec_names:
						spec_names.append(mol.name)
						specs.append(mol)
					mol.idx = spec_names.index(mol.name) + 1
					myrea.products.append(mol) #add molecule object to products

			#increases the index of the photoreaction
			if inPhotoBlock:
				self.nPhotoRea += 1
				myrea.idxph = self.nPhotoRea
				#add the photo reactant to the partner array
				self.photoPartners[myrea.idx] = myrea.reactants[0]

			#check for xsec from file
			if "@xsecFile=" in myrea.krate:
				if not inPhotoBlock:
					print("ERROR: @xsecFile token requires a photorate block!")
					print(" (i.e. @photo_start, @photo_stop tokes)")
					sys.exit()
				myrea.hasXsecFile = True
				myrea.Tmin = 0e0
			#store Tlimits if any
			if myrea.hasTlimitMin:
				if tminFound: myrea.Tmin = format_double(arow[iTmin])
			#if file is SWRI convert to KROME
			if "@xsecFile=SWRI" in myrea.krate:
				SWRI2KROME(self.buildFolder, myrea.reactants[0], myrea.products, myrea.Tmin)
			#if file is LEIDEN convert to KROME
			if "@xsecFile=LEIDEN" in myrea.krate:
				LEIDEN2KROME(self.buildFolder, myrea.reactants[0], myrea.products)
			# When reversed reactions need to be computed with Gibbs free
			# energy tables
			if "revKc_with_GFE" in myrea.krate:
				self.use_GFE_tables = True

			#this reaction is on surface
			if inSurfaceBlock:
				myrea.isSurface = True

			myrea.build_verbatim() #build reaction as string (e.g. A+B->C)
			#myrea.reactants = sorted(myrea.reactants, key=lambda r:r.idx) #sort reactants
			#myrea.products = sorted(myrea.products, key=lambda p:p.idx) #sort products
			myrea.build_RHS(self.useNuclearMult) #build RHS in F90 format (e.g. k(2)*n(10)*n(8) )
			myrea.build_phrate(inPhotoBlock) #build photoionization rate
			myrea.check(self.checkMode) #check mass and charge conservation
			myrea.group = group #add the group to the reaction
			myrea.canUseTabs = not(noTabNext or inStoreOnceBlock) #check if this reaction can use tabs or not
			myrea.isStoreOnce = inStoreOnceBlock
			if myrea.krate.count("(") != myrea.krate.count(")"):
				print("ERROR: unbalanced brackets in reaction " + str(myrea.idx))
				print(" "+myrea.verbatim)
				print(" rate = "+myrea.krate)
				print(" this is the corresponding line in the reaction file")
				print(srow)
				sys.exit()

			myrea.isCR = inCRblock #is a CR reaction
			myrea.isXRay = inXRayBlock #is an XRAY reaction

			#skip duplicated reactions if requested
			skip_append = False
			if skipDup:
				myrea.build_pseudo_hash() #build pseudo_hash
				if myrea.pseudo_hash in pseudo_hash_list:
					skip_append = True
					skipped_dupl += 1
					fdup.write(str(myrea.idx)+" "+myrea.verbatim+"\n")
					rcount -= 1
				else:
					pseudo_hash_list.append(myrea.pseudo_hash)

			#append reactions if not skipped
			if not skip_append: reacts.append(myrea)
			del myrea,row
			if not noTabNextBlock: noTabNext = False #return to default value when outside a block
			#END LOOP ON FILE

		#sort species by charge (negative first)
		specs = sorted(specs, key=lambda x:x.charge)

		#list of species names
		speciesNames = [x.name for x in specs]

		#redefine species index in each reaction
		for react in reacts:
			for reactant in react.reactants:
				reactant.idx = speciesNames.index(reactant.name)+1
			for product in react.products:
				product.idx = speciesNames.index(product.name)+1

		#reorder indexes
		newspecs = []
		for sp in specs:
			sp.idx = len(newspecs)+1
			newspecs.append(sp)
		specs = newspecs

		#after loop on file post-process special reactions
		#shielding reactions requires fsh variable
		if self.useShieldingDB96 or self.useShieldingWG11 or self.useShieldingR14 and not fsh_found:
			print("")
			print("WARNING: no krome_fshield(n(:),Tgas) variable found in rate coefficient")
			print(" even if shielding option is enabled.")
			print(" Please check your network file!")
			a = keyb_input("Any key to continue q to quit... ")
			if a == "q": sys.exit()

		if noTabNextBlock:
			print("ERROR: block of skipped reaction still open!")
			print("Add @noTab_stop or @noTab_end")
			sys.exit()

		if inSurfaceBlock:
			print("ERROR: block of surface reactions still open!")
			print("Add @surface_stop or @surface_end")
			sys.exit()

		if skipDup:
			fdup.close()
			print("Skipped duplicated reactions:", skipped_dupl)

		#check file format
		if not found_one:
			die("ERROR: no valid reactions found in file \""+filename+"\"")
		if unmatch_idx:
			print("WARNING: index in \""+filename+"\" are not sequential!")

		#prepares xray rates including self-shielding and secondary process
		xrayHFound = xrayHeFound = False
		for x in reacts:
			if not x.isXRay: continue
			if "auto" not in x.krate: continue
			ivarcoe = len(self.coevars)
			fake_ivarcoe = 0
			fake_coevars = dict()
			addVarCoe("ncolH","num2col(n(idx_H),n(:))",self.coevars)
			addVarCoe("ncolHe","num2col(n(idx_He),n(:))",self.coevars)
			addVarCoe("logHe","log10(ncolHe)",self.coevars)
			addVarCoe("logH","log10(ncolH)",self.coevars)
			addVarCoe("xe","min(n(idx_e) / (get_Hnuclei(n(:)) + 1d-40), 1d0)",self.coevars)

			#updates anytab arrays
			if x.reactants[0].name == "H":
				mytabvar = "user_xray_H"
				mytabpath = "data/ratexH.dat"
				mytabxxyy = "logH,logHe-logH"

				create_tabvar(mytabvar, mytabpath, mytabxxyy, self.anytabvars, self.anytabfiles,
							  self.anytabpaths, self.anytabsizes, self.coevars)

				addVarCoe("phiH",".3908d0*(1e0-xe**.4092)**1.7592 * 327.832286034056d0",
						  self.coevars)
				addVarCoe("ratexH"," 1d1**user_xray_H", self.coevars)

				xrayHFound = True
				autoRateXray = "(ratexH * (1d0+phiH) + n(idx_He)/(n(idx_H)+1d-40) * ratexHe * phiH)"
				x.krate = autoRateXray + "* J21xray"
				print("H xray ionization found!")

				#heating tabs H
				mytabvar = "user_xheat_H"
				mytabpath = "data/heatxH.dat"
				mytabxxyy = "logH,logHe-logH"
				create_tabvar(mytabvar,mytabpath,mytabxxyy,self.anytabvars,self.anytabfiles,self.anytabpaths,\
					self.anytabsizes,fake_coevars)

			elif x.reactants[0].name.lower() == "he":
				mytabvar = "user_xray_He"
				mytabpath = "data/ratexHe.dat"
				mytabxxyy = "logH,logHe-logH"

				create_tabvar(mytabvar,mytabpath,mytabxxyy,self.anytabvars,self.anytabfiles,self.anytabpaths,\
					self.anytabsizes,self.coevars)

				addVarCoe("phiHe",".0554d0*(1d0-xe**.4614)**1.666 * 180.793458763612d0",self.coevars)
				addVarCoe("ratexHe"," 1d1**user_xray_He",self.coevars)

				autoRateXRay = "(ratexHe * (1d0+phiHe) + n(idx_H)/(n(idx_He)+1d-40) * ratexH * phiHe)"
				x.krate = autoRateXRay + "* J21xray"
				xrayHeFound = True
				print("He xray ionization found!")

				#heating tabs He
				mytabvar = "user_xheat_He"
				mytabpath = "data/heatxHe.dat"
				mytabxxyy = "logH,logHe-logH"
				create_tabvar(mytabvar,mytabpath,mytabxxyy,self.anytabvars,self.anytabfiles,self.anytabpaths,\
					self.anytabsizes,fake_coevars)

			else:
				print("ERROR: xray reaction not tabulated!")
				print(" " + x.verbatim)
				print(" remove it from the chemical network or provide non-automatic rate.")
				print(" Note that you should also provide the heating tab if needed.")
				sys.exit()

		#check if both (H and He) xray reactions are found, since tables are H and He dependant
		if xrayHeFound != xrayHFound:
			print("ERROR: for xrays you must include both H and He reaction in your reaction files, "
				  "e.g.:")
			print(" @format:idx,R,P,P,Tmin,Tmax,rate")
			print(" 7,H,H+,E,NONE,NONE,auto")
			print(" 8,He,He+,E,NONE,NONE,auto")
			print(" where indexes are arbitrary")
			sys.exit()

		#check if both (H and He) xray reactions are found, when xray heating is enabled
		if not xrayHeFound and not xrayHFound and self.useHeatingXRay:
			print("ERROR: with XRAY heating option you must include both H and He reaction")
			print(" in your reaction files, e.g.:")
			print(" @format:idx,R,P,P,Tmin,Tmax,rate")
			print(" 7,H,H+,E,NONE,NONE,auto")
			print(" 8,He,He+,E,NONE,NONE,auto")
			print(" where indexes are arbitrary")
			sys.exit()

		#if both H2 pd and solomon index are not -1 means that user is using both
		if self.indexH2photodissociation >= 0 and self.indexSolomon >= 0:
			print("ERROR: you cannot use H2 photodissociation")
			print(" on the fly method (@next_H2photodissoctiation)")
			print(" and Solomon (@next_Solomom) together")
			sys.exit()

		#check for automatic reactions
		autoFound = False
		for rea in reacts:
			if rea.kphrate is not None:
				if rea.kphrate.lower().strip() == "auto":
					autoFound = True
					break
			if rea.krate is not None:
				if rea.krate.lower().strip() == "auto":
					autoFound = True
					break

		#load auto reaction from the database
		if autoFound:
			autoreacts = [] #dbase array contains dictionary with reaction data
			fdbase = self.fdbase
			print("Automatic reactions found, searching in "+fdbase)
			if not os.path.isdir(fdbase):
				print("ERROR: folder "+fdbase+" not found!")
				sys.exit()
			file_list = sorted([f for f in listdir(self.fdbase) if isfile(join(self.fdbase, f))])
			extraVars = dict() #dict of the extra variables, with key=filename
			isAutoRev = False
			for fname in file_list:
				fname = fdbase + fname
				if "~" in fname: continue
				extraVars[fname] = []
				fhdbase = open(fname) #open the database
				#load the database into an array of dictionaries
				for row in fhdbase:
					srow = row.strip()
					if srow == "": continue #skip blank
					if srow == "#BREAK DATABASE": break
					if srow[0] == "#": continue #skip comments
					#serach for extra variables and append
					if "@photoxsec:" in srow.lower(): break
					if "@cr:" in srow.lower(): break
					if "@photoav:" in srow.lower(): break
					if "@var" in srow:
						extraVars[fname].append(srow)
						continue
					#each reaction block starts with @type, init the reaction dictionary
					if "@type:" in srow:
						myrea = dict()
						myrea["autoFname"] = fname
						myrea["limits"] = ""
					if "@isrev:" in srow: isAutoRev = True
					myrea.update(at_extract(srow)) #append to the dictionary
					#each reaction block ends with @rate, append to the main database array
					if "@rate:" in srow:
						myrea["isAutoRev"] = isAutoRev
						autoreacts.append(myrea)
						isAutoRev = False

			#loop on the reactions to find auto
			necessaryExtraVars = []
			reaMultiTrange = []
			for i in range(len(reacts)):
				rea = reacts[i]
				if rea.kphrate is None:
					if rea.krate.lower().strip() != "auto": continue
				else:
					if rea.krate.lower().strip() != "auto" and \
							rea.kphrate.lower().strip() != "auto": continue
				dbFound = False
				#loop on autoreactions
				for autorea in autoreacts:
					autop = [x.upper().strip() for x in autorea["prods"].split(",")] #list of prods
					autor = [x.upper().strip() for x in autorea["reacts"].split(",")] #list of reacts
					if sorted([x.name for x in rea.reactants]) != sorted(autor): continue
					if sorted([x.name for x in rea.products]) != sorted(autop): continue
					#if there are necessary variables in the datafile that contains this reaction,
					# it appends to the list of the necessaryExtraVars
					extraVar = extraVars[autorea["autoFname"]]
					if len(extraVar) > 0:
						for ev in extraVar:
							if ev in necessaryExtraVars: continue
							necessaryExtraVars.append(ev)
					#if the reaction is found but was already found
					# it means that there are more temperature ranges
					if dbFound:
						if autorea["limits"].strip() == "":
							print("ERROR: two reactions found with overlapping limits")
							print(autorea["reacts"],"->",autorea["prods"])
							print(" Found in files "+autorea["autoFname"]+" and ")
							print(" "+reaFound["autoFname"])
							sys.exit()
						copyRea = copy.copy(rea)
						copyRea.Tmin = autorea["limits"].split(",")[0].strip()
						copyRea.Tmax = autorea["limits"].split(",")[1].strip()
						copyRea.krate = autorea["rate"]
						copyRea = readTOpt(copyRea)
						reaMultiTrange.append(copyRea)
						continue

					dbFound = True
					reaFound = autorea
					reacts[i].isAutoRev = autorea["isAutoRev"]
					if autorea["isAutoRev"]: reacts[i].build_RHS()
					if rea.kphrate == "auto":
						reacts[i].kphrate = autorea["rate"]
					else:
						reacts[i].krate = autorea["rate"]
					if autorea["limits"].strip() != "":
						reacts[i].Tmin = autorea["limits"].split(",")[0].strip()
						reacts[i].Tmax = autorea["limits"].split(",")[1].strip()
						reacts[i] = readTOpt(reacts[i])
						reacts[i].hasTlimitMin = True
						reacts[i].hasTlimitMax = True
					else:
						reacts[i].Tmin = "2.73d0"
						reacts[i].Tmax = "1d8"
						reacts[i].hasTlimitMax = reacts[i].hasTlimitMin = False
						break #no more reactions needed


				#error if automatic reaction not found
				if not dbFound:
					print("ERROR: reaction not found in the automatic database!")
					print(rea.verbatim,[x.name for x in rea.reactants])
					print("you can:")
					print("1. remove it from your network")
					print("2. provide a non-automatic reaction rate")
					print("3. add to the database "+fdbase)
					sys.exit()

			#append auto reactions found with different ranges of temperature
			if len(reaMultiTrange) > 0:
				for rea in reaMultiTrange:
					reacts.append(rea)

			#add @var to varcoe if necessary
			for ev in necessaryExtraVars:
				ev = ev.replace("@var:", "")
				nameVar, exprVar = [x.strip() for x in ev.split("=")]
				nameVar = coeVarArray(nameVar) #check for array
				if nameVar in self.coevars: continue
				self.coevars[nameVar] = [len(self.coevars),exprVar]

		#load bare and ice binding energy from file into a dictionary (K)
		#if useSemenov load the Semenov binding energies file
		if(self.useSemenov):
			print("use Semenov binding energies")
			fhbind = open("data/Ebare_ice_Semenov.dat")
		else:
			fhbind = open("data/Ebare_ice.dat")

		Ebind = dict()
		for row in fhbind:
			srow = row.strip()
			if srow == "": continue
			if srow[0] == "#": continue
			Ebind_spec, Ebind_bare, Ebind_ice = [x for x in srow.split(" ") if x!=""]
			Ebind[(Ebind_spec+"_DUST").upper()] = {"Ebare": Ebind_bare, "Eice":Ebind_ice}

		#increase the species to include bin-based surface species
		uspecs = []
		for sp in specs:
			if sp.is_chemisorbed: self.useChemisorption = True
			#if surface add species for each dust bin (append _BinIndex)
			# note: ignored if no surface reactions found
			if sp.is_surface and self.hasSurfaceReactions:
				#loop on the number of bins (all types)
				for idust in range(self.dustArraySize*len(self.dustTypes)):
					sp2 = parser(sp.name,mass_dic,atoms,thermodata,idust+1) #parse the new species
					sp2.idx = len(uspecs) + 1 #increase species index
					#if binding energy on surface are availble update
					if sp.name.upper() in Ebind:
						sp2.Ebind_ice = Ebind[sp.name.upper()]["Eice"]
						sp2.Ebind_bare = Ebind[sp.name.upper()]["Ebare"]
					uspecs.append(sp2) #append to the new array
			else:
				#non-surface species only need a new index
				sp.idx = len(uspecs) + 1
				uspecs.append(sp)
		specs = uspecs[:] #copy the extended species list to the old one

		#increase the number of reactions to include bin-based surface reactions
		ureacts = []
		for rea in reacts:
			#add reactions when a surface reaction is found
			if rea.isSurface:
				#loop on the number of dust bins (all types)
				for idust in range(self.dustArraySize*len(self.dustTypes)):
					rea2 = copy.copy(rea) #make a copy of the reaction
					dtype = self.dustTypes[idust // self.dustArraySize]
					jdust = idust-self.dustArraySize*int(idust/self.dustArraySize) + 1
					rea2.krate = rea2.krate.replace("auto_jdust", "idx_dust_"+dtype+"_"+str(jdust))
					rea2.krate = rea2.krate.replace("auto_idx", str(jdust))

					ureactants = rea2.reactants[:] #work on a copy of the reactants
					#loop on reactants
					for ir in range(len(ureactants)):
						rr = ureactants[ir]
						if not rr.is_surface: continue #non-surface reactants remain the same
						#copy the object with the name species_BinIndex
						ureactants[ir] = copy.copy(searchSpeciesByName(specs,rr.name+"_"+str(idust+1)))
					rea2.reactants = ureactants[:] #copy back the list to the list of the reactants

					uproducts = rea2.products[:] #work on a copy of the products
					#loop on products
					for ip in range(len(uproducts)):
						pp = uproducts[ip]
						if not pp.is_surface: continue #non-surface reactants remains the same
						#copy the object with the name species_BinIndex
						uproducts[ip] = copy.copy(searchSpeciesByName(specs,pp.name+"_"+str(idust+1)))
					rea2.products = uproducts[:] #copy the list of the products to the list of the products of the copied reaction

					rea2.idx = len(ureacts)+1 #reaction index
					rea2.build_RHS() #prepare RHS
					rea2.build_verbatim() #prepare verbatim reaction
					ureacts.append(rea2) #append to the array
			else:
				#non-surface reactions only need a new index
				rea.idx = len(ureacts)+1
				ureacts.append(rea)

		reacts = ureacts[:] #copy the extended list of reactions to the old one

		# copy Gibss free energy data tables to build folder
		# and make them if not yet done (e.g. JANAF)
		for sp in specs:
			if sp.has_GFE_table:
				self.GFE_species.append(sp)
				# if sp.hasJanafThermoTable:
				# 	janaf2krome(self.buildFolder, sp)
				# else:
				gfe_file = sp.name + ".gfe"

				#import os
				buildFolder = self.buildFolder
				if not os.path.exists(buildFolder):
					os.mkdir(buildFolder)
					print("Created " + buildFolder)

				shutil.copyfile(self.thermochemistryFolder + gfe_file,
				 			self.buildFolder + gfe_file)
				print("copied " + gfe_file)

			# check which clusterable species are present
			if sp.is_clustarable:
				self.clusterablesPresent.append(sp)

		#update number of connection per species
		for rea in reacts:
			for r in rea.reactants:
				specs[r.idx-1].links += 1
			for p in rea.products:
				specs[p.idx-1].links += 1

		#count reactions with unique index
		idxs = []
		nrea = 0
		for rea in reacts:
			if rea.idx in idxs:
				print("skipping "+rea.verbatim+" since index already used!")
				continue #skip reactions same index
			idxs.append(rea.idx)
			nrea += 1


		#check sinks (species that are only formed)
		if self.sinkCheck:
			print("checking sinks/sources...")
			allR = []
			allP = []
			for rea in reacts:
				for RR in rea.reactants:
					allR.append(RR.name)
				for PP in rea.products:
					allP.append(PP.name)

			allP = list(set(allP))
			allR = list(set(allR))

			sinks = []
			sources = []
			for PP in allP:
				if PP not in allR: sinks.append(PP)
			for RR in allR:
				if RR == "CR": continue
				if RR not in allP: sources.append(RR)
			if len(sinks) > 0:
				print("WARNING: sinks found, check your network ("+(", ".join(sinks))+")!")
				print(" Disable this control with -noSinkCheck")
				a = keyb_input("Any key to ignore q to quit... ")
				if a == "q": sys.exit()

			if len(sources) > 0:
				print("ERROR: sources found, check your network ("+(", ".join(sources))+")!")
				print(" Disable this control with -noSinkCheck")
				sys.exit()

		#check recombination (ion species that never recombine with electrons)
		if self.recCheck:
			recErrorOnce = False
			print("checking recombinations...")
			for sp in specs:
				if sp.charge > 0:
					found = False
					for rea in reacts:
						RR = sorted([x.name for x in rea.reactants])
						if RR == sorted([sp.name,"E"]):
							found = True
							break
					if not found:
						recErrorOnce = True
						print("ERROR: "+sp.name+" never recombines with electrons, "
												"check your network!")

			#error and stop if recombination error found
			if recErrorOnce:
				print(" Disable this control with -noRecCheck")
				sys.exit()

		#write reverse report tp file
		if self.checkReverse:
			DHthreshold = 3e3 #enthalpy of formation threshold (K)
			kJmol2K = 120.274e0 #kJ/mol -> K
			fhrev = open(self.buildFolder+"krome_reverse.log","w")
			fhrev.write("#REVERSE REPORT\n")
			fhrev.write("#This file shows the reactions in the network file and indicates if the reverse reaction is present.\n")
			fhrev.write("#The files also reports the reaction enthalpy of formation (K) for the reverse reaction.\n")
			fhrev.write("#If the difference is below "+str(DHthreshold)+" K and the reverse is not present, the symbol '*'\n")
			fhrev.write("# is added. It means that the reverse reaction is probably important.\n")
			for rea in reacts:
				RR = sorted([x.name for x in rea.reactants])
				PP = sorted([x.name for x in rea.products])
				#join the reactants and the products names to check for mutual neutralization
				RRPP = ("".join([x.name for x in (rea.products+rea.reactants)]))
				checkFlag = " *" #check flag
				#search for neutralization
				if ("+" in RRPP) and ("-" in RRPP) or ("E" in RRPP): checkFlag = "  "
				#single reactants/products are not considered as missing reverse
				if len(RR) == 1 or len(PP) == 1: checkFlag = "  "
				DH = compute_DHreact(PP,RR)*kJmol2K
				if DH > DHthreshold: checkFlag = "  "
				#search for reverse in the network
				reverseFound = "NO REVERSE FOUND" + checkFlag +" "+str(DH)
				for rea2 in reacts:
					RR2 = sorted([x.name for x in rea2.reactants])
					PP2 = sorted([x.name for x in rea2.products])
					if RR == PP2 and PP == RR2:
						reverseFound = rea2.verbatim
						break
				#prepares columns with spaces
				numFwd = str(rea.idx) + (" "*(5-len(str(rea.idx))))
				verbFwd = rea.verbatim + (" "*(50-len(rea.verbatim)))
				fhrev.write(numFwd+verbFwd+reverseFound+"\n")

			fhrev.close()

		#rebuild RHS to keep into account multi-range automatic reactions
		for rea in reacts:
			rea.build_RHS()

		#copy local to global vars
		self.nrea = nrea
		self.specs = specs
		self.reacts = reacts
		self.TminAuto = TminAuto
		self.TmaxAuto = TmaxAuto
		print("done!")

	#####################################################
	#define the phys_ variables (will be used in krome_commons and
	# in krome_user to create the get and set functions)
	def definePhysVariables(self):
		#variables are list [name, default_value_string]
		#note that phys_ will be prepended
		self.physVariables = [["Tcmb", "2.73d0"],
			["zredshift", "0d0"],
			["orthoParaRatio", "3d0"],
			["metallicity", "0d0"],
                        ["Tfloor", "2.73d0"]]

	#####################################################
	def photo_warnings(self):
		if self.is_test: return #skip warning if test mode
		if self.usePhIoniz or self.useHeatingPhoto:
			print("************************************************")
			print("REMINDER: note that, since you are using photon-based")
			print(" options, you need to initialize the machinery from")
			print(" your main file! Read the manual for further details.")
			print("************************************************")
			a = keyb_input("Any key to continue...")

	###############################################
	def do_reverse(self):
		#do reverse reaction if needed
		reacts = self.reacts
		if self.useReverse:
			print("")
			print("reversing reactions...")
			count_reverse = len(reacts)
			reacts_copy = [x for x in reacts] #create a copy of reacts to loop on
			#loop over found reactants
			for myrea in reacts_copy:
				#skip reactions with more than four products
				if len(myrea.products) > 3:
					print("WARNING: in reversing reaction "+myrea.verbatim+" more than "
						  "3 products found! Skipped.")
					continue
				elif myrea.kphrate:
					print("WARNING: "+myrea.verbatim+" is photoreaction! Not reversed.")
					continue
				else:
					count_reverse += 1
					myrev = reaction() #create object reaction for reverse
					myrev.idx = count_reverse
					myrev.Tmin = myrea.Tmin #get Tmin
					myrev.Tmax = myrea.Tmax #get Tmax
					myrev.hasTlimtMin = myrea.hasTlimitMin #get info on TlimitMin
					myrev.hasTlimtMax = myrea.hasTlimitMax #get info on TlimitMax
					myrev.reactants = myrea.products
					myrev.products = myrea.reactants
					myrev.TminOp = myrea.TminOp #get Tmin operator
					myrev.TmaxOp = myrea.TmaxOp #get Tmax operator
					myrev.build_verbatim() #build reaction as string (e.g. A+B->C)
					myrev.verbatim += " (REV)" #append REV label to reaction string
					myreactants = myrev.reactants #get list of reactants
					myproducts = myrev.products #get list of products
					myrev.curlyR = [False]*len(myrev.reactants)
					myrev.krate = myrea.krate #set the forward reaction
					myrev.krate = myrev.doReverse() #compute reverse reaction using Simoncini2013PRL
					myrev.build_RHS(self.useNuclearMult) #build RHS in F90 format (e.g. k(2)*n(10)*n(8) )
					myrev.check(self.checkMode) #check mass and charge conservation
					reacts.append(myrev)
			print("Inverse reaction added: " + str(count_reverse))

			self.reacts = reacts
			self.nrea = len(reacts)

	#########################
	#break degenerancy for reactions that contains catalyser
	# e.g. H + e -> H+ + e + e, which is equal to H -> H+ + e
	# arg1 and arg2 are the arrays of reactants and products
	# to return pruned reactant list
	def PRuniq(self,arg1, arg2):

		arg1n = []
		arg1c = [x.name for x in arg1]
		arg2c = [x.name for x in arg2]
		for x in arg1c:
			if x in arg2c:
				ii = arg2c.index(x)
				arg2c[ii] = "@@@@"
				continue
			arg1n.append(x)
		return arg1n

	###########################################
	#check if reactions have their reverse in the chemical network
	def check_reverse(self):
		return #no longer supported
		if not self.checkReverse: return
		idxRev = []
		reacts = self.reacts
		#loop on reacts
		for i in range(len(reacts)):
			if i in idxRev: continue #if already found reverse skip
			rea1 = reacts[i]
			#prune catalysers from reactants and products
			R1 = self.PRuniq(rea1.reactants, rea1.products)
			P1 = self.PRuniq(rea1.products, rea1.reactants)
			revFound = False
			#loop on reacts
			for j in range(i,len(reacts)):
				rea2 = reacts[j]
				#prune catalysers from reactants and products
				R2 = self.PRuniq(rea2.reactants, rea2.products)
				P2 = self.PRuniq(rea2.products, rea2.reactants)
				#verify conditions
				if R1 == P2 and R2==P1:
					revFound = True
					#store reverse index
					idxRev.append(i)
					idxRev.append(j)
					break #break loop when reverse found
			if not revFound:
				print("WARNING: no reverse reaction found for "+rea1.verbatim)

	###################################################
	def verifyThermochem(self):
		if not self.checkThermochem: return
		for x in self.specs:
			if x.name not in self.thermodata:
				print("WARNING: no thermochemical data for "+x.name+"!")

	###########################################add all the metals to cooling
	def addMetals(self):
		Zcools = self.Zcools
		specs = self.specs
		for zcool in Zcools:
			zFound = False #flag metal found
			#loop on specs to found metals
			for mol in specs:
				#when found break loop
				if mol.name.lower() == zcool.lower():
					zFound = True
					break
			#if not found parse and add
			if not zFound:
				print("Adding species \""+zcool+"\" (requested by metal cooling)")
				mymol = parser(zcool,self.mass_dic,self.atoms,self.thermodata)
				mymol.idx = len(specs)+1
				self.specs.append(mymol)
		self.totMetals = "tot_metals = " + (" + ".join(["n(idx_"
					   + x.replace("+","j")+")" for x in Zcools]))

	#######################################
	def addReaMin(self):
		for rea in self.reacts:
			if "krome_" not in rea.krate:
				rea.krate = "small + ("+rea.krate+")"


	##################################
	def computeEnthalpy(self):
		reacts = self.reacts
		for i in range(len(reacts)):
			reacts[i].enthalpy()
			vrb = reacts[i].verbatim.strip()
			avrb = vrb.split("->")
			#print avrb[0]+" "*(15-len(avrb[0])),"->",avrb[1]+" "*(15-len(avrb[1])),reacts[i].dH,"erg"
		self.reacts = reacts
		print("Enthalpy OK!")

	##################################
	def addDust(self):
		dustTypes = self.dustTypes
		specs = self.specs
		dustArraySize = self.dustArraySize
		#add dust to problem
		if self.useDust:
			print("")
			#look for dust atoms in species found
			for dType in dustTypes:
				dTypeFound = False #flag atom found
				#loop on specs to found dust atom
				for mol in specs:
					#when found break loop
					if mol.name.lower() == dType.lower():
						dTypeFound = True
						break
				useDustEvol = (self.useDustEvap or self.useDustGrowth or self.useDustSputter)
				#if not found add to specs parsing the name (e.g. Si)
				if not dTypeFound and useDustEvol:
						print("Add species \""+dType+"\" (request by dust type)")
						mymol = parser(dType, self.mass_dic, self.atoms, self.thermodata)
						mymol.idx = len(specs) + 1
						specs.append(mymol)
			#add dust bins as species
			for dType in dustTypes:
				for i in range(dustArraySize):
						#create the object named dust_type_index
						mymol = molec()
						mymol.name = "dust_"+dType+"_"+str(i+1)
						mymol.charge = 0
						mymol.mass = 0.e0
						mymol.ename = "dust_"+dType+"_"+str(i+1)
						mymol.fidx = "idx_dust_"+dType+"_"+str(i+1)
						mymol.idx = len(specs)+1
						specs.append(mymol)
			print("Dust added:", self.dustArraySize*self.dustTypesSize)

			#add dust temperature as a species
			if self.usedTdust:
				for dType in dustTypes:
					for i in range(dustArraySize):
							#create the object named dust_type_Tdust_index
							mymol = molec()
							mymol.name = "dust_"+dType+"_Tdust_"+str(i+1)
							mymol.charge = 0
							mymol.mass = 0.e0
							mymol.ename = "dust_"+dType+"_Tdust_"+str(i+1)
							mymol.fidx = "idx_dust_"+dType+"_Tdust_"+str(i+1)
							mymol.idx = len(specs)+1
							specs.append(mymol)
				print("Dust temperature added:",self.dustArraySize*self.dustTypesSize)

		self.specs = specs

	###################################
	def addSpecial(self):
		specs = self.specs
		customODEs = self.customODEs
		#look for photons and CR to add to the species list
		has_g = has_CR = False
		for mol in specs:
		#	if(mol.name=="G"):
		#		has_g = True
			if mol.name == "CR":
				has_CR = True

		#append custom ODEs as species
		if len(customODEs) > 0:
			for x in customODEs:
				mymol = molec()
				mymol.name = x[0]
				mymol.charge = 0
				mymol.mass = 0.e0
				mymol.ename = mymol.name
				mymol.fidx = "idx_" + mymol.name
				mymol.idx = len(specs)+1
				specs.append(mymol)
				print("Added "+mymol.name+" as species")

		#append CR as species named CR
		if not has_CR:
			mymol = molec()
			mymol.name = "CR"
			mymol.charge = 0
			mymol.mass = 0.e0
			mymol.ename = "CR"
			mymol.fidx = "idx_CR"
			mymol.idx = len(specs)+1
			specs.append(mymol)
			self.photon_species = mymol #store object
			#print "Cosmic Rays (CR) added!"

		#append photons as species named g
		if not has_g:
			mymol = molec()
			mymol.name = "g"
			mymol.charge = 0
			mymol.mass = 0.e0
			mymol.ename = "g"
			mymol.fidx = "idx_g"
			mymol.idx = len(specs)+1
			specs.append(mymol)
			self.photon_species = mymol #store object
			#print "Photons (g) added!"

		#append temperature as species named Tgas
		mymol = molec()
		mymol.name = "Tgas"
		mymol.charge = 0
		mymol.mass = 0.e0
		mymol.ename = "Tgas"
		mymol.fidx = "idx_Tgas"
		mymol.idx = len(specs)+1
		specs.append(mymol)
		self.Tgas_species = mymol #store object

		#append dummy as species named dummy
		mymol = molec()
		mymol.name = "dummy"
		mymol.charge = 0
		mymol.mass = 0.e0
		mymol.ename = "dummy"
		mymol.fidx = "idx_dummy"
		mymol.idx = len(specs)+1
		specs.append(mymol)
		self.dummy = mymol #store object

		self.specs = specs
	##############################################
	def countSpecies(self):
		#evaluate the number of chemical species (excluding, dust, dummies, Tgas)
		self.nmols = len(self.specs)-4-self.dustArraySize*self.dustTypesSize
		if self.usedTdust: self.nmols -= self.dustArraySize*self.dustTypesSize
		#print found values
		print("")
		print("ODEs needed:", len(self.specs))
		print("Reactions found:", len(self.reacts))
		print("Species found:", self.nmols)
		if self.nPhotoRea > 0:
			print("Photo reactions found: ", self.nPhotoRea)


	########################
	def uniq(self,a):
		u = []
		for x in a:
			if x not in u: u.append(x)
		return u

	###############################################
	def dumpNetwork(self):

		buildFolder = self.buildFolder
		#create build folder if not exists
		#(this block is also in prepareBuild method)
		if not os.path.exists(buildFolder):
			os.mkdir(buildFolder)
			print("Created "+buildFolder)

		#dump species to log file
		fout = open(self.buildFolder+"info.log","w")
		fout.write("#This file contains index list and other info\n")

		fout.write("\n\n#********************************\n")
		fout.write("# Species list with their indexes\n")
		idx = 0
		for mol in self.specs:
			idx += 1
			fout.write(str(idx)+"\t"+mol.name+"\t""krome_"+mol.fidx+"\n")
		fout.write("\n")
		fout.write("#Note: the first "+str(self.nmols)+" variables above should be used\n")
		fout.write("# as framework code passive scalars, while the last "
				   + str(len(self.specs)-self.nmols)+"\n")
		fout.write("# are employed inside KROME.\n")

		#species as a python list
		fout.write("\n\n#********************************\n")
		fout.write("# Species in a Python list of strings\n")
		idx = 0
		pylist = ""
		for mol in self.specs:
			idx += 1
			pylist += "\""+mol.name+"\", "
			if idx % 10 == 0: pylist += "\\\n "
		fout.write("["+pylist+"]\n")


		#table with info as a structure
		addInfo = [["krome_nrea", str(self.nrea), "!number of reactions"],
			["krome_nmols", str(self.nmols), "!number of chemical species"],
			["krome_nspec", str(len(self.specs)), "!number of species including Tgas,CR,..."],
			["krome_ndust", str(self.dustArraySize*self.dustTypesSize), "!number of dust bins (total)"],
			["krome_ndustTypes", str(self.dustTypesSize), "!number of dust types"],
			["krome_nPhotoBins", str(self.photoBins), "!number of radiation bins"],
			["krome_nPhotoRates", str(self.nPhotoRea), "!number of photochemical reactions"]]

		fout.write("\n\n#********************************\n")
		fout.write("#useful parameters\n")
		#dump structure to file
		for row in addInfo:
			fout.write(fillSpaces(row[0],20)+" = "+fillSpaces(row[1],5)+row[2]+"\n")

		fout.write("\n\n#********************************\n")
		fout.write("#list of reactions (including with multiple limits)\n")
		#dump reactions to log file
		idx = maxprod = maxreag = 0
		verbatimList = []
		for rea in self.reacts:
			idx += 1
			rcount = pcount = 0
			#search for the maximum number of reactants and products
			for r in rea.reactants:
				if r.name != "": rcount += 1
			maxreag = max(maxreag,rcount)
			for p in rea.products:
				if p.name != "": pcount += 1
			maxprod = max(maxprod,pcount)
			fout.write(str(rea.idx)+"\t"+rea.verbatim+"\n")
			verbatimList.append(rea.verbatim)

		#dump list of reactions (without reactions with multiple limits)
		verbatimList = list(set(verbatimList))
		idx = 0
		fout.write("\n\n#********************************\n")
		fout.write("#list of reactions (without multiple limits)\n")
		for verb in verbatimList:
			idx += 1
			fout.write(str(idx)+"\t"+verb+"\n")

		idx = 0
		#dump list of reactions (latex format)
		fout.write("\n\n#********************************\n")
		fout.write("#list of reactions (LaTeX format)\n")
		columnsFormat = ("l"*(2*maxreag))+"c"+("l"*(2*maxprod-1))
		fout.write("#Table columns format {"+columnsFormat+"}\n")
		for rea in self.reacts:
			idx += 1
			rList = [x.nameLatex for x in rea.reactants]
			pList = [x.nameLatex for x in rea.products]
			rLatex = (" & + & ".join(rList+[""]*(maxreag-len(rList))))
			pLatex = (" & + & ".join(pList+[""]*(maxprod-len(pList))))
			verbLatex = str(idx)+" & "+rLatex+" & $\\to$ & "+pLatex+"\\\\\n"
			verbLatex = verbLatex.replace("& + &  &","& & &")
			fout.write(" "+verbLatex)

		fout.close()
		print("Species list, reactions, and info saved in "+self.buildFolder+"info.log")

		#dump species to gnuplot initialization
		fout = open(self.buildFolder+"species.gps","w")
		fout.write("#This file is a script to initialize the species indexes for gnuplot\n")
		idx = 0
		fout.write("\n")
		fout.write("\n")
		fout.write("if(!exists(\"nkrome\")) print \"ERROR: first set the value of the column-offset nkrome\"\n")
		inits = []
		for mol in self.specs:
			idx += 1
			inits.append("krome_"+mol.fidx+" = "+str(idx)+" + nkrome")
		fout.write(("\n".join(inits))+"\n")
		fout.write("print \"All variables set as e.g. krome_idx_H2\"\n")
		fout.write("print \"plot 'your_output' u 1:krome_idx_H2\"\n")
		fout.write("print \" the offset is nkrome=\",nkrome\n")
		fout.close()
		print("Species index initialization for gnuplot in "+self.buildFolder+"species.gps")

		#dump heating and cooling index initialization for gnuplot
		fout = open(self.buildFolder+"heatcool.gps","w")
		fout.write("#This file is a script to initialize the heating and cooling index in krome gnuplot\n")
		idx = 0
		fout.write("\n")
		fout.write("\n")
		fout.write("if(!exists(\"nkrome_heatcool\")) print \"ERROR: first you must set the column-offset nkrome_heatcool\"\n")
		idxcools = get_cooling_index_list()
		idxheats = get_heating_index_list()
		inits = []
		for idx in idxcools:
			inits.append("krome_"+idx+" + nkrome_heatcool")
		for idx in idxheats:
			inits.append("krome_"+idx+" + nkrome_heatcool")
		fout.write(("\n".join(inits))+"\n")
		fout.write("print \"All variables set as e.g. krome_idx_cool_H2\"\n")
		fout.write("print \"plot 'your_file' u 1:krome_idx_cool_H2\"\n")
		fout.write("print \" the offset is nkrome_heatcool=\",nkrome_heatcool\n")
		fout.close()
		print("Heating cooling index init for gnuplot in "+self.buildFolder+"heatcool.gps")

		#dump network to dot file
		fout = open(self.buildFolder+"network.dot","w")
		ntw = dict()
		dot = "digraph{\n"
		for rea in self.reacts:
			react = self.uniq(rea.reactants)
			prods = self.uniq(rea.products)
			for x in react:
				dot += "\""+x.name+"\" -> k"+str(rea.idx) +"\n"
			for y in prods:
				dot += "k"+str(rea.idx) +" -> \""+y.name+"\";\n"
		dot +="}\n"
		fout.write(dot)
		fout.close()
		print("Reactions saved in "+self.buildFolder+"reactions.log")

	##############################################
	#write C headers if requested

	###############################################
	# write the C header for krome_main
	def makeMainCHeader(self):
		if not self.interfaceC:
			return

		fh = open(self.srcFolder+"krome.h")
		if self.buildCompact:
			fout = open(self.buildFolder+"krome_all.h","w")
		else:
			fout = open(self.buildFolder+"krome.h","w")

		skip = False
		for row in fh:
			srow = row.strip()

			if srow == "#IFKROME_useX" and not self.useX: skip = True
			if srow == "#ELSEKROME" and not self.useX: skip = False
			if srow == "#ELSEKROME" and self.useX: skip = True
			if srow == "#ENDIFKROME": skip = False

			if skip: continue
			if row[0] != "#": fout.write(row.lower())

		if not self.buildCompact:
			fout.close()

	###############################################
	# write the C header for krome_user
	def makeUserCHeader(self):
		import re

		if not self.interfaceC: return

		fh = open(self.srcFolder+"krome_user.h")
		if self.buildCompact:
			fout = open(self.buildFolder+"krome_all.h","a")
		else:
			fout = open(self.buildFolder+"krome_user.h","w")

		foutc = open(self.buildFolder+"krome_header.c","w")

		skip = False
		for row in fh:
			srow = row.strip()

			if srow == "#IFKROME_usePhotoBins" and self.photoBins <= 0: skip = True
			if srow == "#IFKROME_useStars" and not self.useStars: skip = True
			if srow == "#IFKROME_use_cooling" and not self.use_cooling: skip = True
			if srow == "#IFKROME_use_thermo" and not self.use_thermo: skip = True
			if srow == "#IFKROME_use_coolingZ" and not self.useCoolingZ: skip = True
			if srow == "#IFKROME_use_coolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_useXrays" and not self.useXRay: skip = True
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_has_electrons" and not self.hasElectrons: skip = True
			if srow == "#IFKROME_useTabsTdust" and not self.useDustTabs: skip = True
			if srow == "#IFKROME_dust_opacity" and not self.useDust: skip = True

			if srow == "#ENDIFKROME": skip = False
			if srow == "#ENDIFKROME_dust_opacity": skip = False

			if skip: continue

			if srow == "#KROME_species":
				# write out KROME species
				allBasics = []
				for sp in self.specs:
					if sp.is_surface and self.hasSurfaceReactions:
						xbasic = ("_".join(sp.fidx.split("_")[:-1]))
						xname = ("_".join(sp.name.split("_")[:-1]))
						if xbasic not in allBasics:
							fout.write("extern const int krome_"+xbasic + "; //"+xname+"\n")
							foutc.write("const int krome_"+xbasic + " = " + str(sp.idx-1) +"; //"+xname+"\n")
							allBasics.append(xbasic)

					fout.write("extern const int krome_"+sp.fidx + "; // "+sp.name+"\n")
					foutc.write("const int krome_"+sp.fidx + " = " + str(sp.idx-1) +"; // "+sp.name+"\n")

				# write out the names of the species
				fout.write("extern const char* krome_names[];\n")
				foutc.write("const char* krome_names[] = {\n")
				foutc.write(",\n".join(["  \""+x.name+"\"" for x in self.specs])+"\n};")
				foutc.write("\n")

			elif srow == "#KROME_cool_index":
				# write out KROME cooling terms
				idxcool = get_cooling_index_list()
				for xx in idxcool:
					# C arrays start from 0; decrement all indices by one.
					if xx[:6] != 'ncools':
						xx = re.sub(r'= (\d+)', lambda m: '= {0}'.format(int(m.group(1))-1), xx)
					name=xx.index('=')-1
					fout.write("extern const int krome_"+xx[:name]+";\n")
					foutc.write("const int krome_" + xx + ";\n")

			elif srow == "#KROME_heat_index":
				# write out KROME heating terms
				idxheat = get_heating_index_list()
				for xx in idxheat:
					# C arrays start from 0; decrement all indices by one.
					if xx[:6] != 'nheats':
						xx = re.sub(r'= (\d+)', lambda m: '= {0}'.format(int(m.group(1))-1), xx)
					name=xx.index('=')-1
					fout.write("extern const int krome_"+xx[:name]+";\n")
					foutc.write("const int krome_"+xx+";\n")

			elif srow == "#KROME_constant_list":
				# write out KROME constants
				const = ""
				consth = ""
				constants = self.constantList
				newc = []
				for i in range(len(constants)):
					x = constants[i]
					# we need to replace all named constants on the right hand side
					# (C won't accept named constants on the RHS).
					# C will also not accept function in constants (e.g., sqrt, **2);
					# when necessary, we replace the expression on the RHS with the
					# pre-calculated value.
					if x[0] == "pre_planck":
						x[1] = "1.4744993357e-47"
					elif x[0] == "pre_kvgas_sqrt":
						x[1] = "1.87504433599e-08"
					else:
						sources = re.split('\W+',x[1].replace("krome_",""))
						for j in range(i):
							y = constants[j]
							for s in sources:
								if y[0] == s:
									x[1] = x[1].replace("krome_"+s,y[1])
					newc.append(x)

				for x in newc:
					x[1] = x[1].replace('d','e')
					consth += "extern const double krome_" + x[0] + "; //" + x[2] + "\n"
					const += "const double krome_" + x[0] + " = " + x[1] + "; //" + x[2] + "\n"
				fout.write(consth)
				foutc.write(const)

			elif srow == "#KROME_common_alias":
				#get the list of all the atoms contained in the species, H,C,O,...
				atoms = []
				for x in self.specs:
					atoms += x.atomcount.keys()
				atoms = list(set(atoms))
				atoms = [x for x in atoms if not(x in ["+","-"]) and not x.startswith("_")]

				fout.write("extern const int krome_nrea;\n")
				fout.write("extern const int krome_nmols;\n")
				fout.write("extern const int krome_nspec;\n")
				fout.write("extern const int krome_natoms;\n")
				fout.write("extern const int krome_ndust;\n")
				fout.write("extern const int krome_ndustTypes;\n")
				fout.write("extern const int krome_nPhotoBins;\n")
				fout.write("extern const int krome_nPhotoRates;\n")


				foutc.write("const int krome_nrea=" + str(self.nrea) + ";\n")
				foutc.write("const int krome_nmols=" + str(self.nmols) + ";\n")
				foutc.write("const int krome_nspec=" + str(len(self.specs)) + ";\n")
				foutc.write("const int krome_natoms=" + str(len(atoms)) + ";\n")
				foutc.write("const int krome_ndust=" + str(self.dustArraySize*self.dustTypesSize) + ";\n")
				foutc.write("const int krome_ndustTypes=" + str(self.dustTypesSize) + ";\n")
				foutc.write("const int krome_nPhotoBins=" + str(self.photoBins) + ";\n")
				foutc.write("const int krome_nPhotoRates=" + str(self.nPhotoRea) + ";\n")

			elif srow == "#KROME_user_commons_functions":
				# write interfaces/signatures for user_commons get/set functions
				funcs = ""
				for x in self.commonvars:
					fsetname = "krome_set_"+x.lower()
					fset = "extern void "+fsetname+"(double argset);\n"
					fgetname = "krome_get_"+x.lower()
					fget = "extern double "+fgetname+"();\n"
					funcs += fset + fget
				fout.write(funcs)

			elif srow == "#KROME_set_get_phys_functions":
				for x in self.physVariables:
					funcname = "krome_set_"+x[0].lower()
					fout.write("extern void "+funcname+"(double arg);\n")
					funcname = "krome_get_"+x[0].lower()
					fout.write("extern double "+funcname+"();\n")

			elif srow == "#KROME_cooling_functions":
				for x in self.coolZ_functions:
					funcname =  "krome_"+x[0].lower()
					fout.write("extern double "+funcname+"(double *x, double intgas);\n")

			else:
				if len(srow) > 0:
					if srow[0] != "#": fout.write(row.lower())
				else:
					fout.write(row)
					foutc.write(row)

		if not self.buildCompact:
			fout.close()
		foutc.close()

	###############################################
	def makePythonModule(self):
		if not self.interfacePy: return

		fh = open(self.srcFolder+"pykrome.py")
		fout = open(self.buildFolder+"pykrome.py","w")

		skip = False
		for row in fh:
			srow = row.strip()

			if srow == "#IFKROME_useX" and not self.useX: skip = True
			if srow == "#ELSEKROME" and not self.useX: skip = False
			if srow == "#ELSEKROME" and self.useX: skip = True

			if srow == "#IFKROME_usePhotoBins" and self.photoBins <= 0: skip = True
			if srow == "#IFKROME_useStars" and not self.useStars : skip = True
			if srow == "#IFKROME_use_cooling" and not self.use_cooling: skip = True
			if srow == "#IFKROME_use_thermo" and not self.use_thermo: skip = True
			if srow == "#IFKROME_use_coolingZ" and not self.useCoolingZ: skip = True
			if srow == "#IFKROME_use_coolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_useXrays" and not self.useXRay: skip = True
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_has_electrons" and not self.hasElectrons: skip = True
			if srow == "#IFKROME_useTabsTdust" and not self.useDustTabs: skip = True
			if srow == "#IFKROME_dust_opacity" and not self.useDust: skip = True

			if srow == "#ENDIFKROME": skip = False
			if srow == "#ENDIFKROME_dust_opacity": skip = False

			if skip: continue

			if srow == "#KROME_species":
				# write out KROME species
				allBasics = []
				for sp in self.specs:
					if sp.is_surface and self.hasSurfaceReactions:
						xbasic = ("_".join(sp.fidx.split("_")[:-1]))
						xname = ("_".join(sp.name.split("_")[:-1]))
						if xbasic not in allBasics:
							fout.write("\t\tself.krome_"+xbasic + " = " + str(sp.idx-1)
									   +" # "+xname+"\n")
							allBasics.append(xbasic)

					fout.write("\t\tself.krome_"+sp.fidx + " = " + str(sp.idx-1)
							   +" # "+sp.name+"\n")

				# write out the names of the species
				fout.write("\t\tself.krome_names = (\n")
				fout.write(",\n".join(["\t\t\t\t\t\t\""+xx.name+"\"" for xx in self.specs])
						   +"\n\t\t\t\t\t\t)")
				fout.write("\n")

			elif srow == "#KROME_cool_index":
				# write out KROME cooling terms
				idxcool = get_cooling_index_list()
				for x in idxcool:
					# Python arrays start from 0; decrement all indices by one.
					if x[:6] != 'ncools':
						x = re.sub(r'= (\d+)', lambda m: '= {0}'.format(int(m.group(1))-1), x)
					fout.write("\t\tself.krome_"+x+"\n")

			elif srow == "#KROME_heat_index":
				# write out KROME heating terms
				idxheat = get_heating_index_list()
				for x in idxheat:
					# Python arrays start from 0; decrement all indices by one.
					if x[:6] != 'nheats':
						x = re.sub(r'= (\d+)', lambda m: '= {0}'.format(int(m.group(1))-1), x)
					fout.write("\t\tself.krome_"+x+"\n")

			elif srow == "#KROME_constant_list":
				# write out KROME constants
				const = ""
				constants = self.constantList
				newc = []
				for i in range(len(constants)):
					x = constants[i]
					for j in range(i):
						y = constants[j]
					x[1] = x[1].replace("sqrt","np.sqrt")
					newc.append(x)

				for x in newc:
					x[1] = x[1].replace('d','e')
					x[1] = x[1].replace('krome','self.krome')
					const += "\t\tself.krome_" + x[0] + " = " + x[1] + " # " + x[2] + "\n"
				fout.write(const)

			elif srow == "#KROME_common_alias":
				#get the list of all the atoms contained in the species, H,C,O,...
				atoms = []
				for x in self.specs:
					atoms += x.atomcount.keys()
				atoms = list(set(atoms))
				atoms = [x for x in atoms if not(x in ["+","-"])]

				fout.write("\t\tself.krome_nrea = " + str(self.nrea) + "\n")
				fout.write("\t\tself.krome_nmols = " + str(self.nmols) + "\n")
				fout.write("\t\tself.krome_nspec = " + str(len(self.specs)) + "\n")
				fout.write("\t\tself.krome_natoms = " + str(len(atoms)) + "\n")
				fout.write("\t\tself.krome_ndust = " + str(self.dustArraySize*self.dustTypesSize) + "\n")
				fout.write("\t\tself.krome_ndustTypes = " + str(self.dustTypesSize) + "\n")
				fout.write("\t\tself.krome_nPhotoBins = " + str(self.photoBins) + "\n")
				fout.write("\t\tself.krome_nPhotoRates = " + str(self.nPhotoRea) + "\n")

			elif srow == "#KROME_user_commons_functions":
				# write interfaces/signatures for user_commons get/set functions
				funcs = ""
				for x in self.commonvars:
					fsetname = "krome_set_"+x.lower()
					fset = "\t\tfortran."+fsetname+".restype = None\n"
					fset += "\t\tfortran."+fsetname+".argtypes = [ctypes.c_double]\n"
					fgetname = "krome_get_"+x.lower()
					fget = "\t\tfortran."+fgetname+".restype = ctypes.c_double\n"
					fget += "\t\tfortran."+fgetname+".argtypes = None\n"
					funcs += fset + fget
				fout.write(funcs)

			elif srow == "#KROME_set_get_phys_functions":
				funcs = ""
				for x in self.physVariables:
					fsetname = "krome_set_"+x[0].lower()
					fset = "\t\tfortran."+fsetname+".restype = None\n"
					fset += "\t\tfortran."+fsetname+".argtypes = [ctypes.c_double]\n"
					fgetname = "krome_get_"+x[0].lower()
					fget = "\t\tfortran."+fgetname+".restype = ctypes.c_double\n"
					fget += "\t\tfortran."+fgetname+".argtypes = None\n"
					funcs += fset + fget
				fout.write(funcs)

			elif srow == "#KROME_cooling_functions":
				for x in self.coolZ_functions:
					funcname =  "krome_"+x[0].lower()
					fout.write("\t\tfortran."+funcname+".restype = ctypes.c_double\n")
					fout.write("\t\tfortran."+funcname+".argtypes = [array_1d_double, ctypes.c_double]\n")

			else:
				if len(srow) > 0:
					if srow[0:3] != "#IF" and srow[0:5] != "#ELSE" and srow[0:4] != "#END":
						fout.write(row)
				else:
					fout.write(row)

	###############################################
	def CInterface(self):
		if not self.interfaceC: return

		print("- writing KROME C headers...",)
		# generate C header files, one for each KROME Fortran module which
		# should be accessible to the user.
		self.makeMainCHeader()
		self.makeUserCHeader()

	###############################################
	def PyInterface(self):
		if not self.interfacePy: return

		print("- writing pykrome.py...",)
		# generate pykrome.py file
		self.makePythonModule()

	###############################################
	#show info about ODE system
	def showODE(self):
		dustArraySize = self.dustArraySize
		specs = self.specs
		nmols = self.nmols
		#include dust in ODE array partition
		sdust = sdustT = ""
		if self.useDust:
			#include dust by types
			for dType in self.dustTypes:
				sdust += " + ["+str(dustArraySize)+" "+dType+"-dust]"
		sODEpart = "ODE partition: [" + str(nmols) + " atom/mols] "+sdust+sdustT+" + [1 CR] + [1 PHOT] + [1 Tgas] + [1 dummy] = "
		sODEpart += str(len(specs))+" ODEs"
		print(sODEpart)

		#if species are few print list of species
		if len(specs) < 20: print("ODEs list: "+(", ".join([x.name for x in specs])))
		print("")

	############################################
	def createODE(self):
		reacts = self.reacts
		specs = self.specs
		dustTypes = self.dustTypes
		dustArraySize = self.dustArraySize
		dustTypesSize = len(dustTypes)
		ndust = dustArraySize*dustTypesSize
		nmols = self.nmols
		dummy = self.dummy

		#maximum number of parts in RHS
		maxDnsParts = 1000

		#create explicit differentials
		dns = ["\n!"+sp.name+"\ndn("+sp.fidx+") = 0.d0" for sp in specs] #initialize
		dnsCount = [0 for sp in specs] #initialize RHS part count
		idxs = [] #already employed indexes
		for rea in reacts:
			if rea.idx in idxs: continue #skip if already employed index
			idxs.append(rea.idx)
			rhs = "kflux("+str(rea.idx)+")"
			if self.humanFlux: rhs = rea.RHS
			for r in rea.reactants:
				nameUpper = r.name.upper()
				#add RHS to ice ODE
				if nameUpper in self.iceSpeciesList:
					self.iceSpeciesList[nameUpper]["ODE"] += " -"+rhs
				if r.name == "E" and self.useComputeElectrons: continue

				dnsCount[r.idx-1] += 1
				if dnsCount[r.idx-1] % maxDnsParts == 0:
					dns[r.idx-1] += " dn("+r.fidx+") = "
				dns[r.idx-1] = dns[r.idx-1].replace(" = 0.d0"," =")
				dns[r.idx-1] += " -"+rhs
			for p in rea.products:
				nameUpper = p.name.upper()
				if nameUpper in self.iceSpeciesList:
					self.iceSpeciesList[nameUpper]["ODE"] += " +"+rhs
				if p.name == "E" and self.useComputeElectrons: continue

				dnsCount[p.idx-1] += 1
				if dnsCount[p.idx-1] % maxDnsParts == 0:
					dns[p.idx-1] += " dn("+p.fidx+") = "
				dns[p.idx-1] = dns[p.idx-1].replace(" = 0.d0"," =")
				dns[p.idx-1] += " +"+rhs

		#load binding energies from file
		#load bare and ice binding energy from file into a dictionary (K)
		#if useSemenov load the Semenov binding energies file
		if(self.useSemenov):
			fhbind = open("data/Ebare_ice_Semenov.dat")
		else:
			fhbind = open("data/Ebare_ice.dat")

		Ebind = dict()
		for row in fhbind:
			srow = row.strip()
			if srow=="": continue
			if srow[0] == "#": continue
			Ebind_spec, Ebind_bare, Ebind_ice = [x for x in srow.split(" ") if x!=""]
			Ebind[Ebind_spec] = {"Ebare": Ebind_bare, "Eice":Ebind_ice}
		fhbind.close()


		#modify ODE if ice species are present
		for iceName, iceData in self.iceSpeciesList.items():
			for species in self.specs:
				nameUpper = species.name.upper()
				thisReacts = []
				#search for reaction where iceName is present
				for react in self.reacts:
					RP = react.reactants+react.products
					#add _TOTAL at end of species name if missing
					if not nameUpper.endswith("_TOTAL"):
						nameCheck = nameUpper+"_TOTAL"
					#check if iceName_TOTAL is in reactants+products
					if nameCheck in [x.name.upper() for x in RP]:
						thisReacts.append(react)

				#differential for the GAS phase
				if nameUpper == iceName:
					#get freeze and evaporation rates index
					idxFreeze = str(iceData["reactionFreezeout"].idx)
					idxEvaporation = str(iceData["reactionEvaporation"].idx)
					dns[species.idx-1] = "\n!"+iceName+"_GAS\n" \
						+ "dn("+species.fidx+") = dnChem_"+iceName \
						+ " -n("+species.fidx+")*(k("+idxFreeze+")+k("+idxEvaporation+"))" \
						+ " +k("+idxEvaporation+")*n("+species.fidx+"_total)"
					#if iceName has surface reactions changes GAS ODE accordingly
					for react in thisReacts:
						#build reactants multiplication
						RRs = react.reactants
						#function to write nTOTAL-nGAS
						def ndiff(myfidx):
							return "(n("+myfidx+")-n("+myfidx.replace("_total","")+"))"
						RHS = "*".join([ndiff(x.fidx) for x in RRs])
						#GAS can only get evaporated part from 2body, skip loss
						if nameUpper+"_TOTAL" in [x.name.upper() for x in react.reactants]:
							continue

						#fraction of 2body in the gas phase
						# see https://arxiv.org/pdf/1510.03218.pdf
						mamu = 130.
						protonMass = 1.6726219e-24 #g
						Mass = mamu*protonMass
						epsM = (Mass-species.mass)**2/(Mass+species.mass)**2
						dof = 3e0*sum(species.atomcount.values()) #DOF molecule
						Ebind_bare = float(Ebind[species.name]["Ebare"])
						deltaGas = "exp(-"+str(Ebind_bare) + "/" + str(epsM) \
							+ "/" + str(react.enthalpyK) \
							+ "/" + str(dof) +")"
						from math import exp
						#evaluate to check evaporation probability
						if eval(deltaGas) > 1e0:
							sys.exit("ERROR: 2body evaporation probability >1!")
						#add complete RHS to ODE
						dns[species.idx-1] += " &\n+"+deltaGas+"*k(" + str(react.idx) \
							+ ")" + "*" + RHS

				#differential for TOTAL = ice + gas
				if nameUpper == iceName + "_TOTAL":
					dns[species.idx-1] = "\n!"+iceName+"_TOTAL\n" \
						+ "dn("+species.fidx+") = dnChem_"+iceName
					#if iceName has surface reactions changes TOTAL ODE accordingly
					for react in thisReacts:
						#build reactants multiplication
						RRs = react.reactants
						#function to write nTOTAL-nGAS
						def ndiff(myfidx):
							return "(n("+myfidx+")-n("+myfidx.replace("_total","")+"))"
						RHS = "*".join([ndiff(x.fidx) for x in RRs])
						#get sign depending if iceName is reactant or product
						signRHS = "+"
						if nameUpper in [x.name.upper() for x in react.reactants]:
							signRHS = "-"
						#add complete RHS to ODE
						dns[species.idx-1] += " &\n" + signRHS \
							+ "k("+str(react.idx) +")" + "*" + RHS

		#add dust to ODEs
		if self.useDust:
			j = 0
			iType = 0
			for dType in dustTypes:
				iType += 1
				for i in range(dustArraySize):
					myndust = dustArraySize*dustTypesSize
					j += 1
					partner_mass = "krome_dust_partner_mass("+str(iType)+")"
					#******growth / sputtering******
					if self.useDustGrowth:
						dns[nmols+j-1] += " + krome_dust_growth(n(idx_"+dType+"),Tgas,krome_dust_T("+str(j)+"),"
						dns[nmols+j-1] += "vgas,"+partner_mass+",krome_grain_rho("+str(iType)+"))"
					if self.useDustSputter:
						dns[nmols+j-1] += " &\n- krome_dust_sput(Tgas,krome_dust_asize("
						dns[nmols+j-1] += str(j)+"),ntot,n("+str(nmols+j)+"))"
					if self.useDustEvap:
						dns[nmols+j-1] += " &\n- dust_evap(krome_dust_T("+str(j)+"),krome_dust_Tbind("+str(j)+")"
						dns[nmols+j-1] += ","+partner_mass+",n("+str(nmols+j)+")**2,krome_grain_rho("+str(iType)+"))"
					dns[nmols+j-1] = dns[nmols+j-1].replace("= 0.d0 +", "=")

		#find the maximum number of products and reactants
		maxnprod = maxnreag = 0
		for rea in reacts:
			maxnprod = max(maxnprod, len(rea.products))
			maxnreag = max(maxnreag, len(rea.reactants))

		print("Max number of reactants:", maxnreag)
		print("Max number of products:", maxnprod)

		#create implicit RHS arrays
		arr_rr = [[] for i in range(maxnreag)]
		arr_pp = [[] for i in range(maxnprod)]
		idxs = []
		for rea in reacts:
			if rea.idx in idxs: continue #avoid reactions with same index
			idxs.append(rea.idx)
			for i in range(maxnreag):
				if i < len(rea.reactants): arr_rr[i].append(rea.reactants[i].idx)
				else: arr_rr[i].append(dummy.idx)
			for i in range(maxnprod):
				if i < len(rea.products): arr_pp[i].append(rea.products[i].idx)
				else: arr_pp[i].append(dummy.idx)

		self.maxnreag = maxnreag
		self.maxnprod = maxnprod
		implicit_arrays = ""
		chunk_length = 10000
		for i in range(len(arr_rr)):
			chunks = int(len(arr_rr[i]) // chunk_length + 1)
			for j in range(chunks):
				cstart = chunk_length * j + 1
				cend = min(chunk_length * (j+1), len(arr_rr[i]))
				implicit_arrays += ( "arr_r{0}({1}:{2}) = (/".format(i+1,cstart,cend)
								+ (",".join([str(x) for x in arr_rr[i][cstart-1:cend]]))+"/)\n" )
		for i in range(len(arr_pp)):
			chunks = len(arr_pp[i]) // chunk_length + 1
			for j in range(chunks):
				cstart = chunk_length * j + 1
				cend   = min(chunk_length * (j+1), len(arr_pp[i]))
				implicit_arrays += ( "arr_p{0}({1}:{2}) = (/".format(i+1,cstart,cend)
								+ (",".join([str(x) for x in arr_pp[i][cstart-1:cend]]))+"/)\n" )
		self.implicit_arrays = implicit_arrays

		#wrap RHS (e.g. knn+knn=2*knn)
		dnw = []
		idn = 0
		ldns = dns
		for dn in ldns:
			if idn > nmols - 1:
				dnw.append(dn)
				continue
			RHSs = []
			RHSc = []
			dnspl =  dn.strip().split(" ")
			dns = (" ".join(dnspl[:2]))
			for p in dnspl[2:]:
				if p not in RHSs:
					RHSs.append(p)
					RHSc.append(0)
				RHSc[RHSs.index(p)] += 1
			for i in range(len(RHSs)):

				#if((i+1) % 4 == 0): dns += "&\n" #break long lines
				if "-" in RHSs[i] and RHSc[i] > 1:
					dns += RHSs[i].replace("-"," &\n-"+str(RHSc[i])+".d0*")
				if "+" in RHSs[i] and RHSc[i] > 1:
					dns += RHSs[i].replace("+"," &\n+"+str(RHSc[i])+".d0*")
				if RHSc[i] == 1: dns+= " &\n"+RHSs[i]
			dns = dns.replace("&\n&\n","&\n")
			dns = dns.replace("&\ndn(","\n\ndn(")
			dns = dns.replace("&\n= &\n &","= &")
			#dns = dns.replace("*"," * ")
			dnw.append(dns)
			idn += 1
		#dnw = [x.replace("+"," &\n+").replace("-"," &\n-") for x in ldns]

		self.dnw = dnw

	###############################################
	#build the Jacobian
	def createJAC(self):
		reacts = self.reacts
		specs = self.specs
		Tgas_species = self.Tgas_species
		ndust = self.dustArraySize * self.dustTypesSize
		nmols = self.nmols
		#create explicit JAC for chemical species
		neq = len(specs)
		jac = [["pdj("+str(j+1)+") = 0.d0" for i in range(neq)] for j in range(neq)] #init jacobian
		jsparse = [[0 for i in range(neq)] for j in range(neq)] #sparsity matrix
		idxs = [] #store reaction indexes
		for rea in reacts:
			if rea.idx in idxs: continue #skip reactions with same index
			idxs.append(rea.idx) #store reaction index
			#loop over reactants
			for ri in range(len(rea.reactants)):
				if rea.curlyR[ri] and self.useNuclearMult: continue #skip curly reactants
				sjac = rea.nuclearMult+"k("+str(rea.idx)+")" #init and include nuclearMulteplicity if any
				r1 = rea.reactants[ri] #get ri-th reactant
				#loop over reactants again
				for rj in range(len(rea.reactants)):
					if rea.curlyR[rj] and self.useNuclearMult: continue #skip curly reactants
					r2 = rea.reactants[rj] #get rj-th reactant
					#if reactants are different add rj-th to jacobian
					if ri != rj:
						sjac += "*n("+str(r2.fidx)+")"
				#update built jacobian and sparsity for reactants
				for rr in rea.reactants:
					jac[rr.idx-1][r1.idx-1] = jac[rr.idx-1][r1.idx-1].replace(" = 0.d0"," =")
					jac[rr.idx-1][r1.idx-1] += " &\n-"+sjac
					jsparse[rr.idx-1][r1.idx-1] = 1
				#update built jacobian and sparsity for products
				for pp in rea.products:
					jac[pp.idx-1][r1.idx-1] = jac[pp.idx-1][r1.idx-1].replace(" = 0.d0"," =")
					jac[pp.idx-1][r1.idx-1] += " &\n+"+sjac
					jsparse[pp.idx-1][r1.idx-1] = 1

		#this part compress jacobian terms (e.g. k1*H*H+k1*H*H => 2*k1*H*H)
		jacnew = [["" for i in range(neq)] for j in range(neq)] #create empty jacobian neq*neq
		#loop over jacobian
		for i in range(neq):
			for j in range(neq):
				jcount = dict() #dictionary to count items
				jpz = jac[i][j] #jacobian element
				ajpz = [x.strip() for x in jpz.split("&\n")] #split on returns
				#loop over pieces and count (first piece is initialization)
				for pz in ajpz[1:]:
					if pz in jcount:
						jcount[pz] += 1 #already found, add 1
					else:
						jcount[pz] = 1 #newly found, init to 1
				jacel = ajpz[0] #element start
				#loop on counted pieces
				for (k,v) in jcount.items():
					if(v>1):
						jacel += " "+k[0]+str(v)+".d0*"+k[1:] #add multiplication factor
					else:
						jacel += " "+k #add piece
				#print jacel
				jacel = jacel.replace("+"," &\n +").replace("-"," &\n -") #new line for each term
				jacnew[i][j] = jacel #update element into the jacobian
		jac = jacnew #replace old jacobian

		#create approximated Jacobian term for Tgas
		for i in range(neq):
			if not self.use_thermo: break
			jsparse[i][Tgas_species.idx-1] = jsparse[Tgas_species.idx-1][i] = 1
			s = str(i+1)
			if specs[i].name != "dummy" and specs[i].name != "CR" and specs[i].name != "g":
				jac[Tgas_species.idx-1][i] = "dn0 = (heating(n(:), Tgas, k(:), nH2dust) - cooling(n(:), Tgas)) &\n"
				jac[Tgas_species.idx-1][i] += "* (krome_gamma - 1.d0) / boltzmann_erg / sum(n(1:nmols))\n"
				jac[Tgas_species.idx-1][i] += "nn(:) = n(:)\n"
				if self.deltajacMode == "RELATIVE":
					jac[Tgas_species.idx-1][i] += "dnn = n("+s+")*"+str(self.deltajac)+"\n"
				elif self.deltajacMode == "ABSOLUTE":
					jac[Tgas_species.idx-1][i] += "dnn = "+str(self.deltajac)+"\n"
				else:
					die("ERROR: deltajacMode unknonw!"+self.deltajacMode)
				jac[Tgas_species.idx-1][i] += "if(dnn>0.d0) then\n"
				jac[Tgas_species.idx-1][i] += " nn("+s+") = n("+s+") + dnn\n"
				jac[Tgas_species.idx-1][i] += " dn1 = (heating(nn(:), Tgas, k(:), nH2dust) - cooling(nn(:), Tgas)) &\n"
				jac[Tgas_species.idx-1][i] += "  * (krome_gamma - 1.d0) / boltzmann_erg / sum(n(1:nmols))\n"
				jac[Tgas_species.idx-1][i] += " pdj(idx_Tgas) = (dn1-dn0)/dnn\n"
				jac[Tgas_species.idx-1][i] += "end if\n"

				#jac[Tgas_species.idx-1][i] = "if(abs(n("+s+") - jac_nold(" + s
				#jac[Tgas_species.idx-1][i] += "))>1d-10) pdj(idx_Tgas) = (jac_dn(idx_Tgas) - jac_dnold(idx_Tgas)) / (n("
				#jac[Tgas_species.idx-1][i] += s + ") - jac_nold(" + s + "))"

		self.jac = jac
		self.jsparse = jsparse

	#######################################
	#this function computes the sparsity structure in the yale format
	# namely IA e JA in the DLSODES documentation
	#Do the same for dvodeF90
	def IACJAC(self):
		neq = len(self.specs)
		jsparse = self.jsparse
		#create IAC/JAC (see DLSODES manual)
		isum = 1
		ia = [1]
		ja = []
		for i in range(neq):
			isum += sum(jsparse[i])
			ia.append(isum)
			for j in range(neq):
				if jsparse[i][j] == 1: ja.append(j+1)
		nnz = len(ja)
		iaf = "IWORK(31:" + str(30+neq+1) + ") = (/" + (",".join([str(x) for x in ia])) + "/)"
		jaf = "IWORK("+str(31+neq+1)+":"+str(31+neq+nnz)+") = (/"+(",".join([str(x) for x in ja]))+"/)"

		pnnz = round(nnz*100./neq/neq,2)
		print("")
		print("Jacobian non-zero elements:",nnz,"over",neq*neq)
		print("("+str(pnnz)+"% of total elements, sparsity = "+str(100.-pnnz)+"%)")

		#sparsity for dvodeF90
		jaca = [] #unrolled sparse jacobian
		if self.useDvodeF90:
			ia = [1] #sparsity structure IA (see dvode_f90 manual)
			ja = [] #sparsity structure JA (see dvode_f90 manual)
			isum = 1
			for i in range(neq):
				isum += sum(jsparse[i])
				ia.append(isum)
				for j in range(neq):
					if jsparse[i][j] == 1:
						ja.append(j+1)
						jaca.append(self.jac[i][j].replace("pdj("+str(i+1)+") =  &\n","").strip())
			iaf = "iauser(:) = (/" + (",".join([str(x) for x in ia])) + "/)"
			jaf = "jauser(:) = (/"+(",".join([str(x) for x in ja]))+"/)"


		#if MF=222 no need for sparsity structure arrays
		if self.solver_MF == 222:
			iaf = jaf = ""
		self.ja = ja
		self.ia = ia
		self.iaf = iaf
		self.jaf = jaf
		self.jaca = jaca

	####################################
	def solverParams(self):
		specs = self.specs
		reacts = self.reacts

		solver_MF = self.solver_MF
		solver_moss = int(solver_MF/100)
		solver_meth = int(solver_MF/10) % 10
		solver_miter = solver_MF % 10

		print("solver info:")
		print(" MF:",solver_MF)
		print(" MOSS+METH+MITER:","+".join([str(x)
											for x in [solver_moss,solver_meth,solver_miter]]))


		#estimate size of RWORK array (see DLSODES manual)
		neq = len(specs) #number of equations
		nrea = self.nrea #number of reactions
		lenrat = 2
		nnz_estimate = pow(neq,2) - nrea #estimate non-zero elements (inaccurate)
		nnz = len(self.ja)
		nnz = max(nnz, nnz_estimate) #estimate may be more realistic than actual nnz calculation!
		if nnz<0:
			die("ERROR: nnz<0, please check RWORK size estimation!")

		#calculate LWM
		if solver_miter == 0:
			lwm = 0
		elif solver_miter == 1:
			lwm = 2*nnz + 2*neq + (nnz+9*neq) / lenrat
		elif solver_miter == 2:
			lwm = 2*nnz + 2*neq + (nnz+10*neq) / lenrat
		elif solver_miter == 3:
			lwm = neq + 2
		elif solver_miter == 7:
			lwm = 0
		else:
			die("ERROR: solver_miter value "+str(solver_miter)+" unknown!")

		#calculate LRW as in DLSODES documentation
		if solver_MF == 10:
			lrw = 20+16*neq
		elif solver_MF in [11, 111, 211, 12, 112, 212]:
			lrw = 20+16*neq+lwm
		elif solver_MF == 13:
			lrw = 20+17*neq
		elif solver_MF == 20:
			lrw = 20+9*neq
		elif solver_MF in [21, 121, 221, 22, 122, 222]:
			lrw = 20+9*neq+lwm
		elif solver_MF == 23:
			lrw = 22+10*neq
		elif solver_MF == 227:
			lrw = 0
		elif solver_MF == 27:
			lrw = 0
		else:
			die("ERROR: solver_MF value "+str(solver_MF)+" unknown in LRW calculation!")

		if self.force_rwork:
			lrw = self.myrwork
		#lrw = int(20+(2+0.5)*nnz + (11+4.5)*neq) #RWORK size

		print(" LWM:", lwm, "LRW:", lrw)
		self.lwm = lwm
		self.lrw = lrw

	###################################
	def convertMetal2Roman(self,arg_metal):
		if "+" in arg_metal:
			return arg_metal.replace("+","") + int_to_roman(arg_metal.count("+")+1)
		elif "-" in arg_metal:
			return arg_metal.replace("-","") + "m"+int_to_roman(arg_metal.count("-")+1)
		else:
			return arg_metal+"I" #is not an ion

	###################################
	def convertMetal2F90(self,arg_metal):
		if "+" in arg_metal:
			return arg_metal.replace("+","j")
		elif "-" in arg_metal:
			return arg_metal.replace("-","k")
		else:
			return arg_metal #is not an ion

	#####################################
	#alternative for reading cooling data from file
	def createZcooling(self):
		cooling_data = dict() #structure with all the data
		index_count = 0 #1-based index for all the rates of the cooling
		TgasUpperLimit = TgasLowerLimit = "" #temperature limits for ALL the reactions (empty=no limits)
		#PART1: read the data from file
		for fname in self.coolFile:
			#read the file fname
			skip_metal = False #flag to skip metals not included in the self.zcoolants list
			print("******************")
			print("Reading coolants from "+fname+"...")
			fh = open(fname) #open file
			#loop on file lines
			for row in fh:
				srow = row.strip()
				#skip blank lines and comments
				if srow == "": continue
				if srow[0] == "#": continue
				if srow[:2] == "//": continue
				#read metal name from file and init data structures
				if "metal:" in srow:
					srow = srow.replace("metal:", "").strip()
					mol = parser(srow,self.mass_dic, self.atoms, self.thermodata)
					metal_name = mol.coolname
					metal_name_f90 = self.convertMetal2F90(srow) #metal name in f90 format (e.g. C+ as Cj)
					levels_data = dict() #data of the levels (energy, degeneration), key=level number
					trans_data = dict() #transition data (up, down, Aij), key=up->down (e.g. "4->2")
					rate_data = dict() #rate data (up,dowm,rate,collider), key=up->down_coolider (e.g. "4->2_H")
					skip_metal = False
					needOrthoPara = False #need extra variables for ortho/para H2
					#check if the metal name is in the self.zcoolant list to skip it
					if metal_name.upper() not in [x.upper() for x in self.zcoolants]:
						skip_metal = True
					continue
				if skip_metal: continue #skip the metal if necessary (not included in the self.zcoolant list)

				#if level pragma found read level data
				if "level" in srow:
					srow = srow.replace("level","").replace(":",",") #use only comma to separate and remove "level"
					arow = [x.replace("d","e") for x in srow.split(",")] #split using comma and floating format conversion

					#skip if levels are not requested (-coolLevels)
					cond1 = (len(self.coolLevels)!=0) #condition1: the list of the requested level should be not empty
					cond2 = not(int(arow[0]) in self.coolLevels) #condition2: the level should be in the list
					if(cond1 and cond2): continue #skip if levels are not listed in requested levels

					#add level data to the dictionary
					levels_data[int(arow[0])] = {"energy":float(arow[1]), "g":float(arow[2])}
					continue

				#if 3 elemets is transistion data
				if "->" in srow:
					srow = srow.replace("->",",") #replace the arrow with a comma (easier to split)
					arow = [x.replace("d","e") for x in srow.split(",")] #split using comma and floating format conversion
					trans_name = arow[0].strip()+"->"+arow[1].strip() #prepare the key as up->down (e.g. "4->1")
					#skip if levels are not requested (-coolLevels)
					cond1 = (len(self.coolLevels)!=0) #condition1: the list of the requested level should be not empty
					cond2 = int(arow[0]) not in self.coolLevels #condition2: the up level should be in the list
					cond3 = int(arow[1]) not in self.coolLevels #condition3: the low level should be in the list
					if cond1 and (cond2 or cond3): continue #skip if levels are not listed in requested levels

					#store data
					kboltzmann_eV = 8.617332478e-5 #eV/K
					hplanck_eV = 4.135667516e-15 #eV*s
					clight =  2.99792458e10 #cm/s
					de_eV = kboltzmann_eV * abs(levels_data[int(arow[0])]["energy"] - levels_data[int(arow[1])]["energy"])
					#if wavelength in angstrom is available and positive use it to compute delta energy
					if len(arow) > 3:
						wvl = float(arow[3])/1e8 #cm
						de_eV = 0e0
						if wvl > 0e0:
							de_eV = hplanck_eV*clight/wvl #eV

					#compute pre-factor for photo-induced transitions
					if de_eV != 0e0:
						preB = .5e0*(hplanck_eV*clight)**2/(de_eV)**3 #cm2/eV
					else:
						preB = 0e0

					#compute gi and gj
					gi = levels_data[(int(arow[0]))]["g"]
					gj = levels_data[(int(arow[1]))]["g"]

					#append transition dictionary to transition data
					trans_data[trans_name] = {"up":int(arow[0]), "down":int(arow[1]), "Aij":float(arow[2]),\
						"Bij":float(arow[2])*preB, "denergy_eV":de_eV, "denergy_K":de_eV/kboltzmann_eV,\
						"Bji":float(arow[2])*preB/gj*gi}
					continue

				#if more than 3 elements is a rate data
				if len(srow.split(",")) > 3:
					arow = srow.split(",")
					arow[3] = (",".join(arow[3:])) #join the last element to cope with comma separated f90 functions
					#key for the transition as up->down_collider (e.g. "6->1_H")
					trans_name = arow[1].strip()+"->"+arow[2].strip()+"_"+arow[0].strip()

					#skip if levels are not requested (-coolLevels)
					cond1 = (len(self.coolLevels)!=0) #condition1: the list of the requested level should be not empty
					cond2 = int(arow[1]) not in self.coolLevels #condition2: the up level should be in the list
					cond3 = int(arow[2]) not in self.coolLevels #condition3: the low level should be in the list
					if cond1 and (cond2 or cond3): continue #skip if levels are not listed in requested levels

					#increase the number of the reactions found
					index_count += 1

					#store the reaction rate in the list
					rate_comment = "!"+arow[1].strip()+"->"+arow[2].strip()+", "+metal_name+" - "+arow[0].strip()+"\n"
					self.coolZ_rates.append(rate_comment+"k("+str(index_count)+") = "+arow[3].strip())

					#store data
					rate_data[trans_name] = {"up":int(arow[1]), "down":int(arow[2]),
											 "collider":arow[0].strip(), "rate":index_count}

					#check if para/ortho needed
					if arow[0].strip() == "H2or" or arow[0].strip() == "H2pa":
						needOrthoPara = True

					continue

				#check for if conditions on the rate (note that index_count is not incremented)
				if srow[:3].lower() == "if(":
					arow = srow.split(":")
					self.coolZ_rates.append(arow[0].strip()+" k("+str(index_count)+") = "+arow[1].strip())
					continue

				#read @var
				if "@var:" in srow:
					srow = srow.replace("@var:", "").strip()
					#read extra variables as [variable_name, expression]
					self.coolZ_vars_cool.append([x.strip() for x in srow.split("=")])
					continue

				#look for @TgasUpperLimit
				if "@tgasupperlimit:" in srow.lower():
					TgasUpperLimit = srow.split(":")[1].strip()
					#append the upper limit for the temperature as the first reaction rate
					self.coolZ_rates.append("if(Tgas.ge."+TgasUpperLimit+") return\n")

				#end of data, hence store
				if ("endmetal" in srow) or ("end metal" in srow):
					#store all the data for the given metal_name
					cooling_data[metal_name] = {"levels_data":levels_data, "trans_data":trans_data, "rate_data":rate_data,\
						"metal_name_f90":metal_name_f90, "needOrthoPara":needOrthoPara}
					#pprint(cooling_data)


		#PART2: use data to prepare cooling routine
		#prepare the functions for the cooling looping on metals (which are the key of the cooling_data dictionary)
		for cur_metal, cool_data in cooling_data.items():
			metal_name = cur_metal #alias for metal name
			metal_name_f90 = cool_data["metal_name_f90"] #name in f90 style
			level_list = list(cool_data["levels_data"].keys()) #store the list of the levels as integer values (e.g. [0,1,3])
			#make a local copy of the dictionaries (easy to handle)
			levels_data = cooling_data[cur_metal]["levels_data"]
			trans_data = cooling_data[cur_metal]["trans_data"]
			rate_data = cooling_data[cur_metal]["rate_data"]
			rate_data_rev = dict() #init a dictionary to store the inverse reactions
			print("Prepearing "+cur_metal+" (levels: "+str(len(levels_data))
				  +", transitions: "+str(len(rate_data))+")")

			#PART2.1: prepare excitation rates from de-excitations
			#loop on rates to prepare the reverse (excitation rates)
			for trans_name, r_data in rate_data.items():
				#inverse transition name
				trans_name_rev = str(r_data["down"])+"->"+str(r_data["up"])+"_"+r_data["collider"]
				g_up = levels_data[r_data["up"]]["g"] #upper level
				g_down = levels_data[r_data["down"]]["g"] #lower level
				#delta energy Ej-Ei
				deltaE = abs(levels_data[r_data["up"]]["energy"] - levels_data[r_data["down"]]["energy"])
				deltaE = ("%e" % deltaE).replace("e","d") #f90ish format for deltaE
				#increase the number of the reactions found
				index_count += 1
				#store the size of the k(:) array
				self.coolZ_nkrates = index_count
				#build reverse as Rji = Rij*gi/gj*exp(-deltaE/T)
				rate_comment = "!"+str(r_data["up"])+"<-"+str(r_data["down"])+", "+metal_name+" - "+r_data["collider"]+"\n"
				myrate = "k("+str(r_data["rate"])+") * "+str(float(g_up)/float(g_down))+"d0 * exp(-"+deltaE+" * invT)"
				self.coolZ_rates.append(rate_comment+"k("+str(index_count)+") = " + myrate)
				#store the data for the reverse reaction
				rate_data_rev[trans_name_rev] = {"rate": index_count, "collider":r_data["collider"], "up":r_data["down"],\
					"down":r_data["up"]}

			#merge excitation and de-excitation dictionary
			rate_data = {**rate_data, **rate_data_rev}

			idx_linear_dep_level = 0 #ground level will be removed (for linear dependency)

			#PART 2.2: prepare A matrix for Ax=B system
			#loop on the number of levels (k index, lines of the A matrix)
			nlev = max(level_list)
			collider_list = [] #list of the collider found
			Amatrix = [["0d0" for i in range(nlev+1)] for j in range(nlev+1)] #init matrix to 0d0
			for klev in level_list[:]:
				#if level is suitable for linear depenency use as conservation equation
				if klev == idx_linear_dep_level:
					Amatrix[klev] = ["1d0" for i in range(nlev+1)]
					continue
				#loop on the number of levels (i index, i<->j)
				for ilev in level_list:
					#loop on the number of levels (j index, i<->j)
					for jlev in level_list:
						#no transitions from the same level
						if ilev == jlev: continue
						#k->j transitions (de-populate k level: Akj, Ckj)
						if klev == ilev:
							A_name = "A("+str(klev+1)+","+str(ilev+1)+")" #Ax=b matrix element name
							trans_name = str(ilev)+"->"+str(jlev) #transition key k->j
							#compute excitation Cij
							#loop on rate data to find keys that contain the transition name (rates k->j)
							for r_key in rate_data:
								r_key_part = r_key.split("_")[0]
								if trans_name == r_key_part:
									matrix_rate = " &\n- k("+str(rate_data[r_key]["rate"])+") * coll_"+\
										self.convertMetal2F90(rate_data[r_key]["collider"])
									Amatrix[klev][ilev] += matrix_rate
									collider_list.append(rate_data[r_key]["collider"])
							#search transitions k->j (Aij)
							if trans_name in trans_data:
								Aij_fmt = ("%e" % trans_data[trans_name]["Aij"]).replace("e","d")
								matrix_rate = " &\n- " + Aij_fmt
								Amatrix[klev][ilev] += matrix_rate
								#if photoinduced needed compute it
								if self.usePhotoInduced and trans_data[trans_name]["Bij"] > 0e0:
									Bij_fmt = ("%e" % trans_data[trans_name]["Bij"]).replace("e","d")
									de_eVs = ("%e" % trans_data[trans_name]["denergy_eV"]).replace("e","d")
									matrix_rate = " &\n- "+Bij_fmt+" * get_photoIntensity("+de_eVs+")"
									Amatrix[klev][ilev] += matrix_rate

						#i->k transitions (populate k level: Aik, Cik)
						if klev == jlev:
							A_name = "A("+str(klev+1)+","+str(ilev+1)+")" #Ax=b matrix element name
							trans_name = str(ilev)+"->"+str(jlev) #transition key i->k
							#add collision, i.e. Cij
							#loop on rate data to find keys that contain the transition name (rates i->k)
							for r_key in rate_data:
								r_key_part = r_key.split("_")[0] #key without _collider (e.g. 2->1)
								if trans_name == r_key_part:
									matrix_rate = " &\n+ k("+str(rate_data[r_key]["rate"])+") * coll_"+\
										self.convertMetal2F90(rate_data[r_key]["collider"])
									Amatrix[klev][ilev] += matrix_rate
									collider_list.append(rate_data[r_key]["collider"])
							#add transition, i.e. Aij
							#search transitions i->k
							if trans_name in trans_data:
								Aij_fmt = ("%e" % trans_data[trans_name]["Aij"]).replace("e","d")
								matrix_rate = " &\n+ "+Aij_fmt
								Amatrix[klev][ilev] += matrix_rate
								#if photoinduced needed compute it
								if self.usePhotoInduced and trans_data[trans_name]["Bij"] > 0e0:
									Bij_fmt = ("%e" % trans_data[trans_name]["Bij"]).replace("e","d")
									de_eVs = ("%e" % trans_data[trans_name]["denergy_eV"]).replace("e","d")
									matrix_rate = " &\n+ "+Bij_fmt+" * get_photoIntensity("+de_eVs+")"
									Amatrix[klev][ilev] += matrix_rate

			#PART 2.3: prepare the cooling
			full_B_vector_cool = []
			full_B_vector_heat = []
			for k,t_data in trans_data.items():
				Aij_fmt = ("%e" % t_data["Aij"]).replace("e","d") #f90ish format for Aij
				deltaE = t_data["denergy_K"]
				deltaE_fmt = ("%e" % deltaE).replace("e","d") #f90ish format for deltaE
				photoIB = ""
				if self.usePhotoInduced and t_data["Bij"] > 0e0:
					Bij_fmt = ("%e" % t_data["Bij"]).replace("e","d")
					de_eVs = ("%e" % t_data["denergy_eV"]).replace("e","d")
					photoIB = " &\n + "+Bij_fmt+" * get_photoIntensity("+de_eVs+")"
				#append cooling to final sum over transitions list
				full_B_vector_cool.append("B("+str(t_data["up"]+1)+") * ("+Aij_fmt + photoIB +") * "+deltaE_fmt)

				#add induced heating if needed
				if self.usePhotoInduced and t_data["Bij"] > 0e0:
					Bji_fmt = ("%e" % t_data["Bji"]).replace("e","d")
					de_eVs = ("%e" % t_data["denergy_eV"]).replace("e","d")
					photoIB = Bji_fmt+" * get_photoIntensity("+de_eVs+")"
					full_B_vector_heat.append("B("+str(t_data["up"]+1)+") * "+photoIB +" * "+deltaE_fmt)

			if len(full_B_vector_cool) == 0 and len(full_B_vector_heat) ==0:
				error = "ERROR: not enough data to write "+metal_name+"cooling!"
				error += " (trans data size:"+str(len(trans_data))+")"
				sys.exit(error)

			#join the cooling vector as a sum
			full_B_vector = (" &\n + ".join(full_B_vector_cool))
			if len(full_B_vector_heat) > 0:
				full_B_vector += " &\n - " + (" &\n - ".join(full_B_vector_heat))


			#uniqe collider_list
			ucollider_list = []
			for x in collider_list:
				#replace for f90-friendly name
				x = x.replace("+","j").replace("-","k")
				if x in ucollider_list: continue
				ucollider_list.append(x)
			collider_list = ucollider_list


			#PART 2.4: prepare the function for cooling
			nlev = max(levels_data)+1 #maximum number of levels
			function_name = "cooling"+metal_name #name of the function
			full_function = "!************************************\n"
			full_function += "function "+function_name+"(n,inTgas,k)\n"
			full_function += "use krome_commons\n"
			full_function += "use krome_photo\n"
			full_function += "use krome_subs\n"
			full_function += "implicit none\n"

			#declaration of varaibles
			full_function += "integer::i, hasnegative, nmax\n"
			full_function += "real*8::"+function_name+",n(:),inTgas,k(:)\n"
			full_function += "real*8::A("+str(nlev)+","+str(nlev)+"),Ain("+str(nlev)+","+str(nlev)+")\n"
			full_function += "real*8::B("+str(nlev)+"),tmp("+str(nlev)+")\n"

			#declaration of alias variable for colliders
			for x in collider_list:
				full_function += "real*8::coll_"+x+"\n"

			full_function += "\n!colliders should be >0\n"

			#initialization of colliders (replace ortho/para H2)
			for x in collider_list:
				xn = "n(idx_"+x+")"
				if x == "H2or": xn = "n(idx_H2) * phys_orthoParaRatio / (phys_orthoParaRatio+1d0)"
				if x == "H2pa": xn = "n(idx_H2) / (phys_orthoParaRatio+1d0)"
				full_function += "coll_"+x+" = max("+xn+", 0d0)\n" #collider must be positive

			full_function += "\n!deafault cooling value\n"
			full_function += function_name +" = 0d0\n\n" #default
			full_function += "if(n(idx_"+metal_name_f90+")<1d-15) return\n\n" #if low coolant abundance skip all
			full_function += "A(:,:) = 0d0\n\n" #init A matrix to zero

			#write the initialization of first column of the A matrix
			# (will be used by the f90 to reduce the size of the problem)
			for j in range(nlev):
				if Amatrix[j][0] != "0d0":
					matrix_element = Amatrix[j][0].replace("0d0 &\n","")
					full_function += "A("+str(j+1)+",1) = "+matrix_element+"\n"

			#write the initialization of diagonal elements of the A matrix
			# (will be used by the f90 to reduce the size of the problem)
			for j in range(1,nlev):
				if Amatrix[j][j] != "0d0":
					matrix_element = Amatrix[j][j].replace("0d0 &\n","")
					full_function += "A("+str(j+1)+","+str(j+1)+") = "+matrix_element+"\n"


			#the size of the problem can be reduced up to the last non-zero row of the left triangular matrix
			# only interesting with more levels
			if nlev > 3:
				full_function += "\n!reduce the size of the problem if possible\n" #reverse is faster
				full_function += "nmax = 1\n"
				full_function += "do i="+str(nlev)+",2,-1\n"
				full_function += " if(A(i,1)>0d0) then\n"
				full_function += "  nmax = i\n" #store new size of the problem
				full_function += "  exit\n" #break loop
				full_function += " end if\n"
				full_function += "end do\n\n"

				#control if the problem is not 1-level
				full_function += "!no need to solve a 1-level problem\n"
				full_function += "if(nmax==1) return\n\n"


			#write the A matrix column-wise. A(:,1) matrix column computed above
			# as well as the diagonal elements
			for i in range(1,nlev):
				for j in range(nlev):
					#skip diagonal elements since already written (see above)
					if i == j: continue
					#skip zero elements
					if Amatrix[j][i] != "0d0":
						matrix_element = Amatrix[j][i].replace("0d0 &\n","")
						full_function += "A("+str(j+1)+","+str(i+1)+") = "+matrix_element+"\n"

			full_function += "\n!build matrix B\n"
			full_function += "B(:) = 0d0\n" #default B matrix
			full_function += "B("+str(idx_linear_dep_level+1)+") = n(idx_"+metal_name_f90+")\n\n" #conservation equation RHS
			full_function += "Ain(:,:) = A(:,:)\n\n" #store initial matrix for debug purposes

			#choose the correct solver depending on the number of levels
			# i.e. 2 and 3 algebric, while >3 LAPACK
			if nlev > 3:
				full_function += "call mydgesv(nmax, A(:,:), B(:), \""+function_name+"\")\n\n"
				self.needLAPACK = True
			elif nlev == 2:
				full_function += "call mylin2(A(:,:), B(:))\n\n"
			elif nlev == 3:
				full_function += "call mylin3(A(:,:), B(:))\n\n"
			else:
				sys.exit("ERROR: strange number of levels for linear system in Zcooling: "+str(nlev))

			full_function += "!store population\n"
			full_function += "pop_level_"+metal_name+"(:) = B(:)\n"

			#negative small values can be flushed to 1d-40
			full_function += "!sanitize negative values\n"
			full_function += "hasnegative = 0\n"
			full_function += "do i=1,"+str(nlev)+"\n"
			full_function += " if(B(i)<0d0) then\n"
			full_function += "  if(abs(B(i)/n(idx_"+metal_name_f90+"))>1d-10) then\n"
			full_function += "   hasnegative = 1\n"
			full_function += "  else\n"
			full_function += "   B(i) = 1d-40\n"
			full_function += "  end if\n"
			full_function += " end if\n"
			full_function += "end do\n\n"

			#when negative large value are found print some stuff and stop
			full_function += "!check if B has negative values\n"
			full_function += "if(hasnegative>0)then\n"
			full_function += " print *,\"ERROR: minval(B)<0d0 in "+function_name+"\"\n"
			full_function += " print *,\"ntot_"+metal_name+" =\", n(idx_"+metal_name_f90+")\n"
			full_function += " print *,\"Tgas =\", inTgas\n"
			full_function += " print *,\"B(:) unrolled:\"\n"
			full_function += " do i=1,size(B)\n"
			full_function += "  print *, i, B(i)\n"
			full_function += " end do\n"
			full_function += " print *,\"A(:,:) min/max:\"\n"
			full_function += " do i=1,size(B)\n"
			full_function += "  print *, i, minval(Ain(i,:)), maxval(Ain(i,:))\n"
			full_function += " end do\n\n"
			full_function += " print *,\"A(:,:)\"\n"
			full_function += " do i=1,size(B)\n"
			full_function += "  tmp(:) = Ain(i,:)\n"
			full_function += "  print '(I5,99E17.8)', i, tmp(:)\n"
			full_function += " end do\n"
			full_function += " stop\n"
			full_function += "end if\n\n"

			#when the population for each level is known compute the cooling (see above)
			full_function += function_name + " = " +full_B_vector+"\n\n"
			full_function += "end function "+function_name+"\n\n"

			#append the function to the list of the functions
			self.coolZ_functions.append([function_name,full_function])
			self.coolZ_poplevelvars.append("pop_level_"+metal_name+"("+str(nlev)+")")

	#######################################
	def prepareBuild(self):
		buildFolder = self.buildFolder
		#create build folder if not exists
		if not os.path.exists(buildFolder):
			os.mkdir(buildFolder)
			print("Created " + buildFolder)
		#remove everything
		if self.cleanBuild:
			clear_dir(buildFolder) #clear the build directory
			print("Deleted all contents in " + buildFolder)
		#remove only selected krome files
		else:
			underFiles = ["commons","constants","cooling","dust","ode","reduction","subs","tabs","user",]
			delFiles = [buildFolder+"krome_"+x+".f90" for x in underFiles] + [buildFolder+x for x in ["krome.f90","opkda1.f","opkda2.f"]]
			delFiles += glob.glob(buildFolder+"*~") + glob.glob(buildFolder+"*.mod")
			delFiles += glob.glob(buildFolder+"*_genmod.f90") + glob.glob(buildFolder+"*.i90")
			#for dfile in delFiles:
				#print "deleting "+dfile

		print("")
		print("Prepearing files in /build...")

		#build all the modules in a single file krome_all.f90
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","w")
			fout.close()

	################################################
	def makeCommons(self):

		reacts = self.reacts
		specs = self.specs
		buildFolder = self.buildFolder
		#*********COMMONS****************
		#write parameters in krome_commons.f90
		print("- writing krome_commons.f90...",)
		fh = open(self.srcFolder+"krome_commons.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_commons.f90","w")

		#common dust optical variables
		optVariables = ""
		if self.useDust:
			for dType in self.dustTypes:
				optVariables += "real*8,allocatable::dust_opt_asize_"+dType+"(:), dust_opt_nu_"
				optVariables += dType+"(:),dust_opt_Qabs_"+dType+"(:,:)\n"
				optVariables += "real*8,allocatable::dust_opt_Em_"+dType+"(:,:),dust_opt_Tbb_"+dType+"(:)\n"

		#get the list of all the atoms contained in the species, H,C,O,...
		skipAtoms = ["+","-"]
		atoms = []
		for x in specs:
			atoms += x.atomcount.keys()
		atoms = list(set(atoms))
		#skip atoms in skipAtoms list and every atom starting with underscore
		atoms = [x for x in atoms if(not(x in skipAtoms) and not(x.startswith("_")))]

		#common variables
		skip = False
		for row in fh:
			srow = row.strip()

			if srow == "#IFKROME_useChemisorption" and not self.useChemisorption: skip = True
			if srow == "#IFKROME_useSemenov" and not self.useSemenov: skip = True
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_usePreDustExp" and not((self.usedTdust or self.useDustT)
				and self.useSurface): skip = True
			if srow == "#IFKROME_useOmukaiOpacity" and self.H2opacity != "OMUKAI": skip = True
			if srow == "#IFKROME_useMayerOpacity" and not(self.usedTdust or self.useDustT): skip = True
			if srow == "#IFKROME_useCoolingCO" and not self.useCoolingCO: skip = True
			if srow == "#IFKROME_useCoolingOH" and not self.useCoolingOH: skip = True
			if srow == "#IFKROME_useCoolingH2O" and not self.useCoolingH2O: skip = True
			if srow == "#IFKROME_useCoolingHCN" and not self.useCoolingHCN: skip = True
			if srow == "#IFKROME_useCoolingZCIE" and not self.useCoolingZCIE: skip = True
			if srow == "#IFKROME_useCoolingZCIENOUV" and not self.useCoolingZCIENOUV: skip = True
			if srow == "#IFKROME_useCoolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_hasStoreOnceRates" and not self.hasStoreOnceRates: skip = True
			if srow == "#IFKROME_dust_table_2D" and not (self.dustTableDimension=="2D"): skip = True
			if srow == "#IFKROME_dust_table_3D" and not (self.dustTableDimension=="3D"): skip = True

			if srow == "#ENDIFKROME": skip = False

			if skip: continue

			if srow == "#KROME_species_index":
				for sp in specs:
					nameLower = sp.name.lower()
					if nameLower.endswith("_total") and self.doRamsesTH:
						gasSpecies = [x for x in specs if(x.name.lower()+"_total"==nameLower)][0]
						fout.write("\tinteger,parameter::" + gasSpecies.fidx + "_ice = " + str(sp.idx) +"\t!"+gasSpecies.name+"_ice\n")
						fout.write("\tinteger,parameter::" + gasSpecies.fidx + "_gas = " + str(gasSpecies.idx) +"\t!"+gasSpecies.name+"_gas\n")
					fout.write("\tinteger,parameter::" + sp.fidx + "=" + str(sp.idx) + "\n")
			if srow == "#KROME_atom_index" and self.useConserveLin:
					for atom in atoms:
						fout.write("integer,parameter::idx_atom_" + atom + "=" + str(atoms.index(atom)+1) + "\n")
			elif srow == "#KROME_parameters":
					ndust = self.dustArraySize*self.dustTypesSize
					fout.write("\tinteger,parameter::nrea=" + str(self.nrea) + "\n")
					fout.write("\tinteger,parameter::nmols=" + str(self.nmols) + "\n")
					fout.write("\tinteger,parameter::nspec=" + str(len(specs)) + "\n")
					fout.write("\tinteger,parameter::natoms=" + str(len(atoms)) + "\n")
					fout.write("\tinteger,parameter::ndust=" + str(ndust) + "\n")
					fout.write("\tinteger,parameter::ndustTypes=" + str(self.dustTypesSize) + "\n")
					fout.write("\tinteger,parameter::nPhotoBins=" + str(self.photoBins) + "\n")
					fout.write("\tinteger,parameter::nPhotoRea=" + str(self.nPhotoRea) + "\n")
					idust = 0
					for dType in self.dustTypes:
						nd = ndust/self.dustTypesSize
						fout.write("\tinteger,parameter::idx_dust_"+dType+"_low=nmols+"+str(nd*idust+1)+"\n")
						fout.write("\tinteger,parameter::idx_dust_"+dType+"_up=nmols+"+str(nd*(idust+1))+"\n")
						idust += 1
					#serach for first and last surface species
					firstSurface = 1e99
					lastSurface = -1e99
					surfaceFound = False
					#loop on species to find surface species
					for x in self.specs:
						if(x.is_surface):
							surfaceFound = True
							firstSurface = min(firstSurface,x.idx)
							lastSurface = max(lastSurface,x.idx)
					#write the indexes of the surface species
					#if(surfaceFound):
					#	fout.write("\tinteger,parameter::idx_firstSurface="+str(firstSurface)+"\n")
					#	fout.write("\tinteger,parameter::idx_lastSurface="+str(lastSurface)+"\n")

			elif srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))
			elif srow == "#KROME_phys_commons":
				for x in self.physVariables:
					fout.write("real*8::phys_"+x[0]+"\n")
				for x in self.physVariables:
					fout.write("!$omp threadprivate(phys_"+x[0]+")\n")
			elif srow == "#KROME_xsecs_from_file":
				srow = ""
				for rea in reacts:
					if not rea.hasXsecFile: continue
					sidx = str(rea.idx)
					srow += "!xsec for "+rea.verbatim+"\n"
					srow += "real*8,allocatable::xsec"+sidx+"_val(:)\n"
					srow += "real*8::xsec"+sidx+"_Emin\n"
					srow += "real*8::xsec"+sidx+"_idE\n"
					srow += "integer::xsec"+sidx+"_n\n\n"
				fout.write(srow+"\n")
			elif srow == "#KROME_cool_index":
				idxcool = get_cooling_index_list()
				for x in idxcool:
					fout.write("integer,parameter::"+x+"\n")
			elif srow == "#KROME_heat_index":
				idxheat = get_heating_index_list()
				for x in idxheat:
					fout.write("integer,parameter::"+x+"\n")
			elif srow == "#KROME_implicit_arr_r":
				for j in range(self.maxnreag):
					fout.write("integer::arr_r"+str(j+1)+"(nrea)\n")
			elif srow == "#KROME_implicit_arr_p":
				for j in range(self.maxnprod):
					fout.write("integer::arr_p"+str(j+1)+"(nrea)\n")
			elif srow == "#KROME_photoheating_variables" and self.useHeatingPhoto:
				fout.write("real*8::"+(",".join(pheatvars))+"\n")
			elif srow == "#KROME_opt_variables":
				fout.write(optVariables)
			elif srow == "#KROME_user_commons":
				if len(self.commonvars) > 0:
					fout.write("real*8::"+(",".join(self.commonvars))+"\n")
					fout.write("!$omp threadprivate("+(",".join(self.commonvars))+")\n")
			elif srow == "#KROME_photobins_array":
				if self.photoBins>0:
					fout.write("real*8::photoBinJ(nPhotoBins) !intensity per bin, eV/sr/cm2\n")
					fout.write("real*8::photoBinJ_org(nPhotoBins) !intensity per bin stored, eV/sr/cm2\n")
					fout.write("real*8::photoBinEleft(nPhotoBins) !left limit of the freq bin, eV\n")
					fout.write("real*8::photoBinEright(nPhotoBins) !right limit of the freq bin, eV\n")
					fout.write("real*8::photoBinEmid(nPhotoBins) !middle point of the freq bin, eV\n")
					fout.write("real*8::photoBinEdelta(nPhotoBins) !size of the freq bin, eV\n")
					fout.write("real*8::photoBinEidelta(nPhotoBins) !inverse of the size of the freq bin, 1/eV\n")
					fout.write("real*8::photoBinJTab(nPhotoRea,nPhotoBins) !xsecs table, cm2\n")
					fout.write("real*8::photoBinRates(nPhotoRea) !photo rates, 1/s\n")
					fout.write("real*8::photoBinHeats(nPhotoRea) !photo heating, erg/s\n")
					fout.write("real*8::photoBinEth(nPhotoRea) !energy treshold, eV\n")
					fout.write("real*8::photoPartners(nPhotoRea) !index of the photoreactants\n")
					fout.write("real*8::opacityDust(nPhotoBins) !interpolated opacity from tables\n")
					fout.write("!$omp threadprivate(photoBinJ,photoBinJ_org,photoBinEleft,photoBinEright,photoBinEmid, &\n")
					fout.write("!$omp    photoBinEdelta,photoBinEidelta,photoBinJTab,photoBinRates,photoBinHeats,photoBinEth, &\n")
					fout.write("!$omp    photoPartners)\n")

			elif srow == "#KROME_var_parts" and self.typeGamma == "POPOVAS":
				spec_parts = ["H2even","H2odd","CO"]
				for spec_part in spec_parts:
					spart = "real*8,allocatable::zpart"+spec_part+"(:)\n"
					spart +="real*8::zpartMin"+spec_part+", zpartdT"+spec_part+"\n"
					fout.write(spart)

			#write the anytab common variables
			elif srow == "#KROME_vars_anytab":
				stab = ""
				for i in range(len(self.anytabvars)):
					tabvar = self.anytabvars[i]
					tabfile = self.anytabfiles[i]
					tabsize = self.anytabsizes[i]
					stab += "real*8::" + tabvar+"_anytabx("+tabsize[0]+")\n"
					stab += "real*8::" + tabvar+"_anytaby("+tabsize[1]+")\n"
					stab += "real*8::" + tabvar+"_anytabz("+tabsize[0]+","+tabsize[1]+")\n"
					stab += "real*8::" + tabvar+"_anytabxmul\n"
					stab += "real*8::" + tabvar+"_anytabymul\n"
				fout.write(stab+"\n")

			elif srow == "#KROME_GFE_from_file":
				lines = ""
				if self.use_GFE_tables:
					lines += "integer, parameter :: GFE_tab_imax = 300\n"
					lines += "real*8 :: GFE_tab_Tgas(GFE_tab_imax)\n"
					lines += "real*8 :: GFE_mult_Tgas\n"
					for sp in self.GFE_species:
						lines += ("real*8 :: GFE_tab_gibbs_" + sp.name +
								"(GFE_tab_imax)\n" )
				fout.write(lines + "\n")

			else:
				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	##################################
	def makeConstants(self):
		buildFolder = self.buildFolder
		constants = []
		constants.append(["boltzmann_eV", "8.617332478d-5","eV / K"])
		constants.append(["boltzmann_J", "1.380648d-23","J / K"])
		constants.append(["boltzmann_erg", "1.380648d-16","erg / K"])
		constants.append(["iboltzmann_eV", "1d0/boltzmann_eV","K / eV"])
		constants.append(["iboltzmann_erg", "1d0/boltzmann_erg","K / erg"])
		constants.append(["planck_eV","4.135667516d-15","eV s"])
		constants.append(["planck_J","6.62606957d-34","J s"])
		constants.append(["planck_erg","6.62606957d-27","erg s"])
		constants.append(["iplanck_eV","1d0/planck_eV","1 / eV / s"])
		constants.append(["iplanck_J","1d0/planck_J","1 / J / s"])
		constants.append(["iplanck_erg","1d0/planck_erg","1 / erg / s"])
		constants.append(["gravity","6.674d-8","cm3 / g / s2"])
		constants.append(["e_mass","9.10938188d-28","g"])
		constants.append(["p_mass","1.67262158d-24","g"])
		constants.append(["n_mass","1.674920d-24","g"])
		constants.append(["ip_mass","1d0/p_mass","1/g"])
		constants.append(["clight","2.99792458e10","cm/s"])
		constants.append(["pi","3.14159265359d0","#"])
		constants.append(["eV_to_erg","1.60217646d-12","eV -> erg"])
		constants.append(["ry_to_eV","13.60569d0","rydberg -> eV"])
		constants.append(["ry_to_erg","2.179872d-11","rydberg -> erg"])
		constants.append(["seconds_per_year","365d0*24d0*3600d0","yr -> s"])
		constants.append(["km_to_cm","1d5","km -> cm"])
		constants.append(["cm_to_Mpc","1.d0/3.08d24","cm -> Mpc"])
		constants.append(["kvgas_erg","8.d0*boltzmann_erg/pi/p_mass",""])
		constants.append(["pre_kvgas_sqrt","sqrt(8.d0*boltzmann_erg/pi)",""])
		constants.append(["pre_planck","2.d0*planck_erg/clight**2","erg/cm2*s3"])
		constants.append(["exp_planck","planck_erg / boltzmann_erg","s*K"])
		constants.append(["stefboltz_erg","5.670373d-5","erg/s/cm2/K4"])
		constants.append(["N_avogadro","6.0221d23","#"])
		constants.append(["Rgas_J","8.3144621d0","J/K/mol"])
		constants.append(["Rgas_kJ","8.3144621d-3","kJ/K/mol"])
		constants.append(["hubble","0.704d0","dimensionless"])
		constants.append(["Omega0","1.0d0","dimensionless"])
		constants.append(["Omegab","0.0456d0","dimensionless"])
		constants.append(["Hubble0","1.d2*hubble*km_to_cm*cm_to_Mpc","1/s"])

		#********* CONSTANTS ****************
		fh = open(self.srcFolder+"krome_constants.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_constants.f90","w")

		#prepares list of constants
		const = "!constants\n"
		for x in constants:
			const += "real*8,parameter::" + x[0] + " = " + x[1] + " !" + x[2] + "\n"

		#replace pragmas
		for row in fh:
			if row.strip() == "#KROME_constant_list":
				fout.write(const)
			if row[0] != "#":
				fout.write(row)

		if not self.buildCompact:
			fout.close()
		self.constantList = constants


	########################################
	def makeUserCommons(self):
		buildFolder = self.buildFolder
		#*********USER COMMONS****************
		#write parameters in krome_user_commons.f90
		if not file_exists(buildFolder+"krome_user_commons.f90"):
			print("- writing krome_user_commons.f90...",)

			fh = open(self.srcFolder+"krome_user_commons.f90")
			fouta = open(self.buildFolder+"krome_user_commons.f90","w")

			for row in fh:
				row = row.replace("#KROME_header", get_licence_header(self.version, self.codename,self.shortHead))
				if row[0] != "#": fouta.write(row)

			fouta.close()
			print("done!")
		else:
			print("WARNING: krome_user_commons.f90 already found in " + buildFolder
				  + " : not replaced!")

	############################################
	def get_Ebareice(self, fmult, specs, functionName):
		get_Ebareice_out = ""
		Ebind23list = dict()
		for x in specs:
			if x.Ebind_ice == 0e0: continue
			Ebind_tupla = str(x.Ebind_ice)+"_"+str(x.parentDustBin)
			if Ebind_tupla in Ebind23list:
				get_Ebareice_out += (functionName+"("+str(x.fidx)+") = "+functionName+"("+str(Ebind23list[Ebind_tupla])+")\n")
				continue
			get_Ebareice_out += (functionName+"("+str(x.fidx)+") = get_exp_table("+format_double(fmult*float(x.Ebind_ice))\
				+", invTdust("+str(x.parentDustBin)+"))\n")
			Ebind23list[Ebind_tupla] = x.fidx
		for x in specs:
			if x.Ebind_bare == 0e0: continue
			Ebind_tupla = str(x.Ebind_bare)+"_"+str(x.parentDustBin)
			if Ebind_tupla in Ebind23list:
				get_Ebareice_out += (functionName+"("+str(x.fidx)+"+nspec) = "
					+ functionName+"("+str(Ebind23list[Ebind_tupla])+")\n")
				continue
			get_Ebareice_out += (functionName+"("+str(x.fidx)+"+nspec) = get_exp_table("
				+ format_double(fmult*float(x.Ebind_bare))+", invTdust("
				+ str(x.parentDustBin)+"))\n")
			Ebind23list[Ebind_tupla] = x.fidx+"+nspec"

		return get_Ebareice_out

	################################################
	def makeGrainFuncs(self):
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs

		#*********GRFUNCS****************
		#write parameters in krome_grfuncs.f90
		print("- writing krome_grfuncs.f90...",)
		fh = open(self.srcFolder+"krome_grfuncs.f90")
		if self.buildCompact:
			fout = open(buildFolder + "krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_grfuncs.f90","w")

		# check if total water is in the species list
		hasH2O = ("H2O_TOTAL" in [x.name.upper() for x in specs])

		skip = False
		#loop on src file and replace pragmas 
		for row in fh:
			srow = row.strip()
			if srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

			if srow == "#IFKROME_useChemisorption" and not self.useChemisorption: skip = True
			if srow == "#IFKROME_useSemenov" and not self.useSemenov: skip = True
			if srow == "#ELSEKROME" and not self.useSemenov: skip = False
			if srow == "#ELSEKROME" and self.useSemenov: skip = True
			if srow == "#IFKROME_hasH2O" and not hasH2O: skip = True
			if srow == "#IFKROME_dust_table_2D" and not(self.dustTableDimension=="2D"): skip = True
			if srow == "#IFKROME_dust_table_3D" and not(self.dustTableDimension=="3D"): skip = True

			if srow == "#ENDIFKROME": skip = False

			if skip: continue #skip

			if srow == "#KROME_Ebareice23":
				fout.write(self.get_Ebareice(2e0/3e0, self.specs, "get_Ebareice23_exp_array"))
			elif srow == "#KROME_Ebareice":
				fout.write(self.get_Ebareice(1e0, self.specs, "get_Ebareice_exp_array"))
			elif srow == "#KROME_Ebind_ice":
				for x in specs:
					if x.Ebind_ice == 0e0: continue
					fout.write("get_Ebind_ice("+str(x.fidx)+") = "+format_double(x.Ebind_ice)+"\n")
			elif srow == "#KROME_Ebind_bare":
				for x in specs:
					if x.Ebind_bare == 0e0: continue
					fout.write("get_Ebind_bare("+str(x.fidx)+") = "+format_double(x.Ebind_bare)+"\n")
			elif srow == "#KROME_parent_dust_bin":
				for x in specs:
					if x.parentDustBin == 0: continue
					fout.write("get_parent_dust_bin("+str(x.fidx)+") = "+str(x.parentDustBin)+"\n")
			else:
				if(row[0]!="#"): fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	########################
	#dump reaction verbatims to file
	def dumpReactionsVerbatim(self):
		fout2 = open(self.buildFolder+"reactions_verbatim.dat","w")
		for reaction in self.reacts:
			fout2.write(reaction.verbatim+"\n")
		fout2.close()

	################################################
	def makeFit(self):
		#*********FIT****************
		#write parameters in krome_fit.f90
		print("- writing krome_fit.f90...",)
		fh = open(self.srcFolder+"krome_fit.f90")
		if self.buildCompact :
			fout = open(self.buildFolder+"krome_all.f90","a")
		else:
			fout = open(self.buildFolder+"krome_fit.f90","w")

		#write to file
		for row in fh:
			fout.write(row)

		if not self.buildCompact:
			fout.close()

	################################################
	def makeGetPhys(self):
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs

		#*********PHFUNCS****************
		#write parameters in krome_grfuncs.f90
		print("- writing krome_getphys.f90...",)
		fh = open(self.srcFolder+"krome_getphys.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_getphys.f90","w")

		#prepare metallicity functions
		#metallicity dictionary
		metalDict = dict()
		iceList = []
		for sp in specs:
			if sp.name.lower().endswith("_total"): iceList.append(sp)
			for atom, count in sp.atomcount2.items():
				if atom in metalDict:
					metalDict[atom].append([sp, count])
				else:
					metalDict[atom] = [[sp, count]]


		iceNames = [x.name.lower().replace("_total","") for x in iceList]
		#metallicity functions
		nZs = ["H","He","+","-","E"] #these are not metals
		zGets = []
		for atom,speciesList in metalDict.items():
			if atom in nZs: continue #skip non-metals
			parts = []
			for sp in speciesList:
				if sp[0].name.lower() in iceNames: continue
				smult = "" #multiplication factor string if >1 atom/particle
				if sp[1] > 1: smult = str(sp[1])+"d0*"
				parts.append(smult+"n("+sp[0].fidx+")")
			zGets.append([atom, "z"+atom, (" &\n + ".join(parts))])

#			if(srow == "#KROME_header"):
#				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

		#loop on src file and replace pragmas
		for row in fh:
			srow = row.strip()
			if srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

			elif srow == "#KROME_sum_H_nuclei":
				hsum = []
				iceNames = [x.name.lower().replace("_total","") for x in specs if(x.name.lower().endswith("_total"))]
				for x in specs:
					if x.name.lower() in iceNames: continue
					if "H" not in x.atomcount2: continue
					if x.atomcount2["H"] == 0: continue
					hmult = ("*"+format_double(x.atomcount2["H"]) if x.atomcount2["H"]>1 else "")
					hsum.append("n("+x.fidx+")"+hmult)
				if len(hsum) == 0: hsum.append("0.d0")
				fout.write("nH = "+(" + &\n".join(hsum))+"\n")

			elif srow == "#KROME_col2num_method":
				if self.columnDensityMethod == "DEFAULT":
					fout.write("col2num = 1d3 * (max(ncalc,1d-40)/1.87d21)**1.5\n")
				elif self.columnDensityMethod == "JEANS":
					fout.write("col2num = 2d0 * max(ncalc,1d-40) / get_jeans_length(n(:),Tgas)\n")
				elif self.columnDensityMethod == "JEANS40":
					fout.write("col2num = 2d0 * max(ncalc,1d-40) / get_jeans_length(n(:),min(Tgas,4d1))\n")
				else:
					sys.exit("ERROR: method "+self.columnDensityMethod+" unknown for col2num")
			elif srow == "#KROME_num2col_method":
				if self.columnDensityMethod == "DEFAULT":
					fout.write("num2col = 1.87d21*(max(ncalc,1d-40)*1d-3)**(2./3.)\n")
				elif self.columnDensityMethod == "JEANS":
					fout.write("num2col = 0.5d0 * max(ncalc,1d-40) * get_jeans_length(n(:),Tgas)\n")
				elif self.columnDensityMethod == "JEANS40":
					fout.write("num2col = 0.5d0 * max(ncalc,1d-40) * get_jeans_length(n(:),min(Tgas,4d1))\n")
				else:
					sys.exit("ERROR: method "+self.columnDensityMethod+" unknown for num2col")
			elif srow == "#KROME_masses":
				# fdat = open(buildFolder + "masses.dat", "w")
				for x in specs:
					massrow = "\tget_mass("+str(x.idx)+") = " + str(x.mass).replace("e","d") + "\t!" + x.name + "\n"
					# datarow = x.name + "\t" + str(x.idx) + "\t" + str(x.mass) + "\n"
					fout.write(massrow.replace(" 0.0\t", " 0d0\t"))
					# fdat.write(datarow)
				# fdat.close()
			elif srow == "#KROME_imasses":
				for x in specs:
					myimass = 0e0
					if x.mass != 0e0: myimass = 1e0/x.mass
					imassrow = "\tget_imass("+str(x.idx)+") = " + str(myimass).replace("e","d") + "\t!" + x.name + "\n"
					fout.write(imassrow.replace(" 0.0\t", " 0d0\t"))
			elif srow == "#KROME_imasses_sqrt":
				from math import sqrt
				for x in specs:
					myimass = 0e0
					if x.mass != 0e0: myimass = 1e0 / sqrt(x.mass)
					imassrow = "\tget_imass_sqrt("+str(x.idx)+") = " + str(myimass).replace("e", "d") + "\t!" + x.name + "\n"
					fout.write(imassrow.replace(" 0.0\t", " 0d0\t"))
			elif srow == "#KROME_zatoms":
				for x in specs:
					zatomrow = "\tget_zatoms("+str(x.idx)+") = " + str(x.zatom) + "\t!" + x.name + "\n"
					fout.write(zatomrow)

			elif srow=="#KROME_electrons_balance":
				chargeBalance = []
				for x in specs:
					if x.name == "E": continue
					mult = ""
					if abs(x.charge) > 1: mult = str(abs(x.charge))+"d0*"
					if x.charge > 0: chargeBalance.append(" + "+mult+"n("+x.fidx+")")
					if x.charge < 0: chargeBalance.append(" - "+mult+"n("+x.fidx+")")
				if len(chargeBalance) > 0:
					fout.write("get_electrons = "+(" &\n".join(chargeBalance))+"\n")

			elif srow == "#KROME_names":
				for x in specs:
					fout.write("\tget_names("+str(x.idx)+") = \"" + x.name + "\"\n")

			elif srow == "#KROME_cooling_names":
				coolDict = [[k,v] for (k,v) in get_cooling_dict().items()]
				allCoolings = [xx[0] for xx in sorted(coolDict, key=lambda x:x[1])]
				for cool in allCoolings:
					fout.write("get_cooling_names(idx_cool_"+str(cool)+") = \"" + cool.upper() + "\"\n")

			elif srow == "#KROME_heating_names":
				heatDict = [[k,v] for (k,v) in get_heating_dict().items()]
				allHeatings = [xx[0] for xx in sorted(heatDict, key=lambda x:x[1])]
				for heat in allHeatings:
					fout.write("get_heating_names(idx_heat_"+str(heat)+") = \"" + heat.upper() + "\"\n")

			elif srow == "#KROME_charges":
				for x in specs:
					fout.write("\tget_charges("+str(x.idx)+") = " + str(x.charge) + ".d0 \t!" + x.name + "\n")

			#elif(srow == "#KROME_reaction_names"):
			#	for x in reacts:
			#		kstr = "\tget_rnames("+str(x.idx)+") = \"" + x.verbatim +"\""
			#		fout.write(kstr+"\n")
			elif srow == "#KROME_qeff":
				#look for the largest qeff value
				maxqeff = 0e0
				for x in reacts:
					maxqeff = max(maxqeff,x.qeff)
				#if 0e0 is the largest then compress the array, else write it explicitely
				if maxqeff == 0e0:
					fout.write("get_qeff(:) = 0e0\n")
				else:
					for x in reacts:
						sqeff = "\tget_qeff("+str(x.idx)+") = "+str(x.qeff)+" !" + x.verbatim
						fout.write(sqeff+"\n")

			elif srow == "#KROME_reaction_names":
				for x in reacts:
					kstr = "\tget_rnames("+str(x.idx)+") = \"" + x.verbatim +"\""
					fout.write(kstr+"\n")

			elif srow == "#KROME_get_Ebind_bare":
			#if useSemenov load the Semenov binding energies file
				if(self.useSemenov):
					Ebind = get_Ebind(fileName="data/Ebare_ice_Semenov.dat",surface="bare")
				else:
					Ebind = get_Ebind(surface="bare")
				for sp in specs:
					if sp.name in Ebind:
						fout.write("get_EbindBare("+sp.fidx+") = "+str(Ebind[sp.name])+"d0\n")

			elif srow == "#KROME_get_Ebind_ice":
				if(self.useSemenov):
					Ebind = get_Ebind(fileName="data/Ebare_ice_Semenov.dat",surface="ice")
				else:
					Ebind = get_Ebind(surface="ice")

				#Ebind = get_Ebind(surface="ice")
				for sp in specs:
					if(sp.name in Ebind):
						fout.write("get_EbindIce("+sp.fidx+") = "+str(Ebind[sp.name])+"d0\n")

			elif srow == "#KROME_get_kevap70":
				from math import exp
				if(self.useSemenov):
					Ebind = get_Ebind(fileName="data/Ebare_ice_Semenov.dat",surface="bare")
				else:
					Ebind = get_Ebind(surface="bare")

				#Ebind = get_Ebind(surface="bare")
				nu0 = 1e12 #Debye frequency, 1/s
				for sp in specs:
					if sp.name in Ebind:
						fout.write("get_kevap70("+sp.fidx+") = "+str(nu0*exp(-Ebind[sp.name]/7e1))+"\n")
					else:
						fout.write("get_kevap70("+sp.fidx+") = 0d0\n")

			elif srow == "#KROME_metallicity_functions":
				solar = get_solar_abundances() #get solar abundances
				ffs = "" #metallicity functions
				for zg in zGets:
					if zg[0] not in solar: continue #skip if solar abundance is not present
					#prepare function
					ffname = "get_metallicity"+zg[0]
					ff = "!*****************************\n"
					ff += "! get metallicity using "+zg[0]+" as reference\n"
					ff += "function "+ffname+"(n)\n"
					ff += "use krome_commons\n"
					ff += "implicit none\n"
					ff += "real*8::n(:),"+ffname+","+zg[1]+",nH\n\n"
					ff += "nH = get_Hnuclei(n(:))\n\n"
					ff += zg[1]+" = "+zg[2]+"\n\n"
					ff += zg[1]+" = max("+zg[1]+", 0d0)\n\n"
					ff += ffname + " = log10("+zg[1]+"/nH+1d-40) - ("+solar[zg[0]]+")\n\n" #compute metallicity
					ff += "phys_metallicity = "+ffname + "\n\n" #set Z in the physvariable
					ff += "end function "+ffname+"\n\n"
					ffs += ff #append function to the others
				fout.write(ffs+"\n") #replace functions
			else:
				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")


	##################################
	def makeGammaAdiabatic(self):
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs

		#*********GADIAB****************
		#write parameters in krome_gadiab.f90
		print("- writing krome_gadiab.f90...",)
		fh = open(self.srcFolder+"krome_gadiab.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_gadiab.f90","w")

		#loop on src file and replace pragmas
		for row in fh:
			srow = row.strip()
			if srow == "#KROME_gamma":
				is_multiline = False #flag for multiline gamma
				#computes the adiabatic index if needed or uses a user-defined expression
				if self.typeGamma == "DEFAULT":
					gamma = "1.66666666667d0" #default gamma is 5/3 (atomic)
				elif self.typeGamma == "FULL":
					#build gamma following (Grassi+2010,MerlinPhDTheis)
					specsGamma = ["H","HE","E","H2"] #species used in the gamma
					gammaNm = [] #numerator monoatomics
					gammaDm = [] #denominator monoatomics
					gammaNb = [] #numerator diatomics
					gammaDb = [] #denominator diatomics
					#loop on species
					for mol in specs:
						if mol.name.upper() in specsGamma:
							nfidx = "n("+mol.fidx+")" #number density
							if mol.is_atom: #monoatomic
								gammaNm.append(nfidx)
								gammaDm.append(nfidx)
							else: #diatomic
								gammaNb.append(nfidx)
								gammaDb.append(nfidx)
					#build fortran expression for gamma
					gammaN = "(5.d0*("+(" + ".join(gammaNm)) + ") + 7.d0*("+(" + ".join(gammaNb)) + "))"
					gammaD = "(3.d0*("+(" + ".join(gammaDm)) + ") + 5.d0*("+(" + ".join(gammaDb)) + "))"
					gamma = gammaN + " / &\n" +gammaD

				elif self.typeGamma in ["EXACT", "VIB", "ROT", "REDUCED", "POPOVAS"]:
					#extends Omukai+Nishi1998 eqs.5,6,7
					# and Boley+2007 (+erratum!) eqs.2,3
					header = "real*8::Tgas,invTgas,x,expx,ysum,gsum,mosum,gvib\n"
					header += "real*8::Tgas_vib,invTgas_vib\n"
					gamma = "invTgas_vib = 1d0/Tgas_vib\n\n"
					#gamma += "nH = get_Hnuclei(n(:))\n\n"
					gi_vars = []
					g_vars = []
					di_vars = []
					mo_vars = []
					smallest_ve = 1e99
					print("")
					for mol in specs:
						#monoatomic
						if mol.natoms == 1:
							gi_mo = "1.5d0" #where gi=1/(gamma_atom-1), where gamma_atom=5/3
							mo_vars.append(mol.fidx)
						#diatomic
						elif mol.natoms == 2:
							gtype = self.typeGamma
							#skip every diatoms except H2 and CO if REDUCED
							if gtype == "REDUCED" and mol.name != "H2" and mol.name != "CO": continue
							if gtype == "POPOVAS" and mol.name != "H2" and mol.name != "CO": continue
							#warning if vibrational constant not found
							if mol.ve_vib == "__NONE__" and (gtype == "EXACT" or gtype == "VIB"):
								print("WARNING: no vibrational constant for "+mol.name
									  +" in gamma calculation!")
							#warning if rotational constant not found
							if mol.be_rot == "__NONE__" and (gtype == "EXACT" or gtype == "ROT"):
								print("WARNING: no rotational constant for "+mol.name
									  +" in gamma calculation!")
							#continue if both constants were not found
							if mol.ve_vib == "__NONE__" and mol.be_rot == "__NONE__":
								continue
							gamma += "\n!evaluate 1/(gamma-1) for "+mol.name+"\n"
							#prepare the vibrational part
							vibpart = "0d0"
							if mol.ve_vib != "__NONE__" and (gtype == "EXACT" or gtype == "VIB"):
								smallest_ve = min(smallest_ve, mol.ve_vib) #store the smallest vib constant
								xvar = "x = "+format_double(mol.ve_vib)+"*invTgas_vib\n"
								expvar = "expx = exp(x)\n"
								gamma += xvar
								gamma += expvar
								gamma += "gvib = 2d0*x*x*expx/(expx-1d0)**2\n"
								vibpart = "gvib"
							#prepare the rotational part
							rotpart = "2d0"
							if mol.be_rot != "__NONE__" and \
								(gtype == "EXACT" or gtype == "ROT" or gtype == "REDUCED"):
								if mol.name == "H2":
									rotpart = "2d0*gamma_rotop(Tgas, phys_orthoParaRatio)"
								else:
									rotpart = "2d0*gamma_rot(Tgas, "+format_double(mol.be_rot)+")"
							#rotational partition functions from A.Popovas (2014) Thesis
							if gtype == "POPOVAS":
								if mol.name == "H2":
									rotpart = "2d0*gamma_pop_H2(zpartH2even(:),zpartH2odd" \
											+ "(:),zpartMinH2even,zpartdTH2even,Tgas,phys_orthoParaRatio)"
								elif mol.name == "CO":
									rotpart = "2d0*gamma_pop(zpartCO(:),zpartMinCO,zpartdTCO,Tgas)"
								else:
									print("ERROR: trying to load "+mol.name+" partition function!")
									sys.exit()

							di_vars.append(mol.fname)
							gi_vars.append("gi_"+mol.fname)
							gi = "gi_"+mol.fname+" = 0.5d0*(3d0 + "+rotpart+" + "+vibpart+")\n"
							gamma += gi

						#polyatomic
						else:
							pass #skip polyatomic
					#prepone variables declaration
					header += "real*8::"+(",".join(gi_vars)) + "\n"
					#write sums
					gamma += "\n!sum monotomic abundances\n"
					gamma += "mosum = " + (" + &\n".join(["n("+x+")" for x in mo_vars])) + "\n"
					gamma += "\n!sum all abundances\n"
					gamma += "ysum = mosum + "+(" + &\n".join(["n(idx_"+x+")" for x in di_vars]))+"\n"
					gamma += "\n!computes gamma\n"
					gamma += "gsum = mosum * "+gi_mo+" + "\
						+(" + &\n".join([("n(idx_"+x+")*gi_"+x) for x in di_vars]))+"\n"
					#add sum
					gamma += "krome_gamma = 1d0 + ysum/gsum\n"

					#append Tgas limit to avoid overflows on exp()
					header += "\n!avoid small Tgas that causes large x=a/Tgas below\n"
					header += "Tgas_vib = max(n(idx_Tgas), "+format_double(smallest_ve*1e-2) + ")\n"
					header += "Tgas = n(idx_Tgas)\n"

					#append gamma to the header
					gamma = header + gamma
					is_multiline = True

				else:
					#user-defined gamma
					gamma = self.typeGamma

				#write gamma
				if is_multiline:
					fout.write(gamma)
				else:
					fout.write("krome_gamma = " + gamma + "\n")
			else:
				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	################################################
	def makePhotoFuncs(self):
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs

		#*********PHFUNCS****************
		#write parameters in krome_grfuncs.f90
		print("- writing krome_phfuncs.f90...",)
		fh = open(self.srcFolder+"krome_phfuncs.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_phfuncs.f90","w")

		has_HI = has_HII = has_H2I = False
		#check if electrons are present
		for x in specs:
			if x.name == "H": has_HI = True
			if x.name == "H+": has_HII = True
			if x.name == "H2": has_H2I = True

#			if(srow == "#KROME_header"):
#				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

		#loop on src file and replace pragmas
		skip = False
		for row in fh:
			srow = row.strip()

			#skip when find IF pragmas

			if srow == "#IFKROME_useShieldingWG11" and not self.useShieldingWG11: skip = True
			if srow == "#IFKROME_useShieldingDB96" and not self.useShieldingDB96: skip = True
			if srow == "#IFKROME_useShieldingR14" and not self.useShieldingR14: skip = True
			if srow == "#IFKROME_usePhotoBins" and not (self.photoBins>0): skip = True

			if srow == "#IFKROME_hasHI" and not has_HI: skip = True
			if srow == "#IFKROME_hasHII" and not has_HII: skip = True
			if srow == "#IFKROME_hasH2I" and not has_H2I: skip = True

			if srow == "#ENDIFKROME": skip = False

			if skip:
				continue #skip
			else:
				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")


	###################################################
	def makeSubs(self):
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs
		thermodata = self.thermodata
		coevars = self.coevars

		#*********SUBS****************
		#write parameters in krome_subs.f90
		print("- writing krome_subs.f90...",)
		fh = open(self.srcFolder+"krome_subs.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_subs.f90","w")

		#create list of temperature shortcuts
		sclist = []
		for rea in reacts:
			sclist = get_Tshortcut(rea,sclist,coevars)

		#prepare shortcut definitions
		shortcutVars = ""
		for x in sclist:
			shortcutVars += "real*8::"+x.split("=")[0].strip()+"\n"

		#conserve
		krome_conserve = "" #init full string for the pragma replacement
		if self.useConserve or self.useConserveLin:
			skipa = ["CR","Tgas","dummy","g"] #skip these species
			multi = [] #species with shared atoms
			acount = dict() #store species per atom type (e.g. {"C":["C","CO","C2"], ...})
			has_multiple = False #check if species with shared atoms are present (e.g. CO but not H2)
			#loop on the species
			for x in specs:
				xname = x.name #species name
				if xname in skipa: continue #cycle if the name is present in the skip list
				afound = 0 #cont founded type atoms
				#loop on the dictionary that count the atoms in the species
				for a in x.atomcount2:
					if a in ["+", "-"]: continue #skip non-atoms
					afound +=1 #count founded atoms (for account of shared atoms)
					#append species to dictionary if contains the atom a
					if(a in acount):
						acount[a].append(x)
					else:
						acount[a] = [x]
				if afound > 1:
					has_multiple = True #flag for shared atoms
					multi.append(x.name)
			#if species with shared atoms warns the user (also in the subs file)
			if has_multiple and self.useConserve:
				krome_conserve += "!WARNING: found species with different atoms:\n"
				krome_conserve += "!conservation function may be non-accurate\n"
				krome_conserve += "!the approximation is valid when the following species\n"
				krome_conserve += "! are smaller compared to the others\n"
				krome_conserve += "! "+(", ".join(multi))+"\n"
				print("")
				print("WARNING: found species with different atoms:")
				print(" conservation function may be non-accurate")
			print("")

			#loop on the found atoms. k=atom, v=list of species with k
			for k,v in acount.items():
				if not self.useConserve: break
				if k == "E": continue #skip electrons
				aadd = [] #parts of the summation for ntot
				sdiff = "" #string for scaling
				#loop on the species
				for x in v:
					mult = (str(x.atomcount2[k])+"d0*" if x.atomcount2[k]>1 else "") #multiplication factor
					aadd.append(mult+"n("+x.fidx+")") #append species density with factor
					sdiff += "no("+x.fidx+") = n("+x.fidx+") * factor\n" #rescaling
				#add dust to conservation when needed
				idust = 0
				useDustEvol = (self.useDustEvap or self.useDustGrowth or self.useDustSputter)
				for dType in self.dustTypes:
					if not useDustEvol: break
					if dType == k:
						ilow = str(self.dustArraySize * idust + 1)
						iup = str(self.dustArraySize * (idust+1))
						aadd.append("sum(n(nmols+"+ilow+":nmols+"+iup+")*krome_dust_partner_ratio("\
							+ilow+":"+iup+"))")
					idust += 1
				if len(aadd) > 0:
					sadd = "ntot = " + (" &\n + ".join(aadd)) #current total density of the species k
					#initial total density of the species k
					saddi = "nitot = " + (" &\n + ".join([y.replace("n(","ni(") for y in aadd]))
				#prepare replacing string
				krome_conserve += "\n!********** "+k+" **********\n"
				krome_conserve += sadd + "\n"
				krome_conserve += saddi + "\n"
				krome_conserve += "factor = nitot/ntot\n"
				krome_conserve += sdiff + "\n"
				krome_conserve += "\n"

		nmax = 60 #max number of species for conservation
		if len(specs) > nmax and self.useConserve:
			print("WARNING: more than "+str(nmax)+" species (i.e. "+str(len(specs))
				  +"), -conserve disabled!")
			krome_conserve = "" #with more than NMAX species conserve only electrons

		has_electrons = False #check if electrons are present
		#has_HI = has_HII = has_H2I = False
		#check if electrons are present
		for x in specs:
			if x.name == "E": has_electrons = True
		#	if(x.name=="H"): has_HI = True
		#	if(x.name=="H+"): has_HII = True
		#	if(x.name=="H2"): has_H2I = True

		#charge conservation
		if has_electrons and self.useConserveE:
			consE = "no(idx_E) = max("
			for x in specs:
				if x.name== "E": continue #skip electron
				if x.charge == 0: continue #skip neutrals
				#prepare charge multiplicator
				mult = ""
				if x.charge > 0: mult = "+"
				if x.charge > 1: mult = "+" + str(x.charge) + "d0*"
				if x.charge < 0: mult = "-"
				if x.charge < -1: mult = str(x.charge) + "d0*"
				consE += " &\n"+mult+"n("+x.fidx+")"
			consE += ", 1d-40)"
			krome_conserve += "\n!********** E **********\n"
			krome_conserve += consE + "\n"

		#loop on src file and replace pragmas
		skip = False
		for row in fh:
			srow = row.strip()

			#skip when find IF pragmas
			if srow == "#IFKROME_useXrays" and not self.useXRay: skip = True
			if srow == "#IFKROME_useH2dust_constant" and not self.useDustH2const: skip = True
			if srow == "#IFKROME_has_electrons" and not has_electrons: skip = True
			if srow == "#IFKROME_useLAPACK" and not self.needLAPACK: skip = True #skip calls to LAPACK
			if srow == "#IFKROME_hasStoreOnceRates" and not self.hasStoreOnceRates: skip = True
			if srow == "#IFKROME_use_GFE_tables" and not self.use_GFE_tables: skip = True
			if srow == "#IFKROME_use_cluster_growth" and not self.clusterablesPresent: skip = True
			if srow == "#ENDIFKROME": skip = False

			if skip: continue #skip

            # only proper atoms should be included
			atomSkipList = [x.upper() for x in ["_total", "_dust", "E", "+", "-", "_ortho", "_para", "_meta", "_anti"]]

			#replace the small value for rates according to the maximum number of products
			if "#KROME_small" in srow:
				if self.useTabs:
					fout.write(srow.replace("#KROME_small","0d0")+"\n")
					continue
				maxprod = 0
				for x in reacts:
					maxprod = max(len(x.products),maxprod)
				mysmall = "1d-40/("+("*".join(["nmax"]*maxprod))+")"
				if maxprod == 0: mysmall = "0d0"
				fout.write(srow.replace("#KROME_small",mysmall)+"\n")
				continue

			elif srow=="#KROME_conserve_matrix" and self.useConserveLin:
				conserve_matrix = ""
				#loop on the type of atoms (equation-wise)
				for atomType, speciesList in acount.items():
					if atomType.upper() in atomSkipList: continue
					#loop on the type of atoms (coefficient-wise)
					for atomType2, speciesList2 in acount.items():
						if atomType2.upper() in atomSkipList: continue
						#loop on the species of a given atom type
						for species in speciesList:
							#if the species belongs to both atom groups, e.g. CO in C and O
							if atomType in species.atomcount and atomType2 in species.atomcount:
								#matrix element
								mtxVarA = "A(idx_atom_"+atomType+", idx_atom_"+atomType2+")"
								#product of atoms multipliers
								pp = str(species.atomcount[atomType2]*species.atomcount[atomType])+"d0 *"
								if pp == "1d0 *": pp = ""
								myFidxx = myFidxm = species.fidx
								if myFidxx.lower().endswith("_total"):
									myFidxm = ("_".join(myFidxx.split("_")[:-1]))
									myFidxx = myFidxm+"_ice"
								#mfact = pp*self.mass_dic[atomType2.upper()] / species.mass
								conserve_matrix +=  mtxVarA + " = " + mtxVarA + " + "+str(pp) \
									+" x("+myFidxx + ") * m(idx_"+atomType+") * m(idx_"+atomType2 \
									+") / m("+myFidxm+")**2\n"
				fout.write(conserve_matrix+"\n")

			elif srow == "#KROME_conserve_fscale" and self.useConserveLin:
				specSkip = [x.lower() for x in (["CR", "g", "dummy", "Tgas"] + atomSkipList)]
				conserve_fscale = ""
				for species in self.specs:
					if species.name.lower() in specSkip: continue
					fmult = [str(atomCount)+"d0*m(idx_"+atomType+") * B(idx_atom_"+atomType+")"
							 for atomType, atomCount in species.atomcount.items()
							 if atomType.lower() not in specSkip]
					fact = (" + &\n ".join(fmult))
					myFidxx = myFidxm = species.fidx
					if myFidxx.lower().endswith("_total"):
						myFidxm = ("_".join(myFidxx.split("_")[:-1]))
						myFidxx = myFidxm+"_ice"
					rescale = "x("+myFidxx+") = x("+myFidxx+") * ("+fact+")/m("+myFidxm+")"
					conserve_fscale += rescale.replace(" 1d0*"," ").replace("(1d0*","(")+"\n"
				fout.write(conserve_fscale+"\n")

			elif srow=="#KROME_conserveLin_ref" and self.useConserveLin:
				atomSkip = [x.lower() for x in atomSkipList]
				refMassAll = ""
				for species in self.specs:
					if species.name.lower() in atomSkip: continue
					if species.mass > 0e0 or species.name.lower().endswith("_total"):
						for (atomType,atomCount) in species.atomcount.items():
							if atomType.lower() in atomSkip: continue
							varRef = conserveLinGetRef_x = "conserveLinGetRef_x(idx_atom_"+atomType+")"
							myFidxx = myFidxm = species.fidx
							if myFidxx.lower().endswith("_total"):
								myFidxm = ("_".join(myFidxx.split("_")[:-1]))
								myFidxx = myFidxm+"_ice"
							refMass = varRef + " = "+ varRef + " + " + str(atomCount) \
								+"d0*m(idx_"+atomType+")*x("+myFidxx+")/m("+myFidxm+")\n"
							refMassAll += refMass.replace(" 1d0*"," ")
				fout.write(refMassAll+"\n")

			elif "#KROME_conserveLin_electrons" in srow:
				conserveLinElectrons = []
				for species in specs:
					if species.charge == 0 or species.name == "E": continue
					sgn = ("+" if species.charge>0 else "-")
					conserveLinElectrons.append(sgn + " " + str(abs(species.charge)) \
						+ "d0*x(" + species.fidx +") / m("+species.fidx+")")
				srow = srow.replace("#KROME_conserveLin_electrons",(" &\n".join(conserveLinElectrons)))
				# srow = srow.replace("1d0*","")
				fout.write(srow+"\n")
				continue

			#write reaction rates in coe function
			if srow == "#KROME_krates":
				for rea in reacts:
					#get rate expression including temperature limit conditions
					kstr = rea.getRateF90(self)
					#replace rates with store once if required
					if rea.isStoreOnce:
						kstr = "!"+rea.verbatim+"\n"
						kstr += "k("+str(rea.idx)+") = rateEvaluateOnce("+str(rea.idx)+")"
					fout.write(truncF90(kstr, 60,"/")+"\n\n") #truncate
			#replace arrays for best flux
			elif srow == "#KROME_arr_reactprod":
				for i in range(self.maxnreag):
					fout.write("if(arr_r"+str(i+1)+"(i) == idx_found) found = .true.\n")
				for i in range(self.maxnprod):
					fout.write("if(arr_p"+str(i+1)+"(i) == idx_found) found = .true.\n")
			elif srow == "#KROME_conserve":
				fout.write(krome_conserve+"\n")
			elif(srow == "#KROME_implicit_arrays"):
				fout.write(truncF90(self.implicit_arrays,60,","))
			elif srow == "#KROME_initcoevars":
				if len(coevars) == 0: continue
				#write initialization of variables
				for x in coevars.keys():
					kvars = "real*8::"+x+" !preproc from coevar"
					fout.write(kvars+"\n")
			elif srow == "#KROME_coevars":
				if len(coevars) == 0: continue
				klist = [[[k,v[1]],v[0]] for k,v in coevars.items()] #this mess is to sort dict
				klist = sorted(klist, key=lambda x: x[1])
				#write the variables
				for x in klist:
					varName = x[0][0].strip()
					varExpr = x[0][1].strip()
					#check array variables
					if "(" in varName: varName = varName.split("(")[0]+"(:)"
					#write vars
					#fout.write("!preprocessed from coevars\n")
					fout.write(varName+" = "+varExpr+"\n")

			elif srow == "#KROME_Tshortcuts":
				for shortcut in sclist:
					fout.write(shortcut+"\n")
			elif srow == "#KROME_rvars":
				if self.maxnreag > 0:
					fout.write("integer::"+(",".join(["r"+str(j+1) for j in range(self.maxnreag)]))+"\n")
			elif srow == "#KROME_arrs":
				if self.maxnreag > 0:
					for j in range(self.maxnreag):
						fout.write("r"+str(j+1)+" = arr_r"+str(j+1)+"(i)\n")
			elif srow == "#KROME_arr_flux":
				if self.maxnreag > 0:
					fout.write("arr_flux(i) = k(i)*"+("*".join(["n(r"+str(j+1)+")" for j in range(self.maxnreag)]))+"\n")
			elif srow == "#KROME_var_reverse":
				slen = str(len(specs))
				fout.write("real*8::p1_nasa("+slen+",7), p2_nasa("+slen+",7), Tlim_nasa("+slen+",3), p(7)\n")
				fout.write("real*8::p1_nist("+slen+",7), p2_nist("+slen+",7), Tlim_nist("+slen+",3)\n")
			elif srow == "#KROME_kc_reverse_nasa":
				datarev = ""
				sp1 = sp2 = spt = ""
				for x in specs:
					if all(i == 0 for i in x.poly1_nasa): continue
					sp1 += "p1_nasa("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.poly1_nasa])) + "/)\n"
					sp2 += "p2_nasa("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.poly2_nasa])) + "/)\n"
					spt += "Tlim_nasa("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.Tpoly_nasa])) + "/)\n"
				fout.write(sp1+sp2+spt)
			elif srow == "#KROME_kc_reverse_nist":
				datarev = ""
				sp1 = sp2 = spt = ""
				for x in specs:
					if all(i == 0 for i in x.poly1_nist):
						continue

					sp1 += "p1_nist("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.poly1_nist])) + "/)\n"
					spt += "Tlim_nist("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.Tpoly_nist])) + "/)\n"

					if any(i != 0 for i in x.poly2_nist):
						sp2 += "p2_nist("+x.fidx+",:)  = (/" + (",&\n".join([format_double(pp) for pp in x.poly2_nist])) + "/)\n"

				fout.write(sp1+sp2+spt)
			elif srow == "#KROME_GFE_vars":
				sp_len = str(len(specs))
				fout.write("real(dp) :: y_GFE_table(" + sp_len + ", GFE_tab_imax)\n")

			elif srow == "#KROME_GFE_tables":
				sp1 = ""
				for sp in self.GFE_species:
					sp1 += "y_GFE_table(" + sp.fidx + ",:) = GFE_tab_gibbs_" + sp.name + "(:)\n"
				fout.write(sp1)

			elif srow == "#KROME_shortcut_variables":
				fout.write(shortcutVars)
			elif srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

			elif srow == "#KROME_load_parts" and self.typeGamma == "POPOVAS":
				spec_parts = ["H2even","H2odd","CO"]
				for spec_part in spec_parts:
					spart = "call load_part(\"part"+spec_part+".dat\", zpart"+spec_part+", zpartMin"\
						+spec_part+", zpartdT"+spec_part+")"
					fout.write(spart+"\n")
			elif srow == "#KROME_verbatim_filename":
				fout.write("fname = \"" + self.verbatimFilename + "\"\n")
			elif srow == "#KROME_no_verbatim_file" and not self.useVerbatimFile:
				for x in reacts:
					kstr = "\treactionNames("+str(x.idx)+") = \"" + x.verbatim +"\""
					fout.write(kstr+"\n")
				fout.write("return\n")

			elif srow == "#KROME_nucleation_radii":
				lines = '! References in kromelib.py\n'
				if len(self.clusterablesPresent) > 1:
					cnt = 0
					for sp in self.clusterablesPresent:
						if cnt == 0:
							elseif = ''
						else:
							elseif = 'else '
						lines += (elseif + 'if(monomer_idx == ' + sp.fidx + ') then\n'
								'  monomer_radius = ' + str(sp.radius) +'_dp ! in cm\n')
						cnt += 1
					lines += 'end if\n'
				else:
					sp = self.clusterablesPresent[0]
					lines += ('monomer_radius = ' + str(sp.radius) +'_dp ! '
								+ sp.name + ' in cm')
				fout.write(lines + '\n')

			else:
				if row[0] != "#": fout.write(row)
		if not self.buildCompact:
			fout.close()
		print("done!")


	##############################################
	def makePhoto(self):

		buildFolder = self.buildFolder
		reacts = self.reacts
		#********* PHOTO ****************
		print("- writing krome_photo.f90...",)
		fh = open(self.srcFolder+"krome_photo.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_photo.f90","w")

		#replace photoionization and photoheating functions
		skip = skip_heat = skip_opacity = False
		for row in fh:
			srow = row.strip()
			if row.strip() == "#IFKROME_usePhIoniz" and not self.usePhIoniz: skip = True
			if row.strip() == "#IFKROME_usePhotoBins" and not self.photoBins > 0: skip = True
			if row.strip() == "#ENDIFKROME": skip = False

			if row.strip() == "#IFKROME_usePhotoOpacity" and not self.usePhotoOpacity: skip_opacity = True
			if row.strip() == "#ENDIFKROME_usePhotoOpacity": skip_opacity = False
			if row.strip() == "#IFKROME_photobin_heat" and not self.useHeatingPhoto: skip_heat = True
			if row.strip() == "#ENDIFKROME_photobin_heat": skip_heat = False

			if skip or skip_heat or skip_opacity: continue

			#replace pragma with kernel xsec function
			if "#KROME_xsecKernelFunction" in srow:
				if self.xsecKernelFunction == "":
					row = row.replace("#KROME_xsecKernelFunction","")
				else:
					fpart = "&\n* "+self.xsecKernelFunction+"(energy)"
					row = row.replace("#KROME_xsecKernelFunction",fpart)

			#precompute broadeinng in photochemistry
			if srow == "#KROME_broadening_shift_precalc" and self.useBroadening:
				row = "kt2 = 2d0*boltzmann_erg*Tgas\n"
				#row += "dshift(:) = 0d0\n"
				foundPartner = [] #unique photoreaction partners
				#loop on reactions to get photoreaction partners
				for rea in reacts:
					#use only photoreactions
					if not rea.hasXsecFile: continue
					#get partner index
					sidx = rea.reactants[0].fidx
					#skip if parnter already found
					if sidx in foundPartner: continue
					#compute thermal and turbulent broadening
					row += "dshift("+sidx+") = sqrt(kt2*imass("+sidx+") &\n"
					row += " + broadeningVturb2)/clight\n"
					#add partner to found partners list
					foundPartner.append(sidx)

			#write interpolated xsecs to file
			if srow == "#KROME_save_xsecs_to_file":
				row = ""
				for rea in reacts:
					if not rea.hasXsecFile: continue
					sidx = str(rea.idx)
					dumpFileName = rea.xsecFile.replace(".dat",".interp")
					row += "call save_xsec(\""+dumpFileName+"\","+str(rea.idxph)+")\n"

			#replace pragma with the initialization of the photorate table in bins
			if srow == "#KROME_load_xsecs_from_file":
				row = ""
				for rea in reacts:
					if not rea.hasXsecFile: continue
					sidx = str(rea.idx)
					row += "call load_xsec(\""+rea.xsecFile+"\", xsec"+sidx+"_val, xsec"+sidx+"_Emin,"
					row += " xsec"+sidx+"_n, xsec"+sidx+"_idE)\n"

			if srow == "#KROME_photobin_xsecs":
				phbinx = ""
				for rea in reacts:
					if rea.kphrate is None: continue
					phbinx += "\n!"+rea.verbatim+"\n"
					phbinx += "kk = 0d0\n"
					phbinx += "if(energy_eV>"+str(rea.Tmin)+".and.energy_eV<"+str(rea.Tmax)+") kk = "+rea.kphrate+"\n"
					phbinx += "!$omp parallel\n"
					phbinx += "photoBinJTab("+str(rea.idxph)+",j) = kk\n"
					phbinx += "!$omp end parallel\n"
				row = phbinx+"\n"
			#replace the energy treshold assuming that it is equal to Tmin
			elif srow == "#KROME_photobin_Eth":
				phbinx = ""
				for rea in reacts:
					if rea.kphrate is None: continue
					phbinx += "!$omp parallel\n"
					phbinx += "photoBinEth("+str(rea.idxph)+") = "+str(rea.Tmin)+" !"+rea.verbatim+"\n"
					phbinx += "!$omp end parallel\n"
				row = phbinx+"\n"
			#replace pragma with the opacity calculation as N_i*sigma_i for any species
			elif srow == "#KROME_photobin_opacity" and self.usePhotoOpacity:
				phbintau = ""
				#loop on the species looking for photorates
				for i in range(len(reacts)):
					rea = reacts[i]
					if rea.kphrate is None: continue
					phbintau += "tau = tau + photoBinJTab("+str(rea.idxph)+",j) * ncol(" \
					+ self.photoPartners[rea.idx].fidx+") !"\
					+ rea.verbatim+"\n"
				row = phbintau+"\n"
			#add qabs interpolation on photobins
			elif srow == "#KROME_interpolate_dust_qabs" and self.useDust:
				row = "call interp_qabs()\n"
			#call to subroutine that store the transition to bin correspondence
			elif srow == "#KROME_init_H2kpd_transition_map" and  self.indexH2photodissociation > -1:
				row = "call kpd_bin_map()\n"

			if row.strip() == "":
				fout.write("\n")
				continue
			if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	########################################################
	def makeTabs(self):
		buildFolder = self.buildFolder
		coevars = self.coevars #copy coefficient variables
		#********* TABS ****************
		print("- writing krome_tabs.f90...",)
		fh = open(self.srcFolder+"krome_tabs.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_tabs.f90","w")

		#include reactions that cannot be tabbed
		countNoTab = 0
		noTabReactions = "\n!non tabulated rates\n"
		storeOnceRates = ""
		sclist = [] #list of the temperature short-cuts
		for rea in self.reacts:
			if not rea.canUseTabs:

				if not rea.isStoreOnce:
					noTabReactions += rea.getRateF90(self,varname="coe_tab")+"\n\n"
					#noTabReactions += "coe_tab("+str(rea.idx)+") = "+rea.krate+"\n"
					countNoTab += 1
					sclist = get_Tshortcut(rea,sclist,coevars) #add short-cut if needed
				else:
					noTabReactions += "!"+rea.verbatim+"\n"
					noTabReactions += "coe_tab("+str(rea.idx)+") = rateEvaluateOnce("+str(rea.idx)+")\n\n"
					storeOnceRates += rea.getRateF90(self,varname="rateEvaluateOnce")+"\n\n"

		#if reactions that cannot be tabbed are found
		klist = kvars = shortcutVars = ""
		if countNoTab>0 and self.useTabs:
			if len(coevars) != 0:
				#define variables
				kvars = "real*8::"+(",".join([x.strip() for x in coevars.keys()]))
				#variables initialization
				klist = []
				for k,v in coevars.items():
					if "(" in k: k = k.split("(")[0]+"(:)" #replace array definition with (:) if necessary
					klist.append([k+" = "+v[1]+"\n",v[0]]) #this mess is to sort dict
				klist = sorted(klist, key=lambda x: x[1])
				klist = "".join([x[0] for x in klist])
			if len(sclist) != 0:
				shortcutVars = "real*8::"+(", ".join([x.split("=")[0].strip() for x in sclist]))

		#prepares the reaction modifiers
		#tokenize to replace k(:) with coe_tab(:)
		import tokenize
		try:
			from cStringIO import StringIO
		except:
			from io import StringIO
		kModifierFull = "" #string that will contains all the lines of the rate modifier
		for kmod in self.kModifier:
			#string to tokenizable object
			src = StringIO(kmod).readline
			#tokenize
			tokenized = tokenize.generate_tokens(src)
			kmodTok = "" #string with the k->coe_tab replaced
			for tok in tokenized:
				if tok[1] == "k":
					kmodTok += "coe_tab"
				else:
					kmodTok += tok[1]
			#comments are not tokenized
			if kmod[0] == "!":
				kmodTok = kmod
			#append the correct string
			kModifierFull += kmodTok+"\n"


		#replace pragmas
		skip = False
		for row in fh:
			if row.strip() == "#IFKROME_hasStoreOnceRates" and not self.hasStoreOnceRates:
				skip = True
			if row.strip() == "#IFKROME_useCustomCoe" and not self.useCustomCoe: skip = True
			if row.strip() == "#IFKROME_useTabs" and not self.useTabs: skip = True
			if row.strip() == "#IFKROME_useStandardCoe" and (self.useCustomCoe or self.useTabs): 
				skip = True

			if row.strip() == "#ENDIFKROME": skip = False

			if skip: continue
			#row = row.replace("#KROME_logTlow", "ktab_logTlow = log10(max("+str(self.TminAuto)+",2.73d0))")
			row = row.replace("#KROME_logTlow", "ktab_logTlow = log10(2.73d0)")
			row = row.replace("#KROME_logTup", "ktab_logTup = log10(1d9)")
			#row = row.replace("#KROME_logTup", "ktab_logTup = log10(min("+str(self.TmaxAuto)+",1d8))")
			row = row.replace("#KROME_shortcut_variables",shortcutVars)
			row = row.replace("#KROME_define_vars",kvars)
			row = row.replace("#KROME_init_vars",klist)
			row = row.replace("#KROME_noTabReactions",noTabReactions)
			row = row.replace("#KROME_rateModifier", kModifierFull)
			row = row.replace("#KROME_storeOnceRates",storeOnceRates)

			if row.strip() == "#KROME_Tshortcuts":
				ssc = ""
				for shortcut in sclist:
					if not self.useTabs: break
					ssc += shortcut + "\n"
				row = ssc

			if self.useCustomCoe:
				row = row.replace("#KROMEREPLACE_customCoeFunction", self.customCoeFunction)

			if len(row) == 0: continue
			if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	###############################
	def makeDust(self):
		buildFolder = self.buildFolder
		useDustT = self.useDustT
		usedTdust = self.usedTdust
		#********* DUST ****************
		print("- writing krome_dust.f90...",)
		fh = open(self.srcFolder+"krome_dust.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_dust.f90","w")

		dustPartnerIdx = dustQabs = dustOptInt = ""
		dustGrainDensity = dustEvaporationTemperature = dustKeyFraction = ""
		dustBulkDensity = {"C":2.25e0, "Si":3.13e0} #bulk density in g/cm3 (Zhukovska PhD.Th., Tab.4.1)
		dustTEvap = {"C":8.888e4, "Si":6.056e4} #binding energy in K (Evans+93, Tab.5.1)
		dustKey = {"C":"C", "Si":"Si"} #key element
		zsun = get_solar_abundances()
		useDustEvol = (self.useDustEvap or self.useDustGrowth or self.useDustSputter)
		if self.useDust:
			itype = 0 #dust type index
			#loop in dust types
			for dType in self.dustTypes:
				itype += 1 #increase index
				dustPartnerIdx += "krome_dust_partner_idx("+str(itype)+") = idx_"+dType+"\n"
				dustGrainDensity += "krome_grain_rho("+str(itype)+") = " \
					+ format_double(dustBulkDensity[dType])+"\n"
				i1mult = str(itype-1)+"*nd+1"
				i2mult = str(itype)+"*nd"
				if itype == 1:
					i1mult = "1"
					i2mult = "nd"
				if itype == 2:
					i1mult = "nd+1"
				dustEvaporationTemperature += "krome_dust_Tbind("+i1mult+":"+i2mult+") = " \
					+ format_double(dustTEvap[dType])+"\n"
				dustKeyFraction += "keyFrac("+str(itype)+") = 1d1**("+zsun[dustKey[dType]]+")\n"
				if useDustT or usedTdust: dustQabs += "call dust_load_Qabs(\"opt"+dType+".dat\"," \
					+str(itype)+")\n" #,dust_opt_Qabs_"+dType
			if useDustT or usedTdust: dustOptInt += "call dust_init_intBB()"
		if not useDustEvol: dustPartnerIdx = ""

		skip = skipPhotoDust = skipChemisorption = skipdTdust = skipDustEvol = False
		for row in fh:
			srow = row.strip()
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_usePhotoDust_3D" and not self.usePhotoDust_3D: skip = True
			if srow == "#IFKROME_dust_table_2D" and not(self.dustTableDimension=="2D") or self.usePhotoDust_3D: skip = True
			if srow == "#IFKROME_dust_table_3D" and not(self.dustTableDimension=="3D"): skip = True
			if srow == "#ENDIFKROME": skip = False

			if srow == "#IFKROME_useChemisorption" and not self.useChemisorption: skipChemisorption = True
			if srow == "#ENDIFKROME_useChemisorption": skipChemisorption = False

			if srow == "#IFKROME_useDustEvol" and not useDustEvol: skipDustEvol = True
			if srow == "#ENDIFKROME_useDustEvol": skipDustEvol = False

			if srow == "#IFKROME_usePhotoDust" and not (self.photoBins > 0): skipPhotoDust = True
			if srow == "#ENDIFKROME_usePhotoDust": skipPhotoDust = False

			if srow == "#IFKROME_usedTdust" and not self.usedTdust: skipdTdust = True
			if srow == "#ENDIFKROME_usedTdust": skipdTdust = False

			if skipChemisorption: continue
			if skipdTdust: continue
			if skipPhotoDust: continue
			if skipDustEvol: continue
			if skip: continue

			row = row.replace("#KROME_dust_grain_density", dustGrainDensity)
			row = row.replace("#KROME_dust_binding_energy", dustEvaporationTemperature)
			row = row.replace("#KROME_dust_seed", self.dustSeed)
			row = row.replace("#KROME_dustPartnerIndex", dustPartnerIdx)
			row = row.replace("#KROME_init_Qabs", dustQabs)
			row = row.replace("#KROME_opt_integral", dustOptInt)
			row = row.replace("#KROME_dust_key_fraction", dustKeyFraction)

			if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	################################
	def makeCoolingGH(self):
		buildFolder = self.buildFolder

		#*********COOLING GH****************
		print("- writing krome_coolingGH.f90...",)
		fh = open(self.srcFolder+"krome_coolingGH.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_coolingGH.f90","w")

		skip = False
		#loop on source to replace pragmas
		for row in fh:
			srow = row.strip()
			#cooling pragmas
			if srow == "#IFKROME_useCoolingGH" and not self.useCoolingGH: skip = True
			if srow == "#ENDIFKROME": skip = False

			if skip: continue

			if not srow.startswith("#"):
				fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")

	################################
	def makeCooling(self):
		from math import sqrt
		buildFolder = self.buildFolder
		reacts = self.reacts
		specs = self.specs
		#*********COOLING****************
		#write header in krome_cooling.f90
		print("- writing krome_cooling.f90...",)
		fh = open(self.srcFolder+"krome_cooling.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_cooling.f90","w")

		#create coefficients, flux, and cooling functions for dH cooling
		i = 0
		dH_varsa = []
		dH_coe = dH_cool = ""
		if self.useCoolingdH:
			idxs = []
			for rea in reacts:
				if rea.idx in idxs: continue #skip reactions with the same index
				idxs.append(rea.idx)
				if rea.dH is not None and rea.dH < 0e0:
					i += 1 #count cooling reactions
					kvar = "k"+str(i) #local variable for coefficient
					dH_varsa.append(kvar) #variables array
					dH_coe += "!"+rea.verbatim+"\n" #reaction comment
					dH_coe += kvar+" = 0.d0\n"
					dH_coe += "if(Tgas."+self.TlimitOpLow+"."+rea.Tmin+" .and. Tgas."+self.TlimitOpHigh+"."+rea.Tmax+") then\n"
					dH_coe += kvar+" = "+rea.krate+"\n" #evaluate reation coefficient
					dH_coe += "end if\n\n" #evaluate reation coefficient
					dH_cool += "cool = cool + "+kvar+"*"+("*".join(["n("+x.fidx+")" for x in rea.reactants])) #evautate cooling
					dH_cool += "*"+(str(abs(rea.dH))).replace("e","d")+"\n"

		#store species names
		speciesNames = [x.name.upper() for x in self.specs]

		#bremsstrahlung (free-free) for all the ions as charge**2*n_ion
		bms_ions = "bms_ions ="
		#look for ions (charge>0)
		for x in specs:
			charge = x.charge #store species charge
			if not x.is_atom: continue #only atoms for free-free
			if charge > 0:
				mult = "" #multiplication factor
				if charge > 1: mult = str(charge*charge)+".d0*" #multipy by the square of the charge
				bms_ions += " +"+mult+"n("+x.fidx+")" #add Z^2*abundance

		#load data for free-bound
		fhfb = open("data/ip.dat")
		fbdata = []
		for row in fhfb:
			srow = row.strip()
			if srow == "": continue
			if srow[0] == "#": continue
			arow = [x for x in srow.split(" ") if x!=""]
			#Z: atomic number, ion: ionization degree (e.g. HII=1), energy_eV: ioniz potential, n0: principal quantum number
			fbdata.append({"Z":int(arow[0]), "ion":int(arow[1]), "energy_eV":float(arow[5]), "n0":int(arow[6])})

		skip = skip_nleq = skip_dTdust = skipPhotoDust = skip_tab_2D = skip_tab_3D = False
		skip_speciesH2 = False
		useCoolingZ = self.useCoolingZ
		#loop on source to replace pragmas
		for row in fh:
			srow = row.strip()
			usingTd = (self.usedTdust or self.useDustT)
			#cooling pragmas
			if srow == "#IFKROME_useCoolingZ" and (not useCoolingZ or self.useCoolingZExtended): skip = True
			if srow == "#IFKROME_useCoolingZ_function" and not useCoolingZ: skip = True
			if srow == "#IFKROME_useCoolingdH" and (not self.useCoolingdH or len(dH_varsa) == 0): skip = True
			if srow == "#IFKROME_useCoolingDust" and (not self.useCoolingDust or not(usingTd)): skip = True
			if srow == "#IFKROME_useCoolingDustNoTdust" and (usingTd or not self.useCoolingDust): skip = True
			if srow == "#IFKROME_useCoolingDustTabs" and not self.dustTabsCool: skip = True
			if srow == "#IFKROME_useCoolingAtomic" and not self.useCoolingAtomic: skip = True
			if srow == "#IFKROME_useCoolingH2" and not self.useCoolingH2: skip = True
			if srow == "#IFKROME_useCoolingH2GP" and not self.useCoolingH2GP98: skip = True
			if srow == "#IFKROME_useCoolingHD" and not self.useCoolingHD: skip = True
			if srow == "#IFKROME_useCoolingCompton" and not self.useCoolingCompton: skip = True
			if srow == "#IFKROME_useCoolingExpansion" and not self.useCoolingExpansion: skip = True
			if srow == "#IFKROME_useCoolingCIE" and not self.useCoolingCIE: skip = True
			if srow == "#IFKROME_useCoolingFF" and not self.useCoolingFF: skip = True
			if srow == "#IFKROME_useCoolingCO" and not self.useCoolingCO: skip = True
			if srow == "#IFKROME_useCoolingOH" and not self.useCoolingOH: skip = True
			if srow == "#IFKROME_useCoolingH2O" and not self.useCoolingH2O: skip = True
			if srow == "#IFKROME_useCoolingHCN" and not self.useCoolingHCN: skip = True
			if srow == "#IFKROME_useCoolingZCIENOUV" and not self.useCoolingZCIENOUV: skip = True
			if srow == "#IFKROME_useCoolingZCIE" and not (self.useCoolingZCIE or self.useCoolingZExtended): skip = True
			if srow == "#IFKROME_useCoolingZCIE_function" and not self.useCoolingZCIE: skip = True
			if srow == "#IFKROME_useCoolingZExtended" and not self.useCoolingZExtended: skip = True
			if srow == "#IFKROME_useCoolingContinuum" and not self.useCoolingCont: skip = True
			if srow == "#IFKROME_useCoolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_useLAPACK" and not self.needLAPACK: skip = True #skip calls to LAPACK
			if srow == "#IFKROME_useH2esc_omukai" and (self.H2opacity != "OMUKAI"): skip = True
			if srow == "#IFKROME_use_NLEQ" and not self.useNLEQ: skip_nleq = True #skip calls to NLEQ
			if srow == "#IFKROME_usedTdust" and not self.usedTdust: skip_dTdust = True
			if srow == "#IFKROME_usePhotoDust" and not (self.photoBins > 0): skipPhotoDust = True
			if srow == "#IFKROME_hasH" and "H" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_hasHp" and "H+" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_hasHe" and "HE" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_hasHep" and "HE+" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_hasHepp" and "HE++" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_hasElectrons" and "E" not in speciesNames: skip_speciesH2 = True
			if srow == "#IFKROME_dust_table_2D" and not(self.dustTableDimension=="2D"): skip_tab_2D = True
			if srow == "#IFKROME_dust_table_3D" and not(self.dustTableDimension=="3D"): skip_tab_3D = True

			if srow == "#ENDIFKROME_usedTdust": skip_dTdust = False
			if srow == "#ENDIFKROME_use_NLEQ": skip_nleq = False
			if srow == "#ENDIFKROME_usePhotoDust": skipPhotoDust = False
			if srow == "#ENDIFKROME_hasH": skip_speciesH2 = False
			if srow == "#ENDIFKROME_hasHp": skip_speciesH2 = False
			if srow == "#ENDIFKROME_hasHe": skip_speciesH2 = False
			if srow == "#ENDIFKROME_hasHep": skip_speciesH2 = False
			if srow == "#ENDIFKROME_hasHepp": skip_speciesH2 = False
			if srow == "#ENDIFKROME_hasElectrons": skip_speciesH2 = False
			if srow == "#ENDIFKROME_dust_table_2D": skip_tab_2D = False
			if srow == "#ENDIFKROME_dust_table_3D": skip_tab_3D = False
			if srow == "#ENDIFKROME": skip = False

			if skip or skip_nleq or skip_dTdust or skip_speciesH2 or \
				skipPhotoDust or skip_tab_2D or skip_tab_3D: continue

			#replace the small value for rates according to the maximum number of products
			if "#KROME_small" in srow:
				if self.useTabs:
					fout.write(srow.replace("#KROME_small","0d0")+"\n")
					continue
				maxprod = 0
				for x in reacts:
					maxprod = max(len(x.products),maxprod)
				mysmall = "1d-40/("+("*".join(["nmax"]*maxprod))+")"
				if maxprod == 0: mysmall = "0d0"
				fout.write(srow.replace("#KROME_small",mysmall)+"\n")
				continue

			#individual floors for different cooling functions
			if "#KROME_floor" in srow:
				pragma = "#"+srow.split("#")[1].split(")")[0] #floor PRAGMA, starts with #, ends with )
				floorName = pragma.replace("#KROME_floor","").strip() #get floor name
				floorNameFunction = floorName
				if floorName == "Z_EXTENDED": floorNameFunction = "Z"
				#check if name is in the individual cooling list, if so replace with floor function
				if floorName in self.individualCoolingFloors:
					coolingFunction =  "- cooling_"+floorNameFunction+"(n(:),phys_Tfloor)"
					fout.write(srow.replace("#KROME_floor"+floorName, coolingFunction)+"\n")
				else:
					fout.write(srow.replace("#KROME_floor"+floorName, "")+"\n")
				continue

			if row.strip() == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))
			elif row.strip() == "#KROME_escape_vars":
				if len(self.fcn_levs) > 0:
					fcn_levs = sorted(self.fcn_levs)
					bvars = "real*8::"+(",".join(["beta"+str(x)+"("+str(x)+","+str(x)+")" for x in fcn_levs]))+"\n"
					bvars += "real*8::"+(",".join(["Besc"+str(x)+"("+str(x)+","+str(x)+")" for x in fcn_levs]))+"\n"
					bvars += "real*8::"+(",".join(["MMesc"+str(x)+"("+str(x)+","+str(x)+")" for x in fcn_levs]))+"\n"
					bvars += "real*8::ntotcoll\n"
					fout.write(bvars)
			elif row.strip() == "#KROME_fcn_cases":
				if len(self.fcn_levs) > 0:
					fcn_levs = sorted(self.fcn_levs)
					for i in range(len(fcn_levs)):
						preif = ""
						if i > 0: preif = "else "
						fcase = preif+"if(n=="+str(fcn_levs[i])+") then\n"
						fcase += "call fcn_"+str(fcn_levs[i])+"(n,x(:),f(:))\n"
						fout.write(fcase)
					fout.write("else\n")
					fout.write("print *,\"ERROR: unknown case in fcn subroutine!\", n\n")
					fout.write("stop\n")
					fout.write("end if\n")
			#prepare free-bound calculation
			elif row.strip() == "#KROME_FB_cooling_data":
				for x in self.specs:
					charge = x.charge #species charge
					Zatom = x.zatom #species atomic number
					if not x.is_atom: continue #skip molecules
					if charge <= 0: continue #skip neutral and anions
					#search the data in the table loaded above
					dataFound = False
					for dataip in fbdata:
						if dataip["Z"] == Zatom and dataip["ion"] == charge:
							mydataip = dataip #store the line
							dataFound = True #update found flag
							break #break the loop
					#raise error if no data found
					if not dataFound: sys.exit("ERROR: no data found for "+x.name)
					#start calculation (precompute most of the known stuff)
					n0 = mydataip["n0"] #principal quantum number
					ion = mydataip["ion"] #ionization degree (charge)
					E_eV = mydataip["energy_eV"] #ionization potential eV
					z0 = sqrt(E_eV/1.359808e1)*n0
					E0 = E_eV/1e3 #keV
					En1 = ion**2*1.359808e1/(n0+1)**2/1e3 #keV
					l0 = 911.9e0*(n0/z0)**2 #angstrom
					ln1 = 911.9e0/ion**2*(float(n0+1))**2 #angstrom
					dzion = Zatom-ion
					if dzion > 22:
						zeta0 = -dzion+55e0
					elif dzion <= 22 and dzion > 8:
						zeta0 = -dzion+27e0
					elif dzion <= 8 and dzion > 0:
						zeta0 = -dzion+9e0
					else:
						zeta0 = -dzion+1e0
					print(x.name)
					print("f2 = "+format_double(.9*zeta0*z0**4/n0**5)+"*exp("
						  +format_double(0.1578e0*z0**2/n0**2)+"*invT6)")
					print("gfb = "+"0.1578d0*n("+x.fidx+")*f2*invT6")

			elif row.strip() == "#KROME_nZrate":
					fout.write("integer,parameter::nZrate="+str(self.coolZ_nkrates)+"\n")
			elif row.strip() == "#KROME_coolingZ_call_functions":
				for x in self.coolZ_functions:
					fout.write("cool = cool + "+x[0]+"(n(:),inTgas,k(:))\n")
			elif row.strip() == "#KROME_coolingZ_custom_vars":
				for x in self.coolZ_vars_cool:
					fout.write(x[0]+" = "+x[1]+"\n")
			elif row.strip() == "#KROME_coolingZ_declare_custom_vars":
				vcool = []
				for x in self.coolZ_vars_cool:
					vcool.append(x[0])
				if len(vcool) > 0: fout.write("real*8::"+(",".join(vcool))+"\n")
			elif row.strip() == "#KROME_coolingZ_functions":
				for x in self.coolZ_functions:
					fout.write(x[1]+"\n")
			elif row.strip() == "#KROME_coolingZ_rates":
				for x in self.coolZ_rates:
					fout.write(x+"\n\n")
			elif row.strip() == "#KROME_coolingZ_popvars":
				if len(self.coolZ_poplevelvars) > 0:
					for popvar in self.coolZ_poplevelvars:
						fout.write("real*8::"+popvar+"\n")
					for popvar in self.coolZ_poplevelvars:
						funct_name = popvar.split("(")[0]
						fout.write("!$omp threadprivate("+funct_name+")\n")
			elif row.strip() == "#KROME_popvar_dump":
				if len(self.coolZ_poplevelvars) > 0:
					for popvar in self.coolZ_poplevelvars:
						funct_name = popvar.split("(")[0]
						metal_name = funct_name.split("_")[-1]
						fout.write("!"+popvar+"\n")
						fout.write("do i=1,size("+funct_name+")\n")
						fout.write(" write(nfile,'(a8,I5,3E17.8e3)') \""+metal_name+"\", i, Tgas, "+\
							funct_name+"(i), sum("+funct_name+"(:))\n")
						fout.write("end do\n\n")

			elif "#KROME_custom_cooling_expr" in row.strip():
				coolAll = ""
				if len(self.customCoolList) != 0:
					coolAll = " cooling_custom = " + ("+ &\n".join(self.customCoolList))
				fout.write(row.replace("#KROME_custom_cooling_expr", coolAll))
			elif row.strip() == "#KROME_custom_cooling_var_define":
				vardef = ""
				if len(self.coolVars) > 0: vardef = "real*8::"+(",".join(self.coolVars))
				fout.write(vardef+"\n")
			elif row.strip() == "#KROME_custom_cooling_var":
				klist = [[k+" = "+v[1]+"\n",v[0]] for k,v in self.coolVars.items()] #this mess is to sort dict
				klist = sorted(klist, key=lambda x: x[1])
				fout.write("".join([x[0] for x in klist]))
			else:
				#replace pragma for total metals
				row = row.replace("#KROME_tot_metals", self.totMetals)

				if self.H2opacity == "RIPAMONTI":
					#thick case (note that 1.25d-10 = 1/8e9)
					row = row.replace("#KROME_H2opacity", "&\n* min(1.d0, max(1.25d-10 * sum(n(1:nmols)),1d-40)**(-.45))")
				elif self.H2opacity == "OMUKAI":
					#thick case using table provided by Omukai (priv. comm. 2014)
					row = row.replace("#KROME_H2opacity", "&\n* H2opacity_omukai(Tgas, n(:))")
				else:
					#thin case
					row = row.replace("#KROME_H2opacity", "")

				#replace pragma for dH_cooling
				if self.useCoolingdH:
					row = row.replace("#KROME_vars","real*8::"+(",".join(dH_varsa))+"\n")
					row = row.replace("#KROME_rates",dH_coe)
					row = row.replace("#KROME_dH_cooling",dH_cool)
				#replace pragma of bremsstrahlung ions
				row = row.replace("#KROME_brem_ions",bms_ions)

				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")


	##################################################################
	def makeHeating(self):
		from math import log10

		reacts = self.reacts
		buildFolder = self.buildFolder
		#*********HEATING****************
		#write header in krome_heating.f90
		print("- writing krome_heating.f90...",)

		fh = open(self.srcFolder+"krome_heating.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_heating.f90","w")

		#create coefficients, flux, and cooling functions for dH cooling
		i = 0
		dH_varsa = []
		dH_coe = dH_heat = ""
		idxs = []
		if self.useHeatingdH:
			for rea in reacts:
				if rea.idx in idxs: continue #skip reactions with the same index
				idxs.append(rea.idx)
				if rea.dH is not None and rea.dH > 0e0:
					i += 1 #count heating reactions
					kvar = "k" + str(i) #local variable for coefficient
					dH_varsa.append(kvar) #variables array
					dH_coe += kvar+" = 0.d0\n"
					dH_coe += "if(Tgas."+self.TlimitOpLow+"."+rea.Tmin+" .and. Tgas."+self.TlimitOpHigh+"."+rea.Tmax+") then\n"
					dH_coe += kvar+" = "+rea.krate+"\n" #evaluate reation coefficient
					dH_coe += "end if\n\n" #evaluate reation coefficient
					dH_heat += "heat = heat + "+kvar+"*"+("*".join(["n("+x.fidx+")" for x in rea.reactants])) #evautate heating
					dH_heat += "*"+(str(abs(rea.dH))).replace("e","d")+"\n"


		#build H2 heating according to the rates
		HChem = HChemDust = ""
		sclist = []
		if self.useHeatingChem or self.useCoolingChem or self.useCoolingDISS:
			RPK = []
			#RPK is the list of the heating/cooling processes as
			# [product_list, reactant_list, fortran_rate, heating/cooling_flag]
			if self.useHeatingChem:
				RPK.append([["H","H","H"], ["H2","H"], "4.48d0*h2heatfac","H"])
				RPK.append([["H2","H","H"], ["H2","H2"], "4.48d0*h2heatfac","H"])
				RPK.append([["H-","H"], ["H2","E"], "3.53d0*h2heatfac","H"])
				RPK.append([["H2+","H"], ["H2","H+"], "1.83d0*h2heatfac","H"])
			if self.useCoolingChem or self.useCoolingDISS:
				RPK.append([["H2","H"], ["H","H","H"], "-4.48d0","C"])
				RPK.append([["H2","E"], ["H","H","E"], "-4.48d0","C"])
				RPK.append([["H2","H2"], ["H2","H","H"], "-4.48d0","C"])

			Rref = []
			Pref = []
			kref = []
			href = []
			for rpk in RPK:
				Rref.append(sorted(rpk[0])) #list of the reactants
				Pref.append(sorted(rpk[1])) #list fo the products
				kref.append(rpk[2]) #rate coefficient
				href.append(rpk[3]) #heating/cooling flag

			idxs = []
			for rea in reacts:
				if rea.idx in idxs: continue #skip reactions with the same idx
				idxs.append(rea.idx)
				R = sorted([x.name for x in rea.reactants])
				P = sorted([x.name for x in rea.products])
				rmult = ("*".join(["n("+x.fidx+")" for x in rea.reactants]))
				for i in range(len(Rref)):
					if Rref[i] == R and Pref[i] == P:
						headchem = "!"+rea.verbatim + " ("+("heating" if href[i]=="H"  else "cooling") + ")\n"
						hasTlim = (rea.hasTlimitMin or rea.hasTlimitMax) #Tmin or Tmax are present
						tklim = ""
						if self.useTlimits and hasTlim:
							tklim = "if("
							if rea.hasTlimitMin: tklim += "Tgas." + rea.TminOp + "."  + rea.Tmin #Tmin is present
							if rea.hasTlimitMin and rea.hasTlimitMax: tklim += " .and. " #Tmax and Tmin are present
							if rea.hasTlimitMax: tklim += "Tgas." + rea.TmaxOp + "." + rea.Tmax #Tmax is present
							tklim += ") then\n"
						HChem += headchem + tklim + "HChem = HChem + k("+str(rea.idx)+") * ("+kref[i] + "*"+rmult+")\n"
						if self.useTlimits and hasTlim: HChem += "end if\n\n"
						break
			if self.useDustH2 or self.dustTabsH2 or self.useDustH2const:
				HChemDust += "HChem = HChem + nH2dust * (4.2d0*h2heatfac + 0.2d0)\n"
			if self.indexSolomon > 0:
				HChem += "HChem = HChem + k("+str(self.indexSolomon)+") * (18.7d0*h2heatfac + 0.4d0)*n(idx_H2)\n"

		#build heating terms for photoionization
		pheatvars = []
		if self.usePhIoniz:
			for react in reacts:
				#phstuff = get_ph_stuff(react)
				#if(phstuff==None): continue
				#reaname = phstuff["reaname"]
				#reag = react.reactants
				#fake_opacity = ""
				#if(self.useFakeOpacity): fake_opacity = " * exp(-n(" + reag[0].fidx + ") / n0)"
				if react.idxph <= 0: continue
				prefac = ""
				if react.reactants[0].name in \
					["O","Oj","C","Cj","Si","Sij","Fe","Fej","Fejj","Mg","Mgj"]:
						prefac = "f2 *"
				pheatvars.append(prefac+"photoBinHeats("+str(react.idxph)+") * n(" + react.reactants[0].fidx + ")")

		#replace pragma with strings built above
		skip = False
		for row in fh:
			srow = row.strip()
			if row.strip() == "#KROME_header":

				fout.write(get_licence_header(self.version, self.codename,self.shortHead))
			else:
				if row.strip() == "#IFKROME_useHeatingCR" and not self.useHeatingCR: skip = True
				if row.strip() == "#IFKROME_useHeatingdH" and (not self.useHeatingdH or len(dH_varsa)==0): skip = True
				if row.strip() == "#IFKROME_useHeatingCompress" and not self.useHeatingCompress: skip = True
				if row.strip() == "#IFKROME_useHeatingPhoto" and not self.useHeatingPhoto: skip = True
				if row.strip() == "#IFKROME_useHeatingPhotoAv" and not self.useHeatingPhotoAv: skip = True
				if row.strip() == "#IFKROME_useHeatingPhotoDust" and not self.useHeatingPhotoDust: skip = True
				if row.strip() == "#IFKROME_useHeatingPhotoDustNet" and not self.useHeatingPhotoDustNet: skip = True
				if row.strip() == "#IFKROME_useHeatingXRay" and not self.useHeatingXRay: skip = True
				if row.strip() == "#IFKROME_useHeatingVisc" and not self.useHeatingVisc: skip = True
				#if(row.strip() == "#IFKROME_useHeatingPumpH2" and not(self.useHeatingPumpH2)): skip = True
				if row.strip() == "#IFKROME_useHeatingZCIE" and not self.useCoolingZCIE: skip = True
				if row.strip() == "#IFKROME_useHeatingZExtended" and not self.useCoolingZExtended: skip = True
				if row.strip() == "#IFKROME_useHeatingGH" and not self.useCoolingGH: skip = True
				skipBool = not self.useHeatingChem and not self.useCoolingChem and not self.useCoolingDISS
				if row.strip() == "#IFKROME_useHeatingChem" and skipBool: skip = True

				if row.strip() == "#ENDIFKROME": skip = False

				if skip: continue

				if "#KROME_custom_heating_expr" in row.strip():
					heatAll = ""
					if len(self.customHeatList) != 0: heatAll = " heat_custom = " + ("+ &\n".join(self.customHeatList))
					fout.write(row.replace("#KROME_custom_heating_expr",heatAll))
				if row.strip() == "#KROME_custom_heating_var_define":
					vardef = ""
					if len(self.heatVars) > 0: vardef = "real*8::"+(",".join(self.heatVars))
					fout.write(vardef+"\n")
				if row.strip() == "#KROME_custom_heating_var":
					klist = [[k+" = "+v[1]+"\n",v[0]] for k,v in self.heatVars.items()] #this mess is to sort dict
					klist = sorted(klist, key=lambda x: x[1])
					fout.write("".join([x[0] for x in klist]))

				#replace the small value for rates according to the maximum number of products
				if "#KROME_small" in row:
					if self.useTabs:
						fout.write(row.replace("#KROME_small", "0d0")+"\n")
						continue
					maxprod = 0
					for x in reacts:
						maxprod = max(len(x.products),maxprod)
					mysmall = "1d-40/("+("*".join(["nmax"]*maxprod))+")"
					if maxprod == 0: mysmall = "0d0"
					row = row.replace("#KROME_small",mysmall)

				#replace cosmic ray heating
				if "#KROME_heatingCR" in srow:

					#creates fit from arXiv:1502.03380
					#maximum density (after this is assumed constant)
					maxH2 = 1e10 #cm-3
					#coeffients for the polynomial
					coeffCR = [9.1462198642562, -0.443120145068494, 0.601271617560071, -0.0710284101055109, 0.00242079494592405]
					#coefficients for the linear part (extrapolation)
					coeffCRLin = [1.4510849135088, 7.23277032061136]

					#number of polynomial coefficients
					ncoef = len(coeffCR)
					#find minimum density (i.e. when linear function goes to zero, hence before is zero)
					minH2 = 1e1**(-coeffCRLin[1]/coeffCRLin[0])
					#evaluate maximum to use as a constant after maxH2
					evalMax = sum([coeffCR[i]*log10(maxH2)**i for i in range(ncoef)])

					#prepare polynomial
					QH2fit = " QH2 = " + (" &\n+ ".join([str(coeffCR[i])+"*logH2**"+str(i) for i in range(ncoef)]))
					QH2fit = QH2fit.replace("*logH2**0", "")
					QH2fit = QH2fit.replace("**1", "")
					QH2fit = QH2fit.replace("+ -", "- ")

					#write fitting function with conditions
					QH2function = "!automatically generated fit function\n"
					QH2function += "! for H2 ionization, units: eV\n"
					QH2function += "! data from arXiv:1502.03380\n"
					QH2function += "if(n(idx_H2)>1d10) then\n"
					QH2function += " QH2 = "+str(evalMax)+"\n"
					QH2function += "else if((n(idx_H2).ge."+str(minH2)+").and.(n(idx_H2)<1d5)) then\n"
					QH2function += " QH2 = "+str(coeffCRLin[0])+"*logH2 + "+str(coeffCRLin[1])+"\n"
					QH2function += "else if(n(idx_H2)<"+str(minH2)+") then\n"
					QH2function += " QH2 = 0d0\n"
					QH2function += "else\n"
					QH2function += QH2fit+"\n"
					QH2function += "end if\n\n"
					QH2function += "!convert eV to erg\n"
					QH2function += "QH2 = QH2 * ev2erg\n"

					#prepare heating
					CRheat = QH2function + "\n\n"
					#loop on reactions
					for rea in self.reacts:
						if not rea.isCR: continue
						CRheat += "!"+rea.verbatim+"\n"
						reactantNames = [x.name.upper() for x in rea.reactants]
						productNames = sorted([x.name.upper() for x in rea.products])
						if reactantNames == ["H"] and  productNames == sorted(["H+","E"]):
							CRheat += "heat_CR = heat_CR + k("+str(rea.idx)+") * n("+rea.reactants[0].fidx+") * QH\n\n"
						elif reactantNames == ["H2"] and productNames == sorted(["H2+","E"]):
							CRheat += "heat_CR = heat_CR + k("+str(rea.idx)+") * n("+rea.reactants[0].fidx+") * QH2\n\n"
						elif reactantNames == ["HE"] and productNames == sorted(["HE+","E"]):
							CRheat += "heat_CR = heat_CR + k("+str(rea.idx)+") * n("+rea.reactants[0].fidx+") * QHe\n\n"
						else:
							CRheat += "heat_CR = heat_CR + k("+str(rea.idx)+") * n("+rea.reactants[0].fidx+") * Hfact\n\n"
					row = row.replace("#KROME_heatingCR",CRheat)

				if len(pheatvars) > 0:
					row = row.replace("#KROME_photo_heating", "photo_heating = " + (" &\n+ ".join(pheatvars)))
				row = row.replace("#KROME_HChem_terms", HChem) #replace chemical heating terms
				row = row.replace("#KROME_HChem_dust", HChemDust) #replace chemical heating for dust

				#replace metallicity
				if "#KROME_photoDustZ" in row:
					zFound = False
					for zz in ["Fe","C","O","Si"]:
						for x in self.specs:
							if zz == x.name:
								zFound = True
								dustZ = zz
								break
						if zFound: break
					if not zFound:
						row = row.replace("#KROME_photoDustZ","0d0")
					else:
						row = row.replace("#KROME_photoDustZ","1d1**get_metallicity"+zz+"(n(:))")

				#replace correct dissociation rates
				if "#KROME_RdissH2" in row:
					rdh2Found = False
					for rea in self.reacts:
						R = sorted([x.name for x in rea.reactants])
						P = sorted([x.name for x in rea.products])
						if R == ["H2"] and P == ["H","H"]:
							rateDissH2 = "k("+str(rea.idx)+")"
							rdh2Found = True
							break
					#check if rate photodissiocation rate is present in the network
					if not rdh2Found:
						print("ERROR: if you use PHOTOAV heating you should have")
						print(" H2 photodissiocation rate in your chemical network!")
						sys.exit()

					row = row.replace("#KROME_RdissH2", rateDissH2) #replace pragma with H2 photodissociation rate

				#add attenuation from G0 and Av for photoelectric effect
				if row.strip() == "#KROME_GhabG0":
					row = "Ghab  = "+self.photoDustVarG0+"\n"
					if self.photoDustVarG0 == "": row = "\n"
				if row.strip() == "#KROME_GhabAv":
					row = "Ghab  = Ghab * exp(-2.5*"+self.photoDustVarAv+")\n"
					if self.photoDustVarAv == "": row = "\n"

				#replace shortcuts for temperature
				if row.strip() == "#KROME_Tshortcuts":
					ssc = ""
					for shortcut in sclist:
						ssc += shortcut + "\n"
					row = ssc
				#replace pragma for dH_heating
				if self.useHeatingdH:
					row = row.replace("#KROME_vars","real*8::"+(",".join(dH_varsa))+"\n")
					row = row.replace("#KROME_rates",dH_coe)
					row = row.replace("#KROME_dH_heating",dH_heat)

				if len(row) == 0: continue
				if row[0] != "#": fout.write(row)

		if not self.buildCompact:
			fout.close()
		print("done!")


	########################################
	def makeODE(self):
		buildFolder = self.buildFolder
		dustArraySize = self.dustArraySize
		dustTypes = self.dustTypes
		nmols = self.nmols
		specs = self.specs
		#dnw_grouped = self.dnw_grouped
		dnw = self.dnw
		neq = len(specs)
		solver_MF = self.solver_MF
		Tgas_species = self.Tgas_species
		coevarsODE = self.coevarsODE

		#*********ODE****************
		#write parameters in krome_ode.f90
		print("- writing krome_ode.f90...",)

		fh = open(self.srcFolder+"krome_ode.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_ode.f90","w")

		#check if electrons are present
		hasElectrons = False
		electronIdx = -1
		for x in self.specs:
			if x.name == "E":
				hasElectrons = True
				electronIdx = x.idx #store electron index
				break

		#string for the function computing dust H2 formation
		dustH2 = "\n"
		if self.useDustH2:
			iType = 0
			ndust = self.dustArraySize*len(self.dustTypes)
			for dType in self.dustTypes:
				ilow = nmols + iType * self.dustArraySize + 1
				iup = nmols + (iType + 1) * self.dustArraySize
				limits = str(ilow) + ":" + str(iup) #absolute limits
				limits_rel = str(ilow-nmols) + ":" + str(iup-nmols) #realtive limits
				limits_tdust = str(ilow+ndust) + ":" + str(iup+ndust) #realtive limits
				dustH2 += "nH2dust = nH2dust + krome_H2_dust(xdust(" + limits_rel + "), Tgas,"
				dustT = " krome_dust_T(" + limits_rel + ")"
				if self.usedTdust: dustT = " n(" + limits_tdust + ")"
				dustH2 += dustT+", n(idx_H), H2_eps_"+dType+", vgas)\n"
				iType += 1
		elif self.useDustH2const:
		        #H2 on dust from Jura constant value
			dustH2 +="nH2dust = nH2dust + H2_dustJura(n(:))"

		#H2 on dust from tables
		if self.dustTabsH2:
			dustH2 = "ntot = sum(n(1:nmols))\n"
			if self.dustTableDimension == "2D":
				dustH2 += "nH2dust = get_mu(n(:)) * n(idx_H) * 1d1**fit_anytab2D(dust_tab_ngas(:),\
					dust_tab_Tgas(:), &\n\
					dust_tab_H2(:,:), dust_mult_ngas, dust_mult_Tgas, &\n\
					log10(ntot), log10(Tgas)) * ntot"
			elif self.dustTableDimension == "3D":
				dustH2 += "nH2dust = get_mu(n(:)) * n(idx_H) * 1d1**fit_anytab3D(dust_tab_ngas(:),\
					dust_tab_Tgas(:), dust_tab_AvVariable(:), &\n\
					dust_tab_H2(:,:,:), dust_mult_ngas, dust_mult_Tgas, dust_mult_AvVariable, &\n\
					log10(ntot), log10(Tgas), dust_table_AvVariable_log) * ntot"
			else:
				sys.exit("ERROR: table dimension unknown!")


		#replace pragma with built strings
		skip = False
		for row in fh:
			srow = row.strip()
			if srow == "#IFKROME_use_thermo" and (not self.use_thermo or not self.useODEthermo): skip = True
			if srow == "#IFKROME_use_thermo_toggle" and not self.useThermoToggle: skip = True
			if srow == "#IFKROME_report" and not self.doReport: skip = True
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_usedTdust" and not self.usedTdust: skip = True
			if srow == "#IFKROME_shieldHabingDust" and not self.shieldHabingDust: skip = True

			if srow == "#ENDIFKROME": skip = False

			if skip: continue

			coolPragmaFound = False
			#include cooling cmb floor if necessary
			if "#KROME_coolfloor" in srow:
				coolPragmaFound = True
				if self.useCoolFloor:
					srow = srow.replace("#KROME_coolfloor"," + cooling(n(:), phys_Tfloor)")
				else:
					srow = srow.replace("#KROME_coolfloor","")

			#replace quenching function for cooling
			if "#KROME_coolingQuench" in srow:
				coolPragmaFound = True
				if self.coolingQuench < 0e0:
					srow = srow.replace("#KROME_coolingQuench","")
				else:
					qfunc = " &\n * 0.5d0 * (tanh(Tgas - "+format_double(self.coolingQuench)+") + 1d0)"
					srow = srow.replace("#KROME_coolingQuench", qfunc)

			#quench and cmbfloor are on the same line so write the replacements togheter
			if coolPragmaFound:
				fout.write(srow+"\n")
				continue

			if srow == "#KROME_ODE":
				if self.use_implicit_RHS:
					fout.write(get_implicit_ode(self.maxnreag, self.maxnprod)+"\n")
				else:

					#add dust ODE and partner specie RHS terms
					if self.useDust or self.dustTabsH2:
						ndust = self.dustArraySize*self.dustTypesSize #number of dust ODEs
						#nmols = len(specs)-4-ndust #number of mols ODEs

						#print first dust to sum gas-dust terms up (e.g. dustC<->gasC)
						for x in dnw[nmols:nmols+ndust]:
							fout.write("\t" + x + "\n")
						fout.write("\n") #print a blank line
						useDustEvol = (self.useDustEvap or self.useDustGrowth or self.useDustSputter)
						if useDustEvol:
							if self.useDustEvap:
								#print evaporation control
								iType = 0
								for dType in dustTypes:
									offset = ""
									if iType > 0: offset = str(iType*self.dustArraySize)+"+"
									sumTypeVar = "dSumDust"+dType
									fout.write(sumTypeVar+" = 0d0\n")
									fout.write("!partner species sum with evaporation control for "\
										+ dType+" dust\n")
									fout.write("do i=1,"+str(self.dustArraySize)+"\n")
									fout.write(" if(dn(nmols+"+offset+"i)>-1d0 .and. n(nmols+"+offset\
										+"i)>1d-40) then\n")
									fout.write(sumTypeVar+" = "+sumTypeVar \
										+ " + 4d0*pi*dn(nmols+"+offset+"i)*n(nmols+"\
										+offset+"i)**2*krome_grain_rho("+str(iType+1)\
										+") / krome_dust_partner_mass("\
										+str(iType+1)+")*xdust("+offset+"i)\n")
									fout.write(" else\n")
									fout.write(sumTypeVar+" = "+sumTypeVar + " + 4d0*pi/3d0*n(nmols+"\
										+offset+"i)**3*krome_grain_rho("+str(iType+1)\
										+") / krome_dust_partner_mass("\
										+str(iType+1)+")*xdust("+offset+"i)\n")
									fout.write("   n(nmols+"+offset+"i) = 0d0\n")
									fout.write("   dn(nmols+"+offset+"i) = 0d0\n")
									fout.write("   xdust("+offset+"i) = 0d0\n")
									fout.write(" end if\n")
									fout.write("end do\n\n")
									iType += 1
							else:
								#print sums for different partners (with no evaporation)
								iType = 0
								for dType in dustTypes:
									ilow = nmols + iType * dustArraySize + 1 #lower index for dust in specs array
									iup = nmols + (iType + 1) * dustArraySize #upper index for dust in specs array
									kpart = "krome_dust_partner_ratio(" + str(ilow-nmols) + ":" \
										+ str(iup-nmols) + ")"
									limits = str(ilow) + ":" + str(iup) #absolute limits
									limits_rel = str(ilow-nmols) + ":" + str(iup-nmols) #realtive limits
									fout.write("dSumDust"+dType + " = sum(4d0*pi*n("+limits+")**2*dn("+\
										limits+")*krome_grain_rho("+str(iType+1)+") / "+\
										"krome_dust_partner_mass("+str(iType+1)+") * xdust("\
										+limits_rel+"))\n")
									iType += 1
							fout.write("\n") #print a blank line

						#print mols ODEs and look for dust partners to add summations
						# and H2 formation on dust and H depletion
						idnw = 0
						for x in dnw[:nmols]:
							for dType in dustTypes:
								if dType == specs[idnw].name and useDustEvol:
									x += " - dSumDust"+dType
							if self.useDustH2 or self.dustTabsH2:
								if "H"==specs[idnw].name: x += " - 2d0*nH2dust"
								if "H2"==specs[idnw].name: x += " + nH2dust"
							fout.write("\t" + x + "\n")
							idnw += 1

						#print other species (CR, PHOTONS, Tgas, dummy)
						for x in dnw[nmols+ndust:]:
							fout.write("\t" + x + "\n")

					#simply write ODEs without dust
					else:

						#add init flux var
						if not self.humanFlux:
							for x in self.reacts:
								fout.write("kflux("+str(x.idx)+") = "+x.RHS+"\n")
							fout.write("\n")

						inw = 0
						idnw = 0
						for x in dnw:
							#add H2 formation on dust
							if self.useDustH2const:
								if "H"==specs[idnw].name: x += " - 2d0*nH2dust"
								if "H2"==specs[idnw].name: x += " + nH2dust"
							idnw +=1

							#add custom ODE if needed
							if len(self.customODEs) > 0:
								for ode in self.customODEs:
									#build custom ODE
									if ode[0] == specs[inw].name:
										x = "dn("+str(inw+1)+") = "+ode[1]
										break
							fout.write("\n")
							fout.write("!"+specs[inw].name+"\n")
							fout.write("\t" + x + "\n")
							inw += 1

			#replace ice ODE variables
			elif srow == "#KROME_iceODEVariables":
				if len(self.iceSpeciesList) > 0:
					iceODEVariables = {"dnChem_"+k.replace("_ICE","") for k in self.iceSpeciesList.keys()}
					fout.write("real*8::"+(",".join(iceODEVariables))+"\n")

			#replace ice ODE definitions
			elif srow == "#KROME_iceODEDefinitions":
				#loop on ice species
				for (k,iceData) in self.iceSpeciesList.items():
					#break ODE into lines
					vODEret = iceData["ODE"].replace("-k("," &\n -k(")
					vODEret = vODEret.replace("+k("," &\n +k(")
					fout.write("dnChem_"+k.replace("_ICE","")+" = "+vODEret+"\n")

			#replace the pragma with the computation of the photorates using the opacity computed with
			# the approximation of Glover+2009 Eqn.2
			elif srow == "#KROME_photobins_compute_thick" and self.usePhotoOpacity:
				fout.write("call calc_photoBins_thick(n(:))\n")
			elif srow == "#KROME_flux_variables" and not self.humanFlux:
				#add var declaration for flux
				#for x in self.reacts:
				#	fout.write("real*8::"+x.RHSvar+"\n")
				fout.write("real*8::kflux("+str(len(self.reacts))+")\n")
				fout.write("\n")

			elif srow == "#KROME_H2pdRate":
				#add H2 photodissociation rate if available
				if self.indexH2photodissociation > -1:
					fout.write("!get H2 photodissociation rate\n")
					fout.write("k("+str(self.indexH2photodissociation)+") = kpd_H2(Tgas)\n")

			elif srow == "#KROME_calc_Tdust" and self.useDustT and not self.usedTdust:
				fout.write("call compute_Tdust(n(:),Tgas)"+"\n")

			elif srow == "#KROME_ODEModifier":
				#write the ODE modifiers
				odeModifierFull = "" #string that will contans all the lines of the ode modifier
				for kmod in self.odeModifier:
					#append the correct string
					fout.write(kmod+"\n")

			elif srow == "#KROME_initcoevars":
				if len(coevarsODE) == 0: continue
				kvars = "real*8::"+(",".join([x for x in coevarsODE.keys()]))
				fout.write(kvars+"\n")

			elif srow == "#KROME_coevars":
				if len(coevarsODE)==0: continue
				klist = [[k+" = "+v[1]+"\n",v[0]] for k, v in coevarsODE.items()] #this mess is to sort dict
				klist = sorted(klist, key=lambda x: x[1])
				fout.write("".join([x[0] for x in klist]))

			elif srow == "#KROME_compute_electrons" and hasElectrons and self.useComputeElectrons:
				fout.write("n(idx_e) = get_electrons(n(:))\n")

			elif srow == "#KROME_dustSumVariables" and self.useDust:
				#add partner sum dust variable declarations
				dustSumVar = []
				for dType in dustTypes:
					dustSumVar.append("dSumDust"+dType)
				fout.write("\t real*8::" + (",".join(dustSumVar)) + "\n")

			elif srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))

			elif srow == "#KROME_implicit_variables":
				fout.write("real*8::rr\n")
				ris = (",".join(["r"+str(i+1) for i in range(self.maxnreag)]))
				pis = (",".join(["p"+str(i+1) for i in range(self.maxnprod)]))
				rpis = ",".join([x for x in ["i",ris,pis] if(len(x)>0)])
				fout.write("integer::"+rpis+"\n")

			elif srow == "#KROME_report_flux":
				report_flux = ("*".join(["n(arr_r"+str(j+1)+"(i))" for j in range(self.maxnreag)]))
				fout.write("write(fnum,'(I5,E12.3e3,a2,a50)') i,k(i)*"+report_flux+",'',rnames(i)\n")

			elif srow == "#KROME_odeConstant" and self.useODEConstant:
				fout.write("dn(:) = dn(:) "+self.ODEConstant+"\n") #add the string contains an ODE expression

			elif srow == "#KROME_odeDust":
				fout.write("dn(:) =  krome_dust\n")

			elif srow == "#KROME_Tdust_limits" and self.usedTdust:
				fout.write("do idust=1,ndust\n")
				fout.write(" n(nmols+ndust+idust) = min(n(nmols+ndust+idust),TbbMax-1d0)\n")
				fout.write(" n(nmols+ndust+idust) = max(n(nmols+ndust+idust),0d0)\n")
				fout.write("end do\n")

			elif srow == "#KROME_dust_H2":
				fout.write(dustH2+"\n")

			elif srow == "#KROME_JAC_PDX":
				if not self.doJacobian: continue
				spdj = ""
				for i in range(neq):
					speci = specs[i]
					for j in range(neq):
						specj = specs[j]
						if self.jsparse[j][i] == 1:
							org = "pdj("+str(j+1)+")"
							rep = "pd("+str(j+1)+","+str(i+1)+")"
							orgT = "pdj(idx_Tgas)"
							repT = "pd(idx_Tgas,"+str(i+1)+")"
							spdj += "!d["+str(specj.name)+"_dot]/d["+str(speci.name)+"]\n"
							spdj += self.jac[j][i].replace(org,rep).replace(orgT,repT)+"\n\n"

				fout.write(spdj)

			elif srow == "#KROME_JAC_PD":
				if not self.doJacobian: continue
				#flag to determine if the IF block is open
				isBlockOpen = False
				#build the Jacobian as J(i,j) = df_i/dx_j
				for i in range(neq):
					if i+1 == electronIdx and self.useComputeElectrons: continue
					if not isBlockOpen:
						fout.write("if(j=="+str(i+1)+") then\n")
						isBlockOpen = True
					if isBlockOpen: fout.write("elseif(j=="+str(i+1)+") then\n")
					if i!=Tgas_species.idx - 1:
						spdj = ""
						has_pdj = False
						for j in range(neq):
							if j+1 == electronIdx and self.useComputeElectrons: continue
							if self.jsparse[j][i] == 1:
								has_pdj = True
								spdj += ("\t" + self.jac[j][i] + "\n")
						#if(has_pdj): fout.write("k(:) = coe_tab(n(:))\n")
						fout.write(spdj)
					else:
						jacT = "!use fex to compute temperature-dependent Jacobian\n"
						if self.deltajacMode == "RELATIVE":
							jacT += "dnn = n(idx_Tgas)*"+str(self.deltajac)+"\n"
						elif self.deltajacMode == "ABSOLUTE":
							jacT += "dnn = "+str(self.deltajac)+"\n"
						else:
							die("ERROR: unknown deltajacMode! "+self.deltajacMode)
						jacT += """nn(:) = n(:)
							nn(idx_Tgas) = n(idx_Tgas) + dnn
							call fex(neq,tt,nn(:),dn(:))
							do i=1,neq-1
							  pdj(i) = dn(i) / dnn
							end do"""
						if not self.use_thermo: jacT = ""
						fout.write("\t" + jacT.replace("\t","") + "\n")
				fout.write("end if\n")

			else:
				srow = row.strip()
				if len(srow) > 0:
					if srow[0] != "#": fout.write(row)
				else:
					fout.write(row)
		if not self.buildCompact:
			fout.close()
		print("done!")

	################################
	def makeUser(self):

		reacts = self.reacts
		nmols = self.nmols
		dustArraySize = self.dustArraySize
		dustTypesSize = self.dustTypesSize
		buildFolder = self.buildFolder
		specs = self.specs
		#*********USER****************
		#write parameters in krome_user.f90
		print("- writing krome_user.f90...",)
		fh = open(self.srcFolder+"krome_user.f90")
		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_user.f90","w")

		solar = get_solar_abundances()

		scaleZ = []
		hasElectrons = ("E" in [x.name for x in specs])
		has_H = ("H" in [x.name for x in specs])

		#looks for H to rescale the metallicity otherwise skips
		sHtot = "Htot = get_Hnuclei(n(:))"
		scaleZ.append(sHtot) #Htot= is the first of the list
		#creates the metallicity rescaling subroutine
		for k, v in solar.items():
			if not has_H:
				scaleZ = [] #reset scaleZ since Htot= is no longer needed
				break #skip routine if H is not present
			for mols in specs:
				if mols.name.upper() == "H": continue #skip hydrogen
				if mols.name.upper() == "HE": continue #skip helium
				if mols.name.upper() == "CR": continue #avoid Cr / CR confusion
				if mols.name.upper() == "CO": continue #avoid Co / CO confusion
				if mols.name.upper() == "PD": continue #avoid Pd / PD confusion
				if mols.name.upper() == "ND": continue #avoid Nd / ND confusion
				if k.upper() == mols.name.upper():
					scaleZ.append("x("+mols.fidx+") = max(Htot * 1d1**(Z+("+str(v)+")), 1d-40)")

		#non-negative index means H2 photodissociation reaction is set
		useH2Photodissociation = (self.indexH2photodissociation>-1)

		skip = skipDustOpacity = skipBindC = skipH2pd = skip_tab_2D = skip_tab_3D = False
		#loop on source to pre-process pragmas
		for row in fh:

			srow = row.strip()

			if srow == "#IFKROME_usePhotoBins" and self.photoBins <= 0: skip = True
			if srow == "#IFKROME_useStars" and not self.useStars: skip = True
			if srow == "#IFKROME_use_cooling" and not self.use_cooling: skip = True
			if srow == "#IFKROME_use_thermo" and not self.use_thermo: skip = True
			if srow == "#IFKROME_use_coolingZ" and not self.useCoolingZ: skip = True
			if srow == "#IFKROME_use_coolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_useXrays" and not self.useXRay: skip = True
			if srow == "#IFKROME_useDust" and not self.useDust: skip = True
			if srow == "#IFKROME_has_electrons" and not hasElectrons: skip = True
			if srow == "#IFKROME_useTabsTdust" and not self.useDustTabs: skip = True
			if srow == "#IFKROME_customFex" and not self.useFexCustom: skip = True
			if srow == "#IFKROME_hasStoreOnceRates" and not self.hasStoreOnceRates: skip = True
			if srow == "#IFKROME_dust_opacity" and not self.useDust: skipDustOpacity = True
			if srow == "#IFKROME_useH2pd" and not useH2Photodissociation: skipH2pd = True
			if srow == "#IFKROME_dust_table_2D" and not (self.dustTableDimension=="2D"): skip_tab_2D = True
			if srow == "#IFKROME_dust_table_3D" and not (self.dustTableDimension=="3D"): skip_tab_3D = True


			if srow == "#ENDIFKROME": skip = False
			if srow == "#ENDIFKROME_dust_table_2D": skip_tab_2D = False
			if srow == "#ENDIFKROME_dust_table_3D": skip_tab_3D = False
			if srow == "#ENDIFKROME_dust_opacity": skipDustOpacity = False
			if srow == "#ENDIFKROME_useH2pd": skipH2pd = False

			if srow == "#IFKROME_useBindC" and not(self.interfaceC or self.interfacePy): skipBindC = True
			if srow == "#ELSEKROME_useBindC" and not(self.interfaceC or self.interfacePy): skipBindC = False
			if srow == "#ELSEKROME_useBindC" and (self.interfaceC or self.interfacePy): skipBindC = True
			if srow == "#ENDIFKROME_useBindC": skipBindC = False

			if skip: continue
			if skipDustOpacity: continue
			if skipBindC: continue
			if skipH2pd: continue
			if skip_tab_2D or skip_tab_3D: continue

			row = row.replace("#KROME_single",self.KindSingle)
			row = row.replace("#KROME_double_value_optional",self.KindDoubleValueOptional)
			row = row.replace("#KROME_double_value",self.KindDoubleValue)
			row = row.replace("#KROME_double",self.KindDouble)
			row = row.replace("#KROME_integer_value",self.KindIntegerValue)
			row = row.replace("#KROME_integer",self.KindInteger)
			row = row.replace("#KROME_bool_optional",self.KindBoolValueOptional)
			row = row.replace("#KROME_character",self.KindCharacter)

			if self.interfaceC or self.interfacePy:
				row = row.replace("#KROME_bindC","bind(C)")
			else:
				row = row.replace("#KROME_bindC","")

			if srow == "#KROME_species":
				allBasics = []
				for sp in specs:
					if sp.is_surface and self.hasSurfaceReactions:
						xbasic = ("_".join(sp.fidx.split("_")[:-1]))
						xname = ("_".join(sp.name.split("_")[:-1]))
						if xbasic not in allBasics:
							fout.write("\tinteger,parameter::" + "KROME_"+xbasic + " = " + str(sp.idx) +"\t!"+xname+"\n")
							allBasics.append(xbasic)

					#add _GAS and _ICE species as standard and _TOTAL alias
					nameLower = sp.name.lower()
					if nameLower.endswith("_total") and self.doRamsesTH:
						gasSpecies = [x for x in specs if(x.name.lower()+"_total"==nameLower)][0]
						fout.write("\tinteger,parameter::" + "KROME_"+gasSpecies.fidx + "_ice = " + str(sp.idx) +"\t!"+gasSpecies.name+"_ice\n")
						fout.write("\tinteger,parameter::" + "KROME_"+gasSpecies.fidx + "_gas = " + str(gasSpecies.idx) +"\t!"+gasSpecies.name+"_gas\n")

					fout.write("\tinteger,parameter::" + "KROME_"+sp.fidx + " = " + str(sp.idx) +"\t!"+sp.name+"\n")

			#converter from MOCASSIN abundances to KROME
			elif srow == "#KROME_xmoc_map":
				xMocMap = ""
				for sp in specs:
					if not sp.is_atom: continue
					if sp.charge < 0: continue
					if sp.zatom == 0: continue
					xMocMap += "x("+sp.fidx+") = xmoc(imap("+str(sp.zatom)+"), "+str(sp.charge+1)+")\n"
				fout.write(xMocMap)

			#converter from KROME abundances to MOCASSIN
			elif srow == "#KROME_xmoc_map_return":
				xMocMap = ""
				for sp in specs:
					if not sp.is_atom: continue
					if sp.zatom == 0: continue
					if sp.charge < 0: continue
					xMocMap += "xmoc(imap("+str(sp.zatom)+"), "+str(sp.charge+1)+") = x("+sp.fidx+")\n"
				fout.write(xMocMap)

			elif srow == "#KROME_user_commons_functions":
				funcs = ""
				for x in self.commonvars:
					fsetname = "krome_set_"+x
					fset = "\n!*******************\n"
					fset += "subroutine "+fsetname+"(argset) " + self.BindC + "\n"
					fset += "use krome_commons\n"
					fset += "implicit none\n"
					fset += self.KindDoubleValue + " :: argset\n"
					fset += x+" = argset\n"
					fset += "end subroutine "+fsetname+"\n"

					fgetname = "krome_get_"+x
					fget = "\n!*******************\n"
					fget += "function "+fgetname+"() " + self.BindC + "\n"
					fget += "use krome_commons\n"
					fget += "implicit none\n"
					fget += self.KindDouble + " :: "+fgetname+"\n"
					fget += fgetname+" = "+x+"\n"
					fget += "end function "+fgetname+"\n"
					funcs += fset + fget
				fout.write(funcs)

			elif srow == "#KROME_cool_index":
				idxcool = get_cooling_index_list()
				for x in idxcool:
					fout.write("integer,parameter::krome_"+x+"\n")
			elif srow == "#KROME_heat_index":
				idxheat = get_heating_index_list()
				for x in idxheat:
					fout.write("integer,parameter::krome_"+x+"\n")
			elif srow == "#KROME_Tdust_copy" and (self.useDustT or self.usedTdust):
					fout.write("n(nmols+ndust+1:nmols+2*ndust) = krome_dust_T(:)\n")
			elif srow == "#KROME_print_phys_variables":
					for x in self.physVariables:
						fout.write("print *, \""+x[0]+":\", phys_"+x[0]+"\n")
			elif srow == "#KROME_set_get_phys_functions":
				for x in self.physVariables:
					#set subroutine
					funcname = "krome_set_"+x[0]
					fout.write("!*******************\n")
					fout.write("subroutine "+funcname+"(arg) " + self.BindC + "\n")
					fout.write(" use krome_commons\n")
					fout.write(" implicit none\n")
					fout.write(" " + self.KindDoubleValue + " :: arg\n")
					fout.write(" phys_"+x[0]+" = arg\n")
					fout.write("end subroutine "+funcname+"\n\n")

					#get function
					funcname = "krome_get_"+x[0]
					fout.write("!*******************\n")
					fout.write("function "+funcname+"() " + self.BindC + "\n")
					fout.write(" use krome_commons\n")
					fout.write(" implicit none\n")
					fout.write(" " + self.KindDouble + " :: "+funcname+"\n")
					fout.write(funcname+" = phys_"+x[0]+"\n")
					fout.write("end function "+funcname+"\n\n")
			#write the user alias for the cooling functions
			elif srow == "#KROME_cooling_functions":
				for x in self.coolZ_functions:
					funcname =  "krome_"+x[0]
					fout.write("\n!*******************\n")
					fout.write("function "+funcname+"(xin,inTgas)" + self.BindC + "\n")
					fout.write("use krome_commons\n")
					fout.write("use krome_subs\n")
					fout.write("use krome_cooling\n")
					fout.write("use krome_constants\n")
					fout.write(self.KindDouble + " :: xin(nmols)\n")
					fout.write(self.KindDoubleValue + " :: inTgas\n")
					fout.write(self.KindDouble + " :: "+funcname+"\n")
					fout.write("real*8::n(nspec),k(nZrate)\n")
					fout.write("n(:) = 0d0\n")
					fout.write("n(idx_Tgas) = inTgas\n")
					fout.write("n(1:nmols) = xin(:)\n")
					fout.write("k(:) = coolingZ_rate_tabs(inTgas)\n")
					fout.write(funcname+" = "+x[0]+"(n(:),n(idx_Tgas),k(:)) *  boltzmann_erg\n")
					fout.write("end function "+funcname+"\n")
			elif srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))
			elif srow == "#KROME_zero_electrons":
				#check if electron exists
				for x in specs:
					if x.name == "E":
						fout.write("x(idx_e) = 0.d0\n")
						break
			elif srow == "#KROME_electrons_balance":
				#check if electron exists
				for x in specs:
					if x.name == "E":
						fout.write("x(idx_e) = ee\n")
						break
			elif srow == "#KROME_constant_list":
				const = ""
				constants = self.constantList
				newc = []
				for i in range(len(constants)):
					x = constants[i]
					for j in range(i):
						y = constants[j]
						x[1] = x[1].replace(y[0], "krome_"+y[0])
					newc.append(x)

				for x in newc:
					const += "real*8,parameter::krome_" + x[0] + " = " + x[1] + " !" + x[2] + "\n"
				fout.write(const)
			elif srow == "#KROME_common_alias":
				#get the list of all the atoms contained in the species, H,C,O,...
				atoms = []
				for x in specs:
					atoms += x.atomcount.keys()
				atoms = list(set(atoms))
				atoms = [x for x in atoms if not(x in ["+","-"]) and not x.startswith("_")]

				fout.write("\tinteger,parameter::krome_nrea=" + str(self.nrea) + "\n")
				fout.write("\tinteger,parameter::krome_nmols=" + str(nmols) + "\n")
				fout.write("\tinteger,parameter::krome_nspec=" + str(len(specs)) + "\n")
				fout.write("\tinteger,parameter::krome_natoms=" + str(len(atoms)) + "\n")
				fout.write("\tinteger,parameter::krome_ndust=" + str(dustArraySize*dustTypesSize) + "\n")
				fout.write("\tinteger,parameter::krome_ndustTypes=" + str(dustTypesSize) + "\n")
				fout.write("\tinteger,parameter::krome_nPhotoBins=" + str(self.photoBins) + "\n")
				fout.write("\tinteger,parameter::krome_nPhotoRates=" + str(self.nPhotoRea) + "\n")
			elif srow == "#KROME_cooling_names_header_define":
				coolDict = [k for k, v in get_cooling_dict().items()]
				joinedCools = (" ".join(coolDict))
				fout.write("character*"+str(len(joinedCools))+"::krome_get_cooling_names_header\n")
			elif srow == "#KROME_heating_names_header_define":
				heatDict = [k for k, v in get_heating_dict().items()]
				joinedHeats = (" ".join(heatDict))
				fout.write("character*"+str(len(joinedHeats))+"::krome_get_heating_names_header\n")
			elif srow == "#KROME_names_header_define":
				skipspec = ["CR", "Tgas", "dummy", "g"]
				headlen = 0
				for species in specs:
					if species.name in skipspec: continue
					headlen += len(species.name)+1
				fout.write("character*"+str(headlen)+"::krome_get_names_header\n")
			elif srow == "#KROME_scaleZ":
				fout.write(("\n".join(scaleZ))+"\n")
			else:
				if len(srow) > 0:
					if row[0] != "#": fout.write(row)
				else:
					fout.write(row)
		fout.close()

        #add subroutine wrappers to functions returning array
		if self.interfaceC or self.interfacePy:
			self.makeUserCWrappers()

		print("done!")

	####################################
	#add subroutine wrappers to functions returning array
	def makeUserCWrappers(self):

		#original file
		if not self.buildCompact:
			fh = open(self.buildFolder+"krome_user.f90")
		else:
			fh = open(self.buildFolder+"krome_all.f90")
		#Cheader file
		#ch = open(self.srcFolder+"krome_user.h","rb")
		#temp file
		fout = open(self.buildFolder+"krome_user.tmp", "w")
		functionName = "__NONE__"
		wrapper = allWrappers = ""
		arguments = []
		#cproto = []
		#value_dec =""
		#value_init=""
		for row in fh:
			fout.write(row)
			if row.startswith("module krome_user"): break
		#loop on user file lines
		for row in fh:
			srow = row.strip()
			#look for function
			if srow.startswith("function"):
				arow = srow.split("(")
				#get function name
				functionName = arow[0].replace("function","").strip()

				#print functionName
				#get arguments
				args = arow[1].replace(")","").strip()
				#arguments as list
				arguments = args.split(",")

				#define arguments for wrapper subroutine
				argwrap = (args+","+functionName+"_var")
				if args == "": argwrap = functionName+"_var"

				#start wrapper
				wrapper = "!********************************\n"
				wrapper += "!subroutine wrapper around "+functionName+" function\n"
				wrapper += "subroutine "+functionName+"_wrap("+argwrap+") bind(C,name='%s')\n" %(functionName.lower())

				#flag: this function returns an array
				returnsArray = False

			#get use statements
			if srow.startswith("use krome_commons"):
				wrapper += "\t"+srow+"\n"

			#get implicit none statement
			if srow.startswith("implicit none"):
				wrapper += "\t"+srow+"\n"

			#loop on arguments definitions
			for arg in arguments:
				#arguments can be at the end of line or in a list
				argw = arg.replace(" ","")
				argindec = (srow.endswith(argw) or (argw+"," in srow) or (argw+"(" in srow))
				#if argument definition keeps line
				if argindec and ("::" in srow):
					srown = srow.replace("real*8","real(kind=c_double)")
					srown = srown.replace("real*4","real(kind=c_float)")
					srown = srown.replace("integer","integer(kind=c_int)")
					srown = srown.replace(functionName,functionName+"_var")
					if srown not in wrapper: wrapper += "\t"+srown+"\n"

			#if function name in definition add line
			if (functionName+"(" in srow) and ("::" in srow) and not 'names' in functionName:
				#functionName+"(" in declarations means array returned
				returnsArray = True
				#add declaration line and append _var
				srown = srow.replace("real*8","real(kind=c_double)")
				srown = srown.replace("real*4","real(kind=c_float)")
				srown = srown.replace("integer","integer(kind=c_int)")
				srown = srown.replace(functionName,functionName+"_var")
				if srown not in wrapper: wrapper += "\t"+srown+"\n"

			#when end function stores wrapper
			if srow.startswith("end function"):
				#assign to return variable
				wrapper += "\n\t"+functionName+"_var(:) = "+functionName+"("+args+")\n\n"
				#add subroutine end
				wrapper += "end subroutine "+functionName+"_wrap\n\n"
				#stores only if function returns array
				if returnsArray: allWrappers += wrapper

			#add wrappers before module
			if row.lower().startswith("end module"):
				fout.write(allWrappers)
			fout.write(row)

		fh.close()
		fout.close()
		#ch.close()
		#replace original file (.f90) with generated (.tmp)
		if not self.buildCompact:
				shutil.move(self.buildFolder+"krome_user.tmp",self.buildFolder+"krome_user.f90")
		else:
			shutil.move(self.buildFolder+"krome_user.tmp",self.buildFolder+"krome_all.f90")


	####################################
	def makeReduction(self):
		buildFolder = self.buildFolder
		#********* REDUCTION ****************
		#WARNING: this part is not supported and its use is discouraged
		print("- writing krome_reduction.f90...",)
		fh = open(self.srcFolder+"krome_reduction.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_reduction.f90","w")

		skip = False
		for row in fh:
			srow = row.strip()
			if srow == "#IFKROME_useTopology" and not self.useTopology: skip = True
			if srow == "#ENDIFKROME": skip = False
			if srow == "#IFKROME_useFlux" and not self.useFlux: skip = True
			if srow == "#ENDIFKROME": skip = False
			if srow == "#IFKROME_useReduction" and not self.useTopology and not self.useFlux: skip = True
			if srow == "#ENDIFKROME": skip = False

			if skip: continue
			if srow == "#KROME_rvars":
				if self.maxnreag > 0:
					fout.write("integer::"+(",".join(["r"+str(j+1) for j in range(self.maxnreag)]))+"\n")
			if srow == "#KROME_arrs":
				for j in range(self.maxnreag):
					fout.write("r"+str(j+1)+" = arr_r"+str(j+1)+"(i)\n")
			if srow == "#KROME_arr_flux":
				#print self.maxnreag
				if self.maxnreag > 0:
					fout.write("arr_flux(i) = k(i)*"+("*".join(["n(r"+str(j+1)+")" for j in range(self.maxnreag)]))+"\n")

			if row[0] != "#": fout.write(row)
		if not self.buildCompact:
			fout.close()

		print ("done!")


	##############################
	def makeStars(self):
		buildFolder = self.buildFolder
		#********* STARS ****************
		#intended for nuclear networks of stars
		print("- writing krome_stars.f90...",)
		fh = open(self.srcFolder+"krome_stars.f90")

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome_stars.f90","w")

		skip = False
		for row in fh:
			srow = row.strip()

			if srow == "#IFKROME_useStars" and not self.useStars: skip = True
			if srow == "#ENDIFKROME": skip = False

			if skip: continue
			#computes screening for 3body reactions (as screen products [1,2]*[1+2,3])
			if srow == "#KROME_stars_3body":
				stars3Body = "" #3body string to replace pragma
				idxs = []
				for rea in self.reacts:
					if rea.idx in idxs: continue
					idxs.append(rea.idx)
					if len(rea.reactants) == 3 and not(True in rea.curlyR):
						sdx = str(rea.idx) #string index
						stars3Body += "\n!3body: "+rea.verbatim+"\n"
						stars3Body += "z12 = zz(arr_r1("+sdx+")) + zz(arr_r2("+sdx+"))\n"
						stars3Body += "scr12 = stars_screen(Tgas,rho,n(:), zz(arr_r1("+sdx+")), zz(arr_r2("+sdx+")))\n"
						stars3Body += "scr23 = stars_screen(Tgas,rho,n(:), z12, zz(arr_r3("+sdx+")))\n"
						stars3Body += "k("+sdx+") = ko("+sdx+") * scr12 * scr23 \n"
				fout.write(stars3Body)
			elif srow == "#KROME_stars_energy":
				starsE = "" #energy string replacing pragma
				idxs = []
				for rea in self.reacts:
					if rea.idx in idxs: continue #avoid reactions with same index
					idxs.append(rea.idx)
					sdx = str(rea.idx) #string index
					starsE += "flux("+sdx+") = "+rea.RHS+" !"+rea.verbatim+"\n"
				fout.write(starsE)

			if row[0] != "#": fout.write(row)
		if not self.buildCompact:
			fout.close()
		print("done!")


	###############################
	def makeMain(self):
		buildFolder = self.buildFolder
		dustArraySize = self.dustArraySize
		dustTypes = self.dustTypes
		#*********MAIN****************
		#write WORKS arrays and IAC/JAC in krome.f90
		print("- writing krome.f90...",)
		if self.useDvodeF90:
			fh = open(self.srcFolder+"kromeF90.f90")
		else:
			fh = open(self.srcFolder+"krome.f90")

		ATOL = self.ATOL
		RTOL = self.RTOL
		if is_number(ATOL): ATOL = '%e' % float(ATOL)
		if is_number(RTOL): RTOL = '%e' % float(RTOL)
		ATOL = ATOL.replace("e","d")
		RTOL = RTOL.replace("e","d")

		#check if electrons are present
		hasElectrons = False
		for x in self.specs:
			if x.name == "E":
				hasElectrons = True
				break

		if self.buildCompact:
			fout = open(buildFolder+"krome_all.f90","a")
		else:
			fout = open(buildFolder+"krome.f90","w")

		#non-negative index means H2 photodissociation reaction is set
		useH2Photodissociation = (self.indexH2photodissociation>-1)
		skip = skipBindC = False
		for row in fh:
			srow = row.strip()
			if srow == "#IFKROME_useX" and not self.useX: skip = True
			if srow == "#ELSEKROME" and not self.useX: skip = False
			if srow == "#ELSEKROME" and self.useX: skip = True

			if srow == "#IFKROME_hasStoreOnceRates" and not self.hasStoreOnceRates: skip = True
			if srow == "#IFKROME_usedTdust" and not self.usedTdust: skip = True
			if srow == "#IFKROME_useH2pd" and not useH2Photodissociation: skip = True
			if srow == "#IFKROME_useTabs" and not self.useTabs: skip = True
			if srow == "#IFKROME_useChemisorption" and not self.useChemisorption: skip = True
			if srow == "#IFKROME_usePhotoBins" and not (self.photoBins > 0): skip = True
			if srow == "#IFKROME_useFlux" and not self.useFlux: skip = True
			if srow == "#IFKROME_conserve" and not self.useConserve and not self.useConserveE: skip = True
			if srow == "#IFKROME_report" and not self.doReport: skip = True
			if srow == "#IFKROME_useTopology" and not self.useTopology: skip = True
			if srow == "#IFKROME_check_mass_conservation" and not self.checkConserv: skip = True
			if srow == "#IFKROME_useDustSizeEvol" and not self.useDustSputter and not self.useDustGrowth\
				and not self.useDustEvap: skip = True
			if srow == "#IFKROME_useEquilibrium" and not self.useEquilibrium: skip = True
			if srow == "#IFKROME_useStars" and not self.useStars: skip = True
			if srow == "#IFKROME_useCoolingZ" and not self.useCoolingZ: skip = True
			if srow == "#IFKROME_useCoolingCO" and not self.useCoolingCO: skip = True
			if srow == "#IFKROME_useCoolingOH" and not self.useCoolingOH: skip = True
			if srow == "#IFKROME_useCoolingH2O" and not self.useCoolingH2O: skip = True
			if srow == "#IFKROME_useCoolingHCN" and not self.useCoolingHCN: skip = True
			if srow == "#IFKROME_useCoolingZCIE" and not self.useCoolingZCIE: skip = True
			if srow == "#IFKROME_useCoolingZCIENOUV" and not self.useCoolingZCIENOUV: skip = True
			if srow == "#IFKROME_useCoolingGH" and not self.useCoolingGH: skip = True
			if srow == "#IFKROME_ierr" and not self.useIERR: skip = True
			if srow == "#IFKROME_noierr" and self.useIERR: skip = True
			if srow == "#IFKROME_useH2esc_omukai" and self.H2opacity != "OMUKAI": skip = True
			if srow == "#IFKROME_usePreDustExp" and not((self.usedTdust or self.useDustT) and self.useSurface): skip = True
			if srow == "#IFKROME_useMayerOpacity" and not(self.usedTdust or self.useDustT): skip = True
			if srow == "#IFKROME_useDustTabs" and not self.useDustTabs: skip = True
			if srow == "#IFKROME_reducer" and not self.reducer: skip = True
			if srow == "#IFKROME_useBindC" and not(self.interfaceC or self.interfacePy): skipBindC = True
			if srow == "#ELSEKROME_useBindC" and not(self.interfaceC or self.interfacePy): skipBindC = False
			if srow == "#ELSEKROME_useBindC" and (self.interfaceC or self.interfacePy): skipBindC = True
			if srow == "#ENDIFKROME_useBindC": skipBindC = False
			if srow == "#IFKROME_use_GFE_tables" and not self.use_GFE_tables: skip = True
			if srow == "#IFKROME_useSemenov" and not self.useSemenov: skip = True
			if srow == "#ENDIFKROME": skip = False

			ierr = ""
			if self.useIERR: ierr = ",ierr"
			row = row.replace("#KROME_dust_arguments",""+ierr)

			if self.interfaceC or self.interfacePy:
				row = row.replace("#KROME_bindC","bind(C)")
			else:
				row = row.replace("#KROME_bindC","")


			row = row.replace("#KROME_single",self.KindSingle)
			row = row.replace("#KROME_double_value",self.KindDoubleValue)
			row = row.replace("#KROME_double",self.KindDouble)
			row = row.replace("#KROME_integer_value",self.KindIntegerValue)
			row = row.replace("#KROME_integer",self.KindInteger)
			row = row.replace("#KROME_bool_optional",self.KindBoolValueOptional)
			row = row.replace("#KROME_character",self.KindCharacter)

			row = row.replace("#KROME_ATOL",str(ATOL))
			row = row.replace("#KROME_RTOL",str(RTOL))


			#modfify call to krome main and DLSODES to use a custom fex
			if self.useFexCustom:
				row = row.replace("#KROME_fexCustom",", fexCustom")
				row = row.replace("#KROME_postfixFexCustom","Custom")
				row = row.replace("#KROME_externalFexCustom","external fexCustom")
			else:
				row = row.replace("#KROME_fexCustom","")
				row = row.replace("#KROME_postfixFexCustom","")
				row = row.replace("#KROME_externalFexCustom","")

			if skip: continue
			if skipBindC: continue
			reducerVarsList = [[x+"_Min",x+"_Max"] for x in self.reducerVars]

			if srow == "#KROME_header":
				fout.write(get_licence_header(self.version, self.codename,self.shortHead))
			#writes the arguments in the reducer subroutine interface
			elif "#KROME_reducerVarsInterface" in srow:
				reducerVarsInterface = [(",".join(x)) for x in reducerVarsList]
				row = row.replace("#KROME_reducerVarsInterface",\
					",&\n".join(reducerVarsInterface))
				fout.write(row)
				continue
			#writes the declarations of the argument variables (and their logs)
			elif "#KROME_reducerVarsDeclare" in srow:
				reducerVarsDeclareVals = [("real*8::"+x+"_val" if "user_" in x else "real*8::"+x)\
					 for x in self.reducerVars]
				reducerVarsDeclare = ["real*8::"+(",".join(x)) for x in reducerVarsList]
				reducerVarsDeclareLog = ["real*8::"+(",".join([x+"Log" for x in var])) for var in reducerVarsList]
				fout.write("!automatically generated declarations\n")
				for var in (reducerVarsDeclareVals+reducerVarsDeclare+reducerVarsDeclareLog):
					fout.write(var+"\n")
			#writes conversions to log
			elif "#KROME_reducerVarsLog" in srow:
				fout.write("!converts variables to logarithms\n")
				for var in reducerVarsList:
					for x in var:
						fout.write(x+"Log = log10("+x+")\n")
			#writes randomizer
			elif "#KROME_reducerVarsRandomize" in srow:
				for var in self.reducerVars:
					varname = var
					if "user_" in var: varname += "_val"
					fout.write(varname+" = 1d1**(rand()*("+var+"_MaxLog-"+var+"_MinLog) &\n+"+var+"_MinLog)\n")

			#writes initializations
			elif "#KROME_reducerPrintInits" in srow:
				for var in self.reducerVars:
					varname = var
					if "user_" in var: varname += "_val"
					fout.write("print *,\""+var+"\","+varname+"\n")


			#includes ifport if intel compiler ifort is employed
			elif srow == "#KROME_useIFPORT":
				if self.compiler=="ifort": fout.write("use ifport\n")

			#add user variables if any
			elif "#KROME_reducerVarsUserSet" in srow:
				for var in self.reducerVars:
					if "user_" in var:
						fout.write("call krome_set_"+var+"("+var+"_val)\n")

			elif srow == "#KROME_custom_ATOL":
				#add custom atols
				if len(self.atols) > 0:
					for x in self.atols:
						#check for species in species list
						for y in self.specs:
							#one can use either the name or the idx name (e.g. H+ or idx_Hp)
							if x[0] == y.name or x[0] == y.fidx:
								fout.write("atol("+y.fidx+") = "+format_double(x[1])+"\n")
								break
			elif srow == "#KROME_custom_RTOL":
				#add custom rtols
				if len(self.rtols) > 0:
					for x in self.rtols:
						#check for species in species list
						for y in self.specs:
							#one can use either the name or the idx name (e.g. H+ or idx_Hp)
							if x[0] == y.name or x[0] == y.fidx:
								fout.write("rtol("+y.fidx+") = "+format_double(x[1])+"\n")
								break
			#write the anytab initializations
			elif srow == "#KROME_init_anytab":
				stab = ""
				for i in range(len(self.anytabvars)):
					tabvar = self.anytabvars[i]
					tabfile = self.anytabfiles[i]
					anytabx = tabvar+"_anytabx(:)"
					anytaby = tabvar+"_anytaby(:)"
					anytabz = tabvar+"_anytabz(:,:)"
					anytabxmul = tabvar+"_anytabxmul"
					anytabymul = tabvar+"_anytabymul"
					stab += "call init_anytab2D(\""+tabfile+"\","+anytabx+",&\n"\
						+anytaby+","+anytabz+",&\n"+anytabxmul+","\
						+anytabymul+")\n"
					stab += "call test_anytab2D(\""+tabvar+"\","+anytabx+",&\n"\
						+anytaby+","+anytabz+",&\n"+anytabxmul+","\
						+anytabymul+")\n"
				fout.write(stab+"\n")
			elif srow == "#KROME_init_GFE_tables":
				stab = ""
				for sp in self.GFE_species:
					tabfile = sp.name + ".gfe"
					tabx = "GFE_tab_Tgas(:)"
					taby = "GFE_tab_gibbs_" + sp.name + "(:)"
					tabmul = "GFE_mult_Tgas"
					stab += ("call init_anytab1D(\"" + tabfile + "\"," + tabx + ",&\n"
							+ taby + "," + tabmul + ")\n"
							)
				fout.write(stab+"\n")
			#dump photopartners
			elif srow == "#KROME_photopartners":
				photoPartnersList = ""
				if len(self.photoPartners) > 0:
					listPart = []
					for phKey,phPart in self.photoPartners.items():
						listPart.append([phKey,phPart])
					listPart = sorted(listPart, key=lambda x:x[0])
					for i in range(len(listPart)):
						photoPartnersList += "photoPartners("+str(i+1)+") = "+listPart[i][1].fidx+"\n"
				fout.write(photoPartnersList+"\n")
			elif srow == "#KROME_init_phys_variables":
				if len(self.physVariables) > 0:
					fout.write("!$omp parallel\n")
				for x in self.physVariables:
					fout.write("phys_"+x[0]+" = "+x[1]+"\n")
				if len(self.physVariables) > 0:
					fout.write("!$omp end parallel\n")
			elif srow == "#KROME_rwork_array":
				fout.write("\treal*8::rwork(%d)\n" % self.lrw)
			elif srow == "#KROME_iwork_array":
				fout.write("\tinteger::iwork(%d)\n" % (30+len(self.ia)+len(self.ja)))
			elif srow == "#KROME_init_IAC":
				fout.write("\t"+self.iaf+"\n")
			elif srow == "#KROME_init_JAC":
				fout.write("\t"+self.jaf+"\n")
			elif srow == "#KROME_iaja_parameters":
				fout.write("integer,parameter::niauser="+str(len(self.ia))+",njauser="+str(len(self.ja))+"\n")
			elif srow == "#KROME_maxord" and self.maxord != 0:
				fout.write("iopt = 1 !activate optional inputs\n")
				fout.write("IWORK(5) = "+str(self.maxord)+" !maximum integration order\n")
			elif srow == "#KROME_MF":
				fout.write("MF = "+str(self.solver_MF)+"\n")
			elif srow == "#KROME_compute_electrons" and hasElectrons and self.useComputeElectrons:
				fout.write("n(idx_e) = get_electrons(n(:))\n")
			else:
				if row[0]!="#": fout.write(row)
		if not self.buildCompact:
			fout.close()
		print("done!")

	###################################
	def makeReport(self):
		specs = self.specs
		neq = len(specs)
		#*********REPORT.gps****************
		#write gnuplot script to plot abundances evolution
		if self.doReport:
			fout = open(self.buildFolder+"report.gps","w")
			fout.write("#plot KROME report\n")
			fout.write("reset\n")
			fout.write("set logscale\n")
			fout.write("set autoscale\n")
			fout.write("set xlabel \"log(time/s)\"\n")
			fout.write("plot 'fort.98' u 1:2 w l t \""+specs[0].name+"\",\\\n")
			for i in range(neq-2):
				fout.write("'' u 1:"+str(i+3)+" w l t \""+specs[i+1].name+"\",\\\n")
			fout.write("'' u 1:"+str(neq+2)+" w l t \""+specs[neq-1].name+"\"\n")
			fout.close()

	############################################
	def copyOthers(self):
		buildFolder = self.buildFolder
		test_name = self.test_name

		#make a backup copy of the test before overwrite
		if file_exists(buildFolder+"test.f90"):
			shutil.copyfile(buildFolder+"test.f90", buildFolder+"test.f90.bak")

		#copy surface chemisorption rates
		if self.useChemisorption:
			print("- copying chemisorption rate data...")
			shutil.copyfile("data/surface_chemisorption_rates.dat", buildFolder+"surface_chemisorption_rates.dat")

		#copy other files to build
		if self.useDust:
			print("- copying optical data for dust...")
			shutil.copyfile("data/optC.dat", buildFolder+"optC.dat")
			shutil.copyfile("data/optSi.dat", buildFolder+"optSi.dat")

		#copy cooling CO
		if self.useCoolingCO:
			print("- copying coolCO.dat...")
			shutil.copyfile("data/coolCO.dat", buildFolder + "coolCO.dat")

		#copy cooling HCN
		if self.useCoolingHCN:
			print("- copying coolHCN.dat...")
			shutil.copyfile("data/coolHCN.dat", buildFolder + "coolHCN.dat")

		#copy cooling H2O
		if self.useCoolingH2O:
			print("- copying coolH2O.dat...")
			shutil.copyfile("data/coolH2O.dat", buildFolder + "coolH2O.dat")

		#copy cooling OH
		if self.useCoolingOH:
			print("- copying coolOH.dat...")
			shutil.copyfile("data/coolOH.dat", buildFolder + "coolOH.dat")

		#copy cooling Z_CIE
		if self.useCoolingZCIE:
			print("- copying coolZ_CIE2012.dat...")
			shutil.copyfile("data/coolZ_CIE2012.dat", buildFolder+"coolZ_CIE2012.dat")

		#copy cooling Z_CIE NOUV
		if self.useCoolingZCIENOUV:
			print("- copying coolZ_CIE2012NOUV.dat...")
			shutil.copyfile("data/coolZ_CIE2012NOUV.dat", buildFolder+"coolZ_CIE2012NOUV.dat")

		#cooling Z_Extended
		if self.useCoolingZExtended:
			print("- copying coolZ_CIE2012.dat...")
			shutil.copyfile("data/coolZ_CIE2012.dat", buildFolder+"coolZ_CIE2012.dat")

		#cooling GH
		if self.useCoolingGH:
			print("- copying GnedinHollon/cf_table.I2.dat...")
			shutil.copyfile("data/GnedinHollon/cf_table.I2.dat", buildFolder+"cf_table.I2.dat")

		#copy partition function files
		if self.typeGamma == "POPOVAS":
			partFiles = ["H2even","H2odd","CO"]
			for fbase in partFiles:
				fname = "part"+fbase+".dat"
				print("- copying "+fname+"...")
				shutil.copyfile("data/partition/"+fname, buildFolder+fname)

		#copy OMUKAI datafile
		if self.H2opacity=="OMUKAI":
			shutil.copyfile("data/escape_H2.dat", buildFolder+"escape_H2.dat")

		#copy Mayer opacity file
		if self.usedTdust or self.useDustT :
			shutil.copyfile("data/mayer_E2.dat", buildFolder+"mayer_E2.dat")

		#copy HM2012 flux file
		if self.photoBins > 0:
			shutil.copyfile("data/HM2012.dat", buildFolder+"krome_HMflux.dat")

		#copy H2 dust tables
		if self.dustTabsH2:
			if self.dustTableDimension == "2D":
				dtableFname = "dust_table_"+self.dustTableMode+"_H2.dat"
			elif self.dustTableDimension == "3D":
				dtableFname = "dust_table_"+self.dustTableMode+"_H2_3D.dat"
			else:
				sys.exit("ERROR: table dimension unknown!")
			shutil.copyfile("data/dust_tables/"+dtableFname, buildFolder+"dust_table_H2.dat")

		#copy cool dust tables
		if self.dustTabsCool:
			if self.dustTableDimension == "2D":
				dtableFname = "dust_table_" + self.dustTableMode+"_cool.dat"
			elif self.dustTableDimension == "3D":
				dtableFname = "dust_table_"+self.dustTableMode+"_cool_3D.dat"
			else:
				sys.exit("ERROR: table dimension unknown!")
			shutil.copyfile("data/dust_tables/"+dtableFname, buildFolder+"dust_table_cool.dat")

		#copy averaged Tdust dust tables
		if self.dustTabsH2 and self.dustTabsCool:
			if self.dustTableDimension == "2D":
				dtableFname = "dust_table_"+self.dustTableMode+"_Tdust.dat"
			elif self.dustTableDimension=="3D":
				dtableFname = "dust_table_"+self.dustTableMode+"_Tdust_3D.dat"
			else:
				sys.exit("ERROR: table dimension unknown!")

			shutil.copyfile("data/dust_tables/"+dtableFname, buildFolder+"dust_table_Tdust.dat")

		#copy file that contains table as indicated by the anytab reactions
		print("- copying anytab files...")
		for i in range(len(self.anytabvars)):
			shutil.copyfile(self.anytabpaths[i], buildFolder+self.anytabfiles[i])

		#copy static files to build
		if self.is_test:
			print("- copying test to /build...")
			mypath = "tests/"+test_name
			files = [f for f in listdir(mypath) if isfile(join(mypath,f))]
			for fdir in files:
				shutil.copyfile("tests/"+test_name+"/"+fdir, buildFolder+"/"+fdir)
				print("- copying "+fdir+" to "+buildFolder)
			#if Makefile is not present in the tests directory use the default Makefile
			if not os.path.exists("tests/"+test_name+"/Makefile"):
				foundList = ["#KROME_compiler"]
				repList = [self.compiler]
				if self.useDvodeF90:
					shutil.copyfile("tests/MakefileF90", buildFolder+"Makefile")
				elif self.buildCompact:
					self.replacein("tests/MakefileCompact", buildFolder+"Makefile", foundList, repList, False)
				else:
					if self.pedanticMakefile:
						self.replacein("tests/Makefile_pedantic", buildFolder+"Makefile", foundList, repList, False)
						#shutil.copyfile("tests/Makefile_pedantic", buildFolder+"Makefile")
					else:
						self.replacein("tests/Makefile", buildFolder+"Makefile", foundList, repList, False)
						#shutil.copyfile("tests/Makefile", buildFolder+"Makefile")

			#test_file = "tests/"+test_name+"/test.f90"
			#plot_file = "tests/"+test_name+"/plot.gps"

			#chech if test file exists
			#if(not(os.path.isfile(filename))): die("ERROR: Test file \""+test_file+"\" doesn't exist!")

			#shutil.copyfile(test_file, buildFolder+"test.f90")
			#if(self.has_plot): shutil.copyfile(plot_file, buildFolder+"plot.gps")
			#print " done!"

		#copy solver files to build folder
		print("- copying solver(s) to /build...")
		if self.useDvodeF90:
			shutil.copyfile("solver/dvode_f90_m.f90", buildFolder+"dvode_f90_m.f90")
		else:
			shutil.copyfile("solver/opkdmain.f", buildFolder+"opkdmain.f")
			shutil.copyfile("solver/opkda1.f", buildFolder+"opkda1.f")
			shutil.copyfile("solver/opkda2.f", buildFolder+"opkda2.f")
		#copy non-linear equation solver to build folder
		if self.useNLEQ: shutil.copyfile("solver/nleq_all.f", buildFolder+"nleq_all.f")

		#copy utility to list the user functions
		fname = "tools/list_user_functions.py"
		if os.path.exists(fname):
			shutil.copyfile(fname, buildFolder+"list_user_functions.py")
			

	#######################################################
	def indent(self):
		buildFolder = self.buildFolder
		print("Indenting...",)
		if self.doIndent:
			if self.buildCompact:
				indentF90(buildFolder+"krome_all.f90")
			else:
				indentF90(buildFolder+"krome_user_commons.f90")
				indentF90(buildFolder+"krome_commons.f90")
				indentF90(buildFolder+"krome_constants.f90")
				indentF90(buildFolder+"krome_cooling.f90")
				indentF90(buildFolder+"krome_heating.f90")
				indentF90(buildFolder+"krome_dust.f90")
				indentF90(buildFolder+"krome.f90")
				indentF90(buildFolder+"krome_ode.f90")
				indentF90(buildFolder+"krome_photo.f90")
				indentF90(buildFolder+"krome_stars.f90")
				indentF90(buildFolder+"krome_reduction.f90")
				indentF90(buildFolder+"krome_grfuncs.f90")
				indentF90(buildFolder+"krome_getphys.f90")
				indentF90(buildFolder+"krome_subs.f90")
				indentF90(buildFolder+"krome_tabs.f90")
				indentF90(buildFolder+"krome_user.f90")
				indentF90(buildFolder+"krome_gadiab.f90")
				indentF90(buildFolder+"krome_phfuncs.f90")
				indentF90(buildFolder+"krome_fit.f90")

		print("done!")


	#########################################
	#copy fsrc to fout file and replace the list in pragmas with the list in repls.
	# if trim the each line is trimmed and a return \n is added when write it again
	def replacein(self,fsrc,fout,pragmas,repls,trim=True):
		fh = open(fsrc)
		fw = open(fout, "w")
		if len(pragmas) != len(repls):
			print("ERROR: in replacein len(pragmas)!=len(repls)")
			sys.exit()
		for row in fh:
			srow = row
			if trim: srow = row.strip()
			#replace only with non-empty lists
			if len(pragmas) > 0:
				for i in range(len(pragmas)):
					x = pragmas[i]
					y = str(repls[i])
					srow = srow.replace(x,y)
			if trim:
				fw.write(srow+"\n")
			else:
				fw.write(srow)
		fh.close()
		fw.close()

	#########################################
	def ramses_patch2011(self):
		pfold = "patches/ramses/"
		ramsesFolder = self.buildFolder+"krome_ramses_patch/"
		buildFolder = self.buildFolder
		if not os.path.exists(ramsesFolder): os.makedirs(ramsesFolder)
		specs = self.specs
		ramses_offset = str(self.ramses_offset)

		#some initial abundances. if not found set default
		# all in 1/cm3, except for T which is K
		ndef = {"H": 7.5615e-1,
			"E": 4.4983e-8,
			"H+": 8.1967e-5,
			"HE": 2.4375e-1,
			"H2": 1.5123e-6,
			"Tgas" : 200,
			"default":1e-40
		}

		excl = ["CR","g","Tgas","dummy"] #avoid specials

		#count species excluding what is conteinted in excl list
		chemCount = 0
		for x in specs:
			if(x.name in excl): continue
			chemCount += 1

		#amr_parameters
		#just copy the file fname to the build/ramses folder
		fname = "amr_parameters.f90"
		self.replacein(pfold+fname, ramsesFolder+fname, ["aaa"], ["aaa"])
		indentF90(ramsesFolder+fname)

		#condinit
		#prepares the initial conditions and copy fname
		cheminit = " q(1:nn,ndim+3) = "+str(ndef["Tgas"])+"     !Set temperature in K\n"
		ichem = 0
		fname = "condinit.f90"
		#loop on species
		for x in specs:
			#skip species in exl list
			if x.name in excl: continue
			ichem += 1
			#check if species is in init array (ndef) else default
			if x.name in ndef:
				sdef = str(ndef[x.name]) #default value from array
			else:
				sdef = str(ndef["default"]) #default values if not present in array
			cheminit += "q(1:nn,ndim+3+"+str(ichem)+")  = "+sdef+"  !"+x.name+"\n"
		#replace initialization
		self.replacein(pfold+fname, ramsesFolder+fname, ["#KROME_init_chem"], [cheminit])
		indentF90(ramsesFolder+fname)

		#cooling_fine
		#prepare the array for krome and back (ramses->krome->ramses)
		# updateueq: copy from ramses 2dim array to 1dim array for krome (unoneq<-uold)
		# scaleueq: scale array from code units (RAMSES) to 1/cm3 (KROME) (unoneq<-unoneq)
		# bkscaleueq: scale array from 1/cm3 (KROME) to code units (RAMSES) (unoneq->unoneq)
		# bkupdateueq: copy from 1dim array of krome to 2dim array of ramses (unoneq->uold)
		updateueq = scaleueq = bkscaleueq = bkupdateueq = ""
		ichem = 0
		fname = "cooling_fine.f90"
		for x in specs:
			ichem += 1
			if x.name not in excl:
				updateueq += "unoneq("+str(ichem)+") = uold(ind_leaf(i),ndim+"+ramses_offset+"+"+str(ichem)+") !"+x.name+"\n"
				if x.mass > 0e0: scaleueq += "unoneq("+str(ichem)+") = unoneq("+str(ichem)+")*scale_d/"+str(x.mass)+" !"+x.name+"\n"
				bkscaleueq += "unoneq("+str(ichem)+") = unoneq("+str(ichem)+")*"+str(x.mass)+"/scale_d !"+x.name+"\n"
				bkupdateueq += "uold(ind_leaf(i),ndim+"+ramses_offset+"+"+str(ichem)+") = unoneq("+str(ichem)+")\n"
		org = ["#KROME_update_unoneq","#KROME_scale_unoneq","#KROME_backscale_unoneq","#KROME_backupdate_unoneq"]
		new = [updateueq, scaleueq, bkscaleueq, bkupdateueq]
		#replace pragmas (org) with expressions (new)
		self.replacein(pfold+fname, ramsesFolder+fname, org, new)
		indentF90(ramsesFolder+fname)

		#cooling_module
		# simply copy the cooling_module into build/ramses
		fname = "cooling_module.f90"
		self.replacein(pfold+fname,ramsesFolder+fname, [], [])
		indentF90(ramsesFolder+fname)

		#hydro_parameters
		# simply copy the hydro_parameters into build/ramses
		# extend nvar according to KROME species
		fname = "hydro_parameters.f90"
		self.replacein(pfold+fname, ramsesFolder+fname, [], [])
		#indentF90(ramsesFolder+fname)

		#init_flow_fine
		fname = "init_flow_fine.f90"
		init_array = "if(ivar==ndim+"+ramses_offset+")  init_array = 1.356d-2/aexp**2 ! T in K\n"
		ichem = 0
		for x in specs:
			if x.name in excl: continue
			ichem += 1
			#check if species is contained in the ndef array (see above)
			if x.name in ndef:
				sdef = str(ndef[x.name]) #default value from array
			else:
				sdef = str(ndef["default"]) #default value if not present in array
			init_array += "if(ivar==ndim+"+ramses_offset+"+"+str(ichem)+")  init_array = "+sdef+"  !"+x.name+"\n"
		#replace pragma and copy the file to the build/ramses
		self.replacein(pfold+fname,ramsesFolder+fname,["#KROME_init_array"],[init_array])
		indentF90(ramsesFolder+fname)

		#output_hydro
		fname = "output_hydro.f90"
		self.replacein(pfold+fname,ramsesFolder+fname,[],[])
		indentF90(ramsesFolder+fname)

		#read_hydro_params
		fname = "read_hydro_params.f90"
		self.replacein(pfold+fname,ramsesFolder+fname,[],[])
		indentF90(ramsesFolder+fname)

		#Makefile
		fname = "Makefile"
		#note that makefile will be copied in the build folder
		self.replacein(pfold+fname,buildFolder+fname,["#KROME_nvar"],\
			["#this must be NDIM+"+str(ramses_offset)+"+"+str(chemCount)], False)

		#move the krome files into the ramses patch folder
		shutil.move(buildFolder+"krome_all.f90", ramsesFolder+"krome_all.f90")
		shutil.move(buildFolder+"krome_user_commons.f90", ramsesFolder+"krome_user_commons.f90")
		shutil.move(buildFolder+"opkda1.f", ramsesFolder+"opkda1.f")
		shutil.move(buildFolder+"opkda2.f", ramsesFolder+"opkda2.f")
		shutil.move(buildFolder+"opkdmain.f", ramsesFolder+"opkdmain.f")

	def gizmo_patch(self):
		#move the krome files into the gizmo patch folder
		pfold = "patches/gizmo/"
		gizmoFolder = self.buildFolder+"krome_gizmo_patch/"
		buildFolder = self.buildFolder
		if not os.path.exists(gizmoFolder): os.makedirs(gizmoFolder)

		fname = "krome.c"
		shutil.copy(pfold+fname,gizmoFolder+fname)

		fname = "README.gizmo"
		self.replacein(pfold+fname,gizmoFolder+fname,[],[])

		#shutil.move(buildFolder+"krome_all.h", gizmoFolder+"krome_all.h")
		#shutil.move(buildFolder+"krome_header.c", gizmoFolder+"krome_header.c")
		shutil.move(buildFolder+"krome_all.f90", gizmoFolder+"krome_all.f90")
		shutil.move(buildFolder+"krome_user_commons.f90", gizmoFolder+"krome_user_commons.f90")
		shutil.move(buildFolder+"opkda1.f", gizmoFolder+"opkda1.f")
		shutil.move(buildFolder+"opkda2.f", gizmoFolder+"opkda2.f")
		shutil.move(buildFolder+"opkdmain.f", gizmoFolder+"opkdmain.f")

	#########################################
	def ramses_patch(self):
		pfold = "patches/ramses/"
		ramsesFolder = self.buildFolder+"krome_ramses_patch/"
		buildFolder = self.buildFolder
		if not os.path.exists(ramsesFolder): os.makedirs(ramsesFolder)
		specs = self.specs
		ramses_offset = str(self.ramses_offset)

		#some initial abundances. if not found set default
		# all in 1/cm3, except for T which is K
		ndef = {"H": 7.5615e-1,
			"E": 4.4983e-8,
			"H+": 8.1967e-5,
			"HE": 2.4375e-1,
			"H2": 1.5123e-6,
			"default":1e-40
		}

		excl = ["CR","g","Tgas","dummy"] #avoid specials

		#count species excluding what is conteinted in excl list
		chemCount = 0
		for x in specs:
			if x.name in excl: continue
			chemCount += 1

		#amr_parameters
		#just copy the file fname to the build/ramses folder
		fname = "amr_parameters.f90"
		self.replacein(pfold+fname, ramsesFolder+fname, ["aaa"], ["aaa"])
		indentF90(ramsesFolder+fname)

		#condinit
		#prepares the initial conditions and copy fname
		cheminit = "\n"
		ichem = 0
		fname = "condinit.f90"
		#loop on species
		for x in specs:
			#skip species in exl list
			if x.name in excl: continue
			ichem += 1
			#check if species is in init array (ndef) else default
			if x.name in ndef:
				sdef = str(ndef[x.name]) #default value from array
			else:
				sdef = str(ndef["default"]) #default values if not present in array
			cheminit += "q(1:nn,ndim+"+ramses_offset+"+"+str(ichem)+")  = "+sdef.replace("e","d")+"  !"+x.name+"\n"
		#replace initialization
		self.replacein(pfold+fname, ramsesFolder+fname, ["#KROME_init_chem"], [cheminit])
		indentF90(ramsesFolder+fname)

		#cooling_fine
		#prepare the array for krome and back (ramses->krome->ramses)
		# updateueq: copy from ramses 2dim array to 1dim array for krome (unoneq<-uold)
		# scaleueq: scale array from code units (RAMSES) to 1/cm3 (KROME) (unoneq<-unoneq)
		# bkscaleueq: scale array from 1/cm3 (KROME) to code units (RAMSES) (unoneq->unoneq)
		# bkupdateueq: copy from 1dim array of krome to 2dim array of ramses (unoneq->uold)
		updateueq = scaleueq = bkscaleueq = bkupdateueq = ""
		ichem = 0
		fname = "cooling_fine.f90"
		for x in specs:
			ichem += 1
			if x.name not in excl:
				updateueq += "unoneq("+str(ichem)+") = uold(ind_leaf(i),ndim+"+ramses_offset+"+"+str(ichem)+") !"+x.name+"\n"
				if x.mass > 0e0: scaleueq += "unoneq("+str(ichem)+") = unoneq("+str(ichem)+")*scale_d/"+str(x.mass)+" !"+x.name+"\n"
				bkscaleueq += "unoneq("+str(ichem)+") = unoneq("+str(ichem)+")*"+str(x.mass)+"*iscale_d !"+x.name+"\n"
				bkupdateueq += "uold(ind_leaf(i),ndim+"+ramses_offset+"+"+str(ichem)+") = unoneq("+str(ichem)+")\n"
		org = ["#KROME_update_unoneq","#KROME_scale_unoneq","#KROME_backscale_unoneq","#KROME_backupdate_unoneq"]
		new = [updateueq, scaleueq, bkscaleueq, bkupdateueq]
		#replace pragmas (org) with expressions (new)
		self.replacein(pfold+fname, ramsesFolder+fname, org, new)
		indentF90(ramsesFolder+fname)

		#init_flow_fine
		fname = "init_flow_fine.f90"
		init_array = "\n"
		ichem = 0
		for x in specs:
			if x.name in excl: continue
			ichem += 1
			#check if species is contained in the ndef array (see above)
			if x.name in ndef:
				sdef = str(ndef[x.name]) #default value from array
			else:
				sdef = str(ndef["default"]) #default value if not present in array
			init_array += "if(ivar==ndim+"+ramses_offset+"+"+str(ichem)+")  init_array = "+sdef+"  !"+x.name+"\n"
		#replace pragma and copy the file to the build/ramses
		self.replacein(pfold+fname,ramsesFolder+fname,["#KROME_init_array"],[init_array])
		indentF90(ramsesFolder+fname)

		#read_hydro_params
		fname = "read_hydro_params.f90"
		self.replacein(pfold+fname,ramsesFolder+fname,[],[])
		indentF90(ramsesFolder+fname)

		#Makefile
		fname = "Makefile"
		#note that makefile will be copied in the build folder
		self.replacein(pfold+fname,buildFolder+fname,["#KROME_nvar"],
			["#this must be NDIM+"+str(ramses_offset)+"+"+str(chemCount)], False)

		#move the krome files into the ramses patch folder
		shutil.move(buildFolder+"krome_all.f90", ramsesFolder+"krome_all.f90")
		shutil.move(buildFolder+"krome_user_commons.f90", ramsesFolder+"krome_user_commons.f90")
		shutil.move(buildFolder+"opkda1.f", ramsesFolder+"opkda1.f")
		shutil.move(buildFolder+"opkda2.f", ramsesFolder+"opkda2.f")
		shutil.move(buildFolder+"opkdmain.f", ramsesFolder+"opkdmain.f")


	#########################################
	def ramsesTH_patch(self):
		pfold = "patches/ramsesTH/" #source folder
		ramsesFolder = self.buildFolder+"krome_ramsesTH_patch/" #destination folder
		buildFolder = self.buildFolder
		if not os.path.exists(ramsesFolder): os.makedirs(ramsesFolder)
		specs = self.specs

		#get_depleted()

		#some initial abundances. if not found set default
		# all in 1/cm3, except for T which is K
		ndef = {"H": 7.5615e-1,
			"E": 4.4983e-8,
			"H+": 8.1967e-5,
			"HE": 2.4375e-1,
			"H2": 1.5123e-6,
			"O": 0.003792e0,
			"C": 0.001269e0,
			"Tgas" : 200e0,
			"default":1e-40
		}

		excl = ["CR","g","Tgas","dummy"] #avoid specials

		#count species excluding what is contented in excl list
		chemCount = 0
		for x in specs:
			if x.name in excl: continue
			chemCount += 1

		#cooling_fine
		#prepare the array for krome and back (ramses->krome->ramses)
		# updateueq: copy from ramses 2dim array to 1dim array for krome (unoneq<-uold)
		# scaleueq: scale array from code units (RAMSES) to 1/cm3 (KROME) (unoneq<-unoneq)
		# bkscaleueq: scale array from 1/cm3 (KROME) to code units (RAMSES) (unoneq->unoneq)
		# bkupdateueq: copy from 1dim array of krome to 2dim array of ramses (unoneq->uold)
		# These two are used when updating oct-averaged states
		# vecupdateueq: copy from ramses 2dim array to 1dim array for krome (unoneq<-uin)
		# vecbkupdateueq: copy from 1dim array of krome to 2dim array of ramses (unoneq->uout)
		updateueq = scaleueq = bkscaleueq = bkupdateueq = vecupdateueq = vecbkupdateueq = ""
		ichem = 0
		fname = "cooling_fine.f90"
		gasSpeciesList = []
		for sp in specs:
			ichem += 1
			#skip species in exclusion list
			if sp.name not in excl:
				#lower case name of the species
				nameLower = sp.name.lower()
				#check if species is TOTAL, i.e. TOTAL=ICE+GAS
				isIceTotal = (nameLower.endswith("_total"))
				#if species is TOTAL, get the gas name (i.e. the name without _total)
				if isIceTotal:
					gasSpecies = [x for x in specs if(x.name.lower()+"_total"==nameLower)][0]
					gasSpeciesList.append(gasSpecies)

				if isIceTotal:
					updateueq += "unoneq(krome_"+gasSpecies.fidx + "_ice) = uold(ind_leaf(i),ichem+krome_"+gasSpecies.fidx + "_ice) !" \
					        + gasSpecies.name+"_ICE\n"
					vecupdateueq += "unoneq(krome_"+gasSpecies.fidx + "_ice) = uin(i,2+krome_"+gasSpecies.fidx + "_ice) !" \
					        + gasSpecies.name+"_ICE\n"
				else:
					updateueq += "unoneq(krome_"+sp.fidx+") = uold(ind_leaf(i),ichem+krome_"+sp.fidx+") !"+sp.name+"\n"
					vecupdateueq += "unoneq(krome_"+sp.fidx+") = uin(i,2+krome_"+sp.fidx+") !"+sp.name+"\n"
				#skip species with zero mass only if not TOTAL
				if sp.mass>0e0 or isIceTotal:
					#if species is TOTAL then converts ICE
					if isIceTotal:
						scaleueq += "unoneq(krome_" + gasSpecies.fidx + "_ice) = unoneq(krome_" + gasSpecies.fidx + "_ice)*scale_d/" \
							+ str(gasSpecies.mass) + " !" + gasSpecies.name+"_ICE\n"
					else:
						scaleueq += "unoneq(krome_"+sp.fidx+") = unoneq(krome_"+sp.fidx+")*scale_d/"+str(sp.mass)+" !"+sp.name+"\n"
				#if the species is TOTAL get the ICE value from TOTAL and GAS
				if isIceTotal:
					bkscaleueq += "unoneq(krome_"+gasSpecies.fidx+"_total) = unoneq(krome_"+gasSpecies.fidx+"_total)*" \
						+ str(gasSpecies.mass)+"*iscale_d !"+gasSpecies.name+"_total\n"
					bkupdateueq += "uold(ind_leaf(i),ichem+krome_"+gasSpecies.fidx + "_ice) = unoneq(krome_"+gasSpecies.fidx + "_ice) !" \
					        + gasSpecies.name+"_ICE\n"
					vecbkupdateueq += "uout(i,2+krome_"+gasSpecies.fidx + "_ice) = unoneq(krome_"+gasSpecies.fidx + "_ice) !" \
					        + gasSpecies.name+"_ICE\n"
				else:
					bkscaleueq += "unoneq(krome_"+sp.fidx+") = unoneq(krome_"+sp.fidx+")*"+str(sp.mass)+"*iscale_d !"+sp.name+"\n"
					bkupdateueq += "uold(ind_leaf(i),ichem+krome_"+sp.fidx+") = unoneq(krome_"+sp.fidx+") !"+sp.name+"\n"
					vecbkupdateueq += "uout(i,2+krome_"+sp.fidx+") = unoneq(krome_"+sp.fidx+") !"+sp.name+"\n"

		#loop on ices found to convert KROME<->RAMSES
		for gasSpecies in gasSpeciesList:
			scaleueq += "\n!KROME requires total and gas phase "+gasSpecies.name+", here obtained from ice and gas\n"
			scaleueq += "! "+gasSpecies.name+"_gas and "+gasSpecies.name+" are the same species\n"
			scaleueq += "unoneq(krome_" + gasSpecies.fidx + "_total) = unoneq(krome_" + gasSpecies.fidx + "_ice) " \
				+ " + unoneq(krome_" + gasSpecies.fidx + "_gas) !" + gasSpecies.name+"_TOTAL = " \
				+ gasSpecies.name+"_GAS + " + gasSpecies.name+"_ICE\n"
			bkscaleueq += "\n!RAMSES requires ice and gas phase "+gasSpecies.name+", here obtained from total and gas\n"
			bkscaleueq += "unoneq(krome_"+gasSpecies.fidx+"_ice) = max(unoneq(krome_"+gasSpecies.fidx+"_total) - " \
						+ "unoneq(krome_"+gasSpecies.fidx+"_gas), &\n unoneq(krome_"+gasSpecies.fidx+"_total)*1d-40) !" \
						+ gasSpecies.name+"_ice = "+ gasSpecies.name+"_total - " + gasSpecies.name+"_gas\n"


		org = ["#KROME_update_unoneq","#KROME_scale_unoneq","#KROME_backscale_unoneq","#KROME_backupdate_unoneq",\
			"#KROME_vecupdate_unoneq","#KROME_vecbackupdate_unoneq",]
		new = [updateueq, scaleueq, bkscaleueq, bkupdateueq, vecupdateueq, vecbkupdateueq]
		#replace pragmas (org) with expressions (new)
		self.replacein(pfold+fname, ramsesFolder+fname, org, new)
		indentF90(ramsesFolder+fname)

		#prepares abundances.nml
		abnml = "!This file contains the initialization for the species\n"
		abnml += "! employed by KROME. Change them according to your needs\n"
		ichem = 0
		comma = "" #add a separator here for namelist
		for x in specs:
			if chemCount == ichem+1: comma = ""
			if x.name in excl: continue
			#check if species is contained in the ndef array (see above)
			if x.name in ndef:
				sdef = format_double(ndef[x.name]) #default value from array
			else:
				sdef = format_double(ndef["default"]) #default value if not present in array
			if ichem == 0:
				abpart = "metal_region(1,1) = "
				absize = len(abpart)
				abpart += sdef+comma+(" "*(20-len(sdef)-len(comma)))
			else:
				abpart = (" "*absize)+sdef+comma+(" "*(20-len(sdef)-len(comma)))
			abnml += abpart+"!"+str(ichem+1)+": "+x.name+"\n"
			ichem += 1

		#write abundances.nml
		fh = open(buildFolder+"abundances.nml","w")
		fh.write(abnml)
		fh.close()

		#copy cooling
		fname = "cooling.f90"
		self.replacein(pfold+fname,ramsesFolder+fname,[],[])
		indentF90(ramsesFolder+fname)

		#copy Makefile
		fname = "Makefile"
		liblapack = ""
		if self.needLAPACK: liblapack = "LIBS += -llapack"
		#+1 after ichem is for adiabatic index
		self.replacein(pfold+fname, ramsesFolder+fname, ["#KROME_NMOLS","#KROME_useLAPACK"],
					   [str(ichem+1),liblapack], False)

		#copy Makefile.dep
		fname = "Makefile.dep"
		shutil.copy(pfold+fname,ramsesFolder+fname)

		#move the krome files into the ramses patch folder
		shutil.move(buildFolder+"krome_all.f90", ramsesFolder+"krome_all.f90")
		shutil.move(buildFolder+"krome_user_commons.f90", ramsesFolder+"krome_user_commons.f90")
		shutil.move(buildFolder+"opkda1.f", ramsesFolder+"opkda1.f")
		shutil.move(buildFolder+"opkda2.f", ramsesFolder+"opkda2.f")
		shutil.move(buildFolder+"opkdmain.f", ramsesFolder+"opkdmain.f")



	###########################################
	def flash_patch(self):
		specs = self.specs

		#some initial fractions. if not found set default
		ndef = {"H": 0.76e0,
			"HE": 0.24e0,
			"H+": 1.2e-5,
			"H-": 2e-9,
			"He+": 1e-14,
			"He++": 1e-17,
			"H2": 2e-20,
			"default":1e-40
		}

		buildFolder = self.buildFolder
		flashFolder = buildFolder+"krome_flash_patch/"
		patchFolder = "patches/flash/"

		#create folders
		dirs = [flashFolder + "Driver/DriverMain"]
		dirs.append(flashFolder + "physics/sourceTerms/KromeChemistry/KromeChemistryMain")
		dirs.append(flashFolder + "Simulation/SimulationComposition/KromeChemistry")
		dirs.append(flashFolder + "Simulation/SimulationMain/Chemistry_Krome_Collapse")
		for pdir in dirs:
			if not os.path.exists(pdir): os.makedirs(pdir)

		excl = ["CR","g","Tgas","dummy"] #species to exclude

		########Driver#############
		pFolder = "Driver/DriverMain/"
		flist = ["Driver_finalizeSourceTerms.F90","Driver_initSourceTerms.F90","Driver_sourceTerms.F90"]
		for fle in flist:
			shutil.copy(patchFolder+pFolder+fle, flashFolder+pFolder+fle)

		########physics#############
		#*******Config**********
		pFolder = "physics/sourceTerms/KromeChemistry/KromeChemistryMain/"
		species = ""
		speciesCount = 0
		for x in specs:
			if x.name in excl: continue
			name = x.name.upper().replace("+","P").replace("-","M")
			if name == "E": name="ELEC"
			speciesCount += 1
			species += "SPECIES "+name+"\n"

		fname = "Config"
		self.replacein(patchFolder+pFolder+fname, flashFolder+pFolder+fname,["#KROME_spec_data"],[species])

		#*******build->physics****************
		pFolder = "physics/sourceTerms/KromeChemistry/KromeChemistryMain/"
		kromeFileList = ["krome_all.f90", "krome_user_commons.f90", "opkda1.f", "opkda2.f", "opkdmain.f"]
		for fl in kromeFileList:
			shutil.move(buildFolder+fl, flashFolder+pFolder+fl)

		#***********physics->physics***********
		pFolder = "physics/sourceTerms/KromeChemistry/"
		kromeFileList = ["KromeChemistry.F90","KromeChemistry_finalize.F90",
			"KromeChemistry_init.F90", "KromeChemistry_interface.F90", "Makefile"]
		for fl in kromeFileList:
			shutil.copy(patchFolder+pFolder+fl, flashFolder+pFolder+fl)

		#***********physics_main->physics_main***********
		kromeFileList = ["KromeChemistry.F90", "KromeChemistry_data.F90", "KromeChemistry_init.F90", "Makefile"]
		pFolder = "physics/sourceTerms/KromeChemistry/KromeChemistryMain/"
		for fl in kromeFileList:
			shutil.copy(patchFolder+pFolder+fl, flashFolder+pFolder+fl)

		#**************pchem_mapNetworkToSpecies*********
		pFolder = "physics/sourceTerms/KromeChemistry/KromeChemistryMain/"
		fname = "pchem_mapNetworkToSpecies.F90"
		species = ""
		for x in specs:
			if x.name in excl: continue
			name = x.name.upper().replace("+","P").replace("-","M")
			if name == "E": name="ELEC"
			species += "\tcase(\""+x.name+"\")\n"
			species += "\t\tspecieOut = "+name+"_SPEC\n"

		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_cases"],[species])
		indentF90(flashFolder+pFolder+fname)

		########Simulation#############
		#***********pfold->treeFolder***********
		kromeFileList = ["Config"]
		pFolder = "Simulation/SimulationComposition/KromeChemistry/"
		for fl in kromeFileList:
			shutil.copy(patchFolder+pFolder+fl, flashFolder+pFolder+fl)

		#******simulation_initSpecies
		fname = "Simulation_initSpecies.F90"
		pFolder = "Simulation/SimulationComposition/KromeChemistry/"
		self.replacein(patchFolder+pFolder+fname, flashFolder+pFolder+fname,["#KROME_specnum"],[str(speciesCount)])
		indentF90(flashFolder+pFolder+fname)

		#SpeciesList.txt
		fname = "SpeciesList.txt"
		pFolder = "Simulation/SimulationComposition/KromeChemistry/"
		spec_data = ""
		all_parts = []
		for x in specs:
			if x.name in excl: continue
			name = x.name.upper().replace("+","P").replace("-","M")
			if name == "E": name="ELEC"
			#prepares gamma depending on the model employed for gamma
			if self.typeGamma == "FULL":
				if x.is_atom : #monoatomic
					gamma = 5./3.
				else: #molecule
					gamma = 7./5.
			elif self.typeGamma == "DEFAULT":
				gamma = "1.66666666667d0"
			else:
				gamma = self.typeGamma
			if 'Q' in name: gamma = "1d99"
			all_parts.append([name, x.zatom, x.mass, x.neutrons, x.zatom-x.charge,gamma])
		all_parts = sorted(all_parts,key=lambda x:x[1]) #sort by atomic number
		for parts in all_parts:
			spec_data += ("".join([str(y)+(20-len(str(y)))*" " for y in parts]))	+ "\n"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_spec_data"],[spec_data])

		#************####Collapse example###**************
		pFolder = "Simulation/SimulationMain/Chemistry_Krome_Collapse/"
		specs_config = specs_par = specs_data =  specs_init = ""
		specs_block_vars = specs_block_if = specs_block_prop = ""
		ablocke = []
		for x in specs:
			if x.name in excl: continue
			name = x.name.upper().replace("+","P").replace("-","M")
			if name == "E": name="ELEC"
			if x.name in ndef:
				nn = ndef[x.name]
			else:
				nn = ndef["default"]
			parts = ["PARAMETER", "sim_x"+name, "REAL", nn]
			specs_config += ("".join([str(y)+(20-len(str(y)))*" " for y in parts]))	+ "\n"
			parts = ["sim_x"+name, "=", nn]
			specs_par += ("".join([str(y)+(20-len(str(y)))*" " for y in parts]))	+ "\n"
			specs_data += "real, save :: sim_x"+name +"\n"
			specs_init += "call RuntimeParameters_get(\"sim_x"+name+"\", sim_x"+name+")\n"
			specs_block_vars += "real :: "+name.lower()+"A\n"
			specs_block_if += "if("+name+"_SPEC > 0) massFraction("+name+"_SPEC) = max(sim_x"+name+", smallx)\n"
			specs_block_prop += "call Multispecies_getProperty("+name+"_SPEC,A,"+name.lower()+"A)\n"
			if x.charge != 0 and x.name != "E":
				if x.charge == 1:
					emult = " +"
				elif x.charge == -1:
					emult = " -"
				else:
					emult = " "+str(int(x.charge))+"*"
					if x.charge > 0: emult = "+" + emult
				ablocke.append(emult+"massFraction("+name+"_SPEC)/"+name.lower()+"A")
		specs_block_e = "if (ELEC_SPEC > 0) massFraction(ELEC_SPEC) = max( elecA*(&\n"
		specs_block_e += ("&\n".join(ablocke))
		specs_block_e += "&\n), smallx)"

		fname = "Config"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_specs_config"],[specs_config])
		indentF90(flashFolder+pFolder+fname)

		fname = "flash.par"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_specs_par"],[specs_par])
		indentF90(flashFolder+pFolder+fname)

		fname = "flash.par_1d"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_specs_par"],[specs_par])
		indentF90(flashFolder+pFolder+fname)

		fname = "Simulation_data.F90"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_specs_data"],[specs_data])
		indentF90(flashFolder+pFolder+fname)

		fname = "Simulation_init.F90"
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,["#KROME_specs_init"],[specs_init])
		indentF90(flashFolder+pFolder+fname)

		fname = "Simulation_initBlock.F90"
		pragmas = ["#KROME_specs_block_vars", "#KROME_specs_block_if","#KROME_specs_block_prop","#KROME_specs_block_e"]
		reps = [specs_block_vars, specs_block_if,specs_block_prop,specs_block_e]
		self.replacein(patchFolder+pFolder+fname,flashFolder+pFolder+fname,pragmas,reps)
		indentF90(flashFolder+pFolder+fname)

		#***********pfold->treeFolder***********
		kromeFileList = ["Makefile"]
		pFolder = "Simulation/SimulationMain/Chemistry_Krome_Collapse/"
		for fl in kromeFileList:
			shutil.copy(patchFolder+pFolder+fl, flashFolder+pFolder+fl)

	########################################
	#cut the arg sting using sep as separtor
	# when a piece is longer than largmax append the
	# chracter rep
	def linebreakerC(self,arg,sep,largmax = 40,rep="\n"):

		aarg = arg.split(sep)
		sarg = ""
		larg = 0
		for x in aarg[:len(aarg)-1]:
			sarg += x + sep
			larg += len(x+sep)
			if larg > largmax:
				sarg += rep
				larg = 0
		sarg += aarg[len(aarg)-1]
		return sarg

	###########################################
	def enzo_patch(self):
		buildFolder = self.buildFolder
		enzoFolder = buildFolder+"krome_enzo_patch/"
		patchFolder = "patches/enzo/"

		if not os.path.exists(enzoFolder): os.makedirs(enzoFolder)

		specs = self.specs
		excl = ["CR","g","Tgas","dummy"] #species to exclude
		speciesCount = 0
		krome_driver_args = krome_driver_rprec = krome_driver_scale = ""
		krome_driver_minval = krome_driver_dom = krome_driver_mod = ""
		krome_identify_num = ""
		krome_solve_args = krome_solve_baryon = ""
		krome_driver_suma = []
		kdrive_args = []
		krome_identify_identifya = []
		krome_identify_zeroa = []
		krome_identify_binarya = []
		krome_identify_vfail1a = []
		krome_identify_vfail2a = []
		krome_solve_numa = []
		krome_solve_identifya = []
		krome_grid_identifya = []
		for x in specs:
			if x.name in excl: continue
			uname = x.name.upper() #upper-case name
			uname = uname.replace("HE","He") #Helium is lowecase
			#enzo-like names, e.g. H- -> HM, and H+ -> HII
			if "-" in uname:
				name = uname.replace("-","M") #anions
			else:
				name = (uname+"I").replace("+","I") #neutral and ions
			extname = name+"Density"
			if name == "EI":
				name = "De" #electron is special
				extname = "ElectronDensity"
			speciesCount += 1 #increases species count
			#1. DRIVER file pragama
			krome_driver_args += name+", " #function arguments
			if speciesCount % 4 == 0: krome_driver_args += "&\n"
			krome_driver_rprec += "real*8::"+name+"(in,jn,kn)\n"
			krome_driver_scale += name+"(i,j,k) = "+name+"(i,j,k) * factor\n"
			krome_driver_minval += name+"(i,j,k) = max("+name+"(i,j,k), krome_tiny)\n"
			dmult = ""
			xzn = x.zatom+x.neutrons
			if xzn > 1: dmult = "* "+str(1e0/xzn)+"d0"
			krome_driver_dom += "krome_x(krome_"+x.fidx+") = "+name+"(i,j,k) * dom "+dmult+"\n"
			krome_driver_suma.append("+"+name+"(i,j,k) "+dmult)
			dmult2 = ""
			if(xzn>1): dmult2 = "* "+str(xzn)+"d0"
			krome_driver_mod += name+"(i,j,k) = krome_x(krome_"+x.fidx+") * idom "+dmult2+"\n"

			#2. Grid_IdentifySpeciesFieldsKrome.C
			krome_identify_identifya.append("int &"+name+"Num")
			krome_identify_zeroa.append(name+"Num")
			krome_identify_binarya.append(name+"Num<0")
			krome_identify_vfail1a.append(name+"=%\"ISYM\"")
			krome_identify_vfail2a.append(name+"Num")
			krome_identify_num += " "+name+"Num = FindField("+extname+", FieldType, NumberOfBaryonFields);\n"

			#3. Grid_SolveRateAndCoolEquations
			krome_solve_args += "float *"+name+", "
			krome_solve_baryon += "BaryonField["+name+"Num], "
			krome_solve_numa.append(name+"Num")
			krome_solve_identifya.append(name+"Num")

			#4. GridKrome.h
			krome_grid_identifya.append("int &"+name+"Num")

		if speciesCount % 3 != 0: krome_driver_args += "&"
		krome_driver_sum = (" &\n".join(krome_driver_suma))

		krome_identify_identify = self.linebreakerC((", ".join(krome_identify_identifya)), ",")
		krome_identify_zero = self.linebreakerC((" = ".join(krome_identify_zeroa)), "=")+" = 0;"
		krome_identify_binary = self.linebreakerC((" || ".join(krome_identify_binarya)), "||")
		krome_identify_vfail1 = "\""+(", ".join(krome_identify_vfail1a))+"\\n\""
		krome_identify_vfail2 = self.linebreakerC((", ".join(krome_identify_vfail2a)), ",")

		krome_solve_args = self.linebreakerC(krome_solve_args, ",")
		krome_solve_num = "int "+ self.linebreakerC((", ".join(krome_solve_numa)), ",")+";"
		krome_solve_identify = self.linebreakerC((", ".join(krome_solve_identifya)), ",")
		krome_solve_baryon = self.linebreakerC(krome_solve_baryon, ",")

		krome_grid_identify = self.linebreakerC((", ".join(krome_grid_identifya)), ",")

		#1. replace
		fname = "krome_driver.F90"
		prags = ["#KROME_args","#KROME_rprec","#KROME_scale","#KROME_minval","#KROME_dom","#KROME_mod"]
		reps = [krome_driver_args,krome_driver_rprec,krome_driver_scale,krome_driver_minval]
		reps += [krome_driver_dom,krome_driver_mod]
		self.replacein(patchFolder+fname, enzoFolder+fname, prags, reps)
		indentF90(enzoFolder+fname)

		#2. replace in Grid_IdentifySpeciesFieldsKrome.C
		fname = "Grid_IdentifySpeciesFieldsKrome.C"
		prags = ["#KROME_identify", "#KROME_zero","#KROME_binary", "#KROME_vfail1", "#KROME_vfail2", "#KROME_num"]
		reps = [krome_identify_identify, krome_identify_zero, krome_identify_binary]
		reps += [krome_identify_vfail1, krome_identify_vfail2, krome_identify_num]
		self.replacein(patchFolder+fname, enzoFolder+fname, prags, reps, False)

		#3. replace in Grid_SolveRateAndCoolEquations.C
		fname = "Grid_SolveRateAndCoolEquations.C"
		prags = ["#KROME_args", "#KROME_num", "#KROME_identify","#KROME_baryon"]
		reps = [krome_solve_args, krome_solve_num, krome_solve_identify, krome_solve_baryon]
		self.replacein(patchFolder+fname, enzoFolder+fname, prags, reps, False)

		#4. replace in GridKrome.h
		fname = "GridKrome.h"
		self.replacein(patchFolder+fname, enzoFolder+fname, ["#KROME_identify"], [krome_grid_identify], False)

		#5. copy others
		fname = "evaluate_tgas.F90"
		shutil.copy(patchFolder+fname, enzoFolder+fname)
		fname = "InitializeRateData.C"
		shutil.copy(patchFolder+fname, enzoFolder+fname)
		fname = "krome_initab.F90"
		shutil.copy(patchFolder+fname, enzoFolder+fname)
		fname = "notes.txt"
		shutil.copy(patchFolder+fname, enzoFolder+fname)
		fname = "krome_enzo.py"
		shutil.copy(patchFolder+fname, enzoFolder+fname)

		if self.useDvodeF90:
				fname = "kromebuild_dvode.sh"
				shutil.copy(patchFolder+fname, enzoFolder+"kromebuild.sh")
		else:
			fname = "kromebuild.sh"
			shutil.copy(patchFolder+fname, enzoFolder+fname)

		#6. move others
		flist = ["krome_all", "krome_user_commons"]
		for fle in flist:
			shutil.move(buildFolder+fle+".f90", enzoFolder+fle+".F90")

		if self.useDvodeF90:
			flist = ["dvode_f90_m"]
			for fle in flist:
				shutil.move(buildFolder+fle+".f90", enzoFolder+fle+".F90")
		else:
			flist = ["opkda1", "opkda2", "opkdmain"]
			for fle in flist:
				shutil.move(buildFolder+fle+".f", enzoFolder+fle+".F")

		return
	############################################
	#prepare the patches if needed
	def patches(self):
		if self.doFlash: self.flash_patch()
		if self.doRamses: self.ramses_patch()
		#if(self.doRamses2011): self.ramses_patch()
		if self.doRamsesTH: self.ramsesTH_patch()
		if self.doEnzo: self.enzo_patch()
		if self.doGizmo: self.gizmo_patch()
		return

	#########################################
	def final_report(self):

		buildFolder = self.buildFolder
		reacts = self.reacts
		useX = self.useX
		nmols = self.nmols
		print("")
		print("You'll find the necessary files in "+buildFolder)
		print("Example call to the solver in "+buildFolder+"test.f90")
		print("Example Makefile in "+buildFolder+"Makefile")

		#check for large reaction set
		if len(reacts) > 500 and not self.use_implicit_RHS:
			print("")
			print("WARNING: "+str(len(reacts)) 
				  + " reactions found! Using implicit RHS (option -iRHS)")
			print("could be more efficient and also allows faster compilation.")
			a = keyb_input("Any key to continue q to quit... ")
			if a == "q": sys.exit()

		print("")
		#IF NOT TEST
		if not self.is_test:
			#PATCHES DO NOT NEED MAKEFILE AND TEST.F90, and also if noExample is enabled
			if not(self.doFlash or self.doRamses or self.doEnzo or self.doRamsesTH 
				   or self.noExample):
				#TODO: add description in case of dust
				print("Call KROME in your code as:")
				if useX:
					print("    call krome(x(:), gas_density, gas_temperature, time_step)")
				else:
					print("    call krome(x(:), gas_temperature, time_step)")
				print("where:")
				print(" x(:) is a real*8 array of size "+str(nmols) 
					  + (" of the mass fractions" if useX else " of number densities [1/cm3]"))
				if useX: print(" gas_density  is the gas density in [g/cm3]")
				print(" gas_temperature is the gas temperature in [K]")
				print(" time_step is the integration time-step in [s]")

				if not self.isdry:
					fout = open(buildFolder+"test.f90","w")
					fout.write(get_example(nmols,useX))
					fout.close()
					indentF90(buildFolder+"test.f90")
					foundList = ["#KROME_compiler"]
					repList = [self.compiler]
					if self.buildCompact:
						self.replacein("tests/MakefileCompact", buildFolder+"Makefile", foundList, repList, False)
					else:
						if self.pedanticMakefile:
							self.replacein("tests/Makefile_pedantic", buildFolder+"Makefile", foundList, repList, False)
						else:
							self.replacein("tests/Makefile", buildFolder+"Makefile", foundList, repList, False)

		#IF IT IS A TEST
		else:
			print("This is a test. To run it just type:")
			print("> cd build/")
			print("> make")
			print("> ./test")
		print("")
		print("Everything done, goodbye!")
