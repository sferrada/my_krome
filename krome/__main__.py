#!/usr/bin/env python
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
# THIS IS THE MAIN OF KROME PYHTON
import sys
from kromeobj import *
from kromelib import get_quote

argv = sys.argv

print("******************************")
print("      WELCOME TO KROME")
print("******************************")
print("")
# create the KROME object
krome = krome()

# check prerequisites
krome.checkPrereq()

# prepares mass an species dictionaries
krome.prepare_massdict()

# read thermochemistry data from file
krome.load_thermochemistry()

# init argaprser
krome.init_argparser()

# search argv for tests
krome.select_test(argv)

# read argv options
krome.argparsing(argv)

# check if build folder is empty
krome.safe_check()

# load the phys variables
krome.definePhysVariables()

# read reaction file
krome.read_file()

# issue photo-warning is needed
krome.photo_warnings()

# compute reverse kinetics if necessary
krome.do_reverse()

# check if reactions have reverse in this network
krome.check_reverse()

# verify thermochemical data for species
krome.verifyThermochem()

# add metals if cooling is selected
krome.addMetals()

# add a minimum value to reactions
krome.addReaMin()

# compute enthalpy of reactions if necessary
krome.computeEnthalpy()

# add the dust if selected
krome.addDust()

# add special species (e.g. dummy)
krome.addSpecial()

# count species, dust, ecc...
krome.countSpecies()

# dump reactions and species to a file
krome.dumpNetwork()

# show ODE stats
krome.showODE()

# create ODE differentials
krome.createODE()

# create Jacobian
krome.createJAC()

# create sparsity structure
krome.IACJAC()

# compute solver parameters
krome.solverParams()

# read metal cooling from data file
krome.createZcooling()

if not krome.isdry:
	# prepare build directory
	krome.prepareBuild()

	# replace pragma from src files
	krome.makeCommons()
	krome.makeConstants()
	krome.makeUserCommons()
	krome.makeFit()
	krome.makeGetPhys()
	krome.makeGammaAdiabatic()
	krome.makeGrainFuncs()
	krome.dumpReactionsVerbatim()
	krome.makePhotoFuncs()
	krome.makeSubs()
	krome.makeStars()
	krome.makeDust()
	krome.makePhoto()
	krome.makeTabs()
	krome.makeCoolingGH()
	krome.makeCooling()
	krome.makeHeating()
	krome.makeODE()
	krome.makeUser()
	krome.makeReduction()
	krome.makeMain()

	# create a gnuplot script for report
	krome.makeReport()

	# copy other files
	krome.copyOthers()

	# indent all the f90 files
	krome.indent()

	# create patches if needed
	krome.patches()

# prepares a C interface if desired
krome.CInterface()

# prepares a Python interface if desired
krome.PyInterface()

# print a final report an usage hints
krome.final_report()

# write a random quote
get_quote()

