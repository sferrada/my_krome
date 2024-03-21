
!############### MODULE ##############
module krome_user
  use iso_c_binding
  implicit none

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2024-03-19 13:48:05
  !  Changeset xxxxxxx
  !  see http://kromepackage.org
  !
  !  Written and developed by Tommaso Grassi and Stefano Bovino
  !
  !  Contributors:
  !  J.Boulangier, T.Frostholm, D.Galli, F.A.Gianturco, T.Haugboelle,
  !  A.Lupi, J.Prieto, J.Ramsey, D.R.G.Schleicher, D.Seifried, E.Simoncini,
  !  E.Tognelli
  !  KROME is provided "as it is", without any warranty.
  ! *************************************************************

  integer,parameter::KROME_idx_E = 1	!E
  integer,parameter::KROME_idx_Hk = 2	!H-
  integer,parameter::KROME_idx_H = 3	!H
  integer,parameter::KROME_idx_HE = 4	!HE
  integer,parameter::KROME_idx_H2 = 5	!H2
  integer,parameter::KROME_idx_D = 6	!D
  integer,parameter::KROME_idx_HD = 7	!HD
  integer,parameter::KROME_idx_Hj = 8	!H+
  integer,parameter::KROME_idx_HEj = 9	!HE+
  integer,parameter::KROME_idx_H2j = 10	!H2+
  integer,parameter::KROME_idx_Dj = 11	!D+
  integer,parameter::KROME_idx_HEjj = 12	!HE++
  integer,parameter::KROME_idx_CR = 13	!CR
  integer,parameter::KROME_idx_g = 14	!g
  integer,parameter::KROME_idx_Tgas = 15	!Tgas
  integer,parameter::KROME_idx_dummy = 16	!dummy

  integer,parameter::krome_idx_cool_h2 = 1
  integer,parameter::krome_idx_cool_h2gp = 2
  integer,parameter::krome_idx_cool_atomic = 3
  integer,parameter::krome_idx_cool_cen = 3
  integer,parameter::krome_idx_cool_hd = 4
  integer,parameter::krome_idx_cool_z = 5
  integer,parameter::krome_idx_cool_metal = 5
  integer,parameter::krome_idx_cool_dh = 6
  integer,parameter::krome_idx_cool_enthalpic = 6
  integer,parameter::krome_idx_cool_dust = 7
  integer,parameter::krome_idx_cool_compton = 8
  integer,parameter::krome_idx_cool_cie = 9
  integer,parameter::krome_idx_cool_continuum = 10
  integer,parameter::krome_idx_cool_cont = 10
  integer,parameter::krome_idx_cool_exp = 11
  integer,parameter::krome_idx_cool_expansion = 11
  integer,parameter::krome_idx_cool_ff = 12
  integer,parameter::krome_idx_cool_bss = 12
  integer,parameter::krome_idx_cool_custom = 13
  integer,parameter::krome_idx_cool_co = 14
  integer,parameter::krome_idx_cool_zcie = 15
  integer,parameter::krome_idx_cool_zcienouv = 16
  integer,parameter::krome_idx_cool_zextend = 17
  integer,parameter::krome_idx_cool_gh = 18
  integer,parameter::krome_idx_cool_oh = 19
  integer,parameter::krome_idx_cool_h2o = 20
  integer,parameter::krome_idx_cool_hcn = 21
  integer,parameter::krome_ncools = 21

  integer,parameter::krome_idx_heat_chem = 1
  integer,parameter::krome_idx_heat_compress = 2
  integer,parameter::krome_idx_heat_compr = 2
  integer,parameter::krome_idx_heat_photo = 3
  integer,parameter::krome_idx_heat_dh = 4
  integer,parameter::krome_idx_heat_enthalpic = 4
  integer,parameter::krome_idx_heat_photoav = 5
  integer,parameter::krome_idx_heat_av = 5
  integer,parameter::krome_idx_heat_cr = 6
  integer,parameter::krome_idx_heat_dust = 7
  integer,parameter::krome_idx_heat_xray = 8
  integer,parameter::krome_idx_heat_visc = 9
  integer,parameter::krome_idx_heat_viscous = 9
  integer,parameter::krome_idx_heat_custom = 10
  integer,parameter::krome_idx_heat_zcie = 11
  integer,parameter::krome_nheats = 11

  integer,parameter::krome_nrea=38
  integer,parameter::krome_nmols=12
  integer,parameter::krome_nspec=16
  integer,parameter::krome_natoms=4
  integer,parameter::krome_ndust=0
  integer,parameter::krome_ndustTypes=0
  integer,parameter::krome_nPhotoBins=0
  integer,parameter::krome_nPhotoRates=0

  real*8,parameter::krome_boltzmann_eV = 8.617332478d-5 !eV / K
  real*8,parameter::krome_boltzmann_J = 1.380648d-23 !J / K
  real*8,parameter::krome_boltzmann_erg = 1.380648d-16 !erg / K
  real*8,parameter::krome_iboltzmann_eV = 1d0/krome_boltzmann_eV !K / eV
  real*8,parameter::krome_iboltzmann_erg = 1d0/krome_boltzmann_erg !K / erg
  real*8,parameter::krome_planck_eV = 4.135667516d-15 !eV s
  real*8,parameter::krome_planck_J = 6.62606957d-34 !J s
  real*8,parameter::krome_planck_erg = 6.62606957d-27 !erg s
  real*8,parameter::krome_iplanck_eV = 1d0/krome_planck_eV !1 / eV / s
  real*8,parameter::krome_iplanck_J = 1d0/krome_planck_J !1 / J / s
  real*8,parameter::krome_iplanck_erg = 1d0/krome_planck_erg !1 / erg / s
  real*8,parameter::krome_gravity = 6.674d-8 !cm3 / g / s2
  real*8,parameter::krome_e_mass = 9.10938188d-28 !g
  real*8,parameter::krome_p_mass = 1.67262158d-24 !g
  real*8,parameter::krome_n_mass = 1.674920d-24 !g
  real*8,parameter::krome_ip_mass = 1d0/krome_p_mass !1/g
  real*8,parameter::krome_clight = 2.99792458e10 !cm/s
  real*8,parameter::krome_pi = 3.14159265359d0 !#
  real*8,parameter::krome_eV_to_erg = 1.60217646d-12 !eV -> erg
  real*8,parameter::krome_ry_to_eV = 13.60569d0 !rydberg -> eV
  real*8,parameter::krome_ry_to_erg = 2.179872d-11 !rydberg -> erg
  real*8,parameter::krome_seconds_per_year = 365d0*24d0*3600d0 !yr -> s
  real*8,parameter::krome_km_to_cm = 1d5 !km -> cm
  real*8,parameter::krome_cm_to_Mpc = 1.d0/3.08d24 !cm -> Mpc
  real*8,parameter::krome_kvgas_erg = 8.d0*krome_boltzmann_erg/krome_pi/krome_p_mass !
  real*8,parameter::krome_pre_kvgas_sqrt = sqrt(8.d0*krome_boltzmann_erg/krome_pi) !
  real*8,parameter::krome_pre_planck = 2.d0*krome_planck_erg/krome_clight**2 !erg/cm2*s3
  real*8,parameter::krome_exp_planck = krome_planck_erg / krome_boltzmann_erg !s*K
  real*8,parameter::krome_stefboltz_erg = 5.670373d-5 !erg/s/cm2/K4
  real*8,parameter::krome_N_avogadro = 6.0221d23 !#
  real*8,parameter::krome_Rgas_J = 8.3144621d0 !J/K/mol
  real*8,parameter::krome_Rgas_kJ = 8.3144621d-3 !kJ/K/mol
  real*8,parameter::krome_hubble = 0.704d0 !dimensionless
  real*8,parameter::krome_Omega0 = 1.0d0 !dimensionless
  real*8,parameter::krome_Omegab = 0.0456d0 !dimensionless
  real*8,parameter::krome_Hubble0 = 1.d2*krome_hubble*krome_km_to_cm*krome_cm_to_Mpc !1/s

contains

  !*******************
  subroutine krome_set_user_Tdust(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_Tdust = argset
  end subroutine krome_set_user_Tdust

  !*******************
  function krome_get_user_Tdust() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_Tdust
    krome_get_user_Tdust = user_Tdust
  end function krome_get_user_Tdust

  !*******************
  subroutine krome_set_user_xdust(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_xdust = argset
  end subroutine krome_set_user_xdust

  !*******************
  function krome_get_user_xdust() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_xdust
    krome_get_user_xdust = user_xdust
  end function krome_get_user_xdust

  !*******************
  subroutine krome_set_user_gsize(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_gsize = argset
  end subroutine krome_set_user_gsize

  !*******************
  function krome_get_user_gsize() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_gsize
    krome_get_user_gsize = user_gsize
  end function krome_get_user_gsize

  !*******************
  subroutine krome_set_user_gsize2(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_gsize2 = argset
  end subroutine krome_set_user_gsize2

  !*******************
  function krome_get_user_gsize2() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_gsize2
    krome_get_user_gsize2 = user_gsize2
  end function krome_get_user_gsize2

  !*******************
  subroutine krome_set_user_crflux(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_crflux = argset
  end subroutine krome_set_user_crflux

  !*******************
  function krome_get_user_crflux() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_crflux
    krome_get_user_crflux = user_crflux
  end function krome_get_user_crflux

  !*******************
  subroutine krome_set_user_Av(argset) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: argset
    user_Av = argset
  end subroutine krome_set_user_Av

  !*******************
  function krome_get_user_Av() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_user_Av
    krome_get_user_Av = user_Av
  end function krome_get_user_Av

  !************************
  !returns the Tdust averaged over the number density
  ! as computed in the tables
  function krome_get_table_Tdust(x,Tgas) bind(C)
    use krome_commons
    use krome_grfuncs
    implicit none
    real(kind=c_double), value :: Tgas
    real(kind=c_double) :: x(nmols), krome_get_table_Tdust
    real*8::n(nspec)

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas

    krome_get_table_Tdust = get_table_Tdust(n(:))

  end function krome_get_table_Tdust

  !MOCASSIN interface not used when C interface is active

  !**********************
  !convert number density (cm-3) into column
  ! density (cm-2) using the specific density
  ! column method (see help for option
  ! -columnDensityMethod)
  ! num is the number density, x(:) is the species
  ! array, Tgas is the gas temperature
  ! If the method is not JEANS, x(:) and Tgas
  ! are dummy variables
  function krome_num2col(num,x,Tgas) bind(C)
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    real(kind=c_double) :: x(nmols),krome_num2col
    real(kind=c_double), value :: Tgas,num
    real*8::n(nspec)

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas

    krome_num2col = num2col(num,n(:))

  end function krome_num2col

  !***********************
  !print on screen the current values of all phys variables
  subroutine krome_print_phys_variables() bind(C)
    use krome_commons
    implicit none

    print *, "Tcmb:", phys_Tcmb
    print *, "zredshift:", phys_zredshift
    print *, "orthoParaRatio:", phys_orthoParaRatio
    print *, "metallicity:", phys_metallicity
    print *, "Tfloor:", phys_Tfloor

  end subroutine krome_print_phys_variables

  !*******************
  subroutine krome_set_Tcmb(arg) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: arg
    phys_Tcmb = arg
  end subroutine krome_set_Tcmb

  !*******************
  function krome_get_Tcmb() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_Tcmb
    krome_get_Tcmb = phys_Tcmb
  end function krome_get_Tcmb

  !*******************
  subroutine krome_set_zredshift(arg) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: arg
    phys_zredshift = arg
  end subroutine krome_set_zredshift

  !*******************
  function krome_get_zredshift() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_zredshift
    krome_get_zredshift = phys_zredshift
  end function krome_get_zredshift

  !*******************
  subroutine krome_set_orthoParaRatio(arg) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: arg
    phys_orthoParaRatio = arg
  end subroutine krome_set_orthoParaRatio

  !*******************
  function krome_get_orthoParaRatio() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_orthoParaRatio
    krome_get_orthoParaRatio = phys_orthoParaRatio
  end function krome_get_orthoParaRatio

  !*******************
  subroutine krome_set_metallicity(arg) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: arg
    phys_metallicity = arg
  end subroutine krome_set_metallicity

  !*******************
  function krome_get_metallicity() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_metallicity
    krome_get_metallicity = phys_metallicity
  end function krome_get_metallicity

  !*******************
  subroutine krome_set_Tfloor(arg) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: arg
    phys_Tfloor = arg
  end subroutine krome_set_Tfloor

  !*******************
  function krome_get_Tfloor() bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_Tfloor
    krome_get_Tfloor = phys_Tfloor
  end function krome_get_Tfloor

  ! #IFKROME_useSemenov
  !   !******************************
  !   !this function sets dust variables for Semenov's framework
  !   ! works. it might be incomplete...
  !   subroutine krome_get_dust_variables(ngas,dust_gas_ratio,rho0) bind(C)
  !     use krome_commons
  !     use krome_subs
  !     use krome_constants
  !     use krome_getphys
  !     use krome_grfuncs
  !     implicit none
  !     real(kind=c_double) :: rhogas,dmass,d2g
  !     real(kind=c_double), value :: rho0,dust_gas_ratio
  !     real*8::ngas(nmols)
  !
  !     d2g = dust_gas_ratio
  !     rhogas = get_rho(ngas)
  !
  !     dmass = 4./3d0*pi*rho0*krome_dust_asize3
  !     xdust = dust_gas_ratio*rhogas/(dmass)
  !
  !     Ebinding(:) = get_EbindBare()
  !     call dust_nu0_evaluation()
  !     call dust_evap70()
  !
  !   end subroutine krome_get_dust_variables
  ! #ENDIFKROME_useSemenov

  !*****************************
  !dump the data for restart (UNDER DEVELOPEMENT!)
  !arguments: the species array and the gas temperature
  subroutine krome_store(x,Tgas,dt) bind(C)
    use krome_commons
    implicit none
    integer::nfile,i
    real(kind=c_double) :: x(nmols)
    real(kind=c_double), value :: Tgas,dt

    nfile = 92

    open(nfile,file="krome_dump.dat",status="replace")
    !dump temperature
    write(nfile,*) Tgas
    write(nfile,*) dt
    !dump species
    do i=1,nmols
      write(nfile,*) x(i)
    end do
    close(nfile)

  end subroutine krome_store

  !*****************************
  !restore the data from a dump (UNDER DEVELOPEMENT!)
  !arguments: the species array and the gas temperature
  subroutine krome_restore(x,Tgas,dt) bind(C)
    use krome_commons
    implicit none
    integer::nfile,i
    real(kind=c_double) :: x(nmols)
    real(kind=c_double), value :: Tgas,dt

    nfile = 92

    open(nfile,file="krome_dump.dat",status="old")
    !restore temperature
    read(nfile,*) Tgas
    read(nfile,*) dt
    !restore species
    do i=1,nmols
      read(nfile,*) x(i)
    end do
    close(nfile)

  end subroutine krome_restore

  !****************************
  !switch on the thermal calculation
  subroutine krome_thermo_on() bind(C)
    use krome_commons
    krome_thermo_toggle = 1
  end subroutine krome_thermo_on

  !****************************
  !switch off the thermal calculation
  subroutine krome_thermo_off() bind(C)
    use krome_commons
    krome_thermo_toggle = 0
  end subroutine krome_thermo_off

  !***************************
  !alias for coe in krome_subs
  ! returns the coefficient array of size krome_nrea
  ! for a given Tgas
  function krome_get_coef(Tgas,x)
    use krome_commons
    use krome_subs
    use krome_tabs
    real*8 :: krome_get_coef(nrea),x(nmols)
    real*8,value:: Tgas
    real*8::n(nspec)
    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas

    krome_get_coef(:) = coe(n(:))

  end function krome_get_coef

  !****************************
  !get the mean molecular weight from
  ! mass fractions
  function krome_get_mu_x(xin) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double) :: xin(nmols), krome_get_mu_x
    real*8::n(nmols)
    n(:) = krome_x2n(xin(:),1d0)
    krome_get_mu_x = krome_get_mu(n(:))
  end function krome_get_mu_x

  !****************************
  !return the adiabatic index from mass fractions
  ! and temperature in K
  function krome_get_gamma_x(xin,inTgas) bind(C)
    use krome_commons
    implicit none
    real(kind=c_double), value :: inTgas
    real(kind=c_double) :: xin(nmols), krome_get_gamma_x
    real*8::x(nmols),Tgas,rhogas

    Tgas = inTgas
    x(:) = krome_x2n(xin(:),1d0)
    krome_get_gamma_x = krome_get_gamma(x(:),Tgas)

  end function krome_get_gamma_x

  !***************************
  !normalize mass fractions and
  ! set charge to zero
  subroutine krome_consistent_x(x) bind(C)
    use krome_commons
    use krome_constants
    implicit none
    real(kind=c_double) :: x(nmols)
    real*8::isumx,sumx,xerr,imass(nmols),ee

    !1. charge consistency
    imass(:) = krome_get_imass()

    x(idx_e) = 0.d0

    ee = sum(krome_get_charges()*x(:)*imass(:))
    ee = max(ee*e_mass,0d0)
    x(idx_e) = ee

    !2. mass fraction consistency
    sumx = sum(x)

    !NOTE: uncomment here if you want some additional control
    !conservation error threshold: rise an error if above xerr
    !xerr = 1d-2
    !if(abs(sum-1d0)>xerr) then
    !   print *,"ERROR: some problem with conservation!"
    !   print *,"|sum(x)-1|=",abs(sum-1d0)
    !   stop
    !end if

    isumx = 1d0/sumx
    x(:) = x(:) * isumx

  end subroutine krome_consistent_x

  !*********************
  !return an array sized krome_nmols containing
  ! the mass fractions (#), computed from the number
  ! densities (1/cm3) and the total density in g/cm3
  function krome_n2x(n,rhogas)
    use krome_commons
    implicit none
    real*8 :: n(nmols),krome_n2x(nmols)
    real*8,value :: rhogas

    krome_n2x(:) = n(:) * krome_get_mass() / rhogas

  end function krome_n2x

  !********************
  !return an array sized krome_nmols containing
  ! the number densities (1/cm3), computed from the mass
  ! fractions and the total density in g/cm3
  function krome_x2n(x,rhogas)
    use krome_commons
    implicit none
    real*8 :: x(nmols),krome_x2n(nmols)
    real*8,value :: rhogas

    !compute densities from fractions
    krome_x2n(:) = rhogas * x(:) * krome_get_imass()

  end function krome_x2n

  !******************
  !returns free-fall time using the number density
  ! abundances of array x(:)
  function krome_get_free_fall_time(x)
    use krome_commons
    use krome_getphys
    implicit none
    real*8::krome_get_free_fall_time
    real*8::x(:),n(nspec)

    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0
    krome_get_free_fall_time = get_free_fall_time(n(:))

  end function krome_get_free_fall_time

  !******************
  !returns free-fall time using the total mass density
  !  of gas, rhogas (g/cm3)
  function krome_get_free_fall_time_rho(rhogas)
    use krome_getphys
    implicit none
    real*8::krome_get_free_fall_time_rho
    real*8::rhogas

    krome_get_free_fall_time_rho = get_free_fall_time_rho(rhogas)

  end function krome_get_free_fall_time_rho

  !*******************
  !do only cooling and heating
  subroutine krome_thermo(x,Tgas,dt) bind(C)
    use krome_commons
    use krome_cooling
    use krome_heating
    use krome_subs
    use krome_tabs
    use krome_constants
    use krome_gadiab
    implicit none
    real(kind=c_double) :: x(nmols), Tgas
    real(kind=c_double), value :: dt
    real*8::n(nspec),nH2dust,dTgas,k(nrea),krome_gamma

  end subroutine krome_thermo

  !*************************
  !get heating (erg/cm3/s) for a given species
  ! array x(:) and Tgas
  function krome_get_heating(x,inTgas) bind(C)
    use krome_heating
    use krome_subs
    use krome_commons
    implicit none
    real(kind=c_double), value :: inTgas
    real(kind=c_double) :: x(nmols), krome_get_heating
    real*8::Tgas,k(nrea),nH2dust,n(nspec)
    n(1:nmols) = x(:)
    Tgas = inTgas
    n(idx_Tgas) = Tgas
    k(:) = coe(n(:))
    nH2dust = 0d0
    krome_get_heating = heating(n(:),Tgas,k(:),nH2dust)
  end function krome_get_heating

  !*****************************
  ! get an array containing individual heatings (erg/cm3/s)
  ! the array has size krome_nheats. see heatcool.gps
  ! for index list
  function krome_get_heating_array(x,inTgas)
    use krome_heating
    use krome_subs
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,k(nrea),nH2dust
    real*8 :: x(nmols),krome_get_heating_array(nheats)
    real*8,value :: inTgas

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = inTgas
    !#KROME_Tdust_copy
    k(:) = coe(n(:))
    Tgas = inTgas
    nH2dust = 0d0
    krome_get_heating_array(:) = get_heating_array(n(:),Tgas,k(:),nH2dust)

  end function krome_get_heating_array

  !************************
  !conserve the total amount of nucleii,
  ! alias for conserveLin_x in subs
  subroutine krome_conserveLin_x(x,ref) bind(C)
    use krome_commons
    use krome_subs
    implicit none
    real(kind=c_double) :: x(nmols),ref(natoms)

    call conserveLin_x(x(:),ref(:))

  end subroutine krome_conserveLin_x

  !************************
  !conserve the total amount of nucleii,
  ! alias for conserveLin_x in subs
  function krome_conserveLinGetRef_x(x)
    use krome_commons
    use krome_subs
    implicit none
    real*8 :: x(nmols),krome_conserveLinGetRef_x(natoms)

    krome_conserveLinGetRef_x(:) = &
        conserveLinGetRef_x(x(:))

  end function krome_conserveLinGetRef_x

  !*************************
  !force conservation to array x(:)
  !using xi(:) as initial abundances.
  !alias for conserve in krome_subs
  function krome_conserve(x,xi)
    use krome_subs
    implicit none
    real*8 :: x(krome_nmols),xi(krome_nmols),krome_conserve(krome_nmols)
    real*8::n(krome_nspec),ni(krome_nspec)

    n(:) = 0d0
    ni(:) = 0d0
    n(1:krome_nmols) = x(1:krome_nmols)
    ni(1:krome_nmols) = xi(1:krome_nmols)
    n(:) = conserve(n(:), ni(:))
    krome_conserve(:) = n(1:krome_nmols)

  end function krome_conserve

  !***************************
  !get the adiabatic index for x(:) species abundances
  ! and Tgas.
  ! alias for gamma_index in krome_subs
  function krome_get_gamma(x,Tgas) bind(C)
    use krome_subs
    use krome_commons
    use krome_gadiab
    real(kind=c_double), value :: Tgas
    real(kind=c_double) :: x(nmols), krome_get_gamma
    real*8::n(nspec)
    n(:) = 0.d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas
    krome_get_gamma = gamma_index(n(:))
  end function krome_get_gamma

  !***************************
  !get an integer array containing the atomic numbers Z
  ! of the spcecies.
  ! alias for get_zatoms
  function krome_get_zatoms()
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    integer :: krome_get_zatoms(nmols)
    integer::zatoms(nspec)

    zatoms(:) = get_zatoms()
    krome_get_zatoms(:) = zatoms(1:nmols)

  end function krome_get_zatoms

  !****************************
  !get the mean molecular weight from
  ! number density and mass density.
  ! alias for get_mu in krome_subs module
  function krome_get_mu(x) bind(C)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real(kind=c_double) :: x(nmols), krome_get_mu
    real*8::n(1:nspec)
    n(:) = 0d0
    n(1:nmols) = x(:)
    krome_get_mu = get_mu(n(:))
  end function krome_get_mu

  !***************************
  !get the names of the reactions as a
  ! character*50 array of krome_nrea
  ! elements
  !!bind(C) !! cannot yet be called from C
  function krome_get_rnames()
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    character*50 :: krome_get_rnames(nrea)

    krome_get_rnames(:) = get_rnames()

  end function krome_get_rnames

  !*****************
  !get an array of double containing the masses in g
  ! of the species.
  ! alias for get_mass in krome_subs
  function krome_get_mass()
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    real*8::tmp(nspec)
    real*8 :: krome_get_mass(nmols)
    tmp(:) = get_mass()
    krome_get_mass = tmp(1:nmols)
  end function krome_get_mass

  !*****************
  !get an array of double containing the inverse
  ! of the mass (1/g) of the species
  !alias for get_imass in krome_subs
  function krome_get_imass()
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    real*8::tmp(nspec)
    real*8 :: krome_get_imass(nmols)
    tmp(:) = get_imass()
    krome_get_imass = tmp(1:nmols)
  end function krome_get_imass

  !***********************
  !get the total number of H nuclei
  function krome_get_Hnuclei(x) bind(C)
    use krome_commons
    use krome_subs
    use krome_getphys
    real*8::n(nspec)
    real(kind=c_double) :: krome_get_Hnuclei, x(nmols)
    n(:) = 0d0
    n(1:nmols) = x(:)

    krome_get_Hnuclei = get_Hnuclei(n(:))

  end function krome_get_Hnuclei

  !*****************
  !get an array of size krome_nmols containing the
  ! charges of the species.
  ! alias for get_charges
  function krome_get_charges()
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    real*8::tmp(nspec)
    real*8 :: krome_get_charges(nmols)
    tmp(:) = get_charges()
    krome_get_charges = tmp(1:nmols)
  end function krome_get_charges

  !*****************
  !get an array of character*16 and size krome_nmols
  ! containing the names of all the species.
  ! alias for get_names
  !! bind(C) !! cannot yet be called from C
  function krome_get_names()
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    character*16 :: krome_get_names(nmols)
    character*16::tmp(nspec)
    tmp(:) = get_names()
    krome_get_names = tmp(1:nmols)
  end function krome_get_names

  !********************
  !get space-separated header of chemical species
  function krome_get_names_header()
    use krome_commons
    use krome_getphys
    implicit none
    character*37::krome_get_names_header
    character*16::tmp(nspec)
    integer::i

    tmp(:) = get_names()

    krome_get_names_header = ""
    do i=1,nmols
      krome_get_names_header = trim(krome_get_names_header)//" "//trim(tmp(i))
    end do

  end function krome_get_names_header

  !********************
  !get space-separated header of coolings
  function krome_get_cooling_names_header()
    use krome_commons
    use krome_getphys
    implicit none
    character*141::krome_get_cooling_names_header
    character*16::tmp(ncools)
    integer::i

    tmp(:) = get_cooling_names()

    krome_get_cooling_names_header = ""
    do i=1,ncools
      if(trim(tmp(i))=="") cycle
      krome_get_cooling_names_header = trim(krome_get_cooling_names_header)//" "//trim(tmp(i))
    end do

  end function krome_get_cooling_names_header

  !********************
  !get space-separated header of heatings
  function krome_get_heating_names_header()
    use krome_commons
    use krome_getphys
    implicit none
    character*87::krome_get_heating_names_header
    character*16::tmp(nheats)
    integer::i

    tmp(:) = get_heating_names()

    krome_get_heating_names_header = ""
    do i=1,nheats
      if(trim(tmp(i))=="") cycle
      krome_get_heating_names_header = trim(krome_get_heating_names_header)//" "//trim(tmp(i))
    end do

  end function krome_get_heating_names_header

  !*****************
  !get the index of the species with name name.
  ! alias for get_index
  !!bind(C) !! cannot yet be called from C
  function krome_get_index(name)
    use krome_subs
    implicit none
    integer(kind=c_int) :: krome_get_index
    character*(*) :: name
    krome_get_index = get_index(name)
  end function krome_get_index

  !*******************
  !get the total density of the gas in g/cm3
  ! giving all the number densities n(:)
  function krome_get_rho(n) bind(C)
    use krome_commons
    real(kind=c_double) :: krome_get_rho, n(nmols)
    real*8::m(nmols)
    m(:) = krome_get_mass()
    krome_get_rho = sum(m(:)*n(:))
  end function krome_get_rho

  !*************************
  !scale the abundances of the metals contained in n(:)
  ! to Z according to Asplund+2009.
  ! note that this applies only to neutral atoms.
  subroutine krome_scale_Z(x,Z) bind(C)
    use krome_commons
    use krome_getphys
    real(kind=c_double) :: x(nmols)
    real(kind=c_double), value :: Z
    real*8::Htot,n(nspec)

    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0

    Htot = get_Hnuclei(n(:))

  end subroutine krome_scale_Z

  !*************************
  !set the total metallicity
  ! in terms of Z/Z_solar
  subroutine krome_set_Z(xarg) bind(C)
    use krome_commons
    real(kind=c_double), value :: xarg

    total_Z = xarg

  end subroutine krome_set_Z

  !*************************
  !set D is in terms of D_solar (D/D_sol).
  subroutine krome_set_dust_to_gas(xarg) bind(C)
    use krome_commons
    real(kind=c_double), value :: xarg

    dust2gas_ratio = xarg

  end subroutine

  !*************************
  !set the clumping factor
  subroutine krome_set_clump(xarg) bind(C)
    use krome_commons
    real(kind=c_double), value :: xarg

    clump_factor = xarg

  end subroutine krome_set_clump

  !***********************
  !get the number of electrons assuming
  ! total neutral charge (cations-anions)
  function krome_get_electrons(x) bind(C)
    use krome_commons
    use krome_subs
    use krome_getphys
    real(kind=c_double) :: x(nmols), krome_get_electrons
    real*8::n(nspec)
    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0
    krome_get_electrons = get_electrons(n(:))
  end function krome_get_electrons

  !**********************
  !print on screen the first nbest highest reaction fluxes
  subroutine krome_print_best_flux(xin,Tgas,nbest) bind(C)
    use krome_subs
    use krome_commons
    implicit none
    real(kind=c_double) :: xin(nmols)
    real(kind=c_double), value :: Tgas
    real*8::x(nmols),n(nspec)
    integer(kind=c_int), value :: nbest
    n(1:nmols) = xin(:)
    n(idx_Tgas) = Tgas
    call print_best_flux(n,Tgas,nbest)

  end subroutine krome_print_best_flux

  !*********************
  !print only the highest fluxes greater than a fraction frac
  ! of the maximum flux
  subroutine krome_print_best_flux_frac(xin,Tgas,frac) bind(C)
    use krome_subs
    use krome_commons
    implicit none
    real(kind=c_double) :: xin(nmols)
    real(kind=c_double), value :: Tgas,frac
    real*8::n(nspec)
    n(1:nmols) = xin(:)
    n(idx_Tgas) = Tgas
    call print_best_flux_frac(n,Tgas,frac)

  end subroutine krome_print_best_flux_frac

  !**********************
  !print the highest nbest fluxes for reactions involving
  !a given species using the index idx_find (e.g. krome_idx_H2)
  subroutine krome_print_best_flux_spec(xin,Tgas,nbest,idx_find) bind(C)
    use krome_subs
    use krome_commons
    implicit none
    real(kind=c_double) :: xin(nmols)
    real(kind=c_double), value :: Tgas
    real*8::n(nspec)
    integer(kind=c_int), value :: nbest,idx_find
    n(1:nmols) = xin(:)
    n(idx_Tgas) = Tgas
    call print_best_flux_spec(n,Tgas,nbest,idx_find)
  end subroutine krome_print_best_flux_spec

  !*******************************
  !get an array of size krome_nrea with
  ! the fluxes of all the reactions in cm-3/s
  function krome_get_flux(n,Tgas)
    use krome_commons
    use krome_subs
    real*8 :: krome_get_flux(nrea),n(nmols)
    real*8,value :: Tgas
    real*8::x(nspec)
    x(:) = 0.d0
    x(1:nmols) = n(:)
    x(idx_Tgas) = Tgas
    krome_get_flux(:) = get_flux(x(:), Tgas)
  end function krome_get_flux

  !*****************************
  !store the fluxes to the file unit ifile
  ! using the chemical composition x(:), and the
  ! gas temperature Tgas. xvar is th value of an
  ! user-defined independent variable that
  ! can be employed for plots.
  ! the file columns are as follow
  ! rate number, xvar, absolute flux,
  !  flux/maxflux, flux fraction wrt total,
  !  reaction name (*50 string)
  subroutine krome_explore_flux(x,Tgas,ifile,xvar) bind(C)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real(kind=c_double) :: x(nmols)
    real(kind=c_double), value :: Tgas,xvar
    real*8::flux(nrea),fluxmax,sumflux,n(nspec)
    integer(kind=c_int), value :: ifile
    integer::i
    character*50::rname(nrea)

    !get reaction names
    rname(:) = get_rnames()
    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas
    !get fluxes
    flux(:) = get_flux(n(:), Tgas)
    fluxmax = maxval(flux) !maximum flux
    sumflux = sum(flux) !sum of all the fluxes
    !loop on reactions
    do i=1,nrea
      write(ifile,'(I8,5E17.8e3,a3,a50)') i,xvar,Tgas,flux(i),&
          flux(i)/fluxmax, flux(i)/sumflux," ",rname(i)
    end do
    write(ifile,*)

  end subroutine krome_explore_flux

  !*********************
  !get nulcear qeff for the reactions
  function krome_get_qeff()
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real*8 :: krome_get_qeff(nrea)

    krome_get_qeff(:) = get_qeff()

  end function krome_get_qeff

  !************************
  !dump the fluxes to the file unit nfile
  subroutine krome_dump_flux(n,Tgas,nfile) bind(C)
    use krome_commons
    real(kind=c_double) :: n(nmols)
    real(kind=c_double), value :: Tgas
    real*8::flux(nrea)
    integer(kind=c_int), value :: nfile
    integer::i

    flux(:) = krome_get_flux(n(:),Tgas)
    do i=1,nrea
      write(nfile,'(I8,E17.8e3)') i,flux(i)
    end do
    write(nfile,*)

  end subroutine krome_dump_flux

  !************************
  !dump all the evaluation of the coefficient rates in
  ! the file funit, in the range inTmin, inTmax, using
  ! imax points
  subroutine krome_dump_rates(inTmin,inTmax,imax,funit) bind(C)
    use krome_commons
    use krome_subs
    implicit none
    integer::i,j
    integer(kind=c_int), value :: funit,imax
    real(kind=c_double), value :: inTmin,inTmax
    real*8::Tmin,Tmax,Tgas,k(nrea),n(nspec)

    Tmin = log10(inTmin)
    Tmax = log10(inTmax)

    n(:) = 1d-40
    do i=1,imax
      Tgas = 1d1**((i-1)*(Tmax-Tmin)/(imax-1)+Tmin)
      n(idx_Tgas) = Tgas
      k(:) = coe(n(:))
      do j=1,nrea
        write(funit,'(E17.8e3,I8,E17.8e3)') Tgas,j,k(j)
      end do
      write(funit,*)
    end do

  end subroutine krome_dump_rates

  !************************
  !print species informations on screen
  subroutine krome_get_info(x, Tgas) bind(C)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    integer::i,charges(nspec)
    real(kind=c_double) :: x(nmols)
    real(kind=c_double), value :: Tgas
    real*8::masses(nspec)
    character*16::names(nspec)

    names(:) = get_names()
    charges(:) = get_charges()
    masses(:) = get_mass()

    print '(a4,a10,a11,a5,a11)',"#","Name","m (g)","Chrg","x"
    do i=1,size(x)
      print '(I4,a10,E11.3,I5,E11.3)',i," "//names(i),masses(i),charges(i),x(i)
    end do
    print '(a30,E11.3)'," sum",sum(x)

    print '(a14,E11.3)',"Tgas",Tgas
  end subroutine krome_get_info

  !*****************************
  subroutine krome_set_mpi_rank(xarg) bind(C)
    use krome_commons
    implicit none
    integer(kind=c_int), value :: xarg
    krome_mpi_rank=xarg
  end subroutine krome_set_mpi_rank

  !**************************
  function krome_get_jacobian(j,x,Tgas)
    use krome_ode
    use krome_commons
    implicit none
    integer, value :: j
    real*8,value :: Tgas
    real*8 :: x(nmols),krome_get_jacobian(nspec)
    integer::ian, jan, i
    real*8::tt, n(nspec)
    real*8::pdj(nspec)

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = tgas

    tt = 0d0
    ian = 0
    jan = 0

    call jes(nspec, tt, n, j, ian, jan, pdj)
    krome_get_jacobian(:) = pdj(:)

  end function krome_get_jacobian

  !********************************
  !subroutine wrapper around krome_get_coef function
  subroutine krome_get_coef_wrap(Tgas,x,krome_get_coef_var) bind(C,name='krome_get_coef')
    use krome_commons
    real(kind=c_double) :: krome_get_coef_var(nrea),x(nmols)
    real(kind=c_double),value:: Tgas

    krome_get_coef_var(:) = krome_get_coef(Tgas,x)

  end subroutine krome_get_coef_wrap

  !********************************
  !subroutine wrapper around krome_n2x function
  subroutine krome_n2x_wrap(n,rhogas,krome_n2x_var) bind(C,name='krome_n2x')
    use krome_commons
    implicit none
    real(kind=c_double) :: n(nmols),krome_n2x_var(nmols)
    real(kind=c_double),value :: rhogas

    krome_n2x_var(:) = krome_n2x(n,rhogas)

  end subroutine krome_n2x_wrap

  !********************************
  !subroutine wrapper around krome_x2n function
  subroutine krome_x2n_wrap(x,rhogas,krome_x2n_var) bind(C,name='krome_x2n')
    use krome_commons
    implicit none
    real(kind=c_double) :: x(nmols),krome_x2n_var(nmols)
    real(kind=c_double),value :: rhogas

    krome_x2n_var(:) = krome_x2n(x,rhogas)

  end subroutine krome_x2n_wrap

  !********************************
  !subroutine wrapper around krome_get_heating_array function
  subroutine krome_get_heating_array_wrap(x,inTgas,krome_get_heating_array_var) bind(C,name='krome_get_heating_array')
    use krome_commons
    implicit none
    real(kind=c_double) :: x(nmols),krome_get_heating_array_var(nheats)
    real(kind=c_double),value :: inTgas

    krome_get_heating_array_var(:) = krome_get_heating_array(x,inTgas)

  end subroutine krome_get_heating_array_wrap

  !********************************
  !subroutine wrapper around krome_conserveLinGetRef_x function
  subroutine krome_conserveLinGetRef_x_wrap(x,krome_conserveLinGetRef_x_var) bind(C,name='krome_conservelingetref_x')
    use krome_commons
    implicit none
    real(kind=c_double) :: x(nmols),krome_conserveLinGetRef_x_var(natoms)

    krome_conserveLinGetRef_x_var(:) = krome_conserveLinGetRef_x(x)

  end subroutine krome_conserveLinGetRef_x_wrap

  !********************************
  !subroutine wrapper around krome_conserve function
  subroutine krome_conserve_wrap(x,xi,krome_conserve_var) bind(C,name='krome_conserve')
    implicit none
    real(kind=c_double) :: x(krome_nmols),xi(krome_nmols),krome_conserve_var(krome_nmols)

    krome_conserve_var(:) = krome_conserve(x,xi)

  end subroutine krome_conserve_wrap

  !********************************
  !subroutine wrapper around krome_get_zatoms function
  subroutine krome_get_zatoms_wrap(krome_get_zatoms_var) bind(C,name='krome_get_zatoms')
    use krome_commons
    implicit none
    integer(kind=c_int) :: krome_get_zatoms_var(nmols)
    integer(kind=c_int)::zatoms(nspec)

    krome_get_zatoms_var(:) = krome_get_zatoms()

  end subroutine krome_get_zatoms_wrap

  !********************************
  !subroutine wrapper around krome_get_mass function
  subroutine krome_get_mass_wrap(krome_get_mass_var) bind(C,name='krome_get_mass')
    use krome_commons
    implicit none
    real(kind=c_double)::tmp(nspec)
    real(kind=c_double) :: krome_get_mass_var(nmols)

    krome_get_mass_var(:) = krome_get_mass()

  end subroutine krome_get_mass_wrap

  !********************************
  !subroutine wrapper around krome_get_imass function
  subroutine krome_get_imass_wrap(krome_get_imass_var) bind(C,name='krome_get_imass')
    use krome_commons
    implicit none
    real(kind=c_double)::tmp(nspec)
    real(kind=c_double) :: krome_get_imass_var(nmols)

    krome_get_imass_var(:) = krome_get_imass()

  end subroutine krome_get_imass_wrap

  !********************************
  !subroutine wrapper around krome_get_charges function
  subroutine krome_get_charges_wrap(krome_get_charges_var) bind(C,name='krome_get_charges')
    use krome_commons
    implicit none
    real(kind=c_double)::tmp(nspec)
    real(kind=c_double) :: krome_get_charges_var(nmols)

    krome_get_charges_var(:) = krome_get_charges()

  end subroutine krome_get_charges_wrap

  !********************************
  !subroutine wrapper around krome_get_flux function
  subroutine krome_get_flux_wrap(n,Tgas,krome_get_flux_var) bind(C,name='krome_get_flux')
    use krome_commons
    real(kind=c_double) :: krome_get_flux_var(nrea),n(nmols)
    real(kind=c_double),value :: Tgas

    krome_get_flux_var(:) = krome_get_flux(n,Tgas)

  end subroutine krome_get_flux_wrap

  !********************************
  !subroutine wrapper around krome_get_qeff function
  subroutine krome_get_qeff_wrap(krome_get_qeff_var) bind(C,name='krome_get_qeff')
    use krome_commons
    implicit none
    real(kind=c_double) :: krome_get_qeff_var(nrea)

    krome_get_qeff_var(:) = krome_get_qeff()

  end subroutine krome_get_qeff_wrap

  !********************************
  !subroutine wrapper around krome_get_jacobian function
  subroutine krome_get_jacobian_wrap(j,x,Tgas,krome_get_jacobian_var) bind(C,name='krome_get_jacobian')
    use krome_commons
    implicit none
    integer(kind=c_int), value :: j
    real(kind=c_double),value :: Tgas
    real(kind=c_double) :: x(nmols),krome_get_jacobian_var(nspec)
    real(kind=c_double)::pdj(nspec)

    krome_get_jacobian_var(:) = krome_get_jacobian(j,x,Tgas)

  end subroutine krome_get_jacobian_wrap

end module krome_user
