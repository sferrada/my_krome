!This module contains useful routines to get physical
! quantities, like mean molecular weight, mass density,
! mass, jeans length, etc. etc.

!############### MODULE ##############
module krome_getphys
contains

  !*****************************
  !get the mean molecular weight
  function get_mu(n)
    use krome_commons
    use krome_constants
    implicit none
    real*8::n(:),get_mu,m(nspec)
    m(:) = get_mass()

    !ip_mass is 1/proton_mass_in_g
    get_mu = max(sum(n(1:nmols)*m(1:nmols)),1d-40) &
        / max(sum(n(1:nmols)),1d-40) * ip_mass

  end function get_mu

  !***************************
  !get mean molecular weight
  function get_mu_rho(n,rhogas)
    use krome_commons
    use krome_constants
    implicit none
    real*8::get_mu_rho,rhogas,n(:)

    !ip_mass is 1/proton_mass_in_g
    get_mu_rho = rhogas / max(sum(n(1:nmols)),1d-40) * ip_mass

  end function get_mu_rho

  !************************
  !get species masses (g)
  function get_mass()
    use krome_commons
    implicit none
    real*8::get_mass(nspec)

    get_mass(1) = 9.10938188d-28	!E
    get_mass(2) = 1.6744434563759998d-24	!H-
    get_mass(3) = 1.673532518188d-24	!H
    get_mass(4) = 6.692065036376d-24	!HE
    get_mass(5) = 3.347065036376d-24	!H2
    get_mass(6) = 3.346032518188d-24	!D
    get_mass(7) = 5.0195650363760004d-24	!HD
    get_mass(8) = 1.67262158d-24	!H+
    get_mass(9) = 6.691154098188d-24	!HE+
    get_mass(10) = 3.346154098188d-24	!H2+
    get_mass(11) = 3.34512158d-24	!D+
    get_mass(12) = 6.690243159999999d-24	!HE++
    get_mass(13) = 0d0	!CR
    get_mass(14) = 0d0	!g
    get_mass(15) = 0d0	!Tgas
    get_mass(16) = 0d0	!dummy

  end function get_mass

  !************************
  !get sqrt of the inverse of the masses (1/sqrt(g))
  function get_imass_sqrt()
    use krome_commons
    implicit none
    real*8::get_imass_sqrt(nspec)

    get_imass_sqrt(1) = 33132602150543.92	!E
    get_imass_sqrt(2) = 772795806394.0071	!H-
    get_imass_sqrt(3) = 773006102110.9268	!H
    get_imass_sqrt(4) = 386562679981.0883	!HE
    get_imass_sqrt(5) = 546597856701.2171	!H2
    get_imass_sqrt(6) = 546682184736.54565	!D
    get_imass_sqrt(7) = 446341179966.5731	!HD
    get_imass_sqrt(8) = 773216569600.4055	!H+
    get_imass_sqrt(9) = 386588992536.287	!HE+
    get_imass_sqrt(10) = 546672253002.7066	!H2+
    get_imass_sqrt(11) = 546756615481.1794	!D+
    get_imass_sqrt(12) = 386615310465.34766	!HE++
    get_imass_sqrt(13) = 0d0	!CR
    get_imass_sqrt(14) = 0d0	!g
    get_imass_sqrt(15) = 0d0	!Tgas
    get_imass_sqrt(16) = 0d0	!dummy

  end function get_imass_sqrt

  !************************
  !get inverse of the species masses (1/g)
  function get_imass()
    use krome_commons
    implicit none
    real*8::get_imass(nspec)

    get_imass(1) = 1.0977693252662275d+27	!E
    get_imass(2) = 5.9721335838016375d+23	!H-
    get_imass(3) = 5.9753843390072855d+23	!H
    get_imass(4) = 1.4943070555416133d+23	!HE
    get_imass(5) = 2.9876921695036427d+23	!H2
    get_imass(6) = 2.9886141110832266d+23	!D
    get_imass(7) = 1.9922044893395282d+23	!HD
    get_imass(8) = 5.978638635046188d+23	!H+
    get_imass(9) = 1.494510491502214d+23	!HE+
    get_imass(10) = 2.988505522030552d+23	!H2+
    get_imass(11) = 2.9894279657243426d+23	!D+
    get_imass(12) = 1.4947139828621716d+23	!HE++
    get_imass(13) = 0d0	!CR
    get_imass(14) = 0d0	!g
    get_imass(15) = 0d0	!Tgas
    get_imass(16) = 0d0	!dummy

  end function get_imass

  !************************
  !species binding energies (surface=BARE), K
  function get_EbindBare()
    use krome_commons
    implicit none
    real*8::get_EbindBare(nspec)

    get_EbindBare(:) = 1d99

    get_EbindBare(idx_H) = 500.0d0
    get_EbindBare(idx_H2) = 300.0d0

  end function get_EbindBare

  !************************
  !species binding energies (surface=ICE), K
  function get_EbindIce()
    use krome_commons
    implicit none
    real*8::get_EbindIce(nspec)

    get_EbindIce(:) = 1d99

    get_EbindIce(idx_H) = 650.0d0
    get_EbindIce(idx_H2) = 300.0d0

  end function get_EbindIce

  !************************
  function get_kevap70()
    use krome_commons
    implicit none
    real*8::get_kevap70(nspec)

    get_kevap70(idx_E) = 0d0
    get_kevap70(idx_Hk) = 0d0
    get_kevap70(idx_H) = 790490323.1199661
    get_kevap70(idx_HE) = 0d0
    get_kevap70(idx_H2) = 13763786733.050402
    get_kevap70(idx_D) = 0d0
    get_kevap70(idx_HD) = 0d0
    get_kevap70(idx_Hj) = 0d0
    get_kevap70(idx_HEj) = 0d0
    get_kevap70(idx_H2j) = 0d0
    get_kevap70(idx_Dj) = 0d0
    get_kevap70(idx_HEjj) = 0d0
    get_kevap70(idx_CR) = 0d0
    get_kevap70(idx_g) = 0d0
    get_kevap70(idx_Tgas) = 0d0
    get_kevap70(idx_dummy) = 0d0

  end function get_kevap70

  !************************
  !get verbatim reaction names
  function get_rnames()
    use krome_commons
    implicit none
    character*50::get_rnames(nrea)

    !reaction names are loaded from file
    get_rnames(:) = reactionNames(:)

  end function get_rnames

  !************************
  !get species names
  function get_names()
    use krome_commons
    implicit none
    character*16::get_names(nspec)

    get_names(1) = "E"
    get_names(2) = "H-"
    get_names(3) = "H"
    get_names(4) = "HE"
    get_names(5) = "H2"
    get_names(6) = "D"
    get_names(7) = "HD"
    get_names(8) = "H+"
    get_names(9) = "HE+"
    get_names(10) = "H2+"
    get_names(11) = "D+"
    get_names(12) = "HE++"
    get_names(13) = "CR"
    get_names(14) = "g"
    get_names(15) = "Tgas"
    get_names(16) = "dummy"

  end function get_names

  !************************
  !get cooling names list (empty element if cooling not present)
  function get_cooling_names()
    use krome_commons
    implicit none
    character*16::get_cooling_names(ncools)

    get_cooling_names(:) = ""

    get_cooling_names(idx_cool_h2) = "H2"
    get_cooling_names(idx_cool_h2gp) = "H2GP"
    get_cooling_names(idx_cool_atomic) = "ATOMIC"
    get_cooling_names(idx_cool_cen) = "CEN"
    get_cooling_names(idx_cool_hd) = "HD"
    get_cooling_names(idx_cool_z) = "Z"
    get_cooling_names(idx_cool_metal) = "METAL"
    get_cooling_names(idx_cool_dh) = "DH"
    get_cooling_names(idx_cool_enthalpic) = "ENTHALPIC"
    get_cooling_names(idx_cool_dust) = "DUST"
    get_cooling_names(idx_cool_compton) = "COMPTON"
    get_cooling_names(idx_cool_cie) = "CIE"
    get_cooling_names(idx_cool_continuum) = "CONTINUUM"
    get_cooling_names(idx_cool_cont) = "CONT"
    get_cooling_names(idx_cool_exp) = "EXP"
    get_cooling_names(idx_cool_expansion) = "EXPANSION"
    get_cooling_names(idx_cool_ff) = "FF"
    get_cooling_names(idx_cool_bss) = "BSS"
    get_cooling_names(idx_cool_custom) = "CUSTOM"
    get_cooling_names(idx_cool_co) = "CO"
    get_cooling_names(idx_cool_zcie) = "ZCIE"
    get_cooling_names(idx_cool_zcienouv) = "ZCIENOUV"
    get_cooling_names(idx_cool_zextend) = "ZEXTEND"
    get_cooling_names(idx_cool_gh) = "GH"
    get_cooling_names(idx_cool_oh) = "OH"
    get_cooling_names(idx_cool_h2o) = "H2O"
    get_cooling_names(idx_cool_hcn) = "HCN"

  end function get_cooling_names

  !************************
  !get heating names list (empty element if heating not present)
  function get_heating_names()
    use krome_commons
    implicit none
    character*16::get_heating_names(nheats)

    get_heating_names(:) = ""

    get_heating_names(idx_heat_chem) = "CHEM"
    get_heating_names(idx_heat_compress) = "COMPRESS"
    get_heating_names(idx_heat_compr) = "COMPR"
    get_heating_names(idx_heat_photo) = "PHOTO"
    get_heating_names(idx_heat_dh) = "DH"
    get_heating_names(idx_heat_enthalpic) = "ENTHALPIC"
    get_heating_names(idx_heat_photoav) = "PHOTOAV"
    get_heating_names(idx_heat_av) = "AV"
    get_heating_names(idx_heat_cr) = "CR"
    get_heating_names(idx_heat_dust) = "DUST"
    get_heating_names(idx_heat_xray) = "XRAY"
    get_heating_names(idx_heat_visc) = "VISC"
    get_heating_names(idx_heat_viscous) = "VISCOUS"
    get_heating_names(idx_heat_custom) = "CUSTOM"
    get_heating_names(idx_heat_zcie) = "ZCIE"

  end function get_heating_names

  !******************************
  !get the total number of H nuclei
  function get_Hnuclei(n)
    use krome_commons
    real*8::n(:),get_Hnuclei,nH

    nH = n(idx_Hk) + &
        n(idx_H) + &
        n(idx_H2)*2d0 + &
        n(idx_HD) + &
        n(idx_Hj) + &
        n(idx_H2j)*2d0
    get_Hnuclei = nH

  end function get_Hnuclei

  !***************************
  function get_zatoms()
    use krome_commons
    implicit none
    integer::get_zatoms(nspec)

    get_zatoms(1) = 0	!E
    get_zatoms(2) = 1	!H-
    get_zatoms(3) = 1	!H
    get_zatoms(4) = 2	!HE
    get_zatoms(5) = 2	!H2
    get_zatoms(6) = 1	!D
    get_zatoms(7) = 2	!HD
    get_zatoms(8) = 1	!H+
    get_zatoms(9) = 2	!HE+
    get_zatoms(10) = 2	!H2+
    get_zatoms(11) = 1	!D+
    get_zatoms(12) = 2	!HE++
    get_zatoms(13) = 0	!CR
    get_zatoms(14) = 0	!g
    get_zatoms(15) = 0	!Tgas
    get_zatoms(16) = 0	!dummy

  end function get_zatoms

  !******************************
  function get_qeff()
    use krome_commons
    implicit none
    real*8::get_qeff(nrea)

    get_qeff(:) = 0e0

  end function get_qeff

  !**************************
  function get_free_fall_time(n)
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),m(nspec)
    real*8::rhogas,get_free_fall_time

    m(:) = get_mass()
    rhogas = sum(n(1:nmols)*m(1:nmols))
    get_free_fall_time = sqrt(3d0*pi/32d0/gravity/rhogas)

  end function get_free_fall_time

  !**************************
  function get_free_fall_time_rho(rhogas)
    use krome_constants
    implicit none
    real*8::rhogas,get_free_fall_time_rho

    get_free_fall_time_rho = sqrt(3d0*pi/32d0/gravity/rhogas)

  end function get_free_fall_time_rho

  !********************************
  function get_jeans_length(n,Tgas)
    !get jeans length in cm
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),Tgas,mu,rhogas
    real*8::m(nspec),get_jeans_length
    m(:) = get_mass()
    rhogas = max(sum(n(1:nmols)*m(1:nmols)),1d-40)
    mu = get_mu_rho(n(:),rhogas)
    get_jeans_length = sqrt(pi*boltzmann_erg*Tgas/rhogas&
        /p_mass/gravity/mu)

  end function get_jeans_length

  !********************************
  function get_jeans_length_rho(n,Tgas,rhogas)
    !get jeans length in cm
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),Tgas,mu,rhogas
    real*8::get_jeans_length_rho

    mu = get_mu_rho(n(:),rhogas)
    get_jeans_length_rho = sqrt(pi*boltzmann_erg*Tgas/rhogas&
        /p_mass/gravity/mu)

  end function get_jeans_length_rho

  !***************************
  !number density to column density conversion
  function num2col(ncalc,n)
    use krome_commons
    implicit none
    real*8::num2col,ncalc,n(:),Tgas
    Tgas = max(n(idx_Tgas),phys_Tcmb)

    num2col = 1.87d21*(max(ncalc,1d-40)*1d-3)**(2./3.)

  end function num2col

  !***********************
  !column density to number density conversion
  function col2num(ncalc,n)
    use krome_commons
    implicit none
    real*8::col2num,ncalc,n(:),Tgas
    Tgas = max(n(idx_Tgas),phys_Tcmb)

    col2num = 1d3 * (max(ncalc,1d-40)/1.87d21)**1.5

  end function col2num

  !************************
  !get electrons by balancing charges
  function get_electrons(n)
    use krome_commons
    implicit none
    real*8::get_electrons,n(nspec)

    get_electrons =  - n(idx_Hk) &
        + n(idx_Hj) &
        + n(idx_HEj) &
        + n(idx_H2j) &
        + n(idx_Dj) &
        + 2d0*n(idx_HEjj)
    get_electrons = max(get_electrons,0d0)

  end function get_electrons

  !************************
  !get species charges
  function get_charges()
    use krome_commons
    implicit none
    integer::get_charges(nspec)

    get_charges(1) = -1.d0 	!E
    get_charges(2) = -1.d0 	!H-
    get_charges(3) = 0.d0 	!H
    get_charges(4) = 0.d0 	!HE
    get_charges(5) = 0.d0 	!H2
    get_charges(6) = 0.d0 	!D
    get_charges(7) = 0.d0 	!HD
    get_charges(8) = 1.d0 	!H+
    get_charges(9) = 1.d0 	!HE+
    get_charges(10) = 1.d0 	!H2+
    get_charges(11) = 1.d0 	!D+
    get_charges(12) = 2.d0 	!HE++
    get_charges(13) = 0.d0 	!CR
    get_charges(14) = 0.d0 	!g
    get_charges(15) = 0.d0 	!Tgas
    get_charges(16) = 0.d0 	!dummy

  end function get_charges

end module krome_getphys
