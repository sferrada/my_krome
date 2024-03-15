
!############### MODULE ##############
module krome_subs
contains

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2024-03-14 13:09:11
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

  !************************
  !compute reaction rates cm^3(n-1)/s
  function coe(n)
    use krome_commons
    use krome_constants
    use krome_user_commons
    use krome_getphys
    use krome_grfuncs
    use krome_phfuncs
    use krome_fit
    implicit none
    real*8::coe(nrea),k(nrea),Tgas,n(nspec),kmax
    real*8::T32
    real*8::invT
    real*8::small,nmax
    integer::i
    real*8::nH  !preproc from coevar
    real*8::fact  !preproc from coevar
    real*8::sqrTgas !preproc from coevar
    real*8::imsqrt(nspec) !preproc from coevar
    real*8::ads_stick !preproc from coevar
    !Tgas is in K
    Tgas = max(n(idx_Tgas), phys_Tcmb)
    Tgas = min(Tgas,1d9)

    !maxn initialization can be removed and small can be
    ! replaced with a proper value according to the environment
    nmax = max(maxval(n(1:nmols)),1d0)
    small = 1d-40/(nmax*nmax*nmax*nmax)

    T32 = Tgas*0.0033333333333333335 !Tgas/(300 K) (#)
    invT = 1.d0/Tgas !inverse of T (1/K)

    nH = get_Hnuclei(n(:))
    fact = nH/user_xdust
    sqrTgas = sqrt(Tgas)
    imsqrt(:) = get_imass_sqrt()
    ads_stick = 1d0

    k(:) = small !inizialize coefficients

    !C -> C+ + E
    k(1) = small + (1.02e+03*user_crflux)

    !CL -> CL+ + E
    k(2) = small + (3.00e+03*user_crflux)

    !FE -> FE+ + E
    k(3) = small + (1.50e+03*user_crflux)

    !H -> H+ + E
    k(4) = small + (4.60e-01*user_crflux)

    !HE -> HE+ + E
    k(5) = small + (5.00e-01*user_crflux)

    !MG -> MG+ + E
    k(6) = small + (1.13e+02*user_crflux)

    !N -> N+ + E
    k(7) = small + (2.10e+00*user_crflux)

    !NA -> NA+ + E
    k(8) = small + (1.70e+01*user_crflux)

    !O -> O+ + E
    k(9) = small + (2.80e+00*user_crflux)

    !P -> P+ + E
    k(10) = small + (1.50e+03*user_crflux)

    !S -> S+ + E
    k(11) = small + (9.60e+02*user_crflux)

    !SI -> SI+ + E
    k(12) = small + (4.23e+03*user_crflux)

    !C2 -> C + C
    k(13) = small + (2.37e+02*user_crflux)

    !CCL -> C + CL
    k(14) = small + (5.00e+02*user_crflux)

    !CH -> C + H
    k(15) = small + (7.30e+02*user_crflux)

    !CLO -> CL + O
    k(16) = small + (5.00e+02*user_crflux)

    !CN -> C + N
    k(17) = small + (1.06e+04*user_crflux)

    !CO -> C + O
    k(18) = small + (5.00e+00*user_crflux)

    !CO -> CO+ + E
    k(19) = small + (3.00e+00*user_crflux)

    !CP -> C + P
    k(20) = small + (5.00e+02*user_crflux)

    !CS -> C + S
    k(21) = small + (5.00e+02*user_crflux)

    !H2 -> H + H
    k(22) = small + (1.00e-01*user_crflux)

    !H2 -> H+ + H + E
    k(23) = small + (2.20e-02*user_crflux)

    !H2 -> H+ + H-
    k(24) = small + (3.00e-04*user_crflux)

    !H2 -> H2+ + E
    k(25) = small + (9.30e-01*user_crflux)

    !HCL -> H + CL
    k(26) = small + (6.10e+02*user_crflux)

    !HS -> H + S
    k(27) = small + (5.00e+02*user_crflux)

    !MGH -> MG + H
    k(28) = small + (5.00e+02*user_crflux)

    !N2 -> N + N
    k(29) = small + (5.00e+00*user_crflux)

    !NAH -> NA + H
    k(30) = small + (5.00e+02*user_crflux)

    !NH -> N + H
    k(31) = small + (5.00e+02*user_crflux)

    !NO -> N + O
    k(32) = small + (4.82e+02*user_crflux)

    !NO -> NO+ + E
    k(33) = small + (4.94e+02*user_crflux)

    !NS -> N + S
    k(34) = small + (5.00e+02*user_crflux)

    !O2 -> O + O
    k(35) = small + (7.50e+02*user_crflux)

    !O2 -> O2+ + E
    k(36) = small + (1.17e+02*user_crflux)

    !OH -> O + H
    k(37) = small + (5.10e+02*user_crflux)

    !PH -> P + H
    k(38) = small + (5.00e+02*user_crflux)

    !PN -> P + N
    k(39) = small + (5.00e+02*user_crflux)

    !PO -> P + O
    k(40) = small + (5.00e+02*user_crflux)

    !S2 -> S + S
    k(41) = small + (5.00e+02*user_crflux)

    !SIC -> SI + C
    k(42) = small + (5.00e+02*user_crflux)

    !SIH -> SI + H
    k(43) = small + (5.00e+02*user_crflux)

    !SIN -> SI + N
    k(44) = small + (5.00e+02*user_crflux)

    !SIO -> SI + O
    k(45) = small + (5.00e+02*user_crflux)

    !SIS -> SI + S
    k(46) = small + (5.00e+02*user_crflux)

    !SO -> S + O
    k(47) = small + (5.00e+02*user_crflux)

    !C2H -> C2 + H
    k(48) = small + (5.00e+03*user_crflux)

    !C2N -> C + CN
    k(49) = small + (1.00e+03*user_crflux)

    !C2S -> CS + C
    k(50) = small + (1.50e+03*user_crflux)

    !C3 -> C2 + C
    k(51) = small + (1.12e+03*user_crflux)

    !CCO -> C2 + O
    k(52) = small + (7.50e+02*user_crflux)

    !CCO -> CO + C
    k(53) = small + (7.50e+02*user_crflux)

    !CCP -> C2 + P
    k(54) = small + (7.50e+02*user_crflux)

    !CCP -> CP + C
    k(55) = small + (7.50e+02*user_crflux)

    !CH2 -> CH2+ + E
    k(56) = small + (5.00e+02*user_crflux)

    !CO2 -> CO + O
    k(57) = small + (1.71e+03*user_crflux)

    !H2O -> OH + H
    k(58) = small + (9.70e+02*user_crflux)

    !H2S -> H2 + S
    k(59) = small + (5.15e+03*user_crflux)

    !H2S -> H2S+ + E
    k(60) = small + (1.70e+03*user_crflux)

    !HCN -> CN + H
    k(61) = small + (3.12e+03*user_crflux)

    !HCO -> CO + H
    k(62) = small + (4.21e+02*user_crflux)

    !HCO -> HCO+ + E
    k(63) = small + (1.17e+03*user_crflux)

    !HCP -> CP + H
    k(64) = small + (1.50e+03*user_crflux)

    !HCS -> HCS+ + E
    k(65) = small + (1.50e+03*user_crflux)

    !HCSI -> CH + SI
    k(66) = small + (1.50e+03*user_crflux)

    !HNC -> CN + H
    k(67) = small + (3.00e+03*user_crflux)

    !HNO -> HNO+ + E
    k(68) = small + (1.00e+03*user_crflux)

    !HNSI -> SIN + H
    k(69) = small + (1.50e+03*user_crflux)

    !HPO -> PO + H
    k(70) = small + (1.50e+03*user_crflux)

    !HS2 -> HS + S
    k(71) = small + (1.50e+03*user_crflux)

    !N2O -> NO + N
    k(72) = small + (1.50e+03*user_crflux)

    !NAOH -> NA + OH
    k(73) = small + (1.50e+03*user_crflux)

    !NH2 -> NH + H
    k(74) = small + (8.00e+01*user_crflux)

    !NH2 -> NH2+ + E
    k(75) = small + (6.50e+02*user_crflux)

    !NO2 -> NO + O
    k(76) = small + (1.50e+03*user_crflux)

    !O2H -> O + OH
    k(77) = small + (7.50e+02*user_crflux)

    !O2H -> O2 + H
    k(78) = small + (7.50e+02*user_crflux)

    !OCN -> CN + O
    k(79) = small + (1.50e+03*user_crflux)

    !OCS -> CO + S
    k(80) = small + (5.35e+03*user_crflux)

    !OCS -> OCS+ + E
    k(81) = small + (1.44e+03*user_crflux)

    !PH2 -> PH + H
    k(82) = small + (1.50e+03*user_crflux)

    !SIC2 -> SIC + C
    k(83) = small + (1.50e+03*user_crflux)

    !SIH2 -> SIH + H
    k(84) = small + (1.50e+03*user_crflux)

    !SINC -> SI + CN
    k(85) = small + (1.50e+03*user_crflux)

    !SIO2 -> SIO + O
    k(86) = small + (1.50e+03*user_crflux)

    !SO2 -> SO + O
    k(87) = small + (1.88e+03*user_crflux)

    !C2H2 -> C2H + H
    k(88) = small + (5.15e+03*user_crflux)

    !C2H2 -> C2H2+ + E
    k(89) = small + (1.31e+03*user_crflux)

    !C3H -> C3 + H
    k(90) = small + (5.00e+03*user_crflux)

    !C3N -> C2 + CN
    k(91) = small + (1.75e+03*user_crflux)

    !C3O -> C2 + CO
    k(92) = small + (6.60e+03*user_crflux)

    !C3P -> CCP + C
    k(93) = small + (1.50e+03*user_crflux)

    !C3S -> C2 + CS
    k(94) = small + (1.50e+03*user_crflux)

    !C4 -> C3 + C
    k(95) = small + (1.00e+03*user_crflux)

    !CH3 -> CH2 + H
    k(96) = small + (5.00e+02*user_crflux)

    !CH3 -> CH3+ + E
    k(97) = small + (5.00e+02*user_crflux)

    !H2CO -> CO + H2
    k(98) = small + (2.66e+03*user_crflux)

    !H2CS -> H2 + CS
    k(99) = small + (1.50e+03*user_crflux)

    !H2O2 -> OH + OH
    k(100) = small + (1.50e+03*user_crflux)

    !H2S2 -> HS + HS
    k(101) = small + (1.50e+03*user_crflux)

    !H2SIO -> SIO + H2
    k(102) = small + (1.50e+03*user_crflux)

    !HCCP -> CCP + H
    k(103) = small + (1.50e+03*user_crflux)

    !NH3 -> NH + H2
    k(104) = small + (5.40e+02*user_crflux)

    !NH3 -> NH2 + H
    k(105) = small + (1.32e+03*user_crflux)

    !NH3 -> NH3+ + E
    k(106) = small + (5.75e+02*user_crflux)

    !SIC2H -> SIC2 + H
    k(107) = small + (1.50e+03*user_crflux)

    !SIC3 -> SIC2 + C
    k(108) = small + (1.50e+03*user_crflux)

    !SICH2 -> SIC + H2
    k(109) = small + (1.50e+03*user_crflux)

    !SIH3 -> SIH2 + H
    k(110) = small + (1.50e+03*user_crflux)

    !C2H2N -> CH2 + CN
    k(111) = small + (5.00e+03*user_crflux)

    !C2H2O -> CH2 + CO
    k(112) = small + (9.15e+02*user_crflux)

    !C2H2O -> C2H2O+ + E
    k(113) = small + (1.22e+03&
        *user_crflux)

    !C2H3 -> C2H2 + H
    k(114) = small + (1.50e+03*user_crflux)

    !C3H2 -> C3H + H
    k(115) = small + (5.00e+03*user_crflux)

    !C4H -> C4 + H
    k(116) = small + (5.00e+03*user_crflux)

    !C4N -> C3 + CN
    k(117) = small + (1.00e+03*user_crflux)

    !C4P -> C3P + C
    k(118) = small + (1.50e+03*user_crflux)

    !C4S -> C3 + CS
    k(119) = small + (1.50e+03*user_crflux)

    !C5 -> C4 + C
    k(120) = small + (1.00e+03*user_crflux)

    !CH2O2 -> HCO + OH
    k(121) = small + (2.49e+02*user_crflux)

    !CH2O2 -> CH2O2+ + E
    k(122) = small + (6.50e+02&
        *user_crflux)

    !CH2PH -> HCP + H2
    k(123) = small + (1.50e+03*user_crflux)

    !CH3N -> HCN + H2
    k(124) = small + (4.98e+03*user_crflux)

    !CH4 -> CH2 + H2
    k(125) = small + (2.34e+03*user_crflux)

    !HC3N -> C2H + CN
    k(126) = small + (1.72e+03*user_crflux)

    !SIC2H2 -> SIC2 + H2
    k(127) = small + (1.50e+03&
        *user_crflux)

    !SIC3H -> SIC3 + H
    k(128) = small + (1.50e+03*user_crflux)

    !SIC4 -> SIC2 + C2
    k(129) = small + (1.50e+03*user_crflux)

    !SICH3 -> SICH2 + H
    k(130) = small + (1.50e+03*user_crflux)

    !SIH4 -> SIH2 + H2
    k(131) = small + (1.50e+03*user_crflux)

    !C2H3N -> CH3 + CN
    k(132) = small + (4.76e+03*user_crflux)

    !C2H3N -> C2H3N+ + E
    k(133) = small + (2.24e+03&
        *user_crflux)

    !C2H4 -> C2H2 + H2
    k(134) = small + (3.70e+03*user_crflux)

    !C2H4 -> C2H4+ + E
    k(135) = small + (7.80e+02*user_crflux)

    !C3H3 -> C3H2 + H
    k(136) = small + (2.50e+03*user_crflux)

    !C4H2 -> C2H + C2H
    k(137) = small + (1.73e+03*user_crflux)

    !C4H2 -> C4H + H
    k(138) = small + (1.73e+03*user_crflux)

    !C4H2 -> C4H2+ + E
    k(139) = small + (1.12e+03*user_crflux)

    !C5H -> C5 + H
    k(140) = small + (5.00e+03*user_crflux)

    !C5N -> C4 + CN
    k(141) = small + (1.75e+03*user_crflux)

    !C6 -> C5 + C
    k(142) = small + (1.00e+03*user_crflux)

    !CH4O -> CH3 + OH
    k(143) = small + (1.50e+03*user_crflux)

    !CH4O -> H2CO + H2
    k(144) = small + (3.17e+03*user_crflux)

    !CH4O -> H3CO+ + H + E
    k(145) = small + (9.90e+01&
        *user_crflux)

    !CH4O -> CH4O+ + E
    k(146) = small + (1.44e+03*user_crflux)

    !C2H4O -> CH3 + HCO
    k(147) = small + (5.25e+02*user_crflux)

    !C2H4O -> CH4 + CO
    k(148) = small + (5.25e+02*user_crflux)

    !C2H4O -> C2H4O+ + E
    k(149) = small + (1.12e+03&
        *user_crflux)

    !C2H5 -> C2H4 + H
    k(150) = small + (1.50e+03*user_crflux)

    !C3H3N -> C2H3 + CN
    k(151) = small + (1.50e+03*user_crflux)

    !C3H4 -> C3H3 + H
    k(152) = small + (3.28e+03*user_crflux)

    !C3H4 -> C3H4+ + E
    k(153) = small + (5.30e+03*user_crflux)

    !C5H2 -> C5H + H
    k(154) = small + (1.75e+03*user_crflux)

    !C6H -> C6 + H
    k(155) = small + (5.00e+03*user_crflux)

    !C7 -> C6 + C
    k(156) = small + (1.00e+03*user_crflux)

    !CH5N -> HCN + H2 + H + H
    k(157) = small + (1.41e+03&
        *user_crflux)

    !CH5N -> CH5N+ + E
    k(158) = small + (1.12e+03*user_crflux)

    !HC5N -> C4H + CN
    k(159) = small + (1.75e+03*user_crflux)

    !C6H2 -> C6H + H
    k(160) = small + (1.75e+03*user_crflux)

    !C7H -> C7 + H
    k(161) = small + (5.00e+03*user_crflux)

    !C7N -> C6 + CN
    k(162) = small + (1.75e+03*user_crflux)

    !C8 -> C7 + C
    k(163) = small + (1.00e+03*user_crflux)

    !CH3C3N -> CH3 + C3N
    k(164) = small + (1.50e+03&
        *user_crflux)

    !HCOOCH3 -> H2CO + H2CO
    k(165) = small + (1.50e+03&
        *user_crflux)

    !C2H5OH -> C2H5OH+ + E
    k(166) = small + (2.74e+03&
        *user_crflux)

    !C7H2 -> C7H + H
    k(167) = small + (1.75e+03*user_crflux)

    !C8H -> C8 + H
    k(168) = small + (5.00e+03*user_crflux)

    !C9 -> C8 + C
    k(169) = small + (1.00e+03*user_crflux)

    !CH3C4H -> CH3 + C4H
    k(170) = small + (1.50e+03&
        *user_crflux)

    !CH3OCH3 -> H2CO + CH4
    k(171) = small + (1.72e+03&
        *user_crflux)

    !CH3OCH3 -> CH3OCH3+ + E
    k(172) = small + (1.12e+03&
        *user_crflux)

    !HC7N -> C6H + CN
    k(173) = small + (1.75e+03*user_crflux)

    !C2H6CO -> C2H2O + CH4
    k(174) = small + (1.50e+03&
        *user_crflux)

    !C8H2 -> C8H + H
    k(175) = small + (1.75e+03*user_crflux)

    !C9H -> C9 + H
    k(176) = small + (5.00e+03*user_crflux)

    !C9N -> C8 + CN
    k(177) = small + (1.75e+03*user_crflux)

    !C10 -> C9 + C
    k(178) = small + (1.00e+03*user_crflux)

    !CH3C5N -> CH3 + C5N
    k(179) = small + (1.50e+03&
        *user_crflux)

    !C9H2 -> C9H + H
    k(180) = small + (1.75e+03*user_crflux)

    !CH3C6H -> CH3 + C6H
    k(181) = small + (1.50e+03&
        *user_crflux)

    !CH3C7N -> CH3 + C7N
    k(182) = small + (1.50e+03&
        *user_crflux)

    !HC9N -> C8H + CN
    k(183) = small + (1.75e+03*user_crflux)

    !C4H4 -> C3H4 + C
    k(184) = small + (2.00e+03*user_crflux)

    !CH+ -> C + H+
    k(185) = small + (1.76e+02*user_crflux)

    !C+ + FE -> FE+ + C
    k(186) = small + (2.60e-09)

    !C+ + MG -> MG+ + C
    k(187) = small + (1.10e-09)

    !C+ + NA -> NA+ + C
    k(188) = small + (1.10e-09)

    !C+ + P -> P+ + C
    k(189) = small + (1.00e-09)

    !C+ + S -> S+ + C
    k(190) = small + (1.50e-09)

    !C+ + SI -> SI+ + C
    k(191) = small + (2.10e-09)

    !C+ + CCL -> CCL+ + C
    k(192) = small + (1.93e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH -> C2+ + H
    k(193) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH -> CH+ + C
    k(194) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + CLO -> CLO+ + C
    k(195) = small + (3.65e-09&
        *(T32)**(-5.00e-01))

    !C+ + CP -> CP+ + C
    k(196) = small + (2.58e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCL -> CCL+ + H
    k(197) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !C+ + HS -> CS+ + H
    k(198) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + MGH -> MG+ + CH
    k(199) = small + (4.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + NAH -> NA+ + CH
    k(200) = small + (2.20e-08&
        *(T32)**(-5.00e-01))

    !C+ + NH -> CN+ + H
    k(201) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + NO -> NO+ + C
    k(202) = small + (4.80e-10&
        *(T32)**(-5.00e-01))

    !C+ + NS -> CS+ + N
    k(203) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + NS -> NS+ + C
    k(204) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + O2 -> O+ + CO
    k(205) = small + (4.10e-10)

    !C+ + O2 -> CO+ + O
    k(206) = small + (7.50e-10)

    !C+ + OH -> CO+ + H
    k(207) = small + (2.90e-09&
        *(T32)**(-3.33e-01))

    !C+ + PH -> PH+ + C
    k(208) = small + (1.99e-09&
        *(T32)**(-5.00e-01))

    !C+ + PO -> PO+ + C
    k(209) = small + (5.58e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIC -> SI+ + C2
    k(210) = small + (2.56e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIC -> SIC+ + C
    k(211) = small + (2.56e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIH -> SIC+ + H
    k(212) = small + (3.78e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIN -> SIC+ + N
    k(213) = small + (3.46e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIN -> SIN+ + C
    k(214) = small + (3.46e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIO -> SI+ + CO
    k(215) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIS -> SIC+ + S
    k(216) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIS -> SIS+ + C
    k(217) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !C+ + SO -> S+ + CO
    k(218) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + SO -> CO+ + S
    k(219) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + SO -> CS+ + O
    k(220) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + SO -> SO+ + C
    k(221) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H -> C3+ + H
    k(222) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2S -> C2S+ + C
    k(223) = small + (1.72e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2S -> C3+ + S
    k(224) = small + (1.72e-09&
        *(T32)**(-5.00e-01))

    !C+ + CCO -> C2O+ + C
    k(225) = small + (3.93e-09&
        *(T32)**(-5.00e-01))

    !C+ + CCP -> CP+ + C2
    k(226) = small + (1.46e-09&
        *(T32)**(-5.00e-01))

    !C+ + CCP -> CCP+ + C
    k(227) = small + (1.46e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH2 -> C2H+ + H
    k(228) = small + (4.34e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH2 -> CH2+ + C
    k(229) = small + (4.34e-10&
        *(T32)**(-5.00e-01))

    !C+ + CO2 -> CO+ + CO
    k(230) = small + (1.10e-09)

    !C+ + H2O -> HCO+ + H
    k(231) = small + (8.90e-10&
        *(T32)**(-5.00e-01))

    !C+ + H2O -> HOC+ + H
    k(232) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + H2S -> H2S+ + C
    k(233) = small + (3.20e-10&
        *(T32)**(-5.00e-01))

    !C+ + H2S -> HCS+ + H
    k(234) = small + (9.50e-10&
        *(T32)**(-5.00e-01))

    !C+ + HCN -> C2N+ + H
    k(235) = small + (4.75e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCN -> CNC+ + H
    k(236) = small + (4.75e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCO -> CH+ + CO
    k(237) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + HCO -> HCO+ + C
    k(238) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + HCP -> CCP+ + H
    k(239) = small + (5.85e-10&
        *(T32)**(-5.00e-01))

    !C+ + HCP -> HCP+ + C
    k(240) = small + (5.85e-10&
        *(T32)**(-5.00e-01))

    !C+ + HCSI -> SIC2+ + H
    k(241) = small + (3.01e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNC -> C2N+ + H
    k(242) = small + (8.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNSI -> SINC+ + H
    k(243) = small + (4.79e-10&
        *(T32)**(-5.00e-01))

    !C+ + HPO -> HPO+ + C
    k(244) = small + (6.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + NAOH -> HCO+ + NA
    k(245) = small + (6.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2 -> HCN+ + H
    k(246) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + OCN -> CO+ + CN
    k(247) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + OCS -> CS+ + CO
    k(248) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + OCS -> OCS+ + C
    k(249) = small + (4.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIC2 -> SIC2+ + C
    k(250) = small + (2.97e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIH2 -> SIC+ + H2
    k(251) = small + (7.93e-11&
        *(T32)**(-5.00e-01))

    !C+ + SIH2 -> CHSI+ + H
    k(252) = small + (7.93e-11&
        *(T32)**(-5.00e-01))

    !C+ + SIH2 -> SIH2+ + C
    k(253) = small + (7.93e-11&
        *(T32)**(-5.00e-01))

    !C+ + SO2 -> SO+ + CO
    k(254) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H2 -> C3H+ + H
    k(255) = small + (2.70e-09)

    !C+ + C3H -> C4+ + H
    k(256) = small + (8.29e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3O -> C3+ + CO
    k(257) = small + (2.34e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3O -> C3O+ + C
    k(258) = small + (2.34e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3O -> C4+ + O
    k(259) = small + (2.34e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3P -> C4+ + P
    k(260) = small + (1.22e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3S -> C3S+ + C
    k(261) = small + (2.36e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3S -> C4+ + S
    k(262) = small + (2.36e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH3 -> C2H+ + H2
    k(263) = small + (1.00e-09)

    !C+ + CH3 -> C2H2+ + H
    k(264) = small + (1.00e-09)

    !C+ + CH3 -> CH3+ + C
    k(265) = small + (1.00e-09)

    !C+ + H2CO -> CH2+ + CO
    k(266) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !C+ + H2CO -> HCO+ + CH
    k(267) = small + (6.50e-10&
        *(T32)**(-5.00e-01))

    !C+ + H2CO -> H2CO+ + C
    k(268) = small + (9.60e-10&
        *(T32)**(-5.00e-01))

    !C+ + H2CS -> CH2+ + CS
    k(269) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + H2SIO -> H2SIO+ + C
    k(270) = small + (2.51e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCCP -> CP+ + C2H
    k(271) = small + (5.00e-10)

    !C+ + HCCP -> CCP+ + CH
    k(272) = small + (5.00e-10)

    !C+ + NH3 -> HCN+ + H2
    k(273) = small + (1.08e-10&
        *(T32)**(-5.00e-01))

    !C+ + NH3 -> HCNH+ + H
    k(274) = small + (1.36e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH3 -> NH3+ + C
    k(275) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIC2H -> SIC2H+ + C
    k(276) = small + (8.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIC2H -> SIC3+ + H
    k(277) = small + (8.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIC3 -> SIC3+ + C
    k(278) = small + (2.44e-09&
        *(T32)**(-5.00e-01))

    !C+ + SICH2 -> SIC2+ + H2
    k(279) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SICH2 -> CH2SI+ + C
    k(280) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SICH2 -> SIC2H+ + H
    k(281) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIH3 -> CHSI+ + H2
    k(282) = small + (1.00e-09)

    !C+ + SIH3 -> CH2SI+ + H
    k(283) = small + (1.00e-09)

    !C+ + SIH3 -> SIH3+ + C
    k(284) = small + (1.00e-09)

    !C+ + C2H2N -> C2H2N+ + C
    k(285) = small + (2.04e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H2O -> C2H2O+ + C
    k(286) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H3 -> C3H+ + H2
    k(287) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H3 -> C2H3+ + C
    k(288) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H3 -> C3H2+ + H
    k(289) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H3 -> H2C3+ + H
    k(290) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H2 -> C4+ + H2
    k(291) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3H2 -> C4H+ + H
    k(292) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + C4H -> C5+ + H
    k(293) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + C4P -> C5+ + P
    k(294) = small + (6.01e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4S -> C4S+ + C
    k(295) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + C4S -> C5+ + S
    k(296) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH2PH -> PC2H+ + H2
    k(297) = small + (6.28e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3N -> C2H2N+ + H
    k(298) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH4 -> C2H2+ + H2
    k(299) = small + (5.00e-10)

    !C+ + CH4 -> C2H3+ + H
    k(300) = small + (1.00e-09)

    !C+ + HCNC2 -> C3N+ + CH
    k(301) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCNC2 -> C2H+ + C2N
    k(302) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCNC2 -> HCN+ + C3
    k(303) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCNC2 -> C4N+ + H
    k(304) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCNC2 -> C2N+ + C2H
    k(305) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC2NC -> C3H+ + CN
    k(306) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC2NC -> C2H+ + C2N
    k(307) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC2NC -> CNC+ + C2H
    k(308) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC2NC -> C4N+ + H
    k(309) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC2NC -> C3HN+ + C
    k(310) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC3N -> C3+ + HCN
    k(311) = small + (5.50e-10&
        *(T32)**(-5.00e-01))

    !C+ + HC3N -> C3H+ + CN
    k(312) = small + (7.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC3N -> C4N+ + H
    k(313) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNC3 -> C3+ + HNC
    k(314) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNC3 -> HNC+ + C3
    k(315) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNC3 -> C3N+ + CH
    k(316) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !C+ + HNC3 -> C4N+ + H
    k(317) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIC2H2 -> SIC3H+ + H
    k(318) = small + (3.09e-09&
        *(T32)**(-5.00e-01))

    !C+ + SIC3H -> SIC4+ + H
    k(319) = small + (3.65e-09&
        *(T32)**(-5.00e-01))

    !C+ + SICH3 -> SIC2H+ + H2
    k(320) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SICH3 -> SIC2H2+ + H
    k(321) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SICH3 -> SICH3+ + C
    k(322) = small + (6.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + SIH4 -> CH2SI+ + H2
    k(323) = small + (1.00e-09)

    !C+ + SIH4 -> SICH3+ + H
    k(324) = small + (1.00e-09)

    !C+ + C2H3N -> C2H3+ + CN
    k(325) = small + (2.49e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H3N -> HC2NCH+ + H
    k(326) = small + (2.49e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H4 -> C3H+ + H2 + H
    k(327) = small + (8.50e-11)

    !C+ + C2H4 -> C2H3+ + CH
    k(328) = small + (8.50e-11)

    !C+ + C2H4 -> C3H2+ + H2
    k(329) = small + (1.70e-10)

    !C+ + C2H4 -> H2C3+ + H2
    k(330) = small + (1.70e-10)

    !C+ + C2H4 -> C2H4+ + C
    k(331) = small + (1.70e-10)

    !C+ + C2H4 -> C3H3+ + H
    k(332) = small + (1.00e-09)

    !C+ + C3H3 -> C4H+ + H2
    k(333) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3H3 -> C3H3+ + C
    k(334) = small + (8.50e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H3 -> H3C3+ + C
    k(335) = small + (8.50e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H3 -> C4H2+ + H
    k(336) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + C4H2 -> C3H+ + C2H
    k(337) = small + (1.45e-10)

    !C+ + C4H2 -> C5+ + H2
    k(338) = small + (2.00e-09)

    !C+ + C4H2 -> C4H2+ + C
    k(339) = small + (1.40e-09)

    !C+ + C4H2 -> C5H+ + H
    k(340) = small + (1.50e-09)

    !C+ + C5H -> C6+ + H
    k(341) = small + (1.25e-08&
        *(T32)**(-5.00e-01))

    !C+ + CH4O -> CH3+ + HCO
    k(342) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH4O -> H3CO+ + CH
    k(343) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH4O -> CH4O+ + C
    k(344) = small + (8.10e-10&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> C2H3O+ + N
    k(345) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> H3CO+ + CN
    k(346) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> H2CO+ + HCN
    k(347) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> HCN+ + H2CO
    k(348) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> C2H2N+ + OH
    k(349) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + NH2CHO -> C2H3N+ + O
    k(350) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H4O -> C2H3O+ + CH
    k(351) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H4O -> C2H4O+ + C
    k(352) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H5 -> C3H3+ + H2
    k(353) = small + (3.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H5 -> H3C3+ + H2
    k(354) = small + (3.33e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H5 -> C2H5+ + C
    k(355) = small + (6.65e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H3N -> C3H3N+ + C
    k(356) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> C2H2+ + C2H2
    k(357) = small + (1.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> C2H3+ + C2H
    k(358) = small + (1.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> C3H3+ + CH
    k(359) = small + (1.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> H3C3+ + CH
    k(360) = small + (1.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> C4H2+ + H2
    k(361) = small + (3.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C3H4 -> C3H4+ + C
    k(362) = small + (3.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4H3 -> C5H2+ + H
    k(363) = small + (3.10e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4H3 -> C5H+ + H2
    k(364) = small + (3.10e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4H3 -> C4H3+ + C
    k(365) = small + (3.10e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4H3 -> C3H2+ + C2H
    k(366) = small + (1.55e-10&
        *(T32)**(-5.00e-01))

    !C+ + C4H3 -> H2C3+ + C2H
    k(367) = small + (1.55e-10&
        *(T32)**(-5.00e-01))

    !C+ + C5H2 -> C6+ + H2
    k(368) = small + (1.53e-09&
        *(T32)**(-5.00e-01))

    !C+ + C5H2 -> C6H+ + H
    k(369) = small + (1.53e-09&
        *(T32)**(-5.00e-01))

    !C+ + C6H -> C7+ + H
    k(370) = small + (1.43e-08&
        *(T32)**(-5.00e-01))

    !C+ + CH5N -> CH4N+ + CH
    k(371) = small + (6.20e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH5N -> CH5N+ + C
    k(372) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC5N -> C5H+ + CN
    k(373) = small + (6.20e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC5N -> C5HN+ + C
    k(374) = small + (6.20e-09&
        *(T32)**(-5.00e-01))

    !C+ + C6H2 -> C7+ + H2
    k(375) = small + (1.20e-09)

    !C+ + C6H2 -> C7H+ + H
    k(376) = small + (1.20e-09)

    !C+ + C7H -> C8+ + H
    k(377) = small + (1.27e-08&
        *(T32)**(-5.00e-01))

    !C+ + CH3C3N -> C2H3+ + C3N
    k(378) = small + (2.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH3C3N -> C4H3+ + CN
    k(379) = small + (2.90e-09&
        *(T32)**(-5.00e-01))

    !C+ + HCOOCH3 -> COOCH4+ + C
    k(380) = small + (2.17e-09&
        *(T32)**(-5.00e-01))

    !C+ + C2H5OH -> H3CO+ + C2H3
    k(381) = small + (7.07e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H5OH -> C2H5O+ + CH
    k(382) = small + (7.07e-10&
        *(T32)**(-5.00e-01))

    !C+ + C2H5OH -> C2H5OH+ + C
    k(383) = small + (7.07e-10&
        *(T32)**(-5.00e-01))

    !C+ + C7H2 -> C8+ + H2
    k(384) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !C+ + C7H2 -> C8H+ + H
    k(385) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !C+ + C8H -> C9+ + H
    k(386) = small + (1.40e-08&
        *(T32)**(-5.00e-01))

    !C+ + CH3C4H -> C5H3+ + CH
    k(387) = small + (4.93e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3C4H -> C6H2+ + H2
    k(388) = small + (4.93e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3C4H -> C6H3+ + H
    k(389) = small + (4.93e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3OCH3 -> CH3OCH3+ + C
    k(390) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC7N -> C7H+ + CN
    k(391) = small + (1.30e-08&
        *(T32)**(-5.00e-01))

    !C+ + C2H6CO -> C2H6CO+ + C
    k(392) = small + (3.44e-09&
        *(T32)**(-5.00e-01))

    !C+ + C8H2 -> C9+ + H2
    k(393) = small + (1.20e-09)

    !C+ + C8H2 -> C9H+ + H
    k(394) = small + (1.20e-09)

    !C+ + C9H -> C10+ + H
    k(395) = small + (1.31e-08&
        *(T32)**(-5.00e-01))

    !C+ + CH3C5N -> C6H3+ + CN
    k(396) = small + (6.24e-09&
        *(T32)**(-5.00e-01))

    !C+ + C9H2 -> C10+ + H2
    k(397) = small + (2.95e-09&
        *(T32)**(-5.00e-01))

    !C+ + CH3C6H -> C7H3+ + CH
    k(398) = small + (9.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3C6H -> C8H2+ + H2
    k(399) = small + (9.00e-10&
        *(T32)**(-5.00e-01))

    !C+ + CH3C7N -> C8H3+ + CN
    k(400) = small + (6.44e-09&
        *(T32)**(-5.00e-01))

    !C+ + HC9N -> C9H+ + CN
    k(401) = small + (1.34e-08&
        *(T32)**(-5.00e-01))

    !CL+ + H2 -> HCL+ + H
    k(402) = small + (1.00e-09)

    !CL+ + O2 -> O2+ + CL
    k(403) = small + (4.60e-10)

    !FE+ + NA -> NA+ + FE
    k(404) = small + (1.00e-11)

    !H+ + FE -> FE+ + H
    k(405) = small + (7.40e-09)

    !H+ + MG -> MG+ + H
    k(406) = small + (1.10e-09)

    !H+ + NA -> NA+ + H
    k(407) = small + (1.20e-09)

    !H+ + O -> O+ + H
    k(408) = small + (7.00e-10*exp(-2.32e+02&
        *invT))

    !H+ + P -> P+ + H
    k(409) = small + (1.00e-09)

    !H+ + S -> S+ + H
    k(410) = small + (1.30e-09)

    !H+ + SI -> SI+ + H
    k(411) = small + (9.90e-10)

    !H+ + C2 -> C2+ + H
    k(412) = small + (3.10e-09)

    !H+ + CH -> CH+ + H
    k(413) = small + (1.40e-08&
        *(T32)**(-5.00e-01))

    !H+ + CP -> CP+ + H
    k(414) = small + (7.96e-09&
        *(T32)**(-5.00e-01))

    !H+ + CS -> CS+ + H
    k(415) = small + (1.80e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCL -> HCL+ + H
    k(416) = small + (1.00e-08&
        *(T32)**(-5.00e-01))

    !H+ + HS -> S+ + H2
    k(417) = small + (3.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + HS -> HS+ + H
    k(418) = small + (3.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + MGH -> MG+ + H2
    k(419) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !H+ + NAH -> NA+ + H2
    k(420) = small + (6.30e-08&
        *(T32)**(-5.00e-01))

    !H+ + NH -> NH+ + H
    k(421) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !H+ + NO -> NO+ + H
    k(422) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !H+ + NS -> NS+ + H
    k(423) = small + (1.70e-08&
        *(T32)**(-5.00e-01))

    !H+ + O2 -> O2+ + H
    k(424) = small + (1.20e-09)

    !H+ + OH -> OH+ + H
    k(425) = small + (1.60e-08&
        *(T32)**(-5.00e-01))

    !H+ + PH -> PH+ + H
    k(426) = small + (5.94e-09&
        *(T32)**(-5.00e-01))

    !H+ + PN -> PN+ + H
    k(427) = small + (2.54e-08&
        *(T32)**(-5.00e-01))

    !H+ + PO -> PO+ + H
    k(428) = small + (1.74e-08&
        *(T32)**(-5.00e-01))

    !H+ + S2 -> S2+ + H
    k(429) = small + (3.00e-09)

    !H+ + SIC -> SIC+ + H
    k(430) = small + (1.57e-08&
        *(T32)**(-5.00e-01))

    !H+ + SIH -> SI+ + H2
    k(431) = small + (5.60e-10&
        *(T32)**(-5.00e-01))

    !H+ + SIH -> SIH+ + H
    k(432) = small + (5.60e-10&
        *(T32)**(-5.00e-01))

    !H+ + SIN -> SIN+ + H
    k(433) = small + (2.13e-08&
        *(T32)**(-5.00e-01))

    !H+ + SIO -> SIO+ + H
    k(434) = small + (2.90e-08&
        *(T32)**(-5.00e-01))

    !H+ + SIS -> SIS+ + H
    k(435) = small + (1.60e-08&
        *(T32)**(-5.00e-01))

    !H+ + SO -> SO+ + H
    k(436) = small + (1.40e-08&
        *(T32)**(-5.00e-01))

    !H+ + C2H -> C2+ + H2
    k(437) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H -> C2H+ + H
    k(438) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2N -> C2N+ + H
    k(439) = small + (5.56e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2S -> C2S+ + H
    k(440) = small + (1.09e-08&
        *(T32)**(-5.00e-01))

    !H+ + C3 -> C3+ + H
    k(441) = small + (4.00e-09)

    !H+ + CCO -> C2O+ + H
    k(442) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !H+ + CCP -> CCP+ + H
    k(443) = small + (9.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH2 -> CH+ + H2
    k(444) = small + (1.14e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH2 -> CH2+ + H
    k(445) = small + (1.14e-09&
        *(T32)**(-5.00e-01))

    !H+ + CO2 -> HCO+ + O
    k(446) = small + (3.00e-09)

    !H+ + H2O -> H2O+ + H
    k(447) = small + (7.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2S -> H2S+ + H
    k(448) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCN -> HCN+ + H
    k(449) = small + (2.78e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCO -> CO+ + H2
    k(450) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCO -> H2+ + CO
    k(451) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCO -> HCO+ + H
    k(452) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCP -> HCP+ + H
    k(453) = small + (3.61e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCS -> CS+ + H2
    k(454) = small + (1.85e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCSI -> SIC+ + H2
    k(455) = small + (4.63e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCSI -> CHSI+ + H
    k(456) = small + (4.63e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC -> H+ + HCN
    k(457) = small + (2.51e-08&
        *(T32)**(-5.00e-01))

    !H+ + HNO -> NO+ + H2
    k(458) = small + (6.70e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNSI -> SIN+ + H2
    k(459) = small + (7.40e-10&
        *(T32)**(-5.00e-01))

    !H+ + HNSI -> HNSI+ + H
    k(460) = small + (7.40e-10&
        *(T32)**(-5.00e-01))

    !H+ + HPO -> HPO+ + H
    k(461) = small + (2.15e-08&
        *(T32)**(-5.00e-01))

    !H+ + HS2 -> S2H+ + H
    k(462) = small + (7.79e-09&
        *(T32)**(-5.00e-01))

    !H+ + NAOH -> NA+ + H2O
    k(463) = small + (2.00e-08&
        *(T32)**(-5.00e-01))

    !H+ + NH2 -> NH2+ + H
    k(464) = small + (7.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + OCS -> HS+ + CO
    k(465) = small + (6.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + PH2 -> PH2+ + H
    k(466) = small + (1.96e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC2 -> SIC2+ + H
    k(467) = small + (9.32e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIH2 -> SIH+ + H2
    k(468) = small + (3.54e-10&
        *(T32)**(-5.00e-01))

    !H+ + SIH2 -> SIH2+ + H
    k(469) = small + (3.54e-10&
        *(T32)**(-5.00e-01))

    !H+ + SINC -> SINC+ + H
    k(470) = small + (7.92e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H2 -> C2H+ + H2
    k(471) = small + (2.00e-09)

    !H+ + C2H2 -> C2H2+ + H
    k(472) = small + (2.00e-09)

    !H+ + C3H -> C3+ + H2
    k(473) = small + (1.22e-08&
        *(T32)**(-5.00e-01))

    !H+ + C3H -> C3H+ + H
    k(474) = small + (1.22e-08&
        *(T32)**(-5.00e-01))

    !H+ + C3O -> C3O+ + H
    k(475) = small + (2.21e-08&
        *(T32)**(-5.00e-01))

    !H+ + C3S -> C3S+ + H
    k(476) = small + (1.52e-08&
        *(T32)**(-5.00e-01))

    !H+ + C4 -> C4+ + H
    k(477) = small + (4.00e-09)

    !H+ + CH3 -> CH3+ + H
    k(478) = small + (3.40e-09)

    !H+ + H2CO -> HCO+ + H2
    k(479) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2CO -> H2CO+ + H
    k(480) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2CS -> H2CS+ + H
    k(481) = small + (6.40e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2S2 -> H2S2+ + H
    k(482) = small + (4.67e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2SIO -> HSIO+ + H2
    k(483) = small + (3.91e-09&
        *(T32)**(-5.00e-01))

    !H+ + H2SIO -> H2SIO+ + H
    k(484) = small + (3.91e-09&
        *(T32)**(-5.00e-01))

    !H+ + HCCP -> PC2H+ + H
    k(485) = small + (1.00e-09)

    !H+ + NH3 -> NH3+ + H
    k(486) = small + (5.80e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC2H -> SIC2+ + H2
    k(487) = small + (2.73e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC2H -> SIC2H+ + H
    k(488) = small + (2.73e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC3 -> SIC3+ + H
    k(489) = small + (7.79e-09&
        *(T32)**(-5.00e-01))

    !H+ + SICH2 -> CHSI+ + H2
    k(490) = small + (2.93e-09&
        *(T32)**(-5.00e-01))

    !H+ + SICH2 -> CH2SI+ + H
    k(491) = small + (2.93e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIH3 -> SIH2+ + H2
    k(492) = small + (1.50e-09)

    !H+ + SIH3 -> SIH3+ + H
    k(493) = small + (1.50e-09)

    !H+ + C2H2N -> C2H2N+ + H
    k(494) = small + (6.26e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H2O -> C2H2O+ + H
    k(495) = small + (5.60e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H3 -> C2H2+ + H2
    k(496) = small + (3.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H3 -> C2H3+ + H
    k(497) = small + (3.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H2 -> C3H+ + H2
    k(498) = small + (5.89e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H2 -> C3H2+ + H
    k(499) = small + (5.89e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4H -> C4+ + H2
    k(500) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4H -> C4H+ + H
    k(501) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4P -> C4P+ + H
    k(502) = small + (1.94e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4S -> C4S+ + H
    k(503) = small + (1.17e-08&
        *(T32)**(-5.00e-01))

    !H+ + C5 -> C5+ + H
    k(504) = small + (4.00e-09)

    !H+ + CH2O2 -> HCO2+ + H2
    k(505) = small + (2.80e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH2O2 -> CH2O2+ + H
    k(506) = small + (2.80e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH2PH -> PCH3+ + H
    k(507) = small + (1.95e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3N -> HCNH+ + H2
    k(508) = small + (7.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH4 -> CH3+ + H2
    k(509) = small + (2.30e-09)

    !H+ + CH4 -> CH4+ + H
    k(510) = small + (1.50e-09)

    !H+ + HCNC2 -> HCN+ + C2H
    k(511) = small + (1.90e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCNC2 -> C2H+ + HCN
    k(512) = small + (1.90e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCNC2 -> C3N+ + H2
    k(513) = small + (1.90e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCNC2 -> C2NH+ + CH
    k(514) = small + (1.90e-08&
        *(T32)**(-5.00e-01))

    !H+ + HC2NC -> C2H2N+ + C
    k(515) = small + (7.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HC2NC -> C2H2+ + CN
    k(516) = small + (7.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HC2NC -> C3N+ + H2
    k(517) = small + (7.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HC3N -> C3HN+ + H
    k(518) = small + (3.40e-08&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> H+ + HC3N
    k(519) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> C3HN+ + H
    k(520) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> C3H+ + NH
    k(521) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> C3N+ + H2
    k(522) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> HNC+ + C2H
    k(523) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + HNC3 -> C2H+ + HNC
    k(524) = small + (3.23e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CN -> HNC+ + NH2
    k(525) = small + (8.35e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CN -> NH2+ + HNC
    k(526) = small + (8.35e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC2H2 -> SIC2H+ + H2
    k(527) = small + (4.87e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC2H2 -> SIC2H2+ + H
    k(528) = small + (4.87e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC3H -> SIC3+ + H2
    k(529) = small + (5.85e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC3H -> SIC3H+ + H
    k(530) = small + (5.85e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIC4 -> SIC4+ + H
    k(531) = small + (2.45e-08&
        *(T32)**(-5.00e-01))

    !H+ + SICH3 -> CH2SI+ + H2
    k(532) = small + (2.93e-09&
        *(T32)**(-5.00e-01))

    !H+ + SICH3 -> SICH3+ + H
    k(533) = small + (2.93e-09&
        *(T32)**(-5.00e-01))

    !H+ + SIH4 -> SIH3+ + H2
    k(534) = small + (1.50e-09)

    !H+ + SIH4 -> SIH4+ + H
    k(535) = small + (1.50e-09)

    !H+ + C2H3N -> C2H2N+ + H2
    k(536) = small + (7.70e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H3N -> C2H3N+ + H
    k(537) = small + (7.70e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H4 -> C2H3+ + H2
    k(538) = small + (2.00e-09)

    !H+ + C2H4 -> C2H4+ + H
    k(539) = small + (2.00e-09)

    !H+ + C3H3 -> C3H2+ + H2
    k(540) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H3 -> H2C3+ + H2
    k(541) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H3 -> C3H3+ + H
    k(542) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H3 -> H3C3+ + H
    k(543) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4H2 -> C4H+ + H2
    k(544) = small + (2.00e-09)

    !H+ + C4H2 -> C4H2+ + H
    k(545) = small + (2.00e-09)

    !H+ + C5H -> C5+ + H2
    k(546) = small + (1.98e-08&
        *(T32)**(-5.00e-01))

    !H+ + C5H -> C5H+ + H
    k(547) = small + (1.98e-08&
        *(T32)**(-5.00e-01))

    !H+ + C6 -> C6+ + H
    k(548) = small + (4.00e-09)

    !H+ + CH4O -> H3CO+ + H2
    k(549) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH4O -> CH4O+ + H
    k(550) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CHO -> NH4+ + CO
    k(551) = small + (8.62e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CHO -> HCO+ + NH3
    k(552) = small + (8.62e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CHO -> H2NC+ + H2O
    k(553) = small + (8.62e-09&
        *(T32)**(-5.00e-01))

    !H+ + NH2CHO -> NH2+ + H2CO
    k(554) = small + (8.62e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H4O -> C2H3O+ + H2
    k(555) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H4O -> C2H4O+ + H
    k(556) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H5 -> C2H4+ + H2
    k(557) = small + (1.97e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H5 -> C2H5+ + H
    k(558) = small + (1.97e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H3N -> C3H2N+ + H2
    k(559) = small + (7.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H3N -> C3H3N+ + H
    k(560) = small + (7.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + C3H4 -> C3H3+ + H2
    k(561) = small + (7.50e-10&
        *(T32)**(-5.00e-01))

    !H+ + C3H4 -> H3C3+ + H2
    k(562) = small + (7.50e-10&
        *(T32)**(-5.00e-01))

    !H+ + C3H4 -> C3H4+ + H
    k(563) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4H3 -> C4H3+ + H
    k(564) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C4H3 -> C4H2+ + H2
    k(565) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !H+ + C5H2 -> C5H+ + H2
    k(566) = small + (4.87e-09&
        *(T32)**(-5.00e-01))

    !H+ + C5H2 -> C5H2+ + H
    k(567) = small + (4.87e-09&
        *(T32)**(-5.00e-01))

    !H+ + C6H -> C6+ + H2
    k(568) = small + (2.30e-08&
        *(T32)**(-5.00e-01))

    !H+ + C6H -> C6H+ + H
    k(569) = small + (2.30e-08&
        *(T32)**(-5.00e-01))

    !H+ + C7 -> C7+ + H
    k(570) = small + (4.00e-09)

    !H+ + CH5N -> CH4N+ + H2
    k(571) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH5N -> CH5N+ + H
    k(572) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !H+ + HC5N -> C5HN+ + H
    k(573) = small + (4.00e-08&
        *(T32)**(-5.00e-01))

    !H+ + C6H2 -> C6H+ + H2
    k(574) = small + (2.00e-09)

    !H+ + C6H2 -> C6H2+ + H
    k(575) = small + (2.00e-09)

    !H+ + C7H -> C7+ + H2
    k(576) = small + (2.07e-08&
        *(T32)**(-5.00e-01))

    !H+ + C7H -> C7H+ + H
    k(577) = small + (2.07e-08&
        *(T32)**(-5.00e-01))

    !H+ + C8 -> C8+ + H
    k(578) = small + (4.00e-09)

    !H+ + CH3C3N -> CH3+ + HC3N
    k(579) = small + (1.85e-08&
        *(T32)**(-5.00e-01))

    !H+ + HCOOCH3 -> COOCH4+ + H
    k(580) = small + (6.90e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H5OH -> C2H5O+ + H2
    k(581) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + C2H5OH -> C2H5OH+ + H
    k(582) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !H+ + C7H2 -> C7H+ + H2
    k(583) = small + (4.86e-09&
        *(T32)**(-5.00e-01))

    !H+ + C7H2 -> C7H2+ + H
    k(584) = small + (4.86e-09&
        *(T32)**(-5.00e-01))

    !H+ + C8H -> C8+ + H2
    k(585) = small + (2.30e-08&
        *(T32)**(-5.00e-01))

    !H+ + C8H -> C8H+ + H
    k(586) = small + (2.30e-08&
        *(T32)**(-5.00e-01))

    !H+ + C9 -> C9+ + H
    k(587) = small + (4.00e-09)

    !H+ + CH3C4H -> C5H3+ + H2
    k(588) = small + (2.36e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3C4H -> C5H4+ + H
    k(589) = small + (2.36e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3OCH3 -> C2H5O+ + H2
    k(590) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3OCH3 -> CH3OCH3+ + H
    k(591) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !H+ + HC7N -> C7HN+ + H
    k(592) = small + (4.25e-08&
        *(T32)**(-5.00e-01))

    !H+ + C2H6CO -> C2H6CO+ + H
    k(593) = small + (1.09e-08&
        *(T32)**(-5.00e-01))

    !H+ + C8H2 -> C8H+ + H2
    k(594) = small + (2.00e-09)

    !H+ + C8H2 -> C8H2+ + H
    k(595) = small + (2.00e-09)

    !H+ + C9H -> C9+ + H2
    k(596) = small + (2.16e-08&
        *(T32)**(-5.00e-01))

    !H+ + C9H -> C9H+ + H
    k(597) = small + (2.16e-08&
        *(T32)**(-5.00e-01))

    !H+ + CH3C5N -> CH3+ + HC5N
    k(598) = small + (2.03e-08&
        *(T32)**(-5.00e-01))

    !H+ + C10 -> C10+ + H
    k(599) = small + (4.00e-09)

    !H+ + C9H2 -> C9H+ + H2
    k(600) = small + (4.85e-09&
        *(T32)**(-5.00e-01))

    !H+ + C9H2 -> C9H2+ + H
    k(601) = small + (4.85e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3C6H -> C7H3+ + H2
    k(602) = small + (2.92e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3C6H -> C7H4+ + H
    k(603) = small + (2.92e-09&
        *(T32)**(-5.00e-01))

    !H+ + CH3C7N -> CH3+ + HC7N
    k(604) = small + (2.12e-08&
        *(T32)**(-5.00e-01))

    !H+ + HC9N -> C9HN+ + H
    k(605) = small + (4.44e-08&
        *(T32)**(-5.00e-01))

    !HE+ + H -> H+ + HE
    k(606) = small + (1.90e-15)

    !HE+ + P -> P+ + HE
    k(607) = small + (1.00e-09)

    !HE+ + SI -> SI+ + HE
    k(608) = small + (3.30e-09)

    !HE+ + C2 -> C+ + C + HE
    k(609) = small + (1.60e-09)

    !HE+ + C2 -> C2+ + HE
    k(610) = small + (5.00e-10)

    !HE+ + CCL -> C+ + CL + HE
    k(611) = small + (3.11e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH -> C+ + H + HE
    k(612) = small + (3.83e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH -> CH+ + HE
    k(613) = small + (3.83e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CLO -> CL+ + O + HE
    k(614) = small + (5.91e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CN -> C+ + N + HE
    k(615) = small + (3.60e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CN -> N+ + C + HE
    k(616) = small + (3.60e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CO -> C+ + O + HE
    k(617) = small + (1.60e-09)

    !HE+ + CP -> C+ + P + HE
    k(618) = small + (2.06e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CP -> P+ + C + HE
    k(619) = small + (2.06e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CS -> C+ + S + HE
    k(620) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CS -> S+ + C + HE
    k(621) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2 -> H+ + H + HE
    k(622) = small + (3.30e-15)

    !HE+ + H2 -> H2+ + HE
    k(623) = small + (9.60e-15)

    !HE+ + HCL -> CL+ + H + HE
    k(624) = small + (5.22e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HS -> S+ + H + HE
    k(625) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + MGH -> MG+ + H + HE
    k(626) = small + (6.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + N2 -> N+ + N + HE
    k(627) = small + (8.00e-10)

    !HE+ + N2 -> N2+ + HE
    k(628) = small + (4.00e-10)

    !HE+ + NAH -> NA+ + H + HE
    k(629) = small + (3.30e-08&
        *(T32)**(-5.00e-01))

    !HE+ + NH -> N+ + H + HE
    k(630) = small + (6.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NO -> N+ + O + HE
    k(631) = small + (6.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + NO -> O+ + N + HE
    k(632) = small + (1.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + NS -> N+ + S + HE
    k(633) = small + (4.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NS -> S+ + N + HE
    k(634) = small + (4.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + O2 -> O+ + O + HE
    k(635) = small + (1.00e-09)

    !HE+ + O2 -> O2+ + HE
    k(636) = small + (3.30e-11)

    !HE+ + OH -> O+ + H + HE
    k(637) = small + (8.50e-09&
        *(T32)**(-5.00e-01))

    !HE+ + PH -> P+ + H + HE
    k(638) = small + (3.12e-09&
        *(T32)**(-5.00e-01))

    !HE+ + PN -> P+ + N + HE
    k(639) = small + (1.32e-08&
        *(T32)**(-5.00e-01))

    !HE+ + PO -> P+ + O + HE
    k(640) = small + (8.99e-09&
        *(T32)**(-5.00e-01))

    !HE+ + S2 -> S+ + S + HE
    k(641) = small + (2.00e-09)

    !HE+ + SIC -> C+ + SI + HE
    k(642) = small + (4.09e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC -> SI+ + C + HE
    k(643) = small + (4.09e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIH -> SI+ + H + HE
    k(644) = small + (5.87e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SIN -> SI+ + N + HE
    k(645) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !HE+ + SIO -> O+ + SI + HE
    k(646) = small + (7.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIO -> SI+ + O + HE
    k(647) = small + (7.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIS -> S+ + SI + HE
    k(648) = small + (4.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIS -> SI+ + S + HE
    k(649) = small + (4.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SO -> O+ + S + HE
    k(650) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SO -> S+ + O + HE
    k(651) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H -> C+ + CH + HE
    k(652) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H -> C2+ + H + HE
    k(653) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H -> CH+ + C + HE
    k(654) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2N -> C+ + CN + HE
    k(655) = small + (2.89e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2S -> C+ + CS + HE
    k(656) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2S -> S+ + C2 + HE
    k(657) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2S -> C2+ + S + HE
    k(658) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2S -> CS+ + C + HE
    k(659) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3 -> C+ + C2 + HE
    k(660) = small + (1.00e-09)

    !HE+ + C3 -> C2+ + C + HE
    k(661) = small + (1.00e-09)

    !HE+ + CCO -> C+ + CO + HE
    k(662) = small + (6.26e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CCP -> C+ + CP + HE
    k(663) = small + (2.38e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CCP -> P+ + C2 + HE
    k(664) = small + (2.38e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH2 -> C+ + H2 + HE
    k(665) = small + (6.25e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH2 -> CH+ + H + HE
    k(666) = small + (6.25e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CO2 -> C+ + O2 + HE
    k(667) = small + (4.00e-11)

    !HE+ + CO2 -> O+ + CO + HE
    k(668) = small + (2.00e-10)

    !HE+ + CO2 -> CO+ + O + HE
    k(669) = small + (8.00e-10)

    !HE+ + CO2 -> O2+ + C + HE
    k(670) = small + (1.10e-11)

    !HE+ + CO2 -> CO2+ + HE
    k(671) = small + (1.21e-10)

    !HE+ + H2O -> H+ + OH + HE
    k(672) = small + (1.32e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2O -> OH+ + H + HE
    k(673) = small + (1.32e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2O -> H2O+ + HE
    k(674) = small + (1.32e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2S -> S+ + H2 + HE
    k(675) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2S -> HS+ + H + HE
    k(676) = small + (2.20e-10&
        *(T32)**(-5.00e-01))

    !HE+ + H2S -> H2S+ + HE
    k(677) = small + (1.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCN -> C+ + N + H + HE
    k(678) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCN -> N+ + CH + HE
    k(679) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCN -> CH+ + N + HE
    k(680) = small + (3.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCN -> CN+ + H + HE
    k(681) = small + (6.90e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCO -> CH+ + O + HE
    k(682) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCO -> CO+ + H + HE
    k(683) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCO -> HEH+ + CO
    k(684) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCP -> C+ + PH + HE
    k(685) = small + (9.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCP -> P+ + CH + HE
    k(686) = small + (9.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HCS -> H+ + CS + HE
    k(687) = small + (4.79e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCS -> CS+ + H + HE
    k(688) = small + (4.79e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCSI -> SI+ + CH + HE
    k(689) = small + (2.41e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCSI -> SIC+ + H + HE
    k(690) = small + (2.41e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC -> C+ + N + H + HE
    k(691) = small + (4.43e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC -> CN+ + H + HE
    k(692) = small + (4.43e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC -> NH+ + C + HE
    k(693) = small + (4.43e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNO -> H+ + NO + HE
    k(694) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNO -> NO+ + H + HE
    k(695) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNSI -> SIN+ + H + HE
    k(696) = small + (7.68e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HPO -> PH+ + O + HE
    k(697) = small + (5.55e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HPO -> PO+ + H + HE
    k(698) = small + (5.55e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HS2 -> S+ + HS + HE
    k(699) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HS2 -> S2+ + H + HE
    k(700) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + N2O -> N+ + NO + HE
    k(701) = small + (8.45e-11&
        *(T32)**(-5.00e-01))

    !HE+ + N2O -> O+ + N2 + HE
    k(702) = small + (8.45e-11&
        *(T32)**(-5.00e-01))

    !HE+ + N2O -> N2+ + O + HE
    k(703) = small + (8.45e-11&
        *(T32)**(-5.00e-01))

    !HE+ + N2O -> NO+ + N + HE
    k(704) = small + (8.45e-11&
        *(T32)**(-5.00e-01))

    !HE+ + NAOH -> NA+ + OH + HE
    k(705) = small + (1.00e-08&
        *(T32)**(-5.00e-01))

    !HE+ + NH2 -> N+ + H2 + HE
    k(706) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2 -> NH+ + H + HE
    k(707) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + OCN -> O+ + CN + HE
    k(708) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !HE+ + OCN -> CN+ + O + HE
    k(709) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !HE+ + OCS -> O+ + CS + HE
    k(710) = small + (8.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + OCS -> S+ + CO + HE
    k(711) = small + (8.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + OCS -> CO+ + S + HE
    k(712) = small + (8.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + OCS -> CS+ + O + HE
    k(713) = small + (8.40e-10&
        *(T32)**(-5.00e-01))

    !HE+ + PH2 -> P+ + H2 + HE
    k(714) = small + (1.03e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC2 -> SI+ + C2 + HE
    k(715) = small + (4.81e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIH2 -> SI+ + H2 + HE
    k(716) = small + (1.86e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SIH2 -> SIH+ + H + HE
    k(717) = small + (1.86e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SINC -> CN+ + SI + HE
    k(718) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIO2 -> SI+ + O2 + HE
    k(719) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SO2 -> S+ + O2 + HE
    k(720) = small + (8.15e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SO2 -> O2+ + S + HE
    k(721) = small + (8.15e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SO2 -> SO+ + O + HE
    k(722) = small + (8.15e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SO2 -> SO2+ + HE
    k(723) = small + (8.15e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H2 -> C2+ + H2 + HE
    k(724) = small + (1.60e-09)

    !HE+ + C2H2 -> CH+ + CH + HE
    k(725) = small + (7.70e-10)

    !HE+ + C2H2 -> C2H+ + H + HE
    k(726) = small + (8.80e-10)

    !HE+ + C2H2 -> C2H2+ + HE
    k(727) = small + (2.50e-10)

    !HE+ + C3H -> C3+ + H + HE
    k(728) = small + (1.31e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C3N -> C2+ + CN + HE
    k(729) = small + (5.25e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3N -> C3N+ + HE
    k(730) = small + (5.25e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3O -> C2+ + CO + HE
    k(731) = small + (5.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3O -> C3+ + O + HE
    k(732) = small + (5.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3P -> P+ + C3 + HE
    k(733) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3P -> C3+ + P + HE
    k(734) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3S -> C2+ + CS + HE
    k(735) = small + (3.89e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3S -> CS+ + C2 + HE
    k(736) = small + (3.89e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4 -> C+ + C3 + HE
    k(737) = small + (6.70e-10)

    !HE+ + C4 -> C2+ + C2 + HE
    k(738) = small + (6.70e-10)

    !HE+ + C4 -> C3+ + C + HE
    k(739) = small + (6.70e-10)

    !HE+ + CH3 -> CH+ + H2 + HE
    k(740) = small + (1.00e-09)

    !HE+ + CH3 -> CH2+ + H + HE
    k(741) = small + (1.00e-09)

    !HE+ + H2CO -> CO+ + H2 + HE
    k(742) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2CO -> HCO+ + H + HE
    k(743) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2CS -> S+ + CH2 + HE
    k(744) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2CS -> CS+ + H2 + HE
    k(745) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2CS -> CH2+ + S + HE
    k(746) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2S2 -> HS+ + HS + HE
    k(747) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2S2 -> S2H+ + H + HE
    k(748) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !HE+ + H2SIO -> HSIO+ + H + HE
    k(749) = small + (4.04e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCCP -> CH+ + CP + HE
    k(750) = small + (5.00e-10)

    !HE+ + HCCP -> CP+ + CH + HE
    k(751) = small + (5.00e-10)

    !HE+ + NH3 -> NH+ + H2 + HE
    k(752) = small + (2.50e-10&
        *(T32)**(-5.00e-01))

    !HE+ + NH3 -> NH2+ + H + HE
    k(753) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH3 -> NH3+ + HE
    k(754) = small + (3.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + SIC2H -> SI+ + C2H + HE
    k(755) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC2H -> SIC2+ + H + HE
    k(756) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC3 -> SIC2+ + C + HE
    k(757) = small + (4.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SICH2 -> SIH+ + CH + HE
    k(758) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SICH2 -> CHSI+ + H + HE
    k(759) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIH3 -> SIH+ + H2 + HE
    k(760) = small + (1.00e-09)

    !HE+ + SIH3 -> SIH2+ + H + HE
    k(761) = small + (1.00e-09)

    !HE+ + C2H2N -> CH2+ + CN + HE
    k(762) = small + (3.25e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H2O -> CO+ + CH2 + HE
    k(763) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H2O -> CH2+ + CO + HE
    k(764) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H3 -> C2H+ + H2 + HE
    k(765) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H3 -> C2H2+ + H + HE
    k(766) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H2 -> C3+ + H2 + HE
    k(767) = small + (3.08e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H2 -> C3H+ + H + HE
    k(768) = small + (3.08e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4H -> C2H+ + C2 + HE
    k(769) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4H -> C4+ + H + HE
    k(770) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4P -> C2+ + CCP + HE
    k(771) = small + (4.97e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4P -> CCP+ + C2 + HE
    k(772) = small + (4.97e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4S -> CS+ + C3 + HE
    k(773) = small + (2.98e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4S -> C3+ + CS + HE
    k(774) = small + (2.98e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C5 -> C3+ + C2 + HE
    k(775) = small + (1.00e-09)

    !HE+ + C5 -> C4+ + C + HE
    k(776) = small + (1.00e-09)

    !HE+ + CH2O2 -> CO2+ + H + H + HE
    k(777) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH2O2 -> HCO2+ + H + HE
    k(778) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH2PH -> PH+ + CH2 + HE
    k(779) = small + (5.05e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH2PH -> CH2+ + PH + HE
    k(780) = small + (5.05e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH3N -> NH+ + CH2 + HE
    k(781) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3N -> CH2+ + NH + HE
    k(782) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3N -> HCN+ + H2 + HE
    k(783) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3N -> HCNH+ + H + HE
    k(784) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH4 -> H+ + CH3 + HE
    k(785) = small + (4.80e-10)

    !HE+ + CH4 -> CH+ + H2 + H + HE
    k(786) = small + (2.40e-10)

    !HE+ + CH4 -> CH2+ + H2 + HE
    k(787) = small + (9.50e-10)

    !HE+ + CH4 -> CH3+ + H + HE
    k(788) = small + (8.50e-11)

    !HE+ + CH4 -> CH4+ + HE
    k(789) = small + (5.10e-11)

    !HE+ + HCNC2 -> C2+ + HCN + HE
    k(790) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCNC2 -> C2N+ + CH + HE
    k(791) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCNC2 -> C3N+ + H + HE
    k(792) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCNC2 -> HCN+ + C2 + HE
    k(793) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC2NC -> CN+ + C2H + HE
    k(794) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC2NC -> C2H+ + CN + HE
    k(795) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC2NC -> C3N+ + H + HE
    k(796) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC2NC -> CNC+ + CH + HE
    k(797) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C2+ + HCN + HE
    k(798) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> CN+ + C2H + HE
    k(799) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C2H+ + CN + HE
    k(800) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C2N+ + CH + HE
    k(801) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C3+ + NH + HE
    k(802) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C3H+ + N + HE
    k(803) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC3N -> C3N+ + H + HE
    k(804) = small + (2.53e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC3 -> C2+ + HNC + HE
    k(805) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC3 -> C3+ + NH + HE
    k(806) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC3 -> C3N+ + H + HE
    k(807) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC3 -> NH+ + C3 + HE
    k(808) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HNC3 -> C2NH+ + C + HE
    k(809) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CN -> NH2 + CN+ + HE
    k(810) = small + (4.33e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CN -> NH2+ + CN + HE
    k(811) = small + (4.33e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC2H2 -> SIH+ + C2H + HE
    k(812) = small + (2.51e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC2H2 -> SIC2H+ + H + HE
    k(813) = small + (2.51e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC3H -> SI+ + C3H + HE
    k(814) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC3H -> SIC3+ + H + HE
    k(815) = small + (2.99e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIC4 -> SIC3+ + C + HE
    k(816) = small + (1.25e-08&
        *(T32)**(-5.00e-01))

    !HE+ + SICH3 -> SIH+ + CH2 + HE
    k(817) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SICH3 -> CH2SI+ + H + HE
    k(818) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !HE+ + SIH4 -> SIH2+ + H2 + HE
    k(819) = small + (1.00e-09)

    !HE+ + SIH4 -> SIH3+ + H + HE
    k(820) = small + (1.00e-09)

    !HE+ + C2H3N -> CN+ + CH3 + HE
    k(821) = small + (4.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H3N -> CH3+ + CN + HE
    k(822) = small + (4.00e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H4 -> C2H+ + H + H2 + HE
    k(823) = small + (4.40e-10)

    !HE+ + C2H4 -> CH2+ + CH2 + HE
    k(824) = small + (4.80e-10)

    !HE+ + C2H4 -> C2H2+ + H2 + HE
    k(825) = small + (2.20e-09)

    !HE+ + C2H4 -> C2H3+ + H + HE
    k(826) = small + (1.70e-10)

    !HE+ + C2H4 -> C2H4+ + HE
    k(827) = small + (2.40e-10)

    !HE+ + C3H3 -> C3+ + H + H2 + HE
    k(828) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H3 -> C3H+ + H2 + HE
    k(829) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H3 -> C3H2+ + H + HE
    k(830) = small + (1.35e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H3 -> H2C3+ + H + HE
    k(831) = small + (1.35e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C4H2 -> C2H+ + C2H + HE
    k(832) = small + (1.00e-09)

    !HE+ + C4H2 -> C4+ + H2 + HE
    k(833) = small + (1.00e-09)

    !HE+ + C4H2 -> C4H+ + H + HE
    k(834) = small + (1.00e-09)

    !HE+ + C5H -> C3H+ + C2 + HE
    k(835) = small + (1.02e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C5H -> C5+ + H + HE
    k(836) = small + (1.02e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C5N -> C4+ + CN + HE
    k(837) = small + (1.30e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C6 -> C4+ + C2 + HE
    k(838) = small + (1.40e-09)

    !HE+ + C6 -> C5+ + C + HE
    k(839) = small + (1.40e-09)

    !HE+ + CH4O -> OH+ + CH3 + HE
    k(840) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH4O -> CH3+ + OH + HE
    k(841) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CHO -> HCO+ + NH2 + HE
    k(842) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CHO -> H2CO+ + NH + HE
    k(843) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CHO -> NH+ + H2CO + HE
    k(844) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CHO -> NH3+ + CO + HE
    k(845) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + NH2CHO -> CO+ + NH3 + HE
    k(846) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H4O -> HCO+ + CH3 + HE
    k(847) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H4O -> CH3+ + HCO + HE
    k(848) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H4O -> C2H2O+ + H2 + HE
    k(849) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H4O -> C2H3O+ + H + HE
    k(850) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5 -> C2H3+ + H2 + HE
    k(851) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5 -> C2H4+ + H + HE
    k(852) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5 -> C2H5+ + HE
    k(853) = small + (6.90e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H3N -> C2H3+ + CN + HE
    k(854) = small + (7.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> C3+ + H2 + H2 + HE
    k(855) = small + (4.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> C3H+ + HE + H + H2
    k(856) = small + (4.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> C3H2+ + HE + H2
    k(857) = small + (2.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> H2C3+ + HE + H2
    k(858) = small + (2.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> C3H3+ + HE + H
    k(859) = small + (2.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C3H4 -> H3C3+ + HE + H
    k(860) = small + (2.00e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4H3 -> C4H2+ + HE + H
    k(861) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4H3 -> C3H3+ + HE + C
    k(862) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4H3 -> H3C3+ + HE + C
    k(863) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4H3 -> C3H2+ + CH + HE
    k(864) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C4H3 -> H2C3+ + CH + HE
    k(865) = small + (3.35e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C5H2 -> C3H+ + C2H + HE
    k(866) = small + (1.67e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C5H2 -> C5+ + H2 + HE
    k(867) = small + (1.67e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C5H2 -> C5H+ + H + HE
    k(868) = small + (1.67e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C6H -> C4H+ + C2 + HE
    k(869) = small + (1.18e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C6H -> C6+ + H + HE
    k(870) = small + (1.18e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C7 -> C5+ + C2 + HE
    k(871) = small + (1.40e-09)

    !HE+ + C7 -> C6+ + C + HE
    k(872) = small + (1.40e-09)

    !HE+ + CH5N -> NH2+ + CH3 + HE
    k(873) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH5N -> CH3+ + NH2 + HE
    k(874) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH5N -> HCNH+ + H + H2 + HE
    k(875) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !HE+ + CH5N -> CH4N+ + H + HE
    k(876) = small + (6.70e-10&
        *(T32)**(-5.00e-01))

    !HE+ + HC5N -> C4H + CN+ + HE
    k(877) = small + (6.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC5N -> C2H+ + C3N + HE
    k(878) = small + (6.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC5N -> C4H+ + CN + HE
    k(879) = small + (6.80e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C6H2 -> C4H+ + C2H + HE
    k(880) = small + (1.00e-09)

    !HE+ + C6H2 -> C6+ + H2 + HE
    k(881) = small + (1.00e-09)

    !HE+ + C6H2 -> C6H+ + H + HE
    k(882) = small + (1.00e-09)

    !HE+ + C7H -> C5H+ + C2 + HE
    k(883) = small + (1.06e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C7H -> C7+ + H + HE
    k(884) = small + (1.06e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C7N -> C6+ + CN + HE
    k(885) = small + (1.40e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C8 -> C6+ + C2 + HE
    k(886) = small + (1.50e-09)

    !HE+ + C8 -> C7+ + C + HE
    k(887) = small + (1.50e-09)

    !HE+ + CH3C3N -> CH3+ + C3N + HE
    k(888) = small + (9.49e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HCOOCH3 -> HCO2+ + CH3 + HE
    k(889) = small + (3.54e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5OH -> C2H2O+ + H2 + H2 + HE
    k(890) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5OH -> C2H3O+ + H + H2 + HE
    k(891) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5OH -> C2H4O+ + H2 + HE
    k(892) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5OH -> C2H5+ + OH + HE
    k(893) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C2H5OH -> C2H5O+ + H + HE
    k(894) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !HE+ + C7H2 -> C5H+ + C2H + HE
    k(895) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C7H2 -> C7+ + H2 + HE
    k(896) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C7H2 -> C7H+ + H + HE
    k(897) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C8H -> C6H+ + C2 + HE
    k(898) = small + (1.17e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C8H -> C8+ + H + HE
    k(899) = small + (1.17e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C9 -> C7+ + C2 + HE
    k(900) = small + (1.50e-09)

    !HE+ + C9 -> C8+ + C + HE
    k(901) = small + (1.50e-09)

    !HE+ + CH3C4H -> C5H2+ + H2 + HE
    k(902) = small + (1.21e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3C4H -> C5H3+ + H + HE
    k(903) = small + (1.21e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3OCH3 -> CH3+ + H2CO + HE + H
    k(904) = small + (1.32e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3OCH3 -> H3CO+ + CH3 + HE
    k(905) = small + (1.32e-09&
        *(T32)**(-5.00e-01))

    !HE+ + HC7N -> C6H+ + CN + HE
    k(906) = small + (2.16e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C2H6CO -> C2H3O+ + CH3 + HE
    k(907) = small + (5.61e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C8H2 -> C6H+ + C2H + HE
    k(908) = small + (1.00e-09)

    !HE+ + C8H2 -> C8+ + H2 + HE
    k(909) = small + (1.00e-09)

    !HE+ + C8H2 -> C8H+ + H + HE
    k(910) = small + (1.00e-09)

    !HE+ + C9H -> C7H+ + C2 + HE
    k(911) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C9H -> C9+ + H + HE
    k(912) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C9N -> C8+ + CN + HE
    k(913) = small + (1.54e-08&
        *(T32)**(-5.00e-01))

    !HE+ + CH3C5N -> CH3+ + C5N + HE
    k(914) = small + (1.04e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C10 -> C8+ + C2 + HE
    k(915) = small + (1.50e-09)

    !HE+ + C10 -> C9+ + C + HE
    k(916) = small + (1.50e-09)

    !HE+ + C9H2 -> C7H+ + C2H + HE
    k(917) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C9H2 -> C9+ + H2 + HE
    k(918) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + C9H2 -> C9H+ + H + HE
    k(919) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3C6H -> C7H2+ + H2 + HE
    k(920) = small + (1.49e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3C6H -> C7H3+ + H + HE
    k(921) = small + (1.49e-09&
        *(T32)**(-5.00e-01))

    !HE+ + CH3C7N -> CH3+ + C7N + HE
    k(922) = small + (1.08e-08&
        *(T32)**(-5.00e-01))

    !HE+ + HC9N -> C8H+ + CN + HE
    k(923) = small + (2.26e-08&
        *(T32)**(-5.00e-01))

    !HE+ + C6H6 -> C6H5+ + H + HE
    k(924) = small + (7.00e-10)

    !HE+ + C6H6 -> C5H5+ + CH + HE
    k(925) = small + (7.00e-10)

    !MG+ + NA -> NA+ + MG
    k(926) = small + (1.00e-11)

    !MG+ + NAH -> NA+ + MGH
    k(927) = small + (1.80e-08&
        *(T32)**(-5.00e-01))

    !N+ + FE -> FE+ + N
    k(928) = small + (1.50e-09)

    !N+ + MG -> MG+ + N
    k(929) = small + (1.20e-09)

    !N+ + C2 -> C2+ + N
    k(930) = small + (1.00e-09)

    !N+ + CH -> CH+ + N
    k(931) = small + (3.60e-10)

    !N+ + CH -> CN+ + H
    k(932) = small + (3.60e-10)

    !N+ + CN -> CN+ + N
    k(933) = small + (1.10e-09)

    !N+ + CO -> CO+ + N
    k(934) = small + (4.90e-10)

    !N+ + CO -> NO+ + C
    k(935) = small + (6.00e-11)

    !N+ + H2 -> NH+ + H
    k(936) = small + (1.00e-09&
        *exp(-8.50e+01*invT))

    !N+ + NH -> N2+ + H
    k(937) = small + (3.70e-10)

    !N+ + NH -> NH+ + N
    k(938) = small + (3.70e-10)

    !N+ + NO -> N2+ + O
    k(939) = small + (5.00e-11)

    !N+ + NO -> NO+ + N
    k(940) = small + (5.10e-10)

    !N+ + O2 -> O+ + NO
    k(941) = small + (3.60e-11)

    !N+ + O2 -> NO+ + O
    k(942) = small + (1.70e-10)

    !N+ + O2 -> O2+ + N
    k(943) = small + (4.00e-10)

    !N+ + OH -> NO+ + H
    k(944) = small + (3.70e-10)

    !N+ + OH -> OH+ + N
    k(945) = small + (3.70e-10)

    !N+ + C2H -> C2H+ + N
    k(946) = small + (9.50e-10)

    !N+ + CH2 -> CH2+ + N
    k(947) = small + (1.00e-09)

    !N+ + CO2 -> CO+ + NO
    k(948) = small + (2.50e-10)

    !N+ + CO2 -> CO2+ + N
    k(949) = small + (1.10e-09)

    !N+ + H2O -> H2O+ + N
    k(950) = small + (2.60e-09)

    !N+ + H2S -> HS+ + NH
    k(951) = small + (5.51e-10)

    !N+ + H2S -> H2S+ + N
    k(952) = small + (1.06e-09)

    !N+ + HCN -> HCN+ + N
    k(953) = small + (1.20e-09)

    !N+ + HCO -> NH+ + CO
    k(954) = small + (4.50e-10)

    !N+ + HCO -> HCO+ + N
    k(955) = small + (4.50e-10)

    !N+ + NH2 -> NH2+ + N
    k(956) = small + (1.00e-09)

    !N+ + OCS -> S+ + CO + N
    k(957) = small + (3.08e-10)

    !N+ + OCS -> CS+ + NO
    k(958) = small + (7.00e-11)

    !N+ + OCS -> OCS+ + N
    k(959) = small + (1.02e-09)

    !N+ + H2CO -> NO+ + CH2
    k(960) = small + (2.90e-10)

    !N+ + H2CO -> HCO+ + NH
    k(961) = small + (7.30e-10)

    !N+ + H2CO -> H2CO+ + N
    k(962) = small + (1.90e-09)

    !N+ + NH3 -> N2H+ + H2
    k(963) = small + (2.20e-10)

    !N+ + NH3 -> NH2+ + NH
    k(964) = small + (2.20e-10)

    !N+ + NH3 -> NH3+ + N
    k(965) = small + (2.00e-09)

    !N+ + CH4 -> HCN+ + H2 + H
    k(966) = small + (5.60e-11)

    !N+ + CH4 -> CH3+ + N + H
    k(967) = small + (4.70e-10)

    !N+ + CH4 -> CH3+ + NH
    k(968) = small + (4.70e-10)

    !N+ + CH4 -> HCNH+ + H + H
    k(969) = small + (3.80e-10)

    !N+ + CH4 -> HCNH+ + H2
    k(970) = small + (3.80e-10)

    !N+ + CH4 -> CH4+ + N
    k(971) = small + (2.80e-11)

    !N+ + CH4O -> NO+ + CH3 + H
    k(972) = small + (3.10e-10)

    !N+ + CH4O -> CH3+ + NO + H
    k(973) = small + (1.24e-10)

    !N+ + CH4O -> H2CO+ + NH + H
    k(974) = small + (9.30e-10)

    !N+ + CH4O -> H3CO+ + NH
    k(975) = small + (4.96e-10)

    !N+ + CH4O -> CH4O+ + N
    k(976) = small + (1.24e-09)

    !O+ + FE -> FE+ + O
    k(977) = small + (2.90e-09)

    !O+ + H -> H+ + O
    k(978) = small + (7.00e-10)

    !O+ + C2 -> C2+ + O
    k(979) = small + (4.80e-10)

    !O+ + C2 -> CO+ + C
    k(980) = small + (4.80e-10)

    !O+ + CH -> CH+ + O
    k(981) = small + (3.50e-10)

    !O+ + CH -> CO+ + H
    k(982) = small + (3.50e-10)

    !O+ + CN -> NO+ + C
    k(983) = small + (1.00e-09)

    !O+ + H2 -> OH+ + H
    k(984) = small + (1.60e-09)

    !O+ + N2 -> NO+ + N
    k(985) = small + (1.20e-12)

    !O+ + NH -> NH+ + O
    k(986) = small + (3.60e-10)

    !O+ + NH -> NO+ + H
    k(987) = small + (3.60e-10)

    !O+ + NO -> NO+ + O
    k(988) = small + (1.70e-12)

    !O+ + O2 -> O2+ + O
    k(989) = small + (3.00e-11)

    !O+ + OH -> O2+ + H
    k(990) = small + (3.60e-10)

    !O+ + OH -> OH+ + O
    k(991) = small + (3.60e-10)

    !O+ + C2H -> CO+ + CH
    k(992) = small + (4.60e-10)

    !O+ + C2H -> C2H+ + O
    k(993) = small + (4.60e-10)

    !O+ + CH2 -> CH2+ + O
    k(994) = small + (9.70e-10)

    !O+ + CO2 -> O2+ + CO
    k(995) = small + (1.10e-09)

    !O+ + H2O -> H2O+ + O
    k(996) = small + (3.20e-09)

    !O+ + H2S -> H2O + S+
    k(997) = small + (2.20e-10)

    !O+ + H2S -> HS+ + OH
    k(998) = small + (4.20e-10)

    !O+ + H2S -> H2S+ + O
    k(999) = small + (1.80e-09)

    !O+ + HCN -> CO+ + NH
    k(1000) = small + (1.20e-09)

    !O+ + HCN -> NO+ + CH
    k(1001) = small + (1.20e-09)

    !O+ + HCN -> HCO+ + N
    k(1002) = small + (1.20e-09)

    !O+ + HCO -> OH+ + CO
    k(1003) = small + (4.30e-10)

    !O+ + HCO -> HCO+ + O
    k(1004) = small + (4.30e-10)

    !O+ + N2O -> NO+ + NO
    k(1005) = small + (6.30e-10)

    !O+ + NH2 -> NH2+ + O
    k(1006) = small + (1.00e-09)

    !O+ + NO2 -> NO2+ + O
    k(1007) = small + (1.60e-09)

    !O+ + OCS -> S+ + CO2
    k(1008) = small + (2.00e-11)

    !O+ + OCS -> OCS+ + O
    k(1009) = small + (6.50e-10)

    !O+ + SO2 -> O2+ + SO
    k(1010) = small + (8.00e-10)

    !O+ + H2CO -> HCO+ + OH
    k(1011) = small + (1.40e-09)

    !O+ + H2CO -> H2CO+ + O
    k(1012) = small + (2.10e-09)

    !O+ + NH3 -> NH3+ + O
    k(1013) = small + (1.20e-09)

    !O+ + CH4 -> CH3+ + OH
    k(1014) = small + (1.10e-10)

    !O+ + CH4 -> CH4+ + O
    k(1015) = small + (8.90e-10)

    !O+ + CH4O -> H2CO+ + H2O
    k(1016) = small + (9.50e-11)

    !O+ + CH4O -> H3CO+ + OH
    k(1017) = small + (1.33e-09)

    !O+ + CH4O -> CH4O+ + O
    k(1018) = small + (4.75e-10)

    !P+ + SI -> P + SI+
    k(1019) = small + (1.00e-09)

    !P+ + O2 -> PO+ + O
    k(1020) = small + (4.90e-10)

    !P+ + OH -> PO+ + H
    k(1021) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !P+ + H2O -> PO+ + H2
    k(1022) = small + (1.07e-09&
        *(T32)**(-5.00e-01))

    !P+ + H2O -> HPO+ + H
    k(1023) = small + (1.07e-09&
        *(T32)**(-5.00e-01))

    !P+ + C2H2 -> PC2H+ + H
    k(1024) = small + (1.24e-09)

    !P+ + C2H2 -> PC2H2+
    k(1025) = small + (6.00e-11)

    !P+ + NH3 -> NH3+ + P
    k(1026) = small + (8.60e-10&
        *(T32)**(-5.00e-01))

    !P+ + NH3 -> PNH2+ + H
    k(1027) = small + (8.60e-10&
        *(T32)**(-5.00e-01))

    !P+ + CH4 -> PCH2+ + H2
    k(1028) = small + (9.60e-10)

    !P+ + C2H4 -> PC2H2+ + H2
    k(1029) = small + (1.20e-09)

    !S+ + FE -> FE+ + S
    k(1030) = small + (1.80e-10)

    !S+ + MG -> MG+ + S
    k(1031) = small + (2.80e-10)

    !S+ + NA -> NA+ + S
    k(1032) = small + (2.60e-10)

    !S+ + SI -> SI+ + S
    k(1033) = small + (1.60e-09)

    !S+ + C2 -> CS+ + C
    k(1034) = small + (8.10e-10)

    !S+ + CH -> CS+ + H
    k(1035) = small + (4.40e-09&
        *(T32)**(-5.00e-01))

    !S+ + NH -> NS+ + H
    k(1036) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !S+ + NO -> NO+ + S
    k(1037) = small + (3.60e-10&
        *(T32)**(-5.00e-01))

    !S+ + O2 -> SO+ + O
    k(1038) = small + (2.00e-11)

    !S+ + OH -> SO+ + H
    k(1039) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !S+ + SIC -> SIC+ + S
    k(1040) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !S+ + SIH -> SIH+ + S
    k(1041) = small + (1.41e-10&
        *(T32)**(-5.00e-01))

    !S+ + SIH -> SIS+ + H
    k(1042) = small + (1.41e-10&
        *(T32)**(-5.00e-01))

    !S+ + SIS -> SIS+ + S
    k(1043) = small + (3.50e-09&
        *(T32)**(-5.00e-01))

    !S+ + C2H -> C2S+ + H
    k(1044) = small + (1.96e-09&
        *(T32)**(-5.00e-01))

    !S+ + CH2 -> HCS+ + H
    k(1045) = small + (7.08e-10&
        *(T32)**(-5.00e-01))

    !S+ + H2S -> S2+ + H2
    k(1046) = small + (6.40e-10&
        *(T32)**(-5.00e-01))

    !S+ + H2S -> S2H+ + H
    k(1047) = small + (2.00e-10&
        *(T32)**(-5.00e-01))

    !S+ + H2S -> H2S+ + S
    k(1048) = small + (4.40e-11&
        *(T32)**(-5.00e-01))

    !S+ + HCO -> HS+ + CO
    k(1049) = small + (5.00e-10&
        *(T32)**(-5.00e-01))

    !S+ + HCO -> HCO+ + S
    k(1050) = small + (5.00e-10&
        *(T32)**(-5.00e-01))

    !S+ + OCS -> S2+ + CO
    k(1051) = small + (9.10e-10&
        *(T32)**(-5.00e-01))

    !S+ + C2H2 -> HC2S+ + H
    k(1052) = small + (9.50e-10)

    !S+ + C3H -> C3S+ + H
    k(1053) = small + (6.00e-09&
        *(T32)**(-5.00e-01))

    !S+ + CH3 -> H2CS+ + H
    k(1054) = small + (1.00e-11)

    !S+ + H2CO -> H2S+ + CO
    k(1055) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !S+ + H2CO -> HCO+ + HS
    k(1056) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !S+ + NH3 -> NH3+ + S
    k(1057) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !S+ + C2H3 -> HC2S+ + H2
    k(1058) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !S+ + C3H2 -> HC3S+ + H
    k(1059) = small + (2.87e-09&
        *(T32)**(-5.00e-01))

    !S+ + C4H -> C4S+ + H
    k(1060) = small + (1.88e-09&
        *(T32)**(-5.00e-01))

    !S+ + CH4 -> H3CS+ + H
    k(1061) = small + (1.40e-10)

    !S+ + C4H2 -> C3H2+ + CS
    k(1062) = small + (1.20e-10)

    !S+ + C4H2 -> H2C3+ + CS
    k(1063) = small + (1.20e-10)

    !S+ + C4H2 -> C4H+ + HS
    k(1064) = small + (1.60e-10)

    !S+ + C4H2 -> C4H2+ + S
    k(1065) = small + (7.20e-10)

    !S+ + C4H2 -> HC4S+ + H
    k(1066) = small + (4.80e-10)

    !SI+ + FE -> FE+ + SI
    k(1067) = small + (1.90e-09)

    !SI+ + MG -> MG+ + SI
    k(1068) = small + (2.90e-09)

    !SI+ + NA -> NA+ + SI
    k(1069) = small + (2.70e-09)

    !SI+ + CH -> SIC+ + H
    k(1070) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !SI+ + NH -> SIN+ + H
    k(1071) = small + (3.85e-09&
        *(T32)**(-5.00e-01))

    !SI+ + OH -> SIO+ + H
    k(1072) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !SI+ + CH2 -> CHSI+ + H
    k(1073) = small + (7.23e-10&
        *(T32)**(-5.00e-01))

    !SI+ + H2O -> HSIO+ + H
    k(1074) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !SI+ + HCN -> SINC+ + H
    k(1075) = small + (7.37e-09&
        *(T32)**(-5.00e-01))

    !SI+ + HNC -> SINC+ + H
    k(1076) = small + (6.68e-09&
        *(T32)**(-5.00e-01))

    !SI+ + OCS -> SIS+ + CO
    k(1077) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C2H2 -> SIC2H+ + H
    k(1078) = small + (2.50e-10)

    !SI+ + C3H -> SIC3+ + H
    k(1079) = small + (6.21e-09&
        *(T32)**(-5.00e-01))

    !SI+ + CH3 -> CHSI+ + H2
    k(1080) = small + (1.00e-09)

    !SI+ + CH3 -> CH2SI+ + H
    k(1081) = small + (1.00e-09)

    !SI+ + NH3 -> SINH2+ + H
    k(1082) = small + (1.75e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C2H3 -> SIC2H2+ + H
    k(1083) = small + (1.57e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C3H2 -> SIC3H+ + H
    k(1084) = small + (2.97e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C4H -> SIC4+ + H
    k(1085) = small + (1.96e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C2H4 -> SIC2H3+ + H
    k(1086) = small + (1.00e-09)

    !SI+ + C3H3 -> SIC3H2+ + H
    k(1087) = small + (3.84e-09&
        *(T32)**(-5.00e-01))

    !SI+ + C4H2 -> C4H+ + SIH
    k(1088) = small + (1.60e-09)

    !SI+ + C3H4 -> SIC3H2+ + H2
    k(1089) = small + (7.46e-10&
        *(T32)**(-5.00e-01))

    !C2+ + C -> C+ + C2
    k(1090) = small + (1.10e-10)

    !C2+ + N -> C+ + CN
    k(1091) = small + (4.00e-11)

    !C2+ + O -> CO+ + C
    k(1092) = small + (3.10e-10)

    !C2+ + S -> S+ + C2
    k(1093) = small + (5.80e-10)

    !C2+ + S -> CS+ + C
    k(1094) = small + (5.80e-10)

    !C2+ + C2 -> C3+ + C
    k(1095) = small + (8.70e-10)

    !C2+ + CH -> CH+ + C2
    k(1096) = small + (3.20e-10)

    !C2+ + CH -> C3+ + H
    k(1097) = small + (3.20e-10)

    !C2+ + H2 -> C2H+ + H
    k(1098) = small + (1.10e-09)

    !C2+ + NH -> C2H+ + N
    k(1099) = small + (3.30e-10)

    !C2+ + NH -> C2N+ + H
    k(1100) = small + (3.30e-10)

    !C2+ + NO -> NO+ + C2
    k(1101) = small + (3.40e-10)

    !C2+ + O2 -> CO+ + CO
    k(1102) = small + (8.00e-10)

    !C2+ + CH2 -> CH2+ + C2
    k(1103) = small + (4.50e-10)

    !C2+ + CH2 -> C3H+ + H
    k(1104) = small + (4.50e-10)

    !C2+ + H2O -> C2H+ + OH
    k(1105) = small + (4.40e-10)

    !C2+ + H2O -> C2HO+ + H
    k(1106) = small + (4.40e-10)

    !C2+ + HCN -> C3H+ + N
    k(1107) = small + (7.80e-10)

    !C2+ + HCN -> C3N+ + H
    k(1108) = small + (1.60e-09)

    !C2+ + HCO -> C2H+ + CO
    k(1109) = small + (3.80e-10)

    !C2+ + HCO -> HCO+ + C2
    k(1110) = small + (3.80e-10)

    !C2+ + NH2 -> NH2+ + C2
    k(1111) = small + (4.60e-10)

    !C2+ + NH2 -> C2NH+ + H
    k(1112) = small + (4.60e-10)

    !C2+ + C2H2 -> C4H+ + H
    k(1113) = small + (1.70e-09)

    !C2+ + CH4 -> C2H+ + CH3
    k(1114) = small + (2.38e-10)

    !C2+ + CH4 -> C2H2+ + CH2
    k(1115) = small + (1.82e-10)

    !C2+ + CH4 -> C3H+ + H2 + H
    k(1116) = small + (1.96e-10)

    !C2+ + CH4 -> C3H2+ + H2
    k(1117) = small + (2.87e-10)

    !C2+ + CH4 -> H2C3+ + H2
    k(1118) = small + (2.87e-10)

    !C2+ + CH4 -> C3H3+ + H
    k(1119) = small + (1.05e-10)

    !C2+ + CH4 -> H3C3+ + H
    k(1120) = small + (1.05e-10)

    !CH+ + C -> C2+ + H
    k(1121) = small + (1.20e-09)

    !CH+ + FE -> FE+ + CH
    k(1122) = small + (2.60e-10)

    !CH+ + H -> C+ + H2
    k(1123) = small + (7.50e-10)

    !CH+ + MG -> MG+ + CH
    k(1124) = small + (3.60e-10)

    !CH+ + N -> CN+ + H
    k(1125) = small + (1.90e-10)

    !CH+ + NA -> NA+ + CH
    k(1126) = small + (3.50e-10)

    !CH+ + O -> CO+ + H
    k(1127) = small + (3.50e-10)

    !CH+ + S -> S+ + CH
    k(1128) = small + (4.70e-10)

    !CH+ + S -> CS+ + H
    k(1129) = small + (4.70e-10)

    !CH+ + S -> HS+ + C
    k(1130) = small + (4.70e-10)

    !CH+ + SI -> SI+ + CH
    k(1131) = small + (2.00e-10)

    !CH+ + C2 -> C3+ + H
    k(1132) = small + (1.00e-09)

    !CH+ + CH -> C2+ + H2
    k(1133) = small + (7.40e-10)

    !CH+ + CN -> C2N+ + H
    k(1134) = small + (1.10e-09)

    !CH+ + CN -> CNC+ + H
    k(1135) = small + (5.50e-10)

    !CH+ + H2 -> CH2+ + H
    k(1136) = small + (1.20e-09)

    !CH+ + NH -> CN+ + H2
    k(1137) = small + (7.60e-10)

    !CH+ + NO -> NO+ + CH
    k(1138) = small + (7.60e-10)

    !CH+ + O2 -> HCO + O+
    k(1139) = small + (1.00e-11)

    !CH+ + O2 -> CO+ + OH
    k(1140) = small + (1.00e-11)

    !CH+ + O2 -> HCO+ + O
    k(1141) = small + (9.70e-10)

    !CH+ + OH -> CO+ + H2
    k(1142) = small + (7.50e-10)

    !CH+ + C2H -> C3+ + H2
    k(1143) = small + (9.80e-10)

    !CH+ + CH2 -> C2H+ + H2
    k(1144) = small + (1.00e-09)

    !CH+ + CO2 -> HCO+ + CO
    k(1145) = small + (1.60e-09)

    !CH+ + H2O -> HCO+ + H2
    k(1146) = small + (2.90e-09)

    !CH+ + H2O -> H2CO+ + H
    k(1147) = small + (5.80e-10)

    !CH+ + H2O -> H3O+ + C
    k(1148) = small + (5.80e-10)

    !CH+ + H2S -> HCS+ + H2
    k(1149) = small + (6.60e-10)

    !CH+ + H2S -> H3S+ + C
    k(1150) = small + (6.30e-10)

    !CH+ + HCN -> C2N+ + H2
    k(1151) = small + (3.60e-10)

    !CH+ + HCN -> C2NH+ + H
    k(1152) = small + (2.40e-10)

    !CH+ + HCN -> HCNH+ + C
    k(1153) = small + (2.40e-09)

    !CH+ + HCO -> CH2+ + CO
    k(1154) = small + (4.60e-10)

    !CH+ + HCO -> HCO+ + CH
    k(1155) = small + (4.60e-10)

    !CH+ + HNC -> HCNH+ + C
    k(1156) = small + (1.80e-09)

    !CH+ + NH2 -> HCN+ + H2
    k(1157) = small + (1.10e-09)

    !CH+ + OCS -> HCS+ + CO
    k(1158) = small + (1.05e-09)

    !CH+ + OCS -> HOCS+ + C
    k(1159) = small + (8.55e-10)

    !CH+ + C2H2 -> C3H2+ + H
    k(1160) = small + (1.20e-09)

    !CH+ + C2H2 -> H2C3+ + H
    k(1161) = small + (1.20e-09)

    !CH+ + H2CO -> HCO+ + CH2
    k(1162) = small + (9.60e-10)

    !CH+ + H2CO -> CH3+ + CO
    k(1163) = small + (9.60e-10)

    !CH+ + H2CO -> H3CO+ + C
    k(1164) = small + (9.60e-10)

    !CH+ + NH3 -> H2NC+ + H2
    k(1165) = small + (1.84e-09)

    !CH+ + NH3 -> NH3+ + CH
    k(1166) = small + (4.59e-10)

    !CH+ + NH3 -> NH4+ + C
    k(1167) = small + (4.05e-10)

    !CH+ + CH4 -> C2H2+ + H2 + H
    k(1168) = small + (1.40e-10)

    !CH+ + CH4 -> C2H3+ + H2
    k(1169) = small + (1.10e-09)

    !CH+ + CH4 -> C2H4+ + H
    k(1170) = small + (6.50e-11)

    !CH+ + CH4O -> CH3+ + H2CO
    k(1171) = small + (1.45e-09)

    !CH+ + CH4O -> H3CO+ + CH2
    k(1172) = small + (2.90e-10)

    !CH+ + CH4O -> CH5O+ + C
    k(1173) = small + (1.16e-09)

    !CN+ + C -> C+ + CN
    k(1174) = small + (1.10e-10)

    !CN+ + H -> H+ + CN
    k(1175) = small + (6.40e-10)

    !CN+ + O -> O+ + CN
    k(1176) = small + (6.50e-11)

    !CN+ + S -> S+ + CN
    k(1177) = small + (1.10e-09)

    !CN+ + C2 -> C2+ + CN
    k(1178) = small + (8.50e-10)

    !CN+ + CH -> CH+ + CN
    k(1179) = small + (6.40e-10)

    !CN+ + CO -> CO+ + CN
    k(1180) = small + (6.30e-10)

    !CN+ + H2 -> HCN+ + H
    k(1181) = small + (7.50e-10)

    !CN+ + NH -> NH+ + CN
    k(1182) = small + (6.50e-10)

    !CN+ + NO -> NO+ + CN
    k(1183) = small + (8.10e-10)

    !CN+ + NO -> NCO+ + N
    k(1184) = small + (1.90e-10)

    !CN+ + O2 -> NO+ + CO
    k(1185) = small + (8.60e-11)

    !CN+ + O2 -> O2+ + CN
    k(1186) = small + (7.80e-10)

    !CN+ + O2 -> NCO+ + O
    k(1187) = small + (8.60e-11)

    !CN+ + OH -> OH+ + CN
    k(1188) = small + (6.40e-10)

    !CN+ + C2H -> C2H+ + CN
    k(1189) = small + (8.00e-10)

    !CN+ + CH2 -> CH2+ + CN
    k(1190) = small + (8.80e-10)

    !CN+ + CO2 -> C2O+ + NO
    k(1191) = small + (2.25e-10)

    !CN+ + CO2 -> CO2+ + CN
    k(1192) = small + (4.50e-10)

    !CN+ + CO2 -> NCO+ + CO
    k(1193) = small + (2.25e-10)

    !CN+ + H2O -> HCN+ + OH
    k(1194) = small + (1.60e-09)

    !CN+ + H2O -> HCO+ + NH
    k(1195) = small + (1.60e-10)

    !CN+ + H2O -> H2NC+ + O
    k(1196) = small + (4.80e-10)

    !CN+ + H2O -> HNCO+ + H
    k(1197) = small + (6.40e-10)

    !CN+ + HCN -> HCN+ + CN
    k(1198) = small + (2.40e-09)

    !CN+ + HCN -> C2N2+ + H
    k(1199) = small + (3.15e-10)

    !CN+ + HCO -> HCN+ + CO
    k(1200) = small + (3.70e-10)

    !CN+ + HCO -> HCO+ + CN
    k(1201) = small + (3.70e-10)

    !CN+ + NH2 -> NH2+ + CN
    k(1202) = small + (9.10e-10)

    !CN+ + H2CO -> HCO+ + HCN
    k(1203) = small + (5.20e-10)

    !CN+ + H2CO -> H2CO+ + CN
    k(1204) = small + (5.20e-10)

    !CO+ + C -> C+ + CO
    k(1205) = small + (1.10e-10)

    !CO+ + H -> H+ + CO
    k(1206) = small + (4.00e-10)

    !CO+ + N -> NO+ + C
    k(1207) = small + (8.10e-11)

    !CO+ + O -> O+ + CO
    k(1208) = small + (1.40e-10)

    !CO+ + S -> S+ + CO
    k(1209) = small + (1.10e-09)

    !CO+ + C2 -> C2+ + CO
    k(1210) = small + (8.40e-10)

    !CO+ + CH -> CH+ + CO
    k(1211) = small + (3.20e-10)

    !CO+ + CH -> HCO+ + C
    k(1212) = small + (3.20e-10)

    !CO+ + H2 -> HCO+ + H
    k(1213) = small + (7.50e-10)

    !CO+ + NH -> NH+ + CO
    k(1214) = small + (3.20e-10)

    !CO+ + NH -> HCO+ + N
    k(1215) = small + (3.20e-10)

    !CO+ + NO -> NO+ + CO
    k(1216) = small + (3.30e-10)

    !CO+ + O2 -> O2+ + CO
    k(1217) = small + (1.20e-10)

    !CO+ + OH -> OH+ + CO
    k(1218) = small + (3.10e-10)

    !CO+ + OH -> HCO+ + O
    k(1219) = small + (3.10e-10)

    !CO+ + C2H -> C2H+ + CO
    k(1220) = small + (3.90e-10)

    !CO+ + C2H -> HCO+ + C2
    k(1221) = small + (3.90e-10)

    !CO+ + CH2 -> CH2+ + CO
    k(1222) = small + (4.30e-10)

    !CO+ + CH2 -> HCO+ + CH
    k(1223) = small + (4.30e-10)

    !CO+ + CO2 -> CO2+ + CO
    k(1224) = small + (1.10e-09)

    !CO+ + H2O -> H2O+ + CO
    k(1225) = small + (1.70e-09)

    !CO+ + H2O -> HCO+ + OH
    k(1226) = small + (8.80e-10)

    !CO+ + H2S -> H2S+ + CO
    k(1227) = small + (2.44e-09)

    !CO+ + H2S -> HCO+ + HS
    k(1228) = small + (1.56e-10)

    !CO+ + HCN -> HCN+ + CO
    k(1229) = small + (3.40e-10)

    !CO+ + HCO -> HCO+ + CO
    k(1230) = small + (7.40e-10)

    !CO+ + NH2 -> HCO+ + NH
    k(1231) = small + (4.50e-10)

    !CO+ + NH2 -> NH2+ + CO
    k(1232) = small + (4.50e-10)

    !CO+ + SO2 -> SO+ + CO2
    k(1233) = small + (1.70e-09)

    !CO+ + H2CO -> HCO+ + HCO
    k(1234) = small + (1.70e-09)

    !CO+ + H2CO -> H2CO+ + CO
    k(1235) = small + (1.40e-09)

    !CO+ + NH3 -> HCO+ + NH2
    k(1236) = small + (4.12e-11)

    !CO+ + NH3 -> NH3+ + CO
    k(1237) = small + (2.02e-09)

    !CO+ + CH4 -> HCO+ + CH3
    k(1238) = small + (4.55e-10)

    !CO+ + CH4 -> CH4+ + CO
    k(1239) = small + (7.93e-10)

    !CO+ + CH4 -> C2H3O+ + H
    k(1240) = small + (5.20e-11)

    !CP+ + O -> P+ + CO
    k(1241) = small + (2.00e-10)

    !CP+ + H2 -> HCP+ + H
    k(1242) = small + (1.00e-09)

    !CS+ + C -> C+ + CS
    k(1243) = small + (1.60e-09)

    !CS+ + FE -> FE+ + CS
    k(1244) = small + (1.70e-10)

    !CS+ + MG -> MG+ + CS
    k(1245) = small + (2.70e-10)

    !CS+ + NA -> NA+ + CS
    k(1246) = small + (2.30e-10)

    !CS+ + O -> CO+ + S
    k(1247) = small + (6.00e-11)

    !CS+ + SI -> SI+ + CS
    k(1248) = small + (1.50e-10)

    !CS+ + H2 -> HCS+ + H
    k(1249) = small + (4.80e-10)

    !CS+ + O2 -> OCS+ + O
    k(1250) = small + (1.30e-10)

    !CS+ + CH4 -> HCS+ + CH3
    k(1251) = small + (5.00e-10)

    !H2+ + C -> CH+ + H
    k(1252) = small + (2.40e-09)

    !H2+ + H -> H+ + H2
    k(1253) = small + (6.40e-10)

    !H2+ + N -> NH+ + H
    k(1254) = small + (1.90e-09)

    !H2+ + O -> OH+ + H
    k(1255) = small + (1.50e-09)

    !H2+ + C2 -> C2+ + H2
    k(1256) = small + (1.10e-09)

    !H2+ + C2 -> C2H+ + H
    k(1257) = small + (1.10e-09)

    !H2+ + CH -> CH+ + H2
    k(1258) = small + (7.10e-10)

    !H2+ + CH -> CH2+ + H
    k(1259) = small + (7.10e-10)

    !H2+ + CN -> CN+ + H2
    k(1260) = small + (1.20e-09)

    !H2+ + CN -> HCN+ + H
    k(1261) = small + (1.20e-09)

    !H2+ + CO -> CO+ + H2
    k(1262) = small + (6.00e-10)

    !H2+ + CO -> HCO+ + H
    k(1263) = small + (2.20e-09)

    !H2+ + H2 -> H3+ + H
    k(1264) = small + (2.10e-09)

    !H2+ + N2 -> N2H+ + H
    k(1265) = small + (2.00e-09)

    !H2+ + NH -> NH+ + H2
    k(1266) = small + (7.60e-10)

    !H2+ + NH -> NH2+ + H
    k(1267) = small + (7.60e-10)

    !H2+ + NO -> NO+ + H2
    k(1268) = small + (1.10e-09)

    !H2+ + NO -> HNO+ + H
    k(1269) = small + (1.10e-09)

    !H2+ + O2 -> O2+ + H2
    k(1270) = small + (8.00e-10)

    !H2+ + O2 -> O2H+ + H
    k(1271) = small + (1.90e-09)

    !H2+ + OH -> OH+ + H2
    k(1272) = small + (7.60e-10)

    !H2+ + OH -> H2O+ + H
    k(1273) = small + (7.60e-10)

    !H2+ + C2H -> C2H+ + H2
    k(1274) = small + (1.00e-09)

    !H2+ + C2H -> C2H2+ + H
    k(1275) = small + (1.00e-09)

    !H2+ + CH2 -> CH2+ + H2
    k(1276) = small + (1.00e-09)

    !H2+ + CH2 -> CH3+ + H
    k(1277) = small + (1.00e-09)

    !H2+ + CO2 -> CO+ + H2O
    k(1278) = small + (1.40e-09)

    !H2+ + CO2 -> CO2+ + H2
    k(1279) = small + (1.40e-09)

    !H2+ + CO2 -> HCO2+ + H
    k(1280) = small + (2.35e-09)

    !H2+ + H2O -> H2O+ + H2
    k(1281) = small + (3.90e-09)

    !H2+ + H2O -> H3O+ + H
    k(1282) = small + (3.40e-09)

    !H2+ + H2S -> S+ + H2 + H2
    k(1283) = small + (7.70e-10)

    !H2+ + H2S -> HS+ + H + H2
    k(1284) = small + (8.60e-10)

    !H2+ + H2S -> H2S+ + H2
    k(1285) = small + (2.70e-09)

    !H2+ + HCN -> HCN+ + H2
    k(1286) = small + (2.70e-09)

    !H2+ + HCO -> H3+ + CO
    k(1287) = small + (1.00e-09)

    !H2+ + HCO -> HCO+ + H2
    k(1288) = small + (1.00e-09)

    !H2+ + NH2 -> NH2+ + H2
    k(1289) = small + (2.10e-09)

    !H2+ + C2H2 -> C2H2+ + H2
    k(1290) = small + (4.80e-09)

    !H2+ + C2H2 -> C2H3+ + H
    k(1291) = small + (4.80e-10)

    !H2+ + H2CO -> HCO+ + H + H2
    k(1292) = small + (1.40e-09)

    !H2+ + H2CO -> H2CO+ + H2
    k(1293) = small + (1.40e-09)

    !H2+ + NH3 -> NH3+ + H2
    k(1294) = small + (5.70e-09)

    !H2+ + CH4 -> CH3+ + H + H2
    k(1295) = small + (2.30e-09)

    !H2+ + CH4 -> CH4+ + H2
    k(1296) = small + (1.40e-09)

    !H2+ + CH4 -> CH5+ + H
    k(1297) = small + (1.10e-10)

    !H2+ + C2H4 -> C2H2+ + H2 + H2
    k(1298) = small + (8.82e-10)

    !H2+ + C2H4 -> C2H3+ + H2 + H
    k(1299) = small + (1.81e-09)

    !H2+ + C2H4 -> C2H4+ + H2
    k(1300) = small + (2.21e-09)

    !HCL+ + H2 -> H2CL+ + H
    k(1301) = small + (1.30e-09)

    !HEH+ + H -> H2+ + HE
    k(1302) = small + (9.00e-10)

    !HEH+ + H2 -> H3+ + HE
    k(1303) = small + (1.80e-09)

    !HS+ + C -> CS+ + H
    k(1304) = small + (9.90e-10)

    !HS+ + FE -> FE+ + HS
    k(1305) = small + (1.60e-09)

    !HS+ + H -> S+ + H2
    k(1306) = small + (1.10e-10)

    !HS+ + MG -> MG+ + HS
    k(1307) = small + (2.60e-09)

    !HS+ + N -> NS+ + H
    k(1308) = small + (7.40e-10)

    !HS+ + NA -> NA+ + HS
    k(1309) = small + (2.20e-09)

    !HS+ + O -> S+ + OH
    k(1310) = small + (2.90e-10)

    !HS+ + O -> SO+ + H
    k(1311) = small + (2.90e-10)

    !HS+ + S -> S+ + HS
    k(1312) = small + (9.70e-10)

    !HS+ + SI -> SI+ + HS
    k(1313) = small + (1.40e-09)

    !HS+ + CH -> CH2+ + S
    k(1314) = small + (5.80e-10)

    !HS+ + NO -> NO+ + HS
    k(1315) = small + (4.50e-10)

    !HS+ + H2O -> H3O+ + S
    k(1316) = small + (7.80e-10)

    !HS+ + H2S -> S2H+ + H2
    k(1317) = small + (2.80e-10)

    !HS+ + H2S -> H3S+ + S
    k(1318) = small + (2.90e-10)

    !HS+ + HCN -> HCNH+ + S
    k(1319) = small + (8.90e-10)

    !HS+ + HNC -> HCNH+ + S
    k(1320) = small + (8.60e-10)

    !HS+ + NH3 -> NH3+ + HS
    k(1321) = small + (8.40e-10)

    !HS+ + NH3 -> NH4+ + S
    k(1322) = small + (7.70e-10)

    !HS+ + CH4 -> H3CS+ + H2
    k(1323) = small + (5.40e-10)

    !N2+ + C -> C+ + N2
    k(1324) = small + (1.10e-10)

    !N2+ + FE -> FE+ + N2
    k(1325) = small + (4.30e-10)

    !N2+ + H -> H+ + N2
    k(1326) = small + (1.20e-10)

    !N2+ + MG -> MG+ + N2
    k(1327) = small + (7.00e-10)

    !N2+ + N -> N+ + N2
    k(1328) = small + (1.00e-11)

    !N2+ + NA -> NA+ + N2
    k(1329) = small + (1.90e-09)

    !N2+ + O -> O+ + N2
    k(1330) = small + (1.00e-11)

    !N2+ + O -> NO+ + N
    k(1331) = small + (1.40e-10)

    !N2+ + S -> S+ + N2
    k(1332) = small + (1.10e-09)

    !N2+ + C2 -> C2+ + N2
    k(1333) = small + (8.40e-10)

    !N2+ + CH -> CH+ + N2
    k(1334) = small + (6.30e-10)

    !N2+ + CN -> CN+ + N2
    k(1335) = small + (1.00e-10)

    !N2+ + CO -> CO+ + N2
    k(1336) = small + (7.00e-11)

    !N2+ + H2 -> N2H+ + H
    k(1337) = small + (1.70e-09)

    !N2+ + NH -> NH+ + N2
    k(1338) = small + (6.50e-10)

    !N2+ + NO -> NO+ + N2
    k(1339) = small + (4.40e-10)

    !N2+ + O2 -> O2+ + N2
    k(1340) = small + (5.00e-11)

    !N2+ + OH -> OH+ + N2
    k(1341) = small + (6.30e-10)

    !N2+ + C2H -> C2H+ + N2
    k(1342) = small + (7.90e-10)

    !N2+ + CH2 -> CH2+ + N2
    k(1343) = small + (8.70e-10)

    !N2+ + CO2 -> CO2+ + N2
    k(1344) = small + (9.00e-10)

    !N2+ + H2O -> H2O+ + N2
    k(1345) = small + (2.20e-09)

    !N2+ + H2O -> N2H+ + OH
    k(1346) = small + (2.00e-09)

    !N2+ + H2S -> S+ + N2 + H2
    k(1347) = small + (2.25e-10)

    !N2+ + H2S -> HS+ + N2 + H
    k(1348) = small + (1.13e-09)

    !N2+ + H2S -> H2S+ + N2
    k(1349) = small + (1.50e-10)

    !N2+ + HCN -> HCN+ + N2
    k(1350) = small + (1.00e-09)

    !N2+ + HCO -> HCO+ + N2
    k(1351) = small + (3.70e-10)

    !N2+ + HCO -> N2H+ + CO
    k(1352) = small + (3.70e-10)

    !N2+ + NH2 -> NH2+ + N2
    k(1353) = small + (8.90e-10)

    !N2+ + OCS -> S+ + N2 + CO
    k(1354) = small + (1.04e-09)

    !N2+ + OCS -> OCS+ + N2
    k(1355) = small + (2.60e-10)

    !N2+ + H2CO -> HCO+ + N2 + H
    k(1356) = small + (2.50e-09)

    !N2+ + H2CO -> H2CO+ + N2
    k(1357) = small + (3.77e-10)

    !N2+ + NH3 -> NH3+ + N2
    k(1358) = small + (1.90e-09)

    !N2+ + CH4 -> CH2+ + H2 + N2
    k(1359) = small + (7.00e-11)

    !N2+ + CH4 -> CH3+ + N2 + H
    k(1360) = small + (9.30e-10)

    !NH+ + C -> CH+ + N
    k(1361) = small + (1.60e-09)

    !NH+ + N -> N2+ + H
    k(1362) = small + (1.30e-09)

    !NH+ + O -> OH+ + N
    k(1363) = small + (1.00e-09)

    !NH+ + S -> S+ + NH
    k(1364) = small + (6.90e-10)

    !NH+ + S -> HS+ + N
    k(1365) = small + (6.90e-10)

    !NH+ + S -> NS+ + H
    k(1366) = small + (6.90e-10)

    !NH+ + C2 -> C2H+ + N
    k(1367) = small + (4.90e-10)

    !NH+ + C2 -> C2N+ + H
    k(1368) = small + (4.90e-10)

    !NH+ + C2 -> HCN+ + C
    k(1369) = small + (4.90e-10)

    !NH+ + CH -> CH2+ + N
    k(1370) = small + (9.90e-10)

    !NH+ + CN -> HCN+ + N
    k(1371) = small + (1.60e-09)

    !NH+ + CO -> HCO+ + N
    k(1372) = small + (1.60e-09)

    !NH+ + CO -> NCO+ + H
    k(1373) = small + (5.39e-10)

    !NH+ + H2 -> H3+ + N
    k(1374) = small + (2.25e-10)

    !NH+ + H2 -> NH2+ + H
    k(1375) = small + (1.00e-09)

    !NH+ + N2 -> N2H+ + N
    k(1376) = small + (1.50e-09)

    !NH+ + NH -> NH2+ + N
    k(1377) = small + (1.00e-09)

    !NH+ + NO -> NO+ + NH
    k(1378) = small + (7.10e-10)

    !NH+ + NO -> N2H+ + O
    k(1379) = small + (1.78e-10)

    !NH+ + O2 -> NO+ + OH
    k(1380) = small + (2.00e-10)

    !NH+ + O2 -> O2+ + NH
    k(1381) = small + (4.50e-10)

    !NH+ + O2 -> O2H+ + N
    k(1382) = small + (1.60e-10)

    !NH+ + OH -> H2O+ + N
    k(1383) = small + (1.00e-09)

    !NH+ + C2H -> C2H2+ + N
    k(1384) = small + (1.40e-09)

    !NH+ + CH2 -> CH3+ + N
    k(1385) = small + (1.40e-09)

    !NH+ + CO2 -> NO+ + HCO
    k(1386) = small + (3.30e-10)

    !NH+ + CO2 -> HNO+ + CO
    k(1387) = small + (3.85e-10)

    !NH+ + CO2 -> HCO2+ + N
    k(1388) = small + (3.90e-10)

    !NH+ + H2O -> H2O+ + NH
    k(1389) = small + (1.05e-09)

    !NH+ + H2O -> HNO+ + H2
    k(1390) = small + (3.50e-10)

    !NH+ + H2O -> NH2+ + OH
    k(1391) = small + (8.75e-10)

    !NH+ + H2O -> H3O+ + N
    k(1392) = small + (2.10e-09)

    !NH+ + H2O -> NH3+ + O
    k(1393) = small + (1.75e-10)

    !NH+ + HCN -> HCNH+ + N
    k(1394) = small + (1.80e-09)

    !NH+ + HCO -> H2CO+ + N
    k(1395) = small + (1.30e-09)

    !NH+ + HNC -> HCNH+ + N
    k(1396) = small + (1.80e-09)

    !NH+ + NH2 -> NH3+ + N
    k(1397) = small + (1.50e-09)

    !NH+ + H2CO -> HCO+ + NH2
    k(1398) = small + (1.80e-09)

    !NH+ + H2CO -> H2CO+ + NH
    k(1399) = small + (9.90e-10)

    !NH+ + H2CO -> H3CO+ + N
    k(1400) = small + (4.95e-10)

    !NH+ + NH3 -> NH3+ + NH
    k(1401) = small + (1.80e-09)

    !NH+ + NH3 -> NH4+ + N
    k(1402) = small + (6.00e-10)

    !NO+ + FE -> FE+ + NO
    k(1403) = small + (9.20e-10)

    !NO+ + MG -> MG+ + NO
    k(1404) = small + (8.10e-10)

    !NO+ + NA -> NA+ + NO
    k(1405) = small + (7.00e-11)

    !NO+ + SI -> SI+ + NO
    k(1406) = small + (1.60e-09)

    !NS+ + O -> NO+ + S
    k(1407) = small + (6.10e-10)

    !O2+ + C -> C+ + O2
    k(1408) = small + (5.20e-11)

    !O2+ + C -> CO+ + O
    k(1409) = small + (5.20e-11)

    !O2+ + FE -> FE+ + O2
    k(1410) = small + (1.10e-09)

    !O2+ + MG -> MG+ + O2
    k(1411) = small + (1.20e-09)

    !O2+ + N -> NO+ + O
    k(1412) = small + (1.80e-10)

    !O2+ + NA -> NA+ + O2
    k(1413) = small + (7.10e-10)

    !O2+ + S -> S+ + O2
    k(1414) = small + (5.40e-10)

    !O2+ + S -> SO+ + O
    k(1415) = small + (5.40e-10)

    !O2+ + SI -> SI+ + O2
    k(1416) = small + (1.60e-09)

    !O2+ + C2 -> CO+ + CO
    k(1417) = small + (4.10e-10)

    !O2+ + CH -> CH+ + O2
    k(1418) = small + (3.10e-10)

    !O2+ + CH -> HCO+ + O
    k(1419) = small + (3.10e-10)

    !O2+ + NH -> HNO+ + O
    k(1420) = small + (3.20e-10)

    !O2+ + NH -> NO2+ + H
    k(1421) = small + (3.20e-10)

    !O2+ + NO -> NO+ + O2
    k(1422) = small + (4.50e-10)

    !O2+ + CH2 -> CH2+ + O2
    k(1423) = small + (4.30e-10)

    !O2+ + CH2 -> H2CO+ + O
    k(1424) = small + (4.30e-10)

    !O2+ + H2S -> H2S+ + O2
    k(1425) = small + (1.40e-09)

    !O2+ + HCO -> HCO+ + O2
    k(1426) = small + (3.60e-10)

    !O2+ + HCO -> O2H+ + CO
    k(1427) = small + (3.60e-10)

    !O2+ + NH2 -> NH2+ + O2
    k(1428) = small + (8.70e-10)

    !O2+ + NO2 -> NO2+ + O2
    k(1429) = small + (6.60e-10)

    !O2+ + H2CO -> HCO+ + O2 + H
    k(1430) = small + (2.30e-10)

    !O2+ + H2CO -> H2CO+ + O2
    k(1431) = small + (9.90e-10)

    !O2+ + NH3 -> NH3+ + O2
    k(1432) = small + (2.40e-09)

    !O2+ + CH4 -> CH3O2+ + H
    k(1433) = small + (3.80e-12&
        *(T32)**(-1.80e+00))

    !O2+ + CH4O -> H3CO+ + O2 + H
    k(1434) = small + (5.00e-10)

    !O2+ + CH4O -> CH4O+ + O2
    k(1435) = small + (5.00e-10)

    !OH+ + C -> CH+ + O
    k(1436) = small + (1.20e-09)

    !OH+ + N -> NO+ + H
    k(1437) = small + (8.90e-10)

    !OH+ + O -> O2+ + H
    k(1438) = small + (7.10e-10)

    !OH+ + S -> S+ + OH
    k(1439) = small + (4.30e-10)

    !OH+ + S -> HS+ + O
    k(1440) = small + (4.30e-10)

    !OH+ + S -> SO+ + H
    k(1441) = small + (4.30e-10)

    !OH+ + SI -> SIH+ + O
    k(1442) = small + (1.90e-09)

    !OH+ + C2 -> C2+ + OH
    k(1443) = small + (4.80e-10)

    !OH+ + C2 -> C2H+ + O
    k(1444) = small + (4.80e-10)

    !OH+ + CH -> CH+ + OH
    k(1445) = small + (3.50e-10)

    !OH+ + CH -> CH2+ + O
    k(1446) = small + (3.50e-10)

    !OH+ + CN -> HCN+ + O
    k(1447) = small + (1.00e-09)

    !OH+ + CO -> HCO+ + O
    k(1448) = small + (1.00e-09)

    !OH+ + H2 -> H2O+ + H
    k(1449) = small + (1.10e-09)

    !OH+ + N2 -> N2H+ + O
    k(1450) = small + (3.60e-10)

    !OH+ + NH -> NH2+ + O
    k(1451) = small + (3.60e-10)

    !OH+ + NO -> NO+ + OH
    k(1452) = small + (3.60e-10)

    !OH+ + NO -> HNO+ + O
    k(1453) = small + (6.10e-10)

    !OH+ + O2 -> O2+ + OH
    k(1454) = small + (5.90e-10)

    !OH+ + OH -> H2O+ + O
    k(1455) = small + (7.00e-10)

    !OH+ + SIC -> CHSI+ + O
    k(1456) = small + (4.50e-09)

    !OH+ + SIH -> SIH2+ + O
    k(1457) = small + (1.00e-09)

    !OH+ + SIO -> HSIO+ + O
    k(1458) = small + (9.40e-10)

    !OH+ + C2H -> C2H+ + OH
    k(1459) = small + (4.50e-10)

    !OH+ + C2H -> C2H2+ + O
    k(1460) = small + (4.50e-10)

    !OH+ + CH2 -> CH2+ + OH
    k(1461) = small + (4.80e-10)

    !OH+ + CH2 -> CH3+ + O
    k(1462) = small + (4.80e-10)

    !OH+ + CO2 -> HCO2+ + O
    k(1463) = small + (1.40e-09)

    !OH+ + H2O -> H2O+ + OH
    k(1464) = small + (1.50e-09)

    !OH+ + H2O -> H3O+ + O
    k(1465) = small + (1.30e-09)

    !OH+ + H2S -> H2S+ + OH
    k(1466) = small + (1.20e-09)

    !OH+ + H2S -> H3S+ + O
    k(1467) = small + (8.20e-10)

    !OH+ + HCN -> HCNH+ + O
    k(1468) = small + (1.20e-09)

    !OH+ + HCO -> H2O+ + CO
    k(1469) = small + (2.80e-10)

    !OH+ + HCO -> HCO+ + OH
    k(1470) = small + (2.80e-10)

    !OH+ + HCO -> H2CO+ + O
    k(1471) = small + (2.80e-10)

    !OH+ + HNC -> HCNH+ + O
    k(1472) = small + (1.20e-09)

    !OH+ + NH2 -> NH2+ + OH
    k(1473) = small + (5.00e-10)

    !OH+ + NH2 -> NH3+ + O
    k(1474) = small + (5.00e-10)

    !OH+ + H2CO -> H2CO+ + OH
    k(1475) = small + (7.40e-10)

    !OH+ + H2CO -> H3CO+ + O
    k(1476) = small + (1.10e-09)

    !OH+ + NH3 -> NH3+ + OH
    k(1477) = small + (1.20e-09)

    !OH+ + NH3 -> NH4+ + O
    k(1478) = small + (1.20e-09)

    !OH+ + CH4 -> H3O+ + CH2
    k(1479) = small + (1.40e-09)

    !OH+ + CH4 -> CH5+ + O
    k(1480) = small + (1.95e-10)

    !PH+ + O -> PO+ + H
    k(1481) = small + (1.00e-09)

    !PH+ + O2 -> PO+ + OH
    k(1482) = small + (4.90e-10)

    !PH+ + H2O -> HPO+ + H2
    k(1483) = small + (7.03e-10&
        *(T32)**(-5.00e-01))

    !PH+ + H2O -> H2PO+ + H
    k(1484) = small + (7.03e-10&
        *(T32)**(-5.00e-01))

    !PH+ + H2O -> H3O+ + P
    k(1485) = small + (7.03e-10&
        *(T32)**(-5.00e-01))

    !PH+ + HCN -> HCNH+ + P
    k(1486) = small + (7.15e-09&
        *(T32)**(-5.00e-01))

    !PH+ + C2H2 -> PC2H2+ + H
    k(1487) = small + (1.30e-09)

    !PH+ + NH3 -> PNH2+ + H2
    k(1488) = small + (5.70e-10&
        *(T32)**(-5.00e-01))

    !PH+ + NH3 -> NH4+ + P
    k(1489) = small + (5.70e-10&
        *(T32)**(-5.00e-01))

    !PH+ + NH3 -> PNH3+ + H
    k(1490) = small + (5.70e-10&
        *(T32)**(-5.00e-01))

    !PH+ + CH4 -> PCH3+ + H2
    k(1491) = small + (1.10e-09)

    !PH+ + CH4 -> PCH4+ + H
    k(1492) = small + (5.50e-11)

    !PH+ + C2H4 -> PCH2+ + CH3
    k(1493) = small + (3.60e-10)

    !PH+ + C2H4 -> PC2H3+ + H2
    k(1494) = small + (8.40e-10)

    !S2+ + NO -> NO+ + S2
    k(1495) = small + (5.10e-10)

    !S2+ + C2H5OH -> C2H5O+ + HS2
    k(1496) = small + (1.60e-09)

    !S2+ + C2H5OH -> H2S2+ + C2H4O
    k(1497) = small + (8.50e-11)

    !S2+ + CH3OCH3 -> C2H5O+ + HS2
    k(1498) = small + (1.60e-09)

    !SIC+ + N -> SI+ + CN
    k(1499) = small + (7.70e-10)

    !SIC+ + O -> SIO+ + C
    k(1500) = small + (6.00e-10)

    !SIC+ + H2 -> CHSI+ + H
    k(1501) = small + (1.50e-09)

    !SIH+ + C -> SIC+ + H
    k(1502) = small + (2.00e-10)

    !SIH+ + H -> SI+ + H2
    k(1503) = small + (1.90e-09)

    !SIH+ + N -> SIN+ + H
    k(1504) = small + (2.00e-10)

    !SIH+ + O -> SIO+ + H
    k(1505) = small + (6.10e-10)

    !SIH+ + NH3 -> NH4+ + SI
    k(1506) = small + (1.00e-09)

    !SIN+ + O -> SIO+ + N
    k(1507) = small + (1.00e-09)

    !SIO+ + C -> SI+ + CO
    k(1508) = small + (1.00e-09)

    !SIO+ + FE -> FE+ + SIO
    k(1509) = small + (1.00e-09)

    !SIO+ + MG -> MG+ + SIO
    k(1510) = small + (1.00e-09)

    !SIO+ + N -> SI+ + NO
    k(1511) = small + (2.00e-10)

    !SIO+ + N -> NO+ + SI
    k(1512) = small + (2.00e-10)

    !SIO+ + O -> SI+ + O2
    k(1513) = small + (2.00e-10)

    !SIO+ + S -> SI+ + SO
    k(1514) = small + (1.00e-09)

    !SIO+ + C2 -> SIC+ + CO
    k(1515) = small + (7.60e-10)

    !SIO+ + CH -> HCO+ + SI
    k(1516) = small + (5.90e-10)

    !SIO+ + CO -> SI+ + CO2
    k(1517) = small + (7.90e-10)

    !SIO+ + H2 -> HSIO+ + H
    k(1518) = small + (3.20e-10)

    !SIO+ + NO -> NO+ + SIO
    k(1519) = small + (7.20e-10)

    !SIO+ + CH2 -> SI+ + H2CO
    k(1520) = small + (8.20e-10)

    !SIO+ + HCO -> HCO+ + SIO
    k(1521) = small + (6.60e-10)

    !SIS+ + H -> SI+ + HS
    k(1522) = small + (1.90e-09)

    !SIS+ + H2 -> HSIS+ + H
    k(1523) = small + (1.50e-09)

    !SO+ + FE -> FE+ + SO
    k(1524) = small + (1.60e-09)

    !SO+ + MG -> MG+ + SO
    k(1525) = small + (1.00e-10)

    !SO+ + N -> NS+ + O
    k(1526) = small + (5.00e-11)

    !SO+ + NA -> NA+ + SO
    k(1527) = small + (2.30e-09)

    !SO+ + H2S -> S2+ + H2O
    k(1528) = small + (1.10e-09)

    !SO+ + NH3 -> NH3+ + SO
    k(1529) = small + (1.30e-09)

    !C2H+ + C -> C3+ + H
    k(1530) = small + (1.10e-09)

    !C2H+ + N -> CH+ + CN
    k(1531) = small + (9.00e-11)

    !C2H+ + N -> C2N+ + H
    k(1532) = small + (1.00e-10)

    !C2H+ + O -> HCO+ + C
    k(1533) = small + (3.30e-10)

    !C2H+ + S -> S+ + C2H
    k(1534) = small + (1.20e-09)

    !C2H+ + CH -> CH2+ + C2
    k(1535) = small + (3.20e-10)

    !C2H+ + CH -> C3H+ + H
    k(1536) = small + (3.20e-10)

    !C2H+ + CN -> C3N+ + H
    k(1537) = small + (9.10e-10)

    !C2H+ + H2 -> C2H2+ + H
    k(1538) = small + (1.10e-09)

    !C2H+ + NO -> NO+ + C2H
    k(1539) = small + (1.20e-10)

    !C2H+ + CH2 -> CH3+ + C2
    k(1540) = small + (4.40e-10)

    !C2H+ + CH2 -> C3H2+ + H
    k(1541) = small + (2.20e-10)

    !C2H+ + CH2 -> H2C3+ + H
    k(1542) = small + (2.20e-10)

    !C2H+ + CO2 -> C2HO+ + CO
    k(1543) = small + (9.40e-10)

    !C2H+ + H2O -> C2H2O+ + H
    k(1544) = small + (8.70e-10)

    !C2H+ + HCN -> C2H2+ + CN
    k(1545) = small + (1.40e-09)

    !C2H+ + HCN -> HCNH+ + C2
    k(1546) = small + (9.50e-10)

    !C2H+ + HCN -> C3HN+ + H
    k(1547) = small + (1.80e-09)

    !C2H+ + HCO -> C2H2+ + CO
    k(1548) = small + (7.60e-10)

    !C2H+ + HNC -> HCNH+ + C2
    k(1549) = small + (1.40e-09)

    !C2H+ + NH2 -> NH3+ + C2
    k(1550) = small + (4.60e-10)

    !C2H+ + NH2 -> C2H2N+ + H
    k(1551) = small + (4.60e-10)

    !C2H+ + C2H2 -> C4H2+ + H
    k(1552) = small + (1.70e-09)

    !C2H+ + H2CO -> H3CO+ + C2
    k(1553) = small + (1.10e-09)

    !C2H+ + NH3 -> C2H2N+ + H2
    k(1554) = small + (5.50e-10)

    !C2H+ + NH3 -> NH4+ + C2
    k(1555) = small + (5.50e-10)

    !C2H+ + CH4 -> C2H2+ + CH3
    k(1556) = small + (3.74e-10)

    !C2H+ + CH4 -> C3H3+ + H2
    k(1557) = small + (1.87e-10)

    !C2H+ + CH4 -> H3C3+ + H2
    k(1558) = small + (1.87e-10)

    !C2H+ + CH4 -> C3H4+ + H
    k(1559) = small + (1.32e-10)

    !C2N+ + H2 -> HCNH+ + C
    k(1560) = small + (8.10e-10)

    !C2N+ + CH4 -> C2H3+ + HCN
    k(1561) = small + (4.20e-10)

    !C2N+ + CH4 -> C3H2N+ + H2
    k(1562) = small + (2.10e-10)

    !C2N+ + CH4 -> HCNH+ + C2H2
    k(1563) = small + (7.00e-11)

    !C2N+ + NH3 -> HCNH+ + HCN
    k(1564) = small + (1.90e-09)

    !C2N+ + H2O -> HCO+ + HCN
    k(1565) = small + (1.50e-09)

    !C2N+ + H2O -> HCNH+ + CO
    k(1566) = small + (1.30e-10)

    !C2N+ + C2H2 -> C3H+ + HCN
    k(1567) = small + (1.50e-09)

    !C2N+ + C2H2 -> HCNH+ + C3
    k(1568) = small + (1.30e-10)

    !C2N+ + H2S -> HCS+ + HCN
    k(1569) = small + (1.20e-09)

    !C3+ + H2 -> C3H+ + H
    k(1570) = small + (2.40e-10)

    !C3+ + C2H2 -> C5H+ + H
    k(1571) = small + (6.00e-10)

    !CH2+ + C -> C2H+ + H
    k(1572) = small + (1.20e-09)

    !CH2+ + N -> HCN+ + H
    k(1573) = small + (2.20e-10)

    !CH2+ + O -> HCO+ + H
    k(1574) = small + (7.50e-10)

    !CH2+ + S -> HCS+ + H
    k(1575) = small + (1.40e-09)

    !CH2+ + C2 -> C3H+ + H
    k(1576) = small + (1.00e-09)

    !CH2+ + CH -> C2H2+ + H
    k(1577) = small + (7.20e-10)

    !CH2+ + H2 -> CH3+ + H
    k(1578) = small + (1.20e-09)

    !CH2+ + NH -> HCNH+ + H
    k(1579) = small + (7.50e-10)

    !CH2+ + NO -> NO+ + CH2
    k(1580) = small + (4.20e-10)

    !CH2+ + O2 -> HCO+ + OH
    k(1581) = small + (9.10e-10)

    !CH2+ + O2 -> HCO2+ + H
    k(1582) = small + (4.70e-10)

    !CH2+ + OH -> H2CO+ + H
    k(1583) = small + (7.40e-10)

    !CH2+ + C2H -> C3H2+ + H
    k(1584) = small + (4.75e-10)

    !CH2+ + C2H -> H2C3+ + H
    k(1585) = small + (4.75e-10)

    !CH2+ + CH2 -> C2H3+ + H
    k(1586) = small + (1.00e-09)

    !CH2+ + CO2 -> H2CO+ + CO
    k(1587) = small + (1.20e-09)

    !CH2+ + H2O -> H3CO+ + H
    k(1588) = small + (1.20e-09)

    !CH2+ + H2S -> HCS+ + H2 + H
    k(1589) = small + (1.10e-09)

    !CH2+ + H2S -> H3CS+ + H
    k(1590) = small + (1.60e-09)

    !CH2+ + HCN -> C2H2N+ + H
    k(1591) = small + (1.80e-09)

    !CH2+ + HCO -> CH3+ + CO
    k(1592) = small + (4.50e-10)

    !CH2+ + HCO -> C2H2O+ + H
    k(1593) = small + (4.50e-10)

    !CH2+ + NH2 -> HCNH+ + H2
    k(1594) = small + (1.00e-09)

    !CH2+ + OCS -> HCS+ + HCO
    k(1595) = small + (1.08e-09)

    !CH2+ + OCS -> H2CS+ + CO
    k(1596) = small + (7.20e-10)

    !CH2+ + H2CO -> HCO+ + CH3
    k(1597) = small + (2.80e-09)

    !CH2+ + H2CO -> C2H2O+ + H2
    k(1598) = small + (1.65e-10)

    !CH2+ + H2CO -> C2H3O+ + H
    k(1599) = small + (3.30e-10)

    !CH2+ + NH3 -> NH4+ + CH
    k(1600) = small + (1.30e-09)

    !CH2+ + NH3 -> CH4N+ + H
    k(1601) = small + (1.50e-09)

    !CH2+ + CH4 -> C2H4+ + H2
    k(1602) = small + (9.10e-10)

    !CH2+ + CH4 -> C2H5+ + H
    k(1603) = small + (3.60e-10)

    !CHSI+ + H2 -> CH2SI+ + H
    k(1604) = small + (1.50e-09)

    !CNC+ + NH3 -> HCNH+ + HCN
    k(1605) = small + (1.90e-09)

    !CNC+ + H2O -> HCO+ + HCN
    k(1606) = small + (6.40e-11)

    !CNC+ + C2H2 -> C3H+ + HCN
    k(1607) = small + (6.40e-10)

    !CNC+ + C2H2 -> HCNH+ + C3
    k(1608) = small + (5.60e-11)

    !CO2+ + H -> H+ + CO2
    k(1609) = small + (1.00e-10)

    !CO2+ + H -> HCO+ + O
    k(1610) = small + (2.90e-10)

    !CO2+ + O -> O+ + CO2
    k(1611) = small + (9.62e-11)

    !CO2+ + O -> O2+ + CO
    k(1612) = small + (1.64e-10)

    !CO2+ + H2 -> HCO2+ + H
    k(1613) = small + (8.70e-10)

    !CO2+ + NO -> NO+ + CO2
    k(1614) = small + (1.20e-10)

    !CO2+ + O2 -> O2+ + CO2
    k(1615) = small + (5.00e-11)

    !CO2+ + H2O -> H2O+ + CO2
    k(1616) = small + (2.04e-09)

    !CO2+ + H2O -> HCO2+ + OH
    k(1617) = small + (7.56e-10)

    !CO2+ + H2S -> H2S+ + CO2
    k(1618) = small + (1.40e-09)

    !CO2+ + NH3 -> NH3+ + CO2
    k(1619) = small + (1.98e-09)

    !CO2+ + CH4 -> HCO2+ + CH3
    k(1620) = small + (5.50e-10)

    !CO2+ + CH4 -> CH4+ + CO2
    k(1621) = small + (5.50e-10)

    !H2CL+ + CO -> HCO+ + HCL
    k(1622) = small + (7.80e-10)

    !H2CL+ + H2O -> H3O+ + HCL
    k(1623) = small + (2.00e-09)

    !H2O+ + C -> CH+ + OH
    k(1624) = small + (1.10e-09)

    !H2O+ + FE -> FE+ + H2O
    k(1625) = small + (1.50e-09)

    !H2O+ + MG -> MG+ + H2O
    k(1626) = small + (2.20e-09)

    !H2O+ + N -> HNO+ + H
    k(1627) = small + (1.90e-10)

    !H2O+ + NA -> NA+ + H2O
    k(1628) = small + (2.70e-09)

    !H2O+ + O -> O2+ + H2
    k(1629) = small + (4.00e-11)

    !H2O+ + S -> S+ + H2O
    k(1630) = small + (4.30e-10)

    !H2O+ + S -> HS+ + OH
    k(1631) = small + (4.30e-10)

    !H2O+ + S -> HSO+ + H
    k(1632) = small + (4.30e-10)

    !H2O+ + SI -> SI+ + H2O
    k(1633) = small + (3.00e-09)

    !H2O+ + C2 -> C2+ + H2O
    k(1634) = small + (4.70e-10)

    !H2O+ + C2 -> C2H+ + OH
    k(1635) = small + (4.70e-10)

    !H2O+ + CH -> CH+ + H2O
    k(1636) = small + (3.40e-10)

    !H2O+ + CH -> CH2+ + OH
    k(1637) = small + (3.40e-10)

    !H2O+ + CO -> HCO+ + OH
    k(1638) = small + (5.00e-10)

    !H2O+ + H2 -> H3O+ + H
    k(1639) = small + (6.10e-10)

    !H2O+ + NH -> H3O+ + N
    k(1640) = small + (7.10e-10)

    !H2O+ + NO -> NO+ + H2O
    k(1641) = small + (1.20e-09)

    !H2O+ + O2 -> O2+ + H2O
    k(1642) = small + (4.30e-10)

    !H2O+ + OH -> H3O+ + O
    k(1643) = small + (6.90e-10)

    !H2O+ + C2H -> C2H+ + H2O
    k(1644) = small + (4.40e-10)

    !H2O+ + C2H -> C2H2+ + OH
    k(1645) = small + (4.40e-10)

    !H2O+ + CH2 -> CH2+ + H2O
    k(1646) = small + (4.70e-10)

    !H2O+ + CH2 -> CH3+ + OH
    k(1647) = small + (4.70e-10)

    !H2O+ + H2O -> H3O+ + OH
    k(1648) = small + (2.10e-09)

    !H2O+ + H2S -> H2S+ + H2O
    k(1649) = small + (8.90e-10)

    !H2O+ + H2S -> H3O+ + HS
    k(1650) = small + (5.90e-10)

    !H2O+ + H2S -> H3S+ + OH
    k(1651) = small + (7.00e-10)

    !H2O+ + HCN -> HCNH+ + OH
    k(1652) = small + (2.10e-09)

    !H2O+ + HCO -> HCO+ + H2O
    k(1653) = small + (2.80e-10)

    !H2O+ + HCO -> H2CO+ + OH
    k(1654) = small + (2.80e-10)

    !H2O+ + HCO -> H3O+ + CO
    k(1655) = small + (2.80e-10)

    !H2O+ + HNC -> HCNH+ + OH
    k(1656) = small + (1.10e-09)

    !H2O+ + NH2 -> NH2+ + H2O
    k(1657) = small + (4.90e-10)

    !H2O+ + NH2 -> NH3+ + OH
    k(1658) = small + (4.90e-10)

    !H2O+ + H2CO -> H2CO+ + H2O
    k(1659) = small + (1.40e-09)

    !H2O+ + H2CO -> H3CO+ + OH
    k(1660) = small + (6.60e-10)

    !H2O+ + NH3 -> NH3+ + H2O
    k(1661) = small + (2.20e-09)

    !H2O+ + NH3 -> NH4+ + OH
    k(1662) = small + (9.00e-10)

    !H2O+ + CH4 -> H3O+ + CH3
    k(1663) = small + (1.30e-09)

    !H2S+ + C -> HCS+ + H
    k(1664) = small + (1.00e-09)

    !H2S+ + FE -> FE+ + H2S
    k(1665) = small + (1.80e-09)

    !H2S+ + H -> HS+ + H2
    k(1666) = small + (2.00e-10)

    !H2S+ + MG -> MG+ + H2S
    k(1667) = small + (2.80e-09)

    !H2S+ + N -> NS+ + H2
    k(1668) = small + (7.90e-10)

    !H2S+ + NA -> NA+ + H2S
    k(1669) = small + (2.50e-09)

    !H2S+ + O -> HS+ + OH
    k(1670) = small + (3.10e-10)

    !H2S+ + O -> SO+ + H2
    k(1671) = small + (3.10e-10)

    !H2S+ + S -> S+ + H2S
    k(1672) = small + (1.10e-09)

    !H2S+ + SI -> SI+ + H2S
    k(1673) = small + (1.60e-09)

    !H2S+ + NO -> NO+ + H2S
    k(1674) = small + (3.70e-10)

    !H2S+ + H2O -> H3O+ + HS
    k(1675) = small + (7.00e-10)

    !H2S+ + H2S -> H3S+ + HS
    k(1676) = small + (5.80e-10)

    !H2S+ + HCO -> HCO+ + H2S
    k(1677) = small + (7.00e-10)

    !H2S+ + NH3 -> NH3+ + H2S
    k(1678) = small + (5.60e-10)

    !H2S+ + NH3 -> NH4+ + HS
    k(1679) = small + (1.30e-09)

    !H3+ + C -> CH+ + H2
    k(1680) = small + (2.00e-09)

    !H3+ + CL -> HCL+ + H2
    k(1681) = small + (1.00e-09)

    !H3+ + FE -> FE+ + H + H2
    k(1682) = small + (4.90e-09)

    !H3+ + MG -> MG+ + H + H2
    k(1683) = small + (1.00e-09)

    !H3+ + N -> NH2+ + H
    k(1684) = small + (1.00e-17)

    !H3+ + NA -> NA+ + H + H2
    k(1685) = small + (1.10e-09)

    !H3+ + O -> OH+ + H2
    k(1686) = small + (8.00e-10)

    !H3+ + P -> PH+ + H2
    k(1687) = small + (1.00e-09)

    !H3+ + S -> HS+ + H2
    k(1688) = small + (2.60e-09)

    !H3+ + SI -> SIH+ + H2
    k(1689) = small + (3.70e-09)

    !H3+ + C2 -> C2H+ + H2
    k(1690) = small + (1.80e-09)

    !H3+ + CH -> CH2+ + H2
    k(1691) = small + (8.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CN -> HCN+ + H2
    k(1692) = small + (8.10e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CO -> HCO+ + H2
    k(1693) = small + (1.61e-09)

    !H3+ + CO -> HOC+ + H2
    k(1694) = small + (9.44e-11)

    !H3+ + CP -> HCP+ + H2
    k(1695) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CS -> HCS+ + H2
    k(1696) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HCL -> H2CL+ + H2
    k(1697) = small + (5.93e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HS -> H2S+ + H2
    k(1698) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !H3+ + MGH -> MG+ + H2 + H2
    k(1699) = small + (7.20e-09&
        *(T32)**(-5.00e-01))

    !H3+ + N2 -> N2H+ + H2
    k(1700) = small + (1.70e-09)

    !H3+ + NAH -> NA+ + H2 + H2
    k(1701) = small + (3.80e-08&
        *(T32)**(-5.00e-01))

    !H3+ + NH -> NH2+ + H2
    k(1702) = small + (7.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + NO -> HNO+ + H2
    k(1703) = small + (8.50e-10&
        *(T32)**(-5.00e-01))

    !H3+ + NS -> HNS+ + H2
    k(1704) = small + (9.90e-09&
        *(T32)**(-5.00e-01))

    !H3+ + O2 -> O2H+ + H2
    k(1705) = small + (6.40e-10)

    !H3+ + OH -> H2O+ + H2
    k(1706) = small + (9.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + PH -> PH2+ + H2
    k(1707) = small + (3.54e-09&
        *(T32)**(-5.00e-01))

    !H3+ + PN -> HPN+ + H2
    k(1708) = small + (1.50e-08&
        *(T32)**(-5.00e-01))

    !H3+ + PO -> HPO+ + H2
    k(1709) = small + (1.02e-08&
        *(T32)**(-5.00e-01))

    !H3+ + S2 -> S2H+ + H2
    k(1710) = small + (2.00e-09)

    !H3+ + SIC -> CHSI+ + H2
    k(1711) = small + (9.31e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIH -> SIH2+ + H2
    k(1712) = small + (6.66e-10&
        *(T32)**(-5.00e-01))

    !H3+ + SIN -> HNSI+ + H2
    k(1713) = small + (1.26e-08&
        *(T32)**(-5.00e-01))

    !H3+ + SIO -> HSIO+ + H2
    k(1714) = small + (1.70e-08&
        *(T32)**(-5.00e-01))

    !H3+ + SIS -> HSIS+ + H2
    k(1715) = small + (9.40e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SO -> HSO+ + H2
    k(1716) = small + (8.40e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H -> C2H2+ + H2
    k(1717) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2N -> C2NH+ + H2
    k(1718) = small + (3.29e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2S -> HC2S+ + H2
    k(1719) = small + (6.41e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C3 -> C3H+ + H2
    k(1720) = small + (2.00e-09)

    !H3+ + CCO -> C2HO+ + H2
    k(1721) = small + (7.12e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CCP -> PC2H+ + H2
    k(1722) = small + (5.42e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH2 -> CH3+ + H2
    k(1723) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CO2 -> HCO2+ + H2
    k(1724) = small + (1.90e-09)

    !H3+ + H2O -> H3O+ + H2
    k(1725) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + H2S -> H3S+ + H2
    k(1726) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HCN -> HCNH+ + H2
    k(1727) = small + (1.70e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HCO -> H2CO+ + H2
    k(1728) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HCP -> PCH2+ + H2
    k(1729) = small + (2.13e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HCS -> H2CS+ + H2
    k(1730) = small + (1.09e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HCSI -> CH2SI+ + H2
    k(1731) = small + (5.47e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HNC -> HCNH+ + H2
    k(1732) = small + (1.50e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HNO -> H2NO+ + H2
    k(1733) = small + (4.00e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HNSI -> SINH2+ + H2
    k(1734) = small + (8.74e-10&
        *(T32)**(-5.00e-01))

    !H3+ + HPO -> H2PO+ + H2
    k(1735) = small + (1.27e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HS2 -> H2S2+ + H2
    k(1736) = small + (4.56e-09&
        *(T32)**(-5.00e-01))

    !H3+ + NAOH -> NAH2O+ + H2
    k(1737) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !H3+ + NH2 -> NH3+ + H2
    k(1738) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + NO2 -> NO+ + H2 + OH
    k(1739) = small + (7.28e-10&
        *(T32)**(-5.00e-01))

    !H3+ + OCS -> HOCS+ + H2
    k(1740) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !H3+ + PH2 -> PH3+ + H2
    k(1741) = small + (1.17e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC2 -> SIC2H+ + H2
    k(1742) = small + (5.48e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIH2 -> SIH3+ + H2
    k(1743) = small + (4.21e-10&
        *(T32)**(-5.00e-01))

    !H3+ + SINC -> SINCH+ + H2
    k(1744) = small + (4.65e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIO2 -> HSIO2+ + H2
    k(1745) = small + (1.14e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SO2 -> HSO2+ + H2
    k(1746) = small + (3.72e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H2 -> C2H3+ + H2
    k(1747) = small + (3.50e-09)

    !H3+ + C3H -> C3H2+ + H2
    k(1748) = small + (1.48e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C3N -> C3HN+ + H2
    k(1749) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C3O -> HC3O+ + H2
    k(1750) = small + (1.30e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C3P -> PC3H+ + H2
    k(1751) = small + (2.28e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C3S -> HC3S+ + H2
    k(1752) = small + (8.89e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4 -> C4H+ + H2
    k(1753) = small + (2.00e-09)

    !H3+ + CH3 -> CH4+ + H2
    k(1754) = small + (2.10e-09)

    !H3+ + H2CO -> H3CO+ + H2
    k(1755) = small + (5.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + H2CS -> H3CS+ + H2
    k(1756) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !H3+ + H2S2 -> H3S2+ + H2
    k(1757) = small + (2.74e-09&
        *(T32)**(-5.00e-01))

    !H3+ + H2SIO -> H3SIO+ + H2
    k(1758) = small + (4.61e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HCCP -> PC2H2+ + H2
    k(1759) = small + (1.00e-09)

    !H3+ + NH3 -> NH4+ + H2
    k(1760) = small + (3.60e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC2H -> SIC2H2+ + H2
    k(1761) = small + (3.21e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC3 -> SIC3H+ + H2
    k(1762) = small + (4.57e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SICH2 -> SICH3+ + H2
    k(1763) = small + (3.46e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIH3 -> SIH4+ + H2
    k(1764) = small + (2.00e-09)

    !H3+ + C2H2N -> C2H3N+ + H2
    k(1765) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H2O -> C2H3O+ + H2
    k(1766) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H3 -> C2H4+ + H2
    k(1767) = small + (3.50e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C3H2 -> C3H3+ + H2
    k(1768) = small + (6.98e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4H -> C4H2+ + H2
    k(1769) = small + (4.90e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4P -> PC4H+ + H2
    k(1770) = small + (1.14e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4S -> HC4S+ + H2
    k(1771) = small + (6.82e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C5 -> C5H+ + H2
    k(1772) = small + (2.00e-09)

    !H3+ + CH2O2 -> HCO+ + H2O + H2
    k(1773) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH2O2 -> H3O+ + CO + H2
    k(1774) = small + (9.70e-10&
        *(T32)**(-5.00e-01))

    !H3+ + CH2PH -> PCH4+ + H2
    k(1775) = small + (1.15e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH3N -> CH4N+ + H2
    k(1776) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH4 -> CH5+ + H2
    k(1777) = small + (2.40e-09)

    !H3+ + HCNC2 -> HC2NCH+ + H2
    k(1778) = small + (4.40e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HC2NC -> HC2NCH+ + H2
    k(1779) = small + (1.28e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HC3N -> C3H2N+ + H2
    k(1780) = small + (2.00e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HNC3 -> C3H2N+ + H2
    k(1781) = small + (1.14e-08&
        *(T32)**(-5.00e-01))

    !H3+ + NH2CN -> NH2CNH+ + H2
    k(1782) = small + (9.86e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC2H2 -> SIC2H3+ + H2
    k(1783) = small + (5.73e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC3H -> SIC3H2+ + H2
    k(1784) = small + (6.85e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIC4 -> SIC4H+ + H2
    k(1785) = small + (1.43e-08&
        *(T32)**(-5.00e-01))

    !H3+ + SICH3 -> SICH4+ + H2
    k(1786) = small + (3.46e-09&
        *(T32)**(-5.00e-01))

    !H3+ + SIH4 -> SIH5+ + H2
    k(1787) = small + (2.00e-09)

    !H3+ + C2H3N -> C2H4N+ + H2
    k(1788) = small + (9.10e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H4 -> C2H3+ + H2 + H2
    k(1789) = small + (1.20e-10)

    !H3+ + C2H4 -> C2H5+ + H2
    k(1790) = small + (1.90e-09)

    !H3+ + C3H3 -> C3H4+ + H2
    k(1791) = small + (9.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4H2 -> C4H3+ + H2
    k(1792) = small + (2.00e-09)

    !H3+ + C5H -> C5H2+ + H2
    k(1793) = small + (2.33e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C5N -> C5HN+ + H2
    k(1794) = small + (1.45e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C6 -> C6H+ + H2
    k(1795) = small + (2.00e-09)

    !H3+ + CH4O -> CH3+ + H2O + H2
    k(1796) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH4O -> H3CO+ + H2 + H2
    k(1797) = small + (1.12e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH4O -> CH5O+ + H2
    k(1798) = small + (1.04e-09&
        *(T32)**(-5.00e-01))

    !H3+ + NH2CHO -> NH2CH2O+ + H2
    k(1799) = small + (2.00e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C2H4O -> C2H5O+ + H2
    k(1800) = small + (6.20e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H5 -> C2H6+ + H2
    k(1801) = small + (2.34e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C3H3N -> C3H4N+ + H2
    k(1802) = small + (8.90e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C3H4 -> C3H3+ + H2 + H2
    k(1803) = small + (9.00e-11&
        *(T32)**(-5.00e-01))

    !H3+ + C3H4 -> H3C3+ + H2 + H2
    k(1804) = small + (9.00e-11&
        *(T32)**(-5.00e-01))

    !H3+ + C3H4 -> C3H5+ + H2
    k(1805) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C4H3 -> C4H4+ + H2
    k(1806) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C5H2 -> C5H3+ + H2
    k(1807) = small + (5.71e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C6H -> C6H2+ + H2
    k(1808) = small + (2.69e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C7 -> C7H+ + H2
    k(1809) = small + (2.00e-09)

    !H3+ + CH5N -> CH6N+ + H2
    k(1810) = small + (3.10e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HC5N -> C5H2N+ + H2
    k(1811) = small + (2.30e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C6H2 -> C6H3+ + H2
    k(1812) = small + (2.00e-09)

    !H3+ + C7H -> C7H2+ + H2
    k(1813) = small + (2.42e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C7N -> C7HN+ + H2
    k(1814) = small + (1.61e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C8 -> C8H+ + H2
    k(1815) = small + (2.00e-09)

    !H3+ + CH3C3N -> C4H4N+ + H2
    k(1816) = small + (1.08e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HCOOCH3 -> H5C2O2+ + H2
    k(1817) = small + (4.05e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H5OH -> H3O+ + C2H4 + H2
    k(1818) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H5OH -> C2H5+ + H2O + H2
    k(1819) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C2H5OH -> C2H5OH2+ + H2
    k(1820) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C7H2 -> C7H3+ + H2
    k(1821) = small + (5.68e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C8H -> C8H2+ + H2
    k(1822) = small + (2.68e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C9 -> C9H+ + H2
    k(1823) = small + (2.00e-09)

    !H3+ + CH3C4H -> C5H5+ + H2
    k(1824) = small + (2.76e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH3OCH3 -> CH3OCH4+ + H2
    k(1825) = small + (3.00e-09&
        *(T32)**(-5.00e-01))

    !H3+ + HC7N -> C7H2N+ + H2
    k(1826) = small + (2.48e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C2H6CO -> C3H6OH+ + H2
    k(1827) = small + (6.41e-09&
        *(T32)**(-5.00e-01))

    !H3+ + C8H2 -> C8H3+ + H2
    k(1828) = small + (2.00e-09)

    !H3+ + C9H -> C9H2+ + H2
    k(1829) = small + (2.52e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C9N -> C9HN+ + H2
    k(1830) = small + (1.76e-08&
        *(T32)**(-5.00e-01))

    !H3+ + CH3C5N -> C6H4N+ + H2
    k(1831) = small + (1.19e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C10 -> C10H+ + H2
    k(1832) = small + (2.00e-09)

    !H3+ + C9H2 -> C9H3+ + H2
    k(1833) = small + (5.65e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH3C6H -> C7H5+ + H2
    k(1834) = small + (3.40e-09&
        *(T32)**(-5.00e-01))

    !H3+ + CH3C7N -> C8H4N+ + H2
    k(1835) = small + (1.24e-08&
        *(T32)**(-5.00e-01))

    !H3+ + HC9N -> C9H2N+ + H2
    k(1836) = small + (2.59e-08&
        *(T32)**(-5.00e-01))

    !H3+ + C6H6 -> C6H7+ + H2
    k(1837) = small + (3.90e-09)

    !HCN+ + C -> CH+ + CN
    k(1838) = small + (1.10e-09)

    !HCN+ + H -> H+ + HCN
    k(1839) = small + (3.70e-11)

    !HCN+ + O -> O+ + HCN
    k(1840) = small + (6.50e-11)

    !HCN+ + S -> S+ + HCN
    k(1841) = small + (5.70e-10)

    !HCN+ + S -> HS+ + CN
    k(1842) = small + (5.70e-10)

    !HCN+ + C2 -> C2H+ + CN
    k(1843) = small + (8.40e-10)

    !HCN+ + CH -> CH2+ + CN
    k(1844) = small + (6.30e-10)

    !HCN+ + CO -> HCO+ + CN
    k(1845) = small + (1.40e-10)

    !HCN+ + H2 -> HCNH+ + H
    k(1846) = small + (9.00e-10)

    !HCN+ + NH -> NH2+ + CN
    k(1847) = small + (6.50e-10)

    !HCN+ + NO -> NO+ + HCN
    k(1848) = small + (8.10e-10)

    !HCN+ + O2 -> O2+ + HCN
    k(1849) = small + (3.20e-10)

    !HCN+ + OH -> H2O+ + CN
    k(1850) = small + (6.30e-10)

    !HCN+ + C2H -> C2H2+ + CN
    k(1851) = small + (7.90e-10)

    !HCN+ + CH2 -> CH3+ + CN
    k(1852) = small + (8.70e-10)

    !HCN+ + CO2 -> HCO2+ + CN
    k(1853) = small + (2.10e-10)

    !HCN+ + H2O -> H2O+ + HCN
    k(1854) = small + (1.80e-09)

    !HCN+ + H2O -> H3O+ + CN
    k(1855) = small + (8.50e-10)

    !HCN+ + HCN -> HCNH+ + CN
    k(1856) = small + (1.60e-09)

    !HCN+ + HCO -> HCNH+ + CO
    k(1857) = small + (3.70e-10)

    !HCN+ + HCO -> H2CO+ + CN
    k(1858) = small + (3.70e-10)

    !HCN+ + HNC -> HCNH+ + CN
    k(1859) = small + (1.00e-09)

    !HCN+ + NH2 -> NH3+ + CN
    k(1860) = small + (9.00e-10)

    !HCN+ + H2CO -> H3CO+ + CN
    k(1861) = small + (1.00e-09)

    !HCN+ + NH3 -> HCNH+ + NH2
    k(1862) = small + (8.40e-10)

    !HCN+ + NH3 -> NH3+ + HCN
    k(1863) = small + (1.70e-09)

    !HCN+ + CH4 -> HCNH+ + CH3
    k(1864) = small + (1.04e-09)

    !HCN+ + CH4 -> C2H3+ + NH2
    k(1865) = small + (2.60e-10)

    !HCO+ + C -> CH+ + CO
    k(1866) = small + (1.10e-09)

    !HCO+ + FE -> FE+ + HCO
    k(1867) = small + (1.90e-09)

    !HCO+ + MG -> MG+ + HCO
    k(1868) = small + (2.90e-09)

    !HCO+ + NA -> NA+ + HCO
    k(1869) = small + (2.60e-09)

    !HCO+ + P -> PH+ + CO
    k(1870) = small + (1.00e-09)

    !HCO+ + S -> HS+ + CO
    k(1871) = small + (3.30e-10)

    !HCO+ + SI -> SIH+ + CO
    k(1872) = small + (1.60e-09)

    !HCO+ + C2 -> C2H+ + CO
    k(1873) = small + (8.30e-10)

    !HCO+ + CH -> CH2+ + CO
    k(1874) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CP -> HCP+ + CO
    k(1875) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CS -> HCS+ + CO
    k(1876) = small + (4.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HS -> H2S+ + CO
    k(1877) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + MGH -> MG+ + CO + H2
    k(1878) = small + (3.20e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NAH -> NA+ + CO + H2
    k(1879) = small + (1.70e-08&
        *(T32)**(-5.00e-01))

    !HCO+ + NH -> NH2+ + CO
    k(1880) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NS -> HNS+ + CO
    k(1881) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + OH -> H2O+ + CO
    k(1882) = small + (2.33e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + OH -> HCO2+ + H
    k(1883) = small + (2.33e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + PH -> PH2+ + CO
    k(1884) = small + (1.51e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + PN -> HPN+ + CO
    k(1885) = small + (6.01e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + PO -> HPO+ + CO
    k(1886) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + S2 -> S2H+ + CO
    k(1887) = small + (2.00e-09)

    !HCO+ + SIC -> CHSI+ + CO
    k(1888) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIH -> SIH2+ + CO
    k(1889) = small + (2.89e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + SIO -> HSIO+ + CO
    k(1890) = small + (6.80e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIS -> HSIS+ + CO
    k(1891) = small + (3.60e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SO -> HSO+ + CO
    k(1892) = small + (3.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H -> C2H2+ + CO
    k(1893) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2S -> HC2S+ + CO
    k(1894) = small + (2.48e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3 -> C3H+ + CO
    k(1895) = small + (1.40e-09)

    !HCO+ + CCO -> C2HO+ + CO
    k(1896) = small + (2.91e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CCP -> PC2H+ + CO
    k(1897) = small + (2.11e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CH2 -> CH3+ + CO
    k(1898) = small + (7.19e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + H2O -> H3O+ + CO
    k(1899) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + H2S -> H3S+ + CO
    k(1900) = small + (9.50e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + HCN -> HCNH+ + CO
    k(1901) = small + (7.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HCO -> H2CO+ + CO
    k(1902) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HCP -> PCH2+ + CO
    k(1903) = small + (8.56e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + HCSI -> CH2SI+ + CO
    k(1904) = small + (2.23e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HNC -> HCNH+ + CO
    k(1905) = small + (6.63e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HNO -> H2NO+ + CO
    k(1906) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HNSI -> SINH2+ + CO
    k(1907) = small + (3.53e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + HPO -> H2PO+ + CO
    k(1908) = small + (5.03e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HS2 -> H2S2+ + CO
    k(1909) = small + (1.73e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NAOH -> NAH2O+ + CO
    k(1910) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NH2 -> NH3+ + CO
    k(1911) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + OCS -> HOCS+ + CO
    k(1912) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + PH2 -> PH3+ + CO
    k(1913) = small + (4.94e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC2 -> SIC2H+ + CO
    k(1914) = small + (2.15e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIH2 -> SIH3+ + CO
    k(1915) = small + (1.82e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + SINC -> SINCH+ + CO
    k(1916) = small + (1.81e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIO2 -> HSIO2+ + CO
    k(1917) = small + (4.38e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H2 -> C2H3+ + CO
    k(1918) = small + (1.40e-09)

    !HCO+ + C3H -> C3H2+ + CO
    k(1919) = small + (6.20e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3N -> C3HN+ + CO
    k(1920) = small + (7.06e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3O -> HC3O+ + CO
    k(1921) = small + (5.08e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3P -> PC3H+ + CO
    k(1922) = small + (8.62e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C3S -> HC3S+ + CO
    k(1923) = small + (3.35e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C4 -> C4H+ + CO
    k(1924) = small + (1.40e-09)

    !HCO+ + H2CO -> H3CO+ + CO
    k(1925) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + H2CS -> H3CS+ + CO
    k(1926) = small + (1.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + H2S2 -> H3S2+ + CO
    k(1927) = small + (1.04e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + H2SIO -> H3SIO+ + CO
    k(1928) = small + (1.84e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HCCP -> PC2H2+ + CO
    k(1929) = small + (1.00e-09)

    !HCO+ + NH3 -> NH4+ + CO
    k(1930) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC2H -> SIC2H2+ + CO
    k(1931) = small + (1.25e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC3 -> SIC3H+ + CO
    k(1932) = small + (1.74e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SICH2 -> SICH3+ + CO
    k(1933) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H2O -> C2H3O+ + CO
    k(1934) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H3 -> C2H4+ + CO
    k(1935) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3H2 -> C3H3+ + CO
    k(1936) = small + (2.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C4H -> C4H2+ + CO
    k(1937) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C4P -> PC4H+ + CO
    k(1938) = small + (4.21e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C4S -> HC4S+ + CO
    k(1939) = small + (2.52e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C5 -> C5H+ + CO
    k(1940) = small + (1.40e-09)

    !HCO+ + CH2O2 -> CH3O2+ + CO
    k(1941) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CH2PH -> PCH4+ + CO
    k(1942) = small + (4.60e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + CH3N -> CH4N+ + CO
    k(1943) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HCNC2 -> HC2NCH+ + CO
    k(1944) = small + (1.75e-08&
        *(T32)**(-5.00e-01))

    !HCO+ + HC2NC -> HC2NCH+ + CO
    k(1945) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HC3N -> C3H2N+ + CO
    k(1946) = small + (7.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HNC3 -> C3H2N+ + CO
    k(1947) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NH2CN -> NH2CNH+ + CO
    k(1948) = small + (4.00e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC2H2 -> SIC2H3+ + CO
    k(1949) = small + (2.23e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC3H -> SIC3H2+ + CO
    k(1950) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIC4 -> SIC4H+ + CO
    k(1951) = small + (5.33e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SICH3 -> SICH4+ + CO
    k(1952) = small + (1.40e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + SIH4 -> SIH5+ + CO
    k(1953) = small + (1.40e-09)

    !HCO+ + C2H3N -> C2H4N+ + CO
    k(1954) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H4 -> C2H5+ + CO
    k(1955) = small + (1.40e-09)

    !HCO+ + C3H3 -> C3H4+ + CO
    k(1956) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C4H2 -> C4H3+ + CO
    k(1957) = small + (1.40e-09)

    !HCO+ + C5H -> C5H2+ + CO
    k(1958) = small + (8.90e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C5N -> C5HN+ + CO
    k(1959) = small + (6.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C6 -> C6H+ + CO
    k(1960) = small + (1.40e-09)

    !HCO+ + CH4O -> CH5O+ + CO
    k(1961) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + NH2CHO -> NH2CH2O+ + CO
    k(1962) = small + (8.15e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H4O -> C2H5O+ + CO
    k(1963) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H5 -> C2H6+ + CO
    k(1964) = small + (1.02e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3H3N -> C3H4N+ + CO
    k(1965) = small + (3.50e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C3H4 -> C3H5+ + CO
    k(1966) = small + (7.40e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C4H3 -> C4H4+ + CO
    k(1967) = small + (9.00e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C5H2 -> C5H3+ + CO
    k(1968) = small + (2.18e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C6H -> C6H2+ + CO
    k(1969) = small + (1.01e-08&
        *(T32)**(-5.00e-01))

    !HCO+ + C7 -> C7H+ + CO
    k(1970) = small + (1.40e-09)

    !HCO+ + CH5N -> CH6N+ + CO
    k(1971) = small + (1.30e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HC5N -> C5H2N+ + CO
    k(1972) = small + (8.70e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C6H2 -> C6H3+ + CO
    k(1973) = small + (1.40e-09)

    !HCO+ + C7H -> C7H2+ + CO
    k(1974) = small + (8.88e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C7N -> C7HN+ + CO
    k(1975) = small + (5.82e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C8 -> C8H+ + CO
    k(1976) = small + (1.40e-09)

    !HCO+ + CH3C3N -> C4H4N+ + CO
    k(1977) = small + (4.11e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HCOOCH3 -> H5C2O2+ + CO
    k(1978) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H5OH -> H3O+ + C2H4 + CO
    k(1979) = small + (7.00e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H5OH -> C2H5OH2+ + CO
    k(1980) = small + (8.50e-10&
        *(T32)**(-5.00e-01))

    !HCO+ + C7H2 -> C7H3+ + CO
    k(1981) = small + (2.08e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C8H -> C8H2+ + CO
    k(1982) = small + (9.71e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C9 -> C9H+ + CO
    k(1983) = small + (1.40e-09)

    !HCO+ + CH3C4H -> C5H5+ + CO
    k(1984) = small + (1.05e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CH3OCH3 -> CH3OCH4+ + CO
    k(1985) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HC7N -> C7H2N+ + CO
    k(1986) = small + (8.95e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C2H6CO -> C3H6OH+ + CO
    k(1987) = small + (2.47e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C8H2 -> C8H3+ + CO
    k(1988) = small + (1.40e-09)

    !HCO+ + C9H -> C9H2+ + CO
    k(1989) = small + (9.01e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C9N -> C9HN+ + CO
    k(1990) = small + (6.26e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C10 -> C10H+ + CO
    k(1991) = small + (1.40e-09)

    !HCO+ + CH3C5N -> C6H4N+ + CO
    k(1992) = small + (4.34e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C9H2 -> C9H3+ + CO
    k(1993) = small + (2.02e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CH3C6H -> C7H5+ + CO
    k(1994) = small + (1.25e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + CH3C7N -> C8H4N+ + CO
    k(1995) = small + (4.42e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + HC9N -> C9H2N+ + CO
    k(1996) = small + (9.17e-09&
        *(T32)**(-5.00e-01))

    !HCO+ + C6H6 -> C6H7+ + CO
    k(1997) = small + (1.60e-09)

    !HCP+ + C -> CCP+ + H
    k(1998) = small + (2.00e-10)

    !HCP+ + O -> PH+ + CO
    k(1999) = small + (2.00e-10)

    !HCP+ + H2 -> PCH2+ + H
    k(2000) = small + (1.00e-09)

    !HCS+ + O -> HCO+ + S
    k(2001) = small + (5.00e-10)

    !HCS+ + O -> OCS+ + H
    k(2002) = small + (5.00e-10)

    !HCS+ + NH3 -> NH4+ + CS
    k(2003) = small + (1.62e-09&
        *(T32)**(-5.00e-01))

    !HNC+ + C -> CH+ + CN
    k(2004) = small + (1.10e-09)

    !HNC+ + S -> S+ + HNC
    k(2005) = small + (5.70e-10)

    !HNC+ + S -> HS+ + CN
    k(2006) = small + (5.70e-10)

    !HNC+ + C2 -> C2H+ + CN
    k(2007) = small + (8.40e-10)

    !HNC+ + CH -> CH2+ + CN
    k(2008) = small + (6.30e-10)

    !HNC+ + H2 -> HCNH+ + H
    k(2009) = small + (7.00e-10)

    !HNC+ + NH -> NH2+ + CN
    k(2010) = small + (6.50e-10)

    !HNC+ + NO -> NO+ + HNC
    k(2011) = small + (8.10e-10)

    !HNC+ + O2 -> NO+ + HCO
    k(2012) = small + (9.00e-11)

    !HNC+ + OH -> H2O+ + CN
    k(2013) = small + (6.30e-10)

    !HNC+ + C2H -> C2H2+ + CN
    k(2014) = small + (7.90e-10)

    !HNC+ + CH2 -> CH3+ + CN
    k(2015) = small + (8.70e-10)

    !HNC+ + H2O -> H3O+ + CN
    k(2016) = small + (8.50e-10)

    !HNC+ + HCN -> HCNH+ + CN
    k(2017) = small + (1.60e-09)

    !HNC+ + HCO -> HCNH+ + CO
    k(2018) = small + (3.70e-10)

    !HNC+ + HCO -> H2CO+ + CN
    k(2019) = small + (3.70e-10)

    !HNC+ + NH2 -> NH3+ + CN
    k(2020) = small + (9.00e-10)

    !HNC+ + H2CO -> H3CO+ + CN
    k(2021) = small + (1.00e-09)

    !HNC+ + NH3 -> NH3+ + HNC
    k(2022) = small + (1.70e-09)

    !HNO+ + C -> CH+ + NO
    k(2023) = small + (1.00e-09)

    !HNO+ + O -> NO2+ + H
    k(2024) = small + (1.00e-12)

    !HNO+ + S -> HS+ + NO
    k(2025) = small + (1.10e-09)

    !HNO+ + C2 -> C2H+ + NO
    k(2026) = small + (8.20e-10)

    !HNO+ + CH -> CH2+ + NO
    k(2027) = small + (4.40e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + CO -> HCO+ + NO
    k(2028) = small + (2.70e-10&
        *(T32)**(-5.00e-01))

    !HNO+ + NH -> NH2+ + NO
    k(2029) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + NO -> NO+ + HNO
    k(2030) = small + (3.60e-10&
        *(T32)**(-5.00e-01))

    !HNO+ + OH -> H2O+ + NO
    k(2031) = small + (4.60e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + C2H -> C2H2+ + NO
    k(2032) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + CH2 -> CH3+ + NO
    k(2033) = small + (7.11e-10&
        *(T32)**(-5.00e-01))

    !HNO+ + CO2 -> HCO2+ + NO
    k(2034) = small + (1.00e-10)

    !HNO+ + H2O -> H3O+ + NO
    k(2035) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + HCN -> HCNH+ + NO
    k(2036) = small + (7.20e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + HCO -> H2CO+ + NO
    k(2037) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + HNC -> HCNH+ + NO
    k(2038) = small + (6.52e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + NH2 -> NH3+ + NO
    k(2039) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + H2CO -> H3CO+ + NO
    k(2040) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + NH3 -> NH4+ + NO
    k(2041) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !HNO+ + CH4 -> CH5+ + NO
    k(2042) = small + (1.00e-10)

    !HNSI+ + CO -> HCO+ + SIN
    k(2043) = small + (2.00e-09)

    !HNSI+ + H2 -> SINH2+ + H
    k(2044) = small + (1.00e-09)

    !HNSI+ + H2O -> H3O+ + SIN
    k(2045) = small + (2.00e-09)

    !HOC+ + CO -> HCO+ + CO
    k(2046) = small + (4.00e-10)

    !HOC+ + H2 -> HCO+ + H2
    k(2047) = small + (1.00e-11)

    !HOC+ + N2 -> N2H+ + CO
    k(2048) = small + (2.00e-09)

    !HPO+ + H2O -> H3O+ + PO
    k(2049) = small + (1.00e-09)

    !N2H+ + C -> CH+ + N2
    k(2050) = small + (1.10e-09)

    !N2H+ + S -> HS+ + N2
    k(2051) = small + (1.10e-09)

    !N2H+ + C2 -> C2H+ + N2
    k(2052) = small + (8.30e-10)

    !N2H+ + CH -> CH2+ + N2
    k(2053) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + CO -> HCO+ + N2
    k(2054) = small + (8.80e-10)

    !N2H+ + NH -> NH2+ + N2
    k(2055) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + OH -> H2O+ + N2
    k(2056) = small + (4.70e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + C2H -> C2H2+ + N2
    k(2057) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + CH2 -> CH3+ + N2
    k(2058) = small + (7.19e-10&
        *(T32)**(-5.00e-01))

    !N2H+ + CO2 -> HCO2+ + N2
    k(2059) = small + (9.20e-10)

    !N2H+ + H2O -> H3O+ + N2
    k(2060) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + HCN -> HCNH+ + N2
    k(2061) = small + (7.30e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + HCO -> H2CO+ + N2
    k(2062) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + HNC -> HCNH+ + N2
    k(2063) = small + (6.63e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + NH2 -> NH3+ + N2
    k(2064) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + C2H2 -> C2H3+ + N2
    k(2065) = small + (1.40e-09)

    !N2H+ + H2CO -> H3CO+ + N2
    k(2066) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + NH3 -> NH4+ + N2
    k(2067) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + CH4 -> CH5+ + N2
    k(2068) = small + (9.00e-10)

    !N2H+ + HCNC2 -> HC2NCH+ + N2
    k(2069) = small + (1.75e-08&
        *(T32)**(-5.00e-01))

    !N2H+ + HC2NC -> HC2NCH+ + N2
    k(2070) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + HC3N -> C3H2N+ + N2
    k(2071) = small + (7.94e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + HNC3 -> C3H2N+ + N2
    k(2072) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + NH2CHO -> NH2CH2O+ + N2
    k(2073) = small + (8.15e-09&
        *(T32)**(-5.00e-01))

    !N2H+ + C6H6 -> C6H7+ + N2
    k(2074) = small + (1.60e-09)

    !NAH2+ + H2O -> NAH2O+ + H2
    k(2075) = small + (1.00e-09)

    !NH2+ + N -> N2H+ + H
    k(2076) = small + (9.10e-11)

    !NH2+ + O -> HNO+ + H
    k(2077) = small + (7.20e-11)

    !NH2+ + S -> S+ + NH2
    k(2078) = small + (4.40e-10)

    !NH2+ + S -> HS+ + NH
    k(2079) = small + (4.40e-10)

    !NH2+ + S -> HNS+ + H
    k(2080) = small + (4.40e-10)

    !NH2+ + C2 -> C2H+ + NH
    k(2081) = small + (9.70e-10)

    !NH2+ + CH -> CH+ + NH2
    k(2082) = small + (3.50e-10)

    !NH2+ + CH -> CH2+ + NH
    k(2083) = small + (3.50e-10)

    !NH2+ + CN -> HCNH+ + N
    k(2084) = small + (1.00e-10)

    !NH2+ + CN -> H2NC+ + N
    k(2085) = small + (1.00e-10)

    !NH2+ + H2 -> NH3+ + H
    k(2086) = small + (1.20e-10)

    !NH2+ + NH -> NH3+ + N
    k(2087) = small + (7.30e-10)

    !NH2+ + NO -> NO+ + NH2
    k(2088) = small + (9.40e-10)

    !NH2+ + O2 -> HNO+ + OH
    k(2089) = small + (2.10e-11)

    !NH2+ + O2 -> H2NO+ + O
    k(2090) = small + (1.20e-10)

    !NH2+ + C2H -> C2H2+ + NH
    k(2091) = small + (9.10e-10)

    !NH2+ + CH2 -> CH2+ + NH2
    k(2092) = small + (4.90e-10)

    !NH2+ + CH2 -> CH3+ + NH
    k(2093) = small + (4.90e-10)

    !NH2+ + H2O -> H3O+ + NH
    k(2094) = small + (1.60e-09)

    !NH2+ + H2O -> NH3+ + OH
    k(2095) = small + (1.00e-10)

    !NH2+ + H2O -> NH4+ + O
    k(2096) = small + (3.00e-11)

    !NH2+ + H2S -> HS+ + NH3
    k(2097) = small + (1.80e-10)

    !NH2+ + H2S -> H2S+ + NH2
    k(2098) = small + (3.40e-10)

    !NH2+ + H2S -> H3S+ + NH
    k(2099) = small + (2.40e-10)

    !NH2+ + H2S -> NH3+ + HS
    k(2100) = small + (4.50e-10)

    !NH2+ + H2S -> NH4+ + S
    k(2101) = small + (1.80e-10)

    !NH2+ + HCN -> HCNH+ + NH
    k(2102) = small + (1.20e-09)

    !NH2+ + HCO -> HCO+ + NH2
    k(2103) = small + (4.30e-10)

    !NH2+ + HNC -> HCNH+ + NH
    k(2104) = small + (1.20e-09)

    !NH2+ + NH2 -> NH3+ + NH
    k(2105) = small + (1.00e-09)

    !NH2+ + H2CO -> NH3+ + HCO
    k(2106) = small + (5.60e-10)

    !NH2+ + H2CO -> H3CO+ + NH
    k(2107) = small + (2.20e-09)

    !NH2+ + NH3 -> NH3+ + NH2
    k(2108) = small + (1.50e-09)

    !NH2+ + NH3 -> NH4+ + NH
    k(2109) = small + (1.00e-09)

    !NO2+ + H -> NO+ + OH
    k(2110) = small + (1.90e-10)

    !NO2+ + H2 -> NO+ + H2O
    k(2111) = small + (1.50e-10)

    !O2H+ + C -> CH+ + O2
    k(2112) = small + (1.00e-09)

    !O2H+ + N -> NO2+ + H
    k(2113) = small + (1.00e-12)

    !O2H+ + O -> OH+ + O2
    k(2114) = small + (6.20e-10)

    !O2H+ + S -> HS+ + O2
    k(2115) = small + (1.10e-09)

    !O2H+ + C2 -> C2H+ + O2
    k(2116) = small + (8.10e-10)

    !O2H+ + CH -> CH2+ + O2
    k(2117) = small + (6.20e-10)

    !O2H+ + CN -> HCN+ + O2
    k(2118) = small + (8.60e-10)

    !O2H+ + CO -> HCO+ + O2
    k(2119) = small + (8.40e-10)

    !O2H+ + H2 -> H3+ + O2
    k(2120) = small + (3.20e-10)

    !O2H+ + N2 -> N2H+ + O2
    k(2121) = small + (7.90e-10)

    !O2H+ + NH -> NH2+ + O2
    k(2122) = small + (6.30e-10)

    !O2H+ + NO -> HNO+ + O2
    k(2123) = small + (7.70e-10)

    !O2H+ + OH -> H2O+ + O2
    k(2124) = small + (6.10e-10)

    !O2H+ + C2H -> C2H2+ + O2
    k(2125) = small + (7.60e-10)

    !O2H+ + CH2 -> CH3+ + O2
    k(2126) = small + (8.50e-10)

    !O2H+ + CO2 -> HCO2+ + O2
    k(2127) = small + (1.10e-09)

    !O2H+ + H2O -> H3O+ + O2
    k(2128) = small + (8.20e-10)

    !O2H+ + HCN -> HCNH+ + O2
    k(2129) = small + (9.70e-10)

    !O2H+ + HCO -> H2CO+ + O2
    k(2130) = small + (7.10e-10)

    !O2H+ + HNC -> HCNH+ + O2
    k(2131) = small + (9.70e-10)

    !O2H+ + NH2 -> NH3+ + O2
    k(2132) = small + (8.70e-10)

    !O2H+ + H2CO -> H3CO+ + O2
    k(2133) = small + (9.80e-10)

    !O2H+ + NH3 -> NH4+ + O2
    k(2134) = small + (2.00e-09)

    !OCS+ + NH3 -> NH3+ + OCS
    k(2135) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !PH2+ + C2H2 -> PC2H2+ + H2
    k(2136) = small + (1.40e-09)

    !PH2+ + C2H4 -> PCH2+ + CH4
    k(2137) = small + (1.40e-10)

    !PH2+ + C2H4 -> PC2H4+ + H2
    k(2138) = small + (1.10e-09)

    !S2H+ + H2 -> H3S2+
    k(2139) = small + (1.00e-14&
        *(T32)**(-1.00e+00))

    !S2H+ + H2S -> H3S+ + S2
    k(2140) = small + (7.95e-10&
        *(T32)**(-5.00e-01))

    !SIC2+ + H2 -> SIC2H+ + H
    k(2141) = small + (1.50e-09)

    !SIH2+ + C -> CHSI+ + H
    k(2142) = small + (1.10e-09)

    !SIH2+ + N -> HNSI+ + H
    k(2143) = small + (1.00e-10)

    !SIH2+ + O -> HSIO+ + H
    k(2144) = small + (6.30e-10)

    !SIH2+ + S -> HSIS+ + H
    k(2145) = small + (1.10e-09)

    !SIH2+ + O2 -> HSIO+ + OH
    k(2146) = small + (2.40e-11)

    !SINC+ + O -> SIN+ + CO
    k(2147) = small + (1.00e-10)

    !SO2+ + CO -> SO+ + CO2
    k(2148) = small + (3.00e-10)

    !SO2+ + H2 -> HSO2+ + H
    k(2149) = small + (5.00e-12)

    !SO2+ + O2 -> O2+ + SO2
    k(2150) = small + (2.50e-10)

    !C2H2+ + C -> C2H2 + C+
    k(2151) = small + (1.10e-09)

    !C2H2+ + C -> C3+ + H2
    k(2152) = small + (1.10e-09)

    !C2H2+ + C -> C3H+ + H
    k(2153) = small + (1.10e-09)

    !C2H2+ + FE -> FE+ + C2H2
    k(2154) = small + (2.00e-09)

    !C2H2+ + MG -> MG+ + C2H2
    k(2155) = small + (3.00e-09)

    !C2H2+ + N -> CH+ + HCN
    k(2156) = small + (2.50e-11)

    !C2H2+ + N -> C2N+ + H2
    k(2157) = small + (7.50e-11)

    !C2H2+ + N -> C2NH+ + H
    k(2158) = small + (1.50e-10)

    !C2H2+ + NA -> NA+ + C2H2
    k(2159) = small + (2.70e-09)

    !C2H2+ + O -> HCO+ + CH
    k(2160) = small + (5.00e-11)

    !C2H2+ + O -> HOC+ + CH
    k(2161) = small + (5.00e-11)

    !C2H2+ + O -> C2HO+ + H
    k(2162) = small + (1.00e-10)

    !C2H2+ + P -> PC2H+ + H
    k(2163) = small + (1.00e-09)

    !C2H2+ + SI -> SI+ + C2H2
    k(2164) = small + (1.70e-09)

    !C2H2+ + SI -> SIC2+ + H2
    k(2165) = small + (2.00e-10)

    !C2H2+ + SI -> SIC2H+ + H
    k(2166) = small + (2.00e-10)

    !C2H2+ + CH -> C3H2+ + H
    k(2167) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + CH -> H2C3+ + H
    k(2168) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + CN -> C3HN+ + H
    k(2169) = small + (3.70e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + NH -> C2H2N+ + H
    k(2170) = small + (3.90e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + NO -> NO+ + C2H2
    k(2171) = small + (3.80e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + OH -> C2H2O+ + H
    k(2172) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + O2 -> HCO+ + HCO
    k(2173) = small + (9.80e-13)

    !C2H2+ + C2H -> C4H2+ + H
    k(2174) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + CH2 -> C3H3+ + H
    k(2175) = small + (3.66e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + CH2 -> H3C3+ + H
    k(2176) = small + (3.66e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + H2S -> H2S+ + C2H2
    k(2177) = small + (9.80e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + H2S -> C2H3+ + HS
    k(2178) = small + (2.00e-11&
        *(T32)**(-5.00e-01))

    !C2H2+ + HCN -> C3H2N+ + H
    k(2179) = small + (1.33e-10)

    !C2H2+ + HCO -> HCO+ + C2H2
    k(2180) = small + (5.70e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + HCO -> C2H3+ + CO
    k(2181) = small + (4.30e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + HNC -> HCNH+ + C2H
    k(2182) = small + (2.60e-10)

    !C2H2+ + HNC -> C3H2N+ + H
    k(2183) = small + (1.30e-10)

    !C2H2+ + NH2 -> NH3+ + C2H
    k(2184) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + NH2 -> C2H3N+ + H
    k(2185) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C2H2 -> C4H2+ + H2
    k(2186) = small + (5.20e-10)

    !C2H2+ + C2H2 -> C4H3+ + H
    k(2187) = small + (8.80e-10)

    !C2H2+ + C3H -> C5H+ + H2
    k(2188) = small + (3.18e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H -> C5H2+ + H
    k(2189) = small + (3.18e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + H2CO -> H2CO+ + C2H2
    k(2190) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + NH3 -> NH3+ + C2H2
    k(2191) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + NH3 -> NH4+ + C2H
    k(2192) = small + (5.50e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C2H3 -> C2H3+ + C2H2
    k(2193) = small + (5.30e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C2H3 -> C3H3+ + CH2
    k(2194) = small + (2.65e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C2H3 -> H3C3+ + CH2
    k(2195) = small + (2.65e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C2H3 -> C4H3+ + H2
    k(2196) = small + (5.30e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H2 -> C5H2+ + H2
    k(2197) = small + (1.02e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H2 -> C5H3+ + H
    k(2198) = small + (2.03e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C4H -> C6H+ + H2
    k(2199) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C4H -> C6H2+ + H
    k(2200) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + CH4 -> C3H4+ + H2
    k(2201) = small + (2.00e-10)

    !C2H2+ + CH4 -> C3H5+ + H
    k(2202) = small + (8.00e-10)

    !C2H2+ + C2H4 -> C2H4+ + C2H2
    k(2203) = small + (4.10e-10)

    !C2H2+ + C2H4 -> C3H3+ + CH3
    k(2204) = small + (2.60e-10)

    !C2H2+ + C2H4 -> H3C3+ + CH3
    k(2205) = small + (2.60e-10)

    !C2H2+ + C2H4 -> C4H5+ + H
    k(2206) = small + (2.80e-10)

    !C2H2+ + C3H3 -> C3H3+ + C2H2
    k(2207) = small + (6.55e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H3 -> H3C3+ + C2H2
    k(2208) = small + (6.55e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H3 -> C5H3+ + H2
    k(2209) = small + (1.31e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H3 -> C5H4+ + H
    k(2210) = small + (1.31e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C4H2 -> C4H2+ + C2H2
    k(2211) = small + (1.30e-09)

    !C2H2+ + C4H2 -> C6H2+ + H2
    k(2212) = small + (1.00e-17)

    !C2H2+ + C4H2 -> C6H3+ + H
    k(2213) = small + (1.40e-10)

    !C2H2+ + C5H -> C7H+ + H2
    k(2214) = small + (4.62e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C5H -> C7H2+ + H
    k(2215) = small + (4.62e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H4 -> C3H4+ + C2H2
    k(2216) = small + (2.54e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H4 -> C5H4+ + H2
    k(2217) = small + (2.54e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C3H4 -> C5H5+ + H
    k(2218) = small + (2.54e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C5H2 -> C5H2+ + C2H2
    k(2219) = small + (7.55e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C5H2 -> C7H2+ + H2
    k(2220) = small + (7.55e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C5H2 -> C7H3+ + H
    k(2221) = small + (7.55e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C6H -> C8H+ + H2
    k(2222) = small + (5.03e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C6H -> C8H2+ + H
    k(2223) = small + (5.03e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C6H2 -> C6H2+ + C2H2
    k(2224) = small + (5.00e-10)

    !C2H2+ + C6H2 -> C8H2+ + H2
    k(2225) = small + (5.00e-10)

    !C2H2+ + C6H2 -> C8H3+ + H
    k(2226) = small + (5.00e-10)

    !C2H2+ + C7H -> C9H+ + H2
    k(2227) = small + (4.63e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C7H -> C9H2+ + H
    k(2228) = small + (4.63e-09&
        *(T32)**(-5.00e-01))

    !C2H2+ + C7H2 -> C7H2+ + C2H2
    k(2229) = small + (7.23e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C7H2 -> C9H2+ + H2
    k(2230) = small + (7.23e-10&
        *(T32)**(-5.00e-01))

    !C2H2+ + C7H2 -> C9H3+ + H
    k(2231) = small + (7.23e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + SI -> SIC3+ + H
    k(2232) = small + (2.00e-10)

    !C3H+ + H2 -> C3H2+ + H
    k(2233) = small + (1.00e-12)

    !C3H+ + NO -> NO+ + C3H
    k(2234) = small + (8.65e-11&
        *(T32)**(-5.00e-01))

    !C3H+ + NO -> C2NH+ + CO
    k(2235) = small + (1.73e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + O2 -> HCO+ + CCO
    k(2236) = small + (1.50e-11)

    !C3H+ + O2 -> C2HO+ + CO
    k(2237) = small + (2.50e-12)

    !C3H+ + O2 -> HC3O+ + O
    k(2238) = small + (7.50e-12)

    !C3H+ + CO2 -> HC3O+ + CO
    k(2239) = small + (2.00e-12)

    !C3H+ + H2O -> HCO+ + C2H2
    k(2240) = small + (6.87e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + H2O -> C2H3+ + CO
    k(2241) = small + (6.87e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + H2O -> HC3O+ + H2
    k(2242) = small + (6.87e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + H2S -> HCS+ + C2H2
    k(2243) = small + (2.98e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + H2S -> C2H3+ + CS
    k(2244) = small + (2.98e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + H2S -> HC3S+ + H2
    k(2245) = small + (2.98e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + OCS -> CS+ + C3O + H
    k(2246) = small + (4.57e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + OCS -> HC3O+ + CS
    k(2247) = small + (4.57e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + OCS -> HC3S+ + CO
    k(2248) = small + (4.57e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + C2H2 -> C5H2+ + H
    k(2249) = small + (3.00e-10)

    !C3H+ + NH3 -> H2NC+ + C2H2
    k(2250) = small + (5.57e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + NH3 -> NH3+ + C3H
    k(2251) = small + (2.79e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + NH3 -> NH4+ + C3
    k(2252) = small + (5.57e-10&
        *(T32)**(-5.00e-01))

    !C3H+ + CH4 -> C2H3+ + C2H2
    k(2253) = small + (6.12e-10)

    !C3H+ + CH4 -> C4H3+ + H2
    k(2254) = small + (5.50e-11)

    !C3H+ + C2H4 -> C3H3+ + C2H2
    k(2255) = small + (4.50e-10)

    !C3H+ + C2H4 -> H3C3+ + C2H2
    k(2256) = small + (4.50e-10)

    !C3H+ + C2H4 -> C5H3+ + H2
    k(2257) = small + (5.00e-11)

    !C3H+ + C4H2 -> C5H+ + C2H2
    k(2258) = small + (1.20e-10)

    !C3H+ + C4H2 -> C5H2+ + C2H
    k(2259) = small + (1.00e-09)

    !C3N+ + H2 -> C3HN+ + H
    k(2260) = small + (1.50e-09)

    !C4+ + H2 -> C4H+ + H
    k(2261) = small + (1.30e-10)

    !CH2SI+ + C -> SI+ + C2H2
    k(2262) = small + (1.00e-09)

    !CH2SI+ + C -> SIC2+ + H2
    k(2263) = small + (1.00e-10)

    !CH2SI+ + C -> SIC2H+ + H
    k(2264) = small + (1.00e-10)

    !CH2SI+ + N -> SI+ + HCN + H
    k(2265) = small + (7.60e-10)

    !CH2SI+ + O -> SI+ + H2CO
    k(2266) = small + (6.00e-10)

    !CH2SI+ + O -> HCO+ + SIH
    k(2267) = small + (3.00e-10)

    !CH2SI+ + S -> SI+ + H2CS
    k(2268) = small + (1.00e-09)

    !CH3+ + C -> C2H+ + H2
    k(2269) = small + (1.20e-09)

    !CH3+ + C -> C2H2+ + H
    k(2270) = small + (1.20e-09)

    !CH3+ + FE -> FE+ + CH3
    k(2271) = small + (2.40e-09)

    !CH3+ + MG -> MG+ + CH3
    k(2272) = small + (3.50e-09)

    !CH3+ + N -> HCN+ + H2
    k(2273) = small + (6.70e-11)

    !CH3+ + N -> H2NC+ + H
    k(2274) = small + (6.70e-11)

    !CH3+ + NA -> NA+ + CH3
    k(2275) = small + (3.40e-09)

    !CH3+ + O -> HOC+ + H2
    k(2276) = small + (2.05e-10)

    !CH3+ + O -> HCO+ + H2
    k(2277) = small + (2.05e-10)

    !CH3+ + O -> H2CO+ + H
    k(2278) = small + (1.00e-15)

    !CH3+ + P -> PCH2+ + H
    k(2279) = small + (1.00e-09)

    !CH3+ + S -> HCS+ + H2
    k(2280) = small + (1.40e-09)

    !CH3+ + SI -> SI+ + CH3
    k(2281) = small + (9.70e-10)

    !CH3+ + SI -> CHSI+ + H2
    k(2282) = small + (2.00e-10)

    !CH3+ + SI -> CH2SI+ + H
    k(2283) = small + (9.70e-10)

    !CH3+ + C2 -> C3H+ + H2
    k(2284) = small + (9.90e-10)

    !CH3+ + CH -> C2H2+ + H2
    k(2285) = small + (5.10e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + CN -> C2H2N+ + H
    k(2286) = small + (4.30e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HCL -> H2CCL+ + H2
    k(2287) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HS -> H2CS+ + H2
    k(2288) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + NH -> HCNH+ + H2
    k(2289) = small + (4.40e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + NO -> NO+ + CH3
    k(2290) = small + (4.44e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + O2 -> H3CO+ + O
    k(2291) = small + (5.00e-12)

    !CH3+ + OH -> H2CO+ + H2
    k(2292) = small + (5.40e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + SIH -> CH2SI+ + H2
    k(2293) = small + (3.50e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + SO -> HOCS+ + H2
    k(2294) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H -> C3H2+ + H2
    k(2295) = small + (6.00e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H -> H2C3+ + H2
    k(2296) = small + (6.00e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H -> C3H3+ + H
    k(2297) = small + (6.00e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H -> H3C3+ + H
    k(2298) = small + (6.00e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + CH2 -> C2H3+ + H2
    k(2299) = small + (8.21e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + H2S -> H3CS+ + H2
    k(2300) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HCO -> HCO+ + CH3
    k(2301) = small + (6.10e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + HCO -> CH4+ + CO
    k(2302) = small + (6.10e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + NH2 -> CH4N+ + H
    k(2303) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + OCS -> H3CS+ + CO
    k(2304) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H2 -> C3H3+ + H2
    k(2305) = small + (6.00e-10)

    !CH3+ + C3H -> C3H+ + CH3
    k(2306) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C3H -> C4H2+ + H2
    k(2307) = small + (3.80e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + H2CO -> HCO+ + CH4
    k(2308) = small + (2.90e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + NH3 -> NH4+ + CH2
    k(2309) = small + (3.00e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + NH3 -> CH4N+ + H2
    k(2310) = small + (1.72e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H3 -> C2H3+ + CH3
    k(2311) = small + (5.60e-10&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H3 -> C3H3+ + H + H2
    k(2312) = small + (9.50e-11&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H3 -> H3C3+ + H + H2
    k(2313) = small + (9.50e-11&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H3 -> C3H4+ + H2
    k(2314) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C3H2 -> C4H3+ + H2
    k(2315) = small + (3.64e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C4H -> C5H2+ + H2
    k(2316) = small + (2.44e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + CH4 -> C2H5+ + H2
    k(2317) = small + (9.60e-10)

    !CH3+ + HCNC2 -> C3H3+ + HCN
    k(2318) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + HCNC2 -> H3C3+ + HCN
    k(2319) = small + (1.10e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + HC2NC -> C3H3+ + HCN
    k(2320) = small + (3.16e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HC2NC -> H3C3+ + HCN
    k(2321) = small + (3.16e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HC3N -> C3H3+ + HCN
    k(2322) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HC3N -> H3C3+ + HCN
    k(2323) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HNC3 -> C3H3+ + HNC
    k(2324) = small + (2.84e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + HNC3 -> H3C3+ + HNC
    k(2325) = small + (2.84e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C2H4 -> C2H3+ + CH4
    k(2326) = small + (3.50e-10)

    !CH3+ + C2H4 -> C3H3+ + H2 + H2
    k(2327) = small + (2.30e-11)

    !CH3+ + C2H4 -> H3C3+ + H2 + H2
    k(2328) = small + (2.30e-11)

    !CH3+ + C2H4 -> C3H5+ + H2
    k(2329) = small + (5.20e-10)

    !CH3+ + C3H3 -> C4H5+ + H
    k(2330) = small + (4.71e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C4H2 -> C3H3+ + C2H2
    k(2331) = small + (6.00e-10)

    !CH3+ + C4H2 -> H3C3+ + C2H2
    k(2332) = small + (6.00e-10)

    !CH3+ + C4H2 -> C5H3+ + H2
    k(2333) = small + (1.30e-10)

    !CH3+ + C5H -> C6H2+ + H2
    k(2334) = small + (1.14e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + CH4O -> H3CO+ + CH4
    k(2335) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + NH2CHO -> CH5N+ + HCO
    k(2336) = small + (1.00e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + C5H2 -> C6H3+ + H2
    k(2337) = small + (2.79e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C6H -> C7H2+ + H2
    k(2338) = small + (1.30e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + C6H2 -> C7H3+ + H2
    k(2339) = small + (1.20e-09)

    !CH3+ + C7H -> C8H2+ + H2
    k(2340) = small + (1.16e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + C7H2 -> C8H3+ + H2
    k(2341) = small + (2.71e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + C8H -> C9H2+ + H2
    k(2342) = small + (1.27e-08&
        *(T32)**(-5.00e-01))

    !CH3+ + C8H2 -> C9H3+ + H2
    k(2343) = small + (1.20e-09)

    !HCNH+ + NA -> NA+ + HCN + H
    k(2344) = small + (2.70e-09)

    !HCNH+ + NA -> NA+ + HNC + H
    k(2345) = small + (1.35e-09)

    !HCNH+ + SI -> SINC+ + H2
    k(2346) = small + (5.00e-10)

    !HCNH+ + CH -> CH2+ + HCN
    k(2347) = small + (4.50e-09&
        *(T32)**(-5.00e-01))

    !HCNH+ + CH2 -> CH3+ + HCN
    k(2348) = small + (3.61e-10&
        *(T32)**(-5.00e-01))

    !HCNH+ + CH2 -> CH3+ + HNC
    k(2349) = small + (3.61e-10&
        *(T32)**(-5.00e-01))

    !HCNH+ + NH2 -> NH3+ + HCN
    k(2350) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !HCNH+ + NH2 -> NH3+ + HNC
    k(2351) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !HCNH+ + H2CO -> H3CO+ + HCN
    k(2352) = small + (2.37e-09&
        *(T32)**(-5.00e-01))

    !HCNH+ + NH3 -> NH4+ + HCN
    k(2353) = small + (8.75e-10&
        *(T32)**(-5.00e-01))

    !HCNH+ + NH3 -> NH4+ + HNC
    k(2354) = small + (8.75e-10&
        *(T32)**(-5.00e-01))

    !H2CO+ + FE -> FE+ + H2CO
    k(2355) = small + (1.90e-09)

    !H2CO+ + MG -> MG+ + H2CO
    k(2356) = small + (2.90e-09)

    !H2CO+ + NA -> NA+ + H2CO
    k(2357) = small + (2.60e-09)

    !H2CO+ + S -> S+ + H2CO
    k(2358) = small + (5.50e-10)

    !H2CO+ + S -> HS+ + HCO
    k(2359) = small + (5.50e-10)

    !H2CO+ + SI -> SI+ + H2CO
    k(2360) = small + (1.60e-09)

    !H2CO+ + C2 -> C2H+ + HCO
    k(2361) = small + (8.20e-10)

    !H2CO+ + CH -> CH+ + H2CO
    k(2362) = small + (3.10e-10)

    !H2CO+ + CH -> CH2+ + HCO
    k(2363) = small + (3.10e-10)

    !H2CO+ + NH -> H3CO+ + N
    k(2364) = small + (6.40e-10)

    !H2CO+ + NO -> NO+ + H2CO
    k(2365) = small + (7.80e-10)

    !H2CO+ + O2 -> HCO+ + O2H
    k(2366) = small + (7.70e-11)

    !H2CO+ + C2H -> C2H2+ + HCO
    k(2367) = small + (7.70e-10)

    !H2CO+ + CH2 -> CH2+ + H2CO
    k(2368) = small + (4.30e-10)

    !H2CO+ + CH2 -> CH3+ + HCO
    k(2369) = small + (4.30e-10)

    !H2CO+ + H2O -> H3O+ + HCO
    k(2370) = small + (2.60e-09)

    !H2CO+ + HCN -> HCNH+ + HCO
    k(2371) = small + (1.40e-09)

    !H2CO+ + HCO -> HCO+ + H2CO
    k(2372) = small + (3.20e-09)

    !H2CO+ + HCO -> H3CO+ + CO
    k(2373) = small + (3.60e-10)

    !H2CO+ + HNC -> HCNH+ + HCO
    k(2374) = small + (1.40e-09)

    !H2CO+ + NH2 -> NH3+ + HCO
    k(2375) = small + (8.80e-10)

    !H2CO+ + H2CO -> H3CO+ + HCO
    k(2376) = small + (1.00e-09)

    !H2CO+ + NH3 -> NH3+ + H2CO
    k(2377) = small + (4.30e-10)

    !H2CO+ + NH3 -> NH4+ + HCO
    k(2378) = small + (1.30e-09)

    !H2CO+ + CH4 -> H3CO+ + CH3
    k(2379) = small + (9.35e-11)

    !H2CO+ + CH4 -> C2H5O+ + H
    k(2380) = small + (1.65e-11)

    !H2NC+ + SI -> SINC+ + H2
    k(2381) = small + (5.00e-10)

    !H3O+ + C -> HCO+ + H2
    k(2382) = small + (1.00e-11)

    !H3O+ + NA -> NA+ + H2O + H
    k(2383) = small + (3.10e-09)

    !H3O+ + P -> HPO+ + H2
    k(2384) = small + (1.00e-09)

    !H3O+ + SI -> SIH+ + H2O
    k(2385) = small + (1.80e-09)

    !H3O+ + CH -> CH2+ + H2O
    k(2386) = small + (4.80e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CP -> HCP+ + H2O
    k(2387) = small + (2.17e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CS -> HCS+ + H2O
    k(2388) = small + (4.99e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + PN -> HPN+ + H2O
    k(2389) = small + (6.91e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + S2 -> S2H+ + H2O
    k(2390) = small + (2.00e-09)

    !H3O+ + SIC -> CHSI+ + H2O
    k(2391) = small + (4.35e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + SIH -> SIH2+ + H2O
    k(2392) = small + (3.25e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + SIO -> HSIO+ + H2O
    k(2393) = small + (7.81e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2S -> HC2S+ + H2O
    k(2394) = small + (2.88e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3 -> C3H+ + H2O
    k(2395) = small + (2.00e-09)

    !H3O+ + CCO -> C2HO+ + H2O
    k(2396) = small + (3.32e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CCP -> PC2H+ + H2O
    k(2397) = small + (2.44e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CH2 -> CH3+ + H2O
    k(2398) = small + (7.78e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + H2S -> H3S+ + H2O
    k(2399) = small + (1.10e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HCN -> HCNH+ + H2O
    k(2400) = small + (8.20e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HCP -> PCH2+ + H2O
    k(2401) = small + (9.83e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + HCSI -> CH2SI+ + H2O
    k(2402) = small + (2.55e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HNC -> HCNH+ + H2O
    k(2403) = small + (7.42e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HNSI -> SINH2+ + H2O
    k(2404) = small + (4.04e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + HS2 -> H2S2+ + H2O
    k(2405) = small + (2.02e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + NH2 -> NH3+ + H2O
    k(2406) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + SIC2 -> SIC2H+ + H2O
    k(2407) = small + (2.48e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + SIH2 -> SIH3+ + H2O
    k(2408) = small + (2.05e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + SINC -> SINCH+ + H2O
    k(2409) = small + (2.10e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3H -> C3H2+ + H2O
    k(2410) = small + (7.01e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3N -> C3HN+ + H2O
    k(2411) = small + (5.44e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3O -> HC3O+ + H2O
    k(2412) = small + (5.88e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3P -> PC3H+ + H2O
    k(2413) = small + (1.01e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3S -> HC3S+ + H2O
    k(2414) = small + (3.92e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C4 -> C4H+ + H2O
    k(2415) = small + (1.10e-09)

    !H3O+ + H2CO -> H3CO+ + H2O
    k(2416) = small + (2.60e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + H2S2 -> H3S2+ + H2O
    k(2417) = small + (1.21e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + H2SIO -> H3SIO+ + H2O
    k(2418) = small + (2.11e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HCCP -> PC2H2+ + H2O
    k(2419) = small + (1.00e-09)

    !H3O+ + NH3 -> NH4+ + H2O
    k(2420) = small + (1.90e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + SIC2H -> SIC2H2+ + H2O
    k(2421) = small + (1.45e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H2O -> C2H3O+ + H2O
    k(2422) = small + (1.52e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H3 -> C2H4+ + H2O
    k(2423) = small + (1.74e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3H2 -> C3H3+ + H2O
    k(2424) = small + (3.35e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C4H -> C4H2+ + H2O
    k(2425) = small + (2.23e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C4P -> PC4H+ + H2O
    k(2426) = small + (4.95e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + C4S -> HC4S+ + H2O
    k(2427) = small + (2.97e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CH2PH -> PCH4+ + H2O
    k(2428) = small + (5.29e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + HCNC2 -> HC2NCH+ + H2O
    k(2429) = small + (2.00e-08&
        *(T32)**(-5.00e-01))

    !H3O+ + HC2NC -> HC2NCH+ + H2O
    k(2430) = small + (5.79e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HC3N -> C3H2N+ + H2O
    k(2431) = small + (9.17e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HNC3 -> C3H2N+ + H2O
    k(2432) = small + (5.19e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + SICH3 -> SICH4+ + H2O
    k(2433) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H3N -> C2H4N+ + H2O
    k(2434) = small + (4.22e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3H3 -> C3H4+ + H2O
    k(2435) = small + (4.34e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C4H2 -> C4H3+ + H2O
    k(2436) = small + (1.10e-09)

    !H3O+ + CH4O -> CH5O+ + H2O
    k(2437) = small + (1.91e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + NH2CHO -> NH2CH2O+ + H2O
    k(2438) = small + (9.37e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H4O -> C2H5O+ + H2O
    k(2439) = small + (2.86e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C3H4 -> C3H5+ + H2O
    k(2440) = small + (8.44e-10&
        *(T32)**(-5.00e-01))

    !H3O+ + C4H3 -> C4H4+ + H2O
    k(2441) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + HCOOCH3 -> H5C2O2+ + H2O
    k(2442) = small + (1.81e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H5OH -> C2H5OH2+ + H2O
    k(2443) = small + (1.79e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + CH3OCH3 -> CH3OCH4+ + H2O
    k(2444) = small + (1.37e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C2H6CO -> C3H6OH+ + H2O
    k(2445) = small + (2.87e-09&
        *(T32)**(-5.00e-01))

    !H3O+ + C6H6 -> C6H7+ + H2O
    k(2446) = small + (1.80e-09)

    !H3S+ + H -> H2S+ + H2
    k(2447) = small + (6.00e-11)

    !H3S+ + HCN -> HCNH+ + H2S
    k(2448) = small + (1.50e-09)

    !H3S+ + HNC -> HCNH+ + H2S
    k(2449) = small + (1.50e-09)

    !H3S+ + H2CO -> H3CO+ + H2S
    k(2450) = small + (2.20e-09)

    !H3S+ + NH3 -> NH4+ + H2S
    k(2451) = small + (1.90e-09)

    !HCO2+ + C -> CH+ + CO2
    k(2452) = small + (1.00e-09)

    !HCO2+ + O -> HCO+ + O2
    k(2453) = small + (1.00e-09)

    !HCO2+ + CO -> HCO+ + CO2
    k(2454) = small + (2.47e-10&
        *(T32)**(-5.00e-01))

    !HCO2+ + H2O -> H3O+ + CO2
    k(2455) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !HCO2+ + C2H2 -> C2H3+ + CO2
    k(2456) = small + (1.37e-09)

    !HCO2+ + NH3 -> NH4+ + CO2
    k(2457) = small + (1.62e-09&
        *(T32)**(-5.00e-01))

    !HCO2+ + CH4 -> CH5+ + CO2
    k(2458) = small + (7.80e-10)

    !HCO2+ + C2H3N -> C2H4N+ + CO2
    k(2459) = small + (3.28e-09&
        *(T32)**(-5.00e-01))

    !HSO2+ + H2O -> H3O+ + SO2
    k(2460) = small + (2.13e-09)

    !HSO2+ + NH3 -> NH4+ + SO2
    k(2461) = small + (2.00e-09)

    !NH3+ + FE -> FE+ + NH3
    k(2462) = small + (2.30e-09)

    !NH3+ + MG -> MG+ + NH3
    k(2463) = small + (3.30e-09)

    !NH3+ + NA -> NA+ + NH3
    k(2464) = small + (3.20e-09)

    !NH3+ + O -> HNO+ + H2
    k(2465) = small + (1.00e-11)

    !NH3+ + O -> H2NO+ + H
    k(2466) = small + (1.00e-11)

    !NH3+ + SI -> SI+ + NH3
    k(2467) = small + (1.90e-09)

    !NH3+ + C2 -> C2H2+ + NH
    k(2468) = small + (1.00e-11)

    !NH3+ + CH -> NH4+ + C
    k(2469) = small + (4.90e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + H2 -> NH4+ + H
    k(2470) = small + (1.50e-14&
        *(T32)**(-1.50e+00))

    !NH3+ + NH -> NH4+ + N
    k(2471) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + NO -> NO+ + NH3
    k(2472) = small + (4.30e-10&
        *(T32)**(-5.00e-01))

    !NH3+ + OH -> NH4+ + O
    k(2473) = small + (5.20e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + CH2 -> CH3+ + NH2
    k(2474) = small + (7.97e-10&
        *(T32)**(-5.00e-01))

    !NH3+ + H2O -> NH4+ + OH
    k(2475) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + H2S -> NH4+ + HS
    k(2476) = small + (9.60e-10&
        *(T32)**(-5.00e-01))

    !NH3+ + HCO -> HCO+ + NH3
    k(2477) = small + (5.90e-10&
        *(T32)**(-5.00e-01))

    !NH3+ + HCO -> NH4+ + CO
    k(2478) = small + (5.90e-10&
        *(T32)**(-5.00e-01))

    !NH3+ + NH2 -> NH4+ + NH
    k(2479) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + H2CO -> NH4+ + HCO
    k(2480) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + NH3 -> NH4+ + NH2
    k(2481) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !NH3+ + CH4 -> NH4+ + CH3
    k(2482) = small + (3.90e-10)

    !PC2H+ + C2H2 -> PC4H2+ + H
    k(2483) = small + (9.10e-10)

    !PH3+ + C2H2 -> PC2H3+ + H2
    k(2484) = small + (5.80e-10)

    !SIC2H+ + C -> SIC3+ + H
    k(2485) = small + (3.00e-10)

    !SIC2H+ + C2H2 -> SIC4H+ + H2
    k(2486) = small + (2.00e-11)

    !SIH3+ + C -> CH2SI+ + H
    k(2487) = small + (2.00e-10)

    !SIH3+ + N -> HNSI+ + H2
    k(2488) = small + (1.00e-10)

    !SIH3+ + N -> SINH2+ + H
    k(2489) = small + (1.00e-10)

    !SIH3+ + O -> HSIO+ + H2
    k(2490) = small + (2.00e-10)

    !SIH3+ + O -> H2SIO+ + H
    k(2491) = small + (2.00e-10)

    !SIH3+ + O2 -> H3SIO+ + O
    k(2492) = small + (2.90e-12)

    !SINCH+ + O -> HCO+ + SIN
    k(2493) = small + (2.00e-10)

    !SINH2+ + C -> SINC+ + H2
    k(2494) = small + (1.00e-10)

    !SINH2+ + C -> SINCH+ + H
    k(2495) = small + (1.00e-10)

    !C2H3+ + C -> C3H+ + H2
    k(2496) = small + (1.00e-09)

    !C2H3+ + C -> C3H2+ + H
    k(2497) = small + (5.00e-10)

    !C2H3+ + C -> H2C3+ + H
    k(2498) = small + (5.00e-10)

    !C2H3+ + H -> C2H2+ + H2
    k(2499) = small + (6.80e-11)

    !C2H3+ + N -> C2NH+ + H2
    k(2500) = small + (2.20e-10)

    !C2H3+ + O -> C2H2O+ + H
    k(2501) = small + (8.50e-11)

    !C2H3+ + O -> CH3+ + CO
    k(2502) = small + (5.00e-12)

    !C2H3+ + S -> HC2S+ + H2
    k(2503) = small + (1.00e-09)

    !C2H3+ + SI -> SIC2H+ + H2
    k(2504) = small + (2.00e-10)

    !C2H3+ + SI -> SIC2H2+ + H
    k(2505) = small + (2.00e-10)

    !C2H3+ + C2H -> C2H2+ + C2H2
    k(2506) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H -> C4H2+ + H2
    k(2507) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H -> C4H3+ + H
    k(2508) = small + (6.80e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + H2O -> H3O+ + C2H2
    k(2509) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + H2S -> H3S+ + C2H2
    k(2510) = small + (9.70e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + HCN -> HCNH+ + C2H2
    k(2511) = small + (7.40e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + HNC -> HCNH+ + C2H2
    k(2512) = small + (6.74e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H2 -> C4H3+ + H2
    k(2513) = small + (7.20e-10)

    !C2H3+ + C3H -> C3H2+ + C2H2
    k(2514) = small + (2.09e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H -> C5H2+ + H2
    k(2515) = small + (2.09e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H -> C5H3+ + H
    k(2516) = small + (2.09e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + NH3 -> NH4+ + C2H2
    k(2517) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H3 -> C3H3+ + CH3
    k(2518) = small + (2.65e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H3 -> H3C3+ + CH3
    k(2519) = small + (2.65e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H3 -> C4H5+ + H
    k(2520) = small + (5.30e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H2 -> C3H3+ + C2H2
    k(2521) = small + (1.01e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H2 -> C5H3+ + H2
    k(2522) = small + (1.01e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H2 -> C5H4+ + H
    k(2523) = small + (1.01e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C4H -> C4H2+ + C2H2
    k(2524) = small + (6.60e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C4H -> C6H2+ + H2
    k(2525) = small + (6.60e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C4H -> C6H3+ + H
    k(2526) = small + (6.60e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + CH4 -> C3H5+ + H2
    k(2527) = small + (2.20e-10)

    !C2H3+ + HC3N -> C3H2N+ + C2H2
    k(2528) = small + (8.12e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C2H4 -> C2H5+ + C2H2
    k(2529) = small + (9.30e-10)

    !C2H3+ + C3H3 -> C3H4+ + C2H2
    k(2530) = small + (1.29e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H3 -> C5H4+ + H2
    k(2531) = small + (1.29e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H3 -> C5H5+ + H
    k(2532) = small + (1.29e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C4H2 -> C4H3+ + C2H2
    k(2533) = small + (3.00e-10)

    !C2H3+ + C4H2 -> C6H3+ + H2
    k(2534) = small + (3.00e-10)

    !C2H3+ + C4H2 -> C6H4+ + H
    k(2535) = small + (3.00e-10)

    !C2H3+ + C5H -> C5H2+ + C2H2
    k(2536) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C5H -> C7H2+ + H2
    k(2537) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C5H -> C7H3+ + H
    k(2538) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H4 -> C3H5+ + C2H2
    k(2539) = small + (3.77e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C3H4 -> C5H5+ + H2
    k(2540) = small + (3.77e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C5H2 -> C5H3+ + C2H2
    k(2541) = small + (7.45e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C5H2 -> C7H3+ + H2
    k(2542) = small + (7.45e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C5H2 -> C7H4+ + H
    k(2543) = small + (7.45e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C6H -> C6H2+ + C2H2
    k(2544) = small + (3.34e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C6H -> C8H2+ + H2
    k(2545) = small + (3.34e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C6H -> C8H3+ + H
    k(2546) = small + (3.34e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C6H2 -> C6H3+ + C2H2
    k(2547) = small + (3.00e-10)

    !C2H3+ + C6H2 -> C8H3+ + H2
    k(2548) = small + (3.00e-10)

    !C2H3+ + C6H2 -> C8H4+ + H
    k(2549) = small + (3.00e-10)

    !C2H3+ + C7H -> C7H2+ + C2H2
    k(2550) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C7H -> C9H2+ + H2
    k(2551) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C7H -> C9H3+ + H
    k(2552) = small + (3.04e-09&
        *(T32)**(-5.00e-01))

    !C2H3+ + C7H2 -> C7H3+ + C2H2
    k(2553) = small + (6.79e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C7H2 -> C9H3+ + H2
    k(2554) = small + (6.79e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C7H2 -> C9H4+ + H
    k(2555) = small + (6.79e-10&
        *(T32)**(-5.00e-01))

    !C2H3+ + C6H6 -> C6H7+ + C2H2
    k(2556) = small + (1.60e-09)

    !C3H2+ + C -> C4H+ + H
    k(2557) = small + (1.00e-09)

    !H2C3+ + C -> C4H+ + H
    k(2558) = small + (1.00e-09)

    !C3H2+ + H -> C3H+ + H2
    k(2559) = small + (6.00e-11)

    !H2C3+ + H -> C3H+ + H2
    k(2560) = small + (6.00e-11)

    !C3H2+ + N -> C3HN+ + H
    k(2561) = small + (2.50e-10)

    !H2C3+ + N -> C3HN+ + H
    k(2562) = small + (2.50e-10)

    !C3H2+ + O -> HCO+ + C2H
    k(2563) = small + (2.00e-10)

    !H2C3+ + O -> HCO+ + C2H
    k(2564) = small + (2.00e-10)

    !C3H2+ + P -> PC3H+ + H
    k(2565) = small + (1.00e-09)

    !H2C3+ + P -> PC3H+ + H
    k(2566) = small + (1.00e-09)

    !C3H2+ + S -> HC3S+ + H
    k(2567) = small + (1.00e-09)

    !H2C3+ + S -> HC3S+ + H
    k(2568) = small + (1.00e-09)

    !C3H2+ + SI -> SIC3+ + H2
    k(2569) = small + (2.00e-10)

    !H2C3+ + SI -> SIC3+ + H2
    k(2570) = small + (2.00e-10)

    !C3H2+ + SI -> SIC3H+ + H
    k(2571) = small + (2.00e-10)

    !H2C3+ + SI -> SIC3H+ + H
    k(2572) = small + (2.00e-10)

    !C3H2+ + C2H -> C5H2+ + H
    k(2573) = small + (1.89e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C2H -> C5H2+ + H
    k(2574) = small + (1.89e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C2H2 -> C5H3+ + H
    k(2575) = small + (5.50e-10)

    !H2C3+ + C2H2 -> C5H3+ + H
    k(2576) = small + (5.50e-10)

    !C3H2+ + C3H -> C6H2+ + H
    k(2577) = small + (5.73e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C2H3 -> C5H3+ + H2
    k(2578) = small + (7.30e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C2H3 -> C5H3+ + H2
    k(2579) = small + (7.30e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C2H3 -> C5H4+ + H
    k(2580) = small + (7.30e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C2H3 -> C5H4+ + H
    k(2581) = small + (7.30e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H2 -> C6H2+ + H2
    k(2582) = small + (1.37e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H2 -> C6H3+ + H
    k(2583) = small + (1.37e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C4H -> C7H+ + H2
    k(2584) = small + (8.90e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C4H -> C7H+ + H2
    k(2585) = small + (8.90e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C4H -> C7H2+ + H
    k(2586) = small + (8.90e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C4H -> C7H2+ + H
    k(2587) = small + (8.90e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + CH4 -> C3H3+ + CH3
    k(2588) = small + (5.50e-10)

    !H2C3+ + CH4 -> C4H5+ + H
    k(2589) = small + (8.30e-11)

    !H2C3+ + CH4 -> C3H3+ + CH3
    k(2590) = small + (4.70e-10)

    !C3H2+ + C2H4 -> C4H3+ + CH3
    k(2591) = small + (3.00e-10)

    !H2C3+ + C2H4 -> C4H3+ + CH3
    k(2592) = small + (3.00e-10)

    !C3H2+ + C2H4 -> C5H5+ + H
    k(2593) = small + (3.00e-10)

    !H2C3+ + C2H4 -> C5H5+ + H
    k(2594) = small + (3.00e-10)

    !C3H2+ + C3H3 -> C6H4+ + H
    k(2595) = small + (3.54e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H3 -> C6H4+ + H
    k(2596) = small + (3.54e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C4H2 -> C7H2+ + H2
    k(2597) = small + (3.00e-10)

    !H2C3+ + C4H2 -> C7H2+ + H2
    k(2598) = small + (3.00e-10)

    !C3H2+ + C4H2 -> C7H3+ + H
    k(2599) = small + (3.00e-10)

    !H2C3+ + C4H2 -> C7H3+ + H
    k(2600) = small + (3.00e-10)

    !C3H2+ + C5H -> C8H+ + H2
    k(2601) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C5H -> C8H+ + H2
    k(2602) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C5H -> C8H2+ + H
    k(2603) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C5H -> C8H2+ + H
    k(2604) = small + (4.08e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H4 -> C4H2+ + C2H4
    k(2605) = small + (6.17e-11&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H4 -> C4H2+ + C2H4
    k(2606) = small + (6.17e-11&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H4 -> C4H3+ + C2H3
    k(2607) = small + (8.23e-11&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H4 -> C4H3+ + C2H3
    k(2608) = small + (8.23e-11&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H4 -> C4H4+ + C2H2
    k(2609) = small + (2.81e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H4 -> C4H4+ + C2H2
    k(2610) = small + (2.81e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H4 -> C5H3+ + CH3
    k(2611) = small + (1.24e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H4 -> C5H3+ + CH3
    k(2612) = small + (1.24e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C3H4 -> C6H5+ + H
    k(2613) = small + (1.37e-10&
        *(T32)**(-5.00e-01))

    !H2C3+ + C3H4 -> C6H5+ + H
    k(2614) = small + (1.37e-10&
        *(T32)**(-5.00e-01))

    !C3H2+ + C5H2 -> C8H2+ + H2
    k(2615) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C5H2 -> C8H2+ + H2
    k(2616) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C5H2 -> C8H3+ + H
    k(2617) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C5H2 -> C8H3+ + H
    k(2618) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C6H -> C9H+ + H2
    k(2619) = small + (4.59e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C6H -> C9H+ + H2
    k(2620) = small + (4.59e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C6H -> C9H2+ + H
    k(2621) = small + (4.59e-09&
        *(T32)**(-5.00e-01))

    !H2C3+ + C6H -> C9H2+ + H
    k(2622) = small + (4.59e-09&
        *(T32)**(-5.00e-01))

    !C3H2+ + C6H2 -> C9H2+ + H2
    k(2623) = small + (3.00e-10)

    !H2C3+ + C6H2 -> C9H2+ + H2
    k(2624) = small + (3.00e-10)

    !C3H2+ + C6H2 -> C9H3+ + H
    k(2625) = small + (3.00e-10)

    !H2C3+ + C6H2 -> C9H3+ + H
    k(2626) = small + (3.00e-10)

    !C3HN+ + H2 -> C2H2+ + HCN
    k(2627) = small + (2.00e-12)

    !C3HN+ + H2 -> C3H2N+ + H
    k(2628) = small + (5.00e-12)

    !C4H+ + C -> C5+ + H
    k(2629) = small + (1.00e-09)

    !C4H+ + FE -> FE+ + C4H
    k(2630) = small + (1.00e-09)

    !C4H+ + MG -> MG+ + C4H
    k(2631) = small + (1.00e-09)

    !C4H+ + NA -> NA+ + C4H
    k(2632) = small + (1.00e-09)

    !C4H+ + O -> HCO+ + C3
    k(2633) = small + (2.00e-10)

    !C4H+ + H2 -> C4H2+ + H
    k(2634) = small + (1.65e-10)

    !C4H+ + C2H -> C6+ + H2
    k(2635) = small + (9.03e-10&
        *(T32)**(-5.00e-01))

    !C4H+ + C2H -> C6H+ + H
    k(2636) = small + (9.03e-10&
        *(T32)**(-5.00e-01))

    !C4H+ + C2H2 -> C6H2+ + H
    k(2637) = small + (1.50e-09)

    !C4H+ + C3H -> C7+ + H2
    k(2638) = small + (2.71e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C3H -> C7H+ + H
    k(2639) = small + (2.71e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C2H3 -> C6H3+ + H
    k(2640) = small + (1.39e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C3H2 -> C7H2+ + H
    k(2641) = small + (2.58e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C4H -> C4H2+ + C4
    k(2642) = small + (8.35e-10&
        *(T32)**(-5.00e-01))

    !C4H+ + C4H -> C8H+ + H
    k(2643) = small + (8.35e-10&
        *(T32)**(-5.00e-01))

    !C4H+ + CH4 -> C5H3+ + H2
    k(2644) = small + (1.10e-09)

    !C4H+ + C2H4 -> C4H3+ + C2H2
    k(2645) = small + (7.50e-10)

    !C4H+ + C2H4 -> C6H4+ + H
    k(2646) = small + (7.50e-10)

    !C4H+ + C3H3 -> C7H3+ + H
    k(2647) = small + (3.33e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C4H2 -> C8H2+ + H
    k(2648) = small + (1.60e-09)

    !C4H+ + C5H -> C9H+ + H
    k(2649) = small + (7.57e-09&
        *(T32)**(-5.00e-01))

    !C4H+ + C3H4 -> C7H4+ + H
    k(2650) = small + (6.45e-10&
        *(T32)**(-5.00e-01))

    !C4H+ + C5H2 -> C9H2+ + H
    k(2651) = small + (1.85e-09&
        *(T32)**(-5.00e-01))

    !C4N+ + H2 -> C3H+ + HCN
    k(2652) = small + (2.20e-11)

    !C4N+ + H2O -> HCO+ + HC3N
    k(2653) = small + (7.50e-10)

    !C4N+ + H2O -> C3H2N+ + CO
    k(2654) = small + (7.50e-10)

    !C4N+ + C2H2 -> C5H+ + HCN
    k(2655) = small + (8.00e-10)

    !C4N+ + CH4 -> C5H2N+ + H2
    k(2656) = small + (1.00e-10)

    !C5+ + H2 -> C5H+ + H
    k(2657) = small + (6.20e-10)

    !CH4+ + H -> CH3+ + H2
    k(2658) = small + (1.00e-11)

    !CH4+ + O -> CH3+ + OH
    k(2659) = small + (1.00e-09)

    !CH4+ + CO -> HCO+ + CH3
    k(2660) = small + (1.40e-09)

    !CH4+ + H2 -> CH5+ + H
    k(2661) = small + (3.50e-11)

    !CH4+ + O2 -> O2+ + CH4
    k(2662) = small + (4.00e-10)

    !CH4+ + CO2 -> HCO2+ + CH3
    k(2663) = small + (1.20e-09)

    !CH4+ + H2O -> H3O+ + CH3
    k(2664) = small + (2.50e-09)

    !CH4+ + H2S -> H2S+ + CH4
    k(2665) = small + (1.60e-09)

    !CH4+ + H2S -> H3S+ + CH3
    k(2666) = small + (9.50e-10)

    !CH4+ + OCS -> OCS+ + CH4
    k(2667) = small + (4.20e-10)

    !CH4+ + OCS -> HOCS+ + CH3
    k(2668) = small + (9.80e-10)

    !CH4+ + C2H2 -> C2H2+ + CH4
    k(2669) = small + (1.10e-09)

    !CH4+ + C2H2 -> C2H3+ + CH3
    k(2670) = small + (1.40e-09)

    !CH4+ + C2H2 -> C3H3+ + H2 + H
    k(2671) = small + (6.25e-11)

    !CH4+ + C2H2 -> H3C3+ + H2 + H
    k(2672) = small + (6.25e-11)

    !CH4+ + H2CO -> H2CO+ + CH4
    k(2673) = small + (1.62e-09)

    !CH4+ + H2CO -> H3CO+ + CH3
    k(2674) = small + (1.98e-09)

    !CH4+ + NH3 -> NH3+ + CH4
    k(2675) = small + (6.90e-10)

    !CH4+ + NH3 -> NH4+ + CH3
    k(2676) = small + (6.60e-10)

    !CH4+ + NH3 -> CH5+ + NH2
    k(2677) = small + (3.00e-11)

    !CH4+ + CH4 -> CH5+ + CH3
    k(2678) = small + (1.50e-09)

    !CH4+ + C2H4 -> C2H4+ + CH4
    k(2679) = small + (1.38e-09)

    !CH4+ + C2H4 -> C2H5+ + CH3
    k(2680) = small + (4.23e-10)

    !CH4+ + CH4O -> CH4O+ + CH4
    k(2681) = small + (1.80e-09)

    !CH4+ + CH4O -> CH5O+ + CH3
    k(2682) = small + (1.20e-09)

    !H3CO+ + NA -> NA+ + H2CO + H
    k(2683) = small + (2.60e-09)

    !H3CO+ + CH -> CH2+ + H2CO
    k(2684) = small + (4.40e-09&
        *(T32)**(-5.00e-01))

    !H3CO+ + HNC -> HCNH+ + H2CO
    k(2685) = small + (6.52e-09&
        *(T32)**(-5.00e-01))

    !H3CO+ + NH2 -> NH3+ + H2CO
    k(2686) = small + (2.20e-09&
        *(T32)**(-5.00e-01))

    !H3CO+ + NH3 -> NH4+ + H2CO
    k(2687) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !NH4+ + C -> HCNH+ + H2
    k(2688) = small + (1.00e-11)

    !NH4+ + C -> H2NC+ + H2
    k(2689) = small + (1.00e-11)

    !SIC2H2+ + C -> SIC3H+ + H
    k(2690) = small + (2.00e-10)

    !SIC3H+ + C -> SIC4+ + H
    k(2691) = small + (2.00e-10)

    !SICH3+ + C -> SIC2H2+ + H
    k(2692) = small + (2.00e-10)

    !SICH3+ + H2O -> H3O+ + SICH2
    k(2693) = small + (2.00e-09)

    !SIH4+ + C -> CH2SI+ + H2
    k(2694) = small + (3.00e-10)

    !SIH4+ + CO -> HCO+ + SIH3
    k(2695) = small + (1.00e-09)

    !SIH4+ + H2 -> SIH5+ + H
    k(2696) = small + (1.00e-09)

    !SIH4+ + H2O -> H3O+ + SIH3
    k(2697) = small + (2.00e-09)

    !C2H3N+ + CO -> HCO+ + C2H2N
    k(2698) = small + (2.00e-09)

    !C2H4+ + C -> C3H2+ + H2
    k(2699) = small + (5.00e-10)

    !C2H4+ + C -> H2C3+ + H2
    k(2700) = small + (5.00e-10)

    !C2H4+ + C -> C3H3+ + H
    k(2701) = small + (5.00e-10)

    !C2H4+ + C -> H3C3+ + H
    k(2702) = small + (5.00e-10)

    !C2H4+ + H -> C2H3+ + H2
    k(2703) = small + (3.00e-10)

    !C2H4+ + N -> C2H2N+ + H2
    k(2704) = small + (2.50e-10)

    !C2H4+ + N -> C2H3N+ + H
    k(2705) = small + (1.00e-10)

    !C2H4+ + O -> HCO+ + CH3
    k(2706) = small + (8.40e-11)

    !C2H4+ + O -> CH3+ + HCO
    k(2707) = small + (1.08e-10)

    !C2H4+ + S -> HC2S+ + H2 + H
    k(2708) = small + (1.00e-09)

    !C2H4+ + SI -> SIC2H2+ + H2
    k(2709) = small + (2.00e-10)

    !C2H4+ + SI -> SIC2H3+ + H
    k(2710) = small + (2.00e-10)

    !C2H4+ + C2H -> C3H3+ + CH2
    k(2711) = small + (5.00e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H -> H3C3+ + CH2
    k(2712) = small + (5.00e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H -> C4H3+ + H2
    k(2713) = small + (1.00e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H2 -> C3H3+ + CH3
    k(2714) = small + (3.15e-10)

    !C2H4+ + C2H2 -> H3C3+ + CH3
    k(2715) = small + (3.15e-10)

    !C2H4+ + C2H2 -> C4H5+ + H
    k(2716) = small + (1.60e-10)

    !C2H4+ + C3H -> C4H2+ + CH3
    k(2717) = small + (4.66e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H -> C5H4+ + H
    k(2718) = small + (1.55e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + NH3 -> NH3+ + C2H4
    k(2719) = small + (8.75e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + NH3 -> NH4+ + C2H3
    k(2720) = small + (8.75e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H3 -> C2H3+ + C2H4
    k(2721) = small + (5.20e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H3 -> C2H5+ + C2H2
    k(2722) = small + (5.20e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H3 -> C4H5+ + H2
    k(2723) = small + (5.20e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H2 -> C4H3+ + CH3
    k(2724) = small + (2.23e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H2 -> C5H5+ + H
    k(2725) = small + (7.43e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C4H -> C5H2+ + CH3
    k(2726) = small + (1.47e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C4H -> C6H4+ + H
    k(2727) = small + (4.89e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + HC3N -> C3H2N+ + C2H3
    k(2728) = small + (8.03e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C2H4 -> C3H5+ + CH3
    k(2729) = small + (7.30e-10)

    !C2H4+ + C2H4 -> C4H7+ + H
    k(2730) = small + (7.20e-11)

    !C2H4+ + C3H3 -> C3H3+ + C2H4
    k(2731) = small + (6.45e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H3 -> H3C3+ + C2H4
    k(2732) = small + (6.45e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H3 -> C4H3+ + CH4
    k(2733) = small + (1.29e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H3 -> C5H5+ + H2
    k(2734) = small + (1.29e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C4H2 -> C5H3+ + CH3
    k(2735) = small + (5.00e-10)

    !C2H4+ + C4H2 -> C6H4+ + H2
    k(2736) = small + (5.00e-10)

    !C2H4+ + C5H -> C6H2+ + CH3
    k(2737) = small + (6.76e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C5H -> C7H4+ + H
    k(2738) = small + (2.25e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C3H4 -> C4H5+ + CH3
    k(2739) = small + (7.45e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C5H2 -> C6H3+ + CH3
    k(2740) = small + (1.66e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C5H2 -> C7H5+ + H
    k(2741) = small + (5.52e-10&
        *(T32)**(-5.00e-01))

    !C2H4+ + C6H -> C7H2+ + CH3
    k(2742) = small + (7.65e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C6H -> C8H4+ + H
    k(2743) = small + (2.55e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C6H2 -> C7H3+ + CH3
    k(2744) = small + (5.00e-10)

    !C2H4+ + C6H2 -> C8H4+ + H2
    k(2745) = small + (5.00e-10)

    !C2H4+ + C7H -> C8H2+ + CH3
    k(2746) = small + (6.75e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C7H -> C9H4+ + H
    k(2747) = small + (2.25e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C7H2 -> C8H3+ + CH3
    k(2748) = small + (1.58e-09&
        *(T32)**(-5.00e-01))

    !C2H4+ + C7H2 -> C9H4+ + H2
    k(2749) = small + (5.27e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C -> C4H+ + H2
    k(2750) = small + (1.00e-09)

    !H3C3+ + C -> C4H+ + H2
    k(2751) = small + (1.00e-09)

    !C3H3+ + C -> C4H2+ + H
    k(2752) = small + (1.00e-09)

    !H3C3+ + C -> C4H2+ + H
    k(2753) = small + (1.00e-09)

    !C3H3+ + O -> C2H3+ + CO
    k(2754) = small + (4.50e-11)

    !H3C3+ + O -> C2H3+ + CO
    k(2755) = small + (4.50e-11)

    !C3H3+ + S -> HC3S+ + H2
    k(2756) = small + (1.00e-09)

    !H3C3+ + S -> HC3S+ + H2
    k(2757) = small + (1.00e-09)

    !C3H3+ + SI -> SIC3H+ + H2
    k(2758) = small + (2.00e-10)

    !H3C3+ + SI -> SIC3H+ + H2
    k(2759) = small + (2.00e-10)

    !C3H3+ + SI -> SIC3H2+ + H
    k(2760) = small + (2.00e-10)

    !H3C3+ + SI -> SIC3H2+ + H
    k(2761) = small + (2.00e-10)

    !C3H3+ + C2H -> C5H2+ + H2
    k(2762) = small + (9.40e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C2H -> C5H2+ + H2
    k(2763) = small + (9.40e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C2H -> C5H3+ + H
    k(2764) = small + (9.40e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C2H -> C5H3+ + H
    k(2765) = small + (9.40e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C2H2 -> C5H3+ + H2
    k(2766) = small + (1.00e-09)

    !H3C3+ + C2H2 -> C5H3+ + H2
    k(2767) = small + (1.00e-09)

    !C3H3+ + C3H -> C6H2+ + H2
    k(2768) = small + (2.85e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C3H -> C6H3+ + H
    k(2769) = small + (2.85e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C2H3 -> C5H4+ + H2
    k(2770) = small + (1.46e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C2H3 -> C5H4+ + H2
    k(2771) = small + (1.46e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C3H2 -> C6H3+ + H2
    k(2772) = small + (1.36e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C3H2 -> C6H4+ + H
    k(2773) = small + (1.36e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C4H -> C7H2+ + H2
    k(2774) = small + (8.85e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C4H -> C7H2+ + H2
    k(2775) = small + (8.85e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C4H -> C7H3+ + H
    k(2776) = small + (8.85e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C4H -> C7H3+ + H
    k(2777) = small + (8.85e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C2H4 -> C5H5+ + H2
    k(2778) = small + (1.10e-09)

    !H3C3+ + C2H4 -> C5H5+ + H2
    k(2779) = small + (1.10e-09)

    !C3H3+ + C3H3 -> C6H4+ + H2
    k(2780) = small + (1.76e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C3H3 -> C6H4+ + H2
    k(2781) = small + (1.76e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C3H3 -> C6H5+ + H
    k(2782) = small + (1.76e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C3H3 -> C6H5+ + H
    k(2783) = small + (1.76e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C4H2 -> C7H3+ + H2
    k(2784) = small + (1.00e-09)

    !H3C3+ + C4H2 -> C7H3+ + H2
    k(2785) = small + (1.00e-09)

    !C3H3+ + C5H -> C8H2+ + H2
    k(2786) = small + (4.05e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C5H -> C8H2+ + H2
    k(2787) = small + (4.05e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C5H -> C8H3+ + H
    k(2788) = small + (4.05e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C5H -> C8H3+ + H
    k(2789) = small + (4.05e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C3H4 -> C6H5+ + H2
    k(2790) = small + (6.81e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C3H4 -> C6H5+ + H2
    k(2791) = small + (6.81e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C5H2 -> C8H3+ + H2
    k(2792) = small + (9.91e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C5H2 -> C8H3+ + H2
    k(2793) = small + (9.91e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C5H2 -> C8H4+ + H
    k(2794) = small + (9.91e-10&
        *(T32)**(-5.00e-01))

    !H3C3+ + C5H2 -> C8H4+ + H
    k(2795) = small + (9.91e-10&
        *(T32)**(-5.00e-01))

    !C3H3+ + C6H -> C9H2+ + H2
    k(2796) = small + (4.55e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C6H -> C9H2+ + H2
    k(2797) = small + (4.55e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C6H -> C9H3+ + H
    k(2798) = small + (4.55e-09&
        *(T32)**(-5.00e-01))

    !H3C3+ + C6H -> C9H3+ + H
    k(2799) = small + (4.55e-09&
        *(T32)**(-5.00e-01))

    !C3H3+ + C6H2 -> C9H3+ + H2
    k(2800) = small + (1.00e-09)

    !H3C3+ + C6H2 -> C9H3+ + H2
    k(2801) = small + (1.00e-09)

    !C4H2+ + C -> C5+ + H2
    k(2802) = small + (5.00e-10)

    !C4H2+ + C -> C5H+ + H
    k(2803) = small + (5.00e-10)

    !C4H2+ + N -> HC4N+ + H
    k(2804) = small + (1.00e-18)

    !C4H2+ + O -> HC4O+ + H
    k(2805) = small + (1.35e-10)

    !C4H2+ + O -> C3H2+ + CO
    k(2806) = small + (5.50e-11)

    !C4H2+ + O -> H2C3+ + CO
    k(2807) = small + (5.50e-11)

    !C4H2+ + P -> PC4H+ + H
    k(2808) = small + (1.00e-09)

    !C4H2+ + S -> HC4S+ + H
    k(2809) = small + (1.00e-09)

    !C4H2+ + SI -> SIC4+ + H2
    k(2810) = small + (2.00e-10)

    !C4H2+ + SI -> SIC4H+ + H
    k(2811) = small + (2.00e-10)

    !C4H2+ + C2H -> C6H2+ + H
    k(2812) = small + (1.80e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C2H2 -> C6H3+ + H
    k(2813) = small + (7.00e-12)

    !C4H2+ + C3H -> C7H2+ + H
    k(2814) = small + (5.38e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C2H3 -> C6H3+ + H2
    k(2815) = small + (2.78e-10&
        *(T32)**(-5.00e-01))

    !C4H2+ + C2H3 -> C6H4+ + H
    k(2816) = small + (1.11e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C3H2 -> C7H3+ + H
    k(2817) = small + (2.57e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C4H -> C8H2+ + H
    k(2818) = small + (1.66e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + CH4 -> C5H4+ + H2
    k(2819) = small + (2.00e-10)

    !C4H2+ + CH4 -> C5H5+ + H
    k(2820) = small + (8.00e-10)

    !C4H2+ + C2H4 -> C6H4+ + H2
    k(2821) = small + (2.00e-10)

    !C4H2+ + C2H4 -> C6H5+ + H
    k(2822) = small + (8.00e-10)

    !C4H2+ + C3H3 -> C7H3+ + H2
    k(2823) = small + (6.63e-10&
        *(T32)**(-5.00e-01))

    !C4H2+ + C3H3 -> C7H4+ + H
    k(2824) = small + (2.65e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C4H2 -> C6H2+ + C2H2
    k(2825) = small + (1.00e-09)

    !C4H2+ + C4H2 -> C8H2+ + H2
    k(2826) = small + (1.00e-09)

    !C4H2+ + C5H -> C9H2+ + H
    k(2827) = small + (7.53e-09&
        *(T32)**(-5.00e-01))

    !C4H2+ + C3H4 -> C7H5+ + H
    k(2828) = small + (6.42e-10&
        *(T32)**(-5.00e-01))

    !C4H2+ + C5H2 -> C9H3+ + H
    k(2829) = small + (1.84e-09&
        *(T32)**(-5.00e-01))

    !C5H+ + C -> C6+ + H
    k(2830) = small + (1.00e-09)

    !C5H+ + N -> C5N+ + H
    k(2831) = small + (2.00e-10)

    !C5H+ + O -> HCO+ + C4
    k(2832) = small + (2.00e-10)

    !C5H+ + H2 -> C5H2+ + H
    k(2833) = small + (1.00e-17)

    !C5N+ + H2 -> C5HN+ + H
    k(2834) = small + (1.50e-09)

    !C6+ + H2 -> C6H+ + H
    k(2835) = small + (5.40e-11)

    !CH5+ + C -> CH+ + CH4
    k(2836) = small + (1.00e-09)

    !CH5+ + C -> C2H3+ + H2
    k(2837) = small + (1.00e-09)

    !CH5+ + C -> C2H4+ + H
    k(2838) = small + (1.00e-09)

    !CH5+ + H -> CH4+ + H2
    k(2839) = small + (1.50e-10&
        *exp(-4.81e+02*invT))

    !CH5+ + MG -> MG+ + CH4 + H
    k(2840) = small + (1.40e-09)

    !CH5+ + O -> H3O+ + CH2
    k(2841) = small + (2.20e-10)

    !CH5+ + O -> H3CO+ + H2
    k(2842) = small + (4.40e-12)

    !CH5+ + S -> HS+ + CH4
    k(2843) = small + (1.30e-09)

    !CH5+ + SI -> SICH3+ + H2
    k(2844) = small + (2.00e-10)

    !CH5+ + SI -> SICH4+ + H
    k(2845) = small + (2.00e-10)

    !CH5+ + C2 -> C2H+ + CH4
    k(2846) = small + (9.50e-10)

    !CH5+ + CH -> CH2+ + CH4
    k(2847) = small + (4.90e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + CO -> HCO+ + CH4
    k(2848) = small + (3.16e-10&
        *(T32)**(-5.00e-01))

    !CH5+ + NH -> NH2+ + CH4
    k(2849) = small + (4.20e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + OH -> H2O+ + CH4
    k(2850) = small + (5.20e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + C2H -> C2H2+ + CH4
    k(2851) = small + (2.30e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + CH2 -> CH3+ + CH4
    k(2852) = small + (7.97e-10&
        *(T32)**(-5.00e-01))

    !CH5+ + H2O -> H3O+ + CH4
    k(2853) = small + (2.40e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + HCN -> HCNH+ + CH4
    k(2854) = small + (8.50e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + HCO -> H2CO+ + CH4
    k(2855) = small + (1.20e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + HNC -> HCNH+ + CH4
    k(2856) = small + (7.67e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + NH2 -> NH3+ + CH4
    k(2857) = small + (2.50e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + C2H2 -> C2H3+ + CH4
    k(2858) = small + (1.60e-09)

    !CH5+ + H2CO -> H3CO+ + CH4
    k(2859) = small + (2.70e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + NH3 -> NH4+ + CH4
    k(2860) = small + (2.00e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + HC3N -> C3H2N+ + CH4
    k(2861) = small + (9.55e-09&
        *(T32)**(-5.00e-01))

    !CH5+ + C6H6 -> C6H7+ + CH4
    k(2862) = small + (2.00e-09)

    !HC4N+ + H2 -> H2C4N+ + H
    k(2863) = small + (1.00e-09)

    !SIC2H3+ + C -> SIC3H2+ + H
    k(2864) = small + (2.00e-10)

    !SIC3H2+ + C -> SIC4H+ + H
    k(2865) = small + (2.00e-10)

    !SICH4+ + C -> SIC2H3+ + H
    k(2866) = small + (2.00e-10)

    !SIH5+ + C -> SICH4+ + H
    k(2867) = small + (2.00e-10)

    !SIH5+ + O -> H3SIO+ + H2
    k(2868) = small + (1.00e-10)

    !SIH5+ + H2O -> H3O+ + SIH4
    k(2869) = small + (2.00e-09)

    !C2H5+ + C -> C3H3+ + H2
    k(2870) = small + (5.00e-10)

    !C2H5+ + C -> H3C3+ + H2
    k(2871) = small + (5.00e-10)

    !C2H5+ + C -> C3H4+ + H
    k(2872) = small + (1.00e-09)

    !C2H5+ + H -> C2H4+ + H2
    k(2873) = small + (1.00e-11)

    !C2H5+ + O -> HCO+ + CH4
    k(2874) = small + (1.00e-11)

    !C2H5+ + O -> C2H4O+ + H
    k(2875) = small + (1.00e-11)

    !C2H5+ + H2O -> H3O+ + C2H4
    k(2876) = small + (2.15e-09&
        *(T32)**(-5.00e-01))

    !C2H5+ + H2S -> H3S+ + C2H4
    k(2877) = small + (9.50e-10&
        *(T32)**(-5.00e-01))

    !C2H5+ + HCN -> HCNH+ + C2H4
    k(2878) = small + (7.31e-09&
        *(T32)**(-5.00e-01))

    !C2H5+ + HNC -> HCNH+ + C2H4
    k(2879) = small + (6.62e-09&
        *(T32)**(-5.00e-01))

    !C2H5+ + C2H2 -> C3H3+ + CH4
    k(2880) = small + (3.40e-11)

    !C2H5+ + C2H2 -> H3C3+ + CH4
    k(2881) = small + (3.40e-11)

    !C2H5+ + C2H2 -> C4H5+ + H2
    k(2882) = small + (1.20e-10)

    !C2H5+ + H2CO -> H3CO+ + C2H4
    k(2883) = small + (2.35e-09&
        *(T32)**(-5.00e-01))

    !C2H5+ + NH3 -> NH4+ + C2H4
    k(2884) = small + (1.74e-09&
        *(T32)**(-5.00e-01))

    !C2H5+ + C2H4 -> C3H5+ + CH4
    k(2885) = small + (3.90e-10)

    !C3H4+ + C -> C4H2+ + H2
    k(2886) = small + (1.00e-09)

    !C3H4+ + C -> C4H3+ + H
    k(2887) = small + (1.00e-09)

    !C3H4+ + H -> C3H3+ + H2
    k(2888) = small + (1.50e-10)

    !C3H4+ + H -> H3C3+ + H2
    k(2889) = small + (1.50e-10)

    !C3H4+ + N -> C3H2N+ + H2
    k(2890) = small + (1.00e-10)

    !C3H4+ + N -> C3H3N+ + H
    k(2891) = small + (1.00e-10)

    !C3H4+ + O -> HCO+ + C2H3
    k(2892) = small + (2.00e-10)

    !C3H4+ + C2H2 -> C5H5+ + H
    k(2893) = small + (4.90e-10)

    !C3H4+ + C3H4 -> C6H7+ + H
    k(2894) = small + (7.50e-10)

    !C4H3+ + C -> C5H+ + H2
    k(2895) = small + (5.00e-10)

    !C4H3+ + C -> C5H2+ + H
    k(2896) = small + (5.00e-10)

    !C4H3+ + N -> H2C4N+ + H
    k(2897) = small + (1.00e-18)

    !C4H3+ + O -> HCO+ + C3H2
    k(2898) = small + (2.50e-11)

    !C4H3+ + S -> HC4S+ + H2
    k(2899) = small + (1.00e-09)

    !C4H3+ + C2H -> C6H3+ + H
    k(2900) = small + (1.79e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C3H -> C7H3+ + H
    k(2901) = small + (5.36e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C2H3 -> C6H4+ + H2
    k(2902) = small + (6.93e-10&
        *(T32)**(-5.00e-01))

    !C4H3+ + C2H3 -> C6H5+ + H
    k(2903) = small + (6.93e-10&
        *(T32)**(-5.00e-01))

    !C4H3+ + C3H2 -> C7H4+ + H
    k(2904) = small + (2.56e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C4H -> C8H3+ + H
    k(2905) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + CH4 -> C5H5+ + H2
    k(2906) = small + (5.00e-10)

    !C4H3+ + C3H3 -> C7H4+ + H2
    k(2907) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C3H3 -> C7H5+ + H
    k(2908) = small + (1.65e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C4H2 -> C6H3+ + C2H2
    k(2909) = small + (1.00e-10)

    !C4H3+ + C4H2 -> C8H4+ + H
    k(2910) = small + (5.00e-10)

    !C4H3+ + C5H -> C9H3+ + H
    k(2911) = small + (7.49e-09&
        *(T32)**(-5.00e-01))

    !C4H3+ + C3H4 -> C5H5+ + C2H2
    k(2912) = small + (6.39e-10&
        *(T32)**(-5.00e-01))

    !C4H3+ + C5H2 -> C9H3+ + H2
    k(2913) = small + (9.17e-10&
        *(T32)**(-5.00e-01))

    !C4H3+ + C5H2 -> C9H4+ + H
    k(2914) = small + (9.17e-10&
        *(T32)**(-5.00e-01))

    !C5H2+ + C -> C6+ + H2
    k(2915) = small + (5.00e-10)

    !C5H2+ + C -> C6H+ + H
    k(2916) = small + (5.00e-10)

    !C5H2+ + N -> C5HN+ + H
    k(2917) = small + (2.00e-10)

    !C5H2+ + O -> HCO+ + C4H
    k(2918) = small + (2.00e-10)

    !C5H2+ + C2H -> C7H2+ + H
    k(2919) = small + (1.74e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + C2H2 -> C7H2+ + H2
    k(2920) = small + (3.00e-10)

    !C5H2+ + C2H2 -> C7H3+ + H
    k(2921) = small + (7.00e-10)

    !C5H2+ + C3H -> C8H2+ + H
    k(2922) = small + (5.16e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + C2H3 -> C7H3+ + H2
    k(2923) = small + (8.93e-10&
        *(T32)**(-5.00e-01))

    !C5H2+ + C2H3 -> C7H4+ + H
    k(2924) = small + (4.47e-10&
        *(T32)**(-5.00e-01))

    !C5H2+ + C3H2 -> C8H2+ + H2
    k(2925) = small + (8.25e-10&
        *(T32)**(-5.00e-01))

    !C5H2+ + C3H2 -> C8H3+ + H
    k(2926) = small + (1.64e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + C4H -> C9H2+ + H
    k(2927) = small + (1.58e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + CH4 -> C6H4+ + H2
    k(2928) = small + (2.00e-10)

    !C5H2+ + CH4 -> C6H5+ + H
    k(2929) = small + (8.00e-10)

    !C5H2+ + C2H4 -> C7H4+ + H2
    k(2930) = small + (5.00e-10)

    !C5H2+ + C2H4 -> C7H5+ + H
    k(2931) = small + (5.00e-10)

    !C5H2+ + C3H3 -> C8H3+ + H2
    k(2932) = small + (1.59e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + C3H3 -> C8H4+ + H
    k(2933) = small + (1.59e-09&
        *(T32)**(-5.00e-01))

    !C5H2+ + C4H2 -> C7H3+ + C2H
    k(2934) = small + (6.00e-10)

    !C5H2+ + C4H2 -> C9H3+ + H
    k(2935) = small + (1.00e-09)

    !C5H2+ + C3H4 -> C8H4+ + H2
    k(2936) = small + (6.14e-10&
        *(T32)**(-5.00e-01))

    !C5HN+ + H2 -> C5H2N+ + H
    k(2937) = small + (5.00e-12)

    !C6H+ + C -> C7+ + H
    k(2938) = small + (1.00e-09)

    !C6H+ + O -> HCO+ + C5
    k(2939) = small + (2.00e-10)

    !C6H+ + H2 -> C6H2+ + H
    k(2940) = small + (1.30e-12)

    !C7+ + H2 -> C7H+ + H
    k(2941) = small + (1.90e-10)

    !CH5O+ + H2CO -> H5C2O2+ + H2
    k(2942) = small + (1.00e-09)

    !CH5O+ + CH4O -> CH3OCH4+ + H2O
    k(2943) = small + (1.00e-10&
        *(T32)**(-1.00e+00))

    !H2C4N+ + H2 -> H3C4N+ + H
    k(2944) = small + (1.00e-09)

    !C2H6+ + H -> C2H5+ + H2
    k(2945) = small + (1.00e-10)

    !C3H5+ + C -> C4H3+ + H2
    k(2946) = small + (2.00e-09)

    !C3H5+ + N -> C2H4+ + HCN
    k(2947) = small + (1.00e-10)

    !C3H5+ + N -> C3H3N+ + H2
    k(2948) = small + (1.00e-10)

    !C3H5+ + O -> HCO+ + C2H4
    k(2949) = small + (2.00e-10)

    !C3H5+ + C6H6 -> C6H7+ + C3H4
    k(2950) = small + (1.15e-10)

    !C4H4+ + C2H2 -> C6H4+ + H2
    k(2951) = small + (1.20e-11)

    !C4H4+ + C2H2 -> C6H5+ + H
    k(2952) = small + (9.00e-11)

    !C4H4+ + C4H2 -> C6H4+ + C2H2
    k(2953) = small + (7.00e-10)

    !C5H3+ + C -> C6H+ + H2
    k(2954) = small + (5.00e-10)

    !C5H3+ + C -> C6H2+ + H
    k(2955) = small + (5.00e-10)

    !C5H3+ + N -> C5H2N+ + H
    k(2956) = small + (2.00e-10)

    !C5H3+ + O -> HCO+ + C4H2
    k(2957) = small + (2.00e-10)

    !C6H2+ + C -> C7+ + H2
    k(2958) = small + (5.00e-10)

    !C6H2+ + C -> C7H+ + H
    k(2959) = small + (5.00e-10)

    !C6H2+ + O -> HCO+ + C5H
    k(2960) = small + (2.00e-10)

    !C6H2+ + C2H -> C8H2+ + H
    k(2961) = small + (1.70e-09&
        *(T32)**(-5.00e-01))

    !C6H2+ + C3H -> C9H2+ + H
    k(2962) = small + (5.00e-09&
        *(T32)**(-5.00e-01))

    !C6H2+ + C2H3 -> C8H3+ + H2
    k(2963) = small + (8.72e-10&
        *(T32)**(-5.00e-01))

    !C6H2+ + C2H3 -> C8H4+ + H
    k(2964) = small + (4.36e-10&
        *(T32)**(-5.00e-01))

    !C6H2+ + C3H2 -> C9H2+ + H2
    k(2965) = small + (7.98e-10&
        *(T32)**(-5.00e-01))

    !C6H2+ + C3H2 -> C9H3+ + H
    k(2966) = small + (1.60e-09&
        *(T32)**(-5.00e-01))

    !C6H2+ + CH4 -> C7H4+ + H2
    k(2967) = small + (2.00e-10)

    !C6H2+ + CH4 -> C7H5+ + H
    k(2968) = small + (8.00e-10)

    !C6H2+ + C2H4 -> C8H4+ + H2
    k(2969) = small + (1.00e-09)

    !C6H2+ + C3H3 -> C9H3+ + H2
    k(2970) = small + (1.54e-09&
        *(T32)**(-5.00e-01))

    !C6H2+ + C3H3 -> C9H4+ + H
    k(2971) = small + (1.54e-09&
        *(T32)**(-5.00e-01))

    !C6H2+ + C3H4 -> C9H4+ + H2
    k(2972) = small + (5.94e-10&
        *(T32)**(-5.00e-01))

    !C7H+ + C -> C8+ + H
    k(2973) = small + (1.00e-09)

    !C7H+ + N -> C7N+ + H
    k(2974) = small + (2.00e-10)

    !C7H+ + O -> HCO+ + C6
    k(2975) = small + (2.00e-10)

    !C7H+ + H2 -> C7H2+ + H
    k(2976) = small + (1.00e-17)

    !C7N+ + H2 -> C7HN+ + H
    k(2977) = small + (1.50e-09)

    !C8+ + H2 -> C8H+ + H
    k(2978) = small + (4.70e-12)

    !H3C4N+ + H2 -> C4H4N+ + H
    k(2979) = small + (1.00e-09)

    !C4H4N+ + C -> C5H3N+ + H
    k(2980) = small + (1.00e-09)

    !C4H5+ + N -> C4H4N+ + H
    k(2981) = small + (1.00e-10)

    !C5H4+ + N -> C5H3N+ + H
    k(2982) = small + (1.00e-10)

    !C6H3+ + C -> C7H+ + H2
    k(2983) = small + (5.00e-10)

    !C6H3+ + C -> C7H2+ + H
    k(2984) = small + (5.00e-10)

    !C6H3+ + O -> HCO+ + C5H2
    k(2985) = small + (2.00e-10)

    !C7H2+ + C -> C8+ + H2
    k(2986) = small + (5.00e-10)

    !C7H2+ + C -> C8H+ + H
    k(2987) = small + (5.00e-10)

    !C7H2+ + N -> C7HN+ + H
    k(2988) = small + (2.00e-10)

    !C7H2+ + O -> HCO+ + C6H
    k(2989) = small + (2.00e-10)

    !C7H2+ + C2H -> C9H2+ + H
    k(2990) = small + (1.67e-09&
        *(T32)**(-5.00e-01))

    !C7H2+ + C2H2 -> C9H2+ + H2
    k(2991) = small + (3.00e-10)

    !C7H2+ + C2H2 -> C9H3+ + H
    k(2992) = small + (7.00e-10)

    !C7H2+ + C2H3 -> C9H3+ + H2
    k(2993) = small + (8.53e-10&
        *(T32)**(-5.00e-01))

    !C7H2+ + C2H3 -> C9H4+ + H
    k(2994) = small + (4.27e-10&
        *(T32)**(-5.00e-01))

    !C7H2+ + CH4 -> C8H4+ + H2
    k(2995) = small + (1.00e-09)

    !C7H2+ + C2H4 -> C9H4+ + H2
    k(2996) = small + (1.00e-09)

    !C7HN+ + H2 -> C7H2N+ + H
    k(2997) = small + (5.00e-12)

    !C8H+ + H2 -> C8H2+ + H
    k(2998) = small + (1.00e-09)

    !C9+ + H2 -> C9H+ + H
    k(2999) = small + (4.10e-11)

    !C10+ + H2 -> C10H+ + H
    k(3000) = small + (4.10e-11)

    !C10H+ + H2 -> C10H2+ + H
    k(3001) = small + (4.10e-11)

    !C5H5+ + N -> C5H3N+ + H2
    k(3002) = small + (1.00e-10)

    !C7H3+ + C -> C8H+ + H2
    k(3003) = small + (5.00e-10)

    !C7H3+ + C -> C8H2+ + H
    k(3004) = small + (5.00e-10)

    !C7H3+ + N -> C7H2N+ + H
    k(3005) = small + (2.00e-10)

    !C7H3+ + O -> HCO+ + C6H2
    k(3006) = small + (2.00e-10)

    !C8H2+ + C -> C9+ + H2
    k(3007) = small + (5.00e-10)

    !C8H2+ + C -> C9H+ + H
    k(3008) = small + (5.00e-10)

    !C8H2+ + O -> HCO+ + C7H
    k(3009) = small + (2.00e-10)

    !C8H2+ + CH4 -> C9H4+ + H2
    k(3010) = small + (1.00e-09)

    !C9H+ + C -> C10+ + H
    k(3011) = small + (1.00e-09)

    !C9H+ + N -> C9N+ + H
    k(3012) = small + (2.00e-10)

    !C9H+ + O -> HCO+ + C8
    k(3013) = small + (2.00e-10)

    !C9H+ + H2 -> C9H2+ + H
    k(3014) = small + (1.00e-17)

    !C9N+ + H2 -> C9HN+ + H
    k(3015) = small + (1.50e-09)

    !C6H4N+ + C -> C7H3N+ + H
    k(3016) = small + (1.00e-09)

    !C6H5+ + N -> C6H4N+ + H
    k(3017) = small + (1.00e-10)

    !C6H5+ + C2H4 -> C6H7+ + C2H2
    k(3018) = small + (8.50e-11)

    !C6H5+ + O -> C5H5+ + CO
    k(3019) = small + (6.00e-11)

    !C6H5+ + O2 -> C4H5+ + CO + CO
    k(3020) = small + (4.90e-11)

    !C7H4+ + N -> C7H3N+ + H
    k(3021) = small + (1.00e-10)

    !C8H3+ + C -> C9H+ + H2
    k(3022) = small + (5.00e-10)

    !C8H3+ + C -> C9H2+ + H
    k(3023) = small + (5.00e-10)

    !C8H3+ + O -> HCO+ + C7H2
    k(3024) = small + (2.00e-10)

    !C9H2+ + C -> C10+ + H2
    k(3025) = small + (1.00e-09)

    !C9H2+ + N -> C9HN+ + H
    k(3026) = small + (2.00e-10)

    !C9H2+ + O -> HCO+ + C8H
    k(3027) = small + (2.00e-10)

    !C9HN+ + H2 -> C9H2N+ + H
    k(3028) = small + (5.00e-12)

    !C7H5+ + N -> C7H3N+ + H2
    k(3029) = small + (1.00e-10)

    !C9H3+ + N -> C9H2N+ + H
    k(3030) = small + (2.00e-10)

    !C9H3+ + O -> HCO+ + C8H2
    k(3031) = small + (2.00e-10)

    !C8H4N+ + C -> C9H3N+ + H
    k(3032) = small + (1.00e-09)

    !C9H4+ + N -> C9H3N+ + H
    k(3033) = small + (1.00e-10)

    !CO+ + H2 -> HOC+ + H
    k(3034) = small + (7.50e-10)

    !SO2+ + H -> SO+ + OH
    k(3035) = small + (4.20e-10)

    !CN+ + H2 -> HNC+ + H
    k(3036) = small + (7.50e-10)

    !C2N2+ + H -> HNC+ + CN
    k(3037) = small + (5.00e-10)

    !C2N2+ + H -> C2H+ + N2
    k(3038) = small + (1.20e-10)

    !C- + NO -> CN- + O
    k(3039) = small + (1.00e-09)

    !C- + O2 -> O- + CO
    k(3040) = small + (4.00e-10)

    !C- + CO2 -> CO + CO + E
    k(3041) = small + (4.70e-11)

    !H- + H2O -> OH- + H2
    k(3042) = small + (3.80e-09)

    !H- + HCN -> CN- + H2
    k(3043) = small + (3.80e-09)

    !O- + CN -> CN- + O
    k(3044) = small + (1.00e-09)

    !O- + H2 -> OH- + H
    k(3045) = small + (3.00e-11)

    !O- + HCN -> CN- + OH
    k(3046) = small + (1.20e-09)

    !O- + CH4 -> OH- + CH3
    k(3047) = small + (1.00e-10)

    !OH- + CN -> CN- + OH
    k(3048) = small + (1.00e-09)

    !OH- + HCN -> CN- + H2O
    k(3049) = small + (1.20e-09)

    !C+ + H -> CH+
    k(3050) = small + (1.70e-17)

    !C+ + O -> CO+
    k(3051) = small + (2.50e-18)

    !C+ + H2 -> CH2+
    k(3052) = small + (4.00e-16&
        *(T32)**(-2.00e-01))

    !C+ + C3 -> C4+
    k(3053) = small + (1.00e-13&
        *(T32)**(-1.00e+00))

    !C+ + C4 -> C5+
    k(3054) = small + (1.00e-09)

    !C+ + C5 -> C6+
    k(3055) = small + (1.00e-09)

    !C+ + C6 -> C7+
    k(3056) = small + (1.00e-09)

    !C+ + C7 -> C8+
    k(3057) = small + (1.00e-09)

    !C+ + C8 -> C9+
    k(3058) = small + (1.00e-09)

    !C+ + C9 -> C10+
    k(3059) = small + (1.00e-09)

    !H+ + H -> H2+
    k(3060) = small + (2.00e-20&
        *(T32)**(1.00e+00))

    !NA+ + H2 -> NAH2+
    k(3061) = small + (4.00e-19)

    !NA+ + H2O -> NAH2O+
    k(3062) = small + (1.40e-17&
        *(T32)**(-1.30e+00))

    !P+ + H2 -> PH2+
    k(3063) = small + (7.50e-18&
        *(T32)**(-1.30e+00))

    !S+ + H2 -> H2S+
    k(3064) = small + (1.00e-17&
        *(T32)**(-2.00e-01))

    !SI+ + H -> SIH+
    k(3065) = small + (1.00e-17)

    !SI+ + O -> SIO+
    k(3066) = small + (1.00e-17)

    !SI+ + H2 -> SIH2+
    k(3067) = small + (1.00e-15)

    !SI+ + HCN -> SINCH+
    k(3068) = small + (6.00e-15&
        *(T32)**(-1.50e+00))

    !SI+ + C2H2 -> SIC2H2+
    k(3069) = small + (1.00e-13&
        *(T32)**(-1.00e+00))

    !SI+ + CH4 -> SICH4+
    k(3070) = small + (4.00e-16&
        *(T32)**(-1.50e+00))

    !HS+ + H2 -> H3S+
    k(3071) = small + (1.40e-16&
        *(T32)**(-6.00e-01))

    !NO+ + H2 -> H2NO+
    k(3072) = small + (6.70e-20&
        *(T32)**(-1.00e+00))

    !PH+ + H2 -> PH3+
    k(3073) = small + (2.40e-17&
        *(T32)**(-1.40e+00))

    !SIH+ + H2 -> SIH3+
    k(3074) = small + (3.00e-17&
        *(T32)**(-1.00e+00))

    !C3+ + H -> C3H+
    k(3075) = small + (7.00e-16&
        *(T32)**(-1.50e+00))

    !HCO+ + H2O -> CH3O2+
    k(3076) = small + (4.00e-13&
        *(T32)**(-1.30e+00))

    !HCO+ + CH4 -> C2H5O+
    k(3077) = small + (1.00e-17)

    !OCS+ + H2 -> OCS+H2
    k(3078) = small + (6.10e-20&
        *(T32)**(-1.50e+00))

    !C2H2+ + H -> C2H3+
    k(3079) = small + (7.00e-15&
        *(T32)**(-1.50e+00))

    !C2H2+ + CO -> H2C3O+
    k(3080) = small + (1.10e-15&
        *(T32)**(-2.00e+00))

    !C2H2+ + H2 -> C2H4+
    k(3081) = small + (1.50e-14&
        *(T32)**(-1.00e+00))

    !C2H2+ + HC3N -> C5H3N+
    k(3082) = small + (2.00e-12&
        *(T32)**(-2.50e+00))

    !C2H2+ + HC5N -> C7H3N+
    k(3083) = small + (2.00e-12&
        *(T32)**(-2.50e+00))

    !C2H2+ + HC7N -> C9H3N+
    k(3084) = small + (2.00e-12&
        *(T32)**(-2.50e+00))

    !C3H+ + H -> C3H2+
    k(3085) = small + (1.00e-14&
        *(T32)**(-1.50e+00))

    !C3H+ + H -> H2C3+
    k(3086) = small + (1.00e-14&
        *(T32)**(-1.50e+00))

    !C3H+ + CO -> HC4O+
    k(3087) = small + (1.10e-14&
        *(T32)**(-2.00e+00))

    !C3H+ + H2 -> H3C3+
    k(3088) = small + (1.65e-13&
        *(T32)**(-1.00e+00))

    !C3H+ + H2 -> C3H3+
    k(3089) = small + (1.65e-13&
        *(T32)**(-1.00e+00))

    !C3H+ + C2H2 -> C5H3+
    k(3090) = small + (2.90e-12&
        *(T32)**(-1.00e+00))

    !CH3+ + CO -> C2H3O+
    k(3091) = small + (1.20e-13&
        *(T32)**(-1.30e+00))

    !CH3+ + H2 -> CH5+
    k(3092) = small + (1.30e-14&
        *(T32)**(-1.00e+00))

    !CH3+ + H2O -> CH5O+
    k(3093) = small + (2.00e-12)

    !CH3+ + HCN -> C2H4N+
    k(3094) = small + (9.00e-09&
        *(T32)**(-5.00e-01))

    !CH3+ + NH3 -> CH6N+
    k(3095) = small + (9.40e-10&
        *(T32)**(-9.00e-01))

    !CH3+ + HC3N -> C4H4N+
    k(3096) = small + (8.60e-11&
        *(T32)**(-1.40e+00))

    !CH3+ + CH4O -> CH3OCH4+
    k(3097) = small + (7.80e-12&
        *(T32)**(-1.10e+00))

    !CH3+ + C2H4O -> C3H6OH+
    k(3098) = small + (5.70e-11&
        *(T32)**(-6.60e-01))

    !CH3+ + HC5N -> C6H4N+
    k(3099) = small + (8.60e-11&
        *(T32)**(-1.40e+00))

    !CH3+ + HC7N -> C8H4N+
    k(3100) = small + (8.60e-11&
        *(T32)**(-1.40e+00))

    !HCNH+ + C2H2 -> C3H4N+
    k(3101) = small + (2.20e-15&
        *(T32)**(-2.00e+00))

    !H3O+ + C2H2 -> C2H5O+
    k(3102) = small + (2.70e-14&
        *(T32)**(-1.60e+00))

    !H3O+ + C2H4 -> C2H5OH2+
    k(3103) = small + (2.40e-14&
        *(T32)**(-2.80e+00))

    !SIC2H+ + H2 -> SIC2H3+
    k(3104) = small + (3.00e-16&
        *(T32)**(-1.00e+00))

    !SIH3+ + H2 -> SIH5+
    k(3105) = small + (1.00e-18&
        *(T32)**(-5.00e-01))

    !C2H3+ + CO -> H3C3O+
    k(3106) = small + (2.00e-15&
        *(T32)**(-2.50e+00))

    !C3H2+ + H -> C3H3+
    k(3107) = small + (4.00e-15&
        *(T32)**(-1.50e+00))

    !H2C3+ + H -> H3C3+
    k(3108) = small + (4.00e-15&
        *(T32)**(-1.50e+00))

    !C4H+ + H -> C4H2+
    k(3109) = small + (6.00e-14&
        *(T32)**(-1.50e+00))

    !H3CO+ + CH4 -> CH3OCH4+
    k(3110) = small + (1.00e-17)

    !C3H2N+ + C2H2 -> C5H4N+
    k(3111) = small + (1.00e-09)

    !C3H3+ + C4H2 -> C7H5+
    k(3112) = small + (1.00e-13&
        *(T32)**(-2.50e+00))

    !H3C3+ + C4H2 -> C7H5+
    k(3113) = small + (1.00e-13&
        *(T32)**(-2.50e+00))

    !C4H2+ + H -> C4H3+
    k(3114) = small + (7.00e-11&
        *(T32)**(-1.00e-01))

    !C4H2+ + C2H2 -> C6H4+
    k(3115) = small + (1.00e-09)

    !C4H2+ + HC3N -> C7H3N+
    k(3116) = small + (2.00e-12&
        *(T32)**(-2.50e+00))

    !C4H2+ + C4H2 -> C8H4+
    k(3117) = small + (1.00e-13&
        *(T32)**(-2.00e+00))

    !C4H2+ + HC5N -> C9H3N+
    k(3118) = small + (2.00e-12&
        *(T32)**(-2.50e+00))

    !C5H+ + C4H2 -> C9H3+
    k(3119) = small + (1.00e-13&
        *(T32)**(-2.00e+00))

    !C6+ + H2 -> C6H2+
    k(3120) = small + (5.20e-14&
        *(T32)**(-5.00e-01))

    !CH5+ + CO -> C2H5O+
    k(3121) = small + (1.00e-17)

    !C2H5+ + H2O -> C2H5OH2+
    k(3122) = small + (4.10e-16&
        *(T32)**(-2.40e+00))

    !C4H3+ + H -> C4H4+
    k(3123) = small + (6.00e-14&
        *(T32)**(-7.00e-01))

    !C4H3+ + C2H2 -> C6H5+
    k(3124) = small + (1.00e-09)

    !C4H3+ + C4H2 -> C8H5+
    k(3125) = small + (1.00e-13&
        *(T32)**(-2.50e+00))

    !C5H2+ + C4H2 -> C9H4+
    k(3126) = small + (1.00e-13&
        *(T32)**(-2.00e+00))

    !C5H3+ + C4H2 -> C9H5+
    k(3127) = small + (1.00e-13&
        *(T32)**(-2.50e+00))

    !C6H2+ + C2H2 -> C8H4+
    k(3128) = small + (1.00e-09)

    !C6H4+ + H -> C6H5+
    k(3129) = small + (3.00e-15&
        *(T32)**(-1.50e+00))

    !C6H5+ + H2 -> C6H7+
    k(3130) = small + (6.00e-11)

    !C- + C -> C2 + E
    k(3131) = small + (5.00e-10)

    !C- + H -> CH + E
    k(3132) = small + (5.00e-10)

    !C- + N -> CN + E
    k(3133) = small + (5.00e-10)

    !C- + O -> CO + E
    k(3134) = small + (5.00e-10)

    !C- + CH -> C2H + E
    k(3135) = small + (5.00e-10)

    !C- + H2 -> CH2 + E
    k(3136) = small + (1.00e-13)

    !C- + NH -> HCN + E
    k(3137) = small + (5.00e-10)

    !C- + O2 -> CO2 + E
    k(3138) = small + (5.00e-11)

    !C- + OH -> HCO + E
    k(3139) = small + (5.00e-10)

    !C- + CH2 -> C2H2 + E
    k(3140) = small + (5.00e-10)

    !C- + H2O -> H2CO + E
    k(3141) = small + (5.00e-10)

    !H- + C -> CH + E
    k(3142) = small + (1.00e-09)

    !H- + H -> H2 + E
    k(3143) = small + (1.30e-09)

    !H- + N -> NH + E
    k(3144) = small + (1.00e-09)

    !H- + O -> OH + E
    k(3145) = small + (1.00e-09)

    !H- + C2 -> C2H + E
    k(3146) = small + (1.00e-09)

    !H- + CH -> CH2 + E
    k(3147) = small + (1.00e-10)

    !H- + CN -> HCN + E
    k(3148) = small + (1.00e-10)

    !H- + CO -> HCO + E
    k(3149) = small + (5.00e-11)

    !H- + NH -> NH2 + E
    k(3150) = small + (1.00e-10)

    !H- + OH -> H2O + E
    k(3151) = small + (1.00e-10)

    !H- + C2H -> C2H2 + E
    k(3152) = small + (1.00e-09)

    !H- + CH2 -> CH3 + E
    k(3153) = small + (1.00e-09)

    !H- + HCO -> H2CO + E
    k(3154) = small + (1.00e-09)

    !H- + NH2 -> NH3 + E
    k(3155) = small + (1.00e-09)

    !H- + CH3 -> CH4 + E
    k(3156) = small + (1.00e-09)

    !O- + C -> CO + E
    k(3157) = small + (5.00e-10)

    !O- + H -> OH + E
    k(3158) = small + (5.00e-10)

    !O- + N -> NO + E
    k(3159) = small + (2.20e-10)

    !O- + O -> O2 + E
    k(3160) = small + (1.90e-10)

    !O- + CH -> HCO + E
    k(3161) = small + (5.00e-10)

    !O- + CO -> CO2 + E
    k(3162) = small + (6.50e-10)

    !O- + H2 -> H2O + E
    k(3163) = small + (7.00e-10)

    !O- + CH2 -> H2CO + E
    k(3164) = small + (5.00e-10)

    !S- + C -> CS + E
    k(3165) = small + (1.00e-10)

    !S- + H -> HS + E
    k(3166) = small + (1.00e-10)

    !S- + N -> NS + E
    k(3167) = small + (1.00e-10)

    !S- + O -> SO + E
    k(3168) = small + (1.00e-10)

    !S- + CO -> OCS + E
    k(3169) = small + (3.00e-10)

    !S- + O2 -> SO2 + E
    k(3170) = small + (3.00e-11)

    !CN- + H -> HCN + E
    k(3171) = small + (1.30e-09)

    !CN- + CH3 -> C2H3N + E
    k(3172) = small + (1.00e-09)

    !OH- + C -> HCO + E
    k(3173) = small + (5.00e-10)

    !OH- + H -> H2O + E
    k(3174) = small + (1.40e-09)

    !OH- + CH -> H2CO + E
    k(3175) = small + (5.00e-10)

    !OH- + CH3 -> CH4O + E
    k(3176) = small + (1.00e-09)

    !O + CH -> HCO+ + E
    k(3177) = small + (2.00e-11&
        *(T32)**(4.40e-01))

    !C + CH -> C2 + H
    k(3178) = small + (6.59e-11)

    !C + HS -> CS + H
    k(3179) = small + (1.00e-10)

    !C + NH -> CN + H
    k(3180) = small + (1.20e-10)

    !C + NO -> CN + O
    k(3181) = small + (6.00e-11&
        *(T32)**(-1.60e-01))

    !C + NO -> CO + N
    k(3182) = small + (9.00e-11&
        *(T32)**(-1.60e-01))

    !C + NS -> CN + S
    k(3183) = small + (1.50e-10&
        *(T32)**(-1.60e-01))

    !C + O2 -> CO + O
    k(3184) = small + (4.70e-11&
        *(T32)**(-3.40e-01))

    !C + OH -> CO + H
    k(3185) = small + (1.00e-10)

    !C + PH -> CP + H
    k(3186) = small + (7.50e-11)

    !C + S2 -> S + CS
    k(3187) = small + (7.00e-11)

    !C + SIH -> SIC + H
    k(3188) = small + (6.59e-11)

    !C + SO -> CO + S
    k(3189) = small + (3.50e-11)

    !C + SO -> CS + O
    k(3190) = small + (3.50e-11)

    !C + CH2 -> C2H + H
    k(3191) = small + (1.00e-10)

    !C + HCO -> CH + CO
    k(3192) = small + (1.00e-10)

    !C + HCO -> CCO + H
    k(3193) = small + (1.00e-10)

    !C + C2N -> C2 + CN
    k(3194) = small + (1.00e-10)

    !C + C2H -> C3 + H
    k(3195) = small + (1.00e-10)

    !C + CCO -> C2 + CO
    k(3196) = small + (2.00e-10)

    !C + NH2 -> HNC + H
    k(3197) = small + (3.40e-11&
        *(T32)**(-3.60e-01))

    !C + NH2 -> HCN + H
    k(3198) = small + (3.40e-11&
        *(T32)**(-3.60e-01))

    !C + OCN -> CO + CN
    k(3199) = small + (1.00e-10)

    !C + SIH2 -> HCSI + H
    k(3200) = small + (1.00e-10)

    !C + SO2 -> CO + SO
    k(3201) = small + (7.00e-11)

    !C + CH3 -> C2H2 + H
    k(3202) = small + (1.00e-10)

    !C + C2H2 -> C3H + H
    k(3203) = small + (7.25e-11&
        *(T32)**(-1.20e-01))

    !C + C2H2 -> C3 + H2
    k(3204) = small + (1.45e-10&
        *(T32)**(-1.20e-01))

    !C + H2CN -> C2N + H2
    k(3205) = small + (2.00e-10)

    !C + SIH3 -> SICH2 + H
    k(3206) = small + (1.00e-10)

    !C + C2H4 -> C3H3 + H
    k(3207) = small + (3.10e-10&
        *(T32)**(-7.00e-02))

    !C + HCNC2 -> C3 + HCN
    k(3208) = small + (2.00e-10)

    !C + HNC3 -> C3 + HNC
    k(3209) = small + (2.00e-10)

    !C + C2H2N -> HC3N + H
    k(3210) = small + (1.00e-10)

    !C + C2H3 -> C3H2 + H
    k(3211) = small + (5.00e-11)

    !C + C3H -> C4 + H
    k(3212) = small + (1.00e-10)

    !C + C3H2 -> C4H + H
    k(3213) = small + (1.00e-10)

    !C + C3N -> C3 + CN
    k(3214) = small + (1.00e-13)

    !C + C3O -> C3 + CO
    k(3215) = small + (1.00e-10)

    !C + C3H3 -> C4H2 + H
    k(3216) = small + (1.00e-10)

    !C + C3H4 -> C4H3 + H
    k(3217) = small + (2.70e-10&
        *(T32)**(-1.10e-01))

    !C + C2H5 -> C3H4 + H
    k(3218) = small + (2.00e-10)

    !C + C4H -> C5 + H
    k(3219) = small + (1.00e-10)

    !C + C4H2 -> C5H + H
    k(3220) = small + (6.50e-10)

    !C + C4H3 -> C5H2 + H
    k(3221) = small + (3.00e-10)

    !C + C5H -> C6 + H
    k(3222) = small + (1.00e-10)

    !C + C5N -> C5 + CN
    k(3223) = small + (1.00e-13)

    !C + C5H2 -> C6H + H
    k(3224) = small + (5.30e-10)

    !C + C6H -> C7 + H
    k(3225) = small + (2.00e-10)

    !C + C6H2 -> C7H + H
    k(3226) = small + (7.40e-10)

    !C + C7H -> C8 + H
    k(3227) = small + (2.00e-10)

    !C + C7N -> C7 + CN
    k(3228) = small + (1.00e-13)

    !C + C7H2 -> C8H + H
    k(3229) = small + (8.40e-10)

    !C + C8H -> C9 + H
    k(3230) = small + (2.00e-10)

    !C + C8H2 -> C9H + H
    k(3231) = small + (9.00e-10)

    !C + C9H -> C10 + H
    k(3232) = small + (2.00e-10)

    !C + C9N -> C9 + CN
    k(3233) = small + (1.00e-13)

    !C + CH3C4H -> C6H2 + H2
    k(3234) = small + (5.30e-10)

    !C + CH3C6H -> C8H2 + H2
    k(3235) = small + (7.40e-10)

    !C + C4 -> C2 + C3
    k(3236) = small + (1.00e-10)

    !C + C5 -> C3 + C3
    k(3237) = small + (3.00e-10)

    !C + C6 -> C2 + C5
    k(3238) = small + (5.00e-11)

    !C + C6 -> C3 + C4
    k(3239) = small + (5.00e-11)

    !C + C7 -> C3 + C5
    k(3240) = small + (3.00e-10)

    !C + C8 -> C4 + C5
    k(3241) = small + (3.00e-11)

    !C + C8 -> C3 + C6
    k(3242) = small + (3.00e-11)

    !C + C8 -> C2 + C7
    k(3243) = small + (3.00e-11)

    !C + C10 -> C8 + C3
    k(3244) = small + (5.00e-11)

    !C + CH2 -> CH + CH
    k(3245) = small + (2.69e-12&
        *exp(-2.36e+04*invT))

    !C + NH2 -> CH + NH
    k(3246) = small + (9.61e-13&
        *exp(-1.05e+04*invT))

    !C + CN -> C2 + N
    k(3247) = small + (4.98e-10*exp(-1.81e+04&
        *invT))

    !C + N2 -> CN + N
    k(3248) = small + (8.70e-11*exp(-2.26e+04&
        *invT))

    !C + CO -> C2 + O
    k(3249) = small + (1.00e-10*exp(-5.28e+04&
        *invT))

    !H + HS -> S + H2
    k(3250) = small + (2.50e-11)

    !H + HCO -> H2 + CO
    k(3251) = small + (1.50e-10)

    !H + HCO -> O + CH2
    k(3252) = small + (6.61e-11&
        *exp(-5.16e+04*invT))

    !H + C2H3 -> C2H2 + H2
    k(3253) = small + (2.00e-11)

    !H + H2CN -> H2 + HCN
    k(3254) = small + (1.00e-10)

    !H + HCNC2 -> HC2NC + H
    k(3255) = small + (1.00e-11)

    !H + HNC3 -> HC3N + H
    k(3256) = small + (1.00e-11)

    !H + CH -> C + H2
    k(3257) = small + (2.70e-11&
        *(T32)**(3.80e-01))

    !H + CH2 -> CH + H2
    k(3258) = small + (2.70e-10)

    !H + CH3 -> CH2 + H2
    k(3259) = small + (1.00e-10&
        *exp(-7.60e+03*invT))

    !H + CH4 -> CH3 + H2
    k(3260) = small + (7.34e-12&
        *exp(-4.41e+03*invT))

    !H + OH -> O + H2
    k(3261) = small + (6.86e-14&
        *(T32)**(2.80e+00)*exp(-1.95e+03*invT))

    !H + NH3 -> NH2 + H2
    k(3262) = small + (6.54e-13&
        *(T32)**(2.76e+00)*exp(-5.17e+03*invT))

    !H + H2O -> OH + H2
    k(3263) = small + (6.82e-12&
        *(T32)**(1.60e+00)*exp(-9.72e+03*invT))

    !H + HCN -> CN + H2
    k(3264) = small + (6.19e-10&
        *exp(-1.25e+04*invT))

    !H + NO -> O + NH
    k(3265) = small + (9.30e-10&
        *(T32)**(-1.00e-01)*exp(-3.52e+04*invT))

    !H + NO -> N + OH
    k(3266) = small + (3.60e-10*exp(-2.49e+04&
        *invT))

    !H + H2CO -> HCO + H2
    k(3267) = small + (2.14e-12&
        *(T32)**(1.62e+00)*exp(-1.09e+03*invT))

    !H + HNO -> NH2 + O
    k(3268) = small + (1.05e-09&
        *(T32)**(-3.00e-01)*exp(-1.47e+04*invT))

    !H + HNO -> OH + NH
    k(3269) = small + (2.41e-09&
        *(T32)**(-5.00e-01)*exp(-9.01e+03*invT))

    !H + O2 -> OH + O
    k(3270) = small + (2.94e-10*exp(-8.38e+03&
        *invT))

    !H + O2H -> O2 + H2
    k(3271) = small + (5.60e-12)

    !H + O2H -> OH + OH
    k(3272) = small + (7.21e-11)

    !H + O2H -> H2O + O
    k(3273) = small + (2.42e-12)

    !H + H2O2 -> H2O + OH
    k(3274) = small + (1.69e-11&
        *exp(-1.80e+03*invT))

    !H + H2O2 -> O2H + H2
    k(3275) = small + (2.81e-12&
        *exp(-1.89e+03*invT))

    !H + H2S -> HS + H2
    k(3276) = small + (6.60e-11&
        *exp(-1.35e+03*invT))

    !H + CO2 -> CO + OH
    k(3277) = small + (2.51e-10&
        *exp(-1.33e+04*invT))

    !H + N2O -> OH + N2
    k(3278) = small + (9.22e-14&
        *exp(-2.99e+03*invT))

    !H + NO2 -> NO + OH
    k(3279) = small + (4.00e-10&
        *exp(-3.40e+02*invT))

    !H + OCS -> CO + HS
    k(3280) = small + (1.23e-11&
        *exp(-1.95e+03*invT))

    !H2 + C -> CH + H
    k(3281) = small + (6.64e-10*exp(-1.17e+04&
        *invT))

    !H2 + CH -> CH2 + H
    k(3282) = small + (3.75e-10&
        *exp(-1.66e+03*invT))

    !H2 + CH2 -> CH3 + H
    k(3283) = small + (5.00e-11&
        *exp(-4.87e+03*invT))

    !H2 + CH3 -> CH4 + H
    k(3284) = small + (2.51e-13&
        *exp(-4.21e+03*invT))

    !H2 + O -> OH + H
    k(3285) = small + (3.44e-13&
        *(T32)**(2.67e+00)*exp(-3.16e+03*invT))

    !H2 + OH -> H2O + H
    k(3286) = small + (8.40e-13&
        *exp(-1.04e+03*invT))

    !H2 + N -> NH + H
    k(3287) = small + (4.65e-10*exp(-1.66e+04&
        *invT))

    !H2 + NH -> NH2 + H
    k(3288) = small + (5.96e-11&
        *exp(-7.78e+03*invT))

    !H2 + NH2 -> NH3 + H
    k(3289) = small + (1.76e-13&
        *(T32)**(2.23e+00)*exp(-3.61e+03*invT))

    !H2 + O2H -> H2O2 + H
    k(3290) = small + (4.38e-12&
        *exp(-1.08e+04*invT))

    !N + C2 -> CN + C
    k(3291) = small + (5.00e-11)

    !N + CH -> CN + H
    k(3292) = small + (1.66e-10&
        *(T32)**(-9.00e-02))

    !N + CN -> C + N2
    k(3293) = small + (3.00e-10)

    !N + HS -> NS + H
    k(3294) = small + (1.00e-10)

    !N + NH -> N2 + H
    k(3295) = small + (5.00e-11)

    !N + NO -> N2 + O
    k(3296) = small + (3.00e-11&
        *(T32)**(-6.00e-01))

    !N + NS -> N2 + S
    k(3297) = small + (3.00e-11&
        *(T32)**(-6.00e-01))

    !N + OH -> NO + H
    k(3298) = small + (7.50e-11&
        *(T32)**(-1.80e-01))

    !N + O2 -> NO + O
    k(3299) = small + (1.50e-11*exp(-3.68e+03&
        *invT))

    !N + PH -> PN + H
    k(3300) = small + (5.00e-11)

    !N + PO -> PN + O
    k(3301) = small + (3.00e-11&
        *(T32)**(-6.00e-01))

    !N + SIC -> CN + SI
    k(3302) = small + (5.00e-11)

    !N + SIC -> SIN + C
    k(3303) = small + (5.00e-11)

    !N + SIH -> SIN + H
    k(3304) = small + (1.66e-10&
        *(T32)**(-9.00e-02))

    !N + SO -> NO + S
    k(3305) = small + (1.50e-11*exp(-3.68e+03&
        *invT))

    !N + CH2 -> HCN + H
    k(3306) = small + (3.95e-11&
        *(T32)**(1.67e-01))

    !N + CH2 -> HNC + H
    k(3307) = small + (3.95e-11&
        *(T32)**(1.67e-01))

    !N + HCO -> OCN + H
    k(3308) = small + (1.00e-10)

    !N + HCS -> HCN + S
    k(3309) = small + (1.00e-10)

    !N + NO2 -> N2 + O2
    k(3310) = small + (1.00e-12)

    !N + NO2 -> NO + NO
    k(3311) = small + (1.00e-12)

    !N + NO2 -> N2O + O
    k(3312) = small + (2.10e-11)

    !N + O2H -> NH + O2
    k(3313) = small + (1.70e-13)

    !N + SIH2 -> HNSI + H
    k(3314) = small + (8.00e-11&
        *(T32)**(1.67e-01))

    !N + C2H -> C2N + H
    k(3315) = small + (1.70e-11)

    !N + C2N -> CN + CN
    k(3316) = small + (1.00e-10)

    !N + CCO -> CN + CO
    k(3317) = small + (5.50e-10)

    !N + C3 -> CN + C2
    k(3318) = small + (1.00e-13)

    !N + C3H -> C3N + H
    k(3319) = small + (1.00e-13)

    !N + C3N -> CN + C2N
    k(3320) = small + (1.00e-10)

    !N + CH3 -> HCN + H + H
    k(3321) = small + (3.32e-13)

    !N + CH3 -> H2CN + H
    k(3322) = small + (8.60e-11)

    !N + H2CN -> NH + HCN
    k(3323) = small + (3.70e-11)

    !N + SIH3 -> HNSI + H2
    k(3324) = small + (1.00e-10)

    !N + C2H3 -> C2H2N + H
    k(3325) = small + (6.20e-11)

    !N + C2H3 -> C2H2 + NH
    k(3326) = small + (1.20e-11)

    !N + C3H2 -> HC3N + H
    k(3327) = small + (1.00e-13)

    !N + C3H3 -> HC3N + H2
    k(3328) = small + (8.00e-11)

    !N + C4 -> C3 + CN
    k(3329) = small + (5.00e-11)

    !N + C5 -> CN + C4
    k(3330) = small + (1.00e-13)

    !N + C4H -> C4N + H
    k(3331) = small + (1.70e-11)

    !N + C4N -> CN + C3N
    k(3332) = small + (1.00e-10)

    !N + C5H -> C5N + H
    k(3333) = small + (1.70e-11)

    !N + C5N -> CN + C4N
    k(3334) = small + (1.00e-10)

    !N + C2H5 -> H2CN + CH3
    k(3335) = small + (3.85e-11)

    !N + C2H5 -> C2H4 + NH
    k(3336) = small + (7.15e-11)

    !N + C5H2 -> HC5N + H
    k(3337) = small + (1.00e-13)

    !N + C6 -> CN + C5
    k(3338) = small + (5.00e-11)

    !N + C6H -> CN + C5H
    k(3339) = small + (1.00e-13)

    !N + C7 -> CN + C6
    k(3340) = small + (1.00e-13)

    !N + C7H -> C7N + H
    k(3341) = small + (1.70e-11)

    !N + C7H2 -> HC7N + H
    k(3342) = small + (1.00e-13)

    !N + C7N -> C2N + C5N
    k(3343) = small + (1.00e-10)

    !N + C8 -> CN + C7
    k(3344) = small + (5.00e-11)

    !N + C8H -> CN + C7H
    k(3345) = small + (1.00e-10)

    !N + C9 -> CN + C8
    k(3346) = small + (1.00e-13)

    !N + C9H -> C9N + H
    k(3347) = small + (1.70e-11)

    !N + C9N -> C2N + C7N
    k(3348) = small + (1.00e-10)

    !O + C2 -> CO + C
    k(3349) = small + (1.00e-10)

    !O + CCL -> CLO + C
    k(3350) = small + (1.38e-10&
        *exp(-1.60e+04*invT))

    !O + CCL -> CL + CO
    k(3351) = small + (9.96e-11)

    !O + CH -> CO + H
    k(3352) = small + (6.60e-11)

    !O + CLO -> CL + O2
    k(3353) = small + (4.00e-11)

    !O + CN -> CO + N
    k(3354) = small + (4.00e-11)

    !O + CP -> P + CO
    k(3355) = small + (4.00e-11)

    !O + CS -> CO + S
    k(3356) = small + (1.94e-11*exp(-2.31e+02&
        *invT))

    !O + HS -> SO + H
    k(3357) = small + (1.60e-10&
        *(T32)**(5.00e-01))

    !O + NH -> NO + H
    k(3358) = small + (1.16e-10)

    !O + NH -> OH + N
    k(3359) = small + (1.16e-11)

    !O + NS -> NO + S
    k(3360) = small + (1.00e-10)

    !O + NS -> SO + N
    k(3361) = small + (1.00e-11)

    !O + OH -> O2 + H
    k(3362) = small + (3.50e-11)

    !O + PH -> PO + H
    k(3363) = small + (1.00e-10)

    !O + S2 -> SO + S
    k(3364) = small + (1.70e-11)

    !O + SIC -> CO + SI
    k(3365) = small + (5.00e-11)

    !O + SIC -> SIO + C
    k(3366) = small + (5.00e-11)

    !O + SIH -> SIO + H
    k(3367) = small + (1.00e-10)

    !O + SIN -> NO + SI
    k(3368) = small + (5.00e-11)

    !O + SIN -> SIO + N
    k(3369) = small + (5.00e-11)

    !O + C2H -> CO + CH
    k(3370) = small + (1.70e-11)

    !O + C2N -> CO + CN
    k(3371) = small + (6.00e-12)

    !O + CCO -> CO + CO
    k(3372) = small + (8.60e-11)

    !O + CCP -> CP + CO
    k(3373) = small + (6.00e-12)

    !O + CH2 -> CO + H + H
    k(3374) = small + (1.20e-10)

    !O + CH2 -> CO + H2
    k(3375) = small + (8.00e-11)

    !O + HCO -> H + CO2
    k(3376) = small + (5.00e-11)

    !O + HCO -> OH + CO
    k(3377) = small + (5.00e-11)

    !O + HCN -> OCN + H
    k(3378) = small + (3.61e-13&
        *(T32)**(2.10e+00)*exp(-3.08e+03*invT))

    !O + HCP -> CO + PH
    k(3379) = small + (3.61e-13&
        *(T32)**(2.10e+00)*exp(-3.08e+03*invT))

    !O + HCS -> OCS + H
    k(3380) = small + (5.00e-11)

    !O + HCS -> OH + CS
    k(3381) = small + (5.00e-11)

    !O + HCSI -> SIO + CH
    k(3382) = small + (2.00e-11)

    !O + H2S -> HS + OH
    k(3383) = small + (9.22e-12&
        *exp(-1.80e+03*invT))

    !O + HNO -> OH + NO
    k(3384) = small + (3.80e-11)

    !O + HNO -> NO2 + H
    k(3385) = small + (1.00e-12)

    !O + HPO -> PO + OH
    k(3386) = small + (3.80e-11)

    !O + NH2 -> NH + OH
    k(3387) = small + (2.00e-11)

    !O + NH2 -> HNO + H
    k(3388) = small + (8.00e-11)

    !O + NH2 -> NO + H2
    k(3389) = small + (1.00e-11)

    !O + NO2 -> NO + O2
    k(3390) = small + (1.00e-11)

    !O + O2H -> OH + O2
    k(3391) = small + (5.30e-11)

    !O + OCN -> NO + CO
    k(3392) = small + (1.50e-11&
        *exp(-2.00e+02*invT))

    !O + OCN -> CN + O2
    k(3393) = small + (4.05e-10&
        *(T32)**(-1.43e+00)*exp(-3.50e+03*invT))

    !O + PH2 -> HPO + H
    k(3394) = small + (8.00e-11)

    !O + PH2 -> PH + OH
    k(3395) = small + (2.00e-11)

    !O + C3 -> CO + C2
    k(3396) = small + (5.00e-12&
        *exp(-9.00e+02*invT))

    !O + SIC2 -> SIC + CO
    k(3397) = small + (4.00e-11)

    !O + SIH2 -> SIO + H + H
    k(3398) = small + (1.20e-10)

    !O + SIH2 -> SIO + H2
    k(3399) = small + (8.00e-11)

    !O + SINC -> SIN + CO
    k(3400) = small + (1.00e-11)

    !O + C2H3 -> C2H2O + H
    k(3401) = small + (1.60e-10)

    !O + C2H5 -> C2H4O + H
    k(3402) = small + (1.33e-10)

    !O + C2H5 -> H2CO + CH3
    k(3403) = small + (2.65e-11)

    !O + C3H -> C2H + CO
    k(3404) = small + (1.70e-11)

    !O + C3N -> CO + C2N
    k(3405) = small + (4.00e-11)

    !O + C3O -> CCO + CO
    k(3406) = small + (5.00e-12&
        *exp(-9.00e+02*invT))

    !O + C3P -> CCP + CO
    k(3407) = small + (4.00e-11)

    !O + C4 -> CO + C3
    k(3408) = small + (5.00e-11)

    !O + CH3 -> H2CO + H
    k(3409) = small + (1.40e-10)

    !O + H2CN -> OCN + H2
    k(3410) = small + (1.00e-10)

    !O + HCCP -> HCP + CO
    k(3411) = small + (4.00e-11)

    !O + SIC3 -> SIC2 + CO
    k(3412) = small + (4.00e-11)

    !O + SIH3 -> H2SIO + H
    k(3413) = small + (1.40e-10)

    !O + C5 -> CO + C4
    k(3414) = small + (5.00e-12&
        *exp(-9.00e+02*invT))

    !O + C4H -> C3H + CO
    k(3415) = small + (8.50e-12)

    !O + C4N -> C3N + CO
    k(3416) = small + (6.00e-12)

    !O + C4P -> C3P + CO
    k(3417) = small + (1.00e-11)

    !O + CH2PH -> PH2 + CO + H
    k(3418) = small + (4.00e-11)

    !O + SIC4 -> SIC3 + CO
    k(3419) = small + (4.00e-11)

    !O + C5H -> CO + C4H
    k(3420) = small + (1.70e-11)

    !O + C5N -> C4N + CO
    k(3421) = small + (4.00e-11)

    !O + C6 -> CO + C5
    k(3422) = small + (5.00e-11)

    !O + C6H -> CO + C5H
    k(3423) = small + (1.70e-11)

    !O + C7 -> CO + C6
    k(3424) = small + (5.00e-12&
        *exp(-9.00e+02*invT))

    !O + C7H -> CO + C6H
    k(3425) = small + (1.70e-11)

    !O + C7N -> OCN + C6
    k(3426) = small + (4.00e-11)

    !O + C8 -> CO + C7
    k(3427) = small + (5.00e-11)

    !O + C8H -> CO + C7H
    k(3428) = small + (1.70e-11)

    !O + C9 -> CO + C8
    k(3429) = small + (5.00e-12&
        *exp(-9.00e+02*invT))

    !O + C9H -> CO + C8H
    k(3430) = small + (1.70e-11)

    !O + C9N -> OCN + C8
    k(3431) = small + (4.00e-11)

    !O + C10 -> CO + C9
    k(3432) = small + (5.00e-11)

    !S + C2 -> CS + C
    k(3433) = small + (1.00e-10)

    !S + CH -> CS + H
    k(3434) = small + (5.00e-11)

    !S + NH -> NS + H
    k(3435) = small + (1.00e-10)

    !S + HS -> S2 + H
    k(3436) = small + (4.50e-11)

    !S + O2 -> SO + O
    k(3437) = small + (2.30e-12)

    !S + OH -> SO + H
    k(3438) = small + (6.60e-11)

    !S + CH2 -> HCS + H
    k(3439) = small + (1.00e-10)

    !S + CH2 -> CS + H2
    k(3440) = small + (1.00e-10)

    !S + CH3 -> H2CS + H
    k(3441) = small + (1.40e-10)

    !SI + O2 -> SIO + O
    k(3442) = small + (1.72e-10&
        *(T32)**(-5.30e-01)*exp(-1.70e+01*invT))

    !SI + NO -> SIO + N
    k(3443) = small + (9.00e-11&
        *(T32)**(-9.60e-01)*exp(-2.80e+01*invT))

    !SI + OH -> SIO + H
    k(3444) = small + (1.00e-10)

    !SI + CH2 -> HCSI + H
    k(3445) = small + (1.00e-10)

    !SI + CH3 -> SICH2 + H
    k(3446) = small + (1.00e-10)

    !SI + C2H2 -> SIC2H + H
    k(3447) = small + (2.70e-10&
        *(T32)**(-1.10e-01))

    !CH + NO -> HCN + O
    k(3448) = small + (1.20e-11&
        *(T32)**(-1.30e-01))

    !CH + O2 -> CO + OH
    k(3449) = small + (3.80e-11&
        *(T32)**(-4.80e-01))

    !CH + HNO -> NO + CH2
    k(3450) = small + (1.73e-11)

    !CH + CH4 -> C2H4 + H
    k(3451) = small + (2.23e-12)

    !CH + C2H2 -> C3H2 + H
    k(3452) = small + (4.20e-10&
        *(T32)**(-2.30e-01)*exp(-1.60e+01*invT))

    !CH + C3H4 -> C4H4 + H
    k(3453) = small + (4.20e-10&
        *(T32)**(-2.30e-01)*exp(-1.60e+01*invT))

    !CH + C4H2 -> C5H2 + H
    k(3454) = small + (4.20e-10&
        *(T32)**(-2.30e-01)*exp(-1.60e+01*invT))

    !CH + C6H2 -> C7H2 + H
    k(3455) = small + (4.20e-10&
        *(T32)**(-2.30e-01)*exp(-1.60e+01*invT))

    !CH + C2H4 -> C3H4 + H
    k(3456) = small + (3.45e-10&
        *(T32)**(-5.50e-01)*exp(-2.96e+01*invT))

    !CH + C3 -> C4 + H
    k(3457) = small + (4.00e-10)

    !CH + C5 -> C6 + H
    k(3458) = small + (4.00e-10)

    !CH + C7 -> C8 + H
    k(3459) = small + (4.00e-10)

    !CH + C9 -> C10 + H
    k(3460) = small + (4.00e-10)

    !CH + C2 -> C3 + H
    k(3461) = small + (1.00e-10)

    !CH + C4 -> C5 + H
    k(3462) = small + (1.00e-10)

    !CH + C6 -> C7 + H
    k(3463) = small + (1.00e-10)

    !CH + C8 -> C9 + H
    k(3464) = small + (1.00e-10)

    !C2 + C2H2 -> C4H + H
    k(3465) = small + (1.00e-10)

    !C2 + C2H4 -> C4H3 + H
    k(3466) = small + (1.00e-10)

    !C2 + C4H2 -> C6H + H
    k(3467) = small + (1.00e-10)

    !C2 + C6H2 -> C8H + H
    k(3468) = small + (1.00e-10)

    !CN + NO -> N2 + CO
    k(3469) = small + (1.60e-13)

    !CN + O2 -> O + OCN
    k(3470) = small + (2.40e-11&
        *(T32)**(-6.00e-01))

    !CN + HCO -> HCN + CO
    k(3471) = small + (1.00e-10)

    !CN + OH -> OCN + H
    k(3472) = small + (7.01e-11)

    !CN + OH -> HCN + O
    k(3473) = small + (1.00e-11&
        *exp(-1.00e+03*invT))

    !CN + HNO -> NO + HCN
    k(3474) = small + (3.00e-11)

    !CN + C2H2 -> HC3N + H
    k(3475) = small + (2.72e-10&
        *(T32)**(-5.20e-01)*exp(-1.90e+01*invT))

    !CN + NH3 -> NH2 + HCN
    k(3476) = small + (1.38e-11&
        *(T32)**(-1.14e+00))

    !CN + NH3 -> NH2CN + H
    k(3477) = small + (1.30e-11&
        *(T32)**(-1.11e+00))

    !CN + C2H4 -> C3H3N + H
    k(3478) = small + (2.67e-10&
        *(T32)**(-6.90e-01)*exp(-3.10e+01*invT))

    !CN + C4H2 -> HC5N + H
    k(3479) = small + (2.72e-10&
        *(T32)**(-5.20e-01)*exp(-1.90e+01*invT))

    !CN + C6H2 -> HC7N + H
    k(3480) = small + (2.72e-10&
        *(T32)**(-5.20e-01)*exp(-1.90e+01*invT))

    !CN + C8H2 -> HC9N + H
    k(3481) = small + (2.72e-10&
        *(T32)**(-5.20e-01)*exp(-1.90e+01*invT))

    !CO + OH -> CO2 + H
    k(3482) = small + (2.81e-13&
        *exp(-1.76e+02*invT))

    !CO + HNO -> NH + CO2
    k(3483) = small + (3.32e-12&
        *exp(-6.17e+03*invT))

    !HS + HS -> H2S + S
    k(3484) = small + (3.00e-11)

    !NH + NH -> N2 + H + H
    k(3485) = small + (1.00e-10)

    !NH + NO -> N2O + H
    k(3486) = small + (3.12e-11)

    !NO + NH2 -> N2 + H2O
    k(3487) = small + (1.70e-11)

    !OH + SIO -> SIO2 + H
    k(3488) = small + (2.00e-12)

    !OH + SO -> SO2 + H
    k(3489) = small + (8.60e-11)

    !OH + OH -> H2O + O
    k(3490) = small + (1.65e-12&
        *(T32)**(1.14e+00)*exp(-5.00e+01*invT))

    !OH + CH2 -> H2CO + H
    k(3491) = small + (3.00e-10)

    !OH + H2S -> H2O + HS
    k(3492) = small + (6.00e-12&
        *exp(-7.50e+01*invT))

    !OH + HCO -> H2O + CO
    k(3493) = small + (1.69e-10)

    !OH + HNO -> H2O + NO
    k(3494) = small + (8.00e-11&
        *exp(-5.00e+02*invT))

    !OH + NH2 -> NH + H2O
    k(3495) = small + (1.50e-12)

    !OH + NH2 -> NH3 + O
    k(3496) = small + (9.18e-14)

    !OH + O2H -> H2O + O2
    k(3497) = small + (1.20e-10)

    !OH + H2CO -> HCO + H2O
    k(3498) = small + (1.00e-11)

    !OH + H2CO -> CH2O2 + H
    k(3499) = small + (2.01e-13)

    !OH + H2O2 -> H2O + O2H
    k(3500) = small + (2.91e-12&
        *exp(-1.60e+02*invT))

    !OH + C2H3 -> C2H2 + H2O
    k(3501) = small + (1.00e-10)

    !OH + C2H5 -> C2H4 + H2O
    k(3502) = small + (4.00e-11)

    !C2H + S -> C2S + H
    k(3503) = small + (1.70e-11)

    !C2H + CS -> C3S + H
    k(3504) = small + (1.70e-11)

    !C2H + O2 -> HCO + CO
    k(3505) = small + (4.20e-11&
        *(T32)**(-3.20e-01))

    !C2H + HCN -> HC3N + H
    k(3506) = small + (5.30e-12&
        *exp(-7.70e+02*invT))

    !C2H + HNC -> HC3N + H
    k(3507) = small + (5.30e-12&
        *exp(-7.70e+02*invT))

    !C2H + C2H2 -> C4H2 + H
    k(3508) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C3H2 -> C5H2 + H
    k(3509) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C4H2 -> C6H2 + H
    k(3510) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C5H2 -> C7H2 + H
    k(3511) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C6H2 -> C8H2 + H
    k(3512) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C7H2 -> C9H2 + H
    k(3513) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + C3 -> C5 + H
    k(3514) = small + (2.00e-10&
        *(T32)**(-2.50e-01))

    !C2H + C5 -> C7 + H
    k(3515) = small + (2.00e-10&
        *(T32)**(-2.50e-01))

    !C2H + C7 -> C9 + H
    k(3516) = small + (2.00e-10&
        *(T32)**(-2.50e-01))

    !C2H + C2 -> C4 + H
    k(3517) = small + (1.00e-10)

    !C2H + C4 -> C6 + H
    k(3518) = small + (1.00e-10)

    !C2H + C6 -> C8 + H
    k(3519) = small + (1.00e-10)

    !C2H + C8 -> C10 + H
    k(3520) = small + (1.00e-10)

    !C2H + HC3N -> HC5N + H
    k(3521) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + HC5N -> HC7N + H
    k(3522) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !C2H + HC7N -> HC9N + H
    k(3523) = small + (1.06e-10&
        *(T32)**(-2.50e-01))

    !CH2 + HNO -> NO + CH3
    k(3524) = small + (1.70e-11)

    !HCO + HCO -> H2CO + CO
    k(3525) = small + (5.00e-11)

    !HCO + HNO -> H2CO + NO
    k(3526) = small + (1.00e-12&
        *exp(-9.76e+02*invT))

    !HCO + CH3 -> CH4 + CO
    k(3527) = small + (4.40e-11)

    !HNO + CH3 -> CH4 + NO
    k(3528) = small + (3.32e-12)

    !NH2 + H2CO -> NH2CHO + H
    k(3529) = small + (1.00e-10)

    !C + C -> C2
    k(3530) = small + (1.00e-17)

    !C + H -> CH
    k(3531) = small + (1.00e-17)

    !C + N -> CN
    k(3532) = small + (1.00e-17)

    !C + O -> CO
    k(3533) = small + (2.10e-19)

    !C + H2 -> CH2
    k(3534) = small + (1.00e-17)

    !H + O -> OH
    k(3535) = small + (9.90e-19&
        *(T32)**(-3.80e-01))

    !O + O -> O2
    k(3536) = small + (4.90e-20*(T32)**(1.58e+00))

    !O + SO -> SO2
    k(3537) = small + (3.20e-16&
        *(T32)**(-1.60e+00))

    !S + CO -> OCS
    k(3538) = small + (1.60e-17&
        *(T32)**(-1.50e+00))

    !CH + H2 -> CH3
    k(3539) = small + (3.25e-17&
        *(T32)**(-6.00e-01))

    !CN + C2H -> HC3N
    k(3540) = small + (1.00e-16)

    !CN + CH3 -> C2H3N
    k(3541) = small + (1.00e-16)

    !C + C2 -> C3
    k(3542) = small + (3.30e-16&
        *(T32)**(-1.00e+00))

    !C + C3 -> C4
    k(3543) = small + (3.30e-14&
        *(T32)**(-1.00e+00))

    !H + OH -> H2O
    k(3544) = small + (4.00e-18&
        *(T32)**(-2.00e+00))

    !OH + OH -> H2O2
    k(3545) = small + (1.00e-18&
        *(T32)**(-2.00e+00))

    !C2+ + E -> C + C
    k(3546) = small + (8.84e-08&
        *(T32)**(-5.00e-01))

    !CCL+ + E -> C + CL
    k(3547) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH+ + E -> C + H
    k(3548) = small + (7.00e-08&
        *(T32)**(-5.00e-01))

    !CLO+ + E -> CL + O
    k(3549) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !CN+ + E -> C + N
    k(3550) = small + (3.38e-07&
        *(T32)**(-5.50e-01))

    !CO+ + E -> O + C
    k(3551) = small + (2.75e-07&
        *(T32)**(-5.50e-01))

    !CP+ + E -> P + C
    k(3552) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !CS+ + E -> C + S
    k(3553) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !H2+ + E -> H + H
    k(3554) = small + (2.53e-07&
        *(T32)**(-5.00e-01))

    !HCL+ + E -> H + CL
    k(3555) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HEH+ + E -> H + HE
    k(3556) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !HS+ + E -> S + H
    k(3557) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !N2+ + E -> N + N
    k(3558) = small + (1.80e-07&
        *(T32)**(-3.90e-01))

    !NH+ + E -> N + H
    k(3559) = small + (1.18e-07&
        *(T32)**(-5.00e-01))

    !NO+ + E -> N + O
    k(3560) = small + (4.10e-07&
        *(T32)**(-1.00e+00))

    !NS+ + E -> N + S
    k(3561) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !O2+ + E -> O + O
    k(3562) = small + (1.95e-07&
        *(T32)**(-7.00e-01))

    !OH+ + E -> O + H
    k(3563) = small + (6.30e-09&
        *(T32)**(-4.80e-01))

    !PH+ + E -> P + H
    k(3564) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PN+ + E -> P + N
    k(3565) = small + (1.80e-07&
        *(T32)**(-5.00e-01))

    !PO+ + E -> P + O
    k(3566) = small + (1.80e-07&
        *(T32)**(-5.00e-01))

    !S2+ + E -> S + S
    k(3567) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIC+ + E -> SI + C
    k(3568) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIH+ + E -> SI + H
    k(3569) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIN+ + E -> SI + N
    k(3570) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIO+ + E -> SI + O
    k(3571) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIS+ + E -> SI + S
    k(3572) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SO+ + E -> O + S
    k(3573) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H+ + E -> C2 + H
    k(3574) = small + (1.16e-07&
        *(T32)**(-7.60e-01))

    !C2H+ + E -> CH + C
    k(3575) = small + (1.05e-07&
        *(T32)**(-7.60e-01))

    !C2H+ + E -> C + C + H
    k(3576) = small + (4.80e-08&
        *(T32)**(-7.60e-01))

    !C2N+ + E -> C + CN
    k(3577) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2N+ + E -> C2 + N
    k(3578) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2O+ + E -> CO + C
    k(3579) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2S+ + E -> C2 + S
    k(3580) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2S+ + E -> CS + C
    k(3581) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3+ + E -> C2 + C
    k(3582) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CCP+ + E -> P + C2
    k(3583) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CCP+ + E -> CP + C
    k(3584) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH2+ + E -> C + H2
    k(3585) = small + (7.70e-08&
        *(T32)**(-6.00e-01))

    !CH2+ + E -> CH + H
    k(3586) = small + (1.60e-07&
        *(T32)**(-6.00e-01))

    !CH2+ + E -> C + H + H
    k(3587) = small + (4.00e-07&
        *(T32)**(-6.00e-01))

    !CHSI+ + E -> SI + CH
    k(3588) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CHSI+ + E -> SIC + H
    k(3589) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CNC+ + E -> CN + C
    k(3590) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CO2+ + E -> O + CO
    k(3591) = small + (4.20e-07&
        *(T32)**(-7.50e-01))

    !H2CL+ + E -> CL + H + H
    k(3592) = small + (2.70e-07&
        *(T32)**(-5.00e-01))

    !H2CL+ + E -> HCL + H
    k(3593) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !H2O+ + E -> O + H2
    k(3594) = small + (3.90e-08&
        *(T32)**(-5.00e-01))

    !H2O+ + E -> OH + H
    k(3595) = small + (8.60e-08&
        *(T32)**(-5.00e-01))

    !H2O+ + E -> O + H + H
    k(3596) = small + (3.05e-07&
        *(T32)**(-5.00e-01))

    !H2S+ + E -> S + H + H
    k(3597) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2S+ + E -> HS + H
    k(3598) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3+ + E -> H + H + H
    k(3599) = small + (4.36e-08&
        *(T32)**(-5.20e-01))

    !H3+ + E -> H2 + H
    k(3600) = small + (2.34e-08&
        *(T32)**(-5.20e-01))

    !HCN+ + E -> CN + H
    k(3601) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !HCO+ + E -> CO + H
    k(3602) = small + (2.80e-07&
        *(T32)**(-6.90e-01))

    !HCP+ + E -> P + CH
    k(3603) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HCP+ + E -> CP + H
    k(3604) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HCS+ + E -> CS + H
    k(3605) = small + (1.84e-07&
        *(T32)**(-5.70e-01))

    !HCS+ + E -> CH + S
    k(3606) = small + (7.86e-07&
        *(T32)**(-5.70e-01))

    !HNC+ + E -> CN + H
    k(3607) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !HNO+ + E -> NO + H
    k(3608) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HNS+ + E -> NS + H
    k(3609) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HNSI+ + E -> NH + SI
    k(3610) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HNSI+ + E -> SIN + H
    k(3611) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HOC+ + E -> CO + H
    k(3612) = small + (2.00e-07&
        *(T32)**(-7.50e-01))

    !HPN+ + E -> PH + N
    k(3613) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HPN+ + E -> PN + H
    k(3614) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HPO+ + E -> PH + O
    k(3615) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HPO+ + E -> PO + H
    k(3616) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSIO+ + E -> SI + OH
    k(3617) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSIO+ + E -> SIO + H
    k(3618) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSIS+ + E -> SI + HS
    k(3619) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSIS+ + E -> SIS + H
    k(3620) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSO+ + E -> SO + H
    k(3621) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !N2H+ + E -> N2 + H
    k(3622) = small + (9.00e-08&
        *(T32)**(-5.10e-01))

    !N2H+ + E -> NH + N
    k(3623) = small + (1.00e-08&
        *(T32)**(-5.10e-01))

    !NAH2+ + E -> NA + H2
    k(3624) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NAH2+ + E -> NAH + H
    k(3625) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NCO+ + E -> CO + N
    k(3626) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !NH2+ + E -> N + H + H
    k(3627) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !NH2+ + E -> NH + H
    k(3628) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !NO2+ + E -> O + NO
    k(3629) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !O2H+ + E -> O2 + H
    k(3630) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !OCS+ + E -> CO + S
    k(3631) = small + (2.90e-07&
        *(T32)**(-6.20e-01))

    !OCS+ + E -> CS + O
    k(3632) = small + (4.80e-08&
        *(T32)**(-6.20e-01))

    !OCS+ + E -> C + SO
    k(3633) = small + (1.05e-08&
        *(T32)**(-6.20e-01))

    !PH2+ + E -> P + H2
    k(3634) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PH2+ + E -> PH + H
    k(3635) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !S2H+ + E -> HS + S
    k(3636) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !S2H+ + E -> S2 + H
    k(3637) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC2+ + E -> SI + C2
    k(3638) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC2+ + E -> SIC + C
    k(3639) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH2+ + E -> SI + H + H
    k(3640) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SIH2+ + E -> SI + H2
    k(3641) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH2+ + E -> SIH + H
    k(3642) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !SINC+ + E -> SI + CN
    k(3643) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !SO2+ + E -> SO + O
    k(3644) = small + (2.50e-07&
        *(T32)**(-5.00e-01))

    !C2H2+ + E -> C2H + H
    k(3645) = small + (2.90e-07&
        *(T32)**(-5.00e-01))

    !C2H2+ + E -> C2 + H + H
    k(3646) = small + (1.70e-07&
        *(T32)**(-5.00e-01))

    !C2H2+ + E -> CH + CH
    k(3647) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !C2H2+ + E -> CH2 + C
    k(3648) = small + (2.89e-08&
        *(T32)**(-5.00e-01))

    !C2H2+ + E -> C2 + H2
    k(3649) = small + (1.15e-08&
        *(T32)**(-5.00e-01))

    !C2HO+ + E -> CO + C + H
    k(3650) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2HO+ + E -> CO + CH
    k(3651) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C2HO+ + E -> C2H + O
    k(3652) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2HO+ + E -> CCO + H
    k(3653) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C2N2+ + E -> CN + CN
    k(3654) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2N2+ + E -> C2N + N
    k(3655) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2NH+ + E -> CH + CN
    k(3656) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2NH+ + E -> C2N + H
    k(3657) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H+ + E -> C2H + C
    k(3658) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H+ + E -> C3 + H
    k(3659) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3N+ + E -> C2 + CN
    k(3660) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C3O+ + E -> C2 + CO
    k(3661) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C3S+ + E -> C2 + CS
    k(3662) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C3S+ + E -> C2S + C
    k(3663) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C3S+ + E -> C3 + S
    k(3664) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C4+ + E -> C2 + C2
    k(3665) = small + (1.75e-07&
        *(T32)**(-5.00e-01))

    !C4+ + E -> C3 + C
    k(3666) = small + (2.75e-07&
        *(T32)**(-5.00e-01))

    !CH2SI+ + E -> SI + CH2
    k(3667) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !CH2SI+ + E -> SIC + H2
    k(3668) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !CH2SI+ + E -> HCSI + H
    k(3669) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH3+ + E -> H2 + C + H
    k(3670) = small + (3.00e-07&
        *(T32)**(-3.00e-01))

    !CH3+ + E -> CH + H + H
    k(3671) = small + (1.60e-07&
        *(T32)**(-3.00e-01))

    !CH3+ + E -> CH + H2
    k(3672) = small + (1.40e-07&
        *(T32)**(-3.00e-01))

    !CH3+ + E -> CH2 + H
    k(3673) = small + (4.00e-07&
        *(T32)**(-3.00e-01))

    !H2CCL+ + E -> CCL + H + H
    k(3674) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HCNH+ + E -> CN + H + H
    k(3675) = small + (9.20e-08&
        *(T32)**(-6.50e-01))

    !HCNH+ + E -> HCN + H
    k(3676) = small + (1.85e-07&
        *(T32)**(-6.50e-01))

    !HCNH+ + E -> HNC + H
    k(3677) = small + (1.85e-07&
        *(T32)**(-6.50e-01))

    !H2CO+ + E -> CO + H + H
    k(3678) = small + (5.00e-07&
        *(T32)**(-5.00e-01))

    !H2CO+ + E -> HCO + H
    k(3679) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !H2CS+ + E -> CS + H + H
    k(3680) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H2CS+ + E -> HCS + H
    k(3681) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H2NC+ + E -> CN + H2
    k(3682) = small + (1.80e-08&
        *(T32)**(-5.00e-01))

    !H2NC+ + E -> HNC + H
    k(3683) = small + (1.80e-07&
        *(T32)**(-5.00e-01))

    !H2NO+ + E -> NO + H2
    k(3684) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2NO+ + E -> HNO + H
    k(3685) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2PO+ + E -> PH + OH
    k(3686) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2PO+ + E -> HPO + H
    k(3687) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2S2+ + E -> HS + HS
    k(3688) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2S2+ + E -> HS2 + H
    k(3689) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2SIO+ + E -> SIH + OH
    k(3690) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2SIO+ + E -> SIO + H2
    k(3691) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3O+ + E -> OH + H + H
    k(3692) = small + (2.60e-07&
        *(T32)**(-5.00e-01))

    !H3O+ + E -> H2O + H
    k(3693) = small + (1.10e-07&
        *(T32)**(-5.00e-01))

    !H3O+ + E -> OH + H2
    k(3694) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !H3O+ + E -> H2 + H + O
    k(3695) = small + (5.60e-09&
        *(T32)**(-5.00e-01))

    !H3S+ + E -> HS + H + H
    k(3696) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !H3S+ + E -> H2S + H
    k(3697) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HC2S+ + E -> CS + CH
    k(3698) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC2S+ + E -> C2S + H
    k(3699) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HCO2+ + E -> CO + H + O
    k(3700) = small + (8.10e-07&
        *(T32)**(-6.40e-01))

    !HCO2+ + E -> OH + CO
    k(3701) = small + (3.20e-07&
        *(T32)**(-6.40e-01))

    !HCO2+ + E -> CO2 + H
    k(3702) = small + (6.00e-08&
        *(T32)**(-6.40e-01))

    !HNCO+ + E -> CO + NH
    k(3703) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HOCS+ + E -> OH + CS
    k(3704) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !HOCS+ + E -> OCS + H
    k(3705) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !HSIO2+ + E -> SIO + OH
    k(3706) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSIO2+ + E -> SIO2 + H
    k(3707) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HSO2+ + E -> SO + H + O
    k(3708) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !HSO2+ + E -> SO + OH
    k(3709) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !HSO2+ + E -> SO2 + H
    k(3710) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !NAH2O+ + E -> NA + H2O
    k(3711) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NAH2O+ + E -> NAOH + H
    k(3712) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH3+ + E -> NH + H + H
    k(3713) = small + (1.55e-07&
        *(T32)**(-5.00e-01))

    !NH3+ + E -> NH2 + H
    k(3714) = small + (1.55e-07&
        *(T32)**(-5.00e-01))

    !PC2H+ + E -> CP + CH
    k(3715) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PC2H+ + E -> CCP + H
    k(3716) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH2+ + E -> CP + H2
    k(3717) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH2+ + E -> HCP + H
    k(3718) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PH3+ + E -> PH + H2
    k(3719) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PH3+ + E -> PH2 + H
    k(3720) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PNH2+ + E -> NH2 + P
    k(3721) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !SIC2H+ + E -> C2H + SI
    k(3722) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC2H+ + E -> SIC2 + H
    k(3723) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3+ + E -> SIC + C2
    k(3724) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3+ + E -> SIC2 + C
    k(3725) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH3+ + E -> SIH + H2
    k(3726) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH3+ + E -> SIH2 + H
    k(3727) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SINCH+ + E -> SIN + CH
    k(3728) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SINCH+ + E -> SINC + H
    k(3729) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SINH2+ + E -> SIN + H2
    k(3730) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SINH2+ + E -> HNSI + H
    k(3731) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H2N+ + E -> CH + HCN
    k(3732) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H2N+ + E -> CN + CH2
    k(3733) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H2N+ + E -> C2N + H2
    k(3734) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H2O+ + E -> C2 + H2O
    k(3735) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H2O+ + E -> CH2 + CO
    k(3736) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H2O+ + E -> C2H2 + O
    k(3737) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H3+ + E -> C2H + H + H
    k(3738) = small + (2.95e-07&
        *(T32)**(-8.40e-01))

    !C2H3+ + E -> C2H + H2
    k(3739) = small + (3.00e-08&
        *(T32)**(-8.40e-01))

    !C2H3+ + E -> C2H2 + H
    k(3740) = small + (1.45e-07&
        *(T32)**(-8.40e-01))

    !C2H3+ + E -> C2 + H + H2
    k(3741) = small + (1.50e-08&
        *(T32)**(-8.40e-01))

    !C2H3+ + E -> CH3 + C
    k(3742) = small + (3.00e-09&
        *(T32)**(-8.40e-01))

    !C2H3+ + E -> CH2 + CH
    k(3743) = small + (1.50e-08&
        *(T32)**(-8.40e-01))

    !C3H2+ + E -> C2 + CH2
    k(3744) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !H2C3+ + E -> C2 + CH2
    k(3745) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !C3H2+ + E -> C3 + H + H
    k(3746) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !H2C3+ + E -> C3 + H + H
    k(3747) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !C3H2+ + E -> C3 + H2
    k(3748) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2C3+ + E -> C3 + H2
    k(3749) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H2+ + E -> C2H2 + C
    k(3750) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !H2C3+ + E -> C2H2 + C
    k(3751) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !C3H2+ + E -> C3H + H
    k(3752) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3HN+ + E -> C2 + HCN
    k(3753) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C3HN+ + E -> C2H + CN
    k(3754) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3HN+ + E -> C3N + H
    k(3755) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H+ + E -> C2H + C2
    k(3756) = small + (9.00e-08&
        *(T32)**(-5.00e-01))

    !C4H+ + E -> C3H + C
    k(3757) = small + (4.50e-08&
        *(T32)**(-5.00e-01))

    !C4H+ + E -> C3 + CH
    k(3758) = small + (4.50e-08&
        *(T32)**(-5.00e-01))

    !C4H+ + E -> C4 + H
    k(3759) = small + (1.20e-07&
        *(T32)**(-5.00e-01))

    !C4N+ + E -> C2N + C2
    k(3760) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4N+ + E -> C3N + C
    k(3761) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4P+ + E -> CCP + C2
    k(3762) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4P+ + E -> C3P + C
    k(3763) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4S+ + E -> C2S + C2
    k(3764) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C4S+ + E -> C3 + CS
    k(3765) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C4S+ + E -> C3S + C
    k(3766) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !C5+ + E -> C3 + C2
    k(3767) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5+ + E -> C4 + C
    k(3768) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH2O2+ + E -> CO2 + H + H
    k(3769) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH4+ + E -> CH2 + H + H
    k(3770) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH4+ + E -> CH3 + H
    k(3771) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H3CO+ + E -> CO + H + H2
    k(3772) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !H3CO+ + E -> HCO + H + H
    k(3773) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !H3CO+ + E -> H2CO + H
    k(3774) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !H3CS+ + E -> CS + H + H2
    k(3775) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H3CS+ + E -> H2CS + H
    k(3776) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H3S2+ + E -> HS2 + H2
    k(3777) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3S2+ + E -> H2S2 + H
    k(3778) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3SIO+ + E -> SIO + H2 + H
    k(3779) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3SIO+ + E -> H2SIO + H
    k(3780) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC3O+ + E -> C3H + O
    k(3781) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC3O+ + E -> C3O + H
    k(3782) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC3S+ + E -> C2S + CH
    k(3783) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC3S+ + E -> C3S + H
    k(3784) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH4+ + E -> NH2 + H + H
    k(3785) = small + (3.20e-07&
        *(T32)**(-5.00e-01))

    !NH4+ + E -> NH2 + H2
    k(3786) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH4+ + E -> NH3 + H
    k(3787) = small + (1.05e-06&
        *(T32)**(-5.00e-01))

    !OCS+H2 + E -> OCS + H2
    k(3788) = small + (3.00e-07)

    !PC2H2+ + E -> P + C2H2
    k(3789) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H2+ + E -> CCP + H2
    k(3790) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H2+ + E -> HCCP + H
    k(3791) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC3H+ + E -> CCP + CH
    k(3792) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC3H+ + E -> C3P + H
    k(3793) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC3H+ + E -> HCCP + C
    k(3794) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PCH3+ + E -> CP + H2 + H
    k(3795) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH3+ + E -> HCP + H2
    k(3796) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH3+ + E -> CH3 + P
    k(3797) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !PNH3+ + E -> NH3 + P
    k(3798) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !SIC2H2+ + E -> SIC2 + H2
    k(3799) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC2H2+ + E -> SIC2H + H
    k(3800) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3H+ + E -> SI + C3H
    k(3801) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3H+ + E -> SIC3 + H
    k(3802) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC4+ + E -> SIC2 + C2
    k(3803) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC4+ + E -> SIC3 + C
    k(3804) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SICH3+ + E -> HCSI + H2
    k(3805) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SICH3+ + E -> SICH2 + H
    k(3806) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH4+ + E -> SIH2 + H2
    k(3807) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH4+ + E -> SIH3 + H
    k(3808) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H3N+ + E -> C2N + H2 + H
    k(3809) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H3N+ + E -> CH2 + HCN
    k(3810) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H3N+ + E -> CH3 + CN
    k(3811) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H3N+ + E -> C2H2N + H
    k(3812) = small + (2.00e-07&
        *(T32)**(-5.00e-01))

    !C2H3O+ + E -> CH3 + CO
    k(3813) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H3O+ + E -> C2H2O + H
    k(3814) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4+ + E -> C2H2 + H + H
    k(3815) = small + (3.70e-07&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> C2H2 + H2
    k(3816) = small + (3.36e-08&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> C2H3 + H
    k(3817) = small + (6.16e-08&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> C2H + H2 + H
    k(3818) = small + (5.60e-08&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> CH4 + C
    k(3819) = small + (5.60e-09&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> CH3 + CH
    k(3820) = small + (1.12e-08&
        *(T32)**(-7.60e-01))

    !C2H4+ + E -> CH2 + CH2
    k(3821) = small + (2.24e-08&
        *(T32)**(-7.60e-01))

    !C3H2N+ + E -> C2H + HNC
    k(3822) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !C3H2N+ + E -> HCNC2 + H
    k(3823) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !C3H2N+ + E -> HC2NC + H
    k(3824) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !C3H2N+ + E -> HC3N + H
    k(3825) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H2N+ + E -> HNC3 + H
    k(3826) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !C3H3+ + E -> C2H2 + CH
    k(3827) = small + (6.99e-08&
        *(T32)**(-5.00e-01))

    !H3C3+ + E -> C2H2 + CH
    k(3828) = small + (6.99e-08&
        *(T32)**(-5.00e-01))

    !C3H3+ + E -> C3H + H2
    k(3829) = small + (3.15e-07&
        *(T32)**(-5.00e-01))

    !C3H3+ + E -> C3H2 + H
    k(3830) = small + (3.15e-07&
        *(T32)**(-5.00e-01))

    !C4H2+ + E -> C4 + H2
    k(3831) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H2+ + E -> C4H + H
    k(3832) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H+ + E -> C4H + C
    k(3833) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H+ + E -> C5 + H
    k(3834) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5N+ + E -> C2 + C3N
    k(3835) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C6+ + E -> C4 + C2
    k(3836) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6+ + E -> C5 + C
    k(3837) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !CH3O2+ + E -> CO2 + H2 + H
    k(3838) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH3O2+ + E -> CH2O2 + H
    k(3839) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH4N+ + E -> CN + H2 + H2
    k(3840) = small + (3.00e-08&
        *(T32)**(-5.00e-01))

    !CH4N+ + E -> CH2 + NH2
    k(3841) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH4N+ + E -> HCN + H + H2
    k(3842) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH4N+ + E -> CH3N + H
    k(3843) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH4O+ + E -> CH3 + OH
    k(3844) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH4O+ + E -> H2CO + H2
    k(3845) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH5+ + E -> CH3 + H2
    k(3846) = small + (1.40e-08&
        *(T32)**(-5.20e-01))

    !CH5+ + E -> CH4 + H
    k(3847) = small + (1.40e-08&
        *(T32)**(-5.20e-01))

    !CH5+ + E -> CH3 + H + H
    k(3848) = small + (1.95e-07&
        *(T32)**(-5.20e-01))

    !CH5+ + E -> CH2 + H2 + H
    k(3849) = small + (4.80e-08&
        *(T32)**(-5.20e-01))

    !CH5+ + E -> CH + H2 + H2
    k(3850) = small + (3.00e-09&
        *(T32)**(-5.20e-01))

    !H2C3O+ + E -> C2H2 + CO
    k(3851) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2C3O+ + E -> C3H + OH
    k(3852) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2C3O+ + E -> C3O + H + H
    k(3853) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H2C3O+ + E -> C3O + H2
    k(3854) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> C2H + HCN
    k(3855) = small + (2.28e-07&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> C2H2 + CN
    k(3856) = small + (2.28e-07&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> HCNC2 + H
    k(3857) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> HC2NC + H
    k(3858) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> HC3N + H
    k(3859) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !HC2NCH+ + E -> HNC3 + H
    k(3860) = small + (1.20e-08&
        *(T32)**(-5.00e-01))

    !HC4N+ + E -> C3N + CH
    k(3861) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !HC4O+ + E -> C3H + CO
    k(3862) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC4O+ + E -> C3O + CH
    k(3863) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !HC4S+ + E -> C2S + C2H
    k(3864) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !HC4S+ + E -> C3S + CH
    k(3865) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !HC4S+ + E -> C4S + H
    k(3866) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !NH2CNH+ + E -> NH2 + HNC
    k(3867) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CNH+ + E -> NH2CN + H
    k(3868) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PC2H3+ + E -> PH + C2H2
    k(3869) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H3+ + E -> CCP + H2 + H
    k(3870) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H3+ + E -> HCCP + H2
    k(3871) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC4H+ + E -> CP + C3H
    k(3872) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !PC4H+ + E -> CCP + C2H
    k(3873) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !PC4H+ + E -> C3P + CH
    k(3874) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !PC4H+ + E -> C4P + H
    k(3875) = small + (7.50e-08&
        *(T32)**(-5.00e-01))

    !PCH4+ + E -> HCP + H2 + H
    k(3876) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH4+ + E -> CH2PH + H
    k(3877) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PCH4+ + E -> CH4 + P
    k(3878) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !SIC2H3+ + E -> SIC2H + H2
    k(3879) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC2H3+ + E -> SIC2H2 + H
    k(3880) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3H2+ + E -> SIC3 + H2
    k(3881) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC3H2+ + E -> SIC3H + H
    k(3882) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC4H+ + E -> SIC3 + CH
    k(3883) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIC4H+ + E -> SIC4 + H
    k(3884) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SICH4+ + E -> SICH2 + H2
    k(3885) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SICH4+ + E -> SICH3 + H
    k(3886) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH5+ + E -> SIH3 + H2
    k(3887) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !SIH5+ + E -> SIH4 + H
    k(3888) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4N+ + E -> C2H2N + H + H
    k(3889) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4N+ + E -> C2H3N + H
    k(3890) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4O+ + E -> CH3 + HCO
    k(3891) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4O+ + E -> C2H2O + H + H
    k(3892) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H4O+ + E -> C2H2O + H2
    k(3893) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5+ + E -> C2H + H2 + H2
    k(3894) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5+ + E -> C2H2 + H2 + H
    k(3895) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H5+ + E -> C2H3 + H2
    k(3896) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5+ + E -> C2H4 + H
    k(3897) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H3N+ + E -> C3N + H2 + H
    k(3898) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H3N+ + E -> HC3N + H2
    k(3899) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H4+ + E -> C3H2 + H2
    k(3900) = small + (2.95e-08&
        *(T32)**(-6.70e-01))

    !C3H4+ + E -> C3H3 + H
    k(3901) = small + (2.57e-06&
        *(T32)**(-6.70e-01))

    !C3H4+ + E -> C2H3 + CH
    k(3902) = small + (2.95e-08&
        *(T32)**(-6.70e-01))

    !C3H4+ + E -> C2H2 + CH2
    k(3903) = small + (1.77e-07&
        *(T32)**(-6.70e-01))

    !C3H4+ + E -> C2H + CH3
    k(3904) = small + (2.95e-08&
        *(T32)**(-6.70e-01))

    !C3H4+ + E -> C3H2 + H + H
    k(3905) = small + (1.18e-07&
        *(T32)**(-6.70e-01))

    !C4H3+ + E -> C4H + H2
    k(3906) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H3+ + E -> C4H2 + H
    k(3907) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H2+ + E -> C5 + H2
    k(3908) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H2+ + E -> C5H + H
    k(3909) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5HN+ + E -> CN + C4H
    k(3910) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5HN+ + E -> C3N + C2H
    k(3911) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5HN+ + E -> C5N + H
    k(3912) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C6H+ + E -> C5H + C
    k(3913) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H+ + E -> C6 + H
    k(3914) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7+ + E -> C4 + C3
    k(3915) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C7+ + E -> C5 + C2
    k(3916) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7+ + E -> C6 + C
    k(3917) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !CH5N+ + E -> CH3 + NH2
    k(3918) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH5N+ + E -> CH3N + H2
    k(3919) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH5O+ + E -> H2CO + H2 + H
    k(3920) = small + (9.10e-08&
        *(T32)**(-6.70e-01))

    !CH5O+ + E -> CH3 + H2O
    k(3921) = small + (8.19e-08&
        *(T32)**(-6.70e-01))

    !CH5O+ + E -> CH3 + OH + H
    k(3922) = small + (4.64e-07&
        *(T32)**(-6.70e-01))

    !CH5O+ + E -> CH2 + H2O + H
    k(3923) = small + (1.91e-07&
        *(T32)**(-6.70e-01))

    !CH5O+ + E -> CH4O + H
    k(3924) = small + (2.73e-08&
        *(T32)**(-6.70e-01))

    !H2C4N+ + E -> HC3N + CH
    k(3925) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !H3C3O+ + E -> C3H + H2O
    k(3926) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3C3O+ + E -> C3O + H2 + H
    k(3927) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CH2O+ + E -> NH2CHO + H
    k(3928) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CH2O+ + E -> NH3 + CO + H
    k(3929) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CH2O+ + E -> H2CN + H2O
    k(3930) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CH2O+ + E -> OCN + H2 + H2
    k(3931) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !NH2CH2O+ + E -> NH2 + H2CO
    k(3932) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PC2H4+ + E -> P + C2H4
    k(3933) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H4+ + E -> CCP + H2 + H2
    k(3934) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC2H4+ + E -> HCCP + H2 + H
    k(3935) = small + (1.00e-07&
        *(T32)**(-5.00e-01))

    !PC4H2+ + E -> CCP + C2H2
    k(3936) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !PC4H2+ + E -> C4P + H2
    k(3937) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5O+ + E -> H2CO + CH3
    k(3938) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5O+ + E -> C2H2O + H2 + H
    k(3939) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5O+ + E -> CH4 + CO + H
    k(3940) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H5O+ + E -> C2H4O + H
    k(3941) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H6+ + E -> C2H4 + H2
    k(3942) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H6+ + E -> C2H5 + H
    k(3943) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H4N+ + E -> C3N + H2 + H2
    k(3944) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H4N+ + E -> HC3N + H2 + H
    k(3945) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C3H4N+ + E -> C3H3N + H
    k(3946) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C3H5+ + E -> C3H3 + H2
    k(3947) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H5+ + E -> C3H4 + H
    k(3948) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H4+ + E -> C4H + H2 + H
    k(3949) = small + (3.30e-07&
        *(T32)**(-5.00e-01))

    !C4H4+ + E -> C4H2 + H2
    k(3950) = small + (3.30e-07&
        *(T32)**(-5.00e-01))

    !C4H4+ + E -> C4H3 + H
    k(3951) = small + (3.30e-07&
        *(T32)**(-5.00e-01))

    !C5H2N+ + E -> C5N + H2
    k(3952) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H2N+ + E -> HC5N + H
    k(3953) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H3+ + E -> C5H + H2
    k(3954) = small + (4.50e-07&
        *(T32)**(-5.00e-01))

    !C5H3+ + E -> C5H2 + H
    k(3955) = small + (4.50e-07&
        *(T32)**(-5.00e-01))

    !C6H2+ + E -> C6 + H + H
    k(3956) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C6H2+ + E -> C6 + H2
    k(3957) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H2+ + E -> C6H + H
    k(3958) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H+ + E -> C6H + C
    k(3959) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H+ + E -> C7 + H
    k(3960) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7N+ + E -> C2 + C5N
    k(3961) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C8+ + E -> C6 + C2
    k(3962) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8+ + E -> C7 + C
    k(3963) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !CH6N+ + E -> CH3N + H2 + H
    k(3964) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH6N+ + E -> CH5N + H
    k(3965) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !COOCH4+ + E -> CH3 + CO2 + H
    k(3966) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !COOCH4+ + E -> CH4O + CO
    k(3967) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H3C4N+ + E -> HC3N + CH2
    k(3968) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH+ + E -> C2H2O + H2 + H2
    k(3969) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH+ + E -> CH4 + H2CO
    k(3970) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH+ + E -> C2H4O + H2
    k(3971) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH+ + E -> C2H5 + OH
    k(3972) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H4N+ + E -> CH3 + HC3N
    k(3973) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C4H4N+ + E -> CH3C3N + H
    k(3974) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C4H5+ + E -> C4H + H2 + H2
    k(3975) = small + (1.01e-07&
        *(T32)**(-5.00e-01))

    !C4H5+ + E -> C4H2 + H + H2
    k(3976) = small + (1.01e-07&
        *(T32)**(-5.00e-01))

    !C4H5+ + E -> C3H4 + CH
    k(3977) = small + (4.50e-08&
        *(T32)**(-5.00e-01))

    !C4H5+ + E -> C2H2 + C2H3
    k(3978) = small + (1.01e-07&
        *(T32)**(-5.00e-01))

    !C4H5+ + E -> C2H + C2H4
    k(3979) = small + (1.01e-07&
        *(T32)**(-5.00e-01))

    !C5H3N+ + E -> C5N + H2 + H
    k(3980) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H3N+ + E -> HC5N + H2
    k(3981) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H4+ + E -> C5H + H + H2
    k(3982) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H4+ + E -> C5H2 + H2
    k(3983) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H3+ + E -> C6H + H + H
    k(3984) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !C6H3+ + E -> C6H + H2
    k(3985) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H3+ + E -> C6H2 + H
    k(3986) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H2+ + E -> C7 + H2
    k(3987) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H2+ + E -> C7H + H
    k(3988) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7HN+ + E -> CN + C6H
    k(3989) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7HN+ + E -> C7N + H
    k(3990) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H+ + E -> C7H + C
    k(3991) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H+ + E -> C8 + H
    k(3992) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9+ + E -> C7 + C2
    k(3993) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9+ + E -> C8 + C
    k(3994) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !CH3OCH3+ + E -> CH3 + CH3 + O
    k(3995) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH3OCH3+ + E -> H2CO + CH4
    k(3996) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH3OCH3+ + E -> CH4O + CH2
    k(3997) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H5C2O2+ + E -> CH4O + HCO
    k(3998) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !H5C2O2+ + E -> HCOOCH3 + H
    k(3999) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C10+ + E -> C8 + C2
    k(4000) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C10+ + E -> C9 + C
    k(4001) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C10H+ + E -> C10 + H
    k(4002) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C10H+ + E -> C9H + C
    k(4003) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C10H2+ + E -> C10 + H2
    k(4004) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C2H5OH2+ + E -> C2H4 + H2O + H
    k(4005) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH2+ + E -> C2H4O + H2 + H
    k(4006) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H5OH2+ + E -> C2H5OH + H
    k(4007) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H6CO+ + E -> CH3 + CH3 + CO
    k(4008) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C2H6CO+ + E -> C2H4O + CH2
    k(4009) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H4N+ + E -> C5N + H2 + H2
    k(4010) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H4N+ + E -> HC5N + H2 + H
    k(4011) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C5H5+ + E -> C5H2 + H2 + H
    k(4012) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C5H5+ + E -> CH3C4H + H
    k(4013) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H4+ + E -> C6H + H2 + H
    k(4014) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H4+ + E -> C6H2 + H2
    k(4015) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H2N+ + E -> C7N + H2
    k(4016) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C7H2N+ + E -> HC7N + H
    k(4017) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C7H3+ + E -> C7H + H2
    k(4018) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H3+ + E -> C7H2 + H
    k(4019) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H2+ + E -> C8 + H2
    k(4020) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H2+ + E -> C8H + H
    k(4021) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H+ + E -> C8H + C
    k(4022) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H+ + E -> C9 + H
    k(4023) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9N+ + E -> C2 + C7N
    k(4024) = small + (3.00e-07&
        *(T32)**(-5.00e-01))

    !CH3OCH4+ + E -> CH3 + CH4 + O
    k(4025) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH3OCH4+ + E -> CH4O + CH3
    k(4026) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !CH3OCH4+ + E -> CH3OCH3 + H
    k(4027) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H6OH+ + E -> C2H4O + CH3
    k(4028) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C3H6OH+ + E -> C2H6CO + H
    k(4029) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C4H7+ + E -> C4H + H2 + H2 + H2
    k(4030) = small + (6.00e-08&
        *(T32)**(-5.00e-01))

    !C4H7+ + E -> C3H3 + CH4
    k(4031) = small + (1.95e-07&
        *(T32)**(-5.00e-01))

    !C4H7+ + E -> C2H3 + C2H4
    k(4032) = small + (2.25e-08&
        *(T32)**(-5.00e-01))

    !C4H7+ + E -> C2H2 + C2H5
    k(4033) = small + (2.25e-08&
        *(T32)**(-5.00e-01))

    !C6H4N+ + E -> CH3 + HC5N
    k(4034) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H4N+ + E -> CH3C5N + H
    k(4035) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H5+ + E -> C6H + H2 + H2
    k(4036) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H5+ + E -> C6H2 + H2 + H
    k(4037) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H3N+ + E -> C7N + H2 + H
    k(4038) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H3N+ + E -> HC7N + H2
    k(4039) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H4+ + E -> C7H + H2 + H
    k(4040) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H4+ + E -> C7H2 + H2
    k(4041) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H3+ + E -> C8H + H2
    k(4042) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H3+ + E -> C8H2 + H
    k(4043) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H2+ + E -> C9 + H2
    k(4044) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H2+ + E -> C9H + H
    k(4045) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9HN+ + E -> CN + C8H
    k(4046) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9HN+ + E -> C9N + H
    k(4047) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C7H5+ + E -> C7H2 + H2 + H
    k(4048) = small + (3.50e-07&
        *(T32)**(-5.00e-01))

    !C7H5+ + E -> CH3C6H + H
    k(4049) = small + (3.50e-07&
        *(T32)**(-5.00e-01))

    !C8H4+ + E -> C8H + H2 + H
    k(4050) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H4+ + E -> C8H2 + H2
    k(4051) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H2N+ + E -> C9N + H2
    k(4052) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H2N+ + E -> HC9N + H
    k(4053) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H3+ + E -> C9H + H2
    k(4054) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H3+ + E -> C9H2 + H
    k(4055) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C6H7+ + E -> C6H6 + H
    k(4056) = small + (5.00e-07&
        *(T32)**(-5.00e-01))

    !C6H7+ + E -> C6H2 + H2 + H2 + H
    k(4057) = small + (5.00e-07&
        *(T32)**(-5.00e-01))

    !C8H4N+ + E -> CH3 + HC7N
    k(4058) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H4N+ + E -> CH3C7N + H
    k(4059) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C8H5+ + E -> C8H + H2 + H2
    k(4060) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C8H5+ + E -> C8H2 + H + H2
    k(4061) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C9H3N+ + E -> C9N + H2 + H
    k(4062) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H3N+ + E -> HC9N + H2
    k(4063) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H4+ + E -> C9H + H2 + H
    k(4064) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H4+ + E -> C9H2 + H2
    k(4065) = small + (1.00e-06&
        *(T32)**(-3.00e-01))

    !C9H5+ + E -> C9H + H2 + H2
    k(4066) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C9H5+ + E -> C9H2 + H2 + H
    k(4067) = small + (1.50e-07&
        *(T32)**(-5.00e-01))

    !C+ + E -> C
    k(4068) = small + (4.40e-12&
        *(T32)**(-6.10e-01))

    !CL+ + E -> CL
    k(4069) = small + (1.13e-10&
        *(T32)**(-7.00e-01))

    !FE+ + E -> FE
    k(4070) = small + (3.70e-12&
        *(T32)**(-6.50e-01))

    !H+ + E -> H
    k(4071) = small + (3.50e-12&
        *(T32)**(-7.00e-01))

    !HE+ + E -> HE
    k(4072) = small + (4.50e-12&
        *(T32)**(-6.70e-01))

    !MG+ + E -> MG
    k(4073) = small + (2.80e-12&
        *(T32)**(-8.60e-01))

    !N+ + E -> N
    k(4074) = small + (3.80e-12&
        *(T32)**(-6.20e-01))

    !NA+ + E -> NA
    k(4075) = small + (2.70e-12&
        *(T32)**(-6.90e-01))

    !O+ + E -> O
    k(4076) = small + (3.40e-12&
        *(T32)**(-6.30e-01))

    !S+ + E -> S
    k(4077) = small + (3.90e-12&
        *(T32)**(-6.30e-01))

    !SI+ + E -> SI
    k(4078) = small + (4.90e-12&
        *(T32)**(-6.00e-01))

    !H2+ + E -> H2
    k(4079) = small + (2.25e-07&
        *(T32)**(-4.00e-01))

    !H2S+ + E -> H2S
    k(4080) = small + (1.10e-10&
        *(T32)**(-7.00e-01))

    !CH3+ + E -> CH3
    k(4081) = small + (1.10e-10&
        *(T32)**(-7.00e-01))

    !H2CO+ + E -> H2CO
    k(4082) = small + (1.10e-10&
        *(T32)**(-7.00e-01))

    !H2CS+ + E -> H2CS
    k(4083) = small + (1.10e-10&
        *(T32)**(-7.00e-01))

    !C+ + C- -> C + C
    k(4084) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !C+ + H- -> H + C
    k(4085) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !C+ + S- -> C + S
    k(4086) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !FE+ + C- -> FE + C
    k(4087) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !FE+ + H- -> FE + H
    k(4088) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !FE+ + S- -> FE + S
    k(4089) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H+ + C- -> H + C
    k(4090) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H+ + H- -> H + H
    k(4091) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H+ + S- -> H + S
    k(4092) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !HE+ + C- -> HE + C
    k(4093) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !HE+ + H- -> H + HE
    k(4094) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !HE+ + S- -> HE + S
    k(4095) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !MG+ + C- -> MG + C
    k(4096) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !MG+ + H- -> MG + H
    k(4097) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !MG+ + S- -> MG + S
    k(4098) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !N+ + C- -> N + C
    k(4099) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !N+ + H- -> H + N
    k(4100) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !N+ + S- -> N + S
    k(4101) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !NA+ + C- -> NA + C
    k(4102) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !NA+ + H- -> NA + H
    k(4103) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !NA+ + S- -> NA + S
    k(4104) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !O+ + C- -> O + C
    k(4105) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !O+ + H- -> H + O
    k(4106) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !O+ + S- -> O + S
    k(4107) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !S+ + C- -> S + C
    k(4108) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !S+ + H- -> S + H
    k(4109) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !S+ + S- -> S + S
    k(4110) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !SI+ + C- -> SI + C
    k(4111) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !SI+ + H- -> SI + H
    k(4112) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !SI+ + S- -> SI + S
    k(4113) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H2+ + H- -> H + H2
    k(4114) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H3+ + H- -> H2 + H2
    k(4115) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !HCO+ + H- -> CO + H2
    k(4116) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H3O+ + H- -> OH + H2 + H
    k(4117) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !H3O+ + H- -> H2O + H2
    k(4118) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !NH4+ + H- -> NH3 + H2
    k(4119) = small + (2.30e-07&
        *(T32)**(-5.00e-01))

    !C + E -> C-
    k(4120) = small + (3.00e-15)

    !H + E -> H-
    k(4121) = small + (3.00e-16*(T32)**(1.00e+00))

    !O + E -> O-
    k(4122) = small + (1.50e-15)

    !S + E -> S-
    k(4123) = small + (5.00e-15)

    !C -> C+ + E
    k(4124) = small + (2.16e-10*exp(-2.61e+00&
        *user_Av))

    !CL -> CL+ + E
    k(4125) = small + (3.30e-11*exp(-3.10e+00&
        *user_Av))

    !FE -> FE+ + E
    k(4126) = small + (1.20e-10*exp(-1.50e+00&
        *user_Av))

    !MG -> MG+ + E
    k(4127) = small + (4.50e-11*exp(-1.40e+00&
        *user_Av))

    !NA -> NA+ + E
    k(4128) = small + (5.60e-12*exp(-2.00e+00&
        *user_Av))

    !S -> S+ + E
    k(4129) = small + (7.20e-10*exp(-2.40e+00&
        *user_Av))

    !SI -> SI+ + E
    k(4130) = small + (1.20e-09*exp(-1.60e+00&
        *user_Av))

    !C2 -> C + C
    k(4131) = small + (4.70e-11*exp(-2.60e+00&
        *user_Av))

    !C2 -> C2+ + E
    k(4132) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !CCL -> CL + C
    k(4133) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !CH -> C + H
    k(4134) = small + (1.40e-10*exp(-1.50e+00&
        *user_Av))

    !CH -> CH+ + E
    k(4135) = small + (2.90e-10*exp(-2.80e+00&
        *user_Av))

    !CLO -> CL + O
    k(4136) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !CN -> C + N
    k(4137) = small + (1.00e-09*exp(-2.80e+00&
        *user_Av))

    !CO -> C + O
    k(4138) = small + (3.10e-11*exp(-2.54e+00&
        *user_Av))

    !CS -> C + S
    k(4139) = small + (9.70e-10*exp(-2.00e+00&
        *user_Av))

    !H2 -> H + H
    k(4140) = small + (3.40e-11*exp(-2.50e+00&
        *user_Av))

    !HCL -> CL + H
    k(4141) = small + (1.10e-10*exp(-1.80e+00&
        *user_Av))

    !HS -> H + S
    k(4142) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !N2 -> N + N
    k(4143) = small + (5.00e-12*exp(-3.00e+00&
        *user_Av))

    !NH -> H + N
    k(4144) = small + (4.00e-10*exp(-1.50e+00&
        *user_Av))

    !NH -> NH+ + E
    k(4145) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !NO -> N + O
    k(4146) = small + (3.00e-10*exp(-2.00e+00&
        *user_Av))

    !NO -> NO+ + E
    k(4147) = small + (2.00e-10*exp(-2.00e+00&
        *user_Av))

    !NS -> N + S
    k(4148) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !O2 -> O + O
    k(4149) = small + (3.30e-10*exp(-1.40e+00&
        *user_Av))

    !O2 -> O2+ + E
    k(4150) = small + (6.20e-12*exp(-3.10e+00&
        *user_Av))

    !OH -> O + H
    k(4151) = small + (1.68e-10*exp(-1.66e+00&
        *user_Av))

    !OH -> OH+ + E
    k(4152) = small + (1.60e-12*exp(-3.10e+00&
        *user_Av))

    !SIC -> SI + C
    k(4153) = small + (1.00e-10*exp(-2.30e+00&
        *user_Av))

    !SIH -> SI + H
    k(4154) = small + (1.00e-10*exp(-2.30e+00&
        *user_Av))

    !SIO -> SI + O
    k(4155) = small + (1.00e-10*exp(-2.30e+00&
        *user_Av))

    !SIS -> SI + S
    k(4156) = small + (1.00e-10*exp(-2.30e+00&
        *user_Av))

    !SO -> O + S
    k(4157) = small + (3.30e-10*exp(-1.40e+00&
        *user_Av))

    !C2H -> C2 + H
    k(4158) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C2H -> C2H+ + E
    k(4159) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !C2N -> C2 + N
    k(4160) = small + (1.00e-10*exp(-1.70e+00&
        *user_Av))

    !C2N -> CN + C
    k(4161) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C2S -> C2 + S
    k(4162) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !C3 -> C2 + C
    k(4163) = small + (2.60e-10*exp(-2.28e+00&
        *user_Av))

    !CH2 -> CH + H
    k(4164) = small + (5.00e-11*exp(-1.70e+00&
        *user_Av))

    !CH2 -> CH2+ + E
    k(4165) = small + (1.00e-09*exp(-2.30e+00&
        *user_Av))

    !CO2 -> CO + O
    k(4166) = small + (3.13e-10*exp(-2.03e+00&
        *user_Av))

    !H2O -> OH + H
    k(4167) = small + (3.28e-10*exp(-1.63e+00&
        *user_Av))

    !H2O -> H2O+ + E
    k(4168) = small + (2.10e-11*exp(-3.10e+00&
        *user_Av))

    !H2S -> HS + H
    k(4169) = small + (3.20e-10*exp(-1.70e+00&
        *user_Av))

    !HCN -> CN + H
    k(4170) = small + (5.48e-10*exp(-2.00e+00&
        *user_Av))

    !HCO -> H + CO
    k(4171) = small + (5.87e-10*exp(-5.30e-01&
        *user_Av))

    !HCO -> HCO+ + E
    k(4172) = small + (2.46e-10*exp(-2.11e+00&
        *user_Av))

    !HNC -> CN + H
    k(4173) = small + (5.48e-10*exp(-2.00e+00&
        *user_Av))

    !HNO -> NO + H
    k(4174) = small + (1.70e-10*exp(-5.30e-01&
        *user_Av))

    !NAOH -> NA + OH
    k(4175) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !NH2 -> NH + H
    k(4176) = small + (2.11e-10*exp(-1.52e+00&
        *user_Av))

    !NH2 -> NH2+ + E
    k(4177) = small + (1.73e-10*exp(-2.59e+00&
        *user_Av))

    !NO2 -> NO + O
    k(4178) = small + (1.29e-09*exp(-2.00e+00&
        *user_Av))

    !OCN -> O + CN
    k(4179) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !OCS -> CO + S
    k(4180) = small + (2.28e-09*exp(-1.60e+00&
        *user_Av))

    !OCS -> OCS+ + E
    k(4181) = small + (2.37e-10*exp(-2.71e+00&
        *user_Av))

    !SO2 -> SO + O
    k(4182) = small + (1.05e-09*exp(-1.74e+00&
        *user_Av))

    !C2H2 -> C2H + H
    k(4183) = small + (1.81e-09*exp(-1.72e+00&
        *user_Av))

    !C2H2 -> C2H2+ + E
    k(4184) = small + (2.07e-10&
        *exp(-2.67e+00*user_Av))

    !C3H -> C3 + H
    k(4185) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C3N -> C2 + CN
    k(4186) = small + (5.00e-10*exp(-1.80e+00&
        *user_Av))

    !C3O -> C2 + CO
    k(4187) = small + (4.52e-09*exp(-1.58e+00&
        *user_Av))

    !C3S -> C2 + CS
    k(4188) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !C4 -> C2 + C2
    k(4189) = small + (2.00e-10*exp(-2.30e+00&
        *user_Av))

    !C4 -> C3 + C
    k(4190) = small + (2.00e-10*exp(-2.30e+00&
        *user_Av))

    !CH3 -> CH + H2
    k(4191) = small + (3.00e-11*exp(-1.70e+00&
        *user_Av))

    !CH3 -> CH2 + H
    k(4192) = small + (3.00e-11*exp(-1.70e+00&
        *user_Av))

    !CH3 -> CH3+ + E
    k(4193) = small + (1.00e-10*exp(-2.10e+00&
        *user_Av))

    !H2CO -> CO + H + H
    k(4194) = small + (4.40e-10&
        *exp(-1.60e+00*user_Av))

    !H2CO -> CO + H2
    k(4195) = small + (4.40e-10*exp(-1.60e+00&
        *user_Av))

    !H2CO -> HCO+ + H + E
    k(4196) = small + (1.40e-11&
        *exp(-3.10e+00*user_Av))

    !H2CO -> H2CO+ + E
    k(4197) = small + (8.00e-11&
        *exp(-2.80e+00*user_Av))

    !H2CS -> CS + H2
    k(4198) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !NH3 -> NH + H2
    k(4199) = small + (1.30e-10*exp(-1.91e+00&
        *user_Av))

    !NH3 -> NH2 + H
    k(4200) = small + (4.94e-10*exp(-1.65e+00&
        *user_Av))

    !NH3 -> NH3+ + E
    k(4201) = small + (1.24e-10*exp(-2.47e+00&
        *user_Av))

    !C2H2O -> CH2 + CO
    k(4202) = small + (9.04e-10&
        *exp(-1.58e+00*user_Av))

    !C2H2O -> C2H2O+ + E
    k(4203) = small + (3.44e-10&
        *exp(-2.01e+00*user_Av))

    !C2H3 -> C2H2 + H
    k(4204) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C2H3 -> C2H3+ + E
    k(4205) = small + (3.00e-10&
        *exp(-2.30e+00*user_Av))

    !C3H2 -> C3 + H2
    k(4206) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C3H2 -> C3H + H
    k(4207) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C4H -> C2H + C2
    k(4208) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C4H -> C4 + H
    k(4209) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C4N -> C3 + CN
    k(4210) = small + (5.00e-10*exp(-1.70e+00&
        *user_Av))

    !C4S -> C3 + CS
    k(4211) = small + (1.00e-10*exp(-2.00e+00&
        *user_Av))

    !C5 -> C3 + C2
    k(4212) = small + (1.00e-11*exp(-1.70e+00&
        *user_Av))

    !CH2O2 -> HCO + OH
    k(4213) = small + (2.75e-10&
        *exp(-1.80e+00*user_Av))

    !CH2O2 -> CH2O2+ + E
    k(4214) = small + (1.73e-10&
        *exp(-2.59e+00*user_Av))

    !CH3N -> HCN + H2
    k(4215) = small + (1.70e-09*exp(-1.63e+00&
        *user_Av))

    !CH4 -> CH + H + H2
    k(4216) = small + (1.60e-10&
        *exp(-2.20e+00*user_Av))

    !CH4 -> CH2 + H2
    k(4217) = small + (4.80e-10*exp(-2.20e+00&
        *user_Av))

    !CH4 -> CH3 + H
    k(4218) = small + (1.60e-10*exp(-2.20e+00&
        *user_Av))

    !HC3N -> C2H + CN
    k(4219) = small + (9.54e-10*exp(-1.83e+00&
        *user_Av))

    !C2H3N -> CH3 + CN
    k(4220) = small + (1.56e-09&
        *exp(-1.95e+00*user_Av))

    !C2H3N -> C2H3N+ + E
    k(4221) = small + (5.29e-10&
        *exp(-3.11e+00*user_Av))

    !C2H4 -> C2H2 + H2
    k(4222) = small + (1.62e-09&
        *exp(-1.61e+00*user_Av))

    !C2H4 -> C2H4+ + E
    k(4223) = small + (1.18e-10&
        *exp(-2.66e+00*user_Av))

    !C3H3 -> C3H + H2
    k(4224) = small + (5.00e-10*exp(-1.70e+00&
        *user_Av))

    !C3H3 -> C3H2 + H
    k(4225) = small + (5.00e-10*exp(-1.70e+00&
        *user_Av))

    !C4H2 -> C2H + C2H
    k(4226) = small + (1.13e-09&
        *exp(-1.64e+00*user_Av))

    !C4H2 -> C4H + H
    k(4227) = small + (1.13e-09*exp(-1.64e+00&
        *user_Av))

    !C4H2 -> C4H2+ + E
    k(4228) = small + (2.60e-10&
        *exp(-2.28e+00*user_Av))

    !C5H -> C2H + C3
    k(4229) = small + (1.00e-11*exp(-1.70e+00&
        *user_Av))

    !C5H -> C3H + C2
    k(4230) = small + (5.00e-12*exp(-1.70e+00&
        *user_Av))

    !C5H -> C5 + H
    k(4231) = small + (1.00e-11*exp(-1.70e+00&
        *user_Av))

    !C5N -> C4 + CN
    k(4232) = small + (5.00e-10*exp(-1.70e+00&
        *user_Av))

    !CH4O -> H2CO + H2
    k(4233) = small + (7.19e-10&
        *exp(-1.72e+00*user_Av))

    !CH4O -> CH4O+ + E
    k(4234) = small + (4.80e-10&
        *exp(-2.57e+00*user_Av))

    !C2H4O -> CH3 + HCO
    k(4235) = small + (3.43e-10&
        *exp(-1.52e+00*user_Av))

    !C2H4O -> CH4 + CO
    k(4236) = small + (3.43e-10&
        *exp(-1.52e+00*user_Av))

    !C2H4O -> C2H4O+ + E
    k(4237) = small + (2.60e-10&
        *exp(-2.28e+00*user_Av))

    !C3H4 -> C3H2 + H2
    k(4238) = small + (1.13e-10&
        *exp(-1.69e+00*user_Av))

    !C3H4 -> C3H3 + H
    k(4239) = small + (1.84e-09*exp(-1.72e+00&
        *user_Av))

    !C3H4 -> C3H4+ + E
    k(4240) = small + (9.88e-10&
        *exp(-2.37e+00*user_Av))

    !C4H4 -> C3H4 + C
    k(4241) = small + (1.13e-09*exp(-1.64e+00&
        *user_Av))

    !C5H2 -> C3H + C2H
    k(4242) = small + (5.00e-12&
        *exp(-1.70e+00*user_Av))

    !C5H2 -> C5H + H
    k(4243) = small + (1.00e-11*exp(-1.70e+00&
        *user_Av))

    !C6H -> C2H + C4
    k(4244) = small + (5.00e-12*exp(-1.70e+00&
        *user_Av))

    !C6H -> C3H + C3
    k(4245) = small + (2.50e-12*exp(-1.70e+00&
        *user_Av))

    !CH5N -> CN + H2 + H2 + H
    k(4246) = small + (9.42e-11&
        *exp(-1.76e+00*user_Av))

    !CH5N -> HCN + H2 + H + H
    k(4247) = small + (3.50e-10&
        *exp(-1.73e+00*user_Av))

    !CH5N -> CH3 + NH2
    k(4248) = small + (1.55e-10&
        *exp(-1.74e+00*user_Av))

    !CH5N -> CH3N + H + H
    k(4249) = small + (6.63e-11&
        *exp(-1.51e+00*user_Av))

    !CH5N -> CH5N+ + E
    k(4250) = small + (2.60e-10&
        *exp(-2.28e+00*user_Av))

    !HC5N -> H + C5N
    k(4251) = small + (5.00e-10*exp(-1.83e+00&
        *user_Av))

    !HC5N -> C4H + CN
    k(4252) = small + (5.00e-10*exp(-1.83e+00&
        *user_Av))

    !CH3C3N -> C3N + CH3
    k(4253) = small + (2.00e-11&
        *exp(-1.70e+00*user_Av))

    !C2H5OH -> C2H4 + H2O
    k(4254) = small + (1.38e-09&
        *exp(-1.73e+00*user_Av))

    !C2H5OH -> C2H5OH+ + E
    k(4255) = small + (5.27e-10&
        *exp(-2.35e+00*user_Av))

    !CH3C4H -> C4H + CH3
    k(4256) = small + (2.00e-11&
        *exp(-1.70e+00*user_Av))

    !CH3OCH3 -> H2CO + CH4
    k(4257) = small + (8.21e-10&
        *exp(-1.60e+00*user_Av))

    !CH3OCH3 -> CH3OCH3+ + E
    k(4258) = small + (2.60e-10&
        *exp(-2.28e+00*user_Av))

    !C2+ -> C+ + C
    k(4259) = small + (1.00e-11*exp(-1.70e+00&
        *user_Av))

    !CH+ -> C+ + H
    k(4260) = small + (4.60e-12*exp(-3.00e+00&
        *user_Av))

    !H2+ -> H+ + H
    k(4261) = small + (2.60e-10*exp(-1.80e+00&
        *user_Av))

    !OH+ -> H+ + O
    k(4262) = small + (7.20e-12*exp(-1.80e+00&
        *user_Av))

    !SIH+ -> SI+ + H
    k(4263) = small + (2.20e-09*exp(-2.00e+00&
        *user_Av))

    !C2H+ -> C2+ + H
    k(4264) = small + (1.00e-11*exp(-2.00e+00&
        *user_Av))

    !CH2+ -> CH+ + H
    k(4265) = small + (1.70e-09*exp(-1.70e+00&
        *user_Av))

    !H3+ -> H+ + H2
    k(4266) = small + (2.00e-08*exp(-1.80e+00&
        *user_Av))

    !H3+ -> H2+ + H
    k(4267) = small + (7.90e-09*exp(-2.30e+00&
        *user_Av))

    !CH3+ -> CH+ + H2
    k(4268) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !CH3+ -> CH2+ + H
    k(4269) = small + (1.00e-09*exp(-1.70e+00&
        *user_Av))

    !C- -> C + E
    k(4270) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !H- -> H + E
    k(4271) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !O- -> O + E
    k(4272) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !S- -> S + E
    k(4273) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !CN- -> CN + E
    k(4274) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !OH- -> OH + E
    k(4275) = small + (2.40e-07*exp(-9.00e-01&
        *user_Av))

    !E + GRAIN0 -> GRAIN-
    k(4276) = small + (6.90E-15&
        *(T32)**(5.00E-01)*fact)

    !C+ + GRAIN- -> C + GRAIN0
    k(4277) = small + (4.90E-17&
        *(T32)**(5.00E-01)*fact)

    !FE+ + GRAIN- -> FE + GRAIN0
    k(4278) = small + (2.30E-17&
        *(T32)**(5.00E-01)*fact)

    !H+ + GRAIN- -> H + GRAIN0
    k(4279) = small + (1.70E-16&
        *(T32)**(5.00E-01)*fact)

    !HE+ + GRAIN- -> HE + GRAIN0
    k(4280) = small + (8.50E-17&
        *(T32)**(5.00E-01)*fact)

    !MG+ + GRAIN- -> MG + GRAIN0
    k(4281) = small + (3.70E-17&
        *(T32)**(5.00E-01)*fact)

    !N+ + GRAIN- -> N + GRAIN0
    k(4282) = small + (4.70E-17&
        *(T32)**(5.00E-01)*fact)

    !NA+ + GRAIN- -> NA + GRAIN0
    k(4283) = small + (3.60E-17&
        *(T32)**(5.00E-01)*fact)

    !O+ + GRAIN- -> O + GRAIN0
    k(4284) = small + (4.40E-17&
        *(T32)**(5.00E-01)*fact)

    !S+ + GRAIN- -> S + GRAIN0
    k(4285) = small + (3.00E-17&
        *(T32)**(5.00E-01)*fact)

    !SI+ + GRAIN- -> SI + GRAIN0
    k(4286) = small + (3.30E-17&
        *(T32)**(5.00E-01)*fact)

    !H3+ + GRAIN- -> H2 + H + GRAIN0
    k(4287) = small + (1.00E-16&
        *(T32)**(5.00E-01)*fact)

    !HCO+ + GRAIN- -> H + CO + GRAIN0
    k(4288) = small + (3.10E-17&
        *(T32)**(5.00E-01)*fact)

    !O3 -> O2 + O
    k(4289) = small + (1.50E+03*user_crflux)

    !FEH -> FE + H
    k(4290) = small + (1.00E+03*user_crflux)

    !HNCO -> NH + CO
    k(4291) = small + (6.00E+03*user_crflux)

    !H+ + HNCO -> NH2+ + CO
    k(4292) = small + (1.48E-08&
        *(T32)**(-5.00E-01))

    !HE+ + HNCO -> H+ + OCN + HE
    k(4293) = small + (7.66E-09&
        *(T32)**(-5.00E-01))

    !HC2O -> CO + CH
    k(4294) = small + (1.50E+03*user_crflux)

    !HCCN -> C2N + H
    k(4295) = small + (1.00E+04*user_crflux)

    !HC3O -> CO + C2H
    k(4296) = small + (1.50E+03*user_crflux)

    !MGH2 -> MGH + H
    k(4297) = small + (3.00E+03*user_crflux)

    !N2H2 -> NH2 + N
    k(4298) = small + (2.00E+02*user_crflux)

    !H+ + N2H2 -> N2H+ + H2
    k(4299) = small + (2.00E-09)

    !H3+ + N2H2 -> N2H+ + H2 + H2
    k(4300) = small + (2.00E-09)

    !HE+ + N2H2 -> N2+ + H + H + HE
    k(4301) = small + (2.00E-09)

    !CHNH -> CH + NH
    k(4302) = small + (1.00E+03*user_crflux)

    !H2C3O -> C2H2 + CO
    k(4303) = small + (1.80E+03&
        *user_crflux)

    !H2C3N -> C2H2 + CN
    k(4304) = small + (3.00E+03&
        *user_crflux)

    !H2C5N -> C4H2 + CN
    k(4305) = small + (3.00E+03&
        *user_crflux)

    !H2C7N -> C6H2 + CN
    k(4306) = small + (3.00E+03&
        *user_crflux)

    !H2C9N -> C8H2 + CN
    k(4307) = small + (3.00E+03&
        *user_crflux)

    !NH2OH -> NH2 + OH
    k(4308) = small + (3.00E+03*user_crflux)

    !CH2OH -> CH2 + OH
    k(4309) = small + (3.00E+03*user_crflux)

    !C+ + CH2OH -> CH2+ + HCO
    k(4310) = small + (7.50E-10&
        *(T32)**(-5.00E-01))

    !C+ + CH2OH -> H2CO + CH+
    k(4311) = small + (7.50E-10&
        *(T32)**(-5.00E-01))

    !C+ + CH2OH -> H2CO+ + CH
    k(4312) = small + (7.50E-10&
        *(T32)**(-5.00E-01))

    !H3+ + CH2OH -> CH4O+ + H2
    k(4313) = small + (4.00E-09&
        *(T32)**(-5.00E-01))

    !HCO+ + CH2OH -> CH4O+ + CO
    k(4314) = small + (1.70E-09&
        *(T32)**(-5.00E-01))

    !HE+ + CH2OH -> CH2+ + OH + HE
    k(4315) = small + (1.70E-09&
        *(T32)**(-5.00E-01))

    !HE+ + CH2OH -> CH2 + OH+ + HE
    k(4316) = small + (1.70E-09&
        *(T32)**(-5.00E-01))

    !HCOOCH3 -> HCO + CH2OH
    k(4317) = small + (1.50E+03&
        *user_crflux)

    !HCOOCH3 -> HCO + CH2OH
    k(4318) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C5H3 -> C5H2 + H
    k(4319) = small + (1.00E+04*user_crflux)

    !H3C5N -> C2H3 + C3N
    k(4320) = small + (3.00E+03&
        *user_crflux)

    !C6H3 -> C6H2 + H
    k(4321) = small + (1.00E+04*user_crflux)

    !C7H3 -> C7H2 + H
    k(4322) = small + (1.00E+04*user_crflux)

    !H3C7N -> C2H3 + C5N
    k(4323) = small + (3.00E+03&
        *user_crflux)

    !C8H3 -> C8H2 + H
    k(4324) = small + (1.00E+04*user_crflux)

    !C9H3 -> C9H2 + H
    k(4325) = small + (1.00E+04*user_crflux)

    !H3C9N -> C2H3 + C7N
    k(4326) = small + (3.00E+03&
        *user_crflux)

    !CH3NH -> CH3N + H
    k(4327) = small + (9.50E+03*user_crflux)

    !H4C3N -> C2H4 + CN
    k(4328) = small + (3.00E+03&
        *user_crflux)

    !C5H4 -> C5H2 + H2
    k(4329) = small + (7.50E+03*user_crflux)

    !C+ + C5H4 -> C5H4+ + C
    k(4330) = small + (4.74E-10&
        *(T32)**(-5.00E-01))

    !C+ + C5H4 -> C6H3+ + H
    k(4331) = small + (4.74E-10&
        *(T32)**(-5.00E-01))

    !H+ + C5H4 -> C5H3+ + H2
    k(4332) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H+ + C5H4 -> C5H4+ + H
    k(4333) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H3+ + C5H4 -> C5H3+ + H2 + H2
    k(4334) = small + (8.91E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C5H4 -> C5H5+ + H2
    k(4335) = small + (8.91E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C5H4 -> C5H2+ + H2 + HE
    k(4336) = small + (5.18E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C5H4 -> C5H3+ + H + HE
    k(4337) = small + (5.18E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C5H4 -> C3H2+ + C2H2 + HE
    k(4338) = small + (5.18E-10&
        *(T32)**(-5.00E-01))

    !C6H4 -> C6H2 + H2
    k(4339) = small + (7.50E+03*user_crflux)

    !C+ + C6H4 -> C6H4+ + C
    k(4340) = small + (2.40E-10&
        *(T32)**(-5.00E-01))

    !C+ + C6H4 -> C7H3+ + H
    k(4341) = small + (2.40E-10&
        *(T32)**(-5.00E-01))

    !H+ + C6H4 -> C6H4+ + H
    k(4342) = small + (7.78E-10&
        *(T32)**(-5.00E-01))

    !H+ + C6H4 -> C6H3+ + H2
    k(4343) = small + (7.78E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C6H4 -> C6H3+ + H2 + H2
    k(4344) = small + (4.55E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C6H4 -> C6H5+ + H2
    k(4345) = small + (4.55E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C6H4 -> C6H2+ + H2 + HE
    k(4346) = small + (2.65E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C6H4 -> C6H3+ + H + HE
    k(4347) = small + (2.65E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C6H4 -> C4H2+ + C2H2 + HE
    k(4348) = small + (2.65E-10&
        *(T32)**(-5.00E-01))

    !C6H6 -> C6H4 + H2
    k(4349) = small + (3.00E+03*user_crflux)

    !C7H4 -> C7H2 + H2
    k(4350) = small + (7.50E+03*user_crflux)

    !C+ + C7H4 -> C7H4+ + C
    k(4351) = small + (4.65E-10&
        *(T32)**(-5.00E-01))

    !C+ + C7H4 -> C8H3+ + H
    k(4352) = small + (4.65E-10&
        *(T32)**(-5.00E-01))

    !H+ + C7H4 -> C7H4+ + H
    k(4353) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H+ + C7H4 -> C7H3+ + H2
    k(4354) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H3+ + C7H4 -> C7H5+ + H2
    k(4355) = small + (8.88E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C7H4 -> C7H3+ + H2 + H2
    k(4356) = small + (8.88E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C7H4 -> C7H3+ + H + HE
    k(4357) = small + (5.15E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C7H4 -> C5H2+ + C2H2 + HE
    k(4358) = small + (5.15E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C7H4 -> C7H2+ + H2 + HE
    k(4359) = small + (5.15E-10&
        *(T32)**(-5.00E-01))

    !C8H4 -> C8H2 + H2
    k(4360) = small + (7.50E+03*user_crflux)

    !C+ + C8H4 -> C8H4+ + C
    k(4361) = small + (2.36E-10&
        *(T32)**(-5.00E-01))

    !C+ + C8H4 -> C9H3+ + H
    k(4362) = small + (2.36E-10&
        *(T32)**(-5.00E-01))

    !H+ + C8H4 -> C8H4+ + H
    k(4363) = small + (7.79E-10&
        *(T32)**(-5.00E-01))

    !H+ + C8H4 -> C8H3+ + H2
    k(4364) = small + (7.79E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C8H4 -> C8H5+ + H2
    k(4365) = small + (4.53E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C8H4 -> C8H3+ + H2 + H2
    k(4366) = small + (4.53E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C8H4 -> C8H3+ + H + HE
    k(4367) = small + (2.63E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C8H4 -> C6H2+ + C2H2 + HE
    k(4368) = small + (2.63E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C8H4 -> C8H2+ + H2 + HE
    k(4369) = small + (2.63E-10&
        *(T32)**(-5.00E-01))

    !C9H4 -> C9H2 + H2
    k(4370) = small + (7.50E+03*user_crflux)

    !C+ + C9H4 -> C9H4+ + C
    k(4371) = small + (9.18E-10&
        *(T32)**(-5.00E-01))

    !H+ + C9H4 -> C9H4+ + H
    k(4372) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H+ + C9H4 -> C9H3+ + H2
    k(4373) = small + (1.52E-09&
        *(T32)**(-5.00E-01))

    !H3+ + C9H4 -> C9H5+ + H2
    k(4374) = small + (8.83E-10&
        *(T32)**(-5.00E-01))

    !H3+ + C9H4 -> C9H3+ + H2 + H2
    k(4375) = small + (8.83E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C9H4 -> C9H3+ + H + HE
    k(4376) = small + (5.12E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C9H4 -> C7H2+ + C2H2 + HE
    k(4377) = small + (5.12E-10&
        *(T32)**(-5.00E-01))

    !HE+ + C9H4 -> C9H2+ + H2 + HE
    k(4378) = small + (5.12E-10&
        *(T32)**(-5.00E-01))

    !H5C3N -> C2H5 + CN
    k(4379) = small + (3.00E+03&
        *user_crflux)

    !H+ + H5C3N -> C3H4N+ + H2
    k(4380) = small + (1.57E-08&
        *(T32)**(-5.00E-01))

    !H3+ + H5C3N -> C3H4N+ + H2 + H2
    k(4381) = small + (9.22E-09&
        *(T32)**(-5.00E-01))

    !HE+ + H5C3N -> CH3+ + CH2 + CN + HE
    k(4382) = small + (8.05E-09&
        *(T32)**(-5.00E-01))

    !C2H6 -> C2H4 + H2
    k(4383) = small + (3.00E+03*user_crflux)

    !C+ + C2H6 -> C2H2+ + CH4
    k(4384) = small + (1.70E-10)

    !C+ + C2H6 -> C2H3+ + CH3
    k(4385) = small + (5.10E-10)

    !C+ + C2H6 -> C2H5+ + CH
    k(4386) = small + (1.70E-10)

    !C+ + C2H6 -> C3H3+ + H2 + H
    k(4387) = small + (8.50E-10)

    !H+ + C2H6 -> C2H3+ + H2 + H2
    k(4388) = small + (1.30E-09)

    !H+ + C2H6 -> C2H4+ + H2 + H
    k(4389) = small + (1.30E-09)

    !H+ + C2H6 -> C2H5+ + H2
    k(4390) = small + (1.30E-09)

    !H3+ + C2H6 -> C2H5+ + H2 + H2
    k(4391) = small + (3.37E-09)

    !HE+ + C2H6 -> C2H2+ + H2 + H2 + HE
    k(4392) = small + (8.40E-10)

    !HE+ + C2H6 -> C2H4+ + H2 + HE
    k(4393) = small + (4.20E-10)

    !HE+ + C2H6 -> C2H3+ + H2 + H + HE
    k(4394) = small + (1.74E-09)

    !CN + C2H6 -> C2H5 + HCN
    k(4395) = small + (2.40E-11)

    !OH + C2H5 -> C2H6 + O
    k(4396) = small + (1.04E-18&
        *(T32)**(8.80E+00)*exp(-2.50E+02*invT))

    !H2COHOCH2+ + E -> CH4O + HCO
    k(4397) = small + (1.49E-07&
        *(T32)**(-5.00E-01))

    !H2COHOCH2+ + E -> HCOOCH3 + H
    k(4398) = small + (1.50E-09&
        *(T32)**(-5.00E-01))

    !H3CO+ + H2CO -> H2COHOCH2+
    k(4399) = small + (8.10E-15&
        *(T32)**(-3.00E+00))

    !H7C2O2+ + E -> CH4O + H2CO + H
    k(4400) = small + (1.49E-07&
        *(T32)**(-5.00E-01))

    !H7C2O2+ + E -> HCOOCH3 + H2 + H
    k(4401) = small + (1.50E-09&
        *(T32)**(-5.00E-01))

    !CH5O+ + H2CO -> H7C2O2+
    k(4402) = small + (3.10E-15&
        *(T32)**(-3.00E+00))

    !C_DUST + C_DUST -> C2_DUST
    k(4403) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_C,idx_C,n,user_Tdust))

    !C_DUST + C2_DUST -> C3_DUST
    k(4404) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C2,n,user_Tdust))

    !C_DUST + C2H_DUST -> C3H_DUST
    k(4405) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C2H,n,user_Tdust))

    !C_DUST + C2H3_DUST -> C3H3_DUST
    k(4406) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C2H3,n,user_Tdust))

    !C_DUST + C2N_DUST -> C3N_DUST
    k(4407) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C2N,n,user_Tdust))

    !C_DUST + CCO_DUST -> C3O_DUST
    k(4408) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_CCO,n,user_Tdust))

    !C_DUST + C2S_DUST -> C3S_DUST
    k(4409) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C2S,n,user_Tdust))

    !C_DUST + C3_DUST -> C4_DUST
    k(4410) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C3,n,user_Tdust))

    !C_DUST + C3H_DUST -> C4H_DUST
    k(4411) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C3H,n,user_Tdust))

    !C_DUST + C4_DUST -> C5_DUST
    k(4412) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C4,n,user_Tdust))

    !C_DUST + C4H_DUST -> C5H_DUST
    k(4413) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C4H,n,user_Tdust))

    !C_DUST + C5_DUST -> C6_DUST
    k(4414) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C5,n,user_Tdust))

    !C_DUST + C5H_DUST -> C6H_DUST
    k(4415) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C5H,n,user_Tdust))

    !C_DUST + C6_DUST -> C7_DUST
    k(4416) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C6,n,user_Tdust))

    !C_DUST + C6H_DUST -> C7H_DUST
    k(4417) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C6H,n,user_Tdust))

    !C_DUST + C7_DUST -> C8_DUST
    k(4418) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C7,n,user_Tdust))

    !C_DUST + C7H_DUST -> C8H_DUST
    k(4419) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C7H,n,user_Tdust))

    !C_DUST + C8_DUST -> C9_DUST
    k(4420) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C8,n,user_Tdust))

    !C_DUST + C8H_DUST -> C9H_DUST
    k(4421) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C8H,n,user_Tdust))

    !C_DUST + C9_DUST -> C10_DUST
    k(4422) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_C9,n,user_Tdust))

    !C_DUST + CH_DUST -> C2H_DUST
    k(4423) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_CH,n,user_Tdust))

    !C_DUST + CH2_DUST -> C2H2_DUST
    k(4424) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_CH2,n,user_Tdust))

    !C_DUST + CH3_DUST -> C2H3_DUST
    k(4425) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_CH3,n,user_Tdust))

    !C_DUST + CN_DUST -> C2N_DUST
    k(4426) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_CN,n,user_Tdust))

    !C_DUST + HS_DUST -> CS_DUST + H_DUST
    k(4427) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_HS,n,user_Tdust))

    !C_DUST + N_DUST -> CN_DUST
    k(4428) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_N,n,user_Tdust))

    !C_DUST + NH_DUST -> HNC_DUST
    k(4429) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_NH,n,user_Tdust))

    !C_DUST + NH2_DUST -> HNC_DUST + H_DUST
    k(4430) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_NH2,n,user_Tdust))

    !C_DUST + NO_DUST -> CN_DUST + O_DUST
    k(4431) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_C,idx_NO,n,user_Tdust))

    !C_DUST + NO_DUST -> OCN_DUST
    k(4432) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_C,idx_NO,n,user_Tdust))

    !C_DUST + NS_DUST -> CN_DUST + S_DUST
    k(4433) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_NS,n,user_Tdust))

    !C_DUST + O_DUST -> CO_DUST
    k(4434) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_O,n,user_Tdust))

    !C_DUST + O2_DUST -> CO_DUST + O_DUST
    k(4435) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_O2,n,user_Tdust))

    !C_DUST + OCN_DUST -> CO_DUST + CN_DUST
    k(4436) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_OCN,n,user_Tdust))

    !C_DUST + OH_DUST -> CO_DUST + H_DUST
    k(4437) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_OH,n,user_Tdust))

    !C_DUST + S_DUST -> CS_DUST
    k(4438) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_S,n,user_Tdust))

    !C_DUST + SO_DUST -> CO_DUST + S_DUST
    k(4439) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_C,idx_SO,n,user_Tdust))

    !CH_DUST + C2_DUST -> C3H_DUST
    k(4440) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C2,n,user_Tdust))

    !CH_DUST + C2H_DUST -> C3H2_DUST
    k(4441) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C2H,n,user_Tdust))

    !CH_DUST + C2H3_DUST -> C3H4_DUST
    k(4442) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C2H3,n,user_Tdust))

    !CH_DUST + C3_DUST -> C4H_DUST
    k(4443) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C3,n,user_Tdust))

    !CH_DUST + C3H_DUST -> C4H2_DUST
    k(4444) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C3H,n,user_Tdust))

    !CH_DUST + C4_DUST -> C5H_DUST
    k(4445) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C4,n,user_Tdust))

    !CH_DUST + C4H_DUST -> C5H2_DUST
    k(4446) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C4H,n,user_Tdust))

    !CH_DUST + C5_DUST -> C6H_DUST
    k(4447) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C5,n,user_Tdust))

    !CH_DUST + C5H_DUST -> C6H2_DUST
    k(4448) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C5H,n,user_Tdust))

    !CH_DUST + C6_DUST -> C7H_DUST
    k(4449) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C6,n,user_Tdust))

    !CH_DUST + C6H_DUST -> C7H2_DUST
    k(4450) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C6H,n,user_Tdust))

    !CH_DUST + C7_DUST -> C8H_DUST
    k(4451) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C7,n,user_Tdust))

    !CH_DUST + C7H_DUST -> C8H2_DUST
    k(4452) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C7H,n,user_Tdust))

    !CH_DUST + C8_DUST -> C9H_DUST
    k(4453) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C8,n,user_Tdust))

    !CH_DUST + C8H_DUST -> C9H2_DUST
    k(4454) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_C8H,n,user_Tdust))

    !CH_DUST + CH_DUST -> C2H2_DUST
    k(4455) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_CH,idx_CH,n,user_Tdust))

    !CH_DUST + CH2_DUST -> C2H3_DUST
    k(4456) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_CH2,n,user_Tdust))

    !CH_DUST + CH3_DUST -> C2H4_DUST
    k(4457) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_CH3,n,user_Tdust))

    !CH_DUST + CN_DUST -> HCCN_DUST
    k(4458) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_CN,n,user_Tdust))

    !CH_DUST + HNO_DUST -> NO_DUST + CH2_DUST
    k(4459) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_HNO,n,user_Tdust))

    !CH_DUST + NH_DUST -> HCN_DUST + H_DUST
    k(4460) = small + (krate_2bodySi(3.33d-01,0.00d+00,idx_CH,idx_NH,n,user_Tdust))

    !CH_DUST + NH_DUST -> HNC_DUST + H_DUST
    k(4461) = small + (krate_2bodySi(3.33d-01,0.00d+00,idx_CH,idx_NH,n,user_Tdust))

    !CH_DUST + NH_DUST -> CHNH_DUST
    k(4462) = small + (krate_2bodySi(3.33d-01,0.00d+00,idx_CH,idx_NH,n,user_Tdust))

    !CH_DUST + NH2_DUST -> CH3N_DUST
    k(4463) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_NH2,n,user_Tdust))

    !CH_DUST + NO_DUST -> HCN_DUST + O_DUST
    k(4464) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_NO,n,user_Tdust))

    !CH_DUST + O2_DUST -> HCO_DUST + O_DUST
    k(4465) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH,idx_O2,n,user_Tdust))

    !CH2_DUST + CH2_DUST -> C2H4_DUST
    k(4466) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_CH2,idx_CH2,n,user_Tdust))

    !CH2_DUST + CH3_DUST -> C2H5_DUST
    k(4467) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH2,idx_CH3,n,user_Tdust))

    !CH2_DUST + CN_DUST -> C2H2N_DUST
    k(4468) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH2,idx_CN,n,user_Tdust))

    !CH2_DUST + HNO_DUST -> CH3_DUST + NO_DUST
    k(4469) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH2,idx_HNO,n,user_Tdust))

    !CH2_DUST + NH2_DUST -> CH2NH2_DUST
    k(4470) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH2,idx_NH2,n,user_Tdust))

    !CH2_DUST + O2_DUST -> H2CO_DUST + O_DUST
    k(4471) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH2,idx_O2,n,user_Tdust))

    !CH3_DUST + C3N_DUST -> CH3C3N_DUST
    k(4472) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_C3N,n,user_Tdust))

    !CH3_DUST + C5N_DUST -> CH3C5N_DUST
    k(4473) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_C5N,n,user_Tdust))

    !CH3_DUST + C7N_DUST -> CH3C7N_DUST
    k(4474) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_C7N,n,user_Tdust))

    !CH3_DUST + CH2OH_DUST -> C2H5OH_DUST
    k(4475) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_CH3,idx_CH2OH,n,user_Tdust))

    !CH3_DUST + CH2OH_DUST -> CH3OCH3_DUST
    k(4476) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_CH3,idx_CH2OH,n,user_Tdust))

    !CH3_DUST + CH3_DUST -> C2H6_DUST
    k(4477) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_CH3,idx_CH3,n,user_Tdust))

    !CH3_DUST + CN_DUST -> C2H3N_DUST
    k(4478) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_CN,n,user_Tdust))

    !CH3_DUST + HCO_DUST -> C2H4O_DUST
    k(4479) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_HCO,n,user_Tdust))

    !CH3_DUST + HNO_DUST -> CH4_DUST + NO_DUST
    k(4480) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_CH3,idx_HNO,n,user_Tdust))

    !CH4_DUST + C2H_DUST -> C2H2_DUST + CH3_DUST
    k(4481) = small + (krate_2bodySi(1.00d+00,2.50d+02,idx_CH4,idx_C2H,n,user_Tdust))

    !H_DUST + C_DUST -> CH_DUST
    k(4482) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C,n,user_Tdust))

    !H_DUST + C2_DUST -> C2H_DUST
    k(4483) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2,n,user_Tdust))

    !H_DUST + C2H_DUST -> C2H2_DUST
    k(4484) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2H,n,user_Tdust))

    !H_DUST + C2H2_DUST -> C2H3_DUST
    k(4485) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C2H2,n,user_Tdust))

    !H_DUST + C2H3_DUST -> C2H4_DUST
    k(4486) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2H3,n,user_Tdust))

    !H_DUST + C2H4_DUST -> C2H5_DUST
    k(4487) = small + (krate_2bodySi(1.00d+00,7.50d+02,idx_H,idx_C2H4,n,user_Tdust))

    !H_DUST + C2H5_DUST -> C2H6_DUST
    k(4488) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2H5,n,user_Tdust))

    !H_DUST + C2H6_DUST -> C2H5_DUST + H2_DUST
    k(4489) = small + (krate_2bodySi(1.00d+00,4.89d+03,idx_H,idx_C2H6,n,user_Tdust))

    !H_DUST + C2N_DUST -> HCCN_DUST
    k(4490) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2N,n,user_Tdust))

    !H_DUST + CCO_DUST -> HC2O_DUST
    k(4491) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CCO,n,user_Tdust))

    !H_DUST + C3_DUST -> C3H_DUST
    k(4492) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C3,n,user_Tdust))

    !H_DUST + C3H_DUST -> C3H2_DUST
    k(4493) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C3H,n,user_Tdust))

    !H_DUST + C3H2_DUST -> C3H3_DUST
    k(4494) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C3H2,n,user_Tdust))

    !H_DUST + C3H3_DUST -> C3H4_DUST
    k(4495) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C3H3,n,user_Tdust))

    !H_DUST + C3H3N_DUST -> H4C3N_DUST
    k(4496) = small + (krate_2bodySi(1.00d+00,7.50d+02,idx_H,idx_C3H3N,n,user_Tdust))

    !H_DUST + C3N_DUST -> HC3N_DUST
    k(4497) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C3N,n,user_Tdust))

    !H_DUST + C3O_DUST -> HC3O_DUST
    k(4498) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C3O,n,user_Tdust))

    !H_DUST + C4_DUST -> C4H_DUST
    k(4499) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C4,n,user_Tdust))

    !H_DUST + C4H_DUST -> C4H2_DUST
    k(4500) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C4H,n,user_Tdust))

    !H_DUST + C4H2_DUST -> C4H3_DUST
    k(4501) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C4H2,n,user_Tdust))

    !H_DUST + C4H3_DUST -> C4H4_DUST
    k(4502) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C4H3,n,user_Tdust))

    !H_DUST + C5_DUST -> C5H_DUST
    k(4503) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C5,n,user_Tdust))

    !H_DUST + C5H_DUST -> C5H2_DUST
    k(4504) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C5H,n,user_Tdust))

    !H_DUST + C5H2_DUST -> C5H3_DUST
    k(4505) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C5H2,n,user_Tdust))

    !H_DUST + C5H3_DUST -> C5H4_DUST
    k(4506) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C5H3,n,user_Tdust))

    !H_DUST + C5N_DUST -> HC5N_DUST
    k(4507) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C5N,n,user_Tdust))

    !H_DUST + C6_DUST -> C6H_DUST
    k(4508) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C6,n,user_Tdust))

    !H_DUST + C6H_DUST -> C6H2_DUST
    k(4509) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C6H,n,user_Tdust))

    !H_DUST + C6H2_DUST -> C6H3_DUST
    k(4510) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C6H2,n,user_Tdust))

    !H_DUST + C6H3_DUST -> C6H4_DUST
    k(4511) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C6H3,n,user_Tdust))

    !H_DUST + C7_DUST -> C7H_DUST
    k(4512) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C7,n,user_Tdust))

    !H_DUST + C7H_DUST -> C7H2_DUST
    k(4513) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C7H,n,user_Tdust))

    !H_DUST + C7H2_DUST -> C7H3_DUST
    k(4514) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C7H2,n,user_Tdust))

    !H_DUST + C7H3_DUST -> C7H4_DUST
    k(4515) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C7H3,n,user_Tdust))

    !H_DUST + C7N_DUST -> HC7N_DUST
    k(4516) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C7N,n,user_Tdust))

    !H_DUST + C8_DUST -> C8H_DUST
    k(4517) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C8,n,user_Tdust))

    !H_DUST + C8H_DUST -> C8H2_DUST
    k(4518) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C8H,n,user_Tdust))

    !H_DUST + C8H2_DUST -> C8H3_DUST
    k(4519) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C8H2,n,user_Tdust))

    !H_DUST + C8H3_DUST -> C8H4_DUST
    k(4520) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C8H3,n,user_Tdust))

    !H_DUST + C9_DUST -> C9H_DUST
    k(4521) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C9,n,user_Tdust))

    !H_DUST + C9H_DUST -> C9H2_DUST
    k(4522) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C9H,n,user_Tdust))

    !H_DUST + C9H2_DUST -> C9H3_DUST
    k(4523) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_C9H2,n,user_Tdust))

    !H_DUST + C9H3_DUST -> C9H4_DUST
    k(4524) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C9H3,n,user_Tdust))

    !H_DUST + C9N_DUST -> HC9N_DUST
    k(4525) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C9N,n,user_Tdust))

    !H_DUST + CH_DUST -> CH2_DUST
    k(4526) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH,n,user_Tdust))

    !H_DUST + CH2_DUST -> CH3_DUST
    k(4527) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH2,n,user_Tdust))

    !H_DUST + C2H2N_DUST -> C2H3N_DUST
    k(4528) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_C2H2N,n,user_Tdust))

    !H_DUST + CH3N_DUST -> CH2NH2_DUST
    k(4529) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_H,idx_CH3N,n,user_Tdust))

    !H_DUST + CH3N_DUST -> CH3NH_DUST
    k(4530) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_H,idx_CH3N,n,user_Tdust))

    !H_DUST + CH2NH2_DUST -> CH5N_DUST
    k(4531) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH2NH2,n,user_Tdust))

    !H_DUST + CH2OH_DUST -> CH4O_DUST
    k(4532) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH2OH,n,user_Tdust))

    !H_DUST + CH3_DUST -> CH4_DUST
    k(4533) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH3,n,user_Tdust))

    !H_DUST + CH3NH_DUST -> CH5N_DUST
    k(4534) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CH3NH,n,user_Tdust))

    !H_DUST + CH4_DUST -> CH3_DUST + H2_DUST
    k(4535) = small + (krate_2bodySi(1.00d+00,5.94d+03,idx_H,idx_CH4,n,user_Tdust))

    !H_DUST + CHNH_DUST -> CH3N_DUST
    k(4536) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CHNH,n,user_Tdust))

    !H_DUST + CN_DUST -> HCN_DUST
    k(4537) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_CN,n,user_Tdust))

    !H_DUST + CO_DUST -> HCO_DUST
    k(4538) = small + (krate_2bodySi(1.00d+00,2.50d+03,idx_H,idx_CO,n,user_Tdust))

    !H_DUST + CS_DUST -> HCS_DUST
    k(4539) = small + (krate_2bodySi(1.00d+00,1.00d+03,idx_H,idx_CS,n,user_Tdust))

    !H_DUST + FE_DUST -> FEH_DUST
    k(4540) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_FE,n,user_Tdust))

    !H_DUST + H_DUST -> H2_DUST
    k(4541) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_H,idx_H,n,user_Tdust))

    !H_DUST + H2C3N_DUST -> C3H3N_DUST
    k(4542) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H2C3N,n,user_Tdust))

    !H_DUST + H2C5N_DUST -> H3C5N_DUST
    k(4543) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H2C5N,n,user_Tdust))

    !H_DUST + H2C7N_DUST -> H3C7N_DUST
    k(4544) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H2C7N,n,user_Tdust))

    !H_DUST + H2C9N_DUST -> H3C9N_DUST
    k(4545) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H2C9N,n,user_Tdust))

    !H_DUST + H2CN_DUST -> CH3N_DUST
    k(4546) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H2CN,n,user_Tdust))

    !H_DUST + H2CO_DUST -> HCO_DUST + H2_DUST
    k(4547) = small + (krate_2bodySi(0.00d+00,0.00d+00,idx_H,idx_H2CO,n,user_Tdust))

    !H_DUST + H2CO_DUST -> CH2OH_DUST
    k(4548) = small + (krate_2bodySi(1.00d+00,2.50d+03,idx_H,idx_H2CO,n,user_Tdust))

    !H_DUST + H2O2_DUST -> H2O_DUST + OH_DUST
    k(4549) = small + (krate_2bodySi(5.00d-01,1.40d+03,idx_H,idx_H2O2,n,user_Tdust))

    !H_DUST + H2O2_DUST -> O2H_DUST + H2_DUST
    k(4550) = small + (krate_2bodySi(5.00d-01,1.90d+03,idx_H,idx_H2O2,n,user_Tdust))

    !H_DUST + H2S_DUST -> H2_DUST + HS_DUST
    k(4551) = small + (krate_2bodySi(1.00d+00,8.60d+02,idx_H,idx_H2S,n,user_Tdust))

    !H_DUST + H4C3N_DUST -> H5C3N_DUST
    k(4552) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_H4C3N,n,user_Tdust))

    !H_DUST + HC2O_DUST -> C2H2O_DUST
    k(4553) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HC2O,n,user_Tdust))

    !H_DUST + HC3N_DUST -> H2C3N_DUST
    k(4554) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_HC3N,n,user_Tdust))

    !H_DUST + HC3O_DUST -> H2C3O_DUST
    k(4555) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HC3O,n,user_Tdust))

    !H_DUST + HC5N_DUST -> H2C5N_DUST
    k(4556) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_HC5N,n,user_Tdust))

    !H_DUST + HC7N_DUST -> H2C7N_DUST
    k(4557) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_HC7N,n,user_Tdust))

    !H_DUST + HC9N_DUST -> H2C9N_DUST
    k(4558) = small + (krate_2bodySi(1.00d+00,1.21d+03,idx_H,idx_HC9N,n,user_Tdust))

    !H_DUST + HCCN_DUST -> C2H2N_DUST
    k(4559) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HCCN,n,user_Tdust))

    !H_DUST + HCO_DUST -> H2CO_DUST
    k(4560) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HCO,n,user_Tdust))

    !H_DUST + HCS_DUST -> H2CS_DUST
    k(4561) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HCS,n,user_Tdust))

    !H_DUST + HNO_DUST -> NO_DUST + H2_DUST
    k(4562) = small + (krate_2bodySi(1.00d+00,1.50d+03,idx_H,idx_HNO,n,user_Tdust))

    !H_DUST + HS_DUST -> H2S_DUST
    k(4563) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_HS,n,user_Tdust))

    !H_DUST + MG_DUST -> MGH_DUST
    k(4564) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_MG,n,user_Tdust))

    !H_DUST + MGH_DUST -> MGH2_DUST
    k(4565) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_MGH,n,user_Tdust))

    !H_DUST + N_DUST -> NH_DUST
    k(4566) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_N,n,user_Tdust))

    !H_DUST + N2H2_DUST -> H2_DUST + N2_DUST + H_DUST
    k(4567) = small + (krate_2bodySi(1.00d+00,6.50d+02,idx_H,idx_N2H2,n,user_Tdust))

    !H_DUST + NA_DUST -> NAH_DUST
    k(4568) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_NA,n,user_Tdust))

    !H_DUST + NH_DUST -> NH2_DUST
    k(4569) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_NH,n,user_Tdust))

    !H_DUST + NH2_DUST -> NH3_DUST
    k(4570) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_NH2,n,user_Tdust))

    !H_DUST + NO_DUST -> HNO_DUST
    k(4571) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_NO,n,user_Tdust))

    !H_DUST + O_DUST -> OH_DUST
    k(4572) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_O,n,user_Tdust))

    !H_DUST + O2_DUST -> O2H_DUST
    k(4573) = small + (krate_2bodySi(1.00d+00,1.20d+03,idx_H,idx_O2,n,user_Tdust))

    !H_DUST + O2H_DUST -> H2O2_DUST
    k(4574) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_O2H,n,user_Tdust))

    !H_DUST + O3_DUST -> O2_DUST + OH_DUST
    k(4575) = small + (krate_2bodySi(1.00d+00,4.50d+02,idx_H,idx_O3,n,user_Tdust))

    !H_DUST + OCN_DUST -> HNCO_DUST
    k(4576) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_OCN,n,user_Tdust))

    !H_DUST + OCS_DUST -> CO_DUST + HS_DUST
    k(4577) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_OCS,n,user_Tdust))

    !H_DUST + OH_DUST -> H2O_DUST
    k(4578) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_OH,n,user_Tdust))

    !H_DUST + S_DUST -> HS_DUST
    k(4579) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_S,n,user_Tdust))

    !H_DUST + SI_DUST -> SIH_DUST
    k(4580) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_SI,n,user_Tdust))

    !H_DUST + SIH_DUST -> SIH2_DUST
    k(4581) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_SIH,n,user_Tdust))

    !H_DUST + SIH2_DUST -> SIH3_DUST
    k(4582) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_SIH2,n,user_Tdust))

    !H_DUST + SIH3_DUST -> SIH4_DUST
    k(4583) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_SIH3,n,user_Tdust))

    !H_DUST + SO2_DUST -> O2_DUST + HS_DUST
    k(4584) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_H,idx_SO2,n,user_Tdust))

    !H2_DUST + C_DUST -> CH2_DUST
    k(4585) = small + (krate_2bodySi(1.00d+00,2.50d+03,idx_H2,idx_C,n,user_Tdust))

    !H2_DUST + C2_DUST -> C2H_DUST + H_DUST
    k(4586) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C2,n,user_Tdust))

    !H2_DUST + C2H_DUST -> C2H2_DUST + H_DUST
    k(4587) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C2H,n,user_Tdust))

    !H2_DUST + C3_DUST -> C3H_DUST + H_DUST
    k(4588) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C3,n,user_Tdust))

    !H2_DUST + C3H_DUST -> C3H2_DUST + H_DUST
    k(4589) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C3H,n,user_Tdust))

    !H2_DUST + C4_DUST -> C4H_DUST + H_DUST
    k(4590) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C4,n,user_Tdust))

    !H2_DUST + C4H_DUST -> C4H2_DUST + H_DUST
    k(4591) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C4H,n,user_Tdust))

    !H2_DUST + C5_DUST -> C5H_DUST + H_DUST
    k(4592) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C5,n,user_Tdust))

    !H2_DUST + C5H_DUST -> C5H2_DUST + H_DUST
    k(4593) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C5H,n,user_Tdust))

    !H2_DUST + C6_DUST -> C6H_DUST + H_DUST
    k(4594) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C6,n,user_Tdust))

    !H2_DUST + C6H_DUST -> C6H2_DUST + H_DUST
    k(4595) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C6H,n,user_Tdust))

    !H2_DUST + C7_DUST -> C7H_DUST + H_DUST
    k(4596) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C7,n,user_Tdust))

    !H2_DUST + C7H_DUST -> C7H2_DUST + H_DUST
    k(4597) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C7H,n,user_Tdust))

    !H2_DUST + C8_DUST -> C8H_DUST + H_DUST
    k(4598) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C8,n,user_Tdust))

    !H2_DUST + C8H_DUST -> C8H2_DUST + H_DUST
    k(4599) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C8H,n,user_Tdust))

    !H2_DUST + C9_DUST -> C9H_DUST + H_DUST
    k(4600) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C9,n,user_Tdust))

    !H2_DUST + C9H_DUST -> C9H2_DUST + H_DUST
    k(4601) = small + (krate_2bodySi(1.00d+00,4.20d+03,idx_H2,idx_C9H,n,user_Tdust))

    !H2_DUST + CH2_DUST -> CH3_DUST + H_DUST
    k(4602) = small + (krate_2bodySi(1.00d+00,3.53d+03,idx_H2,idx_CH2,n,user_Tdust))

    !H2_DUST + CH3_DUST -> CH4_DUST + H_DUST
    k(4603) = small + (krate_2bodySi(1.00d+00,6.44d+03,idx_H2,idx_CH3,n,user_Tdust))

    !H2_DUST + CN_DUST -> HCN_DUST + H_DUST
    k(4604) = small + (krate_2bodySi(1.00d+00,2.07d+03,idx_H2,idx_CN,n,user_Tdust))

    !H2_DUST + NH2_DUST -> NH3_DUST + H_DUST
    k(4605) = small + (krate_2bodySi(1.00d+00,6.30d+03,idx_H2,idx_NH2,n,user_Tdust))

    !H2_DUST + OH_DUST -> H2O_DUST + H_DUST
    k(4606) = small + (krate_2bodySi(1.00d+00,2.10d+03,idx_H2,idx_OH,n,user_Tdust))

    !HCO_DUST + CH2OH_DUST -> HCOOCH3_DUST
    k(4607) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_HCO,idx_CH2OH,n,user_Tdust))

    !N_DUST + C2_DUST -> C2N_DUST
    k(4608) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C2,n,user_Tdust))

    !N_DUST + C3_DUST -> C3N_DUST
    k(4609) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C3,n,user_Tdust))

    !N_DUST + C3H_DUST -> HC3N_DUST
    k(4610) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C3H,n,user_Tdust))

    !N_DUST + C5_DUST -> C5N_DUST
    k(4611) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C5,n,user_Tdust))

    !N_DUST + C5H_DUST -> HC5N_DUST
    k(4612) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C5H,n,user_Tdust))

    !N_DUST + C7_DUST -> C7N_DUST
    k(4613) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C7,n,user_Tdust))

    !N_DUST + C7H_DUST -> HC7N_DUST
    k(4614) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C7H,n,user_Tdust))

    !N_DUST + C9_DUST -> C9N_DUST
    k(4615) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C9,n,user_Tdust))

    !N_DUST + C9H_DUST -> HC9N_DUST
    k(4616) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_C9H,n,user_Tdust))

    !N_DUST + CH_DUST -> HCN_DUST
    k(4617) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_CH,n,user_Tdust))

    !N_DUST + CH2_DUST -> H2CN_DUST
    k(4618) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_CH2,n,user_Tdust))

    !N_DUST + CH2OH_DUST -> NH2CHO_DUST
    k(4619) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_CH2OH,n,user_Tdust))

    !N_DUST + CH3_DUST -> CH3N_DUST
    k(4620) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_CH3,n,user_Tdust))

    !N_DUST + HS_DUST -> NS_DUST + H_DUST
    k(4621) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_HS,n,user_Tdust))

    !N_DUST + N_DUST -> N2_DUST
    k(4622) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_N,idx_N,n,user_Tdust))

    !N_DUST + NH_DUST -> N2_DUST + H_DUST
    k(4623) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_NH,n,user_Tdust))

    !N_DUST + NH2_DUST -> N2H2_DUST
    k(4624) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_NH2,n,user_Tdust))

    !N_DUST + NS_DUST -> N2_DUST + S_DUST
    k(4625) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_NS,n,user_Tdust))

    !N_DUST + O_DUST -> NO_DUST
    k(4626) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_O,n,user_Tdust))

    !N_DUST + O2H_DUST -> O2_DUST + NH_DUST
    k(4627) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_O2H,n,user_Tdust))

    !N_DUST + S_DUST -> NS_DUST
    k(4628) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_N,idx_S,n,user_Tdust))

    !NH_DUST + CH2_DUST -> CH3N_DUST
    k(4629) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH,idx_CH2,n,user_Tdust))

    !NH_DUST + CH3_DUST -> CH3NH_DUST
    k(4630) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH,idx_CH3,n,user_Tdust))

    !NH_DUST + NH_DUST -> N2H2_DUST
    k(4631) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_NH,idx_NH,n,user_Tdust))

    !NH_DUST + NH_DUST -> N2_DUST + H2_DUST
    k(4632) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_NH,idx_NH,n,user_Tdust))

    !NH_DUST + NO_DUST -> N2_DUST + O_DUST + H_DUST
    k(4633) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH,idx_NO,n,user_Tdust))

    !NH2_DUST + CH3_DUST -> CH5N_DUST
    k(4634) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH2,idx_CH3,n,user_Tdust))

    !NH2_DUST + HCO_DUST -> NH2CHO_DUST
    k(4635) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH2,idx_HCO,n,user_Tdust))

    !NH2_DUST + NO_DUST -> H2O_DUST + N2_DUST
    k(4636) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_NH2,idx_NO,n,user_Tdust))

    !O_DUST + C2_DUST -> CCO_DUST
    k(4637) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_C2,n,user_Tdust))

    !O_DUST + C3_DUST -> C3O_DUST
    k(4638) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_C3,n,user_Tdust))

    !O_DUST + CH_DUST -> HCO_DUST
    k(4639) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_CH,n,user_Tdust))

    !O_DUST + CH2_DUST -> H2CO_DUST
    k(4640) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_CH2,n,user_Tdust))

    !O_DUST + CH3_DUST -> CH2OH_DUST
    k(4641) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_CH3,n,user_Tdust))

    !O_DUST + CN_DUST -> OCN_DUST
    k(4642) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_CN,n,user_Tdust))

    !O_DUST + CO_DUST -> CO2_DUST
    k(4643) = small + (krate_2bodySi(1.00d+00,1.00d+03,idx_O,idx_CO,n,user_Tdust))

    !O_DUST + CS_DUST -> OCS_DUST
    k(4644) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_CS,n,user_Tdust))

    !O_DUST + HCO_DUST -> CO2_DUST + H_DUST
    k(4645) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_O,idx_HCO,n,user_Tdust))

    !O_DUST + HCO_DUST -> CO_DUST + OH_DUST
    k(4646) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_O,idx_HCO,n,user_Tdust))

    !O_DUST + HNO_DUST -> NO_DUST + OH_DUST
    k(4647) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_HNO,n,user_Tdust))

    !O_DUST + HS_DUST -> SO_DUST + H_DUST
    k(4648) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_HS,n,user_Tdust))

    !O_DUST + NH_DUST -> HNO_DUST
    k(4649) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_NH,n,user_Tdust))

    !O_DUST + NH2_DUST -> HNO_DUST + H_DUST
    k(4650) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_NH2,n,user_Tdust))

    !O_DUST + NS_DUST -> NO_DUST + S_DUST
    k(4651) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_NS,n,user_Tdust))

    !O_DUST + O_DUST -> O2_DUST
    k(4652) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_O,idx_O,n,user_Tdust))

    !O_DUST + O2_DUST -> O3_DUST
    k(4653) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_O2,n,user_Tdust))

    !O_DUST + O2H_DUST -> O2_DUST + OH_DUST
    k(4654) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_O2H,n,user_Tdust))

    !O_DUST + OH_DUST -> O2H_DUST
    k(4655) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_OH,n,user_Tdust))

    !O_DUST + S_DUST -> SO_DUST
    k(4656) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_S,n,user_Tdust))

    !O_DUST + SO_DUST -> SO2_DUST
    k(4657) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_O,idx_SO,n,user_Tdust))

    !OH_DUST + CH2_DUST -> CH2OH_DUST
    k(4658) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_OH,idx_CH2,n,user_Tdust))

    !OH_DUST + CH3_DUST -> CH4O_DUST
    k(4659) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_OH,idx_CH3,n,user_Tdust))

    !OH_DUST + CO_DUST -> CO2_DUST + H_DUST
    k(4660) = small + (krate_2bodySi(1.00d+00,8.00d+01,idx_OH,idx_CO,n,user_Tdust))

    !OH_DUST + H2CO_DUST -> HCO_DUST + H2O_DUST
    k(4661) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_OH,idx_H2CO,n,user_Tdust))

    !OH_DUST + HCO_DUST -> CH2O2_DUST
    k(4662) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_OH,idx_HCO,n,user_Tdust))

    !OH_DUST + NH2_DUST -> NH2OH_DUST
    k(4663) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_OH,idx_NH2,n,user_Tdust))

    !OH_DUST + OH_DUST -> H2O2_DUST
    k(4664) = small + (krate_2bodySi(5.00d-01,0.00d+00,idx_OH,idx_OH,n,user_Tdust))

    !S_DUST + CH_DUST -> HCS_DUST
    k(4665) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_S,idx_CH,n,user_Tdust))

    !S_DUST + CH3_DUST -> H2CS_DUST + H_DUST
    k(4666) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_S,idx_CH3,n,user_Tdust))

    !S_DUST + CO_DUST -> OCS_DUST
    k(4667) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_S,idx_CO,n,user_Tdust))

    !S_DUST + NH_DUST -> NS_DUST + H_DUST
    k(4668) = small + (krate_2bodySi(1.00d+00,0.00d+00,idx_S,idx_NH,n,user_Tdust))

    !C_DUST -> C
    k(4669) = small + (krate_evaporation_total(idx_C))

    !C10_DUST -> C10
    k(4670) = small + (krate_evaporation_total(idx_C10))

    !C2_DUST -> C2
    k(4671) = small + (krate_evaporation_total(idx_C2))

    !C2H_DUST -> C2H
    k(4672) = small + (krate_evaporation_total(idx_C2H))

    !C2H2_DUST -> C2H2
    k(4673) = small + (krate_evaporation_total(idx_C2H2))

    !C2H3_DUST -> C2H3
    k(4674) = small + (krate_evaporation_total(idx_C2H3))

    !C2H4_DUST -> C2H4
    k(4675) = small + (krate_evaporation_total(idx_C2H4))

    !C2H5_DUST -> C2H5
    k(4676) = small + (krate_evaporation_total(idx_C2H5))

    !C2H5OH_DUST -> C2H5OH
    k(4677) = small + (krate_evaporation_total(idx_C2H5OH))

    !C2H6_DUST -> C2H6
    k(4678) = small + (krate_evaporation_total(idx_C2H6))

    !C2N_DUST -> C2N
    k(4679) = small + (krate_evaporation_total(idx_C2N))

    !CCO_DUST -> CCO
    k(4680) = small + (krate_evaporation_total(idx_CCO))

    !C2S_DUST -> C2S
    k(4681) = small + (krate_evaporation_total(idx_C2S))

    !C3_DUST -> C3
    k(4682) = small + (krate_evaporation_total(idx_C3))

    !C3H_DUST -> C3H
    k(4683) = small + (krate_evaporation_total(idx_C3H))

    !C3H2_DUST -> C3H2
    k(4684) = small + (krate_evaporation_total(idx_C3H2))

    !C3H3_DUST -> C3H3
    k(4685) = small + (krate_evaporation_total(idx_C3H3))

    !C3H3N_DUST -> C3H3N
    k(4686) = small + (krate_evaporation_total(idx_C3H3N))

    !C3H4_DUST -> C3H4
    k(4687) = small + (krate_evaporation_total(idx_C3H4))

    !C3N_DUST -> C3N
    k(4688) = small + (krate_evaporation_total(idx_C3N))

    !C3O_DUST -> C3O
    k(4689) = small + (krate_evaporation_total(idx_C3O))

    !C3S_DUST -> C3S
    k(4690) = small + (krate_evaporation_total(idx_C3S))

    !C4_DUST -> C4
    k(4691) = small + (krate_evaporation_total(idx_C4))

    !C4H_DUST -> C4H
    k(4692) = small + (krate_evaporation_total(idx_C4H))

    !C4H2_DUST -> C4H2
    k(4693) = small + (krate_evaporation_total(idx_C4H2))

    !C4H3_DUST -> C4H3
    k(4694) = small + (krate_evaporation_total(idx_C4H3))

    !C4H4_DUST -> C4H4
    k(4695) = small + (krate_evaporation_total(idx_C4H4))

    !C4N_DUST -> C4N
    k(4696) = small + (krate_evaporation_total(idx_C4N))

    !C4S_DUST -> C4S
    k(4697) = small + (krate_evaporation_total(idx_C4S))

    !C5_DUST -> C5
    k(4698) = small + (krate_evaporation_total(idx_C5))

    !C5H_DUST -> C5H
    k(4699) = small + (krate_evaporation_total(idx_C5H))

    !C5H2_DUST -> C5H2
    k(4700) = small + (krate_evaporation_total(idx_C5H2))

    !C5H3_DUST -> C5H3
    k(4701) = small + (krate_evaporation_total(idx_C5H3))

    !C5H4_DUST -> C5H4
    k(4702) = small + (krate_evaporation_total(idx_C5H4))

    !C5N_DUST -> C5N
    k(4703) = small + (krate_evaporation_total(idx_C5N))

    !C6_DUST -> C6
    k(4704) = small + (krate_evaporation_total(idx_C6))

    !C6H_DUST -> C6H
    k(4705) = small + (krate_evaporation_total(idx_C6H))

    !C6H2_DUST -> C6H2
    k(4706) = small + (krate_evaporation_total(idx_C6H2))

    !C6H3_DUST -> C6H3
    k(4707) = small + (krate_evaporation_total(idx_C6H3))

    !C6H4_DUST -> C6H4
    k(4708) = small + (krate_evaporation_total(idx_C6H4))

    !C6H6_DUST -> C6H6
    k(4709) = small + (krate_evaporation_total(idx_C6H6))

    !C7_DUST -> C7
    k(4710) = small + (krate_evaporation_total(idx_C7))

    !C7H_DUST -> C7H
    k(4711) = small + (krate_evaporation_total(idx_C7H))

    !C7H2_DUST -> C7H2
    k(4712) = small + (krate_evaporation_total(idx_C7H2))

    !C7H3_DUST -> C7H3
    k(4713) = small + (krate_evaporation_total(idx_C7H3))

    !C7H4_DUST -> C7H4
    k(4714) = small + (krate_evaporation_total(idx_C7H4))

    !C7N_DUST -> C7N
    k(4715) = small + (krate_evaporation_total(idx_C7N))

    !C8_DUST -> C8
    k(4716) = small + (krate_evaporation_total(idx_C8))

    !C8H_DUST -> C8H
    k(4717) = small + (krate_evaporation_total(idx_C8H))

    !C8H2_DUST -> C8H2
    k(4718) = small + (krate_evaporation_total(idx_C8H2))

    !C8H3_DUST -> C8H3
    k(4719) = small + (krate_evaporation_total(idx_C8H3))

    !C8H4_DUST -> C8H4
    k(4720) = small + (krate_evaporation_total(idx_C8H4))

    !C9_DUST -> C9
    k(4721) = small + (krate_evaporation_total(idx_C9))

    !C9H_DUST -> C9H
    k(4722) = small + (krate_evaporation_total(idx_C9H))

    !C9H2_DUST -> C9H2
    k(4723) = small + (krate_evaporation_total(idx_C9H2))

    !C9H3_DUST -> C9H3
    k(4724) = small + (krate_evaporation_total(idx_C9H3))

    !C9H4_DUST -> C9H4
    k(4725) = small + (krate_evaporation_total(idx_C9H4))

    !C9N_DUST -> C9N
    k(4726) = small + (krate_evaporation_total(idx_C9N))

    !CH_DUST -> CH
    k(4727) = small + (krate_evaporation_total(idx_CH))

    !CH2_DUST -> CH2
    k(4728) = small + (krate_evaporation_total(idx_CH2))

    !C2H2N_DUST -> C2H2N
    k(4729) = small + (krate_evaporation_total(idx_C2H2N))

    !C2H2O_DUST -> C2H2O
    k(4730) = small + (krate_evaporation_total(idx_C2H2O))

    !CH3N_DUST -> CH3N
    k(4731) = small + (krate_evaporation_total(idx_CH3N))

    !CH2NH2_DUST -> CH2NH2
    k(4732) = small + (krate_evaporation_total(idx_CH2NH2))

    !CH2OH_DUST -> CH2OH
    k(4733) = small + (krate_evaporation_total(idx_CH2OH))

    !CH3_DUST -> CH3
    k(4734) = small + (krate_evaporation_total(idx_CH3))

    !CH3C3N_DUST -> CH3C3N
    k(4735) = small + (krate_evaporation_total(idx_CH3C3N))

    !CH3C4H_DUST -> CH3C4H
    k(4736) = small + (krate_evaporation_total(idx_CH3C4H))

    !CH3C5N_DUST -> CH3C5N
    k(4737) = small + (krate_evaporation_total(idx_CH3C5N))

    !CH3C6H_DUST -> CH3C6H
    k(4738) = small + (krate_evaporation_total(idx_CH3C6H))

    !CH3C7N_DUST -> CH3C7N
    k(4739) = small + (krate_evaporation_total(idx_CH3C7N))

    !C2H4O_DUST -> C2H4O
    k(4740) = small + (krate_evaporation_total(idx_C2H4O))

    !C2H3N_DUST -> C2H3N
    k(4741) = small + (krate_evaporation_total(idx_C2H3N))

    !CH3NH_DUST -> CH3NH
    k(4742) = small + (krate_evaporation_total(idx_CH3NH))

    !CH3OCH3_DUST -> CH3OCH3
    k(4743) = small + (krate_evaporation_total(idx_CH3OCH3))

    !CH4O_DUST -> CH4O
    k(4744) = small + (krate_evaporation_total(idx_CH4O))

    !CH4_DUST -> CH4
    k(4745) = small + (krate_evaporation_total(idx_CH4))

    !CH5N_DUST -> CH5N
    k(4746) = small + (krate_evaporation_total(idx_CH5N))

    !CHNH_DUST -> CHNH
    k(4747) = small + (krate_evaporation_total(idx_CHNH))

    !CN_DUST -> CN
    k(4748) = small + (krate_evaporation_total(idx_CN))

    !CO_DUST -> CO
    k(4749) = small + (krate_evaporation_total(idx_CO))

    !CO2_DUST -> CO2
    k(4750) = small + (krate_evaporation_total(idx_CO2))

    !CS_DUST -> CS
    k(4751) = small + (krate_evaporation_total(idx_CS))

    !FE_DUST -> FE
    k(4752) = small + (krate_evaporation_total(idx_FE))

    !FEH_DUST -> FEH
    k(4753) = small + (krate_evaporation_total(idx_FEH))

    !H_DUST -> H
    k(4754) = small + (krate_evaporation_total(idx_H))

    !H2_DUST -> H2
    k(4755) = small + (krate_evaporation_total(idx_H2))

    !H2C3N_DUST -> H2C3N
    k(4756) = small + (krate_evaporation_total(idx_H2C3N))

    !H2C3O_DUST -> H2C3O
    k(4757) = small + (krate_evaporation_total(idx_H2C3O))

    !H2C5N_DUST -> H2C5N
    k(4758) = small + (krate_evaporation_total(idx_H2C5N))

    !H2C7N_DUST -> H2C7N
    k(4759) = small + (krate_evaporation_total(idx_H2C7N))

    !H2C9N_DUST -> H2C9N
    k(4760) = small + (krate_evaporation_total(idx_H2C9N))

    !H2CN_DUST -> H2CN
    k(4761) = small + (krate_evaporation_total(idx_H2CN))

    !H2CO_DUST -> H2CO
    k(4762) = small + (krate_evaporation_total(idx_H2CO))

    !H2CS_DUST -> H2CS
    k(4763) = small + (krate_evaporation_total(idx_H2CS))

    !H2O_DUST -> H2O
    k(4764) = small + (krate_evaporation_total(idx_H2O))

    !H2O2_DUST -> H2O2
    k(4765) = small + (krate_evaporation_total(idx_H2O2))

    !H2S_DUST -> H2S
    k(4766) = small + (krate_evaporation_total(idx_H2S))

    !H2S2_DUST -> H2S2
    k(4767) = small + (krate_evaporation_total(idx_H2S2))

    !H3C5N_DUST -> H3C5N
    k(4768) = small + (krate_evaporation_total(idx_H3C5N))

    !H3C7N_DUST -> H3C7N
    k(4769) = small + (krate_evaporation_total(idx_H3C7N))

    !H3C9N_DUST -> H3C9N
    k(4770) = small + (krate_evaporation_total(idx_H3C9N))

    !H4C3N_DUST -> H4C3N
    k(4771) = small + (krate_evaporation_total(idx_H4C3N))

    !H5C3N_DUST -> H5C3N
    k(4772) = small + (krate_evaporation_total(idx_H5C3N))

    !HC2NC_DUST -> HC3N
    k(4773) = small + (krate_evaporation_total(idx_HC2NC))

    !HC2O_DUST -> HC2O
    k(4774) = small + (krate_evaporation_total(idx_HC2O))

    !HC3N_DUST -> HC3N
    k(4775) = small + (krate_evaporation_total(idx_HC3N))

    !HC3O_DUST -> HC3O
    k(4776) = small + (krate_evaporation_total(idx_HC3O))

    !HC5N_DUST -> HC5N
    k(4777) = small + (krate_evaporation_total(idx_HC5N))

    !HC7N_DUST -> HC7N
    k(4778) = small + (krate_evaporation_total(idx_HC7N))

    !HC9N_DUST -> HC9N
    k(4779) = small + (krate_evaporation_total(idx_HC9N))

    !HCCN_DUST -> HCCN
    k(4780) = small + (krate_evaporation_total(idx_HCCN))

    !HCN_DUST -> HCN
    k(4781) = small + (krate_evaporation_total(idx_HCN))

    !HCNC2_DUST -> HC3N
    k(4782) = small + (krate_evaporation_total(idx_HCNC2))

    !HCO_DUST -> HCO
    k(4783) = small + (krate_evaporation_total(idx_HCO))

    !HCOOCH3_DUST -> HCOOCH3
    k(4784) = small + (krate_evaporation_total(idx_HCOOCH3))

    !CH2O2_DUST -> CH2O2
    k(4785) = small + (krate_evaporation_total(idx_CH2O2))

    !HCS_DUST -> HCS
    k(4786) = small + (krate_evaporation_total(idx_HCS))

    !HE_DUST -> HE
    k(4787) = small + (krate_evaporation_total(idx_HE))

    !HNC_DUST -> HNC
    k(4788) = small + (krate_evaporation_total(idx_HNC))

    !HNC3_DUST -> HC3N
    k(4789) = small + (krate_evaporation_total(idx_HNC3))

    !HNCO_DUST -> HNCO
    k(4790) = small + (krate_evaporation_total(idx_HNCO))

    !HNO_DUST -> HNO
    k(4791) = small + (krate_evaporation_total(idx_HNO))

    !HS_DUST -> HS
    k(4792) = small + (krate_evaporation_total(idx_HS))

    !HS2_DUST -> HS2
    k(4793) = small + (krate_evaporation_total(idx_HS2))

    !MG_DUST -> MG
    k(4794) = small + (krate_evaporation_total(idx_MG))

    !MGH_DUST -> MGH
    k(4795) = small + (krate_evaporation_total(idx_MGH))

    !MGH2_DUST -> MGH2
    k(4796) = small + (krate_evaporation_total(idx_MGH2))

    !N_DUST -> N
    k(4797) = small + (krate_evaporation_total(idx_N))

    !N2_DUST -> N2
    k(4798) = small + (krate_evaporation_total(idx_N2))

    !N2H2_DUST -> N2H2
    k(4799) = small + (krate_evaporation_total(idx_N2H2))

    !NA_DUST -> NA
    k(4800) = small + (krate_evaporation_total(idx_NA))

    !NAH_DUST -> NAH
    k(4801) = small + (krate_evaporation_total(idx_NAH))

    !NAOH_DUST -> NAOH
    k(4802) = small + (krate_evaporation_total(idx_NAOH))

    !NH_DUST -> NH
    k(4803) = small + (krate_evaporation_total(idx_NH))

    !NH2_DUST -> NH2
    k(4804) = small + (krate_evaporation_total(idx_NH2))

    !NH2CHO_DUST -> NH2CHO
    k(4805) = small + (krate_evaporation_total(idx_NH2CHO))

    !NH2OH_DUST -> NH2OH
    k(4806) = small + (krate_evaporation_total(idx_NH2OH))

    !NH3_DUST -> NH3
    k(4807) = small + (krate_evaporation_total(idx_NH3))

    !NO_DUST -> NO
    k(4808) = small + (krate_evaporation_total(idx_NO))

    !NS_DUST -> NS
    k(4809) = small + (krate_evaporation_total(idx_NS))

    !O_DUST -> O
    k(4810) = small + (krate_evaporation_total(idx_O))

    !O2_DUST -> O2
    k(4811) = small + (krate_evaporation_total(idx_O2))

    !O2H_DUST -> O2H
    k(4812) = small + (krate_evaporation_total(idx_O2H))

    !O3_DUST -> O3
    k(4813) = small + (krate_evaporation_total(idx_O3))

    !OCN_DUST -> OCN
    k(4814) = small + (krate_evaporation_total(idx_OCN))

    !OCS_DUST -> OCS
    k(4815) = small + (krate_evaporation_total(idx_OCS))

    !OH_DUST -> OH
    k(4816) = small + (krate_evaporation_total(idx_OH))

    !S_DUST -> S
    k(4817) = small + (krate_evaporation_total(idx_S))

    !S2_DUST -> S2
    k(4818) = small + (krate_evaporation_total(idx_S2))

    !SI_DUST -> SI
    k(4819) = small + (krate_evaporation_total(idx_SI))

    !SIC_DUST -> SIC
    k(4820) = small + (krate_evaporation_total(idx_SIC))

    !SIH_DUST -> SIH
    k(4821) = small + (krate_evaporation_total(idx_SIH))

    !SIH2_DUST -> SIH2
    k(4822) = small + (krate_evaporation_total(idx_SIH2))

    !SIH3_DUST -> SIH3
    k(4823) = small + (krate_evaporation_total(idx_SIH3))

    !SIH4_DUST -> SIH4
    k(4824) = small + (krate_evaporation_total(idx_SIH4))

    !SIO_DUST -> SIO
    k(4825) = small + (krate_evaporation_total(idx_SIO))

    !SIS_DUST -> SIS
    k(4826) = small + (krate_evaporation_total(idx_SIS))

    !SO_DUST -> SO
    k(4827) = small + (krate_evaporation_total(idx_SO))

    !SO2_DUST -> SO2
    k(4828) = small + (krate_evaporation_total(idx_SO2))

    !C10_DUST -> C9_DUST + C_DUST
    k(4829) = small + (1.00E+03&
        *user_crflux)

    !C2_DUST -> C_DUST + C_DUST
    k(4830) = small + (2.37E+02&
        *user_crflux)

    !C2H_DUST -> C2_DUST + H_DUST
    k(4831) = small + (5.00E+03&
        *user_crflux)

    !C2H2_DUST -> C2H_DUST + H_DUST
    k(4832) = small + (5.15E+03&
        *user_crflux)

    !C2H3_DUST -> C2H2_DUST + H_DUST
    k(4833) = small + (1.50E+03&
        *user_crflux)

    !C2H4_DUST -> C2H2_DUST + H2_DUST
    k(4834) = small + (3.70E+03&
        *user_crflux)

    !C2H5_DUST -> C2H4_DUST + H_DUST
    k(4835) = small + (1.50E+03&
        *user_crflux)

    !C2H6_DUST -> C2H4_DUST + H2_DUST
    k(4836) = small + (3.00E+03&
        *user_crflux)

    !C2H6CO_DUST -> C2H2O_DUST + CH4_DUST
    k(4837) = small + (1.50E+03&
        *user_crflux)

    !C2N_DUST -> C_DUST + CN_DUST
    k(4838) = small + (1.00E+03&
        *user_crflux)

    !CCO_DUST -> CO_DUST + C_DUST
    k(4839) = small + (7.50E+02&
        *user_crflux)

    !CCO_DUST -> C2_DUST + O_DUST
    k(4840) = small + (7.50E+02&
        *user_crflux)

    !C2S_DUST -> CS_DUST + C_DUST
    k(4841) = small + (1.50E+03&
        *user_crflux)

    !C3_DUST -> C2_DUST + C_DUST
    k(4842) = small + (1.12E+03&
        *user_crflux)

    !C3H_DUST -> C3_DUST + H_DUST
    k(4843) = small + (1.00E+04&
        *user_crflux)

    !C3H2_DUST -> C3H_DUST + H_DUST
    k(4844) = small + (1.00E+04&
        *user_crflux)

    !C3H3_DUST -> C3H2_DUST + H_DUST
    k(4845) = small + (1.00E+04&
        *user_crflux)

    !C3H3N_DUST -> C2H3_DUST + CN_DUST
    k(4846) = small + (1.50E+03&
        *user_crflux)

    !C3H4_DUST -> C3H3_DUST + H_DUST
    k(4847) = small + (3.28E+03&
        *user_crflux)

    !C3N_DUST -> C2_DUST + CN_DUST
    k(4848) = small + (1.75E+03&
        *user_crflux)

    !C3O_DUST -> C2_DUST + CO_DUST
    k(4849) = small + (6.60E+03&
        *user_crflux)

    !C3P_DUST -> CCP_DUST + C_DUST
    k(4850) = small + (1.50E+03&
        *user_crflux)

    !C3S_DUST -> C2_DUST + CS_DUST
    k(4851) = small + (1.50E+03&
        *user_crflux)

    !C4_DUST -> C3_DUST + C_DUST
    k(4852) = small + (1.00E+03&
        *user_crflux)

    !C4H_DUST -> C4_DUST + H_DUST
    k(4853) = small + (5.00E+03&
        *user_crflux)

    !C4H2_DUST -> C2H_DUST + C2H_DUST
    k(4854) = small + (1.73E+03&
        *user_crflux)

    !C4H2_DUST -> C4H_DUST + H_DUST
    k(4855) = small + (1.73E+03&
        *user_crflux)

    !C4H3_DUST -> C4H2_DUST + H_DUST
    k(4856) = small + (1.00E+04&
        *user_crflux)

    !C4H4_DUST -> C4H2_DUST + H2_DUST
    k(4857) = small + (7.50E+03&
        *user_crflux)

    !C4N_DUST -> C3_DUST + CN_DUST
    k(4858) = small + (1.00E+03&
        *user_crflux)

    !C4P_DUST -> C3P_DUST + C_DUST
    k(4859) = small + (1.50E+03&
        *user_crflux)

    !C4S_DUST -> C3_DUST + CS_DUST
    k(4860) = small + (1.50E+03&
        *user_crflux)

    !C5_DUST -> C4_DUST + C_DUST
    k(4861) = small + (1.00E+03&
        *user_crflux)

    !C5H_DUST -> C5_DUST + H_DUST
    k(4862) = small + (5.00E+03&
        *user_crflux)

    !C5H2_DUST -> C5H_DUST + H_DUST
    k(4863) = small + (1.75E+03&
        *user_crflux)

    !C5H3_DUST -> C5H2_DUST + H_DUST
    k(4864) = small + (1.00E+04&
        *user_crflux)

    !C5H4_DUST -> C5H2_DUST + H2_DUST
    k(4865) = small + (7.50E+03&
        *user_crflux)

    !C5N_DUST -> C4_DUST + CN_DUST
    k(4866) = small + (1.75E+03&
        *user_crflux)

    !C6_DUST -> C5_DUST + C_DUST
    k(4867) = small + (1.00E+03&
        *user_crflux)

    !C6H_DUST -> C6_DUST + H_DUST
    k(4868) = small + (5.00E+03&
        *user_crflux)

    !C6H2_DUST -> C6H_DUST + H_DUST
    k(4869) = small + (1.75E+03&
        *user_crflux)

    !C6H3_DUST -> C6H2_DUST + H_DUST
    k(4870) = small + (1.00E+04&
        *user_crflux)

    !C6H4_DUST -> C6H2_DUST + H2_DUST
    k(4871) = small + (7.50E+03&
        *user_crflux)

    !C6H6_DUST -> C6H4_DUST + H2_DUST
    k(4872) = small + (3.00E+03&
        *user_crflux)

    !C7_DUST -> C6_DUST + C_DUST
    k(4873) = small + (1.00E+03&
        *user_crflux)

    !C7H_DUST -> C7_DUST + H_DUST
    k(4874) = small + (5.00E+03&
        *user_crflux)

    !C7H2_DUST -> C7H_DUST + H_DUST
    k(4875) = small + (1.75E+03&
        *user_crflux)

    !C7H3_DUST -> C7H2_DUST + H_DUST
    k(4876) = small + (1.00E+04&
        *user_crflux)

    !C7H4_DUST -> C7H2_DUST + H2_DUST
    k(4877) = small + (7.50E+03&
        *user_crflux)

    !C7N_DUST -> C6_DUST + CN_DUST
    k(4878) = small + (1.75E+03&
        *user_crflux)

    !C8_DUST -> C7_DUST + C_DUST
    k(4879) = small + (1.00E+03&
        *user_crflux)

    !C8H_DUST -> C8_DUST + H_DUST
    k(4880) = small + (5.00E+03&
        *user_crflux)

    !C8H2_DUST -> C8H_DUST + H_DUST
    k(4881) = small + (1.75E+03&
        *user_crflux)

    !C8H3_DUST -> C8H2_DUST + H_DUST
    k(4882) = small + (1.00E+04&
        *user_crflux)

    !C8H4_DUST -> C8H2_DUST + H2_DUST
    k(4883) = small + (7.50E+03&
        *user_crflux)

    !C9_DUST -> C8_DUST + C_DUST
    k(4884) = small + (1.00E+03&
        *user_crflux)

    !C9H_DUST -> C9_DUST + H_DUST
    k(4885) = small + (5.00E+03&
        *user_crflux)

    !C9H2_DUST -> C9H_DUST + H_DUST
    k(4886) = small + (1.75E+03&
        *user_crflux)

    !C9H3_DUST -> C9H2_DUST + H_DUST
    k(4887) = small + (1.00E+04&
        *user_crflux)

    !C9H4_DUST -> C9H2_DUST + H2_DUST
    k(4888) = small + (7.50E+03&
        *user_crflux)

    !C9N_DUST -> C8_DUST + CN_DUST
    k(4889) = small + (1.75E+03&
        *user_crflux)

    !CCL_DUST -> C_DUST + CL_DUST
    k(4890) = small + (5.00E+02&
        *user_crflux)

    !CCP_DUST -> C2_DUST + P_DUST
    k(4891) = small + (7.50E+02&
        *user_crflux)

    !CCP_DUST -> CP_DUST + C_DUST
    k(4892) = small + (7.50E+02&
        *user_crflux)

    !CH_DUST -> C_DUST + H_DUST
    k(4893) = small + (7.30E+02&
        *user_crflux)

    !C2H2N_DUST -> CH2_DUST + CN_DUST
    k(4894) = small + (5.00E+03&
        *user_crflux)

    !C2H2O_DUST -> CH2_DUST + CO_DUST
    k(4895) = small + (9.15E+02&
        *user_crflux)

    !CH3N_DUST -> HCN_DUST + H2_DUST
    k(4896) = small + (4.98E+03&
        *user_crflux)

    !CH2NH2_DUST -> CH2_DUST + NH2_DUST
    k(4897) = small + (9.50E+03&
        *user_crflux)

    !CH2OH_DUST -> CH2_DUST + OH_DUST
    k(4898) = small + (3.00E+03&
        *user_crflux)

    !CH2PH_DUST -> HCP_DUST + H2_DUST
    k(4899) = small + (1.50E+03&
        *user_crflux)

    !CH3_DUST -> CH2_DUST + H_DUST
    k(4900) = small + (5.00E+02&
        *user_crflux)

    !CH3C3N_DUST -> CH3_DUST + C3N_DUST
    k(4901) = small + (1.50E+03&
        *user_crflux)

    !CH3C4H_DUST -> CH3_DUST + C4H_DUST
    k(4902) = small + (1.50E+03&
        *user_crflux)

    !CH3C5N_DUST -> CH3_DUST + C5N_DUST
    k(4903) = small + (1.50E+03&
        *user_crflux)

    !CH3C6H_DUST -> CH3_DUST + C6H_DUST
    k(4904) = small + (1.50E+03&
        *user_crflux)

    !CH3C7N_DUST -> CH3_DUST + C7N_DUST
    k(4905) = small + (1.50E+03&
        *user_crflux)

    !C2H4O_DUST -> CH3_DUST + HCO_DUST
    k(4906) = small + (5.25E+02&
        *user_crflux)

    !C2H4O_DUST -> CH4_DUST + CO_DUST
    k(4907) = small + (5.25E+02&
        *user_crflux)

    !C2H3N_DUST -> CH3_DUST + CN_DUST
    k(4908) = small + (4.76E+03&
        *user_crflux)

    !CH3NH_DUST -> CH3N_DUST + H_DUST
    k(4909) = small + (9.50E+03&
        *user_crflux)

    !CH3OCH3_DUST -> H2CO_DUST + CH4_DUST
    k(4910) = small + (1.72E+03&
        *user_crflux)

    !CH4O_DUST -> CH3_DUST + OH_DUST
    k(4911) = small + (1.50E+03&
        *user_crflux)

    !CH4O_DUST -> H2CO_DUST + H2_DUST
    k(4912) = small + (3.17E+03&
        *user_crflux)

    !CH4_DUST -> CH2_DUST + H2_DUST
    k(4913) = small + (2.34E+03&
        *user_crflux)

    !CH5N_DUST -> HCN_DUST + H2_DUST + H_DUST + H_DUST
    k(4914) = small + (1.41E+03&
        *user_crflux)

    !CHNH_DUST -> CH_DUST + NH_DUST
    k(4915) = small + (1.00E+03&
        *user_crflux)

    !CLO_DUST -> CL_DUST + O_DUST
    k(4916) = small + (5.00E+02&
        *user_crflux)

    !CN_DUST -> C_DUST + N_DUST
    k(4917) = small + (1.06E+04&
        *user_crflux)

    !CO_DUST -> C_DUST + O_DUST
    k(4918) = small + (5.00E+00&
        *user_crflux)

    !CO2_DUST -> CO_DUST + O_DUST
    k(4919) = small + (1.71E+03&
        *user_crflux)

    !CP_DUST -> C_DUST + P_DUST
    k(4920) = small + (5.00E+02&
        *user_crflux)

    !CS_DUST -> C_DUST + S_DUST
    k(4921) = small + (5.00E+02&
        *user_crflux)

    !FEH_DUST -> FE_DUST + H_DUST
    k(4922) = small + (1.00E+03&
        *user_crflux)

    !H2C3N_DUST -> C2H2_DUST + CN_DUST
    k(4923) = small + (3.00E+03&
        *user_crflux)

    !H2C3O_DUST -> C2H2_DUST + CO_DUST
    k(4924) = small + (1.80E+03&
        *user_crflux)

    !H2C5N_DUST -> C4H2_DUST + CN_DUST
    k(4925) = small + (3.00E+03&
        *user_crflux)

    !H2C7N_DUST -> C6H2_DUST + CN_DUST
    k(4926) = small + (3.00E+03&
        *user_crflux)

    !H2C9N_DUST -> C8H2_DUST + CN_DUST
    k(4927) = small + (3.00E+03&
        *user_crflux)

    !H2CN_DUST -> CH_DUST + NH_DUST
    k(4928) = small + (1.00E+03&
        *user_crflux)

    !H2CO_DUST -> CO_DUST + H2_DUST
    k(4929) = small + (2.66E+03&
        *user_crflux)

    !H2CS_DUST -> H2_DUST + CS_DUST
    k(4930) = small + (1.50E+03&
        *user_crflux)

    !H2O_DUST -> OH_DUST + H_DUST
    k(4931) = small + (9.70E+02&
        *user_crflux)

    !H2O2_DUST -> OH_DUST + OH_DUST
    k(4932) = small + (1.50E+03&
        *user_crflux)

    !H2S_DUST -> H2_DUST + S_DUST
    k(4933) = small + (5.15E+03&
        *user_crflux)

    !H2S2_DUST -> HS_DUST + HS_DUST
    k(4934) = small + (1.50E+03&
        *user_crflux)

    !H2SIO_DUST -> SIO_DUST + H2_DUST
    k(4935) = small + (1.50E+03&
        *user_crflux)

    !H3C5N_DUST -> C2H3_DUST + C3N_DUST
    k(4936) = small + (3.00E+03&
        *user_crflux)

    !H3C7N_DUST -> C2H3_DUST + C5N_DUST
    k(4937) = small + (3.00E+03&
        *user_crflux)

    !H3C9N_DUST -> C2H3_DUST + C7N_DUST
    k(4938) = small + (3.00E+03&
        *user_crflux)

    !H4C3N_DUST -> C2H4_DUST + CN_DUST
    k(4939) = small + (3.00E+03&
        *user_crflux)

    !H5C3N_DUST -> C2H5_DUST + CN_DUST
    k(4940) = small + (3.00E+03&
        *user_crflux)

    !HC2NC_DUST -> C2H_DUST + CN_DUST
    k(4941) = small + (3.45E+03&
        *user_crflux)

    !HC2O_DUST -> CO_DUST + CH_DUST
    k(4942) = small + (1.50E+03&
        *user_crflux)

    !HC3N_DUST -> C2H_DUST + CN_DUST
    k(4943) = small + (1.72E+03&
        *user_crflux)

    !HC3O_DUST -> CO_DUST + C2H_DUST
    k(4944) = small + (1.50E+03&
        *user_crflux)

    !HC5N_DUST -> C4H_DUST + CN_DUST
    k(4945) = small + (1.75E+03&
        *user_crflux)

    !HC7N_DUST -> C6H_DUST + CN_DUST
    k(4946) = small + (1.75E+03&
        *user_crflux)

    !HC9N_DUST -> C8H_DUST + CN_DUST
    k(4947) = small + (1.75E+03&
        *user_crflux)

    !HCCN_DUST -> C2N_DUST + H_DUST
    k(4948) = small + (1.00E+04&
        *user_crflux)

    !HCCP_DUST -> CCP_DUST + H_DUST
    k(4949) = small + (1.50E+03&
        *user_crflux)

    !HCL_DUST -> H_DUST + CL_DUST
    k(4950) = small + (6.10E+02&
        *user_crflux)

    !HCN_DUST -> CN_DUST + H_DUST
    k(4951) = small + (3.12E+03&
        *user_crflux)

    !HCNC2_DUST -> C2H_DUST + CN_DUST
    k(4952) = small + (3.45E+03&
        *user_crflux)

    !HCO_DUST -> CO_DUST + H_DUST
    k(4953) = small + (4.21E+02&
        *user_crflux)

    !HCOOCH3_DUST -> HCO_DUST + CH2OH_DUST
    k(4954) = small + (1.50E+03&
        *user_crflux)

    !CH2O2_DUST -> HCO_DUST + OH_DUST
    k(4955) = small + (2.49E+02&
        *user_crflux)

    !HCP_DUST -> CP_DUST + H_DUST
    k(4956) = small + (1.50E+03&
        *user_crflux)

    !HCSI_DUST -> CH_DUST + SI_DUST
    k(4957) = small + (1.50E+03&
        *user_crflux)

    !HNC_DUST -> CN_DUST + H_DUST
    k(4958) = small + (3.00E+03&
        *user_crflux)

    !HNC3_DUST -> C2H_DUST + CN_DUST
    k(4959) = small + (3.45E+03&
        *user_crflux)

    !HNCO_DUST -> NH_DUST + CO_DUST
    k(4960) = small + (6.00E+03&
        *user_crflux)

    !HNSI_DUST -> SIN_DUST + H_DUST
    k(4961) = small + (1.50E+03&
        *user_crflux)

    !HPO_DUST -> PO_DUST + H_DUST
    k(4962) = small + (1.50E+03&
        *user_crflux)

    !HS_DUST -> H_DUST + S_DUST
    k(4963) = small + (5.00E+02&
        *user_crflux)

    !HS2_DUST -> HS_DUST + S_DUST
    k(4964) = small + (1.50E+03&
        *user_crflux)

    !MGH_DUST -> MG_DUST + H_DUST
    k(4965) = small + (5.00E+02&
        *user_crflux)

    !MGH2_DUST -> MGH_DUST + H_DUST
    k(4966) = small + (3.00E+03&
        *user_crflux)

    !N2_DUST -> N_DUST + N_DUST
    k(4967) = small + (5.00E+00&
        *user_crflux)

    !N2H2_DUST -> NH2_DUST + N_DUST
    k(4968) = small + (2.00E+02&
        *user_crflux)

    !N2O_DUST -> NO_DUST + N_DUST
    k(4969) = small + (1.50E+03&
        *user_crflux)

    !NAH_DUST -> NA_DUST + H_DUST
    k(4970) = small + (5.00E+02&
        *user_crflux)

    !NAOH_DUST -> NA_DUST + OH_DUST
    k(4971) = small + (1.50E+03&
        *user_crflux)

    !NH_DUST -> N_DUST + H_DUST
    k(4972) = small + (5.00E+02&
        *user_crflux)

    !NH2_DUST -> NH_DUST + H_DUST
    k(4973) = small + (8.00E+01&
        *user_crflux)

    !NH2CHO_DUST -> NH2_DUST + HCO_DUST
    k(4974) = small + (3.00E+03&
        *user_crflux)

    !NH2CN_DUST -> NH2_DUST + CN_DUST
    k(4975) = small + (9.50E+03&
        *user_crflux)

    !NH2OH_DUST -> NH2_DUST + OH_DUST
    k(4976) = small + (3.00E+03&
        *user_crflux)

    !NH3_DUST -> NH2_DUST + H_DUST
    k(4977) = small + (1.32E+03&
        *user_crflux)

    !NH3_DUST -> NH_DUST + H2_DUST
    k(4978) = small + (5.40E+02&
        *user_crflux)

    !NO_DUST -> N_DUST + O_DUST
    k(4979) = small + (4.82E+02&
        *user_crflux)

    !NO2_DUST -> NO_DUST + O_DUST
    k(4980) = small + (1.50E+03&
        *user_crflux)

    !NS_DUST -> N_DUST + S_DUST
    k(4981) = small + (5.00E+02&
        *user_crflux)

    !O2_DUST -> O_DUST + O_DUST
    k(4982) = small + (7.50E+02&
        *user_crflux)

    !O2H_DUST -> O2_DUST + H_DUST
    k(4983) = small + (7.50E+02&
        *user_crflux)

    !O2H_DUST -> O_DUST + OH_DUST
    k(4984) = small + (7.50E+02&
        *user_crflux)

    !O3_DUST -> O2_DUST + O_DUST
    k(4985) = small + (1.50E+03&
        *user_crflux)

    !OCN_DUST -> CN_DUST + O_DUST
    k(4986) = small + (1.50E+03&
        *user_crflux)

    !OCS_DUST -> CO_DUST + S_DUST
    k(4987) = small + (5.35E+03&
        *user_crflux)

    !OH_DUST -> O_DUST + H_DUST
    k(4988) = small + (5.10E+02&
        *user_crflux)

    !PH_DUST -> P_DUST + H_DUST
    k(4989) = small + (5.00E+02&
        *user_crflux)

    !PH2_DUST -> PH_DUST + H_DUST
    k(4990) = small + (1.50E+03&
        *user_crflux)

    !PN_DUST -> P_DUST + N_DUST
    k(4991) = small + (5.00E+02&
        *user_crflux)

    !PO_DUST -> P_DUST + O_DUST
    k(4992) = small + (5.00E+02&
        *user_crflux)

    !S2_DUST -> S_DUST + S_DUST
    k(4993) = small + (5.00E+02&
        *user_crflux)

    !SIC_DUST -> SI_DUST + C_DUST
    k(4994) = small + (5.00E+02&
        *user_crflux)

    !SIC2_DUST -> SIC_DUST + C_DUST
    k(4995) = small + (1.50E+03&
        *user_crflux)

    !SIC2H_DUST -> SIC2_DUST + H_DUST
    k(4996) = small + (1.50E+03&
        *user_crflux)

    !SIC2H2_DUST -> SIC2_DUST + H2_DUST
    k(4997) = small + (1.50E+03&
        *user_crflux)

    !SIC3_DUST -> SIC2_DUST + C_DUST
    k(4998) = small + (1.50E+03&
        *user_crflux)

    !SIC3H_DUST -> SIC3_DUST + H_DUST
    k(4999) = small + (1.50E+03&
        *user_crflux)

    !SIC4_DUST -> SIC2_DUST + C2_DUST
    k(5000) = small + (1.50E+03&
        *user_crflux)

    !SICH2_DUST -> SIC_DUST + H2_DUST
    k(5001) = small + (1.50E+03&
        *user_crflux)

    !SICH3_DUST -> SICH2_DUST + H_DUST
    k(5002) = small + (1.50E+03&
        *user_crflux)

    !SIH_DUST -> SI_DUST + H_DUST
    k(5003) = small + (5.00E+02&
        *user_crflux)

    !SIH2_DUST -> SIH_DUST + H_DUST
    k(5004) = small + (1.50E+03&
        *user_crflux)

    !SIH3_DUST -> SIH2_DUST + H_DUST
    k(5005) = small + (1.50E+03&
        *user_crflux)

    !SIH4_DUST -> SIH2_DUST + H2_DUST
    k(5006) = small + (1.50E+03&
        *user_crflux)

    !SIN_DUST -> SI_DUST + N_DUST
    k(5007) = small + (5.00E+02&
        *user_crflux)

    !SINC_DUST -> SI_DUST + CN_DUST
    k(5008) = small + (1.50E+03&
        *user_crflux)

    !SIO_DUST -> SI_DUST + O_DUST
    k(5009) = small + (5.00E+02&
        *user_crflux)

    !SIO2_DUST -> SIO_DUST + O_DUST
    k(5010) = small + (1.50E+03&
        *user_crflux)

    !SIS_DUST -> SI_DUST + S_DUST
    k(5011) = small + (5.00E+02&
        *user_crflux)

    !SO_DUST -> S_DUST + O_DUST
    k(5012) = small + (5.00E+02&
        *user_crflux)

    !SO2_DUST -> SO_DUST + O_DUST
    k(5013) = small + (1.88E+03&
        *user_crflux)

    !C2H2_DUST -> C2H_DUST + H_DUST
    k(5014) = small + (7.10E+02&
        *user_crflux)

    !C2H2_DUST -> C2_DUST + H_DUST + H_DUST
    k(5015) = small + (4.16E+02&
        *user_crflux)

    !C2H2_DUST -> CH_DUST + CH_DUST
    k(5016) = small + (1.87E+02&
        *user_crflux)

    !C2H4_DUST -> C2H2_DUST + H_DUST + H_DUST
    k(5017) = small + (6.20E+02&
        *user_crflux)

    !C2H4_DUST -> C2H2_DUST + H2_DUST
    k(5018) = small + (5.63E+01&
        *user_crflux)

    !C2H4_DUST -> C2H3_DUST + H_DUST
    k(5019) = small + (1.03E+02&
        *user_crflux)

    !C2H5OH_DUST -> C2H5_DUST + OH_DUST
    k(5020) = small + (6.85E+02&
        *user_crflux)

    !C2H5OH_DUST -> C2H2O_DUST + H2_DUST + H2_DUST
    k(5021) = small + (6.85E+02&
        *user_crflux)

    !C2H5OH_DUST -> CH4_DUST + H2CO_DUST
    k(5022) = small + (6.85E+02&
        *user_crflux)

    !C2H5OH_DUST -> C2H4O_DUST + H2_DUST
    k(5023) = small + (6.85E+02&
        *user_crflux)

    !C3H4_DUST -> C3H2_DUST + H2_DUST
    k(5024) = small + (2.92E+02&
        *user_crflux)

    !C3H4_DUST -> C3H3_DUST + H_DUST
    k(5025) = small + (5.01E+03&
        *user_crflux)

    !C4H2_DUST -> C4_DUST + H2_DUST
    k(5026) = small + (5.60E+02&
        *user_crflux)

    !C4H2_DUST -> C4H_DUST + H_DUST
    k(5027) = small + (5.60E+02&
        *user_crflux)

    !CH2_DUST -> C_DUST + H2_DUST
    k(5028) = small + (6.04E+01&
        *user_crflux)

    !CH2_DUST -> CH_DUST + H_DUST
    k(5029) = small + (1.26E+02&
        *user_crflux)

    !CH2_DUST -> C_DUST + H_DUST + H_DUST
    k(5030) = small + (3.14E+02&
        *user_crflux)

    !C2H2O_DUST -> C2_DUST + H2O_DUST
    k(5031) = small + (4.07E+02&
        *user_crflux)

    !C2H2O_DUST -> CH2_DUST + CO_DUST
    k(5032) = small + (4.07E+02&
        *user_crflux)

    !C2H2O_DUST -> C2H2_DUST + O_DUST
    k(5033) = small + (4.07E+02&
        *user_crflux)

    !CH3_DUST -> C_DUST + H2_DUST + H_DUST
    k(5034) = small + (1.50E+02&
        *user_crflux)

    !CH3_DUST -> CH_DUST + H_DUST + H_DUST
    k(5035) = small + (8.00E+01&
        *user_crflux)

    !CH3_DUST -> CH_DUST + H2_DUST
    k(5036) = small + (7.00E+01&
        *user_crflux)

    !CH3_DUST -> CH2_DUST + H_DUST
    k(5037) = small + (2.00E+02&
        *user_crflux)

    !C2H4O_DUST -> CH3_DUST + HCO_DUST
    k(5038) = small + (3.73E+02&
        *user_crflux)

    !C2H4O_DUST -> C2H2O_DUST + H_DUST + H_DUST
    k(5039) = small + (3.73E+02&
        *user_crflux)

    !C2H4O_DUST -> C2H2O_DUST + H2_DUST
    k(5040) = small + (3.73E+02&
        *user_crflux)

    !C2H3N_DUST -> CH3_DUST + CN_DUST
    k(5041) = small + (4.98E+02&
        *user_crflux)

    !C2H3N_DUST -> C2H2N_DUST + H_DUST
    k(5042) = small + (4.98E+02&
        *user_crflux)

    !C2H3N_DUST -> C2N_DUST + H2_DUST + H_DUST
    k(5043) = small + (4.98E+02&
        *user_crflux)

    !C2H3N_DUST -> CH2_DUST + HCN_DUST
    k(5044) = small + (7.47E+02&
        *user_crflux)

    !CH3OCH3_DUST -> CH4O_DUST + CH2_DUST
    k(5045) = small + (2.80E+02&
        *user_crflux)

    !CH3OCH3_DUST -> CH3_DUST + CH3_DUST + O_DUST
    k(5046) = small + (2.80E+02&
        *user_crflux)

    !CH3OCH3_DUST -> H2CO_DUST + CH4_DUST
    k(5047) = small + (5.60E+02&
        *user_crflux)

    !CH4O_DUST -> H2CO_DUST + H_DUST + H_DUST
    k(5048) = small + (3.30E+01&
        *user_crflux)

    !CH4O_DUST -> HCO_DUST + H_DUST + H_DUST + H_DUST
    k(5049) = small + (3.30E+01&
        *user_crflux)

    !CH4O_DUST -> CO_DUST + H2_DUST + H_DUST + H_DUST
    k(5050) = small + (3.30E+01&
        *user_crflux)

    !CH4O_DUST -> CH3_DUST + OH_DUST
    k(5051) = small + (7.20E+02&
        *user_crflux)

    !CH4O_DUST -> H2CO_DUST + H2_DUST
    k(5052) = small + (7.20E+02&
        *user_crflux)

    !CH5N_DUST -> CH3_DUST + NH2_DUST
    k(5053) = small + (5.06E+02&
        *user_crflux)

    !CH5N_DUST -> CH3N_DUST + H2_DUST
    k(5054) = small + (5.06E+02&
        *user_crflux)

    !CO_DUST -> O_DUST + C_DUST
    k(5055) = small + (3.00E+00&
        *user_crflux)

    !H2S_DUST -> S_DUST + H_DUST + H_DUST
    k(5056) = small + (8.50E+02&
        *user_crflux)

    !H2S_DUST -> HS_DUST + H_DUST
    k(5057) = small + (8.50E+02&
        *user_crflux)

    !HCO_DUST -> CO_DUST + H_DUST
    k(5058) = small + (1.17E+03&
        *user_crflux)

    !CH2O2_DUST -> CO2_DUST + H_DUST + H_DUST
    k(5059) = small + (6.50E+02&
        *user_crflux)

    !HCS_DUST -> CS_DUST + H_DUST
    k(5060) = small + (1.50E+03&
        *user_crflux)

    !HNO_DUST -> NO_DUST + H_DUST
    k(5061) = small + (1.00E+03&
        *user_crflux)

    !NH2_DUST -> NH_DUST + H_DUST
    k(5062) = small + (2.17E+02&
        *user_crflux)

    !NH2_DUST -> N_DUST + H_DUST + H_DUST
    k(5063) = small + (4.33E+02&
        *user_crflux)

    !NH3_DUST -> NH2_DUST + H_DUST
    k(5064) = small + (2.88E+02&
        *user_crflux)

    !NH3_DUST -> NH_DUST + H_DUST + H_DUST
    k(5065) = small + (2.88E+02&
        *user_crflux)

    !NO_DUST -> N_DUST + O_DUST
    k(5066) = small + (4.94E+02&
        *user_crflux)

    !O2_DUST -> O_DUST + O_DUST
    k(5067) = small + (1.17E+02&
        *user_crflux)

    !OCS_DUST -> CS_DUST + O_DUST
    k(5068) = small + (4.80E+02&
        *user_crflux)

    !OCS_DUST -> CO_DUST + S_DUST
    k(5069) = small + (9.60E+02&
        *user_crflux)

    !C10_DUST -> C9_DUST + C_DUST
    k(5070) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2_DUST -> C_DUST + C_DUST
    k(5071) = small + (4.70E-11&
        *exp(-2.60E+00*user_Av))

    !C2H_DUST -> C2_DUST + H_DUST
    k(5072) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H2_DUST -> C2H_DUST + H_DUST
    k(5073) = small + (1.81E-09&
        *exp(-1.72E+00*user_Av))

    !C2H3_DUST -> C2H2_DUST + H_DUST
    k(5074) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H4_DUST -> C2H2_DUST + H2_DUST
    k(5075) = small + (1.62E-09&
        *exp(-1.61E+00*user_Av))

    !C2H5_DUST -> C2H4_DUST + H_DUST
    k(5076) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H5OH_DUST -> C2H4_DUST + H2O_DUST
    k(5077) = small + (1.38E-09&
        *exp(-1.73E+00*user_Av))

    !C2H6_DUST -> C2H4_DUST + H2_DUST
    k(5078) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H6CO_DUST -> C2H2O_DUST + CH4_DUST
    k(5079) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2N_DUST -> C2_DUST + N_DUST
    k(5080) = small + (1.00E-10&
        *exp(-1.70E+00*user_Av))

    !C2N_DUST -> CN_DUST + C_DUST
    k(5081) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CCO_DUST -> CO_DUST + C_DUST
    k(5082) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !CCO_DUST -> C2_DUST + O_DUST
    k(5083) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !C2S_DUST -> C2_DUST + S_DUST
    k(5084) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !C3_DUST -> C2_DUST + C_DUST
    k(5085) = small + (2.60E-10&
        *exp(-2.28E+00*user_Av))

    !C3H_DUST -> C3_DUST + H_DUST
    k(5086) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3H2_DUST -> C3_DUST + H2_DUST
    k(5087) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3H2_DUST -> C3H_DUST + H_DUST
    k(5088) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3H3_DUST -> C3H2_DUST + H_DUST
    k(5089) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3H3_DUST -> C3H_DUST + H2_DUST
    k(5090) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3H3N_DUST -> C2H3_DUST + CN_DUST
    k(5091) = small + (1.00E-10&
        *exp(-1.70E+00*user_Av))

    !C3H4_DUST -> C3H2_DUST + H2_DUST
    k(5092) = small + (2.25E-10&
        *exp(-1.69E+00*user_Av))

    !C3H4_DUST -> C3H3_DUST + H_DUST
    k(5093) = small + (1.84E-09&
        *exp(-1.72E+00*user_Av))

    !C3N_DUST -> C2_DUST + CN_DUST
    k(5094) = small + (5.00E-10&
        *exp(-1.80E+00*user_Av))

    !C3O_DUST -> C2_DUST + CO_DUST
    k(5095) = small + (4.52E-09&
        *exp(-1.58E+00*user_Av))

    !C3P_DUST -> CCP_DUST + C_DUST
    k(5096) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C3S_DUST -> C2_DUST + CS_DUST
    k(5097) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !C4_DUST -> C2_DUST + C2_DUST
    k(5098) = small + (2.00E-10&
        *exp(-2.30E+00*user_Av))

    !C4_DUST -> C3_DUST + C_DUST
    k(5099) = small + (2.00E-10&
        *exp(-2.30E+00*user_Av))

    !C4H_DUST -> C2H_DUST + C2_DUST
    k(5100) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C4H_DUST -> C4_DUST + H_DUST
    k(5101) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C4H2_DUST -> C2H_DUST + C2H_DUST
    k(5102) = small + (1.13E-09&
        *exp(-1.64E+00*user_Av))

    !C4H2_DUST -> C4H_DUST + H_DUST
    k(5103) = small + (1.13E-09&
        *exp(-1.64E+00*user_Av))

    !C4H3_DUST -> C4H2_DUST + H_DUST
    k(5104) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C4H4_DUST -> C3H4_DUST + C_DUST
    k(5105) = small + (1.13E-09&
        *exp(-1.64E+00*user_Av))

    !C4N_DUST -> C3_DUST + CN_DUST
    k(5106) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !C4P_DUST -> C3P_DUST + C_DUST
    k(5107) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C4S_DUST -> C3_DUST + CS_DUST
    k(5108) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !C5_DUST -> C3_DUST + C2_DUST
    k(5109) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H_DUST -> C2H_DUST + C3_DUST
    k(5110) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H_DUST -> C3H_DUST + C2_DUST
    k(5111) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H_DUST -> C5_DUST + H_DUST
    k(5112) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H2_DUST -> C3H_DUST + C2H_DUST
    k(5113) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H2_DUST -> C5H_DUST + H_DUST
    k(5114) = small + (1.00E-11&
        *exp(-1.70E+00*user_Av))

    !C5H3_DUST -> C5H2_DUST + H_DUST
    k(5115) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C5H4_DUST -> C5H2_DUST + H2_DUST
    k(5116) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C5N_DUST -> C4_DUST + CN_DUST
    k(5117) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !C6_DUST -> C5_DUST + C_DUST
    k(5118) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C6H_DUST -> C3H_DUST + C3_DUST
    k(5119) = small + (5.00E-12&
        *exp(-1.70E+00*user_Av))

    !C6H_DUST -> C2H_DUST + C4_DUST
    k(5120) = small + (5.00E-12&
        *exp(-1.70E+00*user_Av))

    !C6H2_DUST -> C6H_DUST + H_DUST
    k(5121) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C6H3_DUST -> C6H2_DUST + H_DUST
    k(5122) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C6H4_DUST -> C6H2_DUST + H2_DUST
    k(5123) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C6H6_DUST -> C6H4_DUST + H2_DUST
    k(5124) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7_DUST -> C6_DUST + C_DUST
    k(5125) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7H_DUST -> C7_DUST + H_DUST
    k(5126) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7H2_DUST -> C7H_DUST + H_DUST
    k(5127) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7H3_DUST -> C7H2_DUST + H_DUST
    k(5128) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7H4_DUST -> C7H2_DUST + H2_DUST
    k(5129) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C7N_DUST -> C6_DUST + CN_DUST
    k(5130) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C8_DUST -> C7_DUST + C_DUST
    k(5131) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C8H_DUST -> C8_DUST + H_DUST
    k(5132) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C8H2_DUST -> C8H_DUST + H_DUST
    k(5133) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C8H3_DUST -> C8H2_DUST + H_DUST
    k(5134) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C8H4_DUST -> C8H2_DUST + H2_DUST
    k(5135) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9_DUST -> C8_DUST + C_DUST
    k(5136) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9H_DUST -> C9_DUST + H_DUST
    k(5137) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9H2_DUST -> C9H_DUST + H_DUST
    k(5138) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9H3_DUST -> C9H2_DUST + H_DUST
    k(5139) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9H4_DUST -> C9H2_DUST + H2_DUST
    k(5140) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C9N_DUST -> C8_DUST + CN_DUST
    k(5141) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CCL_DUST -> CL_DUST + C_DUST
    k(5142) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !CCP_DUST -> CP_DUST + C_DUST
    k(5143) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !CCP_DUST -> C2_DUST + P_DUST
    k(5144) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !CH_DUST -> C_DUST + H_DUST
    k(5145) = small + (1.40E-10&
        *exp(-1.50E+00*user_Av))

    !CH2_DUST -> CH_DUST + H_DUST
    k(5146) = small + (5.00E-11&
        *exp(-1.70E+00*user_Av))

    !C2H2N_DUST -> CH2_DUST + CN_DUST
    k(5147) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H2O_DUST -> CH2_DUST + CO_DUST
    k(5148) = small + (9.04E-10&
        *exp(-1.58E+00*user_Av))

    !CH3N_DUST -> HCN_DUST + H2_DUST
    k(5149) = small + (1.70E-09&
        *exp(-1.63E+00*user_Av))

    !CH2NH2_DUST -> CH2_DUST + NH2_DUST
    k(5150) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH2OH_DUST -> CH2_DUST + OH_DUST
    k(5151) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !CH2PH_DUST -> HCP_DUST + H2_DUST
    k(5152) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH3_DUST -> CH_DUST + H2_DUST
    k(5153) = small + (3.00E-11&
        *exp(-1.70E+00*user_Av))

    !CH3_DUST -> CH2_DUST + H_DUST
    k(5154) = small + (3.00E-11&
        *exp(-1.70E+00*user_Av))

    !CH3C3N_DUST -> C3N_DUST + CH3_DUST
    k(5155) = small + (2.00E-11&
        *exp(-1.70E+00*user_Av))

    !CH3C4H_DUST -> C4H_DUST + CH3_DUST
    k(5156) = small + (2.00E-11&
        *exp(-1.70E+00*user_Av))

    !CH3C5N_DUST -> CH3_DUST + C5N_DUST
    k(5157) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH3C6H_DUST -> CH3_DUST + C6H_DUST
    k(5158) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH3C7N_DUST -> CH3_DUST + C7N_DUST
    k(5159) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !C2H4O_DUST -> CH4_DUST + CO_DUST
    k(5160) = small + (3.43E-10&
        *exp(-1.52E+00*user_Av))

    !C2H4O_DUST -> CH3_DUST + HCO_DUST
    k(5161) = small + (3.43E-10&
        *exp(-1.52E+00*user_Av))

    !C2H3N_DUST -> CH3_DUST + CN_DUST
    k(5162) = small + (1.56E-09&
        *exp(-1.95E+00*user_Av))

    !CH3NH_DUST -> CH3N_DUST + H_DUST
    k(5163) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH3OCH3_DUST -> H2CO_DUST + CH4_DUST
    k(5164) = small + (8.21E-10&
        *exp(-1.60E+00*user_Av))

    !CH4O_DUST -> H2CO_DUST + H2_DUST
    k(5165) = small + (7.19E-10&
        *exp(-1.72E+00*user_Av))

    !CH4_DUST -> CH2_DUST + H2_DUST
    k(5166) = small + (4.80E-10&
        *exp(-2.20E+00*user_Av))

    !CH4_DUST -> CH_DUST + H2_DUST + H_DUST
    k(5167) = small + (1.60E-10&
        *exp(-2.20E+00*user_Av))

    !CH4_DUST -> CH3_DUST + H_DUST
    k(5168) = small + (1.60E-10&
        *exp(-2.20E+00*user_Av))

    !CH5N_DUST -> CH3N_DUST + H_DUST + H_DUST
    k(5169) = small + (6.63E-11&
        *exp(-1.51E+00*user_Av))

    !CH5N_DUST -> CN_DUST + H2_DUST + H2_DUST + H_DUST
    k(5170) = small + (9.42E-11&
        *exp(-1.76E+00*user_Av))

    !CH5N_DUST -> HCN_DUST + H2_DUST + H_DUST + H_DUST
    k(5171) = small + (3.50E-10&
        *exp(-1.73E+00*user_Av))

    !CH5N_DUST -> CH3_DUST + NH2_DUST
    k(5172) = small + (1.55E-10&
        *exp(-1.74E+00*user_Av))

    !CHNH_DUST -> CH_DUST + NH_DUST
    k(5173) = small + (1.00E-10&
        *exp(-1.70E+00*user_Av))

    !CLO_DUST -> CL_DUST + O_DUST
    k(5174) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !CN_DUST -> C_DUST + N_DUST
    k(5175) = small + (1.00E-09&
        *exp(-2.80E+00*user_Av))

    !CO_DUST -> C_DUST + O_DUST
    k(5176) = small + (3.10E-11&
        *exp(-2.54E+00*user_Av))

    !CO2_DUST -> CO_DUST + O_DUST
    k(5177) = small + (3.13E-10&
        *exp(-2.03E+00*user_Av))

    !CP_DUST -> C_DUST + P_DUST
    k(5178) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CS_DUST -> C_DUST + S_DUST
    k(5179) = small + (9.70E-10&
        *exp(-2.00E+00*user_Av))

    !H2C3N_DUST -> C2H2_DUST + CN_DUST
    k(5180) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2C3O_DUST -> C2H2_DUST + CO_DUST
    k(5181) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2C5N_DUST -> C4H2_DUST + CN_DUST
    k(5182) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2C7N_DUST -> C6H2_DUST + CN_DUST
    k(5183) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2C9N_DUST -> C8H2_DUST + CN_DUST
    k(5184) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2CN_DUST -> HCN_DUST + H_DUST
    k(5185) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2CO_DUST -> CO_DUST + H2_DUST
    k(5186) = small + (4.40E-10&
        *exp(-1.60E+00*user_Av))

    !H2CO_DUST -> CO_DUST + H_DUST + H_DUST
    k(5187) = small + (4.40E-10&
        *exp(-1.60E+00*user_Av))

    !H2CS_DUST -> CS_DUST + H2_DUST
    k(5188) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2O_DUST -> OH_DUST + H_DUST
    k(5189) = small + (3.28E-10&
        *exp(-1.63E+00*user_Av))

    !H2O2_DUST -> OH_DUST + OH_DUST
    k(5190) = small + (8.30E-10&
        *exp(-1.82E+00*user_Av))

    !H2S_DUST -> HS_DUST + H_DUST
    k(5191) = small + (3.20E-10&
        *exp(-1.70E+00*user_Av))

    !H2S2_DUST -> HS_DUST + HS_DUST
    k(5192) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H2SIO_DUST -> SIO_DUST + H2_DUST
    k(5193) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H3C5N_DUST -> C2H3_DUST + C3N_DUST
    k(5194) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H3C7N_DUST -> C2H3_DUST + C5N_DUST
    k(5195) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H3C9N_DUST -> C2H3_DUST + C7N_DUST
    k(5196) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H4C3N_DUST -> C2H4_DUST + CN_DUST
    k(5197) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !H5C3N_DUST -> C2H5_DUST + CN_DUST
    k(5198) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HC2NC_DUST -> C2H_DUST + CN_DUST
    k(5199) = small + (9.54E-09&
        *exp(-1.83E+00*user_Av))

    !HC2O_DUST -> CO_DUST + CH_DUST
    k(5200) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HC3N_DUST -> C2H_DUST + CN_DUST
    k(5201) = small + (9.54E-10&
        *exp(-1.83E+00*user_Av))

    !HC3O_DUST -> CO_DUST + C2H_DUST
    k(5202) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HC5N_DUST -> C4H_DUST + CN_DUST
    k(5203) = small + (5.00E-10&
        *exp(-1.83E+00*user_Av))

    !HC5N_DUST -> H_DUST + C5N_DUST
    k(5204) = small + (5.00E-10&
        *exp(-1.83E+00*user_Av))

    !HC7N_DUST -> C6H_DUST + CN_DUST
    k(5205) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HC9N_DUST -> C8H_DUST + CN_DUST
    k(5206) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HCCN_DUST -> C2N_DUST + H_DUST
    k(5207) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HCCP_DUST -> CCP_DUST + H_DUST
    k(5208) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HCL_DUST -> CL_DUST + H_DUST
    k(5209) = small + (1.10E-10&
        *exp(-1.80E+00*user_Av))

    !HCN_DUST -> CN_DUST + H_DUST
    k(5210) = small + (5.48E-10&
        *exp(-2.00E+00*user_Av))

    !HCNC2_DUST -> C2H_DUST + CN_DUST
    k(5211) = small + (9.54E-09&
        *exp(-1.83E+00*user_Av))

    !HCO_DUST -> H_DUST + CO_DUST
    k(5212) = small + (5.87E-10&
        *exp(-5.30E-01*user_Av))

    !HCOOCH3_DUST -> HCO_DUST + CH2OH_DUST
    k(5213) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !CH2O2_DUST -> HCO_DUST + OH_DUST
    k(5214) = small + (2.75E-10&
        *exp(-1.80E+00*user_Av))

    !HCP_DUST -> CP_DUST + H_DUST
    k(5215) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HCSI_DUST -> CH_DUST + SI_DUST
    k(5216) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HNC_DUST -> CN_DUST + H_DUST
    k(5217) = small + (5.48E-10&
        *exp(-2.00E+00*user_Av))

    !HNC3_DUST -> C2H_DUST + CN_DUST
    k(5218) = small + (9.54E-09&
        *exp(-1.83E+00*user_Av))

    !HNCO_DUST -> NH_DUST + CO_DUST
    k(5219) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HNO_DUST -> NO_DUST + H_DUST
    k(5220) = small + (1.70E-10&
        *exp(-5.30E-01*user_Av))

    !HNSI_DUST -> SIN_DUST + H_DUST
    k(5221) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HPO_DUST -> PO_DUST + H_DUST
    k(5222) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !HS_DUST -> H_DUST + S_DUST
    k(5223) = small + (1.00E-11&
        *exp(-2.00E+00*user_Av))

    !HS2_DUST -> HS_DUST + S_DUST
    k(5224) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !MGH_DUST -> MG_DUST + H_DUST
    k(5225) = small + (5.00E-10&
        *exp(-1.45E+00*user_Av))

    !MGH2_DUST -> MGH_DUST + H_DUST
    k(5226) = small + (5.00E-10&
        *exp(-1.45E+00*user_Av))

    !N2_DUST -> N_DUST + N_DUST
    k(5227) = small + (5.00E-12&
        *exp(-3.00E+00*user_Av))

    !N2H2_DUST -> NH2_DUST + N_DUST
    k(5228) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !N2O_DUST -> N2_DUST + O_DUST
    k(5229) = small + (1.40E-09&
        *exp(-1.70E+00*user_Av))

    !NAH_DUST -> NA_DUST + H_DUST
    k(5230) = small + (7.30E-09&
        *exp(-1.13E+00*user_Av))

    !NAOH_DUST -> NA_DUST + OH_DUST
    k(5231) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !NH_DUST -> H_DUST + N_DUST
    k(5232) = small + (4.00E-10&
        *exp(-1.50E+00*user_Av))

    !NH2_DUST -> NH_DUST + H_DUST
    k(5233) = small + (2.11E-10&
        *exp(-1.52E+00*user_Av))

    !NH2CHO_DUST -> NH2_DUST + HCO_DUST
    k(5234) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !NH2CN_DUST -> NH2_DUST + CN_DUST
    k(5235) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !NH2OH_DUST -> NH2_DUST + OH_DUST
    k(5236) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !NH3_DUST -> NH_DUST + H2_DUST
    k(5237) = small + (1.30E-10&
        *exp(-1.91E+00*user_Av))

    !NH3_DUST -> NH2_DUST + H_DUST
    k(5238) = small + (4.94E-10&
        *exp(-1.65E+00*user_Av))

    !NO_DUST -> N_DUST + O_DUST
    k(5239) = small + (3.00E-10&
        *exp(-2.00E+00*user_Av))

    !NO2_DUST -> NO_DUST + O_DUST
    k(5240) = small + (1.29E-09&
        *exp(-2.00E+00*user_Av))

    !NS_DUST -> N_DUST + S_DUST
    k(5241) = small + (1.00E-11&
        *exp(-2.00E+00*user_Av))

    !O2_DUST -> O_DUST + O_DUST
    k(5242) = small + (3.30E-10&
        *exp(-1.40E+00*user_Av))

    !O2H_DUST -> OH_DUST + O_DUST
    k(5243) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !O2H_DUST -> O2_DUST + H_DUST
    k(5244) = small + (5.00E-10&
        *exp(-1.70E+00*user_Av))

    !O3_DUST -> O2_DUST + O_DUST
    k(5245) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !OCN_DUST -> O_DUST + CN_DUST
    k(5246) = small + (1.00E-11&
        *exp(-2.00E+00*user_Av))

    !OCS_DUST -> CO_DUST + S_DUST
    k(5247) = small + (2.28E-09&
        *exp(-1.60E+00*user_Av))

    !OH_DUST -> O_DUST + H_DUST
    k(5248) = small + (1.68E-10&
        *exp(-1.66E+00*user_Av))

    !PH_DUST -> P_DUST + H_DUST
    k(5249) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !PH2_DUST -> PH_DUST + H_DUST
    k(5250) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !PN_DUST -> N_DUST + P_DUST
    k(5251) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !PO_DUST -> O_DUST + P_DUST
    k(5252) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !S2_DUST -> S_DUST + S_DUST
    k(5253) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC_DUST -> SI_DUST + C_DUST
    k(5254) = small + (1.00E-10&
        *exp(-2.30E+00*user_Av))

    !SIC2_DUST -> SIC_DUST + C_DUST
    k(5255) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC2H_DUST -> SIC2_DUST + H_DUST
    k(5256) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC2H2_DUST -> SIC2_DUST + H2_DUST
    k(5257) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC3_DUST -> SIC2_DUST + C_DUST
    k(5258) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC3H_DUST -> SIC3_DUST + H_DUST
    k(5259) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIC4_DUST -> SIC2_DUST + C2_DUST
    k(5260) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SICH2_DUST -> SIC_DUST + H2_DUST
    k(5261) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SICH3_DUST -> SICH2_DUST + H_DUST
    k(5262) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIH_DUST -> SI_DUST + H_DUST
    k(5263) = small + (1.00E-10&
        *exp(-2.30E+00*user_Av))

    !SIH2_DUST -> SIH_DUST + H_DUST
    k(5264) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIH3_DUST -> SIH2_DUST + H_DUST
    k(5265) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIH4_DUST -> SIH2_DUST + H2_DUST
    k(5266) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIN_DUST -> SI_DUST + N_DUST
    k(5267) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SINC_DUST -> CN_DUST + SI_DUST
    k(5268) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIO_DUST -> SI_DUST + O_DUST
    k(5269) = small + (1.00E-10&
        *exp(-2.30E+00*user_Av))

    !SIO2_DUST -> SIO_DUST + O_DUST
    k(5270) = small + (1.00E-09&
        *exp(-1.70E+00*user_Av))

    !SIS_DUST -> SI_DUST + S_DUST
    k(5271) = small + (1.00E-10&
        *exp(-2.30E+00*user_Av))

    !SO_DUST -> O_DUST + S_DUST
    k(5272) = small + (3.30E-10&
        *exp(-1.40E+00*user_Av))

    !SO2_DUST -> SO_DUST + O_DUST
    k(5273) = small + (1.05E-09&
        *exp(-1.74E+00*user_Av))

    !C10_DUST -> C8_DUST + C2_DUST
    k(5274) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C10_DUST -> C9_DUST + C_DUST
    k(5275) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C2_DUST -> C_DUST + C_DUST
    k(5276) = small + (1.00E-10&
        *exp(-2.00E+00*user_Av))

    !C2H_DUST -> C2_DUST + H_DUST
    k(5277) = small + (5.25E-12&
        *exp(-2.00E+00*user_Av))

    !C2H_DUST -> CH_DUST + C_DUST
    k(5278) = small + (4.75E-12&
        *exp(-2.00E+00*user_Av))

    !C2H2_DUST -> C2H_DUST + H_DUST
    k(5279) = small + (1.12E-10&
        *exp(-2.67E+00*user_Av))

    !C2H2_DUST -> C2_DUST + H_DUST + H_DUST
    k(5280) = small + (6.58E-11)

    !C2H2_DUST -> CH_DUST + CH_DUST
    k(5281) = small + (2.90E-11)

    !C2H3_DUST -> C2H_DUST + H2_DUST
    k(5282) = small + (1.91E-11&
        *exp(-2.30E+00*user_Av))

    !C2H3_DUST -> C2H2_DUST + H_DUST
    k(5283) = small + (9.26E-11&
        *exp(-2.30E+00*user_Av))

    !C2H3_DUST -> C2H_DUST + H_DUST + H_DUST
    k(5284) = small + (1.88E-10&
        *exp(-2.30E+00*user_Av))

    !C2H4_DUST -> C2H2_DUST + H_DUST + H_DUST
    k(5285) = small + (9.39E-11&
        *exp(-2.66E+00*user_Av))

    !C2H4_DUST -> C2H2_DUST + H2_DUST
    k(5286) = small + (8.52E-12&
        *exp(-2.66E+00*user_Av))

    !C2H4_DUST -> C2H3_DUST + H_DUST
    k(5287) = small + (1.56E-11&
        *exp(-2.66E+00*user_Av))

    !C2H5_DUST -> C2H4_DUST + H_DUST
    k(5288) = small + (4.00E-11&
        *exp(-2.50E+00*user_Av))

    !C2H5_DUST -> C2H_DUST + H2_DUST + H2_DUST
    k(5289) = small + (4.00E-11&
        *exp(-2.50E+00*user_Av))

    !C2H5_DUST -> C2H2_DUST + H2_DUST + H_DUST
    k(5290) = small + (8.00E-11&
        *exp(-2.50E+00*user_Av))

    !C2H5_DUST -> C2H3_DUST + H2_DUST
    k(5291) = small + (4.00E-11&
        *exp(-2.50E+00*user_Av))

    !C2H5OH_DUST -> C2H5_DUST + OH_DUST
    k(5292) = small + (1.32E-10&
        *exp(-2.35E+00*user_Av))

    !C2H5OH_DUST -> C2H2O_DUST + H2_DUST + H2_DUST
    k(5293) = small + (1.32E-10&
        *exp(-2.35E+00*user_Av))

    !C2H5OH_DUST -> CH4_DUST + H2CO_DUST
    k(5294) = small + (1.32E-10&
        *exp(-2.35E+00*user_Av))

    !C2H5OH_DUST -> C2H4O_DUST + H2_DUST
    k(5295) = small + (1.32E-10&
        *exp(-2.35E+00*user_Av))

    !C2H6CO_DUST -> C2H4O_DUST + CH2_DUST
    k(5296) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C2H6CO_DUST -> CH3_DUST + CH3_DUST + CO_DUST
    k(5297) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CCO_DUST -> CO_DUST + C_DUST
    k(5298) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !C3H3N_DUST -> C3N_DUST + H2_DUST + H_DUST
    k(5299) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C3H3N_DUST -> HC3N_DUST + H2_DUST
    k(5300) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C3H4_DUST -> C3H2_DUST + H2_DUST
    k(5301) = small + (5.45E-11&
        *exp(-2.37E+00*user_Av))

    !C3H4_DUST -> C3H3_DUST + H_DUST
    k(5302) = small + (9.34E-10&
        *exp(-2.37E+00*user_Av))

    !C4H2_DUST -> C4_DUST + H2_DUST
    k(5303) = small + (1.30E-10&
        *exp(-2.28E+00*user_Av))

    !C4H2_DUST -> C4H_DUST + H_DUST
    k(5304) = small + (1.30E-10&
        *exp(-2.28E+00*user_Av))

    !C4P_DUST -> C3P_DUST + C_DUST
    k(5305) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C4P_DUST -> CCP_DUST + C2_DUST
    k(5306) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C6_DUST -> C5_DUST + C_DUST
    k(5307) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C6_DUST -> C4_DUST + C2_DUST
    k(5308) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C6H2_DUST -> C6_DUST + H2_DUST
    k(5309) = small + (9.30E-11&
        *exp(-2.50E+00*user_Av))

    !C6H2_DUST -> C6H_DUST + H_DUST
    k(5310) = small + (9.30E-11&
        *exp(-2.50E+00*user_Av))

    !C6H2_DUST -> C6_DUST + H_DUST + H_DUST
    k(5311) = small + (1.40E-11&
        *exp(-2.50E+00*user_Av))

    !C7_DUST -> C6_DUST + C_DUST
    k(5312) = small + (8.70E-11&
        *exp(-2.50E+00*user_Av))

    !C7_DUST -> C5_DUST + C2_DUST
    k(5313) = small + (8.70E-11&
        *exp(-2.50E+00*user_Av))

    !C7_DUST -> C4_DUST + C3_DUST
    k(5314) = small + (2.61E-11&
        *exp(-2.50E+00*user_Av))

    !C7H_DUST -> C7_DUST + H_DUST
    k(5315) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C7H_DUST -> C6H_DUST + C_DUST
    k(5316) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C7H2_DUST -> C7H_DUST + H_DUST
    k(5317) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C7H2_DUST -> C7_DUST + H2_DUST
    k(5318) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8_DUST -> C7_DUST + C_DUST
    k(5319) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8_DUST -> C6_DUST + C2_DUST
    k(5320) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8H_DUST -> C8_DUST + H_DUST
    k(5321) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8H_DUST -> C7H_DUST + C_DUST
    k(5322) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8H2_DUST -> C8_DUST + H2_DUST
    k(5323) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C8H2_DUST -> C8H_DUST + H_DUST
    k(5324) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9_DUST -> C8_DUST + C_DUST
    k(5325) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9_DUST -> C7_DUST + C2_DUST
    k(5326) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9H_DUST -> C8H_DUST + C_DUST
    k(5327) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9H_DUST -> C9_DUST + H_DUST
    k(5328) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9H2_DUST -> C9H_DUST + H_DUST
    k(5329) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9H2_DUST -> C9_DUST + H2_DUST
    k(5330) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C9N_DUST -> C2_DUST + C7N_DUST
    k(5331) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !CCP_DUST -> CP_DUST + C_DUST
    k(5332) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CCP_DUST -> C2_DUST + P_DUST
    k(5333) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CH_DUST -> C_DUST + H_DUST
    k(5334) = small + (2.90E-10&
        *exp(-2.80E+00*user_Av))

    !CH2_DUST -> C_DUST + H2_DUST
    k(5335) = small + (1.21E-10&
        *exp(-2.30E+00*user_Av))

    !CH2_DUST -> CH_DUST + H_DUST
    k(5336) = small + (2.51E-10&
        *exp(-2.30E+00*user_Av))

    !CH2_DUST -> C_DUST + H_DUST + H_DUST
    k(5337) = small + (6.28E-10&
        *exp(-2.30E+00*user_Av))

    !C2H2N_DUST -> CH_DUST + HCN_DUST
    k(5338) = small + (6.67E-11&
        *exp(-2.50E+00*user_Av))

    !C2H2N_DUST -> CN_DUST + CH2_DUST
    k(5339) = small + (6.67E-11&
        *exp(-2.50E+00*user_Av))

    !C2H2N_DUST -> C2N_DUST + H2_DUST
    k(5340) = small + (6.67E-11&
        *exp(-2.50E+00*user_Av))

    !C2H2O_DUST -> C2_DUST + H2O_DUST
    k(5341) = small + (1.15E-10&
        *exp(-2.01E+00*user_Av))

    !C2H2O_DUST -> CH2_DUST + CO_DUST
    k(5342) = small + (1.15E-10&
        *exp(-2.01E+00*user_Av))

    !C2H2O_DUST -> C2H2_DUST + O_DUST
    k(5343) = small + (1.15E-10&
        *exp(-2.01E+00*user_Av))

    !CH2PH_DUST -> CP_DUST + H2_DUST + H_DUST
    k(5344) = small + (5.00E-11&
        *exp(-2.50E+00*user_Av))

    !CH2PH_DUST -> HCP_DUST + H2_DUST
    k(5345) = small + (5.00E-11&
        *exp(-2.50E+00*user_Av))

    !CH2PH_DUST -> CH3_DUST + P_DUST
    k(5346) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CH3_DUST -> H2_DUST + C_DUST + H_DUST
    k(5347) = small + (3.00E-11&
        *exp(-2.10E+00*user_Av))

    !CH3_DUST -> CH_DUST + H_DUST + H_DUST
    k(5348) = small + (1.60E-11&
        *exp(-2.10E+00*user_Av))

    !CH3_DUST -> CH_DUST + H2_DUST
    k(5349) = small + (1.40E-11&
        *exp(-2.10E+00*user_Av))

    !CH3_DUST -> CH2_DUST + H_DUST
    k(5350) = small + (4.00E-11&
        *exp(-2.10E+00*user_Av))

    !CH3C6H_DUST -> C7H2_DUST + H2_DUST
    k(5351) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CH3C6H_DUST -> C7H_DUST + H2_DUST + H_DUST
    k(5352) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !C2H4O_DUST -> CH3_DUST + HCO_DUST
    k(5353) = small + (8.67E-11&
        *exp(-2.28E+00*user_Av))

    !C2H4O_DUST -> C2H2O_DUST + H_DUST + H_DUST
    k(5354) = small + (8.67E-11&
        *exp(-2.28E+00*user_Av))

    !C2H4O_DUST -> C2H2O_DUST + H2_DUST
    k(5355) = small + (8.67E-11&
        *exp(-2.28E+00*user_Av))

    !C2H3N_DUST -> CH3_DUST + CN_DUST
    k(5356) = small + (1.18E-10&
        *exp(-3.11E+00*user_Av))

    !C2H3N_DUST -> C2H2N_DUST + H_DUST
    k(5357) = small + (1.18E-10&
        *exp(-3.11E+00*user_Av))

    !C2H3N_DUST -> C2N_DUST + H2_DUST + H_DUST
    k(5358) = small + (1.18E-10&
        *exp(-3.11E+00*user_Av))

    !C2H3N_DUST -> CH2_DUST + HCN_DUST
    k(5359) = small + (1.76E-10&
        *exp(-3.11E+00*user_Av))

    !CH3OCH3_DUST -> CH4O_DUST + CH2_DUST
    k(5360) = small + (6.50E-11&
        *exp(-2.28E+00*user_Av))

    !CH3OCH3_DUST -> CH3_DUST + CH3_DUST + O_DUST
    k(5361) = small + (6.50E-11&
        *exp(-2.28E+00*user_Av))

    !CH3OCH3_DUST -> H2CO_DUST + CH4_DUST
    k(5362) = small + (1.30E-10&
        *exp(-2.28E+00*user_Av))

    !CH4O_DUST -> CH3_DUST + OH_DUST
    k(5363) = small + (2.40E-10&
        *exp(-2.57E+00*user_Av))

    !CH4O_DUST -> H2CO_DUST + H2_DUST
    k(5364) = small + (2.40E-10&
        *exp(-2.57E+00*user_Av))

    !CH5N_DUST -> CH3_DUST + NH2_DUST
    k(5365) = small + (1.30E-10&
        *exp(-2.28E+00*user_Av))

    !CH5N_DUST -> CH3N_DUST + H2_DUST
    k(5366) = small + (1.30E-10&
        *exp(-2.28E+00*user_Av))

    !CP_DUST -> C_DUST + P_DUST
    k(5367) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !H2CN_DUST -> CN_DUST + H_DUST + H_DUST
    k(5368) = small + (3.98E-11&
        *exp(-2.50E+00*user_Av))

    !H2CN_DUST -> HCN_DUST + H_DUST
    k(5369) = small + (8.01E-11&
        *exp(-2.50E+00*user_Av))

    !H2CN_DUST -> HNC_DUST + H_DUST
    k(5370) = small + (8.01E-11&
        *exp(-2.50E+00*user_Av))

    !H2CO_DUST -> HCO_DUST + H_DUST
    k(5371) = small + (1.33E-11&
        *exp(-2.80E+00*user_Av))

    !H2CO_DUST -> CO_DUST + H_DUST + H_DUST
    k(5372) = small + (6.67E-11&
        *exp(-2.80E+00*user_Av))

    !H2CO_DUST -> CO_DUST + H_DUST + H_DUST
    k(5373) = small + (1.40E-11&
        *exp(-3.10E+00*user_Av))

    !H2O_DUST -> H2_DUST + O_DUST
    k(5374) = small + (1.90E-12&
        *exp(-3.10E+00*user_Av))

    !H2O_DUST -> OH_DUST + H_DUST
    k(5375) = small + (4.20E-12&
        *exp(-3.10E+00*user_Av))

    !H2O_DUST -> O_DUST + H_DUST + H_DUST
    k(5376) = small + (1.49E-11&
        *exp(-3.10E+00*user_Av))

    !H2S2_DUST -> HS_DUST + HS_DUST
    k(5377) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !H2S2_DUST -> HS2_DUST + H_DUST
    k(5378) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !H2SIO_DUST -> SIH_DUST + OH_DUST
    k(5379) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !H2SIO_DUST -> SIO_DUST + H2_DUST
    k(5380) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HC7N_DUST -> C7N_DUST + H_DUST
    k(5381) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HC7N_DUST -> CN_DUST + C6H_DUST
    k(5382) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HC9N_DUST -> C9N_DUST + H_DUST
    k(5383) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HC9N_DUST -> CN_DUST + C8H_DUST
    k(5384) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCCP_DUST -> CCP_DUST + H_DUST
    k(5385) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCCP_DUST -> CP_DUST + CH_DUST
    k(5386) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCO_DUST -> CO_DUST + H_DUST
    k(5387) = small + (2.46E-10&
        *exp(-2.11E+00*user_Av))

    !HCOOCH3_DUST -> CH4O_DUST + CO_DUST
    k(5388) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCOOCH3_DUST -> CH3_DUST + CO2_DUST + H_DUST
    k(5389) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !CH2O2_DUST -> CO2_DUST + H_DUST + H_DUST
    k(5390) = small + (1.73E-10&
        *exp(-2.59E+00*user_Av))

    !HCP_DUST -> CH_DUST + P_DUST
    k(5391) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCP_DUST -> CP_DUST + H_DUST
    k(5392) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCS_DUST -> CS_DUST + H_DUST
    k(5393) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCSI_DUST -> SI_DUST + CH_DUST
    k(5394) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HCSI_DUST -> SIC_DUST + H_DUST
    k(5395) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HNSI_DUST -> SIN_DUST + H_DUST
    k(5396) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HNSI_DUST -> NH_DUST + SI_DUST
    k(5397) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HPO_DUST -> PO_DUST + H_DUST
    k(5398) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HPO_DUST -> PH_DUST + O_DUST
    k(5399) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HS2_DUST -> HS_DUST + S_DUST
    k(5400) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !HS2_DUST -> S2_DUST + H_DUST
    k(5401) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !NH_DUST -> N_DUST + H_DUST
    k(5402) = small + (1.00E-11&
        *exp(-2.00E+00*user_Av))

    !NH2_DUST -> NH_DUST + H_DUST
    k(5403) = small + (5.77E-11&
        *exp(-2.59E+00*user_Av))

    !NH2_DUST -> N_DUST + H_DUST + H_DUST
    k(5404) = small + (1.15E-10&
        *exp(-2.59E+00*user_Av))

    !NH3_DUST -> NH2_DUST + H_DUST
    k(5405) = small + (6.20E-11&
        *exp(-2.47E+00*user_Av))

    !NH3_DUST -> NH_DUST + H_DUST + H_DUST
    k(5406) = small + (6.20E-11&
        *exp(-2.47E+00*user_Av))

    !NO_DUST -> N_DUST + O_DUST
    k(5407) = small + (2.00E-10&
        *exp(-2.00E+00*user_Av))

    !O2_DUST -> O_DUST + O_DUST
    k(5408) = small + (6.20E-12&
        *exp(-3.10E+00*user_Av))

    !O2H_DUST -> O2_DUST + H_DUST
    k(5409) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !OCS_DUST -> CS_DUST + O_DUST
    k(5410) = small + (7.90E-11&
        *exp(-2.71E+00*user_Av))

    !OCS_DUST -> CO_DUST + S_DUST
    k(5411) = small + (1.58E-10&
        *exp(-2.71E+00*user_Av))

    !OH_DUST -> O_DUST + H_DUST
    k(5412) = small + (1.60E-12&
        *exp(-3.10E+00*user_Av))

    !PH_DUST -> P_DUST + H_DUST
    k(5413) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !PH2_DUST -> P_DUST + H2_DUST
    k(5414) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !PH2_DUST -> PH_DUST + H_DUST
    k(5415) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !PN_DUST -> P_DUST + N_DUST
    k(5416) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !PO_DUST -> P_DUST + O_DUST
    k(5417) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !S2_DUST -> S_DUST + S_DUST
    k(5418) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2_DUST -> SI_DUST + C2_DUST
    k(5419) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2_DUST -> SIC_DUST + C_DUST
    k(5420) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2H_DUST -> C2H_DUST + SI_DUST
    k(5421) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2H_DUST -> SIC2_DUST + H_DUST
    k(5422) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2H2_DUST -> SIC2_DUST + H2_DUST
    k(5423) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC2H2_DUST -> SIC2H_DUST + H_DUST
    k(5424) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC3_DUST -> SIC_DUST + C2_DUST
    k(5425) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC3_DUST -> SIC2_DUST + C_DUST
    k(5426) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC3H_DUST -> SI_DUST + C3H_DUST
    k(5427) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC3H_DUST -> SIC3_DUST + H_DUST
    k(5428) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC4_DUST -> SIC2_DUST + C2_DUST
    k(5429) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIC4_DUST -> SIC3_DUST + C_DUST
    k(5430) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SICH2_DUST -> SI_DUST + CH2_DUST
    k(5431) = small + (7.27E-11&
        *exp(-2.50E+00*user_Av))

    !SICH2_DUST -> SIC_DUST + H2_DUST
    k(5432) = small + (7.27E-11&
        *exp(-2.50E+00*user_Av))

    !SICH2_DUST -> HCSI_DUST + H_DUST
    k(5433) = small + (5.45E-11&
        *exp(-2.50E+00*user_Av))

    !SICH3_DUST -> HCSI_DUST + H2_DUST
    k(5434) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SICH3_DUST -> SICH2_DUST + H_DUST
    k(5435) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIH2_DUST -> SI_DUST + H2_DUST
    k(5436) = small + (5.45E-11&
        *exp(-2.50E+00*user_Av))

    !SIH2_DUST -> SIH_DUST + H_DUST
    k(5437) = small + (7.27E-11&
        *exp(-2.50E+00*user_Av))

    !SIH2_DUST -> SI_DUST + H_DUST + H_DUST
    k(5438) = small + (7.27E-11&
        *exp(-2.50E+00*user_Av))

    !SIH3_DUST -> SIH_DUST + H2_DUST
    k(5439) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIH3_DUST -> SIH2_DUST + H_DUST
    k(5440) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIH4_DUST -> SIH2_DUST + H2_DUST
    k(5441) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIH4_DUST -> SIH3_DUST + H_DUST
    k(5442) = small + (1.00E-10&
        *exp(-2.50E+00*user_Av))

    !SIN_DUST -> SI_DUST + N_DUST
    k(5443) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !SINC_DUST -> SI_DUST + CN_DUST
    k(5444) = small + (2.00E-10&
        *exp(-2.50E+00*user_Av))

    !C -> C_DUST
    k(5445) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C),ads_stick,user_gsize2,sqrTgas))

    !C10 -> C10_DUST
    k(5446) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C10),ads_stick,user_gsize2,sqrTgas))

    !C2 -> C2_DUST
    k(5447) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2),ads_stick,user_gsize2,sqrTgas))

    !C2H -> C2H_DUST
    k(5448) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H),ads_stick,user_gsize2,sqrTgas))

    !C2H2 -> C2H2_DUST
    k(5449) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H2),ads_stick,user_gsize2,sqrTgas))

    !C2H3 -> C2H3_DUST
    k(5450) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H3),ads_stick,user_gsize2,sqrTgas))

    !C2H4 -> C2H4_DUST
    k(5451) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H4),ads_stick,user_gsize2,sqrTgas))

    !C2H5 -> C2H5_DUST
    k(5452) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H5),ads_stick,user_gsize2,sqrTgas))

    !C2H5OH -> C2H5OH_DUST
    k(5453) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H5OH),ads_stick,user_gsize2,sqrTgas))

    !C2H6 -> C2H6_DUST
    k(5454) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H6),ads_stick,user_gsize2,sqrTgas))

    !C2H6CO -> C2H6CO_DUST
    k(5455) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H6CO),ads_stick,user_gsize2,sqrTgas))

    !C2N -> C2N_DUST
    k(5456) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2N),ads_stick,user_gsize2,sqrTgas))

    !CCO -> CCO_DUST
    k(5457) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CCO),ads_stick,user_gsize2,sqrTgas))

    !C2S -> C2S_DUST
    k(5458) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2S),ads_stick,user_gsize2,sqrTgas))

    !C3 -> C3_DUST
    k(5459) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3),ads_stick,user_gsize2,sqrTgas))

    !C3H -> C3H_DUST
    k(5460) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3H),ads_stick,user_gsize2,sqrTgas))

    !C3H2 -> C3H2_DUST
    k(5461) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3H2),ads_stick,user_gsize2,sqrTgas))

    !C3H3 -> C3H3_DUST
    k(5462) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3H3),ads_stick,user_gsize2,sqrTgas))

    !C3H3N -> C3H3N_DUST
    k(5463) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3H3N),ads_stick,user_gsize2,sqrTgas))

    !C3H4 -> C3H4_DUST
    k(5464) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3H4),ads_stick,user_gsize2,sqrTgas))

    !C3N -> C3N_DUST
    k(5465) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3N),ads_stick,user_gsize2,sqrTgas))

    !C3O -> C3O_DUST
    k(5466) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3O),ads_stick,user_gsize2,sqrTgas))

    !C3P -> C3P_DUST
    k(5467) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3P),ads_stick,user_gsize2,sqrTgas))

    !C3S -> C3S_DUST
    k(5468) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C3S),ads_stick,user_gsize2,sqrTgas))

    !C4 -> C4_DUST
    k(5469) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4),ads_stick,user_gsize2,sqrTgas))

    !C4H -> C4H_DUST
    k(5470) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4H),ads_stick,user_gsize2,sqrTgas))

    !C4H2 -> C4H2_DUST
    k(5471) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4H2),ads_stick,user_gsize2,sqrTgas))

    !C4H3 -> C4H3_DUST
    k(5472) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4H3),ads_stick,user_gsize2,sqrTgas))

    !C4H4 -> C4H4_DUST
    k(5473) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4H4),ads_stick,user_gsize2,sqrTgas))

    !C4N -> C4N_DUST
    k(5474) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4N),ads_stick,user_gsize2,sqrTgas))

    !C4P -> C4P_DUST
    k(5475) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4P),ads_stick,user_gsize2,sqrTgas))

    !C4S -> C4S_DUST
    k(5476) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C4S),ads_stick,user_gsize2,sqrTgas))

    !C5 -> C5_DUST
    k(5477) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5),ads_stick,user_gsize2,sqrTgas))

    !C5H -> C5H_DUST
    k(5478) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5H),ads_stick,user_gsize2,sqrTgas))

    !C5H2 -> C5H2_DUST
    k(5479) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5H2),ads_stick,user_gsize2,sqrTgas))

    !C5H3 -> C5H3_DUST
    k(5480) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5H3),ads_stick,user_gsize2,sqrTgas))

    !C5H4 -> C5H4_DUST
    k(5481) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5H4),ads_stick,user_gsize2,sqrTgas))

    !C5N -> C5N_DUST
    k(5482) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C5N),ads_stick,user_gsize2,sqrTgas))

    !C6 -> C6_DUST
    k(5483) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6),ads_stick,user_gsize2,sqrTgas))

    !C6H -> C6H_DUST
    k(5484) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6H),ads_stick,user_gsize2,sqrTgas))

    !C6H2 -> C6H2_DUST
    k(5485) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6H2),ads_stick,user_gsize2,sqrTgas))

    !C6H3 -> C6H3_DUST
    k(5486) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6H3),ads_stick,user_gsize2,sqrTgas))

    !C6H4 -> C6H4_DUST
    k(5487) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6H4),ads_stick,user_gsize2,sqrTgas))

    !C6H6 -> C6H6_DUST
    k(5488) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C6H6),ads_stick,user_gsize2,sqrTgas))

    !C7 -> C7_DUST
    k(5489) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7),ads_stick,user_gsize2,sqrTgas))

    !C7H -> C7H_DUST
    k(5490) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7H),ads_stick,user_gsize2,sqrTgas))

    !C7H2 -> C7H2_DUST
    k(5491) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7H2),ads_stick,user_gsize2,sqrTgas))

    !C7H3 -> C7H3_DUST
    k(5492) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7H3),ads_stick,user_gsize2,sqrTgas))

    !C7H4 -> C7H4_DUST
    k(5493) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7H4),ads_stick,user_gsize2,sqrTgas))

    !C7N -> C7N_DUST
    k(5494) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C7N),ads_stick,user_gsize2,sqrTgas))

    !C8 -> C8_DUST
    k(5495) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C8),ads_stick,user_gsize2,sqrTgas))

    !C8H -> C8H_DUST
    k(5496) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C8H),ads_stick,user_gsize2,sqrTgas))

    !C8H2 -> C8H2_DUST
    k(5497) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C8H2),ads_stick,user_gsize2,sqrTgas))

    !C8H3 -> C8H3_DUST
    k(5498) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C8H3),ads_stick,user_gsize2,sqrTgas))

    !C8H4 -> C8H4_DUST
    k(5499) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C8H4),ads_stick,user_gsize2,sqrTgas))

    !C9 -> C9_DUST
    k(5500) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9),ads_stick,user_gsize2,sqrTgas))

    !C9H -> C9H_DUST
    k(5501) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9H),ads_stick,user_gsize2,sqrTgas))

    !C9H2 -> C9H2_DUST
    k(5502) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9H2),ads_stick,user_gsize2,sqrTgas))

    !C9H3 -> C9H3_DUST
    k(5503) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9H3),ads_stick,user_gsize2,sqrTgas))

    !C9H4 -> C9H4_DUST
    k(5504) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9H4),ads_stick,user_gsize2,sqrTgas))

    !C9N -> C9N_DUST
    k(5505) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C9N),ads_stick,user_gsize2,sqrTgas))

    !CCL -> CCL_DUST
    k(5506) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CCL),ads_stick,user_gsize2,sqrTgas))

    !CCP -> CCP_DUST
    k(5507) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CCP),ads_stick,user_gsize2,sqrTgas))

    !CH -> CH_DUST
    k(5508) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH),ads_stick,user_gsize2,sqrTgas))

    !CH2 -> CH2_DUST
    k(5509) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH2),ads_stick,user_gsize2,sqrTgas))

    !C2H2N -> C2H2N_DUST
    k(5510) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H2N),ads_stick,user_gsize2,sqrTgas))

    !C2H2O -> C2H2O_DUST
    k(5511) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H2O),ads_stick,user_gsize2,sqrTgas))

    !CH3N -> CH3N_DUST
    k(5512) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3N),ads_stick,user_gsize2,sqrTgas))

    !CH2NH2 -> CH2NH2_DUST
    k(5513) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH2NH2),ads_stick,user_gsize2,sqrTgas))

    !CH2OH -> CH2OH_DUST
    k(5514) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH2OH),ads_stick,user_gsize2,sqrTgas))

    !CH2PH -> CH2PH_DUST
    k(5515) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH2PH),ads_stick,user_gsize2,sqrTgas))

    !CH3 -> CH3_DUST
    k(5516) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3),ads_stick,user_gsize2,sqrTgas))

    !CH3C3N -> CH3C3N_DUST
    k(5517) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3C3N),ads_stick,user_gsize2,sqrTgas))

    !CH3C4H -> CH3C4H_DUST
    k(5518) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3C4H),ads_stick,user_gsize2,sqrTgas))

    !CH3C5N -> CH3C5N_DUST
    k(5519) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3C5N),ads_stick,user_gsize2,sqrTgas))

    !CH3C6H -> CH3C6H_DUST
    k(5520) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3C6H),ads_stick,user_gsize2,sqrTgas))

    !CH3C7N -> CH3C7N_DUST
    k(5521) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3C7N),ads_stick,user_gsize2,sqrTgas))

    !C2H4O -> C2H4O_DUST
    k(5522) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H4O),ads_stick,user_gsize2,sqrTgas))

    !C2H3N -> C2H3N_DUST
    k(5523) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_C2H3N),ads_stick,user_gsize2,sqrTgas))

    !CH3NH -> CH3NH_DUST
    k(5524) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3NH),ads_stick,user_gsize2,sqrTgas))

    !CH3OCH3 -> CH3OCH3_DUST
    k(5525) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH3OCH3),ads_stick,user_gsize2,sqrTgas))

    !CH4O -> CH4O_DUST
    k(5526) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH4O),ads_stick,user_gsize2,sqrTgas))

    !CH4 -> CH4_DUST
    k(5527) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH4),ads_stick,user_gsize2,sqrTgas))

    !CH5N -> CH5N_DUST
    k(5528) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH5N),ads_stick,user_gsize2,sqrTgas))

    !CHNH -> CHNH_DUST
    k(5529) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CHNH),ads_stick,user_gsize2,sqrTgas))

    !CL -> CL_DUST
    k(5530) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CL),ads_stick,user_gsize2,sqrTgas))

    !CLO -> CLO_DUST
    k(5531) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CLO),ads_stick,user_gsize2,sqrTgas))

    !CN -> CN_DUST
    k(5532) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CN),ads_stick,user_gsize2,sqrTgas))

    !CO -> CO_DUST
    k(5533) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CO),ads_stick,user_gsize2,sqrTgas))

    !CO2 -> CO2_DUST
    k(5534) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CO2),ads_stick,user_gsize2,sqrTgas))

    !CP -> CP_DUST
    k(5535) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CP),ads_stick,user_gsize2,sqrTgas))

    !CS -> CS_DUST
    k(5536) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CS),ads_stick,user_gsize2,sqrTgas))

    !FE -> FE_DUST
    k(5537) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_FE),ads_stick,user_gsize2,sqrTgas))

    !FEH -> FEH_DUST
    k(5538) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_FEH),ads_stick,user_gsize2,sqrTgas))

    !H -> H_DUST
    k(5539) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H),ads_stick,user_gsize2,sqrTgas))

    !H2 -> H2_DUST
    k(5540) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2),ads_stick,user_gsize2,sqrTgas))

    !H2C3N -> H2C3N_DUST
    k(5541) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2C3N),ads_stick,user_gsize2,sqrTgas))

    !H2C3O -> H2C3O_DUST
    k(5542) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2C3O),ads_stick,user_gsize2,sqrTgas))

    !H2C5N -> H2C5N_DUST
    k(5543) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2C5N),ads_stick,user_gsize2,sqrTgas))

    !H2C7N -> H2C7N_DUST
    k(5544) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2C7N),ads_stick,user_gsize2,sqrTgas))

    !H2C9N -> H2C9N_DUST
    k(5545) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2C9N),ads_stick,user_gsize2,sqrTgas))

    !H2CN -> H2CN_DUST
    k(5546) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2CN),ads_stick,user_gsize2,sqrTgas))

    !H2CO -> H2CO_DUST
    k(5547) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2CO),ads_stick,user_gsize2,sqrTgas))

    !H2CS -> H2CS_DUST
    k(5548) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2CS),ads_stick,user_gsize2,sqrTgas))

    !H2O -> H2O_DUST
    k(5549) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2O),ads_stick,user_gsize2,sqrTgas))

    !H2O2 -> H2O2_DUST
    k(5550) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2O2),ads_stick,user_gsize2,sqrTgas))

    !H2S -> H2S_DUST
    k(5551) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2S),ads_stick,user_gsize2,sqrTgas))

    !H2S2 -> H2S2_DUST
    k(5552) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2S2),ads_stick,user_gsize2,sqrTgas))

    !H2SIO -> H2SIO_DUST
    k(5553) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H2SIO),ads_stick,user_gsize2,sqrTgas))

    !H3C5N -> H3C5N_DUST
    k(5554) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H3C5N),ads_stick,user_gsize2,sqrTgas))

    !H3C7N -> H3C7N_DUST
    k(5555) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H3C7N),ads_stick,user_gsize2,sqrTgas))

    !H3C9N -> H3C9N_DUST
    k(5556) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H3C9N),ads_stick,user_gsize2,sqrTgas))

    !H4C3N -> H4C3N_DUST
    k(5557) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H4C3N),ads_stick,user_gsize2,sqrTgas))

    !H5C3N -> H5C3N_DUST
    k(5558) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_H5C3N),ads_stick,user_gsize2,sqrTgas))

    !HC2NC -> HC2NC_DUST
    k(5559) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC2NC),ads_stick,user_gsize2,sqrTgas))

    !HC2O -> HC2O_DUST
    k(5560) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC2O),ads_stick,user_gsize2,sqrTgas))

    !HC3N -> HC3N_DUST
    k(5561) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC3N),ads_stick,user_gsize2,sqrTgas))

    !HC3O -> HC3O_DUST
    k(5562) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC3O),ads_stick,user_gsize2,sqrTgas))

    !HC5N -> HC5N_DUST
    k(5563) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC5N),ads_stick,user_gsize2,sqrTgas))

    !HC7N -> HC7N_DUST
    k(5564) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC7N),ads_stick,user_gsize2,sqrTgas))

    !HC9N -> HC9N_DUST
    k(5565) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HC9N),ads_stick,user_gsize2,sqrTgas))

    !HCCN -> HCCN_DUST
    k(5566) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCCN),ads_stick,user_gsize2,sqrTgas))

    !HCCP -> HCCP_DUST
    k(5567) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCCP),ads_stick,user_gsize2,sqrTgas))

    !HCL -> HCL_DUST
    k(5568) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCL),ads_stick,user_gsize2,sqrTgas))

    !HCN -> HCN_DUST
    k(5569) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCN),ads_stick,user_gsize2,sqrTgas))

    !HCNC2 -> HCNC2_DUST
    k(5570) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCNC2),ads_stick,user_gsize2,sqrTgas))

    !HCO -> HCO_DUST
    k(5571) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCO),ads_stick,user_gsize2,sqrTgas))

    !HCOOCH3 -> HCOOCH3_DUST
    k(5572) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCOOCH3),ads_stick,user_gsize2,sqrTgas))

    !CH2O2 -> CH2O2_DUST
    k(5573) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_CH2O2),ads_stick,user_gsize2,sqrTgas))

    !HCP -> HCP_DUST
    k(5574) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCP),ads_stick,user_gsize2,sqrTgas))

    !HCS -> HCS_DUST
    k(5575) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCS),ads_stick,user_gsize2,sqrTgas))

    !HCSI -> HCSI_DUST
    k(5576) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HCSI),ads_stick,user_gsize2,sqrTgas))

    !HE -> HE_DUST
    k(5577) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HE),ads_stick,user_gsize2,sqrTgas))

    !HNC -> HNC_DUST
    k(5578) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HNC),ads_stick,user_gsize2,sqrTgas))

    !HNC3 -> HNC3_DUST
    k(5579) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HNC3),ads_stick,user_gsize2,sqrTgas))

    !HNCO -> HNCO_DUST
    k(5580) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HNCO),ads_stick,user_gsize2,sqrTgas))

    !HNO -> HNO_DUST
    k(5581) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HNO),ads_stick,user_gsize2,sqrTgas))

    !HNSI -> HNSI_DUST
    k(5582) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HNSI),ads_stick,user_gsize2,sqrTgas))

    !HPO -> HPO_DUST
    k(5583) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HPO),ads_stick,user_gsize2,sqrTgas))

    !HS -> HS_DUST
    k(5584) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HS),ads_stick,user_gsize2,sqrTgas))

    !HS2 -> HS2_DUST
    k(5585) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_HS2),ads_stick,user_gsize2,sqrTgas))

    !MG -> MG_DUST
    k(5586) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_MG),ads_stick,user_gsize2,sqrTgas))

    !MGH -> MGH_DUST
    k(5587) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_MGH),ads_stick,user_gsize2,sqrTgas))

    !MGH2 -> MGH2_DUST
    k(5588) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_MGH2),ads_stick,user_gsize2,sqrTgas))

    !N -> N_DUST
    k(5589) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_N),ads_stick,user_gsize2,sqrTgas))

    !N2 -> N2_DUST
    k(5590) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_N2),ads_stick,user_gsize2,sqrTgas))

    !N2H2 -> N2H2_DUST
    k(5591) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_N2H2),ads_stick,user_gsize2,sqrTgas))

    !N2O -> N2O_DUST
    k(5592) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_N2O),ads_stick,user_gsize2,sqrTgas))

    !NA -> NA_DUST
    k(5593) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NA),ads_stick,user_gsize2,sqrTgas))

    !NAH -> NAH_DUST
    k(5594) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NAH),ads_stick,user_gsize2,sqrTgas))

    !NAOH -> NAOH_DUST
    k(5595) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NAOH),ads_stick,user_gsize2,sqrTgas))

    !NH -> NH_DUST
    k(5596) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH),ads_stick,user_gsize2,sqrTgas))

    !NH2 -> NH2_DUST
    k(5597) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH2),ads_stick,user_gsize2,sqrTgas))

    !NH2CHO -> NH2CHO_DUST
    k(5598) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH2CHO),ads_stick,user_gsize2,sqrTgas))

    !NH2CN -> NH2CN_DUST
    k(5599) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH2CN),ads_stick,user_gsize2,sqrTgas))

    !NH2OH -> NH2OH_DUST
    k(5600) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH2OH),ads_stick,user_gsize2,sqrTgas))

    !NH3 -> NH3_DUST
    k(5601) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NH3),ads_stick,user_gsize2,sqrTgas))

    !NO -> NO_DUST
    k(5602) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NO),ads_stick,user_gsize2,sqrTgas))

    !NO2 -> NO2_DUST
    k(5603) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NO2),ads_stick,user_gsize2,sqrTgas))

    !NS -> NS_DUST
    k(5604) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_NS),ads_stick,user_gsize2,sqrTgas))

    !O -> O_DUST
    k(5605) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_O),ads_stick,user_gsize2,sqrTgas))

    !O2 -> O2_DUST
    k(5606) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_O2),ads_stick,user_gsize2,sqrTgas))

    !O2H -> O2H_DUST
    k(5607) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_O2H),ads_stick,user_gsize2,sqrTgas))

    !O3 -> O3_DUST
    k(5608) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_O3),ads_stick,user_gsize2,sqrTgas))

    !OCN -> OCN_DUST
    k(5609) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_OCN),ads_stick,user_gsize2,sqrTgas))

    !OCS -> OCS_DUST
    k(5610) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_OCS),ads_stick,user_gsize2,sqrTgas))

    !OH -> OH_DUST
    k(5611) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_OH),ads_stick,user_gsize2,sqrTgas))

    !P -> P_DUST
    k(5612) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_P),ads_stick,user_gsize2,sqrTgas))

    !PH -> PH_DUST
    k(5613) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_PH),ads_stick,user_gsize2,sqrTgas))

    !PH2 -> PH2_DUST
    k(5614) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_PH2),ads_stick,user_gsize2,sqrTgas))

    !PN -> PN_DUST
    k(5615) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_PN),ads_stick,user_gsize2,sqrTgas))

    !PO -> PO_DUST
    k(5616) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_PO),ads_stick,user_gsize2,sqrTgas))

    !S -> S_DUST
    k(5617) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_S),ads_stick,user_gsize2,sqrTgas))

    !S2 -> S2_DUST
    k(5618) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_S2),ads_stick,user_gsize2,sqrTgas))

    !SI -> SI_DUST
    k(5619) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SI),ads_stick,user_gsize2,sqrTgas))

    !SIC -> SIC_DUST
    k(5620) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC),ads_stick,user_gsize2,sqrTgas))

    !SIC2 -> SIC2_DUST
    k(5621) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC2),ads_stick,user_gsize2,sqrTgas))

    !SIC2H -> SIC2H_DUST
    k(5622) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC2H),ads_stick,user_gsize2,sqrTgas))

    !SIC2H2 -> SIC2H2_DUST
    k(5623) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC2H2),ads_stick,user_gsize2,sqrTgas))

    !SIC3 -> SIC3_DUST
    k(5624) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC3),ads_stick,user_gsize2,sqrTgas))

    !SIC3H -> SIC3H_DUST
    k(5625) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC3H),ads_stick,user_gsize2,sqrTgas))

    !SIC4 -> SIC4_DUST
    k(5626) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIC4),ads_stick,user_gsize2,sqrTgas))

    !SICH2 -> SICH2_DUST
    k(5627) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SICH2),ads_stick,user_gsize2,sqrTgas))

    !SICH3 -> SICH3_DUST
    k(5628) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SICH3),ads_stick,user_gsize2,sqrTgas))

    !SIH -> SIH_DUST
    k(5629) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIH),ads_stick,user_gsize2,sqrTgas))

    !SIH2 -> SIH2_DUST
    k(5630) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIH2),ads_stick,user_gsize2,sqrTgas))

    !SIH3 -> SIH3_DUST
    k(5631) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIH3),ads_stick,user_gsize2,sqrTgas))

    !SIH4 -> SIH4_DUST
    k(5632) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIH4),ads_stick,user_gsize2,sqrTgas))

    !SIN -> SIN_DUST
    k(5633) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIN),ads_stick,user_gsize2,sqrTgas))

    !SINC -> SINC_DUST
    k(5634) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SINC),ads_stick,user_gsize2,sqrTgas))

    !SIO -> SIO_DUST
    k(5635) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIO),ads_stick,user_gsize2,sqrTgas))

    !SIO2 -> SIO2_DUST
    k(5636) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIO2),ads_stick,user_gsize2,sqrTgas))

    !SIS -> SIS_DUST
    k(5637) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SIS),ads_stick,user_gsize2,sqrTgas))

    !SO -> SO_DUST
    k(5638) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SO),ads_stick,user_gsize2,sqrTgas))

    !SO2 -> SO2_DUST
    k(5639) = small + (dust_adsorption_rate(user_xdust,imsqrt(idx_SO2),ads_stick,user_gsize2,sqrTgas))

    coe(:) = k(:) !set coefficients to return variable

    !!uncomment below to check coefficient values
    !kmax = 1d0
    !if(maxval(k)>kmax.or.minval(k)<0d0) then
    !   print *,"***************"
    !   do i=1,size(k)
    !      if(k(i)<0d0.or.k(i)>kmax) print *,i,k(i)
    !   end do
    !end if
  end function coe

  !*************************
  subroutine loadReactionsVerbatim()
    use krome_commons
    implicit none
    character*255::fname,line
    integer::ios,i,nunit

    ! Verbatim reactions filename defaults to `reactions_verbatim.dat`
    fname = "reactions_verbatim.dat"

    !verbatim reactions are loaded from file
    ! to increase compilation speed
    open(newunit=nunit,file=trim(fname),status="old",iostat=ios)
    if(ios/=0) then
      print *,"ERROR: "//trim(fname)//" file not present!"
      stop
    end if

    !load reactions from file
    do i=1,nrea
      read(nunit,'(a)',iostat=ios) line
      if(ios/=0) then
        print *,"ERROR: problem reading "//trim(fname)
        stop
      end if
      reactionNames(i) = trim(line)
    end do
    close(nunit)

  end subroutine loadReactionsVerbatim

  !*******************
  !The following functions compute the recombination rate
  ! on dust for H+, He+, C+, Si+, and O+. See Weingartner&Draine 2001
  ! dust2gas_ratio, D/D_sol, default is assumed equal to Z/Z_sol
  function H_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::H_recombination_on_dust

    H_recombination_on_dust = 0d0

    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    H_recombination_on_dust =  1.225d-13*dust2gas_ratio &
        /(1.d0+8.074d-6*psi**(1.378)*(1.d0+5.087d2 &
        *Tgas**(0.01586)*psi**(-0.4723-1.102d-5*log(Tgas))))

  end function H_recombination_on_dust

  !******************
  function He_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::He_recombination_on_dust

    He_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    He_recombination_on_dust = 5.572d-14*dust2gas_ratio&
        /(1.d0+3.185d-7*psi**(1.512)*(1.d0+5.115d3&
        *Tgas**(3.903d-7)*psi**(-0.4956-5.494d-7*log(Tgas))))

  end function He_recombination_on_dust

  !*******************
  function C_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::C_recombination_on_dust

    C_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    C_recombination_on_dust = 4.558d-13*dust2gas_ratio&
        /(1.d0+6.089d-3*psi**(1.128)*(1.d0+4.331d2&
        *Tgas**(0.04845)*psi**(-0.8120-1.333d-4*log(Tgas))))

  end function C_recombination_on_dust

  !******************
  function Si_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::Si_recombination_on_dust

    Si_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    Si_recombination_on_dust = 2.166d-14*dust2gas_ratio&
        /(1.d0+5.678d-8*psi**(1.874)*(1.d0+4.375d4&
        *Tgas**(1.635d-6)*psi**(-0.8964-7.538d-5*log(Tgas))))

  end function Si_recombination_on_dust

  !********************
  function O_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,k_H
    real*8::O_recombination_on_dust

    k_H = H_recombination_on_dust(n(:),Tgas)
    O_recombination_on_dust = 0.25d0*k_H

  end function O_recombination_on_dust

  !*********************
  !This function returns the
  ! photorate of H2 occurring in the
  ! Lyman-Werner bands following the approximation
  ! provided by Glover&Jappsen 2007. Rate in 1/s.
  !Approximation valid at low-density, it assumes H2(nu = 0).
  !It also stores the rate as a common, needed for the photoheating
  function H2_solomonLW(myflux)
    use krome_commons
    use krome_constants
    implicit none
    real*8::H2_solomonLW,myflux

    !myflux is the radiation background at E = 12.87 eV
    !should be converted to erg
    H2_solomonLW = 1.38d9*myflux*eV_to_erg

  end function H2_solomonLW

  !****************************
  !tanh smoothing function that
  ! increses when xarg increases.
  ! xpos is the position of the transition point.
  ! slope is the steepness of the curve.
  function smooth_increase(xarg,xpos,slope)
    implicit none
    real*8::smooth_increase,xarg,xpos,slope

    smooth_increase = .5d0 * (tanh(slope * (xarg - xpos)) &
        + 1d0)

  end function smooth_increase

  !****************************
  !tanh smoothing function that
  ! decreses when xarg increases.
  ! xpos is the position of the transition point.
  ! slope is the steepness of the curve.
  function smooth_decrease(xarg,xpos,slope)
    implicit none
    real*8::smooth_decrease,xarg,xpos,slope

    smooth_decrease = .5d0 * (tanh(-slope * (xarg - xpos)) &
        + 1d0)

  end function smooth_decrease

  !*********************
  !sign: return 1d0 if x>=0d0,
  ! else return -1d0
  function get_sgn(x)
    implicit none
    real*8::x,get_sgn

    get_sgn = 1d0
    if(x==0d0) return
    get_sgn = x/abs(x)

  end function get_sgn

  !*********************
  function conserve(n,ni)
    use krome_commons
    implicit none
    real*8::conserve(nspec),n(nspec),ni(nspec),no(nspec)
    real*8::ntot,nitot,factor

    no(:) = n(:)

    conserve(:) = 0d0
    conserve(:) = no(:)

  end function conserve

  !*************************
  !this subroutine changes the x(:) mass fractions of the species
  ! to force conservation according to the reference ref(:)
  subroutine conserveLin_x(x,ref)
    use krome_commons
    use krome_getphys
    implicit none
    real*8::x(nmols),ref(natoms)
    real*8::A(natoms,natoms),B(natoms),m(nspec)

    m(:) = get_mass()
    A(:,:) = 0d0
    B(:) = ref(:)

    !charge conservation
    x(idx_E) = m(idx_E)*(- 1d0*x(idx_Hk) / m(idx_Hk) &
        - 1d0*x(idx_Ck) / m(idx_Ck) &
        - 1d0*x(idx_CNk) / m(idx_CNk) &
        - 1d0*x(idx_Ok) / m(idx_Ok) &
        - 1d0*x(idx_OHk) / m(idx_OHk) &
        - 1d0*x(idx_Sk) / m(idx_Sk) &
        - 1d0*x(idx_GRAINk) / m(idx_GRAINk) &
        + 1d0*x(idx_Cj) / m(idx_Cj) &
        + 1d0*x(idx_CLj) / m(idx_CLj) &
        + 1d0*x(idx_FEj) / m(idx_FEj) &
        + 1d0*x(idx_Hj) / m(idx_Hj) &
        + 1d0*x(idx_HEj) / m(idx_HEj) &
        + 1d0*x(idx_MGj) / m(idx_MGj) &
        + 1d0*x(idx_Nj) / m(idx_Nj) &
        + 1d0*x(idx_NAj) / m(idx_NAj) &
        + 1d0*x(idx_Oj) / m(idx_Oj) &
        + 1d0*x(idx_Pj) / m(idx_Pj) &
        + 1d0*x(idx_Sj) / m(idx_Sj) &
        + 1d0*x(idx_SIj) / m(idx_SIj) &
        + 1d0*x(idx_COj) / m(idx_COj) &
        + 1d0*x(idx_H2j) / m(idx_H2j) &
        + 1d0*x(idx_NOj) / m(idx_NOj) &
        + 1d0*x(idx_O2j) / m(idx_O2j) &
        + 1d0*x(idx_CH2j) / m(idx_CH2j) &
        + 1d0*x(idx_H2Sj) / m(idx_H2Sj) &
        + 1d0*x(idx_HCOj) / m(idx_HCOj) &
        + 1d0*x(idx_HCSj) / m(idx_HCSj) &
        + 1d0*x(idx_HNOj) / m(idx_HNOj) &
        + 1d0*x(idx_NH2j) / m(idx_NH2j) &
        + 1d0*x(idx_OCSj) / m(idx_OCSj) &
        + 1d0*x(idx_C2H2j) / m(idx_C2H2j) &
        + 1d0*x(idx_CH3j) / m(idx_CH3j) &
        + 1d0*x(idx_NH3j) / m(idx_NH3j) &
        + 1d0*x(idx_C2H2Oj) / m(idx_C2H2Oj) &
        + 1d0*x(idx_CH2O2j) / m(idx_CH2O2j) &
        + 1d0*x(idx_C2H3Nj) / m(idx_C2H3Nj) &
        + 1d0*x(idx_C2H4j) / m(idx_C2H4j) &
        + 1d0*x(idx_C4H2j) / m(idx_C4H2j) &
        + 1d0*x(idx_H3COj) / m(idx_H3COj) &
        + 1d0*x(idx_CH4Oj) / m(idx_CH4Oj) &
        + 1d0*x(idx_C2H4Oj) / m(idx_C2H4Oj) &
        + 1d0*x(idx_C3H4j) / m(idx_C3H4j) &
        + 1d0*x(idx_CH5Nj) / m(idx_CH5Nj) &
        + 1d0*x(idx_C2H5OHj) / m(idx_C2H5OHj) &
        + 1d0*x(idx_CH3OCH3j) / m(idx_CH3OCH3j) &
        + 1d0*x(idx_CHj) / m(idx_CHj) &
        + 1d0*x(idx_CCLj) / m(idx_CCLj) &
        + 1d0*x(idx_C2j) / m(idx_C2j) &
        + 1d0*x(idx_CLOj) / m(idx_CLOj) &
        + 1d0*x(idx_CPj) / m(idx_CPj) &
        + 1d0*x(idx_CSj) / m(idx_CSj) &
        + 1d0*x(idx_CNj) / m(idx_CNj) &
        + 1d0*x(idx_NSj) / m(idx_NSj) &
        + 1d0*x(idx_PHj) / m(idx_PHj) &
        + 1d0*x(idx_POj) / m(idx_POj) &
        + 1d0*x(idx_SICj) / m(idx_SICj) &
        + 1d0*x(idx_SINj) / m(idx_SINj) &
        + 1d0*x(idx_SISj) / m(idx_SISj) &
        + 1d0*x(idx_SOj) / m(idx_SOj) &
        + 1d0*x(idx_C3j) / m(idx_C3j) &
        + 1d0*x(idx_C2Sj) / m(idx_C2Sj) &
        + 1d0*x(idx_C2Oj) / m(idx_C2Oj) &
        + 1d0*x(idx_CCPj) / m(idx_CCPj) &
        + 1d0*x(idx_C2Hj) / m(idx_C2Hj) &
        + 1d0*x(idx_HOCj) / m(idx_HOCj) &
        + 1d0*x(idx_C2Nj) / m(idx_C2Nj) &
        + 1d0*x(idx_CNCj) / m(idx_CNCj) &
        + 1d0*x(idx_HCPj) / m(idx_HCPj) &
        + 1d0*x(idx_SIC2j) / m(idx_SIC2j) &
        + 1d0*x(idx_SINCj) / m(idx_SINCj) &
        + 1d0*x(idx_HPOj) / m(idx_HPOj) &
        + 1d0*x(idx_HCNj) / m(idx_HCNj) &
        + 1d0*x(idx_CHSIj) / m(idx_CHSIj) &
        + 1d0*x(idx_SIH2j) / m(idx_SIH2j) &
        + 1d0*x(idx_C3Hj) / m(idx_C3Hj) &
        + 1d0*x(idx_C4j) / m(idx_C4j) &
        + 1d0*x(idx_C3Oj) / m(idx_C3Oj) &
        + 1d0*x(idx_C3Sj) / m(idx_C3Sj) &
        + 1d0*x(idx_H2COj) / m(idx_H2COj) &
        + 1d0*x(idx_H2SIOj) / m(idx_H2SIOj) &
        + 1d0*x(idx_HCNHj) / m(idx_HCNHj) &
        + 1d0*x(idx_SIC2Hj) / m(idx_SIC2Hj) &
        + 1d0*x(idx_SIC3j) / m(idx_SIC3j) &
        + 1d0*x(idx_CH2SIj) / m(idx_CH2SIj) &
        + 1d0*x(idx_SIH3j) / m(idx_SIH3j) &
        + 1d0*x(idx_C2H2Nj) / m(idx_C2H2Nj) &
        + 1d0*x(idx_C2H3j) / m(idx_C2H3j) &
        + 1d0*x(idx_C3H2j) / m(idx_C3H2j) &
        + 1d0*x(idx_H2C3j) / m(idx_H2C3j) &
        + 1d0*x(idx_C4Hj) / m(idx_C4Hj) &
        + 1d0*x(idx_C5j) / m(idx_C5j) &
        + 1d0*x(idx_C4Sj) / m(idx_C4Sj) &
        + 1d0*x(idx_PC2Hj) / m(idx_PC2Hj) &
        + 1d0*x(idx_C3Nj) / m(idx_C3Nj) &
        + 1d0*x(idx_C4Nj) / m(idx_C4Nj) &
        + 1d0*x(idx_C3HNj) / m(idx_C3HNj) &
        + 1d0*x(idx_HNCj) / m(idx_HNCj) &
        + 1d0*x(idx_SIC3Hj) / m(idx_SIC3Hj) &
        + 1d0*x(idx_SIC4j) / m(idx_SIC4j) &
        + 1d0*x(idx_SIC2H2j) / m(idx_SIC2H2j) &
        + 1d0*x(idx_SICH3j) / m(idx_SICH3j) &
        + 1d0*x(idx_HC2NCHj) / m(idx_HC2NCHj) &
        + 1d0*x(idx_C3H3j) / m(idx_C3H3j) &
        + 1d0*x(idx_H3C3j) / m(idx_H3C3j) &
        + 1d0*x(idx_C5Hj) / m(idx_C5Hj) &
        + 1d0*x(idx_C6j) / m(idx_C6j) &
        + 1d0*x(idx_C2H3Oj) / m(idx_C2H3Oj) &
        + 1d0*x(idx_C2H5j) / m(idx_C2H5j) &
        + 1d0*x(idx_C3H3Nj) / m(idx_C3H3Nj) &
        + 1d0*x(idx_C5H2j) / m(idx_C5H2j) &
        + 1d0*x(idx_C4H3j) / m(idx_C4H3j) &
        + 1d0*x(idx_C6Hj) / m(idx_C6Hj) &
        + 1d0*x(idx_C7j) / m(idx_C7j) &
        + 1d0*x(idx_CH4Nj) / m(idx_CH4Nj) &
        + 1d0*x(idx_C5HNj) / m(idx_C5HNj) &
        + 1d0*x(idx_C7Hj) / m(idx_C7Hj) &
        + 1d0*x(idx_C8j) / m(idx_C8j) &
        + 1d0*x(idx_COOCH4j) / m(idx_COOCH4j) &
        + 1d0*x(idx_C2H5Oj) / m(idx_C2H5Oj) &
        + 1d0*x(idx_C8Hj) / m(idx_C8Hj) &
        + 1d0*x(idx_C9j) / m(idx_C9j) &
        + 1d0*x(idx_C5H3j) / m(idx_C5H3j) &
        + 1d0*x(idx_C6H2j) / m(idx_C6H2j) &
        + 1d0*x(idx_C6H3j) / m(idx_C6H3j) &
        + 1d0*x(idx_C2H6COj) / m(idx_C2H6COj) &
        + 1d0*x(idx_C9Hj) / m(idx_C9Hj) &
        + 1d0*x(idx_C10j) / m(idx_C10j) &
        + 1d0*x(idx_C7H3j) / m(idx_C7H3j) &
        + 1d0*x(idx_C8H2j) / m(idx_C8H2j) &
        + 1d0*x(idx_C8H3j) / m(idx_C8H3j) &
        + 1d0*x(idx_HCLj) / m(idx_HCLj) &
        + 1d0*x(idx_HSj) / m(idx_HSj) &
        + 1d0*x(idx_NHj) / m(idx_NHj) &
        + 1d0*x(idx_OHj) / m(idx_OHj) &
        + 1d0*x(idx_PNj) / m(idx_PNj) &
        + 1d0*x(idx_S2j) / m(idx_S2j) &
        + 1d0*x(idx_SIHj) / m(idx_SIHj) &
        + 1d0*x(idx_SIOj) / m(idx_SIOj) &
        + 1d0*x(idx_H2Oj) / m(idx_H2Oj) &
        + 1d0*x(idx_HNSIj) / m(idx_HNSIj) &
        + 1d0*x(idx_S2Hj) / m(idx_S2Hj) &
        + 1d0*x(idx_PH2j) / m(idx_PH2j) &
        + 1d0*x(idx_H2CSj) / m(idx_H2CSj) &
        + 1d0*x(idx_H2S2j) / m(idx_H2S2j) &
        + 1d0*x(idx_HSIOj) / m(idx_HSIOj) &
        + 1d0*x(idx_C4Pj) / m(idx_C4Pj) &
        + 1d0*x(idx_HCO2j) / m(idx_HCO2j) &
        + 1d0*x(idx_PCH3j) / m(idx_PCH3j) &
        + 1d0*x(idx_CH4j) / m(idx_CH4j) &
        + 1d0*x(idx_C2NHj) / m(idx_C2NHj) &
        + 1d0*x(idx_SIH4j) / m(idx_SIH4j) &
        + 1d0*x(idx_NH4j) / m(idx_NH4j) &
        + 1d0*x(idx_H2NCj) / m(idx_H2NCj) &
        + 1d0*x(idx_C3H2Nj) / m(idx_C3H2Nj) &
        + 1d0*x(idx_C7H2j) / m(idx_C7H2j) &
        + 1d0*x(idx_C5H4j) / m(idx_C5H4j) &
        + 1d0*x(idx_C7HNj) / m(idx_C7HNj) &
        + 1d0*x(idx_C9H2j) / m(idx_C9H2j) &
        + 1d0*x(idx_C7H4j) / m(idx_C7H4j) &
        + 1d0*x(idx_C9HNj) / m(idx_C9HNj) &
        + 1d0*x(idx_N2j) / m(idx_N2j) &
        + 1d0*x(idx_CO2j) / m(idx_CO2j) &
        + 1d0*x(idx_HEHj) / m(idx_HEHj) &
        + 1d0*x(idx_SO2j) / m(idx_SO2j) &
        + 1d0*x(idx_C6H5j) / m(idx_C6H5j) &
        + 1d0*x(idx_C5H5j) / m(idx_C5H5j) &
        + 1d0*x(idx_N2Hj) / m(idx_N2Hj) &
        + 1d0*x(idx_NO2j) / m(idx_NO2j) &
        + 1d0*x(idx_PC2H2j) / m(idx_PC2H2j) &
        + 1d0*x(idx_PNH2j) / m(idx_PNH2j) &
        + 1d0*x(idx_PCH2j) / m(idx_PCH2j) &
        + 1d0*x(idx_HC2Sj) / m(idx_HC2Sj) &
        + 1d0*x(idx_HC3Sj) / m(idx_HC3Sj) &
        + 1d0*x(idx_H3CSj) / m(idx_H3CSj) &
        + 1d0*x(idx_HC4Sj) / m(idx_HC4Sj) &
        + 1d0*x(idx_SINH2j) / m(idx_SINH2j) &
        + 1d0*x(idx_SIC2H3j) / m(idx_SIC2H3j) &
        + 1d0*x(idx_SIC3H2j) / m(idx_SIC3H2j) &
        + 1d0*x(idx_C2HOj) / m(idx_C2HOj) &
        + 1d0*x(idx_H3Oj) / m(idx_H3Oj) &
        + 1d0*x(idx_H3Sj) / m(idx_H3Sj) &
        + 1d0*x(idx_HOCSj) / m(idx_HOCSj) &
        + 1d0*x(idx_CH5Oj) / m(idx_CH5Oj) &
        + 1d0*x(idx_NCOj) / m(idx_NCOj) &
        + 1d0*x(idx_HNCOj) / m(idx_HNCOj) &
        + 1d0*x(idx_C2N2j) / m(idx_C2N2j) &
        + 1d0*x(idx_H3j) / m(idx_H3j) &
        + 1d0*x(idx_O2Hj) / m(idx_O2Hj) &
        + 1d0*x(idx_CH5j) / m(idx_CH5j) &
        + 1d0*x(idx_H2CLj) / m(idx_H2CLj) &
        + 1d0*x(idx_CH3O2j) / m(idx_CH3O2j) &
        + 1d0*x(idx_H2POj) / m(idx_H2POj) &
        + 1d0*x(idx_PNH3j) / m(idx_PNH3j) &
        + 1d0*x(idx_PCH4j) / m(idx_PCH4j) &
        + 1d0*x(idx_PC2H3j) / m(idx_PC2H3j) &
        + 1d0*x(idx_HSISj) / m(idx_HSISj) &
        + 1d0*x(idx_HSOj) / m(idx_HSOj) &
        + 1d0*x(idx_HNSj) / m(idx_HNSj) &
        + 1d0*x(idx_HPNj) / m(idx_HPNj) &
        + 1d0*x(idx_H2NOj) / m(idx_H2NOj) &
        + 1d0*x(idx_NAH2Oj) / m(idx_NAH2Oj) &
        + 1d0*x(idx_PH3j) / m(idx_PH3j) &
        + 1d0*x(idx_SINCHj) / m(idx_SINCHj) &
        + 1d0*x(idx_HSIO2j) / m(idx_HSIO2j) &
        + 1d0*x(idx_HSO2j) / m(idx_HSO2j) &
        + 1d0*x(idx_HC3Oj) / m(idx_HC3Oj) &
        + 1d0*x(idx_PC3Hj) / m(idx_PC3Hj) &
        + 1d0*x(idx_H3S2j) / m(idx_H3S2j) &
        + 1d0*x(idx_H3SIOj) / m(idx_H3SIOj) &
        + 1d0*x(idx_PC4Hj) / m(idx_PC4Hj) &
        + 1d0*x(idx_NH2CNHj) / m(idx_NH2CNHj) &
        + 1d0*x(idx_SIC4Hj) / m(idx_SIC4Hj) &
        + 1d0*x(idx_SICH4j) / m(idx_SICH4j) &
        + 1d0*x(idx_SIH5j) / m(idx_SIH5j) &
        + 1d0*x(idx_C2H4Nj) / m(idx_C2H4Nj) &
        + 1d0*x(idx_NH2CH2Oj) / m(idx_NH2CH2Oj) &
        + 1d0*x(idx_C2H6j) / m(idx_C2H6j) &
        + 1d0*x(idx_C3H4Nj) / m(idx_C3H4Nj) &
        + 1d0*x(idx_C3H5j) / m(idx_C3H5j) &
        + 1d0*x(idx_C4H4j) / m(idx_C4H4j) &
        + 1d0*x(idx_CH6Nj) / m(idx_CH6Nj) &
        + 1d0*x(idx_C5H2Nj) / m(idx_C5H2Nj) &
        + 1d0*x(idx_C4H4Nj) / m(idx_C4H4Nj) &
        + 1d0*x(idx_H5C2O2j) / m(idx_H5C2O2j) &
        + 1d0*x(idx_C2H5OH2j) / m(idx_C2H5OH2j) &
        + 1d0*x(idx_CH3OCH4j) / m(idx_CH3OCH4j) &
        + 1d0*x(idx_C7H2Nj) / m(idx_C7H2Nj) &
        + 1d0*x(idx_C3H6OHj) / m(idx_C3H6OHj) &
        + 1d0*x(idx_C6H4Nj) / m(idx_C6H4Nj) &
        + 1d0*x(idx_C10Hj) / m(idx_C10Hj) &
        + 1d0*x(idx_C9H3j) / m(idx_C9H3j) &
        + 1d0*x(idx_C7H5j) / m(idx_C7H5j) &
        + 1d0*x(idx_C8H4Nj) / m(idx_C8H4Nj) &
        + 1d0*x(idx_C9H2Nj) / m(idx_C9H2Nj) &
        + 1d0*x(idx_C6H7j) / m(idx_C6H7j) &
        + 1d0*x(idx_NAH2j) / m(idx_NAH2j) &
        + 1d0*x(idx_PC2H4j) / m(idx_PC2H4j) &
        + 1d0*x(idx_C4H5j) / m(idx_C4H5j) &
        + 1d0*x(idx_H2CCLj) / m(idx_H2CCLj) &
        + 1d0*x(idx_PC4H2j) / m(idx_PC4H2j) &
        + 1d0*x(idx_C6H4j) / m(idx_C6H4j) &
        + 1d0*x(idx_C8H4j) / m(idx_C8H4j) &
        + 1d0*x(idx_C9H4j) / m(idx_C9H4j) &
        + 1d0*x(idx_C4H7j) / m(idx_C4H7j) &
        + 1d0*x(idx_HC4Nj) / m(idx_HC4Nj) &
        + 1d0*x(idx_HC4Oj) / m(idx_HC4Oj) &
        + 1d0*x(idx_C5Nj) / m(idx_C5Nj) &
        + 1d0*x(idx_H2C4Nj) / m(idx_H2C4Nj) &
        + 1d0*x(idx_H3C4Nj) / m(idx_H3C4Nj) &
        + 1d0*x(idx_C7Nj) / m(idx_C7Nj) &
        + 1d0*x(idx_C5H3Nj) / m(idx_C5H3Nj) &
        + 1d0*x(idx_C10H2j) / m(idx_C10H2j) &
        + 1d0*x(idx_C9Nj) / m(idx_C9Nj) &
        + 1d0*x(idx_C7H3Nj) / m(idx_C7H3Nj) &
        + 1d0*x(idx_C9H3Nj) / m(idx_C9H3Nj) &
        + 1d0*x(idx_OCSjH2) / m(idx_OCSjH2) &
        + 1d0*x(idx_H2C3Oj) / m(idx_H2C3Oj) &
        + 1d0*x(idx_H3C3Oj) / m(idx_H3C3Oj) &
        + 1d0*x(idx_C5H4Nj) / m(idx_C5H4Nj) &
        + 1d0*x(idx_C8H5j) / m(idx_C8H5j) &
        + 1d0*x(idx_C9H5j) / m(idx_C9H5j) &
        + 1d0*x(idx_H2COHOCH2j) / m(idx_H2COHOCH2j) &
        + 1d0*x(idx_H7C2O2j) / m(idx_H7C2O2j))
    !check if charge conservation goes wrong
    if(x(idx_E)<0d0) then
      print *,"ERROR in conserveLin, electrons < 0"
      stop
    end if

  end subroutine conserveLin_x

  !***************************
  !compute the total reference mass atom type by atom type
  function conserveLinGetRef_x(x)
    use krome_commons
    use krome_getphys
    implicit none
    real*8::conserveLinGetRef_x(natoms),x(nmols)
    real*8::m(nspec)

    m(:) = get_mass()
    conserveLinGetRef_x(:) = 0d0

  end function conserveLinGetRef_x

  !***************************
  !Ref: Sasaki & Takahara (1993)
  !This function evaluate the recombination rate
  ! for H+ + e --> H + gamma and the same
  ! for D+ + e --> D + gamma
  function elec_recomb_ST93(nabund,nelec,ntot,nucleiH,Trad)
    use krome_commons
    use krome_constants
    implicit none
    real*8::nabund,nelec,Trad
    real*8::nucleiH,elec_recomb_ST93
    real*8::al,ak,rc2,r2c
    real*8::a0,b0,c0,d0,e0
    real*8::a1,b1,c1,d1,e1,f1,g1,h1
    real*8::ntot,ratio

    al = 8.227d0
    ak = 22.06d0 / (hubble  *(1d0 + phys_zredshift) &
        * sqrt(1d0 + Omega0 * phys_zredshift))
    !Rc2 evaluation
    rc2 = 8.76d-11 * (1d0 + phys_zredshift)**(-0.58)
    !R2c evaluation
    r2c = (1.80d10 * Trad)**(1.5) &
        * exp(-3.9472d4 / Trad) * rc2

    !coefficients
    a0 = nucleiH * rc2
    b0 = ak * al * nucleiH
    c0 = ak * rc2 * nucleiH * nucleiH
    d0 = r2c * exp(-1.18416d5/Trad)
    e0 = ak * r2c * nucleiH

    !polynomial terms
    a1 = -d0 * (1d0 + b0)
    b1 = d0 * (1d0 + 2d0 * b0)
    c1 = a0 + b0 * (a0 - d0)
    d1 = -a0 * b0
    e1 = a0 * c0
    f1 = 1d0 + b0 + e0
    g1 = -(b0 + e0)
    h1 = c0

    ratio = nabund / ntot

    elec_recomb_ST93 = ntot*(a1 + b1*ratio + c1*ratio**2 + d1*ratio**3 &
        + e1*ratio**4) / (f1 + g1*ratio + h1*ratio**2)

    elec_recomb_ST93 = elec_recomb_ST93 / (nabund * nelec)

  end function elec_recomb_ST93

  !********************
  subroutine load_parts()
    use krome_commons
    implicit none

  end subroutine load_parts

  !*************************
  subroutine load_part(fname,array_part,min_part,dT_part)
    character(len=*)::fname
    integer::ios,icount,i,cv
    real*8,allocatable::array_part(:),emed(:)
    real*8::min_part,dT_part,Told,array_tmp(int(1e5)),rout(2)

    open(33,file=trim(fname),status="old",iostat=ios)
    if(ios.ne.0) then
      print *,"ERROR: partition function not found"
      print *," in file "//fname
      stop
    end if

    print *,"loading partition function from "//fname
    icount = 0
    min_part = 1d99
    Told = 0d0
    do
      read(33,*,iostat=ios) rout(:)
      if(ios<0) exit
      if(ios.ne.0) cycle
      icount = icount + 1
      min_part = min(min_part,rout(1))
      array_tmp(icount) = rout(2)
      dT_part = rout(1) - Told
      Told = rout(1)
    end do
    close(33)

    allocate(array_part(icount),emed(icount))
    array_part(:) = array_tmp(1:icount)

  end subroutine load_part

  !**********************
  function troe_falloff(k0,kinf,Fc,m)
    implicit none
    real*8::troe_falloff,k0,kinf,Fc,m,rm,xexp
    rm = k0*m/kinf
    xexp = 1d0/(1d0+log10(rm)**2)
    troe_falloff = k0*m/(1d0+rm)*Fc**xexp
  end function troe_falloff

  !*************************
  function k3body(k0,kinf,Fc,nM)
    implicit none
    real*8::k3body,k0,kinf,Fc,nM
    real*8::c,n,d,Pr,xexp,F

    c = -0.4d0-0.67d0*log10(Fc)
    n = 0.75d0-1.27d0*log10(Fc)
    d = 0.14d0
    Pr = k0*nM/kinf
    xexp = (log10(Pr)+c)/(n-d*(log10(Pr)+c))
    F = 1d1**(log10(Fc)/(1d0+xexp**2))
    k3body = kinf*(Pr/(1d0+Pr)) * F

  end function k3body

  !***********************
  !see http://kida.obs.u-bordeaux1.fr/help
  function KIDA3body(ka0,kb0,kc0,kaInf,kbInf,kcInf,kaFc,kbFc,&
        kcFc,kdFc,npart,Tgas,pmin,pmax)
    implicit none
    real*8::ka0,kb0,kc0,kaInf,kbInf,kcInf,kaFc,kbFc,kcFc,kdFc
    real*8::KIDA3body,kinf,p,f,npart,Tgas,fc,fexp,invT
    real*8::k0,cc,dd,nn,pmin,pmax

    KIDA3body = 0d0

    invT = 1d0/Tgas
    k0 = ka0*(Tgas/3d2)**kb0*exp(-kc0*invT)
    kinf = kainf*(Tgas/3d2)**kbinf*exp(-kcinf*invT)

    p = k0*npart/kinf
    if(p<pmin) return
    if(p>pmax) return

    fc = (1d0-kaFc)*exp(-Tgas/kbFc) + kaFc*exp(-Tgas/kbFc) &
        + exp(-kdFc*invT)

    cc = -0.4d0 - 0.67d0 *log10(fc)
    dd = 0.14d0
    nn = 0.75d0 - 1.27d0*log10(fc)
    fexp = 1d0 + ((log10(p)+cc)/(nn-dd*(log10(p)+cc)))**2

    f = fc**(1d0/fexp)

    KIDA3body = kinf*(p/(1d0+p))*f

  end function KIDA3body

  !******************************
  !collisional ionization rate from Verner+96
  ! unit: cm3/s
  function colion_v96(Tgas,dE,P,A,X,K)
    implicit none
    real*8::colion_v96,Tgas,dE,A,X,K,U,Te,P

    Te = Tgas * 8.621738d-5 !K to eV
    U = dE / Te
    colion_v96 = A * (1d0 + P*sqrt(U)) * U**K * exp(-U) / (X+U)

  end function colion_v96

  !****************************
  !radiative recombination rates from
  ! Verner routine, standard fit, cm3/s
  function recV96(Tgas,a,b)
    implicit none
    real*8::recV96,Tgas,a,b

    recV96 = a*(1d4/Tgas)**b

  end function recV96

  !****************************
  !radiative recombination rates from
  ! Verner routine, new fit, cm3/s
  function recNewV96(Tgas,r1,r2,r3,r4)
    implicit none
    real*8::recNewV96,Tgas,r1,r2,r3,r4,tt

    tt = sqrt(Tgas/r3)
    recNewV96 = r1/(tt*(tt + 1d0)**(1.-r2) &
        * (1d0 + sqrt(Tgas/r4))**(1.+r2))

  end function recNewV96

  !****************************
  !radiative recombination rates from
  ! Verner routine, iron only, cm3/s
  function recFeV96(Tgas,r1,r2,r3)
    implicit none
    real*8::recFeV96,Tgas,r1,r2,r3,tt

    tt = sqrt(Tgas*1d-4)
    recFeV96 = r1/tt**(r2 + r3 + log10(tt))

  end function recFeV96

  !******************************
  !radiative recombination rates from Verner+96
  ! unit: cm3/s
  function radrec_v96(Tgas,a,b,T0,T1)
    implicit none
    real*8::Tgas,a,b,T0,T1,radrec_v96,iT0

    iT0 = 1d0/T0
    radrec_v96 = a/(sqrt(Tgas*iT0) + (1d0*sqrt(Tgas*iT0))**(1.-b) &
        * (1d0+sqrt(Tgas/T1))**(1+b))

  end function radrec_v96

  !*******************************
  !radiative recombination rates low-temp fit, Verner+96
  ! unit: cm3/s
  function radrec_low_v96(Tgas,a,b,c,d,f)
    implicit none
    real*8::Tgas,a,b,c,d,f,radrec_low_v96,t,invt

    t = Tgas*1d-4
    invt = 1d0/t

    radrec_low_v96 = 1d-12 * (a*invt + b + c*t + d*t**2) &
        * t**(-1.5) * exp(-f*invt)

    radrec_low_v96 = max(0d0,radrec_low_v96)

  end function radrec_low_v96

  !***************************
  !Collisional dissociation rate (cm-3/s) by Martin et al. 1996
  ! H2+H->H+H+H
  !NOTE: the use of this rate is suggested
  ! for high-density regime and in the presence of UV backgrounds.
  ! if necessary it must be included in the reaction file as
  ! H2,H,,H,H,H,,NONE,NONE,dissH2_Martin96(n,Tgas)
  function dissH2_Martin96(n,Tgas)
    use krome_commons
    use krome_getphys
    integer::i
    real*8::n(nspec),Tgas,dissH2_Martin96
    real*8::CDrates,logTv(4),k_CIDm(21,2),k_CID,invT,logT,n_c1,n_c2,n_H
    real*8::logk_h1,logk_h2,logk_l1,logk_l2,logn_c1,logn_c2,p,logk_CID
    real*8::logT2,logT3

    !k_CID = collision-induced dissociation + dissociative tunneling

    !Collisional dissociation of H2
    k_CIDm(:,1) = (/-178.4239d0, -68.42243d0, 43.20243d0, -4.633167d0, &
        69.70086d0, 40870.38d0, -23705.70d0, 128.8953d0, -53.91334d0, &
        5.315517d0, -19.73427d0, 16780.95d0, -25786.11d0, 14.82123d0, &
        -4.890915d0, 0.4749030d0, -133.8283d0, -1.164408d0, 0.8227443d0,&
        0.5864073d0, -2.056313d0/)

    !Dissociative tunneling of H2
    k_CIDm(:,2) = (/-142.7664d0, 42.70741d0, -2.027365d0, -0.2582097d0, &
        21.36094d0, 27535.31d0, -21467.79d0, 60.34928d0, -27.43096d0, &
        2.676150d0, -11.28215d0, 14254.55d0, -23125.20d0, 9.305564d0, &
        -2.464009d0, 0.1985955d0, 743.0600d0, -1.174242d0, 0.7502286d0, &
        0.2358848d0, 2.937507d0/)

    n_H  = get_Hnuclei(n(:))
    logT = log10(Tgas)
    invT = 1.0d0/Tgas
    logT2 = logT*logT
    logT3 = logT2*logT
    logTv = (/1.d0, logT, logT2, logT3/)
    k_CID = 0.d0
    do i=1,2
      logk_h1 = k_CIDm(1,i)*logTv(1) + k_CIDm(2,i)*logTv(2) + &
          k_CIDm(3,i)*logTv(3) + k_CIDm(4,i)*logTv(4) + &
          k_CIDm(5,i)*log10(1.d0+k_CIDm(6,i)*invT)
      logk_h2 = k_CIDm(7,i)*invT
      logk_l1 = k_CIDm(8,i)*logTv(1) + k_CIDm(9,i)*logTv(2) + &
          k_CIDm(10,i)*logTv(3) + k_CIDm(11,i)*log10(1.d0+k_CIDm(12,i)*invT)
      logk_l2 = k_CIDm(13,i)*invT
      logn_c1 = k_CIDm(14,i)*logTv(1) + k_CIDm(15,i)*logTv(2) &
          + k_CIDm(16,i)*logTv(3) + k_CIDm(17,i)*invT
      logn_c2 = k_CIDm(18,i) + logn_c1
      p = k_CIDm(19,i) + k_CIDm(20,i)*exp(-Tgas/1.850d3) &
          + k_CIDm(21,i)*exp(-Tgas/4.40d2)
      n_c1 = 1d1**(logn_c1)
      n_c2 = 1d1**(logn_c2)
      logk_CID = logk_h1 - (logk_h1 - logk_l1) / (1.d0 + (n_H/n_c1)**p) &
          + logk_h2 - (logk_h2 - logk_l2) / (1.d0 + (n_H/n_c2)**p)
      k_CID = k_CID + 1.d1**logk_CID
    enddo

    dissH2_Martin96 = k_CID

  end function dissH2_Martin96

  !**********************
  ! Cluster growth rate based on kinetic nucleation theory (KNT)
  ! Theory is explained in chapter 13 of Gail and Sedlmayr 2013
  ! (https://doi.org/10.1017/CBO9780511985607)
  function cluster_growth_rate(monomer_idx, cluster_size, temperature, stick) result(rate)
    ! k_N = v_thermal * cross_section_N * stick_N
    ! with N the cluster size of the reactant
    use krome_constants
    use krome_commons
    use krome_getphys
    implicit none
    integer, parameter :: dp=kind(0.d0) ! double precision

    integer, intent(in) :: monomer_idx
    integer, intent(in) :: cluster_size
    real(dp), intent(in) :: temperature
    real(dp), intent(in), optional :: stick
    real(dp) :: rate

    real(dp) :: v_thermal
    real(dp) :: cross_section
    real(dp) :: stick_coefficient
    real(dp) :: monomer_radius
    real(dp) :: cluster_radius
    real(dp) :: inverse_monomer_mass
    real(dp) :: inverse_cluster_mass
    real(dp) :: inverse_reduced_mass
    real(dp) :: inverse_mass(nspec)

    inverse_mass(:) = get_imass()

    ! References in kromelib.py
    monomer_radius = 7.5765e-09_dp ! SIO in cm

    inverse_monomer_mass = inverse_mass(monomer_idx)
    inverse_cluster_mass = 1._dp/cluster_size * inverse_monomer_mass
    inverse_reduced_mass = inverse_monomer_mass + inverse_cluster_mass

    v_thermal = sqrt(8._dp * boltzmann_erg * temperature &
        * inverse_reduced_mass / pi )

    ! Assuming cluster volume is proportional to monomer volume
    ! V_N = N * V_1, and both are considered as a hypothetical sphere
    cluster_radius = monomer_radius * cluster_size**(1._dp/3._dp)

    ! Geometrical cross section
    cross_section = pi * (monomer_radius + cluster_radius)**2._dp

    ! Sticking coefficiet is set to one for simplicity
    if(present(stick)) then
      stick_coefficient = stick
    else
      stick_coefficient = 1._dp
    end if

    rate = v_thermal * cross_section * stick_coefficient

  end function cluster_growth_rate

  function general_cluster_growth_rate(monomer_idx, cluster1_size, cluster2_size,&
        temperature, stick) result(rate)
    ! k_N = v_thermal * cross_section_N * stick_N
    ! with N the cluster size of the reactant
    use krome_constants
    use krome_commons
    use krome_getphys
    implicit none
    integer, parameter :: dp=kind(0.d0) ! double precision

    integer, intent(in) :: monomer_idx
    integer, intent(in) :: cluster1_size
    integer, intent(in) :: cluster2_size
    real(dp), intent(in) :: temperature
    real(dp), intent(in), optional :: stick
    real(dp) :: rate

    real(dp) :: v_thermal
    real(dp) :: cross_section
    real(dp) :: stick_coefficient
    real(dp) :: monomer_radius
    real(dp) :: cluster1_radius
    real(dp) :: cluster2_radius
    real(dp) :: inverse_monomer_mass
    real(dp) :: inverse_cluster1_mass
    real(dp) :: inverse_cluster2_mass
    real(dp) :: inverse_reduced_mass
    real(dp) :: inverse_mass(nspec)

    inverse_mass(:) = get_imass()

    ! References in kromelib.py
    monomer_radius = 7.5765e-09_dp ! SIO in cm

    inverse_monomer_mass = inverse_mass(monomer_idx)
    inverse_cluster1_mass = 1._dp/cluster1_size * inverse_monomer_mass
    inverse_cluster2_mass = 1._dp/cluster2_size * inverse_monomer_mass
    inverse_reduced_mass = inverse_cluster1_mass + inverse_cluster2_mass

    v_thermal = sqrt(8._dp * boltzmann_erg * temperature &
        * inverse_reduced_mass / pi )

    ! Assuming cluster volume is proportional to monomer volume
    ! V_N = N * V_1, and both are considered as a hypothetical sphere
    cluster1_radius = monomer_radius * cluster1_size**(1._dp/3._dp)
    cluster2_radius = monomer_radius * cluster2_size**(1._dp/3._dp)

    ! Geometrical cross section
    cross_section = pi * (cluster1_radius + cluster2_radius)**2._dp

    ! Sticking coefficiet is set to one for simplicity
    if(present(stick)) then
      stick_coefficient = stick
    else
      stick_coefficient = 1._dp
    end if

    rate = v_thermal * cross_section * stick_coefficient

  end function general_cluster_growth_rate

  !***********************************
  subroutine init_exp_table()
    use krome_commons
    implicit none
    integer::i
    real*8::a

    do i=1,exp_table_na
      a = (i-1)*(exp_table_aMax-exp_table_aMin)/(exp_table_na-1) + exp_table_aMin
      exp_table(i) = exp(-a)
    end do

  end subroutine init_exp_table

  !***************************
  !get the index of the specie name
  function get_index(name)
    use krome_commons
    use krome_getphys
    integer::get_index,i
    character*16::names(nspec)
    character*(*)::name
    names(:) = get_names()
    get_index = -1 !default index
    !loop on species to found the specie named name
    do i=1,nspec
      !when found store and break loop
      if(trim(names(i))== trim(name)) then
        get_index = i !store index
        exit
      end if
    end do

    !error if species not found
    if(get_index<0) then
      print *,"ERROR: can't find the index of ",name
      stop
    end if

  end function get_index

  !*****************************
  !computes revers kinetics from reaction and
  ! product indexes
  ! k_rev = k_for * revKc
  ! Note that reaction constant revKc is calculated with
  ! reactants and products from reverse reaction
  function revKc(Tgas,ridx,pidx)
    use krome_constants
    use krome_commons
    implicit none
    real*8::revKc,Tgas,dgibss,stoichiometricChange
    integer::ridx(:),pidx(:),i

    ! when considering forward reaction:
    ! Kc = (P°)**(p+p-r-r) * exp(-dGibss_forward°)
    ! where ° means at standard conditions of
    ! P° = 1 bar = (kb*T/1e6) dyn/cm^2 (cgs)
    ! when considering reverse:
    ! 1/Kc = revKc = (kb*T/1e6)**(p+p-r-r) * exp(-dGibss_reverse°)
    ! kb*T/1e6 is to go from 1 atm pressure to number density cm^-3
    ! When not at standard pressure this does not change:
    ! revKc = P**(p+p-r-r) *exp(-dGibss_reverse° - (p+p-r-r)*ln(P/P°))
    !       = (P°)**(p+p-r-r) * exp(-dGibss_reverse°)

    dgibss = 0.d0 ! Gibbs free energy/(R*T)
    stoichiometricChange = 0d0

    do i=1,size(pidx)
      dgibss = dgibss + revHS(Tgas,pidx(i))
      stoichiometricChange = stoichiometricChange + 1
    end do

    do i=1,size(ridx)
      dgibss = dgibss - revHS(Tgas,ridx(i))
      stoichiometricChange = stoichiometricChange - 1
    end do

    revKc = (boltzmann_erg * Tgas * 1e-6)**(-stoichiometricChange)&
        * exp(-dgibss)

  end function revKc

  !*****************************
  !compute H-S for species with index idx
  ! when temperature is Tgas
  function revHS(Tgas,idx)
    use krome_commons
    use krome_constants
    use krome_fit
    real*8::revHS,Tgas,Tgas2,Tgas3,Tgas4,invT,lnT,H,S
    real*8::Tnist,Tnist2,Tnist3,Tnist4,invTnist,invTnist2,lnTnist
    real*8::p1_nasa(659,7), p2_nasa(659,7), Tlim_nasa(659,3), p(7)
    real*8::p1_nist(659,7), p2_nist(659,7), Tlim_nist(659,3)
    integer::idx

    p(:) = 0.d0
    p1_nasa(:,:) = 0.d0
    p2_nasa(:,:) = 0.d0
    Tlim_nasa(:,:) = 0.d0
    p1_nist(:,:) = 0.d0
    p2_nist(:,:) = 0.d0
    Tlim_nist(:,:) = 0.d0
    Tgas2 = Tgas * Tgas
    Tgas3 = Tgas2 * Tgas
    Tgas4 = Tgas3 * Tgas
    invT = 1d0/Tgas
    lnT = log(Tgas)
    ! NIST polynomials are quite differernt
    ! it doesn't like easy stuff...
    Tnist = Tgas * 1.d-3
    Tnist2 = Tnist * Tnist
    Tnist3 = Tnist2 * Tnist
    Tnist4 = Tnist3 * Tnist2
    invTnist = 1d0/Tnist
    invTnist2 = invTnist * invTnist
    lnTnist = log(Tnist)

    p1_nasa(idx_Hk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        15976.167d0,&
        -1.1390139d0/)
    p1_nasa(idx_Ck,:)  = (/2.50025151d0,&
        -1.19774349d-06,&
        2.28919443d-09,&
        -1.98276803d-12,&
        6.44398056d-16,&
        70064.893d0,&
        4.87847086d0/)
    p1_nasa(idx_Ok,:)  = (/2.90805921d0,&
        -0.00169804907d0,&
        2.98069955d-06,&
        -2.43835127d-09,&
        7.61229311d-13,&
        11435.7717d0,&
        2.80339097d0/)
    p1_nasa(idx_OHk,:)  = (/3.43126659d0,&
        0.000631146866d0,&
        -1.92914359d-06,&
        2.40618712d-09,&
        -8.66679361d-13,&
        -18508.5918d0,&
        1.07990541d0/)
    p1_nasa(idx_C,:)  = (/2.5542395d0,&
        -0.00032153772d0,&
        7.3379223d-07,&
        -7.3223487d-10,&
        2.6652144d-13,&
        85442.681d0,&
        4.5313085d0/)
    p1_nasa(idx_H,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25473.66d0,&
        -0.44668285d0/)
    p1_nasa(idx_HE,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        -745.375d0,&
        0.928723974d0/)
    p1_nasa(idx_N,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        56104.638d0,&
        4.1939088d0/)
    p1_nasa(idx_O,:)  = (/3.1682671d0,&
        -0.00327931884d0,&
        6.64306396d-06,&
        -6.12806624d-09,&
        2.11265971d-12,&
        29122.2592d0,&
        2.05193346d0/)
    p1_nasa(idx_CH,:)  = (/3.4897583d0,&
        0.0003243216d0,&
        -1.6899751d-06,&
        3.162842d-09,&
        -1.4061803d-12,&
        70660.755d0,&
        2.0842841d0/)
    p1_nasa(idx_CO,:)  = (/3.5795335d0,&
        -0.00061035369d0,&
        1.0168143d-06,&
        9.0700586d-10,&
        -9.0442449d-13,&
        -14344.086d0,&
        3.5084093d0/)
    p1_nasa(idx_H2,:)  = (/2.34433112d0,&
        0.00798052075d0,&
        -1.9478151d-05,&
        2.01572094d-08,&
        -7.37611761d-12,&
        -917.935173d0,&
        0.683010238d0/)
    p1_nasa(idx_N2,:)  = (/3.53100528d0,&
        -0.000123660988d0,&
        -5.02999433d-07,&
        2.43530612d-09,&
        -1.40881235d-12,&
        -1046.97628d0,&
        2.96747038d0/)
    p1_nasa(idx_NO,:)  = (/4.21859896d0,&
        -0.00463988124d0,&
        1.10443049d-05,&
        -9.34055507d-09,&
        2.80554874d-12,&
        9845.09964d0,&
        2.28061001d0/)
    p1_nasa(idx_O2,:)  = (/3.78245636d0,&
        -0.00299673416d0,&
        9.84730201d-06,&
        -9.68129509d-09,&
        3.24372837d-12,&
        -1063.94356d0,&
        3.65767573d0/)
    p1_nasa(idx_OH,:)  = (/3.99198424d0,&
        -0.00240106655d0,&
        4.61664033d-06,&
        -3.87916306d-09,&
        1.36319502d-12,&
        3368.89836d0,&
        -0.103998477d0/)
    p1_nasa(idx_SO,:)  = (/3.61859514d0,&
        -0.00232173768d0,&
        1.16462669d-05,&
        -1.4209251d-08,&
        5.6076537d-12,&
        -480.621641d0,&
        6.36504115d0/)
    p1_nasa(idx_CH2,:)  = (/3.84261832d0,&
        -7.36676871d-06,&
        6.16970693d-06,&
        -6.96689962d-09,&
        2.64620979d-12,&
        45863.1528d0,&
        1.2758447d0/)
    p1_nasa(idx_CO2,:)  = (/2.356813d0,&
        0.0089841299d0,&
        -7.1220632d-06,&
        2.4573008d-09,&
        -1.4288548d-13,&
        -48371.971d0,&
        9.9009035d0/)
    p1_nasa(idx_H2O,:)  = (/4.1986352d0,&
        -0.0020364017d0,&
        6.5203416d-06,&
        -5.4879269d-09,&
        1.771968d-12,&
        -30293.726d0,&
        -0.84900901d0/)
    p1_nasa(idx_HCO,:)  = (/4.36380907d0,&
        -0.00535204137d0,&
        2.31954508d-05,&
        -2.6610904d-08,&
        1.02711962d-11,&
        25010.8717d0,&
        2.98106307d0/)
    p1_nasa(idx_N2O,:)  = (/2.2571502d0,&
        0.011304728d0,&
        -1.3671319d-05,&
        9.6819803d-09,&
        -2.9307182d-12,&
        8741.7746d0,&
        10.757992d0/)
    p1_nasa(idx_NO2,:)  = (/3.9440312d0,&
        -0.001585429d0,&
        1.6657812d-05,&
        -2.0475426d-08,&
        7.8350564d-12,&
        2896.618d0,&
        6.3119919d0/)
    p1_nasa(idx_SO2,:)  = (/3.67480752d0,&
        0.00228302107d0,&
        8.46893049d-06,&
        -1.36562039d-08,&
        5.76271873d-12,&
        -36945.5073d0,&
        7.9686643d0/)
    p1_nasa(idx_C3H,:)  = (/3.34917187d0,&
        0.0165822626d0,&
        -2.77115653d-05,&
        2.51382364d-08,&
        -8.85285352d-12,&
        84986.3168d0,&
        6.80362439d0/)
    p1_nasa(idx_CH3,:)  = (/3.6571797d0,&
        0.0021265979d0,&
        5.4583883d-06,&
        -6.6181003d-09,&
        2.4657074d-12,&
        16422.716d0,&
        1.6735354d0/)
    p1_nasa(idx_H2CO,:)  = (/4.65733258d0,&
        -0.00953742306d0,&
        4.04679152d-05,&
        -4.45317569d-08,&
        1.64761516d-11,&
        13861.5127d0,&
        1.97860732d0/)
    p1_nasa(idx_H2O2,:)  = (/4.31515149d0,&
        -0.000847390622d0,&
        1.76404323d-05,&
        -2.26762944d-08,&
        9.08950158d-12,&
        -17706.7437d0,&
        3.27373319d0/)
    p1_nasa(idx_CH4,:)  = (/5.14825732d0,&
        -0.013700241d0,&
        4.93749414d-05,&
        -4.91952339d-08,&
        1.70097299d-11,&
        -10245.3222d0,&
        -4.63322726d0/)
    p1_nasa(idx_O3,:)  = (/3.40738221d0,&
        0.00205379063d0,&
        1.38486052d-05,&
        -2.23311542d-08,&
        9.76073226d-12,&
        15864.4979d0,&
        8.2824758d0/)
    p1_nasa(idx_Cj,:)  = (/2.61332254d0,&
        -0.000540148065d0,&
        1.03037233d-06,&
        -8.90092552d-10,&
        2.88500586d-13,&
        216862.274d0,&
        3.8345479d0/)
    p1_nasa(idx_FEj,:)  = (/2.76418106d0,&
        0.00286948238d0,&
        -7.61235651d-06,&
        8.18183334d-09,&
        -3.11792199d-12,&
        141159.039d0,&
        5.53997981d0/)
    p1_nasa(idx_Hj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184021.488d0,&
        -1.14064664d0/)
    p1_nasa(idx_HEj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        285323.374d0,&
        1.62166556d0/)
    p1_nasa(idx_Oj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        187935.284d0,&
        4.39337676d0/)
    p1_nasa(idx_COj,:)  = (/3.77061642d0,&
        -0.00201773246d0,&
        4.61081738d-06,&
        -2.99175463d-09,&
        6.06065045d-13,&
        149006.795d0,&
        3.38129783d0/)
    p1_nasa(idx_H2j,:)  = (/3.77256072d0,&
        -0.0019574659d0,&
        4.54812047d-06,&
        -2.82152141d-09,&
        5.33969209d-13,&
        178694.654d0,&
        -3.96609192d0/)
    p1_nasa(idx_O2j,:)  = (/4.61017167d0,&
        -0.00635951952d0,&
        1.42425624d-05,&
        -1.20997923d-08,&
        3.70956878d-12,&
        139742.229d0,&
        -0.201326941d0/)
    p1_nasa(idx_OHj,:)  = (/3.50502572d0,&
        0.000241313747d0,&
        -1.42200948d-06,&
        2.64780232d-09,&
        -1.17038711d-12,&
        155210.676d0,&
        1.97907627d0/)
    p1_nasa(idx_H2Oj,:)  = (/4.02465912d0,&
        -0.00108851414d0,&
        5.13576558d-06,&
        -4.40027838d-09,&
        1.40726746d-12,&
        116895.616d0,&
        0.699968812d0/)
    p1_nasa(idx_H3Oj,:)  = (/3.79295251d0,&
        -0.000910852723d0,&
        1.16363521d-05,&
        -1.21364865d-08,&
        4.26159624d-12,&
        71402.7518d0,&
        1.47156927d0/)
    p1_nasa(idx_H3j,:)  = (/4.1795698d0,&
        -0.000868875627d0,&
        -1.09017371d-07,&
        4.13349766d-09,&
        -2.37877027d-12,&
        132635.537d0,&
        -5.838001d0/)
    p2_nasa(idx_Hk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        15976.167d0,&
        -1.1390139d0/)
    p2_nasa(idx_Ck,:)  = (/2.50001597d0,&
        -1.71721376d-08,&
        6.9283294d-12,&
        -1.20607892d-15,&
        7.60308635d-20,&
        70064.9324d0,&
        4.87955907d0/)
    p2_nasa(idx_Ok,:)  = (/2.54474869d0,&
        -4.66695513d-05,&
        1.84912357d-08,&
        -3.18159223d-12,&
        1.98962956d-16,&
        11504.2089d0,&
        4.52131015d0/)
    p2_nasa(idx_OHk,:)  = (/2.80023747d0,&
        0.00113380509d0,&
        -2.99666184d-07,&
        4.01911483d-11,&
        -1.78988913d-15,&
        -18253.5298d0,&
        4.6939462d0/)
    p2_nasa(idx_C,:)  = (/2.605583d0,&
        -0.00019593434d0,&
        1.0673722d-07,&
        -1.642394d-11,&
        8.187058d-16,&
        85411.742d0,&
        4.1923868d0/)
    p2_nasa(idx_H,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25473.66d0,&
        -0.44668285d0/)
    p2_nasa(idx_HE,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        -745.375d0,&
        0.928723974d0/)
    p2_nasa(idx_N,:)  = (/2.4159429d0,&
        0.00017489065d0,&
        -1.1902369d-07,&
        3.0226244d-11,&
        -2.0360983d-15,&
        56133.775d0,&
        4.6496095d0/)
    p2_nasa(idx_O,:)  = (/2.54363697d0,&
        -2.73162486d-05,&
        -4.1902952d-09,&
        4.95481845d-12,&
        -4.79553694d-16,&
        29226.012d0,&
        4.92229457d0/)
    p2_nasa(idx_CH,:)  = (/2.5209369d0,&
        0.0017653639d0,&
        -4.614766d-07,&
        5.9289675d-11,&
        -3.3474501d-15,&
        70994.878d0,&
        7.4051829d0/)
    p2_nasa(idx_CO,:)  = (/3.0484859d0,&
        0.0013517281d0,&
        -4.8579405d-07,&
        7.8853644d-11,&
        -4.6980746d-15,&
        -14266.117d0,&
        6.0170977d0/)
    p2_nasa(idx_H2,:)  = (/2.93286575d0,&
        0.000826608026d0,&
        -1.46402364d-07,&
        1.54100414d-11,&
        -6.888048d-16,&
        -813.065581d0,&
        -1.02432865d0/)
    p2_nasa(idx_N2,:)  = (/2.95257637d0,&
        0.0013969004d0,&
        -4.92631603d-07,&
        7.86010195d-11,&
        -4.60755204d-15,&
        -923.948688d0,&
        5.87188762d0/)
    p2_nasa(idx_NO,:)  = (/3.26071234d0,&
        0.00119101135d0,&
        -4.29122646d-07,&
        6.94481463d-11,&
        -4.03295681d-15,&
        9921.43132d0,&
        6.36900518d0/)
    p2_nasa(idx_O2,:)  = (/3.66096065d0,&
        0.000656365811d0,&
        -1.41149627d-07,&
        2.05797935d-11,&
        -1.29913436d-15,&
        -1215.97718d0,&
        3.41536279d0/)
    p2_nasa(idx_OH,:)  = (/2.83853033d0,&
        0.00110741289d0,&
        -2.94000209d-07,&
        4.20698729d-11,&
        -2.4228989d-15,&
        3697.80808d0,&
        5.84494652d0/)
    p2_nasa(idx_SO,:)  = (/3.96894225d0,&
        0.000377296831d0,&
        7.67102696d-09,&
        -1.37544433d-11,&
        1.37139416d-15,&
        -728.571725d0,&
        3.73493087d0/)
    p2_nasa(idx_CH2,:)  = (/3.11049513d0,&
        0.00373779517d0,&
        -1.37371977d-06,&
        2.23054839d-10,&
        -1.33567178d-14,&
        45971.5953d0,&
        4.62796405d0/)
    p2_nasa(idx_CO2,:)  = (/4.6365111d0,&
        0.0027414569d0,&
        -9.9589759d-07,&
        1.6038666d-10,&
        -9.1619857d-15,&
        -49024.904d0,&
        -1.9348955d0/)
    p2_nasa(idx_H2O,:)  = (/2.6770389d0,&
        0.0029731816d0,&
        -7.7376889d-07,&
        9.4433514d-11,&
        -4.2689991d-15,&
        -29885.894d0,&
        6.88255d0/)
    p2_nasa(idx_HCO,:)  = (/4.23892214d0,&
        0.0019657617d0,&
        -3.82075171d-07,&
        4.80137647d-11,&
        -3.11176347d-15,&
        24726.1645d0,&
        1.99698242d0/)
    p2_nasa(idx_N2O,:)  = (/4.8230729d0,&
        0.0026270251d0,&
        -9.5850872d-07,&
        1.6000712d-10,&
        -9.7752302d-15,&
        8073.4047d0,&
        -2.2017208d0/)
    p2_nasa(idx_NO2,:)  = (/4.884754d0,&
        0.0021723955d0,&
        -8.2806909d-07,&
        1.574751d-10,&
        -1.0510895d-14,&
        2316.4982d0,&
        -0.11741695d0/)
    p2_nasa(idx_SO2,:)  = (/5.38423482d0,&
        0.0016793056d0,&
        -6.32062944d-07,&
        1.08465348d-10,&
        -6.66890336d-15,&
        -37606.7022d0,&
        -1.83130517d0/)
    p2_nasa(idx_C3H,:)  = (/6.14184491d0,&
        0.00339661013d0,&
        -1.21915444d-06,&
        1.97782838d-10,&
        -1.18312807d-14,&
        84422.5753d0,&
        -6.44480148d0/)
    p2_nasa(idx_CH3,:)  = (/2.9781206d0,&
        0.005797852d0,&
        -1.97558d-06,&
        3.072979d-10,&
        -1.7917416d-14,&
        16509.513d0,&
        4.7224799d0/)
    p2_nasa(idx_H2CO,:)  = (/3.65237205d0,&
        0.0055580706d0,&
        -1.97617181d-06,&
        3.16823378d-10,&
        -1.88747598d-14,&
        13553.6156d0,&
        4.2214084d0/)
    p2_nasa(idx_H2O2,:)  = (/4.57977305d0,&
        0.00405326003d0,&
        -1.2984473d-06,&
        1.982114d-10,&
        -1.13968792d-14,&
        -18007.1775d0,&
        0.664970694d0/)
    p2_nasa(idx_CH4,:)  = (/1.911786d0,&
        0.0096026796d0,&
        -3.38387841d-06,&
        5.3879724d-10,&
        -3.19306807d-14,&
        -10099.2136d0,&
        8.48241861d0/)
    p2_nasa(idx_O3,:)  = (/12.3302914d0,&
        -0.0119324783d0,&
        7.98741278d-06,&
        -1.77194552d-09,&
        1.26075824d-13,&
        12675.5831d0,&
        -40.8823374d0/)
    p2_nasa(idx_Cj,:)  = (/2.50827618d0,&
        -1.04354146d-05,&
        5.16160809d-09,&
        -1.14187475d-12,&
        9.43539946d-17,&
        216879.645d0,&
        4.3188599d0/)
    p2_nasa(idx_FEj,:)  = (/3.33602399d0,&
        -0.000272549262d0,&
        8.05440344d-09,&
        1.51229089d-11,&
        -1.43376595d-15,&
        141036.455d0,&
        2.86476968d0/)
    p2_nasa(idx_Hj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184021.488d0,&
        -1.14064664d0/)
    p2_nasa(idx_HEj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        285323.374d0,&
        1.62166556d0/)
    p2_nasa(idx_Oj,:)  = (/2.48542028d0,&
        2.56978695d-05,&
        -1.28833378d-08,&
        1.65525487d-12,&
        1.09933344d-16,&
        187940.874d0,&
        4.47425446d0/)
    p2_nasa(idx_COj,:)  = (/2.93062935d0,&
        0.00156033262d0,&
        -6.16246355d-07,&
        1.09957336d-10,&
        -6.66119284d-15,&
        149147.222d0,&
        7.3384673d0/)
    p2_nasa(idx_H2j,:)  = (/3.44204765d0,&
        0.000599083239d0,&
        6.69133685d-08,&
        -3.43574373d-11,&
        1.97626599d-15,&
        178650.236d0,&
        -2.79499055d0/)
    p2_nasa(idx_O2j,:)  = (/3.31675922d0,&
        0.00111522244d0,&
        -3.83492556d-07,&
        5.72784687d-11,&
        -2.77648381d-15,&
        139876.823d0,&
        5.44726469d0/)
    p2_nasa(idx_OHj,:)  = (/2.68358996d0,&
        0.00157006435d0,&
        -5.39972815d-07,&
        9.37643877d-11,&
        -5.70068067d-15,&
        155479.296d0,&
        6.44375894d0/)
    p2_nasa(idx_H2Oj,:)  = (/3.31570445d0,&
        0.00210648746d0,&
        -3.76341515d-07,&
        3.47525972d-11,&
        -1.70335643d-15,&
        117017.475d0,&
        4.03220514d0/)
    p2_nasa(idx_H3Oj,:)  = (/2.49647765d0,&
        0.0057284484d0,&
        -1.83953239d-06,&
        2.73577348d-10,&
        -1.54093917d-14,&
        71624.4227d0,&
        7.45850493d0/)
    p2_nasa(idx_H3j,:)  = (/2.01435718d0,&
        0.00415925769d0,&
        -1.42664877d-06,&
        2.22372739d-10,&
        -1.29346518d-14,&
        133230.507d0,&
        5.46168967d0/)
    Tlim_nasa(idx_Hk,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Ck,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Ok,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OHk,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_C,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HE,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_NO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OH,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_SO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CO2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HCO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N2O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_NO2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_SO2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_C3H,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH3,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2CO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2O2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH4,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O3,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Cj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_FEj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Hj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HEj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_COj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2j,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O2j,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OHj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H3Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H3j,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    p1_nist(idx_CL,:)  = (/13.38298d0,&
        42.33999d0,&
        -64.74656d0,&
        32.99532d0,&
        0.063319d0,&
        116.1491d0,&
        171.7038d0/)
    p2_nist(idx_CL,:)  = (/23.26597d0,&
        -1.555939d0,&
        0.34691d0,&
        -0.025961d0,&
        0.153212d0,&
        114.6604d0,&
        193.8882d0/)
    Tlim_nist(idx_CL,:)  = (/298.0d0,&
        600.0d0,&
        6000.0d0/)

    ! pick NASA data if present for species
    if (Tlim_nasa(idx,2) /= 0.d0) then
      !select set of NASA polynomials using temperature
      if(Tlim_nasa(idx,1).le.Tgas .and. Tgas.le.Tlim_nasa(idx,2)) then
        p(:) = p1_nasa(idx,:)

      else if(Tlim_nasa(idx,2)<Tgas .and. Tgas.le.Tlim_nasa(idx,3)) then
        p(:) = p2_nasa(idx,:)

        ! currently no option when Tgas not in Tlim range p(:) = 0
      end if

      !compute NASA polynomials for enthalpy and enthropy (unitless)
      H = p(1) + p(2)*0.5d0*Tgas + p(3)*Tgas2/3.d0 + p(4)*Tgas3*0.25d0 + &
          p(5)*Tgas4*0.2d0 + p(6)*invT
      S = p(1)*lnT + p(2)*Tgas + p(3)*Tgas2*0.5d0 + p(4)*Tgas3/3.d0 + &
          p(5)*Tgas4*0.25d0 + p(7)

      revHS = H - S

      ! else pick NIST data (if present)
    else if (Tlim_nist(idx,2) /= 0.d0) then
      if (Tlim_nist(idx,1) < Tgas .and. Tgas < Tlim_nist(idx,2)) then
        p(:) = p1_nist(idx,:)

      else if (Tlim_nist(idx,2) < Tgas .and. Tgas < Tlim_nist(idx,3)) then
        p(:) = p2_nist(idx,:)

        ! currently no option when Tgas not in Tlim range p(:) = 0
      end if

      !compute NIST polynomials for enthalpy and enthropy
      ! H in (kJ/mol)
      H = p(1)*Tnist + p(2)*0.5d0*Tnist2 + p(3)*Tnist3/3.d0 + p(4)*Tnist4*0.25d0&
          - p(5)*invTnist + p(6)
      !  Unitsless
      H = H / (Rgas_kJ * Tgas)

      ! S in (J/mol*K)
      S = p(1)*lnTnist + p(2)*Tnist + p(3)*Tnist2*0.5d0 + p(4)*Tnist3/3.d0&
          - p(5)*invTnist2*0.5d0 + p(7)
      !  Unitless. Note: do not use Tnist
      S = S / Rgas_J

      revHS = H - S

      ! return zero is no data exists
    else
      print *, "No thermochemical data of species index", idx
      revHS = 0.d0

    end if

  end function revHS

  !******************************
  subroutine print_best_flux(n,Tgas,nbestin)
    !print the first nbestin fluxes
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea)
    integer::nbest,idx(nrea),i,nbestin
    character*50::name(nrea)

    nbest = min(nbestin,nrea) !cannot exceed the number of reactions

    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nbest
      print '(I8,a1,a50,E17.8)',idx(i)," ",name(idx(i)),flux(idx(i))
    end do

  end subroutine print_best_flux

  !******************************
  subroutine print_best_flux_frac(n,Tgas,frac)
    !print the first nbestin fluxes
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea),frac
    integer::idx(nrea),i
    character*50::name(nrea)

    if(frac>1d0) then
      print *,"ERROR: fraction in krome_print_best_flux should be <=1!"
      stop
    end if

    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nrea
      if(flux(idx(i))<flux(idx(1))*frac) exit
      print '(I8,a1,a50,E17.8)',idx(i)," ",name(idx(i)),flux(idx(i))
    end do

  end subroutine print_best_flux_frac

  !******************************
  subroutine print_best_flux_spec(n,Tgas,nbestin,idx_found)
    !print the first nbestin fluxes for the reactions
    ! that contains the species with index idx_found
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea),maxflux
    integer::nbest,idx(nrea),i,nbestin,idx_found
    character*50::name(nrea)
    logical::found

    nbest = min(nbestin,nrea) !cannot exceed the number of reactions
    maxflux = 0d0
    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names
    do i=1,nrea
      found = .false.
      if(arr_r1(i) == idx_found) found = .true.
      if(arr_r2(i) == idx_found) found = .true.
      if(arr_p1(i) == idx_found) found = .true.
      if(arr_p2(i) == idx_found) found = .true.
      if(arr_p3(i) == idx_found) found = .true.
      if(arr_p4(i) == idx_found) found = .true.
      maxflux = max(maxflux,flux(i))
      if(.not.found) flux(i) = 0d0
    end do

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nbest
      print '(I8,a1,a50,2E17.8)',idx(i)," ",name(idx(i)),flux(idx(i)),&
          flux(idx(i))/maxflux
    end do

  end subroutine print_best_flux_spec

  !*****************************
  function idx_sort(fin)
    !sorting algorithm: requires an array of real values fin
    ! and returns the sorted index list. descending.
    ! bubblesort: not very efficient, replace with what you prefer
    implicit none
    real*8::fin(:),f(size(fin)),ftmp
    integer::idx_sort(size(fin)),n,itmp,i
    logical::found

    f(:) = fin(:) !copy to local

    n = size(f)
    !init indexes
    do i=1,n
      idx_sort(i) = i
    end do

    !loop to sort
    do
      found = .false. !swapped something flag
      do i=2,n
        !> for descending, < for ascending
        if(f(i)>f(i-1)) then
          found = .true.
          !swap real value
          ftmp = f(i)
          f(i) = f(i-1)
          f(i-1) = ftmp
          !swap index
          itmp = idx_sort(i)
          idx_sort(i) = idx_sort(i-1)
          idx_sort(i-1) = itmp
        end if
      end do
      !if nothing swapped exit
      if(.not.found) exit
    end do

  end function idx_sort

  !******************************
  function get_flux(n,Tgas)
    !get the flux k*n*n*... of the rates
    use krome_commons
    implicit none
    integer::i
    integer::r1,r2
    real*8::get_flux(nrea),n(nspec),k(nrea),rrmax,Tgas

    k(:) = coe(n(:))
    rrmax = 0.d0
    n(idx_dummy) = 1.d0
    n(idx_g) = 1.d0
    n(idx_CR) = 1.d0
    do i=1,nrea
      r1 = arr_r1(i)
      r2 = arr_r2(i)
      arr_flux(i) = k(i)*n(r1)*n(r2)
    end do
    get_flux(:) = arr_flux(:)

  end function get_flux

  !*****************************
  subroutine load_arrays()
    !load the array containing reactants
    ! and product index
    use krome_commons

    arr_r1(1:5639) = (/9,10,11,12,13,14,15,16,17,18,19,20,21,22&
        ,23,24,25,26,26,27,28,29,29,29,29,30,31,32,33,34,35,36,36,37&
        ,38,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,54,55&
        ,55,56,57,58,59,59,60,61,61,62,63,64,65,66,67,68,69,70,71,72&
        ,72,73,74,74,75,76,76,77,78,79,80,81,82,83,83,84,85,86,87,88&
        ,89,90,90,91,92,93,94,95,96,97,97,97,98,99,100,101,102,103&
        ,103,104,105,106,107,108,109,110,111,111,112,113,114,115,116&
        ,117,118,119,120,121,121,122,122,123,124,124,124,125,126,127&
        ,128,128,128,128,129,129,129,130,131,132,132,133,134,135,136&
        ,136,137,138,139,140,141,142,143,144,145,146,147,148,149,149&
        ,150,151,152,153,154,155,156,157,158,159,160,161,438,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,400,400,400,400,400,400,400,400,400,400,400&
        ,400,400,400,400,401,401,402,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,403,403&
        ,403,403,403,403,403,403,403,403,403,403,403,403,403,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,404,404,404,404,404,404,404,404,404,404,404,404&
        ,404,404,404,405,405,406,406,406,406,406,406,406,406,406,406&
        ,406,406,406,406,406,406,406,406,406,406,406,406,406,406,406&
        ,406,406,406,406,406,406,406,406,406,406,406,406,406,406,406&
        ,406,406,406,406,406,406,406,406,406,408,408,408,408,408,408&
        ,408,408,408,408,408,408,408,408,408,408,408,408,408,408,408&
        ,408,408,408,408,408,408,408,408,408,408,408,408,408,408,408&
        ,408,408,408,408,408,408,409,409,409,409,409,409,409,409,409&
        ,409,409,410,410,410,410,410,410,410,410,410,410,410,410,410&
        ,410,410,410,410,410,410,410,410,410,410,410,410,410,410,410&
        ,410,410,410,410,410,410,410,410,410,411,411,411,411,411,411&
        ,411,411,411,411,411,411,411,411,411,411,411,411,411,411,411&
        ,411,411,440,440,440,440,440,440,440,440,440,440,440,440,440&
        ,440,440,440,440,440,440,440,440,440,440,440,440,440,440,440&
        ,440,440,440,438,438,438,438,438,438,438,438,438,438,438,438&
        ,438,438,438,438,438,438,438,438,438,438,438,438,438,438,438&
        ,438,438,438,438,438,438,438,438,438,438,438,438,438,438,438&
        ,438,438,438,438,438,438,438,438,438,438,438,444,444,444,444&
        ,444,444,444,444,444,444,444,444,444,444,444,444,444,444,444&
        ,444,444,444,444,444,444,444,444,444,444,444,444,412,412,412&
        ,412,412,412,412,412,412,412,412,412,412,412,412,412,412,412&
        ,412,412,412,412,412,412,412,412,412,412,412,412,412,412,412&
        ,412,412,412,442,442,443,443,443,443,443,443,443,443,443,413&
        ,413,413,413,413,413,413,413,413,413,413,413,413,413,413,413&
        ,413,413,413,413,413,413,413,413,413,413,413,413,413,413,413&
        ,413,413,413,413,413,413,413,413,413,413,413,413,413,413,413&
        ,413,413,413,523,555,555,524,524,524,524,524,524,524,524,524&
        ,524,524,524,524,524,524,524,524,524,524,524,553,553,553,553&
        ,553,553,553,553,553,553,553,553,553,553,553,553,553,553,553&
        ,553,553,553,553,553,553,553,553,553,553,553,553,553,553,553&
        ,553,553,553,525,525,525,525,525,525,525,525,525,525,525,525&
        ,525,525,525,525,525,525,525,525,525,525,525,525,525,525,525&
        ,525,525,525,525,525,525,525,525,525,525,525,525,525,525,525&
        ,414,414,414,414,445,415,415,415,415,415,415,415,415,415,415&
        ,415,415,415,415,415,415,415,415,415,415,415,415,415,415,415&
        ,415,415,415,526,526,526,526,526,526,526,526,526,526,526,526&
        ,526,526,526,526,526,526,526,526,526,526,526,526,526,526,526&
        ,526,526,526,526,526,526,526,526,526,526,526,526,526,526,526&
        ,526,526,526,446,446,446,446,446,446,446,446,446,446,446,446&
        ,446,446,528,528,528,528,448,448,448,529,529,529,529,529,449&
        ,530,530,530,530,530,530,530,530,530,530,530,530,530,530,450&
        ,450,451,451,451,451,451,451,456,456,456,456,456,456,456,456&
        ,456,456,456,456,456,456,456,456,456,456,456,456,456,456,456&
        ,456,456,456,456,456,456,456,458,458,458,458,458,458,458,458&
        ,458,458,452,452,416,416,416,416,416,416,416,416,416,416,416&
        ,416,416,416,416,416,416,416,416,416,416,416,416,416,416,416&
        ,416,416,416,416,416,416,465,459,459,459,459,554,554,554,554&
        ,554,554,554,554,554,554,554,554,554,582,582,531,531,531,531&
        ,531,531,531,531,531,531,531,531,531,531,531,531,531,531,531&
        ,531,531,531,531,531,531,531,531,531,531,531,531,531,531,531&
        ,531,531,531,531,531,531,417,417,417,417,417,417,417,417,417&
        ,417,417,417,417,417,417,417,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,579,579,579,579,579,579,579,579,579,579,579,579,579,579,579&
        ,464,464,464,464,464,464,464,464,464,464,464,464,464,464,464&
        ,464,464,464,464,464,464,464,464,464,464,464,464,464,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,418,418,418,418,418&
        ,418,418,418,418,418,418,418,418,418,418,460,460,460,419,419&
        ,419,489,489,489,489,489,489,489,489,489,489,489,489,489,489&
        ,489,489,489,489,489,420,420,420,420,420,420,420,420,420,420&
        ,420,420,420,420,420,420,420,420,420,420,532,532,532,457,457&
        ,457,463,559,559,559,559,559,559,559,559,559,559,559,559,559&
        ,559,559,559,559,559,559,559,559,559,559,559,559,628,421,421&
        ,421,421,421,421,421,421,421,421,421,421,421,421,421,421,421&
        ,421,421,421,421,421,421,421,421,421,421,421,421,421,421,421&
        ,421,421,560,560,580,580,580,580,580,580,580,580,580,580,580&
        ,580,580,580,580,580,580,580,580,580,580,580,580,422,534,534&
        ,534,533,533,461,466,466,466,466,466,462,556,556,556,423,423&
        ,423,423,423,423,423,423,423,423,423,423,423,423,423,423,423&
        ,423,423,423,423,423,423,423,423,423,423,423,423,423,423,423&
        ,423,423,423,423,423,423,423,423,423,423,423,423,423,423,423&
        ,423,423,423,423,423,423,423,423,423,423,423,423,423,423,423&
        ,423,423,423,423,423,423,423,423,423,423,423,423,423,423,423&
        ,423,423,423,423,467,467,467,467,467,467,467,467,467,467,467&
        ,467,467,467,467,467,467,467,467,467,467,467,467,467,467,467&
        ,467,467,486,468,476,476,476,476,476,476,476,424,424,424,424&
        ,424,424,424,424,424,424,424,424,424,424,424,424,424,424,424&
        ,424,424,424,424,424,424,424,424,424,424,424,424,424,424,424&
        ,424,424,424,424,424,424,424,424,424,424,424,424,424,424,424&
        ,424,424,424,424,424,424,424,424,424,424,424,424,424,424,424&
        ,424,424,424,424,424,424,424,424,424,424,424,473,473,473,473&
        ,473,473,473,473,473,473,473,471,471,471,471,471,471,471,471&
        ,471,471,471,471,471,471,471,471,471,471,471,471,471,471,471&
        ,471,471,471,545,572,572,572,572,572,572,572,572,572,572,572&
        ,572,572,572,572,572,572,572,572,572,572,572,572,572,572,572&
        ,572,572,572,572,572,572,572,572,572,572,572,572,572,572,572&
        ,572,572,572,572,572,572,572,572,572,572,572,572,572,572,572&
        ,572,572,572,572,572,572,572,572,572,573,573,573,573,573,539&
        ,539,539,539,539,539,539,539,597,597,425,425,425,425,425,425&
        ,425,425,425,425,425,425,425,425,425,425,425,425,425,425,425&
        ,485,594,474,474,477,477,477,477,477,477,595,568,568,479,479&
        ,479,479,479,479,479,479,479,479,479,479,479,479,479,479,479&
        ,479,479,479,479,479,479,479,479,479,479,479,479,479,479,479&
        ,479,479,479,479,479,479,479,479,479,479,479,479,479,479,479&
        ,479,479,479,479,479,479,479,479,479,479,479,479,479,479,480&
        ,481,480,481,480,481,480,481,480,481,480,481,480,481,480,481&
        ,480,481,480,481,480,480,481,480,481,480,480,480,481,480,481&
        ,480,481,481,480,481,480,481,480,481,480,481,480,481,480,481&
        ,480,481,480,481,480,481,480,481,481,480,480,481,480,481,480&
        ,481,480,481,480,481,480,481,480,481,488,488,482,482,482,482&
        ,482,482,482,482,482,482,482,482,482,482,482,482,482,482,482&
        ,482,482,482,482,487,487,487,487,487,483,541,541,541,541,541&
        ,541,541,541,541,541,541,541,541,541,541,541,541,541,541,541&
        ,541,541,541,541,541,431,431,431,431,431,544,544,492,490,493&
        ,493,543,543,543,543,428,429,429,429,429,429,429,429,429,429&
        ,429,429,429,429,429,429,429,429,429,429,429,429,429,429,429&
        ,429,429,429,429,429,429,429,429,429,429,429,429,429,429,429&
        ,429,429,429,429,429,429,429,429,429,429,429,429,495,496,495&
        ,496,495,496,495,496,495,496,495,496,495,496,495,496,495,496&
        ,495,495,495,496,495,495,495,496,495,496,495,496,495,496,495&
        ,496,495,496,495,496,495,496,495,496,495,496,495,496,495,496&
        ,495,496,495,496,430,430,430,430,430,430,430,430,430,430,430&
        ,430,430,430,430,430,430,430,430,430,430,430,430,430,430,430&
        ,430,430,497,497,497,497,639,498,581,581,581,581,581,581,581&
        ,581,581,581,581,581,581,581,581,581,581,581,581,581,581,581&
        ,581,581,581,581,581,637,569,570,605,606,606,606,500,500,500&
        ,500,500,500,500,500,500,500,500,500,500,500,500,500,434,434&
        ,434,434,434,434,434,434,434,503,503,503,503,503,503,503,503&
        ,503,503,503,503,503,503,503,503,503,503,503,503,502,502,502&
        ,502,502,502,502,502,502,502,502,502,502,502,502,502,502,502&
        ,502,502,502,502,507,504,504,504,505,575,575,640,609,611,611&
        ,611,611,611,612,612,612,514,514,514,514,515,515,515,515,515&
        ,515,515,515,515,515,515,515,515,515,515,508,508,508,508,642&
        ,509,641,615,630,548,516,516,516,547,547,547,547,547,547,547&
        ,547,547,547,547,549,512,513,519,622,558,520,520,520,520,521&
        ,521,521,521,518,518,518,518,645,621,557,557,557,557,551,522&
        ,522,522,550,550,550,552,624,623,623,625,635,412,556,444,578&
        ,578,3,3,3,2,2,5,5,5,5,6,6,400,400,400,400,400,400,400,400&
        ,400,400,403,407,407,409,410,411,411,411,411,411,411,524,414&
        ,446,529,452,418,418,422,423,423,423,423,423,423,467,467,467&
        ,467,467,467,424,424,424,424,424,424,424,424,424,424,473,572&
        ,572,474,477,479,480,481,482,431,546,495,496,430,430,430,430&
        ,430,497,498,581,500,503,503,503,502,514,515,633,557,3,3,3,3&
        ,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,5,5,5,5,5,5,5,5&
        ,7,7,7,7,7,7,4,4,6,6,6,6,17,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9&
        ,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9&
        ,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,12,12,12&
        ,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12&
        ,12,12,12,12,12,12,12,12,29,29,29,29,29,29,29,29,29,29,15,15&
        ,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15&
        ,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15&
        ,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,17,17,17,17&
        ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17&
        ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17&
        ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17&
        ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17&
        ,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,23,23,23,23,23&
        ,23,23,23,23,23,23,23,23,23,23,23,23,21,21,21,21,25,25,25,25&
        ,25,25,25,25,25,25,25,25,25,26,26,31,35,35,36,39,39,39,39,39&
        ,39,39,39,39,39,39,39,39,39,39,50,50,50,50,50,50,50,50,50,50&
        ,50,50,50,50,50,50,50,50,50,50,50,56,61,61,61,66,72,9,9,9,9,9&
        ,12,17,17,19,23,25,25,9,9,12,39,440,439,438,441,444,412,442&
        ,443,413,523,555,524,553,525,414,445,415,526,446,527,447,528&
        ,448,529,449,530,450,451,456,456,456,458,458,454,453,453,452&
        ,455,455,416,416,416,465,465,459,554,582,582,531,531,531,417&
        ,417,579,579,464,418,460,460,419,419,489,420,590,532,532,457&
        ,591,591,463,463,537,537,588,588,589,559,559,628,628,576,421&
        ,421,560,580,422,422,422,534,534,533,533,461,461,466,466,466&
        ,462,556,423,423,423,423,423,571,571,571,571,578,578,542,542&
        ,467,467,486,469,470,470,470,468,468,476,476,476,424,424,424&
        ,424,631,473,473,473,471,471,535,535,545,545,592,592,584,584&
        ,536,536,472,472,572,572,572,572,573,573,564,564,539,539,539&
        ,577,574,574,596,596,597,597,597,593,593,425,425,485,485,563&
        ,563,594,594,562,474,474,475,475,477,477,595,595,568,568,478&
        ,478,478,426,426,426,479,479,479,479,479,479,480,481,480,481&
        ,480,481,480,481,480,488,488,488,482,482,482,482,487,487,538&
        ,538,484,484,484,483,483,427,541,541,431,431,431,566,566,600&
        ,600,601,601,598,598,565,565,544,544,544,648,561,561,561,599&
        ,599,599,540,540,540,585,492,492,490,490,491,491,493,493,543&
        ,543,428,428,428,428,499,499,429,429,429,429,429,429,429,546&
        ,546,546,546,546,495,496,495,495,430,430,497,497,639,498,498&
        ,583,583,506,506,506,506,432,432,581,581,581,581,581,649,649&
        ,649,649,494,494,494,494,494,494,637,638,638,567,567,567,603&
        ,603,587,587,587,602,602,602,602,586,586,586,569,569,570,570&
        ,604,604,605,605,606,606,607,607,433,433,433,500,500,500,500&
        ,501,501,434,434,434,434,434,434,503,503,502,502,507,507,507&
        ,504,504,505,505,505,435,435,575,575,575,575,575,640,650,650&
        ,608,608,608,608,608,629,629,629,632,632,511,511,511,511,609&
        ,609,610,610,610,611,611,612,612,612,614,614,514,514,515,515&
        ,515,508,508,642,509,509,613,613,510,510,641,436,436,436,436&
        ,615,615,630,630,630,630,630,643,643,548,548,516,516,516,547&
        ,547,549,549,512,512,513,513,437,437,437,616,616,519,519,622&
        ,622,644,617,617,617,517,517,651,651,558,558,633,633,619,619&
        ,520,520,521,521,518,518,645,618,618,618,620,620,636,636,636&
        ,636,621,621,557,557,646,646,551,551,522,522,550,550,552,552&
        ,624,624,634,634,626,626,623,623,627,627,625,625,652,652,647&
        ,647,635,635,653,653,400,401,402,403,404,405,406,407,408,410&
        ,411,413,417,424,471,535,400,400,400,402,402,402,403,403,403&
        ,404,404,404,405,405,405,406,406,406,407,407,407,408,408,408&
        ,410,410,410,411,411,411,413,579,418,572,572,544,9,12,17,19,9&
        ,10,11,14,16,19,20,21,21,22,23,23,24,25,26,28,29,30,31,33,35&
        ,35,36,36,37,38,38,39,39,44,45,47,48,49,50,50,51,51,52,53,56&
        ,56,57,58,58,59,60,61,61,65,66,71,72,72,73,75,76,76,82,83,83&
        ,84,85,86,88,89,89,90,90,90,91,91,91,91,92,97,97,97,103,103&
        ,104,104,105,105,106,106,107,109,110,111,111,113,114,114,114&
        ,115,121,121,122,122,123,123,124,124,124,125,125,125,126,128&
        ,128,129,129,129,132,132,132,161,133,133,134,134,136,136,136&
        ,136,136,137,137,142,144,144,148,149,149,440,438,413,526,529&
        ,456,416,579,579,424,424,3,2,5,7,4,6,1,400,402,403,404,405&
        ,406,407,408,410,411,579,418,171,172,173,403,404,174,175,176&
        ,177,178,403,579,404,179,180,181,182,183,184,185,186,400,400&
        ,400,579,418,404,404,143,143,187,188,189,190,191,192,193,194&
        ,195,196,197,400,400,403,403,579,579,404,404,404,198,400,400&
        ,403,403,579,579,404,404,404,168,199,400,400,403,403,579,579&
        ,404,404,404,200,400,400,403,403,579,579,404,404,404,201,400&
        ,403,403,579,579,404,404,404,202,403,579,404,203,400,400,400&
        ,400,403,403,403,579,404,404,404,25,39,654,654,431,655,655&
        ,575,204,204,204,204,204,204,204,204,204,204,204,204,204,204&
        ,204,204,204,204,204,204,204,204,204,204,204,204,204,204,204&
        ,204,204,204,204,204,204,204,204,230,230,230,230,230,230,230&
        ,230,230,230,230,230,230,230,230,230,230,230,230,230,230,230&
        ,230,230,230,230,231,231,231,231,231,231,233,233,233,233,233&
        ,233,233,233,233,281,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,282,282,282,282,282,282,282,282,282,282,282,282&
        ,282,282,282,282,282,282,282,282,282,282,265,238,238,238,238&
        ,238,238,238,238,238,238,238,238,238,238,238,238,238,238,238&
        ,238,238,239,239,239,239,239,241,241,241,243,243,243,243,243&
        ,243,243,243,243,243,243,243,243,243,243,243,243,243,243,243&
        ,243,249,249,249,249,249,249,249,246,246,246,246,204,229,205&
        ,207,232,209,259,266,276,278,211,213,215,206,208,251,210,284&
        ,252,212,214,216,217,218,253,288,289,348,349,219,220,254,290&
        ,291,271,221,222,255,293,294,350,223,224,256,295,296,273,225&
        ,226,257,298,299,227,228,258,300,301,302,230,231,267,323,264&
        ,268,275,233,270,352,272,353,274,280,279,304,277,306,281,305&
        ,263,234,247,345,236,308,309,237,282,310,324,311,313,315,317&
        ,269,325,319,318,321,354,312,314,316,285,322,355,283,286,287&
        ,292,297,303,260,262,356,265,343,346,307,357,240,358,335,261&
        ,235,359,326,327,328,238,330,329,331,332,360,239,241,344,347&
        ,333,242,245,243,248,320,334,244,336,249,246,361,337,362,338&
        ,339,340,341,363,364,250,342,229,205,207,232,209,259,266,278&
        ,365,211,213,213,215,206,208,251,210,284,252,212,214,366,216&
        ,217,218,253,253,288,289,348,368,349,219,220,254,290,291,271&
        ,221,222,255,293,294,350,223,224,256,295,296,273,225,226,257&
        ,298,299,227,228,258,300,301,302,369,367,367,230,267,323,264&
        ,268,275,373,233,270,352,272,353,274,280,280,279,304,277,306&
        ,306,281,305,263,375,234,247,345,372,236,309,310,324,311,313&
        ,315,317,269,325,319,318,321,354,376,312,314,316,285,322,355&
        ,283,286,287,292,297,303,260,377,378,262,356,265,343,346,374&
        ,379,240,358,335,380,382,235,359,327,328,330,329,384,332,360&
        ,239,241,344,385,347,333,333,242,386,245,248,320,320,334,244&
        ,336,249,387,388,389,383,361,362,390,391,392,393,394,395,396&
        ,397,338,339,340,341,381,398,363,399,364,250,342,232,232,232&
        ,259,259,259,276,276,276,276,252,252,253,253,231,231,231,323&
        ,323,323,233,233,233,233,280,280,280,279,279,279,279,277,277&
        ,277,306,306,306,306,306,305,305,247,321,321,265,346,307,261&
        ,241,241,333,333,242,248,336,336,229,205,207,232,209,259,266&
        ,276,278,365,211,211,213,213,215,206,208,251,251,210,210,284&
        ,252,252,212,214,366,216,217,217,218,218,253,253,288,289,348&
        ,368,349,219,220,220,220,254,254,290,291,271,221,222,222,255&
        ,293,294,350,223,224,256,295,296,273,225,226,257,298,299,227&
        ,228,258,300,301,302,369,367,367,230,231,267,323,264,268,275&
        ,373,233,233,270,352,272,353,274,280,280,279,304,277,306,281&
        ,281,281,305,305,305,305,263,375,234,247,345,372,236,310,324&
        ,311,313,315,317,269,269,325,319,318,321,354,376,312,314,316&
        ,285,322,355,283,286,287,292,292,297,303,260,377,378,262,356&
        ,265,343,346,374,379,240,358,335,261,380,382,235,359,327,328&
        ,330,329,384,332,360,239,241,344,385,347,333,333,242,386,245&
        ,248,320,320,334,244,336,249,387,388,389,383,361,362,390,391&
        ,392,393,394,395,396,397,338,339,340,341,381,398,363,399,364&
        ,250,342,229,229,205,207,207,232,232,232,209,209,209,259,259&
        ,259,266,266,266,266,276,276,276,276,365,365,213,284,284,252&
        ,252,253,253,368,368,221,221,255,255,255,223,223,223,224,224&
        ,256,256,225,225,226,226,257,257,227,227,228,228,258,258,302&
        ,367,367,230,231,231,231,267,267,267,323,323,323,373,373,373&
        ,233,233,233,233,353,353,280,280,280,279,279,279,279,277,277&
        ,277,306,306,305,305,372,317,317,317,269,269,269,319,319,319&
        ,354,354,376,376,297,297,303,303,377,377,265,343,343,346,374&
        ,374,307,379,379,380,380,382,382,359,359,239,241,241,333,333&
        ,242,248,320,336,336,249,387,388,388,389,383,361,390,390,391&
        ,391,392,392,393,393,394,394,395,395,396,396,396,397,397,339&
        ,339,339,340,340,341,341,381,398,9,155,21,50,83,104,122,130&
        ,144,203,151,51,54,52,53,84,105,123,131,132,85,86,87,88,89&
        ,106,124,166,161,107,108,109,110,125,133,187,197,126,127,134&
        ,138,189,198,168,135,139,145,190,199,140,141,146,152,192,200&
        ,147,153,157,193,201,154,22,55,23,56,102,103,113,351,186,112&
        ,90,142,148,156,158,159,129,121,195,149,128,114,136,179,10,24&
        ,25,26,57,27,28,11,172,12,29,181,180,182,183,184,169,91,92,58&
        ,93,59,94,95,188,191,194,196,202,163,174,115,176,137,150,160&
        ,175,96,30,60,162,61,143,111,62,63,64,13,65,164,173,66,67,68&
        ,31,69,14,32,177,15,33,178,70,16,34,71,35,72,165,167,185,97&
        ,36,73,37,17,38,74,171,75,76,39,18,40,77,41,42,19,43,20,44,78&
        ,98,116,99,117,118,100,119,45,79,101,120,46,80,47,81,48,49&
        ,82/)
    arr_r2(1:5639) = (/659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,11,14,16,18&
        ,19,20,22,23,23,24,27,30,31,32,34,35,36,37,37,38,38,39,40,42&
        ,44,44,45,46,46,47,48,48,49,49,49,49,50,52,52,54,55,55,56,56&
        ,57,58,58,59,59,60,60,61,61,62,62,64,65,67,68,71,72,75,76,76&
        ,78,79,79,79,82,83,84,86,86,86,87,88,88,90,90,90,91,91,91,92&
        ,95,96,96,97,97,97,98,98,99,100,100,100,101,101,101,102,103&
        ,104,104,104,104,105,105,106,108,109,109,112,113,114,114,162&
        ,162,162,162,162,163,163,163,163,163,115,115,115,164,164,164&
        ,164,116,117,119,119,119,120,120,121,121,122,122,122,122,122&
        ,122,123,123,123,123,124,124,124,124,125,128,128,128,165,165&
        ,165,165,165,165,129,129,130,130,130,131,132,132,132,132,132&
        ,132,166,166,166,166,166,133,133,134,136,136,137,137,138,138&
        ,139,142,142,143,144,144,144,145,145,146,148,148,148,149,150&
        ,151,152,152,153,156,157,158,158,159,160,29,38,16,11,14,16,17&
        ,18,19,20,21,23,27,28,30,31,31,32,34,35,36,37,38,39,40,41,42&
        ,43,44,45,45,46,47,48,49,50,50,51,52,53,54,55,56,56,57,58,59&
        ,60,61,61,61,62,63,64,64,65,66,67,67,68,69,71,72,76,77,78,79&
        ,79,80,83,83,84,84,86,88,89,90,91,91,92,94,95,95,96,97,98,98&
        ,99,100,100,101,101,102,103,104,104,105,105,106,106,108,109&
        ,110,111,111,112,113,114,114,162,162,162,162,163,163,163,115&
        ,164,164,164,164,164,164,167,167,116,116,117,117,118,119,119&
        ,120,120,121,121,122,122,123,123,123,123,124,124,125,125,127&
        ,128,128,165,165,165,165,129,129,130,130,131,131,132,132,132&
        ,166,166,133,133,134,134,135,136,136,137,138,138,139,139,141&
        ,142,143,144,144,145,145,146,146,147,148,148,149,149,150,151&
        ,152,152,153,153,156,155,157,157,158,158,159,160,12,18,20,21&
        ,21,22,23,23,24,25,25,26,27,27,28,28,29,29,30,31,32,33,33,34&
        ,35,36,36,37,37,38,38,39,40,41,42,43,44,44,45,46,47,47,48,48&
        ,49,49,50,50,50,51,52,52,52,52,53,53,54,55,55,56,56,57,57,57&
        ,57,57,58,58,58,59,59,59,60,60,60,60,61,61,61,62,62,63,63,64&
        ,64,65,65,65,66,66,67,68,68,69,69,70,70,70,70,71,72,72,75,75&
        ,76,76,76,76,77,78,79,79,80,81,82,82,82,82,83,83,83,83,84,85&
        ,85,86,86,87,87,88,88,89,89,89,90,90,91,91,92,92,92,94,94,95&
        ,96,96,97,97,97,98,98,99,100,100,101,101,102,103,103,104,104&
        ,105,105,106,106,108,108,109,109,110,110,111,111,112,112,113&
        ,113,113,113,114,114,114,114,114,162,162,162,162,163,163,163&
        ,163,115,115,115,115,115,115,115,164,164,164,164,164,167,167&
        ,116,116,117,117,118,119,119,120,120,121,121,122,122,122,122&
        ,122,123,123,123,123,124,124,124,125,125,126,127,127,128,128&
        ,165,165,165,165,165,129,129,129,129,130,130,130,131,132,132&
        ,132,132,132,132,166,166,166,166,166,133,133,133,134,134,135&
        ,135,136,136,136,136,137,137,137,138,138,138,139,139,140,141&
        ,141,142,143,144,144,144,144,144,145,145,145,146,146,147,147&
        ,148,148,149,149,150,151,152,152,152,153,153,154,156,155,155&
        ,157,157,157,158,158,159,160,168,168,16,34,11,14,21,23,23,25&
        ,26,26,29,35,35,36,36,38,38,38,39,39,50,56,57,57,58,59,59,60&
        ,61,61,72,76,76,76,91,91,91,97,97,97,114,114,114,114,114,114&
        ,128,128,128,128,128,11,12,21,21,23,23,25,29,33,35,35,36,38&
        ,39,39,50,50,56,57,58,59,59,59,60,60,60,61,61,70,72,73,76,76&
        ,82,91,91,97,114,114,128,128,128,20,38,39,58,58,83,83,97,97&
        ,114,122,11,14,16,20,21,23,35,36,38,39,44,45,45,48,50,56,59&
        ,59,59,61,61,76,83,84,90,91,91,97,104,105,106,114,124,124,124&
        ,124,124,11,14,16,23,35,39,56,58,60,65,76,83,84,90,90,97,104&
        ,105,106,122,123,124,132,9,15,17,19,19,21,23,23,29,35,35,36&
        ,38,56,56,58,58,60,60,61,61,72,72,83,114,114,114,114,114,114&
        ,114,9,11,12,14,15,16,17,19,19,19,20,21,23,25,25,29,35,36,38&
        ,38,38,39,50,56,57,58,58,58,59,59,60,60,60,61,61,65,72,76,76&
        ,83,83,91,91,91,97,97,97,114,114,114,128,128,128,9,12,17,19&
        ,21,23,26,29,35,36,36,38,38,38,39,50,56,57,57,57,58,58,58,58&
        ,60,60,61,61,72,91,91,9,12,15,17,19,21,23,23,29,35,35,36,38&
        ,39,39,50,50,56,56,57,58,58,59,59,60,61,72,72,82,91,91,97,97&
        ,114,114,114,17,29,9,11,14,16,17,20,29,38,114,9,12,15,17,21&
        ,21,23,23,25,25,26,26,29,33,35,35,36,36,38,38,39,39,50,50,56&
        ,56,57,57,57,58,58,59,59,59,60,61,61,72,83,83,91,91,97,114&
        ,114,114,122,122,122,29,12,29,9,11,12,14,15,16,17,17,19,20,23&
        ,36,58,59,59,60,65,97,97,114,9,11,12,14,15,16,17,17,19,21,23&
        ,25,26,29,35,36,38,39,50,56,57,58,58,59,59,59,60,61,61,72,76&
        ,76,91,91,97,114,114,9,15,17,19,19,19,21,21,21,23,25,26,26,29&
        ,29,33,35,36,36,38,38,38,39,50,56,57,57,57,58,58,58,58,58,60&
        ,61,65,72,91,91,91,97,97,11,14,16,20,17,9,9,11,14,15,16,19,19&
        ,20,21,23,23,35,35,36,56,56,59,61,61,72,73,91,91,97,114,128&
        ,128,9,15,17,19,19,19,20,21,21,23,23,25,26,29,33,35,36,36,38&
        ,39,44,45,47,50,50,56,56,57,58,58,59,59,60,61,61,61,65,72,72&
        ,91,91,97,97,114,114,17,38,58,58,58,60,83,97,97,97,114,114&
        ,122,122,36,144,144,149,15,17,29,9,12,15,17,97,17,9,11,14,15&
        ,15,17,19,21,23,26,29,36,56,61,12,29,11,14,15,16,59,97,9,15&
        ,15,17,19,23,23,25,29,36,56,56,56,57,58,60,60,60,61,65,72,72&
        ,83,91,97,97,114,114,114,114,29,114,114,114,97,58,58,83,83,59&
        ,29,83,9,15,17,19,21,23,29,35,36,38,38,39,50,50,56,57,58,59&
        ,59,60,61,61,72,76,76,91,91,91,97,97,114,114,29,97,58,83,83&
        ,12,12,17,17,29,36,38,58,58,59,97,114,114,26,58,9,11,14,15,16&
        ,17,19,19,19,20,21,21,23,23,26,29,35,36,38,39,50,50,56,56,58&
        ,59,59,59,60,61,61,61,65,72,72,91,91,97,97,114,9,11,12,14,15&
        ,16,17,17,19,20,36,58,59,61,97,97,9,10,11,14,15,16,17,18,19&
        ,20,21,23,25,26,26,27,28,30,31,32,33,34,35,36,37,38,39,40,41&
        ,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61&
        ,62,63,64,65,66,67,68,69,71,72,73,76,77,78,79,80,81,82,83,84&
        ,85,86,87,88,89,90,91,92,94,95,96,97,98,99,100,101,102,103&
        ,104,105,106,108,109,110,111,111,112,113,114,162,163,115,164&
        ,167,116,117,118,119,120,121,122,122,123,124,125,126,127,128&
        ,128,128,165,129,130,131,132,132,132,166,133,134,135,136,137&
        ,138,139,140,141,142,143,144,144,144,145,146,147,148,149,150&
        ,151,152,153,154,156,155,157,158,159,160,168,9,12,17,19,19,21&
        ,23,26,29,35,36,38,39,50,56,57,58,58,60,61,61,65,72,91,97,97&
        ,114,114,9,11,14,16,18,19,20,21,23,27,28,31,32,34,35,37,39,39&
        ,40,41,42,43,44,45,47,48,49,50,52,53,54,55,56,58,59,60,61,62&
        ,64,65,66,67,68,69,71,72,76,77,78,79,80,81,83,84,85,86,87,88&
        ,89,91,92,94,95,96,97,98,99,100,103,104,105,106,108,109,110&
        ,111,112,113,162,163,115,164,167,116,117,118,119,120,121,122&
        ,123,124,125,126,127,128,165,129,130,131,132,166,133,134,135&
        ,136,137,138,139,140,141,142,143,144,144,145,146,147,148,149&
        ,150,151,152,153,154,155,156,157,158,159,160,168,9,17,29,17&
        ,17,97,9,19,19,21,23,29,35,36,38,39,50,56,58,60,61,61,72,91&
        ,97,9,17,19,21,23,26,35,36,39,50,56,57,58,60,61,65,72,91,97&
        ,114,26,29,58,26,29,33,58,9,19,21,23,26,35,39,50,56,57,58,60&
        ,61,65,72,83,91,97,114,162,163,115,164,165,168,58,15,17,19,19&
        ,19,21,23,23,25,25,29,35,36,38,38,50,56,56,58,58,58,59,59,59&
        ,59,59,60,61,65,72,91,91,97,97,12,29,9,15,17,19,21,23,25,26&
        ,29,33,35,36,39,50,56,57,58,60,61,65,72,91,97,97,83,122,122&
        ,29,59,29,9,15,17,19,38,17,26,29,38,9,9,9,11,14,15,15,15,16&
        ,17,17,17,18,20,20,20,23,23,25,35,36,39,38,50,56,56,59,59,60&
        ,61,61,65,65,72,72,83,83,84,84,91,97,97,104,104,104,104,105&
        ,105,106,106,114,114,122,122,122,122,123,123,123,123,124,124&
        ,124,125,125,132,132,132,133,133,133,134,134,138,138,138,139&
        ,139,145,145,145,20,29,36,36,38,38,38,57,58,58,58,59,59,59,76&
        ,76,76,83,97,97,97,114,114,122,122,122,124,124,29,29,9,9,9,15&
        ,17,17,19,9,9,11,14,15,15,16,17,17,17,18,19,20,20,20,21,23,25&
        ,30,31,35,36,38,39,45,49,50,50,50,50,56,59,61,61,72,76,83,84&
        ,84,91,97,97,104,104,104,104,105,106,114,162,162,163,163,115&
        ,115,164,164,122,122,122,122,123,124,124,124,125,128,165,133&
        ,134,138,139,145,146,152,16,16,20,23,56,56,72,72,91,97,97,11&
        ,14,16,19,19,20,21,23,23,35,36,38,50,56,56,58,60,61,61,65,72&
        ,91,97,97,114,114,20,9,16,18,20,23,27,28,41,43,44,45,47,52,53&
        ,54,55,56,59,60,62,64,65,67,69,72,78,79,80,84,85,86,87,88,89&
        ,91,94,95,96,97,98,103,104,105,106,108,109,112,162,163,115&
        ,164,119,121,123,124,128,165,129,132,166,143,144,149,151,168&
        ,12,60,65,91,97,9,17,26,58,83,97,114,121,58,97,11,14,16,17,17&
        ,20,21,23,29,35,36,39,56,58,59,61,61,72,91,97,114,83,83,9,83&
        ,9,15,15,17,17,38,17,9,9,9,9,9,12,15,17,17,19,20,20,50,50,50&
        ,58,59,60,65,83,84,84,84,97,104,104,104,105,105,105,106,106&
        ,106,114,115,122,123,123,123,124,124,124,125,125,125,132,132&
        ,133,133,133,134,134,134,138,138,138,139,139,139,145,145,145&
        ,168,9,9,12,12,15,15,17,17,18,18,19,19,20,20,20,20,50,50,83&
        ,83,84,104,104,104,104,105,105,106,106,106,106,114,114,114&
        ,122,122,122,122,123,123,124,124,124,124,125,125,125,125,132&
        ,132,132,132,132,132,132,132,132,132,133,133,133,133,134,134&
        ,134,134,138,138,138,138,29,29,9,11,14,16,17,29,50,50,83,84&
        ,84,104,105,106,106,114,122,122,123,124,125,132,133,29,58,58&
        ,83,114,29,12,17,26,29,38,57,58,59,59,76,76,83,83,83,83,91,91&
        ,97,97,97,114,122,122,128,128,16,23,65,72,97,9,9,9,9,9,58,9&
        ,26,29,58,26,9,9,9,9,12,15,15,17,17,19,20,20,50,50,50,83,83&
        ,83,84,84,97,97,104,104,104,105,105,106,106,115,122,122,123&
        ,123,123,123,124,124,125,125,132,133,133,134,134,138,138,139&
        ,139,145,145,9,9,9,9,17,17,19,19,20,20,20,20,50,50,50,50,83&
        ,83,84,84,104,104,105,105,106,106,106,106,122,122,123,123,123&
        ,123,124,124,125,125,125,125,132,132,133,133,133,133,134,134&
        ,134,134,138,138,9,9,15,17,17,17,18,19,20,20,50,83,84,104,104&
        ,105,106,114,114,122,122,123,123,124,124,125,132,133,9,15,17&
        ,29,29,29,9,9,9,12,14,17,17,19,20,20,21,23,26,35,39,50,56,58&
        ,60,61,65,72,83,91,97,115,168,29,9,9,9,9,17,58,9,9,9,12,17,17&
        ,58,59,60,65,83,83,83,91,97,122,9,9,12,12,15,15,17,83,132,9,9&
        ,15,17,19,50,84,104,104,105,106,114,123,123,124,124,125,132&
        ,133,133,9,9,15,17,50,83,83,84,104,104,105,105,106,114,114&
        ,122,122,123,123,124,124,132,29,9,17,29,29,91,128,29,12,9,15&
        ,15,17,168,83,83,124,9,9,15,17,9,9,17,50,84,104,104,105,105&
        ,114,114,122,123,123,132,9,15,17,29,29,29,29,9,15,15,9,9,17,9&
        ,9,15,17,50,83,83,104,104,114,122,29,29,29,29,29,15,9,9,15,17&
        ,9,9,17,114,9,15,17,29,29,9,15,122,17,38,15,9,9,17,9,15,17,29&
        ,15,15,17,9,15,29,12,29,12,12,36,38,57,58,60,25,29,60,114,25&
        ,60,12,17,29,53,89,110,127,135,141,147,12,29,58,29,29,12,17&
        ,29,60,83,114,29,29,29,29,12,58,114,29,12,26,29,115,137,150&
        ,12,12,26,29,29,83,26,29,58,60,97,115,128,129,137,150,83,83&
        ,122,29,29,26,12,12,12,114,83,124,124,12,83,115,124,137,124&
        ,29,26,58,12,83,124,124,124,83,12,29,9,12,15,17,23,29,35,38&
        ,39,56,58,9,12,15,17,21,23,25,26,35,39,50,56,61,72,90,9,12,15&
        ,17,23,26,29,56,9,12,15,17,26,38,12,90,9,12,23,90,23,23,31,35&
        ,36,36,37,38,39,40,43,45,49,49,56,61,61,51,50,54,72,72,75,79&
        ,82,90,83,83,169,101,122,162,164,102,104,84,105,85,86,123,132&
        ,130,106,124,166,125,126,133,134,138,139,140,145,146,152,153&
        ,154,148,158,89,110,127,127,135,141,141,141,155,56,72,25,33&
        ,26,31,61,61,104,169,162,164,23,56,90,114,39,97,58,60,36,36&
        ,91,66,66,38,74,74,74,93,93,59,57,70,73,76,9,23,56,90,17,39&
        ,15,35,72,74,21,23,25,31,35,36,37,39,38,40,42,44,44,45,49,56&
        ,56,61,63,73,73,73,74,79,50,51,54,53,84,85,90,90,169,101,104&
        ,104,105,123,89,110,106,107,125,126,130,130,133,127,134,135&
        ,139,145,140,141,146,147,153,154,21,22,22,23,24,25,27,28,31&
        ,35,35,37,37,39,40,43,44,44,45,46,46,50,51,54,55,56,56,61,61&
        ,60,62,63,63,64,59,66,66,68,72,72,72,73,74,75,75,77,77,53,78&
        ,79,79,80,104,130,130,84,85,86,87,89,90,169,96,99,101,110,106&
        ,107,108,112,118,125,126,127,134,135,139,140,141,146,147,153&
        ,154,155,21,23,35,31,38,39,56,56,90,38,36,39,56,90,83,36,38&
        ,66,114,83,132,124,138,122,53,110,135,147,21,89,127,141,83&
        ,122,124,138,36,38,61,39,39,66,83,97,97,122,124,138,152,39,66&
        ,31,35,36,72,47,49,39,56,59,61,66,72,72,74,91,91,93,104,130&
        ,19,28,38,60,65,83,105,124,133,138,145,53,110,135,21,89,127&
        ,141,115,137,150,66,61,66,90,90,91,9,12,15,17,29,17,17,49,26&
        ,29,50,90,21,53,39,39,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,3,2,7,3,2,7,3,2,7,3,2,7,3,2,7,3,2,7,3,2,7,3&
        ,2,7,3,2,7,3,2,7,2,2,2,2,2,2,1,1,1,1,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,170,8,8,8,8,8,8&
        ,8,8,8,8,8,8,659,659,659,173,173,659,659,659,659,659,178,178&
        ,178,659,659,659,659,659,659,659,659,186,186,186,186,186,186&
        ,186,659,659,659,659,659,659,659,659,659,659,659,659,659,197&
        ,197,197,197,197,197,197,197,197,659,198,198,198,198,198,198&
        ,198,198,198,659,659,199,199,199,199,199,199,199,199,199,659&
        ,200,200,200,200,200,200,200,200,200,659,201,201,201,201,201&
        ,201,201,201,659,202,202,202,659,203,203,203,203,203,203,203&
        ,203,203,203,203,203,130,1,1,91,1,1,91,204,205,207,209,211&
        ,213,215,206,208,217,218,219,220,221,222,223,224,225,226,227&
        ,230,231,233,234,235,238,239,241,242,242,245,243,248,244,249&
        ,246,250,205,207,209,206,208,217,218,219,220,221,222,223,224&
        ,225,226,230,231,233,234,261,239,239,239,241,242,248,231,233&
        ,234,261,241,248,212,271,273,275,275,233,234,265,261,207,204&
        ,205,207,232,209,259,266,278,211,213,206,208,251,210,284,212&
        ,214,217,218,253,288,219,220,254,290,271,221,222,255,293,223&
        ,224,256,295,273,225,226,257,298,227,228,258,300,302,230,231&
        ,267,264,264,268,275,233,304,281,263,234,247,236,308,237,310&
        ,311,313,315,317,269,269,318,318,321,285,283,286,287,292,297&
        ,303,260,265,307,261,235,326,327,238,329,331,239,241,242,243&
        ,248,320,334,244,336,249,246,337,338,339,340,342,204,205,207&
        ,206,208,217,218,219,220,221,222,223,224,225,226,227,228,231&
        ,233,234,241,249,275,205,206,208,219,220,223,224,227,228,230&
        ,231,275,233,235,238,239,241,245,243,320,246,231,233,239,239&
        ,242,233,265,242,205,206,230,231,233,234,247,236,265,265,261&
        ,235,239,241,245,243,248,320,249,246,250,231,233,247,269,265&
        ,241,249,230,233,247,239,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659/)
    arr_p1(1:5639) = (/400,401,402,403,404,405,406&
        ,407,408,409,410,411,9,9,9,10,9,9,412,9,9,12,403,403,413,12&
        ,12,14,15,16,15,15,414,15,17,415,17,18,18,18,19,20,20,20,20&
        ,20,19,21,9,28,21,21,26,21,27,416,26,39,29,417,25,26,418,27&
        ,419,23,25,420,46,42,31,36,16,35,421,36,17,38,25,26,422,40,44&
        ,45,20,47,49,50,423,53,21,21,55,21,53,56,424,26,29,39,31,47&
        ,55,35,72,425,78,78,44,79,56,56,426,83,84,89,53,87,53,89,61&
        ,427,62,60,56,50,78,99,78,100,79,90,428,83,429,105,50,106,430&
        ,110,89,110,90,91,431,432,90,114,433,122,104,123,434,125,127&
        ,127,60,435,106,134,135,127,135,90,91,436,139,141,141,90,91&
        ,437,134,103,146,147,141,147,90,153,90,90,146,132,9,402,405&
        ,407,409,410,411,439,440,438,441,442,439,443,405,407,444,414&
        ,443,445,408,412,412,446,447,411,448,448,448,449,411,448,450&
        ,410,412,443,451,452,453,452,454,442,455,456,416,412,418,457&
        ,417,419,458,459,438,418,455,460,461,458,462,463,418,464,412&
        ,443,422,461,448,465,466,451,467,468,452,469,468,468,470,468&
        ,456,423,424,416,418,471,416,472,442,455,464,473,425,474,475&
        ,475,461,476,474,465,476,477,478,426,467,479,480,481,468,482&
        ,483,483,484,483,485,478,423,479,486,456,464,487,458,467,456&
        ,459,487,488,452,467,487,452,489,486,487,490,491,474,492,493&
        ,476,493,479,494,467,479,480,481,429,495,482,495,496,430,467&
        ,483,430,497,498,424,431,432,499,431,471,464,478,428,499,433&
        ,495,496,500,501,423,479,495,496,430,434,502,497,503,480,481&
        ,498,504,505,506,435,497,507,505,508,509,479,503,510,431,511&
        ,436,509,512,513,514,515,516,437,508,517,513,518,519,516,519&
        ,520,521,522,518,523,415,407,402,405,407,408,409,410,411,440&
        ,438,442,443,523,410,524,405,407,525,414,445,415,526,446,527&
        ,447,528,448,411,529,449,530,450,451,440,456,458,453,452,454&
        ,455,438,416,418,531,417,464,412,413,418,460,443,448,465,403&
        ,414,449,532,463,533,407,421,524,534,461,529,466,462,456,423&
        ,452,467,469,470,468,424,418,471,535,536,537,472,485,425,461&
        ,474,475,465,476,466,477,478,426,423,479,467,480,468,482,538&
        ,484,483,539,427,540,473,424,541,464,456,486,542,478,423,486&
        ,488,403,488,467,486,489,456,489,421,474,492,475,490,491,476&
        ,493,477,543,478,428,479,429,480,481,495,496,482,430,483,497&
        ,498,431,432,544,418,545,421,499,433,429,500,546,501,495,496&
        ,434,503,430,497,502,498,504,505,506,435,507,504,515,505,508&
        ,509,424,510,511,436,508,547,509,512,513,514,548,511,437,549&
        ,517,512,521,513,518,424,519,518,550,520,551,424,552,403,409&
        ,411,400,440,400,400,438,401,400,406,400,400,409,400,410,403&
        ,413,401,410,405,406,553,407,406,406,408,406,410,408,415,408&
        ,409,409,409,410,400,411,411,411,408,411,410,411,408,410,400&
        ,440,438,400,400,410,440,443,400,440,400,400,409,400,438,400&
        ,408,412,415,554,403,526,531,410,524,417,400,406,438,444,438&
        ,412,555,400,409,403,443,411,448,400,444,525,403,414,449,446&
        ,447,410,528,406,408,553,414,407,406,525,408,444,408,410,412&
        ,443,409,411,411,529,444,411,410,415,451,556,440,438,456,423&
        ,452,440,486,440,452,409,452,440,443,400,440,452,438,416,412&
        ,418,410,443,416,524,533,537,438,442,525,421,425,411,461,461&
        ,529,465,529,466,416,412,416,456,423,452,467,456,468,440,455&
        ,443,452,452,468,554,539,446,416,525,416,464,473,403,438,416&
        ,424,541,440,458,486,464,444,456,486,459,440,444,456,458,452&
        ,467,486,440,452,486,525,542,72,421,529,474,411,475,475,529&
        ,476,466,477,444,424,456,416,423,479,429,452,467,480,481,456&
        ,468,482,467,483,468,468,483,526,424,418,471,525,425,412,418&
        ,424,426,499,479,429,500,479,452,467,480,481,495,496,430,495&
        ,496,480,481,467,483,497,482,498,483,498,421,424,473,506,106&
        ,456,482,482,498,504,497,505,498,498,505,424,539,426,499,433&
        ,500,511,497,505,508,504,509,505,509,502,514,424,431,504,499&
        ,504,509,512,508,513,509,424,509,513,508,513,518,547,520,424&
        ,512,557,558,407,407,402,405,440,438,444,444,412,414,525,553&
        ,525,553,414,408,414,415,414,526,456,416,412,554,531,524,417&
        ,464,525,418,421,410,443,422,414,418,471,559,421,425,464,424&
        ,424,473,473,541,414,424,471,431,432,402,403,440,412,438,412&
        ,414,526,414,525,414,414,415,415,526,412,456,416,415,531,58&
        ,524,417,412,414,418,526,418,414,421,560,410,422,415,418,471&
        ,425,424,541,471,431,432,18,447,447,447,463,485,561,425,562&
        ,563,561,402,405,407,411,443,443,445,414,451,451,448,529,450&
        ,450,453,419,528,533,417,524,418,528,564,470,535,417,418,425&
        ,564,565,484,566,480,481,482,430,567,402,405,407,448,449,530&
        ,465,537,462,462,450,474,475,465,476,568,492,490,491,569,570&
        ,482,570,400,400,412,410,443,452,438,452,456,456,458,414,412&
        ,416,467,456,571,467,486,456,418,421,542,482,456,423,467,480&
        ,481,495,496,440,402,400,405,444,407,412,410,443,524,411,452&
        ,440,458,459,416,444,414,61,412,418,412,452,456,418,418,471&
        ,572,419,573,458,542,473,416,418,473,464,419,574,480,481,418&
        ,424,431,545,425,544,423,479,429,424,431,575,400,403,408,410&
        ,440,438,412,464,525,414,576,414,415,576,526,456,416,454,554&
        ,576,464,418,545,577,464,578,464,418,421,418,471,400,403,414&
        ,408,410,440,438,418,418,525,418,414,415,526,418,456,418,416&
        ,418,554,531,418,417,418,464,418,418,421,451,418,471,418,425&
        ,418,541,499,409,460,400,402,405,407,412,411,419,422,419,438&
        ,403,525,526,440,456,438,416,444,464,412,418,579,559,525,421&
        ,414,420,415,580,526,531,456,423,416,424,412,554,539,531,572&
        ,410,524,417,464,579,418,421,423,479,418,471,425,424,541,581&
        ,423,479,429,582,413,579,443,402,410,405,445,407,410,451,410&
        ,411,416,414,572,533,573,473,473,425,544,566,400,402,403,405&
        ,406,407,408,414,410,440,438,444,412,559,525,414,415,526,456&
        ,416,554,531,559,410,524,417,464,418,559,421,410,422,418,471&
        ,425,416,424,438,553,526,410,524,445,456,458,464,416,464,418&
        ,576,579,421,559,421,414,559,414,415,580,531,423,424,414,420&
        ,539,531,420,421,572,425,473,471,473,425,418,471,431,425,544&
        ,402,405,407,411,414,400,412,402,405,414,407,410,451,411,412&
        ,438,418,420,560,414,416,471,417,418,580,421,560,418,471,425&
        ,583,431,432,438,414,415,410,524,451,529,440,456,438,416,464&
        ,418,531,559,421,414,420,415,531,465,466,537,456,423,416,424&
        ,539,531,572,417,573,473,531,418,471,473,421,425,471,431,425&
        ,544,572,581,447,447,463,584,572,473,561,562,544,585,540,586&
        ,563,587,414,511,536,511,411,530,465,448,411,449,530,544,530&
        ,411,402,405,411,414,411,411,448,418,411,537,414,411,418,411&
        ,588,402,405,445,407,528,425,452,438,458,418,410,416,467,486&
        ,423,414,424,480,481,571,426,423,473,488,423,473,425,478,430&
        ,431,478,544,423,495,496,434,473,479,546,473,473,418,473,467&
        ,473,419,467,497,456,464,418,419,467,423,424,473,414,418,539&
        ,471,480,481,479,471,431,419,566,478,424,426,473,419,535,418&
        ,426,499,544,506,429,500,476,473,418,467,473,403,418,408,415&
        ,539,414,415,531,539,417,425,539,541,418,572,438,402,405,420&
        ,407,415,410,524,589,411,440,456,438,416,418,572,572,414,415&
        ,572,456,423,416,424,572,417,572,573,473,418,471,572,473,421&
        ,425,471,431,425,544,572,419,402,524,405,445,407,524,451,410&
        ,411,414,572,573,418,425,544,438,523,402,405,421,407,526,446&
        ,524,529,456,416,464,418,457,460,419,582,417,405,559,407,421&
        ,420,590,580,531,534,591,463,533,465,466,532,537,588,589,423&
        ,542,564,467,571,485,424,539,572,573,473,471,563,535,476,473&
        ,592,568,584,536,593,425,414,574,594,474,477,595,596,597,479&
        ,480,488,598,599,565,482,541,431,566,600,601,561,544,492,490&
        ,493,543,428,499,429,495,430,602,567,497,418,572,586,506,581&
        ,494,494,546,546,603,569,570,604,605,606,607,479,500,434,503&
        ,502,507,504,424,431,575,608,511,609,610,495,496,611,612,514&
        ,515,508,613,614,516,547,549,512,615,616,572,500,617,520,521&
        ,518,558,618,619,620,522,550,552,621,622,623,624,625,626,627&
        ,438,403,408,410,524,456,416,418,473,421,414,415,531,423,424&
        ,539,531,572,473,473,471,473,425,431,473,425,473,479,438,402&
        ,405,407,446,524,529,456,416,460,419,417,405,407,421,590,531&
        ,539,534,591,463,533,465,466,537,588,589,423,564,467,571,485&
        ,424,572,573,473,471,563,476,473,592,568,584,536,593,425,574&
        ,594,474,477,595,596,479,480,488,598,599,565,482,431,566,600&
        ,601,561,544,492,490,493,499,429,495,430,602,567,497,583,586&
        ,506,494,494,546,546,603,569,570,604,605,606,607,500,434,503&
        ,502,507,504,575,608,511,609,610,611,612,514,515,508,613,614&
        ,516,547,549,512,615,616,572,617,520,521,518,558,618,619,620&
        ,522,550,552,622,621,623,624,625,626,627,455,446,563,418,422&
        ,544,438,410,524,456,416,473,421,414,414,531,423,424,572,473&
        ,473,471,425,431,425,438,560,524,456,416,418,421,414,531,423&
        ,424,539,572,473,471,473,425,431,544,581,418,568,572,418,418&
        ,559,572,438,524,456,416,418,421,531,423,424,539,572,473,471&
        ,473,425,479,431,544,581,494,494,546,546,608,627,593,559,420&
        ,410,524,590,456,438,416,473,545,425,425,414,420,592,423,416&
        ,424,572,425,544,524,417,573,425,544,473,418,473,425,425,431&
        ,425,544,414,414,438,560,526,524,456,416,464,418,579,559,421&
        ,420,531,423,424,539,572,473,471,473,425,431,544,425,561,563&
        ,629,600,573,474,465,532,537,588,537,449,451,597,415,83,452&
        ,467,402,405,438,458,542,407,418,457,571,485,411,461,474,480&
        ,481,488,478,414,426,418,430,495,496,417,479,546,418,479,473&
        ,546,425,428,430,503,497,502,471,425,544,479,495,496,503,502&
        ,514,504,515,434,611,429,495,496,630,495,496,514,548,430,515&
        ,516,508,547,434,548,558,502,547,520,512,521,515,521,522,518&
        ,550,547,550,623,475,480,414,542,418,571,598,598,418,479,598&
        ,419,479,565,443,598,565,502,545,425,544,479,503,495,496,514&
        ,497,502,488,482,411,461,474,411,411,418,411,456,423,402,405&
        ,464,545,407,457,418,471,563,419,411,465,476,467,423,478,631&
        ,535,473,414,431,471,476,574,480,481,495,496,479,566,418,541&
        ,506,566,495,467,430,418,544,506,479,495,496,434,503,502,500&
        ,495,496,495,496,495,496,495,496,479,495,496,611,630,495,496&
        ,514,515,431,435,516,547,520,521,522,550,623,407,407,462,416&
        ,424,424,425,425,431,544,544,402,405,407,410,524,411,456,438&
        ,416,431,414,418,423,416,424,572,473,418,431,473,425,431,425&
        ,544,431,511,462,418,407,463,529,416,460,419,591,533,465,466&
        ,537,564,467,571,485,424,573,473,563,476,473,568,536,425,474&
        ,477,595,480,488,598,599,565,482,431,600,601,561,544,492,499&
        ,429,495,430,602,567,586,494,494,546,546,605,607,434,503,575&
        ,608,511,611,612,616,617,618,620,627,417,473,473,431,544,438&
        ,418,418,572,479,544,581,607,572,544,402,405,407,420,592,411&
        ,423,544,544,544,414,544,424,544,544,418,544,544,544,544,544&
        ,632,587,475,604,476,532,568,537,472,601,418,462,595,467,480&
        ,481,423,542,426,424,564,474,492,423,430,503,572,573,473,473&
        ,503,480,502,514,544,495,496,630,495,514,548,430,515,516,611&
        ,546,500,434,548,558,503,516,633,502,547,520,611,558,514,520&
        ,551,515,521,522,516,522,634,547,550,623,520,623,635,627,482&
        ,482,467,467,488,488,418,418,599,599,565,565,475,475,490,490&
        ,502,502,514,514,515,514,514,548,548,515,516,508,508,547,547&
        ,495,630,495,503,503,558,558,633,633,547,547,520,520,512,512&
        ,521,521,430,430,503,503,612,612,514,514,557,557,521,521,522&
        ,522,518,518,550,550,550,550,623,623,423,546,483,402,405,407&
        ,418,430,498,504,515,505,508,516,547,430,512,514,503,633,520&
        ,521,518,551,550,467,418,546,497,614,497,424,424,418,581,415&
        ,539,572,417,573,422,574,423,479,495,496,471,431,425,544,581&
        ,581,429,500,432,575,407,416,473,425,544,473,545,490,491,492&
        ,572,476,418,606,572,418,480,481,495,496,479,478,428,418,424&
        ,564,492,569,495,496,503,495,496,630,430,548,425,544,479,500&
        ,630,503,558,502,633,546,611,636,495,496,503,558,514,633,515&
        ,551,630,516,624,547,634,520,634,521,635,522,635,482,482,430&
        ,430,479,479,565,565,490,490,570,570,502,502,514,514,514,514&
        ,515,516,548,548,516,633,547,547,520,520,558,558,633,633,557&
        ,557,520,520,521,521,522,522,557,557,522,522,634,634,550,550&
        ,623,623,623,623,483,497,637,638,480,481,602,567,491,604,515&
        ,516,547,516,633,520,521,548,558,633,557,520,551,515,521,550&
        ,624,623,498,639,418,502,507,504,438,479,429,541,405,572,431&
        ,524,493,605,456,416,418,421,531,423,424,572,473,471,473,425&
        ,479,431,544,546,627,640,570,604,569,605,601,572,495,496,434&
        ,429,418,433,572,573,473,473,495,496,630,431,544,611,430,503&
        ,495,496,546,501,418,558,627,497,502,640,418,567,516,520,633&
        ,557,551,522,558,551,624,516,634,623,558,623,635,498,504,507&
        ,418,547,547,520,521,520,551,521,522,550,633,557,551,624,522&
        ,634,520,623,634,614,505,418,515,508,616,618,641,500,503,429&
        ,501,418,627,633,557,633,504,515,614,418,505,508,418,521,550&
        ,522,634,550,623,551,624,634,623,635,635,509,642,418,547,549&
        ,512,615,643,615,643,508,547,418,509,512,549,418,550,550,623&
        ,623,635,634,635,619,521,518,622,644,643,512,521,619,418,513&
        ,518,418,635,519,645,418,550,552,646,621,627,558,630,646,518&
        ,550,418,519,552,418,626,646,626,418,647,647,457,451,489,489&
        ,456,4,5,26,6,4,4,6,4,6,4,4,438,412,416,468,483,498,505,509&
        ,513,519,413,628,593,534,417,529,530,466,595,492,605,573,592&
        ,594,477,467,583,511,648,479,649,429,643,646,647,480,481,638&
        ,496,495,514,499,581,575,607,613,615,618,620,621,625,610,511&
        ,617,569,606,650,495,496,430,618,651,624,624,503,633,646,634&
        ,647,623,515,511,617,612,557,652,635,653,634,557,627,21,23,25&
        ,26,50,56,60,57,61,83,91,23,29,35,39,50,56,60,61,72,58,83,90&
        ,91,97,114,26,39,36,38,61,57,58,91,28,31,37,49,76,82,60,121&
        ,61,58,91,128,418,21,28,25,25,26,25,26,26,27,19,44,26,28,50&
        ,23,54,21,53,21,65,60,26,64,26,83,84,53,51,100,123,53,53,115&
        ,105,89,106,53,53,124,166,132,110,125,133,127,110,134,135,139&
        ,141,135,146,147,153,155,147,138,152,21,53,21,53,53,89,53,21&
        ,141,23,23,21,25,21,19,29,17,83,29,163,115,9,23,56,90,17,72&
        ,39,25,17,15,61,72,39,39,38,39,58,58,74,31,26,39,36,26,23,56&
        ,90,114,39,58,35,72,97,93,25,25,9,37,33,33,33,36,36,41,41,25&
        ,46,46,36,60,65,75,60,33,36,70,35,67,51,25,25,25,85,25,60,169&
        ,35,67,102,83,115,115,53,25,107,25,126,25,169,122,137,25,25&
        ,25,140,150,51,25,25,25,154,51,26,24,10,26,10,26,18,26,49,36&
        ,39,36,49,38,42,49,26,47,47,36,47,26,26,26,27,26,26,12,39,75&
        ,26,76,39,47,31,39,73,42,35,66,36,36,39,36,25,68,40,26,44,47&
        ,47,46,103,129,91,50,26,54,55,26,91,75,62,78,95,26,84,85,87&
        ,77,99,26,107,26,26,26,26,75,26,26,26,26,75,26,28,28,37,43,49&
        ,49,63,28,92,47,47,47,64,100,98,60,26,36,122,105,161,133,145&
        ,132,89,127,141,155,53,110,135,147,106,166,134,146,33,17,60&
        ,75,60,36,115,72,167,131,137,150,160,57,35,59,33,70,33,81,82&
        ,58,91,58,58,58,35,97,58,61,111,58,83,122,52,88,61,115,115&
        ,124,133,138,145,152,157,110,135,147,89,127,141,155,137,150&
        ,160,36,91,91,114,114,165,21,23,25,26,56,39,38,82,76,90,115&
        ,121,53,89,58,93,9,9,9,10,9,17,18,9,12,12,12,19,15,15,15,15&
        ,17,17,18,18,18,19,20,20,20,20,20,17,21,23,9,9,21,26,21,28,21&
        ,18,27,9,23,9,20,44,25,17,10,30,17,39,17,19,31,12,29,25,26,18&
        ,27,28,23,25,36,37,35,46,26,40,41,40,42,20,47,20,48,49,33,35&
        ,16,34,26,15,35,17,38,26,28,9,18,40,31,43,20,44,20,20,45,20&
        ,49,50,21,23,56,21,26,26,50,54,25,51,23,51,50,53,21,21,21,52&
        ,53,21,53,20,44,64,29,23,23,56,22,25,60,65,26,61,28,63,25,65&
        ,36,66,40,68,31,69,45,47,39,58,39,29,31,59,28,52,26,39,57,26&
        ,39,76,47,81,49,49,82,16,71,35,72,27,55,27,62,40,77,72,50,78&
        ,44,78,45,79,46,80,46,67,23,25,51,21,56,83,50,50,83,21,90,56&
        ,21,21,53,53,53,53,83,83,84,21,50,85,50,84,53,89,51,85,55,87&
        ,52,53,88,53,89,57,56,90,26,61,91,28,92,69,94,47,95,84,86,52&
        ,88,72,72,97,76,18,55,96,55,87,96,27,62,90,97,78,98,20,99,78&
        ,99,64,100,79,101,51,56,90,102,90,103,83,83,104,50,114,90,56&
        ,50,162,163,115,164,83,83,84,105,89,106,106,110,21,89,110,57&
        ,111,25,56,60,113,90,91,90,114,90,56,23,83,84,86,86,50,83,162&
        ,163,115,164,85,84,86,52,88,109,72,167,40,55,96,27,55,87,108&
        ,62,112,114,98,116,99,117,99,118,100,119,101,120,102,121,90&
        ,103,103,50,83,104,122,85,115,105,123,104,83,50,105,106,124&
        ,110,125,25,85,126,125,127,89,110,127,90,113,91,90,90,56,128&
        ,115,84,86,165,97,169,75,72,18,55,96,55,108,91,103,114,129&
        ,122,130,85,115,131,123,132,106,124,166,126,137,125,133,127&
        ,127,134,134,135,21,127,135,113,136,90,128,115,103,114,129&
        ,130,90,142,106,124,132,83,50,126,137,125,133,134,134,138,135&
        ,139,25,140,139,141,135,141,90,91,128,128,143,141,147,155,153&
        ,155,122,129,144,90,129,126,137,133,148,134,138,140,150,139&
        ,145,141,146,146,147,21,90,128,149,129,151,106,123,104,83,90&
        ,156,134,138,140,150,139,145,146,152,147,153,25,154,145,158&
        ,146,152,154,160,153,157,168,138,90,159,146,152,154,160,153&
        ,157,153,157,9,10,11,12,13,14,15,16,17,19,20,29,59,90,91,92,9&
        ,12,9,11,11,11,12,12,12,13,12,13,14,14,14,15,12,15,16,16,16&
        ,17,12,17,19,19,19,20,20,20,12,29,26,39,58,97,3,2,5,7,400,401&
        ,402,405,407,410,411,9,440,10,9,438,10,9,9,9,12,10,12,15,12&
        ,525,15,414,15,17,415,17,526,20,20,20,20,17,21,456,21,25,21&
        ,21,23,416,26,39,531,31,25,12,418,25,36,16,35,421,36,17,26&
        ,422,49,50,423,53,21,21,21,21,53,23,56,424,26,26,418,471,28&
        ,35,72,425,56,426,83,479,53,84,50,89,53,53,53,61,427,60,23,56&
        ,90,50,90,428,83,429,84,105,50,106,430,50,84,110,89,91,432,90&
        ,114,433,105,123,434,132,84,125,50,84,25,60,90,113,435,12,106&
        ,85,122,436,106,91,437,400,400,403,403,411,440,438,403,413&
        ,438,416,9,12,17,19,25,39,8,9,11,12,13,14,15,16,17,19,20,29&
        ,12,38,11,35,421,403,26,51,26,32,72,559,559,553,23,83,83,124&
        ,138,152,72,56,416,91,471,432,432,416,56,61,61,133,104,138&
        ,145,104,152,157,104,113,122,133,548,516,514,548,514,558,502&
        ,514,480,138,633,520,633,516,516,557,515,516,430,198,145,551&
        ,522,551,520,624,520,520,502,547,152,634,623,634,522,652,522&
        ,522,515,521,157,635,635,623,653,623,623,547,550,130,610,610&
        ,424,122,423,479,500,495,479,429,500,500,423,429,479,130,203&
        ,128,143,654,128,143,655,205,206,208,210,212,214,216,217,218&
        ,219,220,221,222,223,224,225,226,227,228,229,207,232,209,211&
        ,236,234,240,240,234,244,234,247,247,247,247,236,247,208,251&
        ,252,218,253,220,254,222,255,224,256,226,257,228,258,232,209&
        ,259,260,242,262,240,263,264,262,265,259,266,267,233,268,269&
        ,270,272,274,276,277,278,279,280,281,232,230,207,232,209,259&
        ,266,278,266,260,283,208,251,210,252,285,286,287,218,253,288&
        ,289,220,254,290,291,292,222,255,293,294,224,256,295,296,297&
        ,226,257,298,299,228,258,300,301,303,231,233,279,268,304,305&
        ,306,281,305,233,264,262,265,307,309,282,284,312,314,316,264&
        ,265,275,319,320,282,322,323,310,324,311,313,315,267,269,325&
        ,242,321,327,328,239,282,332,241,333,261,249,320,318,248,335&
        ,247,319,235,338,339,340,341,248,231,207,232,208,251,218,253&
        ,220,254,222,255,224,256,226,257,228,258,233,281,262,333,319&
        ,343,211,212,286,271,292,273,297,302,303,262,317,344,264,245&
        ,330,330,329,330,242,248,245,264,304,329,330,330,305,344,319&
        ,213,214,265,269,275,244,345,336,345,247,242,250,261,261,242&
        ,248,334,248,320,250,342,275,306,345,265,346,347,318,307,325&
        ,336,245,9,155,21,50,83,104,122,130,144,203,51,54,52,53,84&
        ,105,123,131,132,85,86,88,89,106,124,166,161,107,109,110,125&
        ,133,187,197,126,127,134,138,189,198,168,135,139,145,190,199&
        ,140,141,146,152,192,200,147,153,157,193,201,154,23,56,102&
        ,103,113,351,186,90,142,148,156,158,159,129,121,195,149,128&
        ,114,136,179,25,26,57,28,11,172,12,29,181,180,182,183,184,169&
        ,91,92,58,93,59,94,188,191,194,196,202,115,174,115,176,137&
        ,150,160,175,60,115,61,143,111,63,13,65,115,173,66,31,69,14&
        ,32,177,15,33,178,16,34,71,35,72,165,185,97,36,37,17,38,74&
        ,171,75,76,39,19,43,20,44,45,79,101,120,47,48,49,82,227,204&
        ,205,207,232,232,259,259,323,204,247,205,236,205,206,208,251&
        ,209,210,205,205,367,205,206,217,207,218,253,253,206,366,206&
        ,217,219,220,254,254,217,219,221,222,255,255,294,221,223,224&
        ,256,256,221,223,225,226,257,257,225,227,228,258,258,225,204&
        ,205,372,204,231,231,262,231,231,374,231,233,233,233,233,233&
        ,233,281,233,264,269,233,269,231,262,230,370,204,204,247,204&
        ,204,308,232,232,253,255,257,230,247,282,249,249,282,235,363&
        ,209,209,209,259,266,207,247,207,247,218,222,226,211,367,237&
        ,234,207,247,265,265,372,230,234,207,239,381,383,237,235,326&
        ,327,238,241,242,331,331,238,239,241,241,241,241,239,238,242&
        ,238,243,248,243,248,234,247,243,371,387,371,371,246,337,362&
        ,390,390,390,393,390,362,396,337,338,339,339,337,337,337,363&
        ,337,246,250,207,205,230,232,232,209,266,323,281,280,251,210&
        ,217,218,204,230,204,205,231,232,204,230,230,231,233,323,323&
        ,233,267,211,231,306,233,269,269,265,247,233,269,233,264,243&
        ,246,235,247,345,236,242,239,238,241,239,238,243,236,247,227&
        ,204,205,207,232,232,259,259,259,323,205,234,247,205,205,205&
        ,206,206,208,251,208,209,251,210,205,205,367,205,205,206,207&
        ,217,207,218,253,252,206,366,206,206,207,208,219,208,220,254&
        ,254,217,219,208,207,222,255,255,294,221,223,224,256,256,221&
        ,223,225,226,257,257,225,227,228,258,258,225,370,372,205,204&
        ,230,231,231,262,231,231,374,230,231,212,218,233,233,233,281&
        ,233,233,264,269,269,231,230,233,264,234,262,233,230,370,204&
        ,204,247,204,204,232,232,253,255,257,262,247,247,236,249,249&
        ,235,235,363,209,209,209,259,266,207,247,207,247,218,237,222&
        ,226,211,367,370,234,207,237,265,265,372,230,234,207,239,242&
        ,381,383,237,235,326,327,238,241,330,331,331,237,239,241,241&
        ,241,239,241,238,242,238,243,249,248,248,243,247,243,371,387&
        ,238,243,246,337,362,390,390,390,393,390,362,396,337,338,339&
        ,339,337,234,337,363,337,243,250,225,227,204,205,230,207,205&
        ,230,207,232,207,232,232,209,259,207,232,209,266,323,281,280&
        ,280,233,247,212,286,251,210,217,218,366,367,219,217,221,222&
        ,221,221,219,217,223,222,224,223,223,221,225,224,225,226,225&
        ,223,226,227,228,227,205,372,205,204,204,230,204,230,234,211&
        ,205,231,232,372,374,233,282,230,230,231,256,224,233,323,323&
        ,233,267,211,231,306,233,269,233,269,233,264,204,234,262,240&
        ,265,247,247,282,249,243,235,359,338,363,273,234,302,234,367&
        ,372,247,306,233,345,230,372,236,337,362,381,239,383,387,235&
        ,361,238,239,238,241,239,238,243,248,236,247,243,371,371,387&
        ,371,371,246,337,362,207,390,390,391,362,390,337,393,390,393&
        ,337,362,379,379,396,337,338,337,338,339,339,340,337,337,204&
        ,229,205,207,232,209,259,266,276,278,365,211,213,215,206,208&
        ,251,210,284,252,212,214,366,216,217,218,253,288,289,348,368&
        ,349,219,220,254,290,291,271,221,222,255,293,294,350,223,224&
        ,256,295,296,273,225,226,257,298,299,227,228,258,300,301,302&
        ,369,367,230,231,267,323,264,268,275,373,233,270,352,272,353&
        ,274,280,279,304,277,306,281,305,263,370,375,234,247,345,372&
        ,236,308,309,237,282,310,324,311,313,315,317,269,325,319,318&
        ,321,354,376,312,314,316,285,322,355,283,286,287,292,297,303&
        ,260,377,378,262,356,265,343,346,374,307,379,357,240,358,335&
        ,261,380,382,235,359,326,327,328,238,330,329,384,331,332,360&
        ,239,241,344,385,347,333,242,386,245,243,248,320,334,244,336&
        ,249,371,387,388,389,383,246,361,337,362,390,391,392,393,394&
        ,395,396,397,338,339,340,341,381,398,363,399,364,250&
        ,342/)
    arr_p2(1:5639) = (/1,1,1,1,1,1,1,1,1,1,1,1,9,10,12&
        ,17,15,17,1,18,19,12,12,2,1,10,19,12,15,12,12,17,1,19,17,1,12&
        ,12,15,17,19,9,12,15,17,19,17,12,25,9,9,17,9,18,9,1,17,12,19&
        ,1,12,12,1,12,1,20,12,1,12,12,19,15,39,12,1,17,39,12,17,19,1&
        ,12,9,12,25,17,17,12,1,12,25,26,9,28,9,12,1,29,28,39,31,29,12&
        ,29,12,1,12,9,29,12,25,26,1,12,12,12,25,9,28,9,39,1,29,29,29&
        ,25,29,12,21,12,29,25,1,29,1,12,50,12,1,12,25,9,39,29,12,1,61&
        ,26,1,12,25,12,1,12,12,9,29,1,25,12,12,25,9,85,91,1,12,12,9&
        ,106,114,1,25,114,12,12,25,9,126,12,134,140,25,9,403,9,9,9,9&
        ,9,9,9,12,9,9,9,12,12,23,23,12,9,15,9,26,17,12,9,9,21,9,12,15&
        ,9,26,19,9,26,19,17,9,12,9,19,9,21,9,12,9,26,12,12,9,12,12,12&
        ,26,9,12,9,12,12,12,9,16,12,25,26,9,9,29,12,9,26,12,12,26,9&
        ,17,18,9,19,29,12,9,26,23,9,28,9,50,23,29,12,9,9,12,9,29,9,12&
        ,29,12,9,9,9,29,9,12,12,29,12,12,18,9,19,29,12,29,12,23,51,53&
        ,12,50,25,51,50,12,9,60,25,12,65,53,23,12,12,12,29,12,9,29,12&
        ,25,12,29,23,29,29,9,12,29,9,9,12,50,29,9,12,12,61,23,9,15,25&
        ,60,91,39,17,23,9,29,29,9,9,83,50,23,23,29,9,12,29,9,50,50,29&
        ,12,12,23,9,25,9,29,12,12,85,25,9,104,23,9,29,12,12,23,29,12&
        ,9,25,9,29,12,12,25,29,23,29,25,25,12,10,11,12,12,12,12,12,12&
        ,12,12,12,12,12,12,29,12,29,29,12,12,12,12,12,12,12,12,12,12&
        ,29,12,12,12,12,12,29,12,12,12,12,12,12,29,12,17,12,12,12,29&
        ,26,12,12,29,29,12,60,29,29,12,12,12,58,12,26,12,12,29,12,12&
        ,29,12,29,12,12,12,12,12,29,12,12,12,29,12,12,12,29,12,12,29&
        ,12,29,12,12,12,29,12,29,12,29,12,12,12,12,29,12,12,29,29,12&
        ,50,60,29,23,9,25,29,12,115,12,35,29,50,65,72,65,29,12,29,12&
        ,12,29,12,29,12,29,12,29,12,29,29,12,12,29,12,29,12,12,29,12&
        ,26,97,58,91,29,12,29,12,29,12,29,29,12,12,29,29,12,29,12,12&
        ,29,12,12,29,12,29,12,12,115,12,29,12,29,12,29,12,12,29,12,29&
        ,12,12,12,29,12,29,12,137,12,29,12,29,12,150,12,13,13,13,9,13&
        ,10,12,13,17,15,9,17,18,9,19,9,12,13,12,12,12,15,13,12,12,17&
        ,15,19,15,17,13,12,12,15,17,19,20,9,12,15,20,17,20,19,19,17&
        ,23,12,9,25,28,21,19,9,21,9,26,27,21,29,12,38,26,17,9,13,39&
        ,12,13,29,12,13,15,23,15,12,17,12,26,40,23,28,12,23,12,15,12&
        ,9,36,12,12,17,12,31,12,36,33,17,15,39,29,12,25,17,28,26,19&
        ,17,29,21,29,12,20,38,38,19,17,13,29,23,12,13,12,25,13,26,17&
        ,53,18,28,21,53,21,9,29,12,29,12,56,29,19,31,12,12,27,23,29&
        ,12,13,50,12,9,23,12,29,12,25,56,26,29,12,29,12,21,12,55,21&
        ,53,28,21,9,12,12,56,40,56,35,29,12,90,29,29,12,13,60,23,12&
        ,21,50,25,12,23,60,50,25,23,35,15,12,65,35,12,53,9,444,25,50&
        ,12,84,12,9,56,12,29,12,90,25,12,56,29,12,13,12,29,12,12,50&
        ,29,12,21,12,25,21,9,90,39,72,35,91,26,97,90,61,29,12,29,12&
        ,13,25,29,13,13,13,13,13,13,13,13,23,23,50,29,12,21,12,21,9&
        ,90,72,12,12,444,85,25,50,29,12,21,12,25,21,9,85,90,29,12,29&
        ,39,12,50,29,12,21,12,21,9,29,12,91,90,25,90,50,29,12,21,12&
        ,25,126,21,9,50,29,12,29,12,140,25,12,23,14,32,15,15,15,15,12&
        ,15,15,9,12,12,15,17,15,36,17,15,12,15,15,15,36,15,15,35,15&
        ,15,26,15,15,26,36,15,56,35,15,29,35,15,29,15,35,12,29,15,90&
        ,36,35,35,15,17,17,17,9,17,12,9,12,15,17,12,17,17,12,17,23,17&
        ,17,26,17,410,39,17,35,23,15,26,17,36,17,17,57,17,49,39,17,17&
        ,39,17,58,39,17,411,17,12,29,12,12,659,18,12,29,29,19,19,19&
        ,19,9,12,12,19,17,12,19,19,12,19,12,12,29,12,19,26,19,26,12&
        ,12,12,26,31,19,29,12,12,12,28,28,31,19,12,20,20,20,12,12,12&
        ,12,12,12,12,26,12,12,29,12,12,12,12,12,12,12,45,29,21,25,9&
        ,21,9,9,21,12,12,15,12,21,26,21,12,39,12,15,12,26,21,21,12,12&
        ,90,56,29,29,29,12,12,12,23,29,23,12,23,12,23,12,9,23,12,29&
        ,12,12,12,29,23,408,39,17,29,29,29,26,29,12,9,29,9,29,12,9,26&
        ,23,9,29,26,9,12,12,56,26,9,29,23,9,29,29,12,91,56,9,25,25,25&
        ,25,25,25,25,12,25,25,15,26,25,17,25,25,25,36,25,26,39,35,17&
        ,12,25,12,26,25,25,60,25,26,26,9,26,26,26,26,9,12,26,15,26,26&
        ,26,17,26,21,26,23,26,26,39,26,31,26,26,35,26,57,61,26,72,26&
        ,90,26,12,26,12,28,28,28,28,19,28,12,17,90,12,29,12,12,29,12&
        ,29,12,29,12,29,12,12,12,29,12,29,12,29,12,29,12,29,12,29,12&
        ,58,29,12,29,12,29,12,29,29,26,29,29,29,12,12,29,29,12,29,12&
        ,29,29,29,12,13,13,12,31,29,31,12,31,39,12,31,31,19,31,19,29&
        ,19,19,19,31,19,29,33,33,33,33,33,33,33,15,33,33,33,33,33,12&
        ,33,33,33,33,33,33,33,33,39,33,33,33,33,33,26,33,33,33,33,33&
        ,33,29,33,15,12,15,35,15,12,15,12,9,15,15,15,12,15,12,15,15&
        ,35,17,39,35,15,15,15,15,61,26,15,35,29,39,15,17,15,15,15,15&
        ,72,35,15,35,15,36,36,36,36,19,38,17,38,38,17,38,38,17,38,26&
        ,38,17,17,12,38,38,17,38,38,26,38,38,38,38,38,12,38,38,17,12&
        ,12,39,17,12,17,39,17,39,17,17,17,12,17,17,39,17,39,17,17,17&
        ,17,39,17,39,17,17,39,17,39,17,17,26,39,17,17,39,17,39,17,39&
        ,17,56,17,12,39,29,12,18,18,12,29,18,12,29,12,90,29,43,69,129&
        ,69,25,9,12,12,29,12,12,20,15,26,47,47,36,20,38,49,26,20,57&
        ,12,47,91,47,31,12,49,49,17,49,58,49,12,25,12,9,50,21,12,12&
        ,12,50,21,12,12,26,12,25,21,12,26,21,21,12,12,21,29,21,90,29&
        ,29,12,9,60,29,83,60,60,26,60,53,60,12,12,12,12,12,12,12,12&
        ,12,12,56,39,12,12,12,12,12,26,12,29,12,12,26,12,29,61,26,90&
        ,29,12,23,12,29,12,12,60,60,60,53,57,17,57,26,12,57,57,57,39&
        ,57,57,90,57,30,30,39,58,58,12,58,29,58,39,12,58,58,39,58,39&
        ,39,12,15,58,58,17,58,39,58,39,39,58,31,39,39,58,39,26,39,58&
        ,39,58,39,58,39,90,12,59,29,59,29,59,39,29,59,59,59,31,31,59&
        ,59,31,29,29,12,12,12,12,29,29,29,29,29,29,29,29,29,29,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,58,26,29,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,58,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29&
        ,122,58,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29&
        ,25,60,60,60,25,25,25,25,12,25,60,60,25,25,25,25,60,25,25,26&
        ,25,25,25,25,72,60,90,72,26,61,61,61,26,26,26,26,26,26,26,26&
        ,26,26,26,26,26,12,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,122,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,12,26,12,19,12,28,25,65,25,25,25,12,25,65,61,25,25,25,25,25&
        ,26,25,25,25,65,36,12,36,36,36,36,36,66,36,36,36,36,36,36,36&
        ,36,36,36,36,36,46,12,46,26,29,26,42,33,33,33,33,33,33,33,33&
        ,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,29,12,12&
        ,72,35,12,35,72,35,15,15,12,15,72,39,17,35,72,35,35,39,17,97&
        ,72,35,31,19,35,72,35,35,61,35,72,35,39,58,38,12,38,38,38,38&
        ,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,76,29,114&
        ,29,659,43,12,12,12,12,12,39,26,57,12,82,400,29,12,83,83,60&
        ,29,12,83,23,23,12,12,83,29,12,12,12,12,12,83,12,61,12,12,12&
        ,83,31,12,83,26,50,12,50,12,29,12,29,12,83,83,50,83,56,56,29&
        ,29,12,29,12,29,12,83,90,90,12,83,83,29,12,83,29,12,29,12,83&
        ,29,12,83,29,12,29,12,83,29,12,29,12,83,29,12,12,12,84,26,54&
        ,26,17,26,83,26,29,83,28,29,86,28,26,12,83,84,53,83,29,83,83&
        ,29,83,50,12,12,83,29,12,60,91,45,92,29,12,90,90,29,12,90,29&
        ,29,12,12,29,90,29,12,29,29,12,29,29,29,90,17,29,29,29,29,29&
        ,12,12,29,29,90,26,12,26,29,90,29,114,56,29,90,12,12,29,29,29&
        ,29,60,60,60,60,60,60,65,65,114,29,29,29,12,83,83,29,29,114&
        ,61,29,29,29,29,29,29,29,60,65,29,60,60,65,60,65,60,60,65,91&
        ,91,91,91,61,91,61,91,61,15,91,74,61,91,61,61,61,91,26,61,61&
        ,61,91,61,90,12,29,29,58,29,58,58,58,58,58,58,58,58,58,58,58&
        ,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58&
        ,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58&
        ,58,58,58,58,58,58,58,58,58,58,58,29,59,59,59,59,57,38,57,57&
        ,57,57,57,57,82,82,97,97,97,29,12,97,35,9,12,15,97,17,72,39&
        ,31,97,26,35,61,72,90,12,29,12,29,12,29,12,29,12,17,46,29,12&
        ,29,12,12,29,29,12,26,29,29,12,83,29,12,83,83,83,83,29,83,29&
        ,12,83,90,90,12,83,29,12,83,29,12,29,83,83,83,29,12,83,29,12&
        ,83,29,12,83,29,83,29,12,83,29,12,83,29,12,83,29,12,83,29,12&
        ,83,12,12,29,29,12,12,50,50,12,12,12,12,29,29,12,12,12,12,12&
        ,12,12,29,29,12,12,29,12,29,29,12,12,90,12,90,90,90,12,12,12&
        ,12,29,29,12,12,29,29,12,12,122,122,104,104,83,83,90,90,12,12&
        ,29,29,12,12,29,29,12,12,29,29,12,12,60,12,12,106,106,106,53&
        ,12,29,12,12,29,12,12,12,89,12,29,83,12,12,12,12,12,12,60,115&
        ,26,60,29,12,29,39,90,12,114,90,90,114,90,114,90,114,90,29,29&
        ,114,90,114,90,72,90,114,90,114,90,91,91,91,91,91,29,29,12,12&
        ,12,100,29,101,12,101,102,29,29,12,12,29,29,12,90,61,29,29,12&
        ,56,56,29,90,90,12,90,12,122,104,122,83,29,90,12,90,12,104,90&
        ,12,122,122,114,29,90,29,90,12,90,90,12,90,12,90,29,90,12,90&
        ,29,29,29,12,12,26,26,29,29,29,29,12,12,29,29,12,12,29,29,29&
        ,12,29,29,29,12,29,29,12,12,29,29,29,29,12,12,29,29,29,29,12&
        ,12,29,29,29,29,12,12,29,29,12,12,29,29,29,12,12,12,26,26,12&
        ,12,29,12,12,12,12,29,12,12,12,29,12,29,12,29,12,83,29,12,12&
        ,12,12,12,89,12,12,12,114,29,12,29,114,56,29,114,29,12,114&
        ,114,114,114,114,114,114,114,114,114,114,114,114,114,114,114&
        ,114,12,12,12,12,12,29,120,29,29,12,29,114,12,122,122,122,122&
        ,114,114,29,122,122,114,29,12,29,29,29,12,104,12,12,29,12,12&
        ,105,29,12,12,29,12,12,12,29,29,12,83,12,12,83,29,12,29,12,12&
        ,106,12,29,12,12,29,12,29,12,12,29,12,29,12,29,12,50,12,29,12&
        ,12,110,12,12,29,58,12,29,29,60,29,122,132,29,12,83,29,12,12&
        ,124,29,12,125,12,12,29,12,29,12,29,12,29,29,12,29,12,12,127&
        ,12,12,12,12,12,12,12,29,12,133,29,12,12,134,12,29,12,29,12&
        ,29,29,12,12,12,12,12,29,29,12,12,138,29,12,139,29,12,12,141&
        ,12,12,12,12,83,26,26,12,29,12,145,29,12,146,12,29,12,152,12&
        ,12,12,39,12,25,33,17,26,26,29,29,17,12,39,90,39,58,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,12,12,12&
        ,17,15,19,17,12,12,28,12,19,17,12,26,12,25,12,26,12,12,25,12&
        ,49,12,12,29,29,12,12,60,65,12,12,12,12,25,26,12,12,12,12,12&
        ,12,12,25,12,12,12,12,25,12,12,12,12,25,29,29,53,53,110,89&
        ,110,110,127,135,53,23,35,15,15,17,29,26,56,29,60,12,12,29,29&
        ,29,29,29,29,29,29,35,39,29,17,35,17,29,39,17,39,29,29,39,33&
        ,39,31,12,12,12,12,12,12,12,12,12,12,9,12,33,12,12,17,19,12&
        ,17,12,17,20,9,12,19,12,12,12,19,38,36,17,38,12,12,25,26,21&
        ,12,51,12,12,60,29,12,35,12,29,25,89,12,85,12,107,90,35,12&
        ,110,125,127,12,12,126,135,139,141,12,140,9,9,26,12,38,15,26&
        ,19,12,12,15,19,15,12,12,19,20,9,12,20,15,23,25,26,26,12,29&
        ,57,26,12,40,12,28,23,39,36,12,39,39,12,29,38,38,26,38,12,39&
        ,21,26,12,29,26,12,12,90,26,51,26,26,53,12,29,26,26,12,89,26&
        ,26,26,26,26,106,26,110,125,127,134,127,135,139,141,146,141&
        ,147,9,12,12,12,17,12,12,29,12,17,15,12,12,12,12,17,39,56,12&
        ,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,26,75,26&
        ,12,17,60,12,60,12,12,12,12,12,12,57,19,12,12,58,12,12,17,12&
        ,31,26,36,58,17,38,58,12,74,58,58,12,12,26,12,12,12,12,12,12&
        ,12,12,12,12,12,12,12,12,12,12,12,12,90,26,36,26,36,12,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,9,10,12,17,15,9,9,19,12,10,13,12,15,12,17,19,17,12,12,15,17&
        ,19,9,12,15,17,19,19,12,9,9,25,15,9,19,9,9,21,9,29,12,12,23&
        ,12,9,26,12,12,29,12,12,12,12,12,12,12,12,23,12,12,19,12,12&
        ,12,20,12,12,15,12,17,12,39,12,31,12,12,12,15,29,12,15,12,12&
        ,36,12,19,17,49,29,12,19,12,21,9,12,29,12,25,17,12,12,23,9,29&
        ,9,23,17,12,25,15,25,12,9,12,25,26,28,9,19,21,9,56,29,12,9,12&
        ,29,12,12,12,12,12,12,12,12,12,29,12,29,12,39,12,31,12,39,29&
        ,12,12,29,12,12,12,23,12,12,26,12,35,28,12,39,12,12,39,12,58&
        ,12,12,12,23,12,29,12,29,12,18,20,12,21,9,29,12,23,12,29,12&
        ,60,56,29,58,26,17,12,29,12,12,9,23,56,56,12,12,29,29,9,9,12&
        ,60,25,12,21,9,23,12,21,9,21,9,21,28,9,21,9,12,12,12,12,12,12&
        ,12,12,29,12,29,12,17,12,23,12,12,29,12,29,83,29,12,23,12,9&
        ,29,29,18,18,29,12,84,12,21,9,29,12,29,12,29,60,25,12,26,12&
        ,12,29,12,29,9,23,56,65,12,12,12,12,23,23,29,12,29,12,9,12,85&
        ,21,9,29,12,29,72,12,12,39,29,29,12,12,29,29,26,39,12,29,60&
        ,25,12,12,12,12,23,26,23,50,23,12,65,12,83,29,29,84,50,23,12&
        ,29,12,18,29,12,29,12,23,12,29,12,29,12,12,12,61,12,29,29,29&
        ,29,12,29,29,29,12,23,56,90,12,29,12,29,12,106,50,12,9,12,53&
        ,21,9,72,29,29,58,39,58,12,23,58,29,12,26,58,29,91,122,29,29&
        ,83,29,90,29,26,12,29,12,29,29,12,29,12,29,29,12,29,12,29,12&
        ,12,29,12,9,12,126,21,9,29,12,57,26,56,29,91,29,39,115,12,29&
        ,12,23,104,122,29,29,12,29,12,29,12,29,12,134,12,9,12,21,9,90&
        ,114,56,61,12,21,9,12,9,29,58,29,12,90,56,29,29,29,12,29,29&
        ,29,12,29,12,29,12,9,12,140,114,90,12,90,12,29,114,122,130&
        ,137,12,29,29,29,29,29,29,29,12,29,12,146,12,29,12,29,29,29&
        ,12,29,12,12,29,150,12,29,12,29,29,29,29,29,29,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,9,9,19,9&
        ,12,19,9,12,19,9,13,19,9,12,19,9,15,19,9,12,19,9,17,19,9,12&
        ,19,9,12,19,29,29,29,29,29,29,659,659,659,659,1,1,1,1,1,1,1,9&
        ,1,9,12,1,17,15,17,19,12,12,19,15,15,1,17,1,19,17,1,12,1,9,12&
        ,17,19,19,12,1,15,9,19,9,12,1,17,12,1,12,12,26,1,12,12,39,12&
        ,1,17,25,19,1,17,12,1,12,25,26,28,21,9,29,12,1,12,29,12,1,29&
        ,29,12,1,26,1,12,1,29,12,21,12,25,28,21,39,1,29,12,29,12,25&
        ,25,1,29,1,29,12,50,12,1,53,21,12,25,29,1,61,26,1,29,12,1,9&
        ,50,12,89,53,29,29,72,12,1,126,25,90,58,1,90,114,1,9,12,12,17&
        ,12,12,12,29,12,29,12,1,1,1,1,1,1,659,170,170,170,170,170,170&
        ,170,170,170,170,12,26,17,12,26,26,75,23,12,50,12,15,29,29,12&
        ,35,26,25,25,25,25,39,39,61,438,23,29,26,39,526,186,186,12,85&
        ,12,12,126,12,12,140,12,25,29,9,12,29,12,29,29,29,12,83,29,9&
        ,12,12,29,29,29,29,12,83,29,29,9,12,12,29,29,29,12,83,29,29,9&
        ,12,12,29,29,29,12,83,29,29,9,12,29,29,29,12,83,29,25,29,29&
        ,56,29,114,90,23,29,29,29,29,29,29,29,29,60,17,61,12,659,91&
        ,29,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,237,659,659,237&
        ,243,659,246,659,243,234,237,659,246,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,231,237&
        ,237,659,659,243,243,659,659,659,242,659,243,659,659,659,659&
        ,659,659,659,659,242,233,659,659,659,659,659,659,659,282,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,282&
        ,659,659,659,659,659,659,659,659,659,659,659,282,659,249,282&
        ,235,659,659,659,659,659,659,659,659,659,659,282,659,659,659&
        ,659,330,659,659,659,659,659,659,659,249,659,235,659,659,659&
        ,659,659,659,235,659,237,237,237,237,237,237,237,237,237,237&
        ,237,237,237,237,237,237,237,237,237,237,237,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,237,659,237,659,246&
        ,659,239,659,659,659,659,282,243,659,659,330,659,659,659,659&
        ,659,659,659,659,237,249,249,237,659,237,246,659,659,249,659&
        ,659,659,659,659,237,319,659,659,659,659,237,659,237,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,204,204,237,237,237,282,237&
        ,282,281,234,204,243,204,204,237,237,237,234,237,234,247,204&
        ,236,204,237,207,237,237,282,234,204,236,204,237,237,237,282&
        ,234,204,237,237,237,282,282,204,237,237,237,282,234,204,237&
        ,237,237,282,204,237,237,237,282,234,370,371,204,237,234,247&
        ,282,241,249,282,237,212,218,271,222,273,265,247,234,237,281&
        ,249,282,282,282,239,243,238,243,243,371,246,237,234,247,234&
        ,234,234,239,282,236,237,249,246,235,282,212,271,273,234,234&
        ,234,230,234,207,234,234,234,237,237,370,237,234,237,275,249&
        ,237,337,237,234,247,237,237,246,246,237,237,238,238,238,237&
        ,249,237,237,265,234,249,237,282,243,243,246,243,237,249,243&
        ,243,246,237,237,237,238,243,246,204,204,237,282,204,237,205&
        ,282,237,237,237,237,282,238,234,243,243,246,243,243,237,237&
        ,230,237,282,237,249,282,269,282,282,237,282,237,282,237,237&
        ,319,247,243,282,237,282,237,265,237,282,234,237,282,262,231&
        ,233,281,237,237,282,249,282,241,282,204,237,237,237,237,237&
        ,237,237,237,237,237,243,243,243,246,204,204,237,237,237,282&
        ,237,319,282,281,238,204,204,243,246,204,237,282,237,237,282&
        ,234,282,237,234,247,204,236,205,204,205,237,207,237,237,204&
        ,234,204,236,205,206,205,237,207,237,237,282,234,204,206,217&
        ,237,237,282,282,204,237,237,237,282,234,204,237,237,237,282&
        ,204,237,237,237,282,234,204,204,371,237,237,234,247,282,241&
        ,249,282,282,237,233,233,271,222,273,247,265,234,237,281,282&
        ,282,282,237,237,282,282,241,239,243,238,243,243,371,246,234&
        ,247,234,234,234,237,282,237,282,237,249,237,235,282,212,271&
        ,273,234,234,234,230,234,207,234,271,234,234,237,237,237,237&
        ,234,247,275,249,237,337,237,234,247,237,237,237,246,246,237&
        ,237,238,238,243,237,249,238,237,265,234,249,282,237,243,243&
        ,246,243,243,237,243,234,246,237,237,237,371,371,246,204,204&
        ,237,282,204,237,205,282,237,237,237,237,282,238,337,243,243&
        ,246,246,243,205,204,204,237,204,237,237,230,282,237,237,237&
        ,282,237,237,282,282,282,249,282,269,282,231,233,204,282,282&
        ,282,237,282,237,204,205,204,205,282,237,237,204,205,206,237&
        ,204,237,282,204,205,237,204,282,237,204,205,204,237,237,282&
        ,273,204,371,237,282,237,237,262,231,282,319,247,243,282,282&
        ,371,204,237,282,237,282,282,265,237,282,234,237,282,262,231&
        ,233,281,249,282,241,282,371,237,237,237,237,237,237,243,237&
        ,237,235,237,249,282,237,222,237,226,237,230,237,247,345,237&
        ,371,237,237,230,237,237,337,237,243,246,237,237,237,237,237&
        ,237,243,243,237,243,246,237,237,282,237,238,243,246,205,204&
        ,337,237,282,237,205,204,208,237,205,204,231,282,237,282,237&
        ,282,237,237,282,237,282,237,238,234,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659&
        ,659/)
    arr_p3(1:5639) = (/659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,1&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,1,659,659,659,659,659,659,659,659,659,659,659,12,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,12,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,13&
        ,659,13,13,659,13,13,13,13,13,13,13,13,13,659,13,13,13,13,659&
        ,13,13,13,13,13,13,13,659,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,659,13,13,659,13,13,659,12,13,13,13,13,13,659,13,13,13&
        ,13,13,13,12,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,659,13,13,13&
        ,659,13,13,659,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,659,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,13,13,13,12,13,13,13,13,13,13,13,13&
        ,12,13,13,659,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,29,13,13&
        ,13,659,29,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,659,13,29,12,29,29,12,12,12,9,9,13,13&
        ,13,13,13,13,13,13,13,13,13,29,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,29,29,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,15,659,659,659,659,659,659,659,659,12,12,659,12,659,659&
        ,12,12,12,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,12,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,12,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,29,29,659,659,659&
        ,659,659,659,659,29,659,659,29,659,659,29,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,29,12&
        ,659,659,659,659,659,26,659,12,659,659,33,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,12,659,659,659,12,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,12&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,29,29,659,29,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,29,659,29,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,39,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,29,29,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,29,659,659,659,659,659,659,29,29,659,659,659,659&
        ,659,29,29,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,29,29,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,29,29,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,26,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,12,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,12,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,29,29,659,659,659,659,659,659,659,659,659,659,659,659,659,29&
        ,29,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,12,12,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,12,12&
        ,659,659,659,659,659,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,12,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,12,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,26,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,1,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,12,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,12,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,12,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,12,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,12,659,659,659,659,659,659,659,659,659,659,12&
        ,659,659,659,659,12,659,659,659,12,12,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,12,659,659,659,659,659,659&
        ,659,659,659,659,659,659,12,659,659,659,659,659,12,659,659&
        ,659,12,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,12,12,659,659,12,12,659,659,12,659&
        ,12,659,659,659,659,659,659,659,659,659,659,659,12,659,659,17&
        ,12,659,659,659,17,659,659,659,659,659,659,659,17,659,659,659&
        ,659,12,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,12,659,659,29&
        ,659,659,659,659,12,12,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,12,12,659,29&
        ,12,659,29,659,659,659,12,659,659,659,659,659,12,659,659,659&
        ,659,659,659,659,659,659,12,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,12,659,659,659,659,659,12,659,659,12,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,12,659,29,659,29,659,659,659,659,659,12,12,29&
        ,659,659,12,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,12,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,12,659,659,12,659,29,12,659&
        ,659,12,659,659,659,659,659,659,12,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,12,659,12,12,659,659,659,12&
        ,659,12,659,29,659,659,29,12,659,659,659,12,12,659,659,659,29&
        ,12,659,659,659,12,659,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,12,659,12,659,659,29,659,659,659,659,659,29,29&
        ,659,659,659,12,659,29,659,12,659,659,659,659,659,659,659,659&
        ,659,659,17,659,659,659,659,659,659,659,659,659,12,12,659,26&
        ,659,29,12,12,659,12,659,659,659,659,659,659,659,659,659,659&
        ,17,659,659,659,659,29,659,659,659,659,659,29,12,12,659,12&
        ,659,659,659,659,659,659,659,12,659,12,659,659,659,659,659&
        ,659,29,659,659,29,29,12,659,12,659,29,12,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,12&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,12,659,1,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,29,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,29,12,659,12,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,170,170,659,659,659,659,13,659,659,659,659,659,659&
        ,29,12,659,659,659,659,659,659,659,659,659,659,659,659,659,13&
        ,13,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,29,659,13,13,13,659,659,659,659,659,29,659,13,13&
        ,13,659,659,659,659,659,659,659,29,13,13,13,659,659,659,659&
        ,659,659,29,13,13,13,659,659,659,659,659,29,13,13,13,659,659&
        ,29,25,659,659,659,659,12,29,12,659,29,29,13,12,659,659,659&
        ,659,659,12,12,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,237,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,237,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,237,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,237,659,237,659,659,659,282,659,659,659,659,659,659&
        ,659,659,237,659,659,659,237,237,659,659,659,237,659,659,659&
        ,237,659,659,243,659,237,237,237,659,659,659,659,659,237,659&
        ,659,237,659,659,659,237,659,237,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,237,659,237,282,237,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,237,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,237,659,659&
        ,659,237,237,659,659,659,282,237,659,659,282,659,659,659,247&
        ,659,237,659,659,659,659,659,659,659,659,659,659,659,237,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,237,659,659,659,659,659&
        ,659,237,659,659,237,237,659,659,659,237,659,237,659,659,659&
        ,237,659,659,243,659,659,659,659,659,659,237,659,659,659,237&
        ,237,659,659,237,659,659,659,659,659,659,659,659,659,659,659&
        ,659,237,237,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,237,659,237,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,237,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659&
        ,659/)
    arr_p4(1:5639) = (/659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,12,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,13,659,659,659,659,659&
        ,659,659,659,659,659,659,659,13,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,13,659,659,659,659,659,659,659,659,13,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,13,659,659,659,659,13,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,13,29,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,13,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,13,13,659,659&
        ,659,659,659,659,659,659,659,659,659,659,12,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,29,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,12,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,12,12,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,13,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,13,659,659,659,659,659,659&
        ,659,659,659,13,659,13,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,237,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,237,237,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,237,237,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659,659,659,659,659,659,659,659,659,659&
        ,659,659,659,659,659,659/)

  end subroutine load_arrays

  ! ************************************
  ! solves linear least squares
  subroutine llsq(n, x, y, a, b)

    !****************************************************
    !
    !! LLSQ solves a linear least squares problem matching a line to data.
    !
    !  Discussion:
    !
    !    A formula for a line of the form Y = A * X + B is sought, which
    !    will minimize the root-mean-square error to N data points
    !    ( X(I), Y(I) );
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 March 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    In: N, the number of data values.
    !
    !    In: X(N), Y(N), the coordinates of the data points.
    !
    !    Out: A, B, the slope and Y-intercept of the
    !    least-squares approximant to the data.
    !
    implicit none
    integer,intent(in)::n
    real*8,intent(out)::a, b
    real*8,intent(in)::x(n), y(n)
    real*8::bot, top, xbar, ybar

    ! special case
    if(n == 1) then
      a = 0d0
      b = y(1)
      return
    end if

    ! average X and Y
    xbar = sum(x) / n
    ybar = sum(y) / n

    ! compute beta
    top = dot_product(x(:) - xbar, y(:) - ybar)
    bot = dot_product(x(:) - xbar, x(:) - xbar)

    ! if top is zero a is zero
    if(top==0d0) then
      a = 0d0
    else
      a = top / bot
    end if

    b = ybar - a * xbar

  end subroutine llsq

end module krome_subs
