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
    get_mass(3) = 2.0077106047315998d-23	!C-
    get_mass(4) = 4.3499333674632d-23	!CN-
    get_mass(5) = 2.6769171083692d-23	!O-
    get_mass(6) = 2.8442703601880005d-23	!OH-
    get_mass(7) = 5.3537431229196d-23	!S-
    get_mass(8) = 2.0076204218509883d-21	!GRAIN-
    get_mass(9) = 2.0076195109128d-23	!C
    get_mass(10) = 5.8555052809196d-23	!CL
    get_mass(11) = 9.201434547288801d-23	!FE
    get_mass(12) = 1.673532518188d-24	!H
    get_mass(13) = 6.692065036376d-24	!HE
    get_mass(14) = 4.0152390218256d-23	!MG
    get_mass(15) = 2.3422227627316d-23	!N
    get_mass(16) = 3.8478857700068d-23	!NA
    get_mass(17) = 2.6768260145504d-23	!O
    get_mass(18) = 5.1862987772820005d-23	!P
    get_mass(19) = 5.3536520291008d-23	!S
    get_mass(20) = 4.6844455254632d-23	!SI
    get_mass(21) = 4.0152390218256d-23	!C2
    get_mass(22) = 7.863124791832399d-23	!CCL
    get_mass(23) = 2.1749727627316d-23	!CH
    get_mass(24) = 8.53233129547d-23	!CLO
    get_mass(25) = 4.3498422736443996d-23	!CN
    get_mass(26) = 4.6844455254632d-23	!CO
    get_mass(27) = 7.193918288194801d-23	!CP
    get_mass(28) = 7.3612715400136d-23	!CS
    get_mass(29) = 3.347065036376d-24	!H2
    get_mass(30) = 6.0228585327384d-23	!HCL
    get_mass(31) = 5.5210052809196d-23	!HS
    get_mass(32) = 4.1825922736444d-23	!MGH
    get_mass(33) = 4.6844455254632d-23	!N2
    get_mass(34) = 4.0152390218256004d-23	!NAH
    get_mass(35) = 2.5095760145504d-23	!NH
    get_mass(36) = 5.0190487772820004d-23	!NO
    get_mass(37) = 7.6958747918324d-23	!NS
    get_mass(38) = 5.3536520291008d-23	!O2
    get_mass(39) = 2.8441792663692003d-23	!OH
    get_mass(40) = 5.3536520291008d-23	!PH
    get_mass(41) = 7.528521540013601d-23	!PN
    get_mass(42) = 7.8631247918324d-23	!PO
    get_mass(43) = 1.07073040582016d-22	!S2
    get_mass(44) = 6.692065036376d-23	!SIC
    get_mass(45) = 4.851798777282d-23	!SIH
    get_mass(46) = 7.0266682881948d-23	!SIN
    get_mass(47) = 7.3612715400136d-23	!SIO
    get_mass(48) = 1.0038097554564001d-22	!SIS
    get_mass(49) = 8.0304780436512d-23	!SO
    get_mass(50) = 4.1825922736444d-23	!C2H
    get_mass(51) = 6.3574617845572d-23	!C2N
    get_mass(52) = 9.3688910509264d-23	!C2S
    get_mass(53) = 6.0228585327384d-23	!C3
    get_mass(54) = 6.692065036376d-23	!CCO
    get_mass(55) = 9.2015377991076d-23	!CCP
    get_mass(56) = 2.3423260145503998d-23	!CH2
    get_mass(57) = 7.3612715400136d-23	!CO2
    get_mass(58) = 3.011532518188d-23	!H2O
    get_mass(59) = 5.688358532738401d-23	!H2S
    get_mass(60) = 4.5171955254632d-23	!HCN
    get_mass(61) = 4.8517987772819996d-23	!HCO
    get_mass(62) = 7.3612715400136d-23	!HCP
    get_mass(63) = 7.5286247918324d-23	!HCS
    get_mass(64) = 6.8594182881948d-23	!HCSI
    get_mass(65) = 4.5171955254632d-23	!HNC
    get_mass(66) = 5.1864020291008d-23	!HNO
    get_mass(67) = 7.1940215400136d-23	!HNSI
    get_mass(68) = 8.030478043651201d-23	!HPO
    get_mass(69) = 1.08746573100204d-22	!HS2
    get_mass(70) = 7.3612715400136d-23	!N2O
    get_mass(71) = 6.692065036376d-23	!NAOH
    get_mass(72) = 2.6769292663692d-23	!NH2
    get_mass(73) = 7.6958747918324d-23	!NO2
    get_mass(74) = 5.5210052809196d-23	!O2H
    get_mass(75) = 7.0266682881948d-23	!OCN
    get_mass(76) = 1.0038097554564001d-22	!OCS
    get_mass(77) = 5.5210052809196d-23	!PH2
    get_mass(78) = 8.699684547288799d-23	!SIC2
    get_mass(79) = 5.0191520291008d-23	!SIH2
    get_mass(80) = 9.0342877991076d-23	!SINC
    get_mass(81) = 1.0038097554564001d-22	!SIO2
    get_mass(82) = 1.07073040582016d-22	!SO2
    get_mass(83) = 4.3499455254632d-23	!C2H2
    get_mass(84) = 6.1902117845572d-23	!C3H
    get_mass(85) = 8.36508129547d-23	!C3N
    get_mass(86) = 8.699684547288799d-23	!C3O
    get_mass(87) = 1.12091573100204d-22	!C3P
    get_mass(88) = 1.13765105618392d-22	!C3S
    get_mass(89) = 8.0304780436512d-23	!C4
    get_mass(90) = 2.5096792663692d-23	!CH3
    get_mass(91) = 5.0191520291008d-23	!H2CO
    get_mass(92) = 7.6959780436512d-23	!H2CS
    get_mass(93) = 5.688358532738401d-23	!H2O2
    get_mass(94) = 1.10420105618392d-22	!H2S2
    get_mass(95) = 7.6959780436512d-23	!H2SIO
    get_mass(96) = 9.368891050926401d-23	!HCCP
    get_mass(97) = 2.844282518188d-23	!NH3
    get_mass(98) = 8.867037799107601d-23	!SIC2H
    get_mass(99) = 1.07073040582016d-22	!SIC3
    get_mass(100) = 7.0267715400136d-23	!SICH2
    get_mass(101) = 5.1865052809196d-23	!SIH3
    get_mass(102) = 6.6921682881948d-23	!C2H2N
    get_mass(103) = 7.0267715400136d-23	!C2H2O
    get_mass(104) = 4.517298777282d-23	!C2H3
    get_mass(105) = 6.357565036376d-23	!C3H2
    get_mass(106) = 8.19783129547d-23	!C4H
    get_mass(107) = 1.0372700806382799d-22	!C4N
    get_mass(108) = 1.32167768209332d-22	!C4P
    get_mass(109) = 1.3384130072752d-22	!C4S
    get_mass(110) = 1.0038097554564d-22	!C5
    get_mass(111) = 7.6959780436512d-23	!CH2O2
    get_mass(112) = 7.6959780436512d-23	!CH2PH
    get_mass(113) = 4.8519020291008004d-23	!CH3N
    get_mass(114) = 2.677032518188d-23	!CH4
    get_mass(115) = 8.5324345472888d-23	!HC3N
    get_mass(116) = 9.0343910509264d-23	!SIC2H2
    get_mass(117) = 1.08746573100204d-22	!SIC3H
    get_mass(118) = 1.27149235691144d-22	!SIC4
    get_mass(119) = 7.1941247918324d-23	!SICH3
    get_mass(120) = 5.3538585327384d-23	!SIH4
    get_mass(121) = 6.8595215400136d-23	!C2H3N
    get_mass(122) = 4.6846520291007997d-23	!C2H4
    get_mass(123) = 6.524918288194799d-23	!C3H3
    get_mass(124) = 8.3651845472888d-23	!C4H2
    get_mass(125) = 1.02054508063828d-22	!C5H
    get_mass(126) = 1.23803203172956d-22	!C5N
    get_mass(127) = 1.20457170654768d-22	!C6
    get_mass(128) = 5.3538585327384d-23	!CH4O
    get_mass(129) = 7.361478043651199d-23	!C2H4O
    get_mass(130) = 4.8520052809196d-23	!C2H5
    get_mass(131) = 8.867141050926399d-23	!C3H3N
    get_mass(132) = 6.6922715400136d-23	!C3H4
    get_mass(133) = 1.03728040582016d-22	!C5H2
    get_mass(134) = 1.2213070317295599d-22	!C6H
    get_mass(135) = 1.40533365763896d-22	!C7
    get_mass(136) = 5.1866085327383997d-23	!CH5N
    get_mass(137) = 1.25476735691144d-22	!HC5N
    get_mass(138) = 1.23804235691144d-22	!C6H2
    get_mass(139) = 1.42206898282084d-22	!C7H
    get_mass(140) = 1.6395559339121201d-22	!C7N
    get_mass(141) = 1.60609560873024d-22	!C8
    get_mass(142) = 1.08747605618392d-22	!CH3C3N
    get_mass(143) = 1.00383040582016d-22	!HCOOCH3
    get_mass(144) = 7.6961845472888d-23	!C2H5OH
    get_mass(145) = 1.43880430800272d-22	!C7H2
    get_mass(146) = 1.6228309339121198d-22	!C8H
    get_mass(147) = 1.8068575598215198d-22	!C9
    get_mass(148) = 1.07075105618392d-22	!CH3C4H
    get_mass(149) = 7.6961845472888d-23	!CH3OCH3
    get_mass(150) = 1.656291259094d-22	!HC7N
    get_mass(151) = 9.703804058201601d-23	!C2H6CO
    get_mass(152) = 1.639566259094d-22	!C8H2
    get_mass(153) = 1.8235928850033997d-22	!C9H
    get_mass(154) = 2.04107983609468d-22	!C9N
    get_mass(155) = 2.0076195109128d-22	!C10
    get_mass(156) = 1.48899995836648d-22	!CH3C5N
    get_mass(157) = 1.8403282101852799d-22	!C9H2
    get_mass(158) = 1.47227495836648d-22	!CH3C6H
    get_mass(159) = 1.8905238605490402d-22	!CH3C7N
    get_mass(160) = 2.0578151612765598d-22	!HC9N
    get_mass(161) = 8.6998910509264d-23	!C4H4
    get_mass(162) = 8.5324345472888d-23	!HCNC2
    get_mass(163) = 8.5324345472888d-23	!HC2NC
    get_mass(164) = 8.5324345472888d-23	!HNC3
    get_mass(165) = 7.5287280436512d-23	!NH2CHO
    get_mass(166) = 8.532537799107599d-23	!C4H3
    get_mass(167) = 7.0267715400136d-23	!NH2CN
    get_mass(168) = 1.3049836576389599d-22	!C6H6
    get_mass(169) = 4.684548777282d-23	!H2CN
    get_mass(170) = 2.0076195109128002d-21	!GRAIN0
    get_mass(171) = 8.0304780436512d-23	!O3
    get_mass(172) = 9.368787799107602d-23	!FEH
    get_mass(173) = 7.194021540013599d-23	!HNCO
    get_mass(174) = 6.8594182881948d-23	!HC2O
    get_mass(175) = 6.524815036376d-23	!HCCN
    get_mass(176) = 8.867037799107601d-23	!HC3O
    get_mass(177) = 4.3499455254632d-23	!MGH2
    get_mass(178) = 5.0191520291008d-23	!N2H2
    get_mass(179) = 4.684548777282d-23	!CHNH
    get_mass(180) = 9.0343910509264d-23	!H2C3O
    get_mass(181) = 8.6997877991076d-23	!H2C3N
    get_mass(182) = 1.27150268209332d-22	!H2C5N
    get_mass(183) = 1.6730265842758802d-22	!H2C7N
    get_mass(184) = 2.07455048645844d-22	!H2C9N
    get_mass(185) = 5.5211085327384d-23	!NH2OH
    get_mass(186) = 5.1865052809196d-23	!CH2OH
    get_mass(187) = 1.0540157310020399d-22	!C5H3
    get_mass(188) = 1.2882380072751999d-22	!H3C5N
    get_mass(189) = 1.25477768209332d-22	!C6H3
    get_mass(190) = 1.4555396331846d-22	!C7H3
    get_mass(191) = 1.68976190945776d-22	!H3C7N
    get_mass(192) = 1.65630158427588d-22	!C8H3
    get_mass(193) = 1.8570635353671598d-22	!C9H3
    get_mass(194) = 2.0912858116403198d-22	!H3C9N
    get_mass(195) = 5.0192552809195994d-23	!CH3NH
    get_mass(196) = 9.034494302745199d-23	!H4C3N
    get_mass(197) = 1.07075105618392d-22	!C5H4
    get_mass(198) = 1.2715130072752d-22	!C6H4
    get_mass(199) = 1.4722749583664802d-22	!C7H4
    get_mass(200) = 1.67303690945776d-22	!C8H4
    get_mass(201) = 1.87379886054904d-22	!C9H4
    get_mass(202) = 9.201847554564d-23	!H5C3N
    get_mass(203) = 5.0193585327384d-23	!C2H6
    get_mass(204) = 2.0076195109128d-23	!C_DUST
    get_mass(205) = 4.0152390218256d-23	!C2_DUST
    get_mass(206) = 6.0228585327384d-23	!C3_DUST
    get_mass(207) = 4.1825922736444d-23	!C2H_DUST
    get_mass(208) = 6.1902117845572d-23	!C3H_DUST
    get_mass(209) = 4.517298777282d-23	!C2H3_DUST
    get_mass(210) = 6.524918288194799d-23	!C3H3_DUST
    get_mass(211) = 6.3574617845572d-23	!C2N_DUST
    get_mass(212) = 8.36508129547d-23	!C3N_DUST
    get_mass(213) = 6.692065036376d-23	!CCO_DUST
    get_mass(214) = 8.699684547288799d-23	!C3O_DUST
    get_mass(215) = 9.3688910509264d-23	!C2S_DUST
    get_mass(216) = 1.13765105618392d-22	!C3S_DUST
    get_mass(217) = 8.0304780436512d-23	!C4_DUST
    get_mass(218) = 8.19783129547d-23	!C4H_DUST
    get_mass(219) = 1.0038097554564d-22	!C5_DUST
    get_mass(220) = 1.02054508063828d-22	!C5H_DUST
    get_mass(221) = 1.20457170654768d-22	!C6_DUST
    get_mass(222) = 1.2213070317295599d-22	!C6H_DUST
    get_mass(223) = 1.40533365763896d-22	!C7_DUST
    get_mass(224) = 1.42206898282084d-22	!C7H_DUST
    get_mass(225) = 1.60609560873024d-22	!C8_DUST
    get_mass(226) = 1.6228309339121198d-22	!C8H_DUST
    get_mass(227) = 1.8068575598215198d-22	!C9_DUST
    get_mass(228) = 1.8235928850033997d-22	!C9H_DUST
    get_mass(229) = 2.0076195109128d-22	!C10_DUST
    get_mass(230) = 2.1749727627316d-23	!CH_DUST
    get_mass(231) = 2.3423260145503998d-23	!CH2_DUST
    get_mass(232) = 4.3499455254632d-23	!C2H2_DUST
    get_mass(233) = 2.5096792663692d-23	!CH3_DUST
    get_mass(234) = 4.3498422736443996d-23	!CN_DUST
    get_mass(235) = 5.5210052809196d-23	!HS_DUST
    get_mass(236) = 7.3612715400136d-23	!CS_DUST
    get_mass(237) = 1.673532518188d-24	!H_DUST
    get_mass(238) = 2.3422227627316d-23	!N_DUST
    get_mass(239) = 2.5095760145504d-23	!NH_DUST
    get_mass(240) = 4.5171955254632d-23	!HNC_DUST
    get_mass(241) = 2.6769292663692d-23	!NH2_DUST
    get_mass(242) = 5.0190487772820004d-23	!NO_DUST
    get_mass(243) = 2.6768260145504d-23	!O_DUST
    get_mass(244) = 7.0266682881948d-23	!OCN_DUST
    get_mass(245) = 7.6958747918324d-23	!NS_DUST
    get_mass(246) = 5.3536520291008d-23	!S_DUST
    get_mass(247) = 4.6844455254632d-23	!CO_DUST
    get_mass(248) = 5.3536520291008d-23	!O2_DUST
    get_mass(249) = 2.8441792663692003d-23	!OH_DUST
    get_mass(250) = 8.0304780436512d-23	!SO_DUST
    get_mass(251) = 6.357565036376d-23	!C3H2_DUST
    get_mass(252) = 6.6922715400136d-23	!C3H4_DUST
    get_mass(253) = 8.3651845472888d-23	!C4H2_DUST
    get_mass(254) = 1.03728040582016d-22	!C5H2_DUST
    get_mass(255) = 1.23804235691144d-22	!C6H2_DUST
    get_mass(256) = 1.43880430800272d-22	!C7H2_DUST
    get_mass(257) = 1.639566259094d-22	!C8H2_DUST
    get_mass(258) = 1.8403282101852799d-22	!C9H2_DUST
    get_mass(259) = 4.6846520291007997d-23	!C2H4_DUST
    get_mass(260) = 6.524815036376d-23	!HCCN_DUST
    get_mass(261) = 5.1864020291008d-23	!HNO_DUST
    get_mass(262) = 4.5171955254632d-23	!HCN_DUST
    get_mass(263) = 4.684548777282d-23	!CHNH_DUST
    get_mass(264) = 4.8519020291008004d-23	!CH3N_DUST
    get_mass(265) = 4.8517987772819996d-23	!HCO_DUST
    get_mass(266) = 4.8520052809196d-23	!C2H5_DUST
    get_mass(267) = 6.6921682881948d-23	!C2H2N_DUST
    get_mass(268) = 5.0192552809195994d-23	!CH2NH2_DUST
    get_mass(269) = 5.0191520291008d-23	!H2CO_DUST
    get_mass(270) = 1.08747605618392d-22	!CH3C3N_DUST
    get_mass(271) = 1.23803203172956d-22	!C5N_DUST
    get_mass(272) = 1.48899995836648d-22	!CH3C5N_DUST
    get_mass(273) = 1.6395559339121201d-22	!C7N_DUST
    get_mass(274) = 1.8905238605490402d-22	!CH3C7N_DUST
    get_mass(275) = 5.1865052809196d-23	!CH2OH_DUST
    get_mass(276) = 7.6961845472888d-23	!C2H5OH_DUST
    get_mass(277) = 7.6961845472888d-23	!CH3OCH3_DUST
    get_mass(278) = 5.0193585327384d-23	!C2H6_DUST
    get_mass(279) = 6.8595215400136d-23	!C2H3N_DUST
    get_mass(280) = 7.361478043651199d-23	!C2H4O_DUST
    get_mass(281) = 2.677032518188d-23	!CH4_DUST
    get_mass(282) = 3.347065036376d-24	!H2_DUST
    get_mass(283) = 6.8594182881948d-23	!HC2O_DUST
    get_mass(284) = 8.867141050926399d-23	!C3H3N_DUST
    get_mass(285) = 9.034494302745199d-23	!H4C3N_DUST
    get_mass(286) = 8.5324345472888d-23	!HC3N_DUST
    get_mass(287) = 8.867037799107601d-23	!HC3O_DUST
    get_mass(288) = 8.532537799107599d-23	!C4H3_DUST
    get_mass(289) = 8.6998910509264d-23	!C4H4_DUST
    get_mass(290) = 1.0540157310020399d-22	!C5H3_DUST
    get_mass(291) = 1.07075105618392d-22	!C5H4_DUST
    get_mass(292) = 1.25476735691144d-22	!HC5N_DUST
    get_mass(293) = 1.25477768209332d-22	!C6H3_DUST
    get_mass(294) = 1.2715130072752d-22	!C6H4_DUST
    get_mass(295) = 1.4555396331846d-22	!C7H3_DUST
    get_mass(296) = 1.4722749583664802d-22	!C7H4_DUST
    get_mass(297) = 1.656291259094d-22	!HC7N_DUST
    get_mass(298) = 1.65630158427588d-22	!C8H3_DUST
    get_mass(299) = 1.67303690945776d-22	!C8H4_DUST
    get_mass(300) = 1.8570635353671598d-22	!C9H3_DUST
    get_mass(301) = 1.87379886054904d-22	!C9H4_DUST
    get_mass(302) = 2.04107983609468d-22	!C9N_DUST
    get_mass(303) = 2.0578151612765598d-22	!HC9N_DUST
    get_mass(304) = 5.0192552809195994d-23	!CH3NH_DUST
    get_mass(305) = 5.1866085327383997d-23	!CH5N_DUST
    get_mass(306) = 5.3538585327384d-23	!CH4O_DUST
    get_mass(307) = 7.5286247918324d-23	!HCS_DUST
    get_mass(308) = 9.201434547288801d-23	!FE_DUST
    get_mass(309) = 9.368787799107602d-23	!FEH_DUST
    get_mass(310) = 8.6997877991076d-23	!H2C3N_DUST
    get_mass(311) = 1.27150268209332d-22	!H2C5N_DUST
    get_mass(312) = 1.2882380072751999d-22	!H3C5N_DUST
    get_mass(313) = 1.6730265842758802d-22	!H2C7N_DUST
    get_mass(314) = 1.68976190945776d-22	!H3C7N_DUST
    get_mass(315) = 2.07455048645844d-22	!H2C9N_DUST
    get_mass(316) = 2.0912858116403198d-22	!H3C9N_DUST
    get_mass(317) = 4.684548777282d-23	!H2CN_DUST
    get_mass(318) = 5.688358532738401d-23	!H2O2_DUST
    get_mass(319) = 3.011532518188d-23	!H2O_DUST
    get_mass(320) = 5.5210052809196d-23	!O2H_DUST
    get_mass(321) = 5.688358532738401d-23	!H2S_DUST
    get_mass(322) = 9.201847554564d-23	!H5C3N_DUST
    get_mass(323) = 7.0267715400136d-23	!C2H2O_DUST
    get_mass(324) = 9.0343910509264d-23	!H2C3O_DUST
    get_mass(325) = 7.6959780436512d-23	!H2CS_DUST
    get_mass(326) = 4.0152390218256d-23	!MG_DUST
    get_mass(327) = 4.1825922736444d-23	!MGH_DUST
    get_mass(328) = 4.3499455254632d-23	!MGH2_DUST
    get_mass(329) = 5.0191520291008d-23	!N2H2_DUST
    get_mass(330) = 4.6844455254632d-23	!N2_DUST
    get_mass(331) = 3.8478857700068d-23	!NA_DUST
    get_mass(332) = 4.0152390218256004d-23	!NAH_DUST
    get_mass(333) = 2.844282518188d-23	!NH3_DUST
    get_mass(334) = 8.0304780436512d-23	!O3_DUST
    get_mass(335) = 7.194021540013599d-23	!HNCO_DUST
    get_mass(336) = 1.0038097554564001d-22	!OCS_DUST
    get_mass(337) = 4.6844455254632d-23	!SI_DUST
    get_mass(338) = 4.851798777282d-23	!SIH_DUST
    get_mass(339) = 5.0191520291008d-23	!SIH2_DUST
    get_mass(340) = 5.1865052809196d-23	!SIH3_DUST
    get_mass(341) = 5.3538585327384d-23	!SIH4_DUST
    get_mass(342) = 1.07073040582016d-22	!SO2_DUST
    get_mass(343) = 1.00383040582016d-22	!HCOOCH3_DUST
    get_mass(344) = 7.5287280436512d-23	!NH2CHO_DUST
    get_mass(345) = 7.3612715400136d-23	!CO2_DUST
    get_mass(346) = 7.6959780436512d-23	!CH2O2_DUST
    get_mass(347) = 5.5211085327384d-23	!NH2OH_DUST
    get_mass(348) = 1.0372700806382799d-22	!C4N_DUST
    get_mass(349) = 1.3384130072752d-22	!C4S_DUST
    get_mass(350) = 1.3049836576389599d-22	!C6H6_DUST
    get_mass(351) = 5.0192552809195994d-23	!CH2NH2
    get_mass(352) = 1.07075105618392d-22	!CH3C4H_DUST
    get_mass(353) = 1.47227495836648d-22	!CH3C6H_DUST
    get_mass(354) = 1.10420105618392d-22	!H2S2_DUST
    get_mass(355) = 8.5324345472888d-23	!HC2NC_DUST
    get_mass(356) = 8.5324345472888d-23	!HCNC2_DUST
    get_mass(357) = 6.692065036376d-24	!HE_DUST
    get_mass(358) = 8.5324345472888d-23	!HNC3_DUST
    get_mass(359) = 1.08746573100204d-22	!HS2_DUST
    get_mass(360) = 6.692065036376d-23	!NAOH_DUST
    get_mass(361) = 1.07073040582016d-22	!S2_DUST
    get_mass(362) = 6.692065036376d-23	!SIC_DUST
    get_mass(363) = 7.3612715400136d-23	!SIO_DUST
    get_mass(364) = 1.0038097554564001d-22	!SIS_DUST
    get_mass(365) = 9.703804058201601d-23	!C2H6CO_DUST
    get_mass(366) = 1.12091573100204d-22	!C3P_DUST
    get_mass(367) = 9.2015377991076d-23	!CCP_DUST
    get_mass(368) = 1.32167768209332d-22	!C4P_DUST
    get_mass(369) = 7.863124791832399d-23	!CCL_DUST
    get_mass(370) = 5.8555052809196d-23	!CL_DUST
    get_mass(371) = 5.1862987772820005d-23	!P_DUST
    get_mass(372) = 7.193918288194801d-23	!CP_DUST
    get_mass(373) = 7.6959780436512d-23	!CH2PH_DUST
    get_mass(374) = 7.3612715400136d-23	!HCP_DUST
    get_mass(375) = 8.53233129547d-23	!CLO_DUST
    get_mass(376) = 7.6959780436512d-23	!H2SIO_DUST
    get_mass(377) = 9.368891050926401d-23	!HCCP_DUST
    get_mass(378) = 6.0228585327384d-23	!HCL_DUST
    get_mass(379) = 6.8594182881948d-23	!HCSI_DUST
    get_mass(380) = 7.1940215400136d-23	!HNSI_DUST
    get_mass(381) = 7.0266682881948d-23	!SIN_DUST
    get_mass(382) = 8.030478043651201d-23	!HPO_DUST
    get_mass(383) = 7.8631247918324d-23	!PO_DUST
    get_mass(384) = 7.3612715400136d-23	!N2O_DUST
    get_mass(385) = 7.0267715400136d-23	!NH2CN_DUST
    get_mass(386) = 7.6958747918324d-23	!NO2_DUST
    get_mass(387) = 5.3536520291008d-23	!PH_DUST
    get_mass(388) = 5.5210052809196d-23	!PH2_DUST
    get_mass(389) = 7.528521540013601d-23	!PN_DUST
    get_mass(390) = 8.699684547288799d-23	!SIC2_DUST
    get_mass(391) = 8.867037799107601d-23	!SIC2H_DUST
    get_mass(392) = 9.0343910509264d-23	!SIC2H2_DUST
    get_mass(393) = 1.07073040582016d-22	!SIC3_DUST
    get_mass(394) = 1.08746573100204d-22	!SIC3H_DUST
    get_mass(395) = 1.27149235691144d-22	!SIC4_DUST
    get_mass(396) = 7.0267715400136d-23	!SICH2_DUST
    get_mass(397) = 7.1941247918324d-23	!SICH3_DUST
    get_mass(398) = 9.0342877991076d-23	!SINC_DUST
    get_mass(399) = 1.0038097554564001d-22	!SIO2_DUST
    get_mass(400) = 2.007528417094d-23	!C+
    get_mass(401) = 5.855414187100799d-23	!CL+
    get_mass(402) = 9.201343453470001d-23	!FE+
    get_mass(403) = 1.67262158d-24	!H+
    get_mass(404) = 6.691154098188d-24	!HE+
    get_mass(405) = 4.0151479280067996d-23	!MG+
    get_mass(406) = 2.3421316689128d-23	!N+
    get_mass(407) = 3.847794676188d-23	!NA+
    get_mass(408) = 2.6767349207316d-23	!O+
    get_mass(409) = 5.1862076834632d-23	!P+
    get_mass(410) = 5.353560935282d-23	!S+
    get_mass(411) = 4.6843544316444d-23	!SI+
    get_mass(412) = 4.6843544316444d-23	!CO+
    get_mass(413) = 3.346154098188d-24	!H2+
    get_mass(414) = 5.0189576834632d-23	!NO+
    get_mass(415) = 5.353560935282d-23	!O2+
    get_mass(416) = 2.3422349207316d-23	!CH2+
    get_mass(417) = 5.6882674389196d-23	!H2S+
    get_mass(418) = 4.8517076834631994d-23	!HCO+
    get_mass(419) = 7.5285336980136d-23	!HCS+
    get_mass(420) = 5.186310935282d-23	!HNO+
    get_mass(421) = 2.6768381725504d-23	!NH2+
    get_mass(422) = 1.00380064607452d-22	!OCS+
    get_mass(423) = 4.3498544316443995d-23	!C2H2+
    get_mass(424) = 2.5095881725504002d-23	!CH3+
    get_mass(425) = 2.8441914243691997d-23	!NH3+
    get_mass(426) = 7.0266804461948d-23	!C2H2O+
    get_mass(427) = 7.6958869498324d-23	!CH2O2+
    get_mass(428) = 6.8594304461948d-23	!C2H3N+
    get_mass(429) = 4.6845609352819995d-23	!C2H4+
    get_mass(430) = 8.36509345347d-23	!C4H2+
    get_mass(431) = 5.1864141871008d-23	!H3CO+
    get_mass(432) = 5.3537674389196d-23	!CH4O+
    get_mass(433) = 7.361386949832399d-23	!C2H4O+
    get_mass(434) = 6.692180446194799d-23	!C3H4+
    get_mass(435) = 5.1865174389195995d-23	!CH5N+
    get_mass(436) = 7.69609345347d-23	!C2H5OH+
    get_mass(437) = 7.69609345347d-23	!CH3OCH3+
    get_mass(438) = 2.1748816689128d-23	!CH+
    get_mass(439) = 7.863033698013599d-23	!CCL+
    get_mass(440) = 4.0151479280067996d-23	!C2+
    get_mass(441) = 8.5322402016512d-23	!CLO+
    get_mass(442) = 7.193827194376001d-23	!CP+
    get_mass(443) = 7.3611804461948d-23	!CS+
    get_mass(444) = 4.3497511798255994d-23	!CN+
    get_mass(445) = 7.6957836980136d-23	!NS+
    get_mass(446) = 5.353560935282d-23	!PH+
    get_mass(447) = 7.8630336980136d-23	!PO+
    get_mass(448) = 6.6919739425572d-23	!SIC+
    get_mass(449) = 7.026577194376d-23	!SIN+
    get_mass(450) = 1.00380064607452d-22	!SIS+
    get_mass(451) = 8.0303869498324d-23	!SO+
    get_mass(452) = 6.0227674389196d-23	!C3+
    get_mass(453) = 9.3687999571076d-23	!C2S+
    get_mass(454) = 6.6919739425572d-23	!C2O+
    get_mass(455) = 9.2014467052888d-23	!CCP+
    get_mass(456) = 4.1825011798256d-23	!C2H+
    get_mass(457) = 4.8517076834631994d-23	!HOC+
    get_mass(458) = 6.357370690738399d-23	!C2N+
    get_mass(459) = 6.357370690738399d-23	!CNC+
    get_mass(460) = 7.3611804461948d-23	!HCP+
    get_mass(461) = 8.699593453469999d-23	!SIC2+
    get_mass(462) = 9.0341967052888d-23	!SINC+
    get_mass(463) = 8.030386949832401d-23	!HPO+
    get_mass(464) = 4.5171044316443997d-23	!HCN+
    get_mass(465) = 6.859327194375999d-23	!CHSI+
    get_mass(466) = 5.0190609352819997d-23	!SIH2+
    get_mass(467) = 6.1901206907384d-23	!C3H+
    get_mass(468) = 8.0303869498324d-23	!C4+
    get_mass(469) = 8.699593453469999d-23	!C3O+
    get_mass(470) = 1.13764194680204d-22	!C3S+
    get_mass(471) = 5.0190609352819997d-23	!H2CO+
    get_mass(472) = 7.6958869498324d-23	!H2SIO+
    get_mass(473) = 4.6844576834632d-23	!HCNH+
    get_mass(474) = 8.8669467052888d-23	!SIC2H+
    get_mass(475) = 1.0707212964382801d-22	!SIC3+
    get_mass(476) = 7.0266804461948d-23	!CH2SI+
    get_mass(477) = 5.1864141871008d-23	!SIH3+
    get_mass(478) = 6.692077194376d-23	!C2H2N+
    get_mass(479) = 4.5172076834632d-23	!C2H3+
    get_mass(480) = 6.3574739425572d-23	!C3H2+
    get_mass(481) = 6.3574739425572d-23	!H2C3+
    get_mass(482) = 8.1977402016512d-23	!C4H+
    get_mass(483) = 1.0038006460745199d-22	!C5+
    get_mass(484) = 1.3384038978933202d-22	!C4S+
    get_mass(485) = 9.368799957107601d-23	!PC2H+
    get_mass(486) = 8.364990201651199d-23	!C3N+
    get_mass(487) = 1.0372609712563999d-22	!C4N+
    get_mass(488) = 8.53234345347d-23	!C3HN+
    get_mass(489) = 4.5171044316443997d-23	!HNC+
    get_mass(490) = 1.08745662162016d-22	!SIC3H+
    get_mass(491) = 1.27148324752956d-22	!SIC4+
    get_mass(492) = 9.0342999571076d-23	!SIC2H2+
    get_mass(493) = 7.1940336980136d-23	!SICH3+
    get_mass(494) = 8.699696705288799d-23	!HC2NCH+
    get_mass(495) = 6.524827194375999d-23	!C3H3+
    get_mass(496) = 6.524827194375999d-23	!H3C3+
    get_mass(497) = 1.0205359712564d-22	!C5H+
    get_mass(498) = 1.2045625971658d-22	!C6+
    get_mass(499) = 7.1940336980136d-23	!C2H3O+
    get_mass(500) = 4.8519141871007997d-23	!C2H5+
    get_mass(501) = 8.867049957107599d-23	!C3H3N+
    get_mass(502) = 1.03727129643828d-22	!C5H2+
    get_mass(503) = 8.532446705288799d-23	!C4H3+
    get_mass(504) = 1.22129792234768d-22	!C6H+
    get_mass(505) = 1.4053245482570802d-22	!C7+
    get_mass(506) = 5.019164187100799d-23	!CH4N+
    get_mass(507) = 1.2547582475295602d-22	!C5HN+
    get_mass(508) = 1.42205987343896d-22	!C7H+
    get_mass(509) = 1.60608649934836d-22	!C8+
    get_mass(510) = 1.00382129643828d-22	!COOCH4+
    get_mass(511) = 7.5287402016512d-23	!C2H5O+
    get_mass(512) = 1.62282182453024d-22	!C8H+
    get_mass(513) = 1.80684845043964d-22	!C9+
    get_mass(514) = 1.0540066216201599d-22	!C5H3+
    get_mass(515) = 1.23803324752956d-22	!C6H2+
    get_mass(516) = 1.25476857271144d-22	!C6H3+
    get_mass(517) = 9.7037129643828d-23	!C2H6CO+
    get_mass(518) = 1.8235837756215198d-22	!C9H+
    get_mass(519) = 2.00761040153092d-22	!C10+
    get_mass(520) = 1.45553052380272d-22	!C7H3+
    get_mass(521) = 1.63955714971212d-22	!C8H2+
    get_mass(522) = 1.656292474894d-22	!C8H3+
    get_mass(523) = 6.0227674389196d-23	!HCL+
    get_mass(524) = 5.5209141871008d-23	!HS+
    get_mass(525) = 2.5094849207316d-23	!NH+
    get_mass(526) = 2.8440881725504d-23	!OH+
    get_mass(527) = 7.5284304461948d-23	!PN+
    get_mass(528) = 1.0707212964382801d-22	!S2+
    get_mass(529) = 4.8517076834632d-23	!SIH+
    get_mass(530) = 7.3611804461948d-23	!SIO+
    get_mass(531) = 3.0114414243692d-23	!H2O+
    get_mass(532) = 7.1939304461948d-23	!HNSI+
    get_mass(533) = 1.08745662162016d-22	!S2H+
    get_mass(534) = 5.5209141871008d-23	!PH2+
    get_mass(535) = 7.6958869498324d-23	!H2CS+
    get_mass(536) = 1.1041919468020402d-22	!H2S2+
    get_mass(537) = 7.5285336980136d-23	!HSIO+
    get_mass(538) = 1.32166857271144d-22	!C4P+
    get_mass(539) = 7.5285336980136d-23	!HCO2+
    get_mass(540) = 7.6958869498324d-23	!PCH3+
    get_mass(541) = 2.6769414243691995d-23	!CH4+
    get_mass(542) = 6.5247239425572d-23	!C2NH+
    get_mass(543) = 5.3537674389196d-23	!SIH4+
    get_mass(544) = 3.011544676188d-23	!NH4+
    get_mass(545) = 4.6844576834632d-23	!H2NC+
    get_mass(546) = 8.6996967052888d-23	!C3H2N+
    get_mass(547) = 1.4387951986208402d-22	!C7H2+
    get_mass(548) = 1.07074194680204d-22	!C5H4+
    get_mass(549) = 1.6562821497121201d-22	!C7HN+
    get_mass(550) = 1.8403191008034d-22	!C9H2+
    get_mass(551) = 1.4722658489846003d-22	!C7H4+
    get_mass(552) = 2.05780605189468d-22	!C9HN+
    get_mass(553) = 4.6843544316444d-23	!N2+
    get_mass(554) = 7.3611804461948d-23	!CO2+
    get_mass(555) = 8.364686616375999d-24	!HEH+
    get_mass(556) = 1.0707212964382801d-22	!SO2+
    get_mass(557) = 1.2882392230752d-22	!C6H5+
    get_mass(558) = 1.08747727198392d-22	!C5H5+
    get_mass(559) = 4.8517076834632d-23	!N2H+
    get_mass(560) = 7.6957836980136d-23	!NO2+
    get_mass(561) = 9.5361532089264d-23	!PC2H2+
    get_mass(562) = 7.863136949832401d-23	!PNH2+
    get_mass(563) = 7.5285336980136d-23	!PCH2+
    get_mass(564) = 9.5361532089264d-23	!HC2S+
    get_mass(565) = 1.15437727198392d-22	!HC3S+
    get_mass(566) = 7.8632402016512d-23	!H3CS+
    get_mass(567) = 1.3551392230752d-22	!HC4S+
    get_mass(568) = 7.361283698013599d-23	!SINH2+
    get_mass(569) = 9.2016532089264d-23	!SIC2H3+
    get_mass(570) = 1.1041919468020402d-22	!SIC3H2+
    get_mass(571) = 6.859327194375999d-23	!C2HO+
    get_mass(572) = 3.178794676188d-23	!H3O+
    get_mass(573) = 5.8556206907384d-23	!H3S+
    get_mass(574) = 1.0205359712564d-22	!HOCS+
    get_mass(575) = 5.521120690738399d-23	!CH5O+
    get_mass(576) = 7.026577194376d-23	!NCO+
    get_mass(577) = 7.193930446194799d-23	!HNCO+
    get_mass(578) = 8.699593453469999d-23	!C2N2+
    get_mass(579) = 5.0196866163760004d-24	!H3+
    get_mass(580) = 5.5209141871008d-23	!O2H+
    get_mass(581) = 2.844294676188d-23	!CH5+
    get_mass(582) = 6.1901206907384d-23	!H2CL+
    get_mass(583) = 7.8632402016512d-23	!CH3O2+
    get_mass(584) = 8.197740201651201d-23	!H2PO+
    get_mass(585) = 8.0304902016512d-23	!PNH3+
    get_mass(586) = 7.8632402016512d-23	!PCH4+
    get_mass(587) = 9.7035064607452d-23	!PC2H3+
    get_mass(588) = 1.0205359712564d-22	!HSIS+
    get_mass(589) = 8.197740201651201d-23	!HSO+
    get_mass(590) = 7.8631369498324d-23	!HNS+
    get_mass(591) = 7.6957836980136d-23	!HPN+
    get_mass(592) = 5.3536641871008d-23	!H2NO+
    get_mass(593) = 6.859327194375999d-23	!NAH2O+
    get_mass(594) = 5.6882674389196d-23	!PH3+
    get_mass(595) = 9.201549957107599d-23	!SINCH+
    get_mass(596) = 1.0205359712564d-22	!HSIO2+
    get_mass(597) = 1.08745662162016d-22	!HSO2+
    get_mass(598) = 8.8669467052888d-23	!HC3O+
    get_mass(599) = 1.13764194680204d-22	!PC3H+
    get_mass(600) = 1.12092727198392d-22	!H3S2+
    get_mass(601) = 7.8632402016512d-23	!H3SIO+
    get_mass(602) = 1.3384038978933202d-22	!PC4H+
    get_mass(603) = 7.1940336980136d-23	!NH2CNH+
    get_mass(604) = 1.2882185727114401d-22	!SIC4H+
    get_mass(605) = 7.3613869498324d-23	!SICH4+
    get_mass(606) = 5.521120690738399d-23	!SIH5+
    get_mass(607) = 7.026783698013599d-23	!C2H4N+
    get_mass(608) = 7.695990201651199d-23	!NH2CH2O+
    get_mass(609) = 5.0192674389196d-23	!C2H6+
    get_mass(610) = 9.034403208926399d-23	!C3H4N+
    get_mass(611) = 6.8595336980136d-23	!C3H5+
    get_mass(612) = 8.699799957107599d-23	!C4H4+
    get_mass(613) = 5.3538706907384d-23	!CH6N+
    get_mass(614) = 1.27149357271144d-22	!C5H2N+
    get_mass(615) = 1.10420227198392d-22	!C4H4N+
    get_mass(616) = 1.02055662162016d-22	!H5C2O2+
    get_mass(617) = 7.8634467052888d-23	!C2H5OH2+
    get_mass(618) = 7.8634467052888d-23	!CH3OCH4+
    get_mass(619) = 1.6730174748940003d-22	!C7H2N+
    get_mass(620) = 9.8710662162016d-23	!C3H6OH+
    get_mass(621) = 1.5057261741664802d-22	!C6H4N+
    get_mass(622) = 2.0243457267128d-22	!C10H+
    get_mass(623) = 1.8570544259852799d-22	!C9H3+
    get_mass(624) = 1.4890011741664802d-22	!C7H5+
    get_mass(625) = 1.9072500763490402d-22	!C8H4N+
    get_mass(626) = 2.07454137707656d-22	!C9H2N+
    get_mass(627) = 1.32170987343896d-22	!C6H7+
    get_mass(628) = 4.1825011798256d-23	!NAH2+
    get_mass(629) = 9.870859712564d-23	!PC2H4+
    get_mass(630) = 8.8671532089264d-23	!C4H5+
    get_mass(631) = 8.1977402016512d-23	!H2CCL+
    get_mass(632) = 1.3551392230752d-22	!PC4H2+
    get_mass(633) = 1.2715038978933201d-22	!C6H4+
    get_mass(634) = 1.6730278000758801d-22	!C8H4+
    get_mass(635) = 1.87378975116716d-22	!C9H4+
    get_mass(636) = 9.201859712564d-23	!C4H7+
    get_mass(637) = 1.0539962964382799d-22	!HC4N+
    get_mass(638) = 1.08745662162016d-22	!HC4O+
    get_mass(639) = 1.23802292234768d-22	!C5N+
    get_mass(640) = 1.0707316216201602d-22	!H2C4N+
    get_mass(641) = 1.08746694680204d-22	!H3C4N+
    get_mass(642) = 1.6395468245302402d-22	!C7N+
    get_mass(643) = 1.28822889789332d-22	!C5H3N+
    get_mass(644) = 2.04108105189468d-22	!C10H2+
    get_mass(645) = 2.0410707267128d-22	!C9N+
    get_mass(646) = 1.6897528000758802d-22	!C7H3N+
    get_mass(647) = 2.09127670225844d-22	!C9H3N+
    get_mass(648) = 1.03727129643828d-22	!OCS+H2
    get_mass(649) = 9.0342999571076d-23	!H2C3O+
    get_mass(650) = 9.201653208926399d-23	!H3C3O+
    get_mass(651) = 1.3049642230752001d-22	!C5H4N+
    get_mass(652) = 1.68976312525776d-22	!C8H5+
    get_mass(653) = 1.89052507634904d-22	!C9H5+
    get_mass(654) = 1.0205566216201601d-22	!H2COHOCH2+
    get_mass(655) = 1.0540272719839199d-22	!H7C2O2+
    get_mass(656) = 0d0	!CR
    get_mass(657) = 0d0	!g
    get_mass(658) = 0d0	!Tgas
    get_mass(659) = 0d0	!dummy

  end function get_mass

  !************************
  !get sqrt of the inverse of the masses (1/sqrt(g))
  function get_imass_sqrt()
    use krome_commons
    implicit none
    real*8::get_imass_sqrt(nspec)

    get_imass_sqrt(1) = 33132602150543.92	!E
    get_imass_sqrt(2) = 772795806394.0071	!H-
    get_imass_sqrt(3) = 223177004181.41986	!C-
    get_imass_sqrt(4) = 151620769970.40408	!CN-
    get_imass_sqrt(5) = 193278051340.87015	!O-
    get_imass_sqrt(6) = 187505737903.66562	!OH-
    get_imass_sqrt(7) = 136669383456.05603	!S-
    get_imass_sqrt(8) = 22318201671.239548	!GRAIN-
    get_imass_sqrt(9) = 223182067345.7445	!C
    get_imass_sqrt(10) = 130682613569.38446	!CL
    get_imass_sqrt(11) = 104249079615.21292	!FE
    get_imass_sqrt(12) = 773006102110.9268	!H
    get_imass_sqrt(13) = 386562679981.0883	!HE
    get_imass_sqrt(14) = 157813553259.4087	!MG
    get_imass_sqrt(15) = 206626443857.4815	!N
    get_imass_sqrt(16) = 161208862859.36072	!NA
    get_imass_sqrt(17) = 193281339990.54416	!O
    get_imass_sqrt(18) = 138858104892.15924	!P
    get_imass_sqrt(19) = 136670546184.13641	!S
    get_imass_sqrt(20) = 146106959624.0866	!SI
    get_imass_sqrt(21) = 157813553259.4087	!C2
    get_imass_sqrt(22) = 112772294263.15701	!CCL
    get_imass_sqrt(23) = 214423849574.04788	!CH
    get_imass_sqrt(24) = 108259531918.45573	!CLO
    get_imass_sqrt(25) = 151622357573.09943	!CN
    get_imass_sqrt(26) = 146106959624.0866	!CO
    get_imass_sqrt(27) = 117900935127.6621	!CP
    get_imass_sqrt(28) = 116553033404.68169	!CS
    get_imass_sqrt(29) = 546597856701.2171	!H2
    get_imass_sqrt(30) = 128854226660.3628	!HCL
    get_imass_sqrt(31) = 134583221186.17943	!HS
    get_imass_sqrt(32) = 154624117717.66168	!MGH
    get_imass_sqrt(33) = 146106959624.0866	!N2
    get_imass_sqrt(34) = 157813553259.4087	!NAH
    get_imass_sqrt(35) = 199618056317.88782	!NH
    get_imass_sqrt(36) = 141152733143.5285	!NO
    get_imass_sqrt(37) = 113991115425.78232	!NS
    get_imass_sqrt(38) = 136670546184.13641	!O2
    get_imass_sqrt(39) = 187508740611.18686	!OH
    get_imass_sqrt(40) = 136670546184.13641	!PH
    get_imass_sqrt(41) = 115251119158.44125	!PN
    get_imass_sqrt(42) = 112772294263.15701	!PO
    get_imass_sqrt(43) = 96640669995.27208	!S2
    get_imass_sqrt(44) = 122241852715.9014	!SIC
    get_imass_sqrt(45) = 143565011358.4871	!SIH
    get_imass_sqrt(46) = 119295832982.81203	!SIN
    get_imass_sqrt(47) = 116553033404.68169	!SIO
    get_imass_sqrt(48) = 99810054788.80415	!SIS
    get_imass_sqrt(49) = 111591033672.87225	!SO
    get_imass_sqrt(50) = 154624117717.66168	!C2H
    get_imass_sqrt(51) = 125417494605.29665	!C2N
    get_imass_sqrt(52) = 103313221928.74075	!C2S
    get_imass_sqrt(53) = 128854226660.3628	!C3
    get_imass_sqrt(54) = 122241852715.9014	!CCO
    get_imass_sqrt(55) = 104248494716.38899	!CCP
    get_imass_sqrt(56) = 206621889668.37103	!CH2
    get_imass_sqrt(57) = 116553033404.68169	!CO2
    get_imass_sqrt(58) = 182224271009.1322	!H2O
    get_imass_sqrt(59) = 132588702017.9196	!H2S
    get_imass_sqrt(60) = 148787194663.8679	!HCN
    get_imass_sqrt(61) = 143565011358.4871	!HCO
    get_imass_sqrt(62) = 116553033404.68169	!HCP
    get_imass_sqrt(63) = 115250328846.21283	!HCS
    get_imass_sqrt(64) = 120741441507.20366	!HCSI
    get_imass_sqrt(65) = 148787194663.8679	!HNC
    get_imass_sqrt(66) = 138856722679.2915	!HNO
    get_imass_sqrt(67) = 117900089041.11632	!HNSI
    get_imass_sqrt(68) = 111591033672.87225	!HPO
    get_imass_sqrt(69) = 95894171204.22452	!HS2
    get_imass_sqrt(70) = 116553033404.68169	!N2O
    get_imass_sqrt(71) = 122241852715.9014	!NAOH
    get_imass_sqrt(72) = 193277612428.0371	!NH2
    get_imass_sqrt(73) = 113991115425.78232	!NO2
    get_imass_sqrt(74) = 134583221186.17943	!O2H
    get_imass_sqrt(75) = 119295832982.81203	!OCN
    get_imass_sqrt(76) = 99810054788.80415	!OCS
    get_imass_sqrt(77) = 134583221186.17943	!PH2
    get_imass_sqrt(78) = 107213197219.43008	!SIC2
    get_imass_sqrt(79) = 141151281269.65662	!SIH2
    get_imass_sqrt(80) = 105209035506.27245	!SINC
    get_imass_sqrt(81) = 99810054788.80415	!SIO2
    get_imass_sqrt(82) = 96640669995.27208	!SO2
    get_imass_sqrt(83) = 151620558081.93347	!C2H2
    get_imass_sqrt(84) = 127100496230.1777	!C3H
    get_imass_sqrt(85) = 109336436947.30914	!C3N
    get_imass_sqrt(86) = 107213197219.43008	!C3O
    get_imass_sqrt(87) = 94452513153.1178	!C3P
    get_imass_sqrt(88) = 93755221204.93323	!C3S
    get_imass_sqrt(89) = 111591033672.87225	!C4
    get_imass_sqrt(90) = 199613949988.51508	!CH3
    get_imass_sqrt(91) = 141151281269.65662	!H2CO
    get_imass_sqrt(92) = 113990350751.72755	!H2CS
    get_imass_sqrt(93) = 132588702017.9196	!H2O2
    get_imass_sqrt(94) = 95164708334.6765	!H2S2
    get_imass_sqrt(95) = 113990350751.72755	!H2SIO
    get_imass_sqrt(96) = 103313221928.74075	!HCCP
    get_imass_sqrt(97) = 187505337152.829	!NH3
    get_imass_sqrt(98) = 106196626295.6403	!SIC2H
    get_imass_sqrt(99) = 96640669995.27208	!SIC3
    get_imass_sqrt(100) = 119294956509.39111	!SICH2
    get_imass_sqrt(101) = 138855340507.6992	!SIH3
    get_imass_sqrt(102) = 122240909692.52188	!C2H2N
    get_imass_sqrt(103) = 119294956509.39111	!C2H2O
    get_imass_sqrt(104) = 148785494241.0038	!C2H3
    get_imass_sqrt(105) = 125416476162.15594	!C3H2
    get_imass_sqrt(106) = 110446132225.24516	!C4H
    get_imass_sqrt(107) = 98187018902.26443	!C4N
    get_imass_sqrt(108) = 86983568721.54436	!C4P
    get_imass_sqrt(109) = 86438043000.22107	!C4S
    get_imass_sqrt(110) = 99810054788.80417	!C5
    get_imass_sqrt(111) = 113990350751.72755	!CH2O2
    get_imass_sqrt(112) = 113990350751.72755	!CH2PH
    get_imass_sqrt(113) = 143563483769.1712	!CH3N
    get_imass_sqrt(114) = 193273885081.1875	!CH4
    get_imass_sqrt(115) = 108258876886.92119	!HC3N
    get_imass_sqrt(116) = 105208434300.54387	!SIC2H2
    get_imass_sqrt(117) = 95894171204.22452	!SIC3H
    get_imass_sqrt(118) = 88683560914.8325	!SIC4
    get_imass_sqrt(119) = 117899242972.78545	!SICH3
    get_imass_sqrt(120) = 136667910399.41039	!SIH4
    get_imass_sqrt(121) = 120740532783.44785	!C2H3N
    get_imass_sqrt(122) = 146103739326.0838	!C2H4
    get_imass_sqrt(123) = 123797667272.25238	!C3H3
    get_imass_sqrt(124) = 109335762173.14557	!C4H2
    get_imass_sqrt(125) = 98988308511.48834	!C5H
    get_imass_sqrt(126) = 89873997548.56836	!C5N
    get_imass_sqrt(127) = 91113697456.09094	!C6
    get_imass_sqrt(128) = 136667910399.41039	!CH4O
    get_imass_sqrt(129) = 116551398624.68074	!C2H4O
    get_imass_sqrt(130) = 143561956228.61682	!C2H5
    get_imass_sqrt(131) = 106196008000.14502	!C3H3N
    get_imass_sqrt(132) = 122239966690.96661	!C3H4
    get_imass_sqrt(133) = 98186530219.86688	!C5H2
    get_imass_sqrt(134) = 90487287806.23341	!C6H
    get_imass_sqrt(135) = 84354892469.44418	!C7
    get_imass_sqrt(136) = 138853958377.3802	!CH5N
    get_imass_sqrt(137) = 89272643286.89857	!HC5N
    get_imass_sqrt(138) = 89873622776.53387	!C6H2
    get_imass_sqrt(139) = 83857066928.74515	!C7H
    get_imass_sqrt(140) = 78097454962.36366	!C7N
    get_imass_sqrt(141) = 78906776629.70435	!C8
    get_imass_sqrt(142) = 95893715963.34656	!CH3C3N
    get_imass_sqrt(143) = 99809028158.94391	!HCOOCH3
    get_imass_sqrt(144) = 113988821449.78314	!C2H5OH
    get_imass_sqrt(145) = 83367952463.4711	!C7H2
    get_imass_sqrt(146) = 78498862078.93077	!C8H
    get_imass_sqrt(147) = 74394022448.58151	!C9
    get_imass_sqrt(148) = 96639738091.16608	!CH3C4H
    get_imass_sqrt(149) = 113988821449.78314	!CH3OCH3
    get_imass_sqrt(150) = 77701901172.70824	!HC7N
    get_imass_sqrt(151) = 101514712903.94582	!C2H6CO
    get_imass_sqrt(152) = 78097209052.29694	!C8H2
    get_imass_sqrt(153) = 74051874333.64355	!C9H
    get_imass_sqrt(154) = 69995481248.56636	!C9N
    get_imass_sqrt(155) = 70576366571.76427	!C10
    get_imass_sqrt(156) = 81950698047.14629	!CH3C5N
    get_imass_sqrt(157) = 73714403963.4852	!C9H2
    get_imass_sqrt(158) = 82414862300.22026	!CH3C6H
    get_imass_sqrt(159) = 72729218087.02512	!CH3C7N
    get_imass_sqrt(160) = 69710278643.12831	!HC9N
    get_imass_sqrt(161) = 107211924787.02394	!C4H4
    get_imass_sqrt(162) = 108258876886.92119	!HCNC2
    get_imass_sqrt(163) = 108258876886.92119	!HC2NC
    get_imass_sqrt(164) = 108258876886.92119	!HNC3
    get_imass_sqrt(165) = 115249538550.24242	!NH2CHO
    get_imass_sqrt(166) = 108258221867.27646	!C4H3
    get_imass_sqrt(167) = 119294956509.39111	!NH2CN
    get_imass_sqrt(168) = 87538170023.28557	!C6H6
    get_imass_sqrt(169) = 146105349448.46832	!H2CN
    get_imass_sqrt(170) = 22318206734.57445	!GRAIN0
    get_imass_sqrt(171) = 111591033672.87225	!O3
    get_imass_sqrt(172) = 103313791225.90643	!FEH
    get_imass_sqrt(173) = 117900089041.11632	!HNCO
    get_imass_sqrt(174) = 120741441507.20366	!HC2O
    get_imass_sqrt(175) = 123798646785.36269	!HCCN
    get_imass_sqrt(176) = 106196626295.6403	!HC3O
    get_imass_sqrt(177) = 151620558081.93347	!MGH2
    get_imass_sqrt(178) = 141151281269.65662	!N2H2
    get_imass_sqrt(179) = 146105349448.46832	!CHNH
    get_imass_sqrt(180) = 105208434300.54387	!H2C3O
    get_imass_sqrt(181) = 107212560997.56389	!H2C3N
    get_imass_sqrt(182) = 88683200838.61401	!H2C5N
    get_imass_sqrt(183) = 77312297426.42	!H2C7N
    get_imass_sqrt(184) = 69428534114.3194	!H2C9N
    get_imass_sqrt(185) = 134581962740.77362	!NH2OH
    get_imass_sqrt(186) = 138855340507.6992	!CH2OH
    get_imass_sqrt(187) = 97403924111.83961	!C5H3
    get_imass_sqrt(188) = 88105282071.54898	!H3C5N
    get_imass_sqrt(189) = 89272275987.50037	!C6H3
    get_imass_sqrt(190) = 82887297954.0342	!C7H3
    get_imass_sqrt(191) = 76928496033.27704	!H3C7N
    get_imass_sqrt(192) = 77701658980.24763	!C8H3
    get_imass_sqrt(193) = 73381505712.62811	!C9H3
    get_imass_sqrt(194) = 69150178340.43712	!H3C9N
    get_imass_sqrt(195) = 141149829440.58502	!CH3NH
    get_imass_sqrt(196) = 105207833105.12177	!H4C3N
    get_imass_sqrt(197) = 96639738091.16608	!C5H4
    get_imass_sqrt(198) = 88682840766.78146	!C6H4
    get_imass_sqrt(199) = 82414862300.22026	!C7H4
    get_imass_sqrt(200) = 77312058858.83084	!C8H4
    get_imass_sqrt(201) = 73053077264.81154	!C9H4
    get_imass_sqrt(202) = 104246740078.98465	!H5C3N
    get_imass_sqrt(203) = 141148377656.31137	!C2H6
    get_imass_sqrt(204) = 223182067345.7445	!C_DUST
    get_imass_sqrt(205) = 157813553259.4087	!C2_DUST
    get_imass_sqrt(206) = 128854226660.3628	!C3_DUST
    get_imass_sqrt(207) = 154624117717.66168	!C2H_DUST
    get_imass_sqrt(208) = 127100496230.1777	!C3H_DUST
    get_imass_sqrt(209) = 148785494241.0038	!C2H3_DUST
    get_imass_sqrt(210) = 123797667272.25238	!C3H3_DUST
    get_imass_sqrt(211) = 125417494605.29665	!C2N_DUST
    get_imass_sqrt(212) = 109336436947.30914	!C3N_DUST
    get_imass_sqrt(213) = 122241852715.9014	!CCO_DUST
    get_imass_sqrt(214) = 107213197219.43008	!C3O_DUST
    get_imass_sqrt(215) = 103313221928.74075	!C2S_DUST
    get_imass_sqrt(216) = 93755221204.93323	!C3S_DUST
    get_imass_sqrt(217) = 111591033672.87225	!C4_DUST
    get_imass_sqrt(218) = 110446132225.24516	!C4H_DUST
    get_imass_sqrt(219) = 99810054788.80417	!C5_DUST
    get_imass_sqrt(220) = 98988308511.48834	!C5H_DUST
    get_imass_sqrt(221) = 91113697456.09094	!C6_DUST
    get_imass_sqrt(222) = 90487287806.23341	!C6H_DUST
    get_imass_sqrt(223) = 84354892469.44418	!C7_DUST
    get_imass_sqrt(224) = 83857066928.74515	!C7H_DUST
    get_imass_sqrt(225) = 78906776629.70435	!C8_DUST
    get_imass_sqrt(226) = 78498862078.93077	!C8H_DUST
    get_imass_sqrt(227) = 74394022448.58151	!C9_DUST
    get_imass_sqrt(228) = 74051874333.64355	!C9H_DUST
    get_imass_sqrt(229) = 70576366571.76427	!C10_DUST
    get_imass_sqrt(230) = 214423849574.04788	!CH_DUST
    get_imass_sqrt(231) = 206621889668.37103	!CH2_DUST
    get_imass_sqrt(232) = 151620558081.93347	!C2H2_DUST
    get_imass_sqrt(233) = 199613949988.51508	!CH3_DUST
    get_imass_sqrt(234) = 151622357573.09943	!CN_DUST
    get_imass_sqrt(235) = 134583221186.17943	!HS_DUST
    get_imass_sqrt(236) = 116553033404.68169	!CS_DUST
    get_imass_sqrt(237) = 773006102110.9268	!H_DUST
    get_imass_sqrt(238) = 206626443857.4815	!N_DUST
    get_imass_sqrt(239) = 199618056317.88782	!NH_DUST
    get_imass_sqrt(240) = 148787194663.8679	!HNC_DUST
    get_imass_sqrt(241) = 193277612428.0371	!NH2_DUST
    get_imass_sqrt(242) = 141152733143.5285	!NO_DUST
    get_imass_sqrt(243) = 193281339990.54416	!O_DUST
    get_imass_sqrt(244) = 119295832982.81203	!OCN_DUST
    get_imass_sqrt(245) = 113991115425.78232	!NS_DUST
    get_imass_sqrt(246) = 136670546184.13641	!S_DUST
    get_imass_sqrt(247) = 146106959624.0866	!CO_DUST
    get_imass_sqrt(248) = 136670546184.13641	!O2_DUST
    get_imass_sqrt(249) = 187508740611.18686	!OH_DUST
    get_imass_sqrt(250) = 111591033672.87225	!SO_DUST
    get_imass_sqrt(251) = 125416476162.15594	!C3H2_DUST
    get_imass_sqrt(252) = 122239966690.96661	!C3H4_DUST
    get_imass_sqrt(253) = 109335762173.14557	!C4H2_DUST
    get_imass_sqrt(254) = 98186530219.86688	!C5H2_DUST
    get_imass_sqrt(255) = 89873622776.53387	!C6H2_DUST
    get_imass_sqrt(256) = 83367952463.4711	!C7H2_DUST
    get_imass_sqrt(257) = 78097209052.29694	!C8H2_DUST
    get_imass_sqrt(258) = 73714403963.4852	!C9H2_DUST
    get_imass_sqrt(259) = 146103739326.0838	!C2H4_DUST
    get_imass_sqrt(260) = 123798646785.36269	!HCCN_DUST
    get_imass_sqrt(261) = 138856722679.2915	!HNO_DUST
    get_imass_sqrt(262) = 148787194663.8679	!HCN_DUST
    get_imass_sqrt(263) = 146105349448.46832	!CHNH_DUST
    get_imass_sqrt(264) = 143563483769.1712	!CH3N_DUST
    get_imass_sqrt(265) = 143565011358.4871	!HCO_DUST
    get_imass_sqrt(266) = 143561956228.61682	!C2H5_DUST
    get_imass_sqrt(267) = 122240909692.52188	!C2H2N_DUST
    get_imass_sqrt(268) = 141149829440.58502	!CH2NH2_DUST
    get_imass_sqrt(269) = 141151281269.65662	!H2CO_DUST
    get_imass_sqrt(270) = 95893715963.34656	!CH3C3N_DUST
    get_imass_sqrt(271) = 89873997548.56836	!C5N_DUST
    get_imass_sqrt(272) = 81950698047.14629	!CH3C5N_DUST
    get_imass_sqrt(273) = 78097454962.36366	!C7N_DUST
    get_imass_sqrt(274) = 72729218087.02512	!CH3C7N_DUST
    get_imass_sqrt(275) = 138855340507.6992	!CH2OH_DUST
    get_imass_sqrt(276) = 113988821449.78314	!C2H5OH_DUST
    get_imass_sqrt(277) = 113988821449.78314	!CH3OCH3_DUST
    get_imass_sqrt(278) = 141148377656.31137	!C2H6_DUST
    get_imass_sqrt(279) = 120740532783.44785	!C2H3N_DUST
    get_imass_sqrt(280) = 116551398624.68074	!C2H4O_DUST
    get_imass_sqrt(281) = 193273885081.1875	!CH4_DUST
    get_imass_sqrt(282) = 546597856701.2171	!H2_DUST
    get_imass_sqrt(283) = 120741441507.20366	!HC2O_DUST
    get_imass_sqrt(284) = 106196008000.14502	!C3H3N_DUST
    get_imass_sqrt(285) = 105207833105.12177	!H4C3N_DUST
    get_imass_sqrt(286) = 108258876886.92119	!HC3N_DUST
    get_imass_sqrt(287) = 106196626295.6403	!HC3O_DUST
    get_imass_sqrt(288) = 108258221867.27646	!C4H3_DUST
    get_imass_sqrt(289) = 107211924787.02394	!C4H4_DUST
    get_imass_sqrt(290) = 97403924111.83961	!C5H3_DUST
    get_imass_sqrt(291) = 96639738091.16608	!C5H4_DUST
    get_imass_sqrt(292) = 89272643286.89857	!HC5N_DUST
    get_imass_sqrt(293) = 89272275987.50037	!C6H3_DUST
    get_imass_sqrt(294) = 88682840766.78146	!C6H4_DUST
    get_imass_sqrt(295) = 82887297954.0342	!C7H3_DUST
    get_imass_sqrt(296) = 82414862300.22026	!C7H4_DUST
    get_imass_sqrt(297) = 77701901172.70824	!HC7N_DUST
    get_imass_sqrt(298) = 77701658980.24763	!C8H3_DUST
    get_imass_sqrt(299) = 77312058858.83084	!C8H4_DUST
    get_imass_sqrt(300) = 73381505712.62811	!C9H3_DUST
    get_imass_sqrt(301) = 73053077264.81154	!C9H4_DUST
    get_imass_sqrt(302) = 69995481248.56636	!C9N_DUST
    get_imass_sqrt(303) = 69710278643.12831	!HC9N_DUST
    get_imass_sqrt(304) = 141149829440.58502	!CH3NH_DUST
    get_imass_sqrt(305) = 138853958377.3802	!CH5N_DUST
    get_imass_sqrt(306) = 136667910399.41039	!CH4O_DUST
    get_imass_sqrt(307) = 115250328846.21283	!HCS_DUST
    get_imass_sqrt(308) = 104249079615.21292	!FE_DUST
    get_imass_sqrt(309) = 103313791225.90643	!FEH_DUST
    get_imass_sqrt(310) = 107212560997.56389	!H2C3N_DUST
    get_imass_sqrt(311) = 88683200838.61401	!H2C5N_DUST
    get_imass_sqrt(312) = 88105282071.54898	!H3C5N_DUST
    get_imass_sqrt(313) = 77312297426.42	!H2C7N_DUST
    get_imass_sqrt(314) = 76928496033.27704	!H3C7N_DUST
    get_imass_sqrt(315) = 69428534114.3194	!H2C9N_DUST
    get_imass_sqrt(316) = 69150178340.43712	!H3C9N_DUST
    get_imass_sqrt(317) = 146105349448.46832	!H2CN_DUST
    get_imass_sqrt(318) = 132588702017.9196	!H2O2_DUST
    get_imass_sqrt(319) = 182224271009.1322	!H2O_DUST
    get_imass_sqrt(320) = 134583221186.17943	!O2H_DUST
    get_imass_sqrt(321) = 132588702017.9196	!H2S_DUST
    get_imass_sqrt(322) = 104246740078.98465	!H5C3N_DUST
    get_imass_sqrt(323) = 119294956509.39111	!C2H2O_DUST
    get_imass_sqrt(324) = 105208434300.54387	!H2C3O_DUST
    get_imass_sqrt(325) = 113990350751.72755	!H2CS_DUST
    get_imass_sqrt(326) = 157813553259.4087	!MG_DUST
    get_imass_sqrt(327) = 154624117717.66168	!MGH_DUST
    get_imass_sqrt(328) = 151620558081.93347	!MGH2_DUST
    get_imass_sqrt(329) = 141151281269.65662	!N2H2_DUST
    get_imass_sqrt(330) = 146106959624.0866	!N2_DUST
    get_imass_sqrt(331) = 161208862859.36072	!NA_DUST
    get_imass_sqrt(332) = 157813553259.4087	!NAH_DUST
    get_imass_sqrt(333) = 187505337152.829	!NH3_DUST
    get_imass_sqrt(334) = 111591033672.87225	!O3_DUST
    get_imass_sqrt(335) = 117900089041.11632	!HNCO_DUST
    get_imass_sqrt(336) = 99810054788.80415	!OCS_DUST
    get_imass_sqrt(337) = 146106959624.0866	!SI_DUST
    get_imass_sqrt(338) = 143565011358.4871	!SIH_DUST
    get_imass_sqrt(339) = 141151281269.65662	!SIH2_DUST
    get_imass_sqrt(340) = 138855340507.6992	!SIH3_DUST
    get_imass_sqrt(341) = 136667910399.41039	!SIH4_DUST
    get_imass_sqrt(342) = 96640669995.27208	!SO2_DUST
    get_imass_sqrt(343) = 99809028158.94391	!HCOOCH3_DUST
    get_imass_sqrt(344) = 115249538550.24242	!NH2CHO_DUST
    get_imass_sqrt(345) = 116553033404.68169	!CO2_DUST
    get_imass_sqrt(346) = 113990350751.72755	!CH2O2_DUST
    get_imass_sqrt(347) = 134581962740.77362	!NH2OH_DUST
    get_imass_sqrt(348) = 98187018902.26443	!C4N_DUST
    get_imass_sqrt(349) = 86438043000.22107	!C4S_DUST
    get_imass_sqrt(350) = 87538170023.28557	!C6H6_DUST
    get_imass_sqrt(351) = 141149829440.58502	!CH2NH2
    get_imass_sqrt(352) = 96639738091.16608	!CH3C4H_DUST
    get_imass_sqrt(353) = 82414862300.22026	!CH3C6H_DUST
    get_imass_sqrt(354) = 95164708334.6765	!H2S2_DUST
    get_imass_sqrt(355) = 108258876886.92119	!HC2NC_DUST
    get_imass_sqrt(356) = 108258876886.92119	!HCNC2_DUST
    get_imass_sqrt(357) = 386562679981.0883	!HE_DUST
    get_imass_sqrt(358) = 108258876886.92119	!HNC3_DUST
    get_imass_sqrt(359) = 95894171204.22452	!HS2_DUST
    get_imass_sqrt(360) = 122241852715.9014	!NAOH_DUST
    get_imass_sqrt(361) = 96640669995.27208	!S2_DUST
    get_imass_sqrt(362) = 122241852715.9014	!SIC_DUST
    get_imass_sqrt(363) = 116553033404.68169	!SIO_DUST
    get_imass_sqrt(364) = 99810054788.80415	!SIS_DUST
    get_imass_sqrt(365) = 101514712903.94582	!C2H6CO_DUST
    get_imass_sqrt(366) = 94452513153.1178	!C3P_DUST
    get_imass_sqrt(367) = 104248494716.38899	!CCP_DUST
    get_imass_sqrt(368) = 86983568721.54436	!C4P_DUST
    get_imass_sqrt(369) = 112772294263.15701	!CCL_DUST
    get_imass_sqrt(370) = 130682613569.38446	!CL_DUST
    get_imass_sqrt(371) = 138858104892.15924	!P_DUST
    get_imass_sqrt(372) = 117900935127.6621	!CP_DUST
    get_imass_sqrt(373) = 113990350751.72755	!CH2PH_DUST
    get_imass_sqrt(374) = 116553033404.68169	!HCP_DUST
    get_imass_sqrt(375) = 108259531918.45573	!CLO_DUST
    get_imass_sqrt(376) = 113990350751.72755	!H2SIO_DUST
    get_imass_sqrt(377) = 103313221928.74075	!HCCP_DUST
    get_imass_sqrt(378) = 128854226660.3628	!HCL_DUST
    get_imass_sqrt(379) = 120741441507.20366	!HCSI_DUST
    get_imass_sqrt(380) = 117900089041.11632	!HNSI_DUST
    get_imass_sqrt(381) = 119295832982.81203	!SIN_DUST
    get_imass_sqrt(382) = 111591033672.87225	!HPO_DUST
    get_imass_sqrt(383) = 112772294263.15701	!PO_DUST
    get_imass_sqrt(384) = 116553033404.68169	!N2O_DUST
    get_imass_sqrt(385) = 119294956509.39111	!NH2CN_DUST
    get_imass_sqrt(386) = 113991115425.78232	!NO2_DUST
    get_imass_sqrt(387) = 136670546184.13641	!PH_DUST
    get_imass_sqrt(388) = 134583221186.17943	!PH2_DUST
    get_imass_sqrt(389) = 115251119158.44125	!PN_DUST
    get_imass_sqrt(390) = 107213197219.43008	!SIC2_DUST
    get_imass_sqrt(391) = 106196626295.6403	!SIC2H_DUST
    get_imass_sqrt(392) = 105208434300.54387	!SIC2H2_DUST
    get_imass_sqrt(393) = 96640669995.27208	!SIC3_DUST
    get_imass_sqrt(394) = 95894171204.22452	!SIC3H_DUST
    get_imass_sqrt(395) = 88683560914.8325	!SIC4_DUST
    get_imass_sqrt(396) = 119294956509.39111	!SICH2_DUST
    get_imass_sqrt(397) = 117899242972.78545	!SICH3_DUST
    get_imass_sqrt(398) = 105209035506.27245	!SINC_DUST
    get_imass_sqrt(399) = 99810054788.80415	!SIO2_DUST
    get_imass_sqrt(400) = 223187130854.6853	!C+
    get_imass_sqrt(401) = 130683630092.86522	!CL+
    get_imass_sqrt(402) = 104249595649.8177	!FE+
    get_imass_sqrt(403) = 773216569600.4055	!H+
    get_imass_sqrt(404) = 386588992536.287	!HE+
    get_imass_sqrt(405) = 157815343449.70123	!MG+
    get_imass_sqrt(406) = 206630462036.53433	!N+
    get_imass_sqrt(407) = 161210771100.99835	!NA+
    get_imass_sqrt(408) = 193284628808.09424	!O+
    get_imass_sqrt(409) = 138859324382.4182	!P+
    get_imass_sqrt(410) = 136671708941.89337	!S+
    get_imass_sqrt(411) = 146108380244.195	!SI+
    get_imass_sqrt(412) = 146108380244.195	!CO+
    get_imass_sqrt(413) = 546672253002.7066	!H2+
    get_imass_sqrt(414) = 141154014095.06906	!NO+
    get_imass_sqrt(415) = 136671708941.89337	!O2+
    get_imass_sqrt(416) = 206625907581.7343	!CH2+
    get_imass_sqrt(417) = 132589763673.59694	!H2S+
    get_imass_sqrt(418) = 143566359113.19513	!HCO+
    get_imass_sqrt(419) = 115251026097.57918	!HCS+
    get_imass_sqrt(420) = 138857942133.13364	!HNO+
    get_imass_sqrt(421) = 193280901055.30634	!NH2+
    get_imass_sqrt(422) = 99810507670.48209	!OCS+
    get_imass_sqrt(423) = 151622145677.97278	!C2H2+
    get_imass_sqrt(424) = 199617572780.53006	!CH3+
    get_imass_sqrt(425) = 187508339841.09717	!NH3+
    get_imass_sqrt(426) = 119295729776.22852	!C2H2O+
    get_imass_sqrt(427) = 113991025383.88477	!CH2O2+
    get_imass_sqrt(428) = 120741334503.0502	!C2H3N+
    get_imass_sqrt(429) = 146105159852.25903	!C2H4+
    get_imass_sqrt(430) = 109336357491.35979	!C4H2+
    get_imass_sqrt(431) = 138856559925.12622	!H3CO+
    get_imass_sqrt(432) = 136669073089.89433	!CH4O+
    get_imass_sqrt(433) = 116552119757.75081	!C2H4O+
    get_imass_sqrt(434) = 122240798652.05705	!C3H4+
    get_imass_sqrt(435) = 138855177758.39398	!CH5N+
    get_imass_sqrt(436) = 113989496054.78781	!C2H5OH+
    get_imass_sqrt(437) = 113989496054.78781	!CH3OCH3+
    get_imass_sqrt(438) = 214428340044.27756	!CH+
    get_imass_sqrt(439) = 112772947498.89154	!CCL+
    get_imass_sqrt(440) = 157815343449.70123	!C2+
    get_imass_sqrt(441) = 108260109829.27568	!CLO+
    get_imass_sqrt(442) = 117901681601.83582	!CP+
    get_imass_sqrt(443) = 116553754568.09676	!CS+
    get_imass_sqrt(444) = 151623945225.66663	!CN+
    get_imass_sqrt(445) = 113991790071.51646	!NS+
    get_imass_sqrt(446) = 136671708941.89337	!PH+
    get_imass_sqrt(447) = 112772947498.89153	!PO+
    get_imass_sqrt(448) = 122242684715.50136	!SIC+
    get_imass_sqrt(449) = 119296606266.69353	!SIN+
    get_imass_sqrt(450) = 99810507670.48209	!SIS+
    get_imass_sqrt(451) = 111591666595.33522	!SO+
    get_imass_sqrt(452) = 128855201111.00455	!C3+
    get_imass_sqrt(453) = 103313724190.13412	!C2S+
    get_imass_sqrt(454) = 122242684715.50136	!C2O+
    get_imass_sqrt(455) = 104249010742.308	!CCP+
    get_imass_sqrt(456) = 154625801545.599	!C2H+
    get_imass_sqrt(457) = 143566359113.19513	!HOC+
    get_imass_sqrt(458) = 125418393146.38841	!C2N+
    get_imass_sqrt(459) = 125418393146.38841	!CNC+
    get_imass_sqrt(460) = 116553754568.09676	!HCP+
    get_imass_sqrt(461) = 107213758534.9701	!SIC2+
    get_imass_sqrt(462) = 105209565928.01166	!SINC+
    get_imass_sqrt(463) = 111591666595.33522	!HPO+
    get_imass_sqrt(464) = 148788694908.72876	!HCN+
    get_imass_sqrt(465) = 120742243244.90813	!CHSI+
    get_imass_sqrt(466) = 141152562181.67023	!SIH2+
    get_imass_sqrt(467) = 127101431432.21623	!C3H+
    get_imass_sqrt(468) = 111591666595.33522	!C4+
    get_imass_sqrt(469) = 107213758534.9701	!C3O+
    get_imass_sqrt(470) = 93755596564.86345	!C3S+
    get_imass_sqrt(471) = 141152562181.67023	!H2CO+
    get_imass_sqrt(472) = 113991025383.88477	!H2SIO+
    get_imass_sqrt(473) = 146106770021.60883	!HCNH+
    get_imass_sqrt(474) = 106197171795.21918	!SIC2H+
    get_imass_sqrt(475) = 96641081089.59637	!SIC3+
    get_imass_sqrt(476) = 119295729776.22852	!CH2SI+
    get_imass_sqrt(477) = 138856559925.12622	!SIH3+
    get_imass_sqrt(478) = 122241741672.8667	!C2H2N+
    get_imass_sqrt(479) = 148786994434.42786	!C2H3+
    get_imass_sqrt(480) = 125417374681.35814	!C3H2+
    get_imass_sqrt(481) = 125417374681.35814	!H2C3+
    get_imass_sqrt(482) = 110446745865.81924	!C4H+
    get_imass_sqrt(483) = 99810507670.4821	!C5+
    get_imass_sqrt(484) = 86438337154.99905	!C4S+
    get_imass_sqrt(485) = 103313724190.13412	!PC2H+
    get_imass_sqrt(486) = 109337032276.54564	!C3N+
    get_imass_sqrt(487) = 98187450047.90276	!C4N+
    get_imass_sqrt(488) = 108259454787.2511	!C3HN+
    get_imass_sqrt(489) = 148788694908.72876	!HNC+
    get_imass_sqrt(490) = 95894572845.4389	!SIC3H+
    get_imass_sqrt(491) = 88683878595.37509	!SIC4+
    get_imass_sqrt(492) = 105208964713.18997	!SIC2H2+
    get_imass_sqrt(493) = 117899989414.81844	!SICH3+
    get_imass_sqrt(494) = 107213122303.11108	!HC2NCH+
    get_imass_sqrt(495) = 123798531445.55338	!C3H3+
    get_imass_sqrt(496) = 123798531445.55338	!H3C3+
    get_imass_sqrt(497) = 98988750299.09656	!C5H+
    get_imass_sqrt(498) = 91114041974.46526	!C6+
    get_imass_sqrt(499) = 117899989414.81844	!C2H3O+
    get_imass_sqrt(500) = 143563303897.2834	!C2H5+
    get_imass_sqrt(501) = 106196553490.1959	!C3H3N+
    get_imass_sqrt(502) = 98186961359.0677	!C5H2+
    get_imass_sqrt(503) = 108258799757.11661	!C4H3+
    get_imass_sqrt(504) = 90487625267.59636	!C6H+
    get_imass_sqrt(505) = 84355165865.25325	!C7+
    get_imass_sqrt(506) = 141151110313.07373	!CH4N+
    get_imass_sqrt(507) = 89272967340.20717	!C5HN+
    get_imass_sqrt(508) = 83857335512.66237	!C7H+
    get_imass_sqrt(509) = 78907000400.63521	!C8+
    get_imass_sqrt(510) = 99809481026.64713	!COOCH4+
    get_imass_sqrt(511) = 115249445493.2091	!C2H5O+
    get_imass_sqrt(512) = 78499082397.3593	!C8H+
    get_imass_sqrt(513) = 74394209980.27675	!C9+
    get_imass_sqrt(514) = 97404345023.62802	!C5H3+
    get_imass_sqrt(515) = 89873953418.56085	!C6H2+
    get_imass_sqrt(516) = 89272600036.80914	!C6H3+
    get_imass_sqrt(517) = 101515189388.62723	!C2H6CO+
    get_imass_sqrt(518) = 74052059289.7637	!C9H+
    get_imass_sqrt(519) = 70576526689.07214	!C10+
    get_imass_sqrt(520) = 82887557327.11375	!C7H3+
    get_imass_sqrt(521) = 78097426006.09845	!C8H2+
    get_imass_sqrt(522) = 77701872654.19771	!C8H3+
    get_imass_sqrt(523) = 128855201111.00455	!HCL+
    get_imass_sqrt(524) = 134584331477.7524	!HS+
    get_imass_sqrt(525) = 199621679333.48865	!NH+
    get_imass_sqrt(526) = 187511743462.96832	!OH+
    get_imass_sqrt(527) = 115251816424.15169	!PN+
    get_imass_sqrt(528) = 96641081089.59637	!S2+
    get_imass_sqrt(529) = 143566359113.19513	!SIH+
    get_imass_sqrt(530) = 116553754568.09676	!SIO+
    get_imass_sqrt(531) = 182227027061.27744	!H2O+
    get_imass_sqrt(532) = 117900835499.21938	!HNSI+
    get_imass_sqrt(533) = 95894572845.4389	!S2H+
    get_imass_sqrt(534) = 134584331477.7524	!PH2+
    get_imass_sqrt(535) = 113991025383.88477	!H2CS+
    get_imass_sqrt(536) = 95165100879.59782	!H2S2+
    get_imass_sqrt(537) = 115251026097.57918	!HSIO+
    get_imass_sqrt(538) = 86983868480.95297	!C4P+
    get_imass_sqrt(539) = 115251026097.57918	!HCO2+
    get_imass_sqrt(540) = 113991025383.88477	!PCH3+
    get_imass_sqrt(541) = 193277173518.19424	!CH4+
    get_imass_sqrt(542) = 123799510979.17654	!C2NH+
    get_imass_sqrt(543) = 136669073089.89433	!SIH4+
    get_imass_sqrt(544) = 182223903177.14545	!NH4+
    get_imass_sqrt(545) = 146106770021.60883	!H2NC+
    get_imass_sqrt(546) = 107213122303.11108	!C3H2N+
    get_imass_sqrt(547) = 83368216375.0112	!C7H2+
    get_imass_sqrt(548) = 96640149173.5979	!C5H4+
    get_imass_sqrt(549) = 77702114848.65634	!C7HN+
    get_imass_sqrt(550) = 73714586402.45041	!C9H2+
    get_imass_sqrt(551) = 82415117263.44328	!C7H4+
    get_imass_sqrt(552) = 69710432937.7582	!C9HN+
    get_imass_sqrt(553) = 146108380244.195	!N2+
    get_imass_sqrt(554) = 116553754568.09676	!CO2+
    get_imass_sqrt(555) = 345760328884.13617	!HEH+
    get_imass_sqrt(556) = 96641081089.59637	!SO2+
    get_imass_sqrt(557) = 88105240496.0305	!C6H5+
    get_imass_sqrt(558) = 95893662358.72629	!C5H5+
    get_imass_sqrt(559) = 143566359113.19513	!N2H+
    get_imass_sqrt(560) = 113991790071.51646	!NO2+
    get_imass_sqrt(561) = 102403167444.47696	!PC2H2+
    get_imass_sqrt(562) = 112772207078.73605	!PNH2+
    get_imass_sqrt(563) = 115251026097.57918	!PCH2+
    get_imass_sqrt(564) = 102403167444.47696	!HC2S+
    get_imass_sqrt(565) = 93073515109.0492	!HC3S+
    get_imass_sqrt(566) = 112771466673.16428	!H3CS+
    get_imass_sqrt(567) = 85902942867.81284	!HC4S+
    get_imass_sqrt(568) = 116552937154.32489	!SINH2+
    get_imass_sqrt(569) = 104247840956.82332	!SIC2H3+
    get_imass_sqrt(570) = 95165100879.59782	!SIC3H2+
    get_imass_sqrt(571) = 120742243244.90813	!C2HO+
    get_imass_sqrt(572) = 177365342346.192	!H3O+
    get_imass_sqrt(573) = 130681325735.70607	!H3S+
    get_imass_sqrt(574) = 98988750299.09656	!HOCS+
    get_imass_sqrt(575) = 134581814559.95168	!CH5O+
    get_imass_sqrt(576) = 119296606266.69353	!NCO+
    get_imass_sqrt(577) = 117900835499.21938	!HNCO+
    get_imass_sqrt(578) = 107213758534.9701	!C2N2+
    get_imass_sqrt(579) = 446335774600.3221	!H3+
    get_imass_sqrt(580) = 134584331477.7524	!O2H+
    get_imass_sqrt(581) = 187504936404.56192	!CH5+
    get_imass_sqrt(582) = 127101431432.21623	!H2CL+
    get_imass_sqrt(583) = 112771466673.16428	!CH3O2+
    get_imass_sqrt(584) = 110446745865.81923	!H2PO+
    get_imass_sqrt(585) = 111590949199.55452	!PNH3+
    get_imass_sqrt(586) = 112771466673.16428	!PCH4+
    get_imass_sqrt(587) = 101516269572.60168	!PC2H3+
    get_imass_sqrt(588) = 98988750299.09656	!HSIS+
    get_imass_sqrt(589) = 110446745865.81923	!HSO+
    get_imass_sqrt(590) = 112772207078.73607	!HNS+
    get_imass_sqrt(591) = 113991790071.51646	!HPN+
    get_imass_sqrt(592) = 136670390996.83052	!H2NO+
    get_imass_sqrt(593) = 120742243244.90813	!NAH2O+
    get_imass_sqrt(594) = 132589763673.59694	!PH3+
    get_imass_sqrt(595) = 104248425844.64328	!SINCH+
    get_imass_sqrt(596) = 98988750299.09656	!HSIO2+
    get_imass_sqrt(597) = 95894572845.4389	!HSO2+
    get_imass_sqrt(598) = 106197171795.21918	!HC3O+
    get_imass_sqrt(599) = 93755596564.86345	!PC3H+
    get_imass_sqrt(600) = 94452026913.92284	!H3S2+
    get_imass_sqrt(601) = 112771466673.16428	!H3SIO+
    get_imass_sqrt(602) = 86438337154.99905	!PC4H+
    get_imass_sqrt(603) = 117899989414.81844	!NH2CNH+
    get_imass_sqrt(604) = 88105946664.22768	!SIC4H+
    get_imass_sqrt(605) = 116552119757.75078	!SICH4+
    get_imass_sqrt(606) = 134581814559.95168	!SIH5+
    get_imass_sqrt(607) = 119294853305.08235	!C2H4N+
    get_imass_sqrt(608) = 113990260711.64207	!NH2CH2O+
    get_imass_sqrt(609) = 141149658489.27722	!C2H6+
    get_imass_sqrt(610) = 105208363508.675	!C3H4N+
    get_imass_sqrt(611) = 120740425781.71036	!C3H5+
    get_imass_sqrt(612) = 107212486082.5786	!C4H4+
    get_imass_sqrt(613) = 136667755221.08295	!CH6N+
    get_imass_sqrt(614) = 88683518515.28702	!C5H2N+
    get_imass_sqrt(615) = 95164655943.33156	!C4H4N+
    get_imass_sqrt(616) = 98987748804.42238	!H5C2O2+
    get_imass_sqrt(617) = 112769985905.76982	!C2H5OH2+
    get_imass_sqrt(618) = 112769985905.76982	!CH3OCH4+
    get_imass_sqrt(619) = 77312507904.28206	!C7H2N+
    get_imass_sqrt(620) = 100650970634.53056	!C3H6OH+
    get_imass_sqrt(621) = 81494256159.81026	!C6H4N+
    get_imass_sqrt(622) = 70284192148.05168	!C10H+
    get_imass_sqrt(623) = 73381685691.02007	!C9H3+
    get_imass_sqrt(624) = 81950664589.92653	!C7H5+
    get_imass_sqrt(625) = 72409605191.18365	!C8H4N+
    get_imass_sqrt(626) = 69428686545.68166	!C9H2N+
    get_imass_sqrt(627) = 86982509436.43497	!C6H7+
    get_imass_sqrt(628) = 154625801545.599	!NAH2+
    get_imass_sqrt(629) = 100652023464.95583	!PC2H4+
    get_imass_sqrt(630) = 106195935195.97227	!C4H5+
    get_imass_sqrt(631) = 110446745865.81924	!H2CCL+
    get_imass_sqrt(632) = 85902942867.81284	!PC4H2+
    get_imass_sqrt(633) = 88683158439.58498	!C6H4+
    get_imass_sqrt(634) = 77312269334.74445	!C8H4+
    get_imass_sqrt(635) = 73053254837.44719	!C9H4+
    get_imass_sqrt(636) = 104246671210.71649	!C4H7+
    get_imass_sqrt(637) = 97404822119.7614	!HC4N+
    get_imass_sqrt(638) = 95894572845.4389	!HC4O+
    get_imass_sqrt(639) = 89874328194.73167	!C5N+
    get_imass_sqrt(640) = 96640615128.22717	!H2C4N+
    get_imass_sqrt(641) = 95894117598.84077	!H3C4N+
    get_imass_sqrt(642) = 78097671918.2146	!C7N+
    get_imass_sqrt(643) = 88105593578.0066	!C5H3N+
    get_imass_sqrt(644) = 69995460401.64343	!C10H2+
    get_imass_sqrt(645) = 69995637444.73555	!C9N+
    get_imass_sqrt(646) = 76928703392.0408	!C7H3N+
    get_imass_sqrt(647) = 69150328945.73361	!C9H3N+
    get_imass_sqrt(648) = 98186961359.0677	!OCS+H2
    get_imass_sqrt(649) = 105208964713.18997	!H2C3O+
    get_imass_sqrt(650) = 104247840956.82332	!H3C3O+
    get_imass_sqrt(651) = 87538821864.83897	!C5H4N+
    get_imass_sqrt(652) = 76928468357.89436	!C8H5+
    get_imass_sqrt(653) = 72729194700.87735	!C9H5+
    get_imass_sqrt(654) = 98987748804.42236	!H2COHOCH2+
    get_imass_sqrt(655) = 97403390852.3926	!H7C2O2+
    get_imass_sqrt(656) = 0d0	!CR
    get_imass_sqrt(657) = 0d0	!g
    get_imass_sqrt(658) = 0d0	!Tgas
    get_imass_sqrt(659) = 0d0	!dummy

  end function get_imass_sqrt

  !************************
  !get inverse of the species masses (1/g)
  function get_imass()
    use krome_commons
    implicit none
    real*8::get_imass(nspec)

    get_imass(1) = 1.0977693252662275d+27	!E
    get_imass(2) = 5.9721335838016375d+23	!H-
    get_imass(3) = 4.98079751953935d+22	!C-
    get_imass(4) = 2.2988857886418185d+22	!CN-
    get_imass(5) = 3.7356405130124037d+22	!O-
    get_imass(6) = 3.515840174679815d+22	!OH-
    get_imass(7) = 1.8678520374258483d+22	!S-
    get_imass(8) = 4.981021258381197d+20	!GRAIN-
    get_imass(9) = 4.981023518472045d+22	!C
    get_imass(10) = 1.707794548932507d+22	!CL
    get_imass(11) = 1.0867870600619004d+22	!FE
    get_imass(12) = 5.9753843390072855d+23	!H
    get_imass(13) = 1.4943070555416133d+23	!HE
    get_imass(14) = 2.4905117592360225d+22	!MG
    get_imass(15) = 4.269448730118895d+22	!N
    get_imass(16) = 2.598829746440817d+22	!NA
    get_imass(17) = 3.7357676388540333d+22	!O
    get_imass(18) = 1.9281573294241894d+22	!P
    get_imass(19) = 1.8678838194270166d+22	!S
    get_imass(20) = 2.1347243650594476d+22	!SI
    get_imass(21) = 2.4905117592360225d+22	!C2
    get_imass(22) = 1.2717590353376077d+22	!CCL
    get_imass(23) = 4.5977587266153915d+22	!CH
    get_imass(24) = 1.1720126251203136d+22	!CLO
    get_imass(25) = 2.2989339316024823d+22	!CN
    get_imass(26) = 2.1347243650594476d+22	!CO
    get_imass(27) = 1.3900630503977187d+22	!CP
    get_imass(28) = 1.3584609595832849d+22	!CS
    get_imass(29) = 2.9876921695036427d+23	!H2
    get_imass(30) = 1.660341172824015d+22	!HCL
    get_imass(31) = 1.8112643424848093d+22	!HS
    get_imass(32) = 2.3908617779965302d+22	!MGH
    get_imass(33) = 2.1347243650594476d+22	!N2
    get_imass(34) = 2.490511759236022d+22	!NAH
    get_imass(35) = 3.984736840813143d+22	!NH
    get_imass(36) = 1.9924094073888177d+22	!NO
    get_imass(37) = 1.2993974396014029d+22	!NS
    get_imass(38) = 1.8678838194270166d+22	!O2
    get_imass(39) = 3.5159527805593353d+22	!OH
    get_imass(40) = 1.8678838194270166d+22	!PH
    get_imass(41) = 1.3282820467273226d+22	!PN
    get_imass(42) = 1.2717590353376075d+22	!PO
    get_imass(43) = 9.339419097135083d+21	!S2
    get_imass(44) = 1.4943070555416132d+22	!SIC
    get_imass(45) = 2.061091248636252d+22	!SIH
    get_imass(46) = 1.4231495767062984d+22	!SIN
    get_imass(47) = 1.3584609595832849d+22	!SIO
    get_imass(48) = 9.962047036944089d+21	!SIS
    get_imass(49) = 1.2452558796180112d+22	!SO
    get_imass(50) = 2.3908617779965302d+22	!C2H
    get_imass(51) = 1.5729547953069615d+22	!C2N
    get_imass(52) = 1.0673621825297238d+22	!C2S
    get_imass(53) = 1.660341172824015d+22	!C3
    get_imass(54) = 1.4943070555416132d+22	!CCO
    get_imass(55) = 1.0867748650632983d+22	!CCP
    get_imass(56) = 4.269260529012849d+22	!CH2
    get_imass(57) = 1.3584609595832849d+22	!CO2
    get_imass(58) = 3.320568494480966d+22	!H2O
    get_imass(59) = 1.7579763902796676d+22	!H2S
    get_imass(60) = 2.2137629295943715d+22	!HCN
    get_imass(61) = 2.0610912486362525d+22	!HCO
    get_imass(62) = 1.3584609595832849d+22	!HCP
    get_imass(63) = 1.3282638299160197d+22	!HCS
    get_imass(64) = 1.4578495697237486d+22	!HCSI
    get_imass(65) = 2.2137629295943715d+22	!HNC
    get_imass(66) = 1.9281189433233667d+22	!HNO
    get_imass(67) = 1.3900430995903155d+22	!HNSI
    get_imass(68) = 1.245255879618011d+22	!HPO
    get_imass(69) = 9.195692070945122d+21	!HS2
    get_imass(70) = 1.3584609595832849d+22	!N2O
    get_imass(71) = 1.4943070555416132d+22	!NAOH
    get_imass(72) = 3.7356235465882524d+22	!NH2
    get_imass(73) = 1.2993974396014029d+22	!NO2
    get_imass(74) = 1.8112643424848093d+22	!O2H
    get_imass(75) = 1.4231495767062984d+22	!OCN
    get_imass(76) = 9.962047036944089d+21	!OCS
    get_imass(77) = 1.8112643424848093d+22	!PH2
    get_imass(78) = 1.1494669658012411d+22	!SIC2
    get_imass(79) = 1.9923684204065715d+22	!SIH2
    get_imass(80) = 1.10689411521601d+22	!SINC
    get_imass(81) = 9.962047036944089d+21	!SIO2
    get_imass(82) = 9.339419097135083d+21	!SO2
    get_imass(83) = 2.2988793633076958d+22	!C2H2
    get_imass(84) = 1.6154536141957416d+22	!C3H
    get_imass(85) = 1.1954456444332908d+22	!C3N
    get_imass(86) = 1.1494669658012411d+22	!C3O
    get_imass(87) = 8.92127724093989d+21	!C3P
    get_imass(88) = 8.79004150318596d+21	!C3S
    get_imass(89) = 1.2452558796180112d+22	!C4
    get_imass(90) = 3.98457290300174d+22	!CH3
    get_imass(91) = 1.9923684204065715d+22	!H2CO
    get_imass(92) = 1.2993800064501876d+22	!H2CS
    get_imass(93) = 1.7579763902796676d+22	!H2O2
    get_imass(94) = 9.056321712424047d+21	!H2S2
    get_imass(95) = 1.2993800064501876d+22	!H2SIO
    get_imass(96) = 1.0673621825297236d+22	!HCCP
    get_imass(97) = 3.515825146079608d+22	!NH3
    get_imass(98) = 1.1277723436575881d+22	!SIC2H
    get_imass(99) = 9.339419097135083d+21	!SIC3
    get_imass(100) = 1.4231286648577515d+22	!SICH2
    get_imass(101) = 1.9280805587509085d+22	!SIH3
    get_imass(102) = 1.494284000245529d+22	!C2H2N
    get_imass(103) = 1.4231286648577515d+22	!C2H2O
    get_imass(104) = 2.2137123296539774d+22	!C2H3
    get_imass(105) = 1.5729292492932632d+22	!C3H2
    get_imass(106) = 1.219834812351634d+22	!C4H
    get_imass(107) = 9.640690680913635d+21	!C4N
    get_imass(108) = 7.566141227535632d+21	!C4P
    get_imass(109) = 7.471535277708066d+21	!C4S
    get_imass(110) = 9.962047036944089d+21	!C5
    get_imass(111) = 1.2993800064501876d+22	!CH2O2
    get_imass(112) = 1.2993800064501876d+22	!CH2PH
    get_imass(113) = 2.061047387194109d+22	!CH3N
    get_imass(114) = 3.735479465437607d+22	!CH4
    get_imass(115) = 1.171998442481756d+22	!HC3N
    get_imass(116) = 1.1068814647971858d+22	!SIC2H2
    get_imass(117) = 9.195692070945122d+21	!SIC3H
    get_imass(118) = 7.864773976534807d+21	!SIC4
    get_imass(119) = 1.3900231493555897d+22	!SICH3
    get_imass(120) = 1.8678117732941262d+22	!SIH4
    get_imass(121) = 1.4578276256830842d+22	!C2H3N
    get_imass(122) = 2.1346302645064247d+22	!C2H4
    get_imass(123) = 1.5325862422051304d+22	!C3H3
    get_imass(124) = 1.1954308889982651d+22	!C4H2
    get_imass(125) = 9.798685221965594d+21	!C5H
    get_imass(126) = 8.077335435360073d+21	!C5N
    get_imass(127) = 8.301705864120075d+21	!C6
    get_imass(128) = 1.8678117732941262d+22	!CH4O
    get_imass(129) = 1.3584228521369234d+22	!C2H4O
    get_imass(130) = 2.0610035276187294d+22	!C2H5
    get_imass(131) = 1.1277592115166866d+22	!C3H3N
    get_imass(132) = 1.4942609456608628d+22	!C3H4
    get_imass(133) = 9.640594716616833d+21	!C5H2
    get_imass(134) = 8.18794925452812d+21	!C6H
    get_imass(135) = 7.115747883531492d+21	!C7
    get_imass(136) = 1.9280421757067234d+22	!CH5N
    get_imass(137) = 7.969604839429838d+21	!HC5N
    get_imass(138) = 8.077268070978708d+21	!C6H2
    get_imass(139) = 7.032007673892044d+21	!C7H
    get_imass(140) = 6.099212471598421d+21	!C7N
    get_imass(141) = 6.226279398090056d+21	!C8
    get_imass(142) = 9.195604761258987d+21	!CH3C3N
    get_imass(143) = 9.961842102032858d+21	!HCOOCH3
    get_imass(144) = 1.299345141551054d+22	!C2H5OH
    get_imass(145) = 6.950215497951577d+21	!C7H2
    get_imass(146) = 6.162071347686994d+21	!C8H
    get_imass(147) = 5.53447057608005d+21	!C9
    get_imass(148) = 9.339238978329177d+21	!CH3C4H
    get_imass(149) = 1.299345141551054d+22	!CH3OCH3
    get_imass(150) = 6.037585445853317d+21	!HC7N
    get_imass(151) = 1.0305236935970545d+22	!C2H6CO
    get_imass(152) = 6.09917406175817d+21	!C8H2
    get_imass(153) = 5.483680092325737d+21	!C9H
    get_imass(154) = 4.899367395218405d+21	!C9N
    get_imass(155) = 4.981023518472044d+21	!C10
    get_imass(156) = 6.715916910414547d+21	!CH3C5N
    get_imass(157) = 5.433813351691883d+21	!C9H2
    get_imass(158) = 6.792209527964268d+21	!CH3C6H
    get_imass(159) = 5.28953916355006d+21	!CH3C7N
    get_imass(160) = 4.85952294850259d+21	!HC9N
    get_imass(161) = 1.1494396816538479d+22	!C4H4
    get_imass(162) = 1.171998442481756d+22	!HCNC2
    get_imass(163) = 1.171998442481756d+22	!HC2NC
    get_imass(164) = 1.171998442481756d+22	!HNC3
    get_imass(165) = 1.328245613604381d+22	!NH2CHO
    get_imass(166) = 1.1719842601864452d+22	!C4H3
    get_imass(167) = 1.4231286648577515d+22	!NH2CN
    get_imass(168) = 7.662931211025652d+21	!C6H6
    get_imass(169) = 2.134677313745904d+22	!H2CN
    get_imass(170) = 4.981023518472044d+20	!GRAIN0
    get_imass(171) = 1.2452558796180112d+22	!O3
    get_imass(172) = 1.0673739457470178d+22	!FEH
    get_imass(173) = 1.3900430995903157d+22	!HNCO
    get_imass(174) = 1.4578495697237486d+22	!HC2O
    get_imass(175) = 1.5326104945886988d+22	!HCCN
    get_imass(176) = 1.1277723436575881d+22	!HC3O
    get_imass(177) = 2.2988793633076958d+22	!MGH2
    get_imass(178) = 1.9923684204065715d+22	!N2H2
    get_imass(179) = 2.134677313745904d+22	!CHNH
    get_imass(180) = 1.1068814647971858d+22	!H2C3O
    get_imass(181) = 1.149453323565636d+22	!H2C3N
    get_imass(182) = 7.864710110981949d+21	!H2C5N
    get_imass(183) = 5.977191333351229d+21	!H2C7N
    get_imass(184) = 4.820321349263212d+21	!H2C9N
    get_imass(185) = 1.8112304695158975d+22	!NH2OH
    get_imass(186) = 1.9280805587509085d+22	!CH2OH
    get_imass(187) = 9.48752443238501d+21	!C5H3
    get_imass(188) = 7.762540728907209d+21	!H3C5N
    get_imass(189) = 7.969539259988435d+21	!C6H3
    get_imass(190) = 6.870304162120841d+21	!C7H3
    get_imass(191) = 5.917993501941923d+21	!H3C7N
    get_imass(192) = 6.037547808282699d+21	!C8H3
    get_imass(193) = 5.384845380652473d+21	!C9H3
    get_imass(194) = 4.781747164514259d+21	!H3C9N
    get_imass(195) = 1.992327435110624d+22	!CH3NH
    get_imass(196) = 1.1068688146675154d+22	!H4C3N
    get_imass(197) = 9.339238978329177d+21	!C5H4
    get_imass(198) = 7.864646246466316d+21	!C6H4
    get_imass(199) = 6.792209527964267d+21	!C7H4
    get_imass(200) = 5.977154444991326d+21	!C8H4
    get_imass(201) = 5.336752097858523d+21	!C9H4
    get_imass(202) = 1.0867382817095386d+22	!H5C3N
    get_imass(203) = 1.99228645150087d+22	!C2H6
    get_imass(204) = 4.981023518472045d+22	!C_DUST
    get_imass(205) = 2.4905117592360225d+22	!C2_DUST
    get_imass(206) = 1.660341172824015d+22	!C3_DUST
    get_imass(207) = 2.3908617779965302d+22	!C2H_DUST
    get_imass(208) = 1.6154536141957416d+22	!C3H_DUST
    get_imass(209) = 2.2137123296539774d+22	!C2H3_DUST
    get_imass(210) = 1.5325862422051304d+22	!C3H3_DUST
    get_imass(211) = 1.5729547953069615d+22	!C2N_DUST
    get_imass(212) = 1.1954456444332908d+22	!C3N_DUST
    get_imass(213) = 1.4943070555416132d+22	!CCO_DUST
    get_imass(214) = 1.1494669658012411d+22	!C3O_DUST
    get_imass(215) = 1.0673621825297238d+22	!C2S_DUST
    get_imass(216) = 8.79004150318596d+21	!C3S_DUST
    get_imass(217) = 1.2452558796180112d+22	!C4_DUST
    get_imass(218) = 1.219834812351634d+22	!C4H_DUST
    get_imass(219) = 9.962047036944089d+21	!C5_DUST
    get_imass(220) = 9.798685221965594d+21	!C5H_DUST
    get_imass(221) = 8.301705864120075d+21	!C6_DUST
    get_imass(222) = 8.18794925452812d+21	!C6H_DUST
    get_imass(223) = 7.115747883531492d+21	!C7_DUST
    get_imass(224) = 7.032007673892044d+21	!C7H_DUST
    get_imass(225) = 6.226279398090056d+21	!C8_DUST
    get_imass(226) = 6.162071347686994d+21	!C8H_DUST
    get_imass(227) = 5.53447057608005d+21	!C9_DUST
    get_imass(228) = 5.483680092325737d+21	!C9H_DUST
    get_imass(229) = 4.981023518472044d+21	!C10_DUST
    get_imass(230) = 4.5977587266153915d+22	!CH_DUST
    get_imass(231) = 4.269260529012849d+22	!CH2_DUST
    get_imass(232) = 2.2988793633076958d+22	!C2H2_DUST
    get_imass(233) = 3.98457290300174d+22	!CH3_DUST
    get_imass(234) = 2.2989339316024823d+22	!CN_DUST
    get_imass(235) = 1.8112643424848093d+22	!HS_DUST
    get_imass(236) = 1.3584609595832849d+22	!CS_DUST
    get_imass(237) = 5.9753843390072855d+23	!H_DUST
    get_imass(238) = 4.269448730118895d+22	!N_DUST
    get_imass(239) = 3.984736840813143d+22	!NH_DUST
    get_imass(240) = 2.2137629295943715d+22	!HNC_DUST
    get_imass(241) = 3.7356235465882524d+22	!NH2_DUST
    get_imass(242) = 1.9924094073888177d+22	!NO_DUST
    get_imass(243) = 3.7357676388540333d+22	!O_DUST
    get_imass(244) = 1.4231495767062984d+22	!OCN_DUST
    get_imass(245) = 1.2993974396014029d+22	!NS_DUST
    get_imass(246) = 1.8678838194270166d+22	!S_DUST
    get_imass(247) = 2.1347243650594476d+22	!CO_DUST
    get_imass(248) = 1.8678838194270166d+22	!O2_DUST
    get_imass(249) = 3.5159527805593353d+22	!OH_DUST
    get_imass(250) = 1.2452558796180112d+22	!SO_DUST
    get_imass(251) = 1.5729292492932632d+22	!C3H2_DUST
    get_imass(252) = 1.4942609456608628d+22	!C3H4_DUST
    get_imass(253) = 1.1954308889982651d+22	!C4H2_DUST
    get_imass(254) = 9.640594716616833d+21	!C5H2_DUST
    get_imass(255) = 8.077268070978708d+21	!C6H2_DUST
    get_imass(256) = 6.950215497951577d+21	!C7H2_DUST
    get_imass(257) = 6.09917406175817d+21	!C8H2_DUST
    get_imass(258) = 5.433813351691883d+21	!C9H2_DUST
    get_imass(259) = 2.1346302645064247d+22	!C2H4_DUST
    get_imass(260) = 1.5326104945886988d+22	!HCCN_DUST
    get_imass(261) = 1.9281189433233667d+22	!HNO_DUST
    get_imass(262) = 2.2137629295943715d+22	!HCN_DUST
    get_imass(263) = 2.134677313745904d+22	!CHNH_DUST
    get_imass(264) = 2.061047387194109d+22	!CH3N_DUST
    get_imass(265) = 2.0610912486362525d+22	!HCO_DUST
    get_imass(266) = 2.0610035276187294d+22	!C2H5_DUST
    get_imass(267) = 1.494284000245529d+22	!C2H2N_DUST
    get_imass(268) = 1.992327435110624d+22	!CH2NH2_DUST
    get_imass(269) = 1.9923684204065715d+22	!H2CO_DUST
    get_imass(270) = 9.195604761258987d+21	!CH3C3N_DUST
    get_imass(271) = 8.077335435360073d+21	!C5N_DUST
    get_imass(272) = 6.715916910414547d+21	!CH3C5N_DUST
    get_imass(273) = 6.099212471598421d+21	!C7N_DUST
    get_imass(274) = 5.28953916355006d+21	!CH3C7N_DUST
    get_imass(275) = 1.9280805587509085d+22	!CH2OH_DUST
    get_imass(276) = 1.299345141551054d+22	!C2H5OH_DUST
    get_imass(277) = 1.299345141551054d+22	!CH3OCH3_DUST
    get_imass(278) = 1.99228645150087d+22	!C2H6_DUST
    get_imass(279) = 1.4578276256830842d+22	!C2H3N_DUST
    get_imass(280) = 1.3584228521369234d+22	!C2H4O_DUST
    get_imass(281) = 3.735479465437607d+22	!CH4_DUST
    get_imass(282) = 2.9876921695036427d+23	!H2_DUST
    get_imass(283) = 1.4578495697237486d+22	!HC2O_DUST
    get_imass(284) = 1.1277592115166866d+22	!C3H3N_DUST
    get_imass(285) = 1.1068688146675154d+22	!H4C3N_DUST
    get_imass(286) = 1.171998442481756d+22	!HC3N_DUST
    get_imass(287) = 1.1277723436575881d+22	!HC3O_DUST
    get_imass(288) = 1.1719842601864452d+22	!C4H3_DUST
    get_imass(289) = 1.1494396816538479d+22	!C4H4_DUST
    get_imass(290) = 9.48752443238501d+21	!C5H3_DUST
    get_imass(291) = 9.339238978329177d+21	!C5H4_DUST
    get_imass(292) = 7.969604839429838d+21	!HC5N_DUST
    get_imass(293) = 7.969539259988435d+21	!C6H3_DUST
    get_imass(294) = 7.864646246466316d+21	!C6H4_DUST
    get_imass(295) = 6.870304162120841d+21	!C7H3_DUST
    get_imass(296) = 6.792209527964267d+21	!C7H4_DUST
    get_imass(297) = 6.037585445853317d+21	!HC7N_DUST
    get_imass(298) = 6.037547808282699d+21	!C8H3_DUST
    get_imass(299) = 5.977154444991326d+21	!C8H4_DUST
    get_imass(300) = 5.384845380652473d+21	!C9H3_DUST
    get_imass(301) = 5.336752097858523d+21	!C9H4_DUST
    get_imass(302) = 4.899367395218405d+21	!C9N_DUST
    get_imass(303) = 4.85952294850259d+21	!HC9N_DUST
    get_imass(304) = 1.992327435110624d+22	!CH3NH_DUST
    get_imass(305) = 1.9280421757067234d+22	!CH5N_DUST
    get_imass(306) = 1.8678117732941262d+22	!CH4O_DUST
    get_imass(307) = 1.3282638299160197d+22	!HCS_DUST
    get_imass(308) = 1.0867870600619004d+22	!FE_DUST
    get_imass(309) = 1.0673739457470178d+22	!FEH_DUST
    get_imass(310) = 1.149453323565636d+22	!H2C3N_DUST
    get_imass(311) = 7.864710110981949d+21	!H2C5N_DUST
    get_imass(312) = 7.762540728907209d+21	!H3C5N_DUST
    get_imass(313) = 5.977191333351229d+21	!H2C7N_DUST
    get_imass(314) = 5.917993501941923d+21	!H3C7N_DUST
    get_imass(315) = 4.820321349263212d+21	!H2C9N_DUST
    get_imass(316) = 4.781747164514259d+21	!H3C9N_DUST
    get_imass(317) = 2.134677313745904d+22	!H2CN_DUST
    get_imass(318) = 1.7579763902796676d+22	!H2O2_DUST
    get_imass(319) = 3.320568494480966d+22	!H2O_DUST
    get_imass(320) = 1.8112643424848093d+22	!O2H_DUST
    get_imass(321) = 1.7579763902796676d+22	!H2S_DUST
    get_imass(322) = 1.0867382817095386d+22	!H5C3N_DUST
    get_imass(323) = 1.4231286648577515d+22	!C2H2O_DUST
    get_imass(324) = 1.1068814647971858d+22	!H2C3O_DUST
    get_imass(325) = 1.2993800064501876d+22	!H2CS_DUST
    get_imass(326) = 2.4905117592360225d+22	!MG_DUST
    get_imass(327) = 2.3908617779965302d+22	!MGH_DUST
    get_imass(328) = 2.2988793633076958d+22	!MGH2_DUST
    get_imass(329) = 1.9923684204065715d+22	!N2H2_DUST
    get_imass(330) = 2.1347243650594476d+22	!N2_DUST
    get_imass(331) = 2.598829746440817d+22	!NA_DUST
    get_imass(332) = 2.490511759236022d+22	!NAH_DUST
    get_imass(333) = 3.515825146079608d+22	!NH3_DUST
    get_imass(334) = 1.2452558796180112d+22	!O3_DUST
    get_imass(335) = 1.3900430995903157d+22	!HNCO_DUST
    get_imass(336) = 9.962047036944089d+21	!OCS_DUST
    get_imass(337) = 2.1347243650594476d+22	!SI_DUST
    get_imass(338) = 2.061091248636252d+22	!SIH_DUST
    get_imass(339) = 1.9923684204065715d+22	!SIH2_DUST
    get_imass(340) = 1.9280805587509085d+22	!SIH3_DUST
    get_imass(341) = 1.8678117732941262d+22	!SIH4_DUST
    get_imass(342) = 9.339419097135083d+21	!SO2_DUST
    get_imass(343) = 9.961842102032858d+21	!HCOOCH3_DUST
    get_imass(344) = 1.328245613604381d+22	!NH2CHO_DUST
    get_imass(345) = 1.3584609595832849d+22	!CO2_DUST
    get_imass(346) = 1.2993800064501876d+22	!CH2O2_DUST
    get_imass(347) = 1.8112304695158975d+22	!NH2OH_DUST
    get_imass(348) = 9.640690680913635d+21	!C4N_DUST
    get_imass(349) = 7.471535277708066d+21	!C4S_DUST
    get_imass(350) = 7.662931211025652d+21	!C6H6_DUST
    get_imass(351) = 1.992327435110624d+22	!CH2NH2
    get_imass(352) = 9.339238978329177d+21	!CH3C4H_DUST
    get_imass(353) = 6.792209527964268d+21	!CH3C6H_DUST
    get_imass(354) = 9.056321712424047d+21	!H2S2_DUST
    get_imass(355) = 1.171998442481756d+22	!HC2NC_DUST
    get_imass(356) = 1.171998442481756d+22	!HCNC2_DUST
    get_imass(357) = 1.4943070555416133d+23	!HE_DUST
    get_imass(358) = 1.171998442481756d+22	!HNC3_DUST
    get_imass(359) = 9.195692070945122d+21	!HS2_DUST
    get_imass(360) = 1.4943070555416132d+22	!NAOH_DUST
    get_imass(361) = 9.339419097135083d+21	!S2_DUST
    get_imass(362) = 1.4943070555416132d+22	!SIC_DUST
    get_imass(363) = 1.3584609595832849d+22	!SIO_DUST
    get_imass(364) = 9.962047036944089d+21	!SIS_DUST
    get_imass(365) = 1.0305236935970545d+22	!C2H6CO_DUST
    get_imass(366) = 8.92127724093989d+21	!C3P_DUST
    get_imass(367) = 1.0867748650632983d+22	!CCP_DUST
    get_imass(368) = 7.566141227535632d+21	!C4P_DUST
    get_imass(369) = 1.2717590353376077d+22	!CCL_DUST
    get_imass(370) = 1.707794548932507d+22	!CL_DUST
    get_imass(371) = 1.9281573294241894d+22	!P_DUST
    get_imass(372) = 1.3900630503977187d+22	!CP_DUST
    get_imass(373) = 1.2993800064501876d+22	!CH2PH_DUST
    get_imass(374) = 1.3584609595832849d+22	!HCP_DUST
    get_imass(375) = 1.1720126251203136d+22	!CLO_DUST
    get_imass(376) = 1.2993800064501876d+22	!H2SIO_DUST
    get_imass(377) = 1.0673621825297236d+22	!HCCP_DUST
    get_imass(378) = 1.660341172824015d+22	!HCL_DUST
    get_imass(379) = 1.4578495697237486d+22	!HCSI_DUST
    get_imass(380) = 1.3900430995903155d+22	!HNSI_DUST
    get_imass(381) = 1.4231495767062984d+22	!SIN_DUST
    get_imass(382) = 1.245255879618011d+22	!HPO_DUST
    get_imass(383) = 1.2717590353376075d+22	!PO_DUST
    get_imass(384) = 1.3584609595832849d+22	!N2O_DUST
    get_imass(385) = 1.4231286648577515d+22	!NH2CN_DUST
    get_imass(386) = 1.2993974396014029d+22	!NO2_DUST
    get_imass(387) = 1.8678838194270166d+22	!PH_DUST
    get_imass(388) = 1.8112643424848093d+22	!PH2_DUST
    get_imass(389) = 1.3282820467273226d+22	!PN_DUST
    get_imass(390) = 1.1494669658012411d+22	!SIC2_DUST
    get_imass(391) = 1.1277723436575881d+22	!SIC2H_DUST
    get_imass(392) = 1.1068814647971858d+22	!SIC2H2_DUST
    get_imass(393) = 9.339419097135083d+21	!SIC3_DUST
    get_imass(394) = 9.195692070945122d+21	!SIC3H_DUST
    get_imass(395) = 7.864773976534807d+21	!SIC4_DUST
    get_imass(396) = 1.4231286648577515d+22	!SICH2_DUST
    get_imass(397) = 1.3900231493555897d+22	!SICH3_DUST
    get_imass(398) = 1.10689411521601d+22	!SINC_DUST
    get_imass(399) = 9.962047036944089d+21	!SIO2_DUST
    get_imass(400) = 4.981249537914642d+22	!C+
    get_imass(401) = 1.707821117424883d+22	!CL+
    get_imass(402) = 1.086797819315049d+22	!FE+
    get_imass(403) = 5.978638635046188d+23	!H+
    get_imass(404) = 1.494510491502214d+23	!HE+
    get_imass(405) = 2.4905682628147157d+22	!MG+
    get_imass(406) = 4.269614784143167d+22	!N+
    get_imass(407) = 2.5988912718978482d+22	!NA+
    get_imass(408) = 3.7358947733482775d+22	!O+
    get_imass(409) = 1.9281911967941646d+22	!P+
    get_imass(410) = 1.8679156025097617d+22	!S+
    get_imass(411) = 2.1347658777582276d+22	!SI+
    get_imass(412) = 2.1347658777582276d+22	!CO+
    get_imass(413) = 2.988505522030552d+23	!H2+
    get_imass(414) = 1.9924455695150953d+22	!NO+
    get_imass(415) = 1.8679156025097617d+22	!O2+
    get_imass(416) = 4.269426568397541d+22	!CH2+
    get_imass(417) = 1.7580045431020288d+22	!H2S+
    get_imass(418) = 2.061129946901891d+22	!HCO+
    get_imass(419) = 1.3282799016544876d+22	!HCS+
    get_imass(420) = 1.9281528093448685d+22	!HNO+
    get_imass(421) = 3.7357506712751116d+22	!NH2+
    get_imass(422) = 9.962137441439364d+21	!OCS+
    get_imass(423) = 2.2989275059992396d+22	!C2H2+
    get_imass(424) = 3.98471753627902d+22	!CH3+
    get_imass(425) = 3.5159377509964384d+22	!NH3+
    get_imass(426) = 1.4231471142842933d+22	!C2H2O+
    get_imass(427) = 1.299395386806946d+22	!CH2O2+
    get_imass(428) = 1.457846985757746d+22	!C2H3N+
    get_imass(429) = 2.1346717735454165d+22	!C2H4+
    get_imass(430) = 1.1954439069478429d+22	!C4H2+
    get_imass(431) = 1.9281144234240167d+22	!H3CO+
    get_imass(432) = 1.8678435539250875d+22	!CH4O+
    get_imass(433) = 1.3584396620025083d+22	!C2H4O+
    get_imass(434) = 1.4942812855092753d+22	!C3H4+
    get_imass(435) = 1.928076039031519d+22	!CH5N+
    get_imass(436) = 1.2993605210824486d+22	!C2H5OH+
    get_imass(437) = 1.2993605210824486d+22	!CH3OCH3+
    get_imass(438) = 4.597951301414432d+22	!CH+
    get_imass(439) = 1.2717737687587748d+22	!CCL+
    get_imass(440) = 2.4905682628147157d+22	!C2+
    get_imass(441) = 1.1720251380246835d+22	!CLO+
    get_imass(442) = 1.3900806524540669d+22	!CP+
    get_imass(443) = 1.3584777703920136d+22	!CS+
    get_imass(444) = 2.298982076579595d+22	!CN+
    get_imass(445) = 1.299412820370868d+22	!NS+
    get_imass(446) = 1.8679156025097617d+22	!PH+
    get_imass(447) = 1.2717737687587746d+22	!PO+
    get_imass(448) = 1.4943273966453471d+22	!SIC+
    get_imass(449) = 1.42316802667505d+22	!SIN+
    get_imass(450) = 9.962137441439364d+21	!SIS+
    get_imass(451) = 1.2452700053524454d+22	!SO+
    get_imass(452) = 1.6603662853357426d+22	!C3+
    get_imass(453) = 1.0673725606035108d+22	!C2S+
    get_imass(454) = 1.4943273966453471d+22	!C2O+
    get_imass(455) = 1.0867856240749847d+22	!CCP+
    get_imass(456) = 2.3909138503618966d+22	!C2H+
    get_imass(457) = 2.061129946901891d+22	!HOC+
    get_imass(458) = 1.5729773339422048d+22	!C2N+
    get_imass(459) = 1.5729773339422048d+22	!CNC+
    get_imass(460) = 1.3584777703920136d+22	!HCP+
    get_imass(461) = 1.1494790019194873d+22	!SIC2+
    get_imass(462) = 1.1069052762760633d+22	!SINC+
    get_imass(463) = 1.2452700053524454d+22	!HPO+
    get_imass(464) = 2.2138075732642772d+22	!HCN+
    get_imass(465) = 1.457868930381256d+22	!CHSI+
    get_imass(466) = 1.992404581045028d+22	!SIH2+
    get_imass(467) = 1.6154773872118367d+22	!C3H+
    get_imass(468) = 1.2452700053524454d+22	!C4+
    get_imass(469) = 1.1494790019194873d+22	!C3O+
    get_imass(470) = 8.790111887233436d+21	!C3S+
    get_imass(471) = 1.992404581045028d+22	!H2CO+
    get_imass(472) = 1.299395386806946d+22	!H2SIO+
    get_imass(473) = 2.1347188246147295d+22	!HCNH+
    get_imass(474) = 1.1277839297303294d+22	!SIC2H+
    get_imass(475) = 9.339498554165942d+21	!SIC3+
    get_imass(476) = 1.4231471142842933d+22	!CH2SI+
    get_imass(477) = 1.9281144234240167d+22	!SIH3+
    get_imass(478) = 1.4943043407215877d+22	!C2H2N+
    get_imass(479) = 2.2137569712830467d+22	!C2H3+
    get_imass(480) = 1.5729517871964172d+22	!C3H2+
    get_imass(481) = 1.5729517871964172d+22	!H2C3+
    get_imass(482) = 1.2198483672348859d+22	!C4H+
    get_imass(483) = 9.962137441439366d+21	!C5+
    get_imass(484) = 7.471586130121288d+21	!C4S+
    get_imass(485) = 1.0673725606035106d+22	!PC2H+
    get_imass(486) = 1.1954586627042385d+22	!C3N+
    get_imass(487) = 9.6407753469094d+21	!C4N+
    get_imass(488) = 1.1720109550832863d+22	!C3HN+
    get_imass(489) = 2.2138075732642772d+22	!HNC+
    get_imass(490) = 9.195769101209189d+21	!SIC3H+
    get_imass(491) = 7.864830322719227d+21	!SIC4+
    get_imass(492) = 1.1068926256021254d+22	!SIC2H2+
    get_imass(493) = 1.39004075040143d+22	!SICH3+
    get_imass(494) = 1.1494653593981857d+22	!HC2NCH+
    get_imass(495) = 1.5326076388075666d+22	!C3H3+
    get_imass(496) = 1.5326076388075666d+22	!H3C3+
    get_imass(497) = 9.798772685776888d+21	!C5H+
    get_imass(498) = 8.301768644924616d+21	!C6+
    get_imass(499) = 1.39004075040143d+22	!C2H3O+
    get_imass(500) = 2.061042222590374d+22	!C2H5+
    get_imass(501) = 1.1277707973196044d+22	!C3H3N+
    get_imass(502) = 9.640679380927054d+21	!C5H2+
    get_imass(503) = 1.1719967724851471d+22	!C4H3+
    get_imass(504) = 8.188010326568945d+21	!C6H+
    get_imass(505) = 7.115794008154386d+21	!C7+
    get_imass(506) = 1.992363594261351d+22	!CH4N+
    get_imass(507) = 7.969662697725696d+21	!C5HN+
    get_imass(508) = 7.032052719283226d+21	!C7H+
    get_imass(509) = 6.226314712225845d+21	!C8+
    get_imass(510) = 9.961932502808633d+21	!COOCH4+
    get_imass(511) = 1.3282434686492177d+22	!C2H5O+
    get_imass(512) = 6.162105937227404d+21	!C8H+
    get_imass(513) = 5.534498478589509d+21	!C9+
    get_imass(514) = 9.487606429481972d+21	!C5H3+
    get_imass(515) = 8.077327503081644d+21	!C6H2+
    get_imass(516) = 7.969597117332096d+21	!C6H3+
    get_imass(517) = 1.0305333676608854d+22	!C2H6CO+
    get_imass(518) = 5.483707485054679d+21	!C9H+
    get_imass(519) = 4.981046119493312d+21	!C10+
    get_imass(520) = 6.870347159655569d+21	!C7H3+
    get_imass(521) = 6.099207948778022d+21	!C8H2+
    get_imass(522) = 6.037581013969156d+21	!C8H3+
    get_imass(523) = 1.6603662853357426d+22	!HCL+
    get_imass(524) = 1.8112942279313536d+22	!HS+
    get_imass(525) = 3.984881485992217d+22	!NH+
    get_imass(526) = 3.516065393652204d+22	!OH+
    get_imass(527) = 1.328298118906636d+22	!PN+
    get_imass(528) = 9.339498554165942d+21	!S2+
    get_imass(529) = 2.0611299469018905d+22	!SIH+
    get_imass(530) = 1.3584777703920136d+22	!SIO+
    get_imass(531) = 3.320668939159153d+22	!H2O+
    get_imass(532) = 1.3900607011413987d+22	!HNSI+
    get_imass(533) = 9.195769101209189d+21	!S2H+
    get_imass(534) = 1.8112942279313536d+22	!PH2+
    get_imass(535) = 1.299395386806946d+22	!H2CS+
    get_imass(536) = 9.056396425424032d+21	!H2S2+
    get_imass(537) = 1.3282799016544876d+22	!HSIO+
    get_imass(538) = 7.566193375911724d+21	!C4P+
    get_imass(539) = 1.3282799016544876d+22	!HCO2+
    get_imass(540) = 1.299395386806946d+22	!PCH3+
    get_imass(541) = 3.735606580318216d+22	!CH4+
    get_imass(542) = 1.5326318918683255d+22	!C2NH+
    get_imass(543) = 1.8678435539250875d+22	!SIH4+
    get_imass(544) = 3.3205550889113676d+22	!NH4+
    get_imass(545) = 2.1347188246147295d+22	!H2NC+
    get_imass(546) = 1.1494653593981855d+22	!C3H2N+
    get_imass(547) = 6.950259501550686d+21	!C7H2+
    get_imass(548) = 9.339318432295257d+21	!C5H4+
    get_imass(549) = 6.03761865195378d+21	!C7HN+
    get_imass(550) = 5.433840248484327d+21	!C9H2+
    get_imass(551) = 6.792251553547106d+21	!C7H4+
    get_imass(552) = 4.859544460369682d+21	!C9HN+
    get_imass(553) = 2.1347658777582276d+22	!N2+
    get_imass(554) = 1.3584777703920136d+22	!CO2+
    get_imass(555) = 1.1955020503006603d+23	!HEH+
    get_imass(556) = 9.339498554165942d+21	!SO2+
    get_imass(557) = 7.762533402863372d+21	!C6H5+
    get_imass(558) = 9.195594480569398d+21	!C5H5+
    get_imass(559) = 2.0611299469018905d+22	!N2H+
    get_imass(560) = 1.299412820370868d+22	!NO2+
    get_imass(561) = 1.0486408702661585d+22	!PC2H2+
    get_imass(562) = 1.2717570689409328d+22	!PNH2+
    get_imass(563) = 1.3282799016544876d+22	!PCH2+
    get_imass(564) = 1.0486408702661585d+22	!HC2S+
    get_imass(565) = 8.662679214754408d+21	!HC3S+
    get_imass(566) = 1.2717403695616602d+22	!H3CS+
    get_imass(567) = 7.379315593350717d+21	!HC4S+
    get_imass(568) = 1.3584587159300005d+22	!SINH2+
    get_imass(569) = 1.086761234415913d+22	!SIC2H3+
    get_imass(570) = 9.056396425424032d+21	!SIC3H2+
    get_imass(571) = 1.457868930381256d+22	!C2HO+
    get_imass(572) = 3.1458464665581883d+22	!H3O+
    get_imass(573) = 1.707760889604171d+22	!H3S+
    get_imass(574) = 9.798772685776888d+21	!HOCS+
    get_imass(575) = 1.8112264810249224d+22	!CH5O+
    get_imass(576) = 1.42316802667505d+22	!NCO+
    get_imass(577) = 1.390060701141399d+22	!HNCO+
    get_imass(578) = 1.1494790019194873d+22	!C2N2+
    get_imass(579) = 1.9921562368806946d+23	!H3+
    get_imass(580) = 1.8112942279313536d+22	!O2H+
    get_imass(581) = 3.5158101176078804d+22	!CH5+
    get_imass(582) = 1.6154773872118367d+22	!H2CL+
    get_imass(583) = 1.2717403695616602d+22	!CH3O2+
    get_imass(584) = 1.2198483672348857d+22	!H2PO+
    get_imass(585) = 1.2452539943257557d+22	!PNH3+
    get_imass(586) = 1.2717403695616602d+22	!PCH4+
    get_imass(587) = 1.0305552987937137d+22	!PC2H3+
    get_imass(588) = 9.798772685776888d+21	!HSIS+
    get_imass(589) = 1.2198483672348857d+22	!HSO+
    get_imass(590) = 1.271757068940933d+22	!HNS+
    get_imass(591) = 1.299412820370868d+22	!HPN+
    get_imass(592) = 1.8678795775226533d+22	!H2NO+
    get_imass(593) = 1.457868930381256d+22	!NAH2O+
    get_imass(594) = 1.7580045431020288d+22	!PH3+
    get_imass(595) = 1.0867734291086091d+22	!SINCH+
    get_imass(596) = 9.798772685776888d+21	!HSIO2+
    get_imass(597) = 9.195769101209189d+21	!HSO2+
    get_imass(598) = 1.1277839297303294d+22	!HC3O+
    get_imass(599) = 8.790111887233436d+21	!PC3H+
    get_imass(600) = 8.921185388148404d+21	!H3S2+
    get_imass(601) = 1.2717403695616602d+22	!H3SIO+
    get_imass(602) = 7.471586130121288d+21	!PC4H+
    get_imass(603) = 1.39004075040143d+22	!NH2CNH+
    get_imass(604) = 7.762657837599731d+21	!SIC4H+
    get_imass(605) = 1.358439662002508d+22	!SICH4+
    get_imass(606) = 1.8112264810249224d+22	!SIH5+
    get_imass(607) = 1.4231262025081118d+22	!C2H4N+
    get_imass(608) = 1.2993779537108128d+22	!NH2CH2O+
    get_imass(609) = 1.9923226091639592d+22	!C2H6+
    get_imass(610) = 1.10687997521735d+22	!C3H4N+
    get_imass(611) = 1.4578250417948708d+22	!C3H5+
    get_imass(612) = 1.1494517172007107d+22	!C4H4+
    get_imass(613) = 1.867807531716985d+22	!CH6N+
    get_imass(614) = 7.864766456251255d+21	!C5H2N+
    get_imass(615) = 9.05631174081267d+21	!C4H4N+
    get_imass(616) = 9.798574413367424d+21	!H5C2O2+
    get_imass(617) = 1.2717069721187526d+22	!C2H5OH2+
    get_imass(618) = 1.2717069721187526d+22	!CH3OCH4+
    get_imass(619) = 5.977223878449676d+21	!C7H2N+
    get_imass(620) = 1.0130617889673133d+22	!C3H6OH+
    get_imass(621) = 6.641313787040772d+21	!C6H4N+
    get_imass(622) = 4.93986766590425d+21	!C10H+
    get_imass(623) = 5.384871794855659d+21	!C9H3+
    get_imass(624) = 6.715911426730638d+21	!C7H5+
    get_imass(625) = 5.243150923943091d+21	!C8H4N+
    get_imass(626) = 4.820342515458516d+21	!C9H2N+
    get_imass(627) = 7.5659569478595d+21	!C6H7+
    get_imass(628) = 2.3909138503618966d+22	!NAH2+
    get_imass(629) = 1.0130829827590018d+22	!PC2H4+
    get_imass(630) = 1.1277576652147145d+22	!C4H5+
    get_imass(631) = 1.2198483672348859d+22	!H2CCL+
    get_imass(632) = 7.379315593350717d+21	!PC4H2+
    get_imass(633) = 7.864702590820532d+21	!C6H4+
    get_imass(634) = 5.977186989688067d+21	!C8H4+
    get_imass(635) = 5.336778042345d+21	!C9H4+
    get_imass(636) = 1.0867368458515227d+22	!C4H7+
    get_imass(637) = 9.487699372182358d+21	!HC4N+
    get_imass(638) = 9.195769101209189d+21	!HC4O+
    get_imass(639) = 8.077394868454343d+21	!C5N+
    get_imass(640) = 9.339408492362131d+21	!H2C4N+
    get_imass(641) = 9.195681790060306d+21	!H3C4N+
    get_imass(642) = 6.099246359045085d+21	!C7N+
    get_imass(643) = 7.762595619732879d+21	!C5H3N+
    get_imass(644) = 4.899364476838032d+21	!C10H2+
    get_imass(645) = 4.899389261294865d+21	!C9N+
    get_imass(646) = 5.91802540558059d+21	!C7H3N+
    get_imass(647) = 4.781767993303164d+21	!C9H3N+
    get_imass(648) = 9.640679380927054d+21	!OCS+H2
    get_imass(649) = 1.1068926256021254d+22	!H2C3O+
    get_imass(650) = 1.086761234415913d+22	!H3C3O+
    get_imass(651) = 7.663045333484011d+21	!C5H4N+
    get_imass(652) = 5.917989243891554d+21	!C8H5+
    get_imass(653) = 5.289535761838126d+21	!C9H5+
    get_imass(654) = 9.798574413367424d+21	!H2COHOCH2+
    get_imass(655) = 9.487420549543957d+21	!H7C2O2+
    get_imass(656) = 0d0	!CR
    get_imass(657) = 0d0	!g
    get_imass(658) = 0d0	!Tgas
    get_imass(659) = 0d0	!dummy

  end function get_imass

  !************************
  !species binding energies (surface=BARE), K
  function get_EbindBare()
    use krome_commons
    implicit none
    real*8::get_EbindBare(nspec)

    get_EbindBare(:) = 1d99

    get_EbindBare(idx_C) = 800.0d0
    get_EbindBare(idx_FE) = 4200.0d0
    get_EbindBare(idx_H) = 450.0d0
    get_EbindBare(idx_HE) = 200.0d0
    get_EbindBare(idx_MG) = 5300.0d0
    get_EbindBare(idx_N) = 800.0d0
    get_EbindBare(idx_NA) = 11800.0d0
    get_EbindBare(idx_O) = 800.0d0
    get_EbindBare(idx_S) = 1100.0d0
    get_EbindBare(idx_SI) = 2700.0d0
    get_EbindBare(idx_C2) = 1600.0d0
    get_EbindBare(idx_CH) = 925.0d0
    get_EbindBare(idx_CN) = 1600.0d0
    get_EbindBare(idx_CO) = 1150.0d0
    get_EbindBare(idx_CS) = 1900.0d0
    get_EbindBare(idx_H2) = 23.0d0
    get_EbindBare(idx_HS) = 1450.0d0
    get_EbindBare(idx_MGH) = 5750.0d0
    get_EbindBare(idx_N2) = 1000.0d0
    get_EbindBare(idx_NAH) = 12300.0d0
    get_EbindBare(idx_NH) = 2380.0d0
    get_EbindBare(idx_NO) = 1600.0d0
    get_EbindBare(idx_NS) = 1900.0d0
    get_EbindBare(idx_O2) = 1000.0d0
    get_EbindBare(idx_OH) = 2850.0d0
    get_EbindBare(idx_S2) = 2200.0d0
    get_EbindBare(idx_SIC) = 3500.0d0
    get_EbindBare(idx_SIH) = 3150.0d0
    get_EbindBare(idx_SIO) = 3500.0d0
    get_EbindBare(idx_SIS) = 3800.0d0
    get_EbindBare(idx_SO) = 2600.0d0
    get_EbindBare(idx_C2H) = 2140.0d0
    get_EbindBare(idx_C2N) = 2400.0d0
    get_EbindBare(idx_C2S) = 2700.0d0
    get_EbindBare(idx_C3) = 2400.0d0
    get_EbindBare(idx_CCO) = 1950.0d0
    get_EbindBare(idx_CH2) = 1050.0d0
    get_EbindBare(idx_CO2) = 2580.0d0
    get_EbindBare(idx_H2O) = 5700.0d0
    get_EbindBare(idx_H2S) = 2740.0d0
    get_EbindBare(idx_HCN) = 2050.0d0
    get_EbindBare(idx_HCO) = 1600.0d0
    get_EbindBare(idx_HCS) = 2350.0d0
    get_EbindBare(idx_HNC) = 2050.0d0
    get_EbindBare(idx_HNO) = 2050.0d0
    get_EbindBare(idx_HS2) = 2650.0d0
    get_EbindBare(idx_NAOH) = 14700.0d0
    get_EbindBare(idx_NH2) = 3960.0d0
    get_EbindBare(idx_O2H) = 3650.0d0
    get_EbindBare(idx_OCN) = 2400.0d0
    get_EbindBare(idx_OCS) = 2890.0d0
    get_EbindBare(idx_SIH2) = 3600.0d0
    get_EbindBare(idx_SO2) = 3410.0d0
    get_EbindBare(idx_C2H2) = 2590.0d0
    get_EbindBare(idx_C3H) = 2940.0d0
    get_EbindBare(idx_C3N) = 3200.0d0
    get_EbindBare(idx_C3O) = 2750.0d0
    get_EbindBare(idx_C3S) = 3500.0d0
    get_EbindBare(idx_C4) = 3200.0d0
    get_EbindBare(idx_CH3) = 1180.0d0
    get_EbindBare(idx_H2CO) = 2050.0d0
    get_EbindBare(idx_H2CS) = 2700.0d0
    get_EbindBare(idx_H2O2) = 5700.0d0
    get_EbindBare(idx_H2S2) = 3100.0d0
    get_EbindBare(idx_NH3) = 5530.0d0
    get_EbindBare(idx_SIH3) = 4050.0d0
    get_EbindBare(idx_C2H2N) = 4230.0d0
    get_EbindBare(idx_C2H2O) = 2200.0d0
    get_EbindBare(idx_C2H3) = 3040.0d0
    get_EbindBare(idx_C3H2) = 3390.0d0
    get_EbindBare(idx_C4H) = 3740.0d0
    get_EbindBare(idx_C4N) = 4000.0d0
    get_EbindBare(idx_C4S) = 4300.0d0
    get_EbindBare(idx_C5) = 4000.0d0
    get_EbindBare(idx_CH2O2) = 5570.0d0
    get_EbindBare(idx_CH3N) = 2850.0d0
    get_EbindBare(idx_CH4) = 1300.0d0
    get_EbindBare(idx_HC3N) = 4580.0d0
    get_EbindBare(idx_SIH4) = 4500.0d0
    get_EbindBare(idx_C2H3N) = 4680.0d0
    get_EbindBare(idx_C2H4) = 3490.0d0
    get_EbindBare(idx_C3H3) = 3840.0d0
    get_EbindBare(idx_C4H2) = 4190.0d0
    get_EbindBare(idx_C5H) = 4540.0d0
    get_EbindBare(idx_C5N) = 4800.0d0
    get_EbindBare(idx_C6) = 4800.0d0
    get_EbindBare(idx_CH4O) = 5530.0d0
    get_EbindBare(idx_C2H4O) = 2450.0d0
    get_EbindBare(idx_C2H5) = 3940.0d0
    get_EbindBare(idx_C3H3N) = 5480.0d0
    get_EbindBare(idx_C3H4) = 4290.0d0
    get_EbindBare(idx_C5H2) = 4990.0d0
    get_EbindBare(idx_C6H) = 5340.0d0
    get_EbindBare(idx_C7) = 5600.0d0
    get_EbindBare(idx_CH5N) = 6580.0d0
    get_EbindBare(idx_HC5N) = 6180.0d0
    get_EbindBare(idx_C6H2) = 5790.0d0
    get_EbindBare(idx_C7H) = 6140.0d0
    get_EbindBare(idx_C7N) = 6400.0d0
    get_EbindBare(idx_C8) = 6400.0d0
    get_EbindBare(idx_CH3C3N) = 6480.0d0
    get_EbindBare(idx_HCOOCH3) = 6300.0d0
    get_EbindBare(idx_C2H5OH) = 6580.0d0
    get_EbindBare(idx_C7H2) = 6590.0d0
    get_EbindBare(idx_C8H) = 6940.0d0
    get_EbindBare(idx_C9) = 7200.0d0
    get_EbindBare(idx_CH3C4H) = 5890.0d0
    get_EbindBare(idx_CH3OCH3) = 3150.0d0
    get_EbindBare(idx_HC7N) = 7780.0d0
    get_EbindBare(idx_C8H2) = 7390.0d0
    get_EbindBare(idx_C9H) = 7740.0d0
    get_EbindBare(idx_C9N) = 8000.0d0
    get_EbindBare(idx_C10) = 8000.0d0
    get_EbindBare(idx_CH3C5N) = 7880.0d0
    get_EbindBare(idx_C9H2) = 8190.0d0
    get_EbindBare(idx_CH3C6H) = 7490.0d0
    get_EbindBare(idx_CH3C7N) = 9480.0d0
    get_EbindBare(idx_HC9N) = 9380.0d0
    get_EbindBare(idx_C4H4) = 5090.0d0
    get_EbindBare(idx_HCNC2) = 4580.0d0
    get_EbindBare(idx_HC2NC) = 4580.0d0
    get_EbindBare(idx_HNC3) = 4580.0d0
    get_EbindBare(idx_NH2CHO) = 5560.0d0
    get_EbindBare(idx_C4H3) = 4640.0d0
    get_EbindBare(idx_C6H6) = 7590.0d0
    get_EbindBare(idx_H2CN) = 2400.0d0
    get_EbindBare(idx_O3) = 1800.0d0
    get_EbindBare(idx_FEH) = 4650.0d0
    get_EbindBare(idx_HNCO) = 2850.0d0
    get_EbindBare(idx_HC2O) = 2400.0d0
    get_EbindBare(idx_HCCN) = 3780.0d0
    get_EbindBare(idx_HC3O) = 3200.0d0
    get_EbindBare(idx_MGH2) = 6200.0d0
    get_EbindBare(idx_N2H2) = 4760.0d0
    get_EbindBare(idx_CHNH) = 3300.0d0
    get_EbindBare(idx_H2C3O) = 3650.0d0
    get_EbindBare(idx_H2C3N) = 5030.0d0
    get_EbindBare(idx_H2C5N) = 6630.0d0
    get_EbindBare(idx_H2C7N) = 8230.0d0
    get_EbindBare(idx_H2C9N) = 9830.0d0
    get_EbindBare(idx_NH2OH) = 6810.0d0
    get_EbindBare(idx_CH2OH) = 5080.0d0
    get_EbindBare(idx_C5H3) = 5440.0d0
    get_EbindBare(idx_H3C5N) = 7080.0d0
    get_EbindBare(idx_C6H3) = 6240.0d0
    get_EbindBare(idx_C7H3) = 7040.0d0
    get_EbindBare(idx_H3C7N) = 8680.0d0
    get_EbindBare(idx_C8H3) = 7840.0d0
    get_EbindBare(idx_C9H3) = 8640.0d0
    get_EbindBare(idx_H3C9N) = 10300.0d0
    get_EbindBare(idx_CH3NH) = 3550.0d0
    get_EbindBare(idx_H4C3N) = 5930.0d0
    get_EbindBare(idx_C5H4) = 5890.0d0
    get_EbindBare(idx_C6H4) = 6690.0d0
    get_EbindBare(idx_C7H4) = 7490.0d0
    get_EbindBare(idx_C8H4) = 8290.0d0
    get_EbindBare(idx_C9H4) = 9090.0d0
    get_EbindBare(idx_H5C3N) = 6380.0d0
    get_EbindBare(idx_C2H6) = 4390.0d0
    get_EbindBare(idx_CH2NH2) = 5010.0d0

  end function get_EbindBare

  !************************
  !species binding energies (surface=ICE), K
  function get_EbindIce()
    use krome_commons
    implicit none
    real*8::get_EbindIce(nspec)

    get_EbindIce(:) = 1d99

    get_EbindIce(idx_C) = 800.0d0
    get_EbindIce(idx_FE) = 4200.0d0
    get_EbindIce(idx_H) = 450.0d0
    get_EbindIce(idx_HE) = 200.0d0
    get_EbindIce(idx_MG) = 5300.0d0
    get_EbindIce(idx_N) = 800.0d0
    get_EbindIce(idx_NA) = 11800.0d0
    get_EbindIce(idx_O) = 800.0d0
    get_EbindIce(idx_S) = 1100.0d0
    get_EbindIce(idx_SI) = 2700.0d0
    get_EbindIce(idx_C2) = 1600.0d0
    get_EbindIce(idx_CH) = 925.0d0
    get_EbindIce(idx_CN) = 1600.0d0
    get_EbindIce(idx_CO) = 1150.0d0
    get_EbindIce(idx_CS) = 1900.0d0
    get_EbindIce(idx_H2) = 23.0d0
    get_EbindIce(idx_HS) = 1450.0d0
    get_EbindIce(idx_MGH) = 5750.0d0
    get_EbindIce(idx_N2) = 1000.0d0
    get_EbindIce(idx_NAH) = 12300.0d0
    get_EbindIce(idx_NH) = 2380.0d0
    get_EbindIce(idx_NO) = 1600.0d0
    get_EbindIce(idx_NS) = 1900.0d0
    get_EbindIce(idx_O2) = 1000.0d0
    get_EbindIce(idx_OH) = 2850.0d0
    get_EbindIce(idx_S2) = 2200.0d0
    get_EbindIce(idx_SIC) = 3500.0d0
    get_EbindIce(idx_SIH) = 3150.0d0
    get_EbindIce(idx_SIO) = 3500.0d0
    get_EbindIce(idx_SIS) = 3800.0d0
    get_EbindIce(idx_SO) = 2600.0d0
    get_EbindIce(idx_C2H) = 2140.0d0
    get_EbindIce(idx_C2N) = 2400.0d0
    get_EbindIce(idx_C2S) = 2700.0d0
    get_EbindIce(idx_C3) = 2400.0d0
    get_EbindIce(idx_CCO) = 1950.0d0
    get_EbindIce(idx_CH2) = 1050.0d0
    get_EbindIce(idx_CO2) = 2580.0d0
    get_EbindIce(idx_H2O) = 5700.0d0
    get_EbindIce(idx_H2S) = 2740.0d0
    get_EbindIce(idx_HCN) = 2050.0d0
    get_EbindIce(idx_HCO) = 1600.0d0
    get_EbindIce(idx_HCS) = 2350.0d0
    get_EbindIce(idx_HNC) = 2050.0d0
    get_EbindIce(idx_HNO) = 2050.0d0
    get_EbindIce(idx_HS2) = 2650.0d0
    get_EbindIce(idx_NAOH) = 14700.0d0
    get_EbindIce(idx_NH2) = 3960.0d0
    get_EbindIce(idx_O2H) = 3650.0d0
    get_EbindIce(idx_OCN) = 2400.0d0
    get_EbindIce(idx_OCS) = 2890.0d0
    get_EbindIce(idx_SIH2) = 3600.0d0
    get_EbindIce(idx_SO2) = 3410.0d0
    get_EbindIce(idx_C2H2) = 2590.0d0
    get_EbindIce(idx_C3H) = 2940.0d0
    get_EbindIce(idx_C3N) = 3200.0d0
    get_EbindIce(idx_C3O) = 2750.0d0
    get_EbindIce(idx_C3S) = 3500.0d0
    get_EbindIce(idx_C4) = 3200.0d0
    get_EbindIce(idx_CH3) = 1180.0d0
    get_EbindIce(idx_H2CO) = 2050.0d0
    get_EbindIce(idx_H2CS) = 2700.0d0
    get_EbindIce(idx_H2O2) = 5700.0d0
    get_EbindIce(idx_H2S2) = 3100.0d0
    get_EbindIce(idx_NH3) = 5530.0d0
    get_EbindIce(idx_SIH3) = 4050.0d0
    get_EbindIce(idx_C2H2N) = 4230.0d0
    get_EbindIce(idx_C2H2O) = 2200.0d0
    get_EbindIce(idx_C2H3) = 3040.0d0
    get_EbindIce(idx_C3H2) = 3390.0d0
    get_EbindIce(idx_C4H) = 3740.0d0
    get_EbindIce(idx_C4N) = 4000.0d0
    get_EbindIce(idx_C4S) = 4300.0d0
    get_EbindIce(idx_C5) = 4000.0d0
    get_EbindIce(idx_CH2O2) = 5570.0d0
    get_EbindIce(idx_CH3N) = 2850.0d0
    get_EbindIce(idx_CH4) = 1300.0d0
    get_EbindIce(idx_HC3N) = 4580.0d0
    get_EbindIce(idx_SIH4) = 4500.0d0
    get_EbindIce(idx_C2H3N) = 4680.0d0
    get_EbindIce(idx_C2H4) = 3490.0d0
    get_EbindIce(idx_C3H3) = 3840.0d0
    get_EbindIce(idx_C4H2) = 4190.0d0
    get_EbindIce(idx_C5H) = 4540.0d0
    get_EbindIce(idx_C5N) = 4800.0d0
    get_EbindIce(idx_C6) = 4800.0d0
    get_EbindIce(idx_CH4O) = 5530.0d0
    get_EbindIce(idx_C2H4O) = 2450.0d0
    get_EbindIce(idx_C2H5) = 3940.0d0
    get_EbindIce(idx_C3H3N) = 5480.0d0
    get_EbindIce(idx_C3H4) = 4290.0d0
    get_EbindIce(idx_C5H2) = 4990.0d0
    get_EbindIce(idx_C6H) = 5340.0d0
    get_EbindIce(idx_C7) = 5600.0d0
    get_EbindIce(idx_CH5N) = 6580.0d0
    get_EbindIce(idx_HC5N) = 6180.0d0
    get_EbindIce(idx_C6H2) = 5790.0d0
    get_EbindIce(idx_C7H) = 6140.0d0
    get_EbindIce(idx_C7N) = 6400.0d0
    get_EbindIce(idx_C8) = 6400.0d0
    get_EbindIce(idx_CH3C3N) = 6480.0d0
    get_EbindIce(idx_HCOOCH3) = 6300.0d0
    get_EbindIce(idx_C2H5OH) = 6580.0d0
    get_EbindIce(idx_C7H2) = 6590.0d0
    get_EbindIce(idx_C8H) = 6940.0d0
    get_EbindIce(idx_C9) = 7200.0d0
    get_EbindIce(idx_CH3C4H) = 5890.0d0
    get_EbindIce(idx_CH3OCH3) = 3150.0d0
    get_EbindIce(idx_HC7N) = 7780.0d0
    get_EbindIce(idx_C8H2) = 7390.0d0
    get_EbindIce(idx_C9H) = 7740.0d0
    get_EbindIce(idx_C9N) = 8000.0d0
    get_EbindIce(idx_C10) = 8000.0d0
    get_EbindIce(idx_CH3C5N) = 7880.0d0
    get_EbindIce(idx_C9H2) = 8190.0d0
    get_EbindIce(idx_CH3C6H) = 7490.0d0
    get_EbindIce(idx_CH3C7N) = 9480.0d0
    get_EbindIce(idx_HC9N) = 9380.0d0
    get_EbindIce(idx_C4H4) = 5090.0d0
    get_EbindIce(idx_HCNC2) = 4580.0d0
    get_EbindIce(idx_HC2NC) = 4580.0d0
    get_EbindIce(idx_HNC3) = 4580.0d0
    get_EbindIce(idx_NH2CHO) = 5560.0d0
    get_EbindIce(idx_C4H3) = 4640.0d0
    get_EbindIce(idx_C6H6) = 7590.0d0
    get_EbindIce(idx_H2CN) = 2400.0d0
    get_EbindIce(idx_O3) = 1800.0d0
    get_EbindIce(idx_FEH) = 4650.0d0
    get_EbindIce(idx_HNCO) = 2850.0d0
    get_EbindIce(idx_HC2O) = 2400.0d0
    get_EbindIce(idx_HCCN) = 3780.0d0
    get_EbindIce(idx_HC3O) = 3200.0d0
    get_EbindIce(idx_MGH2) = 6200.0d0
    get_EbindIce(idx_N2H2) = 4760.0d0
    get_EbindIce(idx_CHNH) = 3300.0d0
    get_EbindIce(idx_H2C3O) = 3650.0d0
    get_EbindIce(idx_H2C3N) = 5030.0d0
    get_EbindIce(idx_H2C5N) = 6630.0d0
    get_EbindIce(idx_H2C7N) = 8230.0d0
    get_EbindIce(idx_H2C9N) = 9830.0d0
    get_EbindIce(idx_NH2OH) = 6810.0d0
    get_EbindIce(idx_CH2OH) = 5080.0d0
    get_EbindIce(idx_C5H3) = 5440.0d0
    get_EbindIce(idx_H3C5N) = 7080.0d0
    get_EbindIce(idx_C6H3) = 6240.0d0
    get_EbindIce(idx_C7H3) = 7040.0d0
    get_EbindIce(idx_H3C7N) = 8680.0d0
    get_EbindIce(idx_C8H3) = 7840.0d0
    get_EbindIce(idx_C9H3) = 8640.0d0
    get_EbindIce(idx_H3C9N) = 10300.0d0
    get_EbindIce(idx_CH3NH) = 3550.0d0
    get_EbindIce(idx_H4C3N) = 5930.0d0
    get_EbindIce(idx_C5H4) = 5890.0d0
    get_EbindIce(idx_C6H4) = 6690.0d0
    get_EbindIce(idx_C7H4) = 7490.0d0
    get_EbindIce(idx_C8H4) = 8290.0d0
    get_EbindIce(idx_C9H4) = 9090.0d0
    get_EbindIce(idx_H5C3N) = 6380.0d0
    get_EbindIce(idx_C2H6) = 4390.0d0
    get_EbindIce(idx_CH2NH2) = 5010.0d0

  end function get_EbindIce

  !************************
  function get_kevap70()
    use krome_commons
    implicit none
    real*8::get_kevap70(nspec)

    get_kevap70(idx_E) = 0d0
    get_kevap70(idx_Hk) = 0d0
    get_kevap70(idx_Ck) = 0d0
    get_kevap70(idx_CNk) = 0d0
    get_kevap70(idx_Ok) = 0d0
    get_kevap70(idx_OHk) = 0d0
    get_kevap70(idx_Sk) = 0d0
    get_kevap70(idx_GRAINk) = 0d0
    get_kevap70(idx_C) = 10880140.221963316
    get_kevap70(idx_CL) = 0d0
    get_kevap70(idx_FE) = 8.75651076269652e-15
    get_kevap70(idx_H) = 1614755981.8205848
    get_kevap70(idx_HE) = 57432619267.61735
    get_kevap70(idx_MG) = 1.3113043836341294e-21
    get_kevap70(idx_N) = 10880140.221963316
    get_kevap70(idx_NA) = 6.171045732166068e-62
    get_kevap70(idx_O) = 10880140.221963316
    get_kevap70(idx_P) = 0d0
    get_kevap70(idx_S) = 149751.92964078687
    get_kevap70(idx_SI) = 1.7727251750583404e-05
    get_kevap70(idx_C2) = 118.37745124958396
    get_kevap70(idx_CCL) = 0d0
    get_kevap70(idx_CH) = 1824351.9784525777
    get_kevap70(idx_CLO) = 0d0
    get_kevap70(idx_CN) = 118.37745124958396
    get_kevap70(idx_CO) = 73309.80815820694
    get_kevap70(idx_CP) = 0d0
    get_kevap70(idx_CS) = 1.6293219930013456
    get_kevap70(idx_H2) = 719951501281.9313
    get_kevap70(idx_HCL) = 0d0
    get_kevap70(idx_HS) = 1009.0205649303961
    get_kevap70(idx_MGH) = 2.1174365974607613e-24
    get_kevap70(idx_N2) = 624874.9509463086
    get_kevap70(idx_NAH) = 4.878151934808065e-65
    get_kevap70(idx_NH) = 0.001713908431542013
    get_kevap70(idx_NO) = 118.37745124958396
    get_kevap70(idx_NS) = 1.6293219930013456
    get_kevap70(idx_O2) = 624874.9509463086
    get_kevap70(idx_OH) = 2.079746392521286e-06
    get_kevap70(idx_PH) = 0d0
    get_kevap70(idx_PN) = 0d0
    get_kevap70(idx_PO) = 0d0
    get_kevap70(idx_S2) = 0.022425640431139184
    get_kevap70(idx_SIC) = 1.9287498479639177e-10
    get_kevap70(idx_SIH) = 2.8625185805493938e-08
    get_kevap70(idx_SIN) = 0d0
    get_kevap70(idx_SIO) = 1.9287498479639177e-10
    get_kevap70(idx_SIS) = 2.6546901568778775e-12
    get_kevap70(idx_SO) = 7.397110404273267e-05
    get_kevap70(idx_C2H) = 0.052844192694200876
    get_kevap70(idx_C2N) = 0.0012879632687141022
    get_kevap70(idx_C2S) = 1.7727251750583404e-05
    get_kevap70(idx_C3) = 0.0012879632687141022
    get_kevap70(idx_CCO) = 0.7976209924065204
    get_kevap70(idx_CCP) = 0d0
    get_kevap70(idx_CH2) = 305902.3205018258
    get_kevap70(idx_CO2) = 9.843425040831194e-05
    get_kevap70(idx_H2O) = 4.325345057205303e-24
    get_kevap70(idx_H2S) = 1.0010900316948171e-05
    get_kevap70(idx_HCN) = 0.19115069751794067
    get_kevap70(idx_HCO) = 118.37745124958396
    get_kevap70(idx_HCP) = 0d0
    get_kevap70(idx_HCS) = 0.0026309574345107648
    get_kevap70(idx_HCSI) = 0d0
    get_kevap70(idx_HNC) = 0.19115069751794067
    get_kevap70(idx_HNO) = 0.19115069751794067
    get_kevap70(idx_HNSI) = 0d0
    get_kevap70(idx_HPO) = 0d0
    get_kevap70(idx_HS2) = 3.6211937032339616e-05
    get_kevap70(idx_N2O) = 0d0
    get_kevap70(idx_NAOH) = 6.282880511239463e-80
    get_kevap70(idx_NH2) = 2.6998568509080676e-13
    get_kevap70(idx_NO2) = 0d0
    get_kevap70(idx_O2H) = 2.262793237675391e-11
    get_kevap70(idx_OCN) = 0.0012879632687141022
    get_kevap70(idx_OCS) = 1.1744704770370351e-06
    get_kevap70(idx_PH2) = 0d0
    get_kevap70(idx_SIC2) = 0d0
    get_kevap70(idx_SIH2) = 4.6222690010146945e-11
    get_kevap70(idx_SINC) = 0d0
    get_kevap70(idx_SIO2) = 0d0
    get_kevap70(idx_SO2) = 6.976771902059595e-10
    get_kevap70(idx_C2H2) = 8.533047625744065e-05
    get_kevap70(idx_C3H) = 5.749522264293559e-07
    get_kevap70(idx_C3N) = 1.4013220964347626e-08
    get_kevap70(idx_C3O) = 8.678228241364494e-06
    get_kevap70(idx_C3P) = 0d0
    get_kevap70(idx_C3S) = 1.9287498479639177e-10
    get_kevap70(idx_C4) = 1.4013220964347626e-08
    get_kevap70(idx_CH3) = 47756.87233436472
    get_kevap70(idx_H2CO) = 0.19115069751794067
    get_kevap70(idx_H2CS) = 1.7727251750583404e-05
    get_kevap70(idx_H2O2) = 4.325345057205303e-24
    get_kevap70(idx_H2S2) = 5.847344193628064e-08
    get_kevap70(idx_H2SIO) = 0d0
    get_kevap70(idx_HCCP) = 0d0
    get_kevap70(idx_NH3) = 4.906094730649281e-23
    get_kevap70(idx_SIC2H) = 0d0
    get_kevap70(idx_SIC3) = 0d0
    get_kevap70(idx_SICH2) = 0d0
    get_kevap70(idx_SIH3) = 7.463836518972376e-14
    get_kevap70(idx_C2H2N) = 5.704333118511554e-15
    get_kevap70(idx_C2H2O) = 0.022425640431139184
    get_kevap70(idx_C2H3) = 1.3778789696830142e-07
    get_kevap70(idx_C3H2) = 9.284075468878642e-10
    get_kevap70(idx_C4H) = 6.255560844481385e-12
    get_kevap70(idx_C4N) = 1.5246580905345788e-13
    get_kevap70(idx_C4P) = 0d0
    get_kevap70(idx_C4S) = 2.0985068798937815e-15
    get_kevap70(idx_C5) = 1.5246580905345788e-13
    get_kevap70(idx_CH2O2) = 2.7705606026844305e-23
    get_kevap70(idx_CH2PH) = 0d0
    get_kevap70(idx_CH3N) = 2.079746392521286e-06
    get_kevap70(idx_CH4) = 8600.645559650315
    get_kevap70(idx_HC3N) = 3.843549421765877e-17
    get_kevap70(idx_SIC2H2) = 0d0
    get_kevap70(idx_SIC3H) = 0d0
    get_kevap70(idx_SIC4) = 0d0
    get_kevap70(idx_SICH3) = 0d0
    get_kevap70(idx_SIH4) = 1.2052274666341467e-16
    get_kevap70(idx_C2H3N) = 9.211106025413786e-18
    get_kevap70(idx_C2H4) = 2.2249383085204434e-10
    get_kevap70(idx_C3H3) = 1.4991516399045618e-12
    get_kevap70(idx_C4H2) = 1.0101204293268997e-14
    get_kevap70(idx_C5H) = 6.806137915498059e-17
    get_kevap70(idx_C5N) = 1.6588493815567146e-18
    get_kevap70(idx_C6) = 1.6588493815567146e-18
    get_kevap70(idx_CH4O) = 4.906094730649281e-23
    get_kevap70(idx_C2H4O) = 0.0006305116760146989
    get_kevap70(idx_C2H5) = 3.5927324428651533e-13
    get_kevap70(idx_C3H3N) = 1.0021812515587301e-22
    get_kevap70(idx_C3H4) = 2.420764078192026e-15
    get_kevap70(idx_C5H2) = 1.0990251912146357e-19
    get_kevap70(idx_C6H) = 7.405173489063987e-22
    get_kevap70(idx_C7) = 1.8048513878454152e-23
    get_kevap70(idx_CH5N) = 1.500785762707395e-29
    get_kevap70(idx_HC5N) = 4.5498958430045514e-27
    get_kevap70(idx_C6H2) = 1.1957548187885431e-24
    get_kevap70(idx_C7H) = 8.056932592898252e-27
    get_kevap70(idx_C7N) = 1.9637036179563177e-28
    get_kevap70(idx_C8) = 1.9637036179563177e-28
    get_kevap70(idx_CH3C3N) = 6.262379604070772e-29
    get_kevap70(idx_HCOOCH3) = 8.194012623990515e-28
    get_kevap70(idx_C2H5OH) = 1.500785762707395e-29
    get_kevap70(idx_C7H2) = 1.300998009950766e-29
    get_kevap70(idx_C8H) = 8.76605563696393e-32
    get_kevap70(idx_C9) = 2.1365370717741383e-33
    get_kevap70(idx_CH3C4H) = 2.865638816529216e-25
    get_kevap70(idx_CH3OCH3) = 2.8625185805493938e-08
    get_kevap70(idx_HC7N) = 5.3860507334596165e-37
    get_kevap70(idx_C2H6CO) = 0d0
    get_kevap70(idx_C8H2) = 1.4155040776759537e-34
    get_kevap70(idx_C9H) = 9.537591452369934e-37
    get_kevap70(idx_C9N) = 2.3245822930325483e-38
    get_kevap70(idx_C10) = 2.3245822930325483e-38
    get_kevap70(idx_CH3C5N) = 1.2907726406015813e-37
    get_kevap70(idx_C9H2) = 1.5400882849875202e-39
    get_kevap70(idx_CH3C6H) = 3.392270193026015e-35
    get_kevap70(idx_CH3C7N) = 1.5279837533711204e-47
    get_kevap70(idx_HC9N) = 6.375869581278994e-47
    get_kevap70(idx_C4H4) = 2.6338252615021148e-20
    get_kevap70(idx_HCNC2) = 3.843549421765877e-17
    get_kevap70(idx_HC2NC) = 3.843549421765877e-17
    get_kevap70(idx_HNC3) = 3.843549421765877e-17
    get_kevap70(idx_NH2CHO) = 3.196021727492239e-23
    get_kevap70(idx_C4H3) = 1.631098005614774e-17
    get_kevap70(idx_NH2CN) = 0d0
    get_kevap70(idx_C6H6) = 8.129610676492257e-36
    get_kevap70(idx_H2CN) = 0.0012879632687141022
    get_kevap70(idx_GRAIN0) = 0d0
    get_kevap70(idx_O3) = 6.7987270874882855
    get_kevap70(idx_FEH) = 1.413962813394051e-17
    get_kevap70(idx_HNCO) = 2.079746392521286e-06
    get_kevap70(idx_HC2O) = 0.0012879632687141022
    get_kevap70(idx_HCCN) = 3.532628572200807e-12
    get_kevap70(idx_HC3O) = 1.4013220964347626e-08
    get_kevap70(idx_MGH2) = 3.419143411875584e-27
    get_kevap70(idx_N2H2) = 2.937482111710803e-18
    get_kevap70(idx_CHNH) = 3.3582829279935226e-09
    get_kevap70(idx_H2C3O) = 2.262793237675391e-11
    get_kevap70(idx_H2C3N) = 6.206394420219489e-20
    get_kevap70(idx_H2C5N) = 7.346971529152301e-30
    get_kevap70(idx_H2C7N) = 8.697157640243048e-40
    get_kevap70(idx_H2C9N) = 1.0295473545678291e-49
    get_kevap70(idx_NH2OH) = 5.615017545999893e-31
    get_kevap70(idx_CH2OH) = 3.038288624339281e-20
    get_kevap70(idx_C5H3) = 1.7746575016853672e-22
    get_kevap70(idx_H3C5N) = 1.1863566224964188e-32
    get_kevap70(idx_C6H3) = 1.930852246429586e-27
    get_kevap70(idx_C7H3) = 2.1007943189046726e-32
    get_kevap70(idx_H3C7N) = 1.4043787324419037e-42
    get_kevap70(idx_C8H3) = 2.2856936767186715e-37
    get_kevap70(idx_C9H3) = 2.4868667707153994e-42
    get_kevap70(idx_H3C9N) = 1.2493067641109701e-52
    get_kevap70(idx_CH3NH) = 9.442034014424764e-11
    get_kevap70(idx_H4C3N) = 1.6182781708229198e-25
    get_kevap70(idx_C5H4) = 2.865638816529216e-25
    get_kevap70(idx_C6H4) = 3.1178552149338822e-30
    get_kevap70(idx_C7H4) = 3.392270193026015e-35
    get_kevap70(idx_C8H4) = 3.6908375370909545e-40
    get_kevap70(idx_C9H4) = 4.0156829940035244e-45
    get_kevap70(idx_H5C3N) = 2.613124356585979e-28
    get_kevap70(idx_C2H6) = 5.801386203197379e-16
    get_kevap70(idx_C_DUST) = 0d0
    get_kevap70(idx_C2_DUST) = 0d0
    get_kevap70(idx_C3_DUST) = 0d0
    get_kevap70(idx_C2H_DUST) = 0d0
    get_kevap70(idx_C3H_DUST) = 0d0
    get_kevap70(idx_C2H3_DUST) = 0d0
    get_kevap70(idx_C3H3_DUST) = 0d0
    get_kevap70(idx_C2N_DUST) = 0d0
    get_kevap70(idx_C3N_DUST) = 0d0
    get_kevap70(idx_CCO_DUST) = 0d0
    get_kevap70(idx_C3O_DUST) = 0d0
    get_kevap70(idx_C2S_DUST) = 0d0
    get_kevap70(idx_C3S_DUST) = 0d0
    get_kevap70(idx_C4_DUST) = 0d0
    get_kevap70(idx_C4H_DUST) = 0d0
    get_kevap70(idx_C5_DUST) = 0d0
    get_kevap70(idx_C5H_DUST) = 0d0
    get_kevap70(idx_C6_DUST) = 0d0
    get_kevap70(idx_C6H_DUST) = 0d0
    get_kevap70(idx_C7_DUST) = 0d0
    get_kevap70(idx_C7H_DUST) = 0d0
    get_kevap70(idx_C8_DUST) = 0d0
    get_kevap70(idx_C8H_DUST) = 0d0
    get_kevap70(idx_C9_DUST) = 0d0
    get_kevap70(idx_C9H_DUST) = 0d0
    get_kevap70(idx_C10_DUST) = 0d0
    get_kevap70(idx_CH_DUST) = 0d0
    get_kevap70(idx_CH2_DUST) = 0d0
    get_kevap70(idx_C2H2_DUST) = 0d0
    get_kevap70(idx_CH3_DUST) = 0d0
    get_kevap70(idx_CN_DUST) = 0d0
    get_kevap70(idx_HS_DUST) = 0d0
    get_kevap70(idx_CS_DUST) = 0d0
    get_kevap70(idx_H_DUST) = 0d0
    get_kevap70(idx_N_DUST) = 0d0
    get_kevap70(idx_NH_DUST) = 0d0
    get_kevap70(idx_HNC_DUST) = 0d0
    get_kevap70(idx_NH2_DUST) = 0d0
    get_kevap70(idx_NO_DUST) = 0d0
    get_kevap70(idx_O_DUST) = 0d0
    get_kevap70(idx_OCN_DUST) = 0d0
    get_kevap70(idx_NS_DUST) = 0d0
    get_kevap70(idx_S_DUST) = 0d0
    get_kevap70(idx_CO_DUST) = 0d0
    get_kevap70(idx_O2_DUST) = 0d0
    get_kevap70(idx_OH_DUST) = 0d0
    get_kevap70(idx_SO_DUST) = 0d0
    get_kevap70(idx_C3H2_DUST) = 0d0
    get_kevap70(idx_C3H4_DUST) = 0d0
    get_kevap70(idx_C4H2_DUST) = 0d0
    get_kevap70(idx_C5H2_DUST) = 0d0
    get_kevap70(idx_C6H2_DUST) = 0d0
    get_kevap70(idx_C7H2_DUST) = 0d0
    get_kevap70(idx_C8H2_DUST) = 0d0
    get_kevap70(idx_C9H2_DUST) = 0d0
    get_kevap70(idx_C2H4_DUST) = 0d0
    get_kevap70(idx_HCCN_DUST) = 0d0
    get_kevap70(idx_HNO_DUST) = 0d0
    get_kevap70(idx_HCN_DUST) = 0d0
    get_kevap70(idx_CHNH_DUST) = 0d0
    get_kevap70(idx_CH3N_DUST) = 0d0
    get_kevap70(idx_HCO_DUST) = 0d0
    get_kevap70(idx_C2H5_DUST) = 0d0
    get_kevap70(idx_C2H2N_DUST) = 0d0
    get_kevap70(idx_CH2NH2_DUST) = 0d0
    get_kevap70(idx_H2CO_DUST) = 0d0
    get_kevap70(idx_CH3C3N_DUST) = 0d0
    get_kevap70(idx_C5N_DUST) = 0d0
    get_kevap70(idx_CH3C5N_DUST) = 0d0
    get_kevap70(idx_C7N_DUST) = 0d0
    get_kevap70(idx_CH3C7N_DUST) = 0d0
    get_kevap70(idx_CH2OH_DUST) = 0d0
    get_kevap70(idx_C2H5OH_DUST) = 0d0
    get_kevap70(idx_CH3OCH3_DUST) = 0d0
    get_kevap70(idx_C2H6_DUST) = 0d0
    get_kevap70(idx_C2H3N_DUST) = 0d0
    get_kevap70(idx_C2H4O_DUST) = 0d0
    get_kevap70(idx_CH4_DUST) = 0d0
    get_kevap70(idx_H2_DUST) = 0d0
    get_kevap70(idx_HC2O_DUST) = 0d0
    get_kevap70(idx_C3H3N_DUST) = 0d0
    get_kevap70(idx_H4C3N_DUST) = 0d0
    get_kevap70(idx_HC3N_DUST) = 0d0
    get_kevap70(idx_HC3O_DUST) = 0d0
    get_kevap70(idx_C4H3_DUST) = 0d0
    get_kevap70(idx_C4H4_DUST) = 0d0
    get_kevap70(idx_C5H3_DUST) = 0d0
    get_kevap70(idx_C5H4_DUST) = 0d0
    get_kevap70(idx_HC5N_DUST) = 0d0
    get_kevap70(idx_C6H3_DUST) = 0d0
    get_kevap70(idx_C6H4_DUST) = 0d0
    get_kevap70(idx_C7H3_DUST) = 0d0
    get_kevap70(idx_C7H4_DUST) = 0d0
    get_kevap70(idx_HC7N_DUST) = 0d0
    get_kevap70(idx_C8H3_DUST) = 0d0
    get_kevap70(idx_C8H4_DUST) = 0d0
    get_kevap70(idx_C9H3_DUST) = 0d0
    get_kevap70(idx_C9H4_DUST) = 0d0
    get_kevap70(idx_C9N_DUST) = 0d0
    get_kevap70(idx_HC9N_DUST) = 0d0
    get_kevap70(idx_CH3NH_DUST) = 0d0
    get_kevap70(idx_CH5N_DUST) = 0d0
    get_kevap70(idx_CH4O_DUST) = 0d0
    get_kevap70(idx_HCS_DUST) = 0d0
    get_kevap70(idx_FE_DUST) = 0d0
    get_kevap70(idx_FEH_DUST) = 0d0
    get_kevap70(idx_H2C3N_DUST) = 0d0
    get_kevap70(idx_H2C5N_DUST) = 0d0
    get_kevap70(idx_H3C5N_DUST) = 0d0
    get_kevap70(idx_H2C7N_DUST) = 0d0
    get_kevap70(idx_H3C7N_DUST) = 0d0
    get_kevap70(idx_H2C9N_DUST) = 0d0
    get_kevap70(idx_H3C9N_DUST) = 0d0
    get_kevap70(idx_H2CN_DUST) = 0d0
    get_kevap70(idx_H2O2_DUST) = 0d0
    get_kevap70(idx_H2O_DUST) = 0d0
    get_kevap70(idx_O2H_DUST) = 0d0
    get_kevap70(idx_H2S_DUST) = 0d0
    get_kevap70(idx_H5C3N_DUST) = 0d0
    get_kevap70(idx_C2H2O_DUST) = 0d0
    get_kevap70(idx_H2C3O_DUST) = 0d0
    get_kevap70(idx_H2CS_DUST) = 0d0
    get_kevap70(idx_MG_DUST) = 0d0
    get_kevap70(idx_MGH_DUST) = 0d0
    get_kevap70(idx_MGH2_DUST) = 0d0
    get_kevap70(idx_N2H2_DUST) = 0d0
    get_kevap70(idx_N2_DUST) = 0d0
    get_kevap70(idx_NA_DUST) = 0d0
    get_kevap70(idx_NAH_DUST) = 0d0
    get_kevap70(idx_NH3_DUST) = 0d0
    get_kevap70(idx_O3_DUST) = 0d0
    get_kevap70(idx_HNCO_DUST) = 0d0
    get_kevap70(idx_OCS_DUST) = 0d0
    get_kevap70(idx_SI_DUST) = 0d0
    get_kevap70(idx_SIH_DUST) = 0d0
    get_kevap70(idx_SIH2_DUST) = 0d0
    get_kevap70(idx_SIH3_DUST) = 0d0
    get_kevap70(idx_SIH4_DUST) = 0d0
    get_kevap70(idx_SO2_DUST) = 0d0
    get_kevap70(idx_HCOOCH3_DUST) = 0d0
    get_kevap70(idx_NH2CHO_DUST) = 0d0
    get_kevap70(idx_CO2_DUST) = 0d0
    get_kevap70(idx_CH2O2_DUST) = 0d0
    get_kevap70(idx_NH2OH_DUST) = 0d0
    get_kevap70(idx_C4N_DUST) = 0d0
    get_kevap70(idx_C4S_DUST) = 0d0
    get_kevap70(idx_C6H6_DUST) = 0d0
    get_kevap70(idx_CH2NH2) = 8.258924757155297e-20
    get_kevap70(idx_CH3C4H_DUST) = 0d0
    get_kevap70(idx_CH3C6H_DUST) = 0d0
    get_kevap70(idx_H2S2_DUST) = 0d0
    get_kevap70(idx_HC2NC_DUST) = 0d0
    get_kevap70(idx_HCNC2_DUST) = 0d0
    get_kevap70(idx_HE_DUST) = 0d0
    get_kevap70(idx_HNC3_DUST) = 0d0
    get_kevap70(idx_HS2_DUST) = 0d0
    get_kevap70(idx_NAOH_DUST) = 0d0
    get_kevap70(idx_S2_DUST) = 0d0
    get_kevap70(idx_SIC_DUST) = 0d0
    get_kevap70(idx_SIO_DUST) = 0d0
    get_kevap70(idx_SIS_DUST) = 0d0
    get_kevap70(idx_C2H6CO_DUST) = 0d0
    get_kevap70(idx_C3P_DUST) = 0d0
    get_kevap70(idx_CCP_DUST) = 0d0
    get_kevap70(idx_C4P_DUST) = 0d0
    get_kevap70(idx_CCL_DUST) = 0d0
    get_kevap70(idx_CL_DUST) = 0d0
    get_kevap70(idx_P_DUST) = 0d0
    get_kevap70(idx_CP_DUST) = 0d0
    get_kevap70(idx_CH2PH_DUST) = 0d0
    get_kevap70(idx_HCP_DUST) = 0d0
    get_kevap70(idx_CLO_DUST) = 0d0
    get_kevap70(idx_H2SIO_DUST) = 0d0
    get_kevap70(idx_HCCP_DUST) = 0d0
    get_kevap70(idx_HCL_DUST) = 0d0
    get_kevap70(idx_HCSI_DUST) = 0d0
    get_kevap70(idx_HNSI_DUST) = 0d0
    get_kevap70(idx_SIN_DUST) = 0d0
    get_kevap70(idx_HPO_DUST) = 0d0
    get_kevap70(idx_PO_DUST) = 0d0
    get_kevap70(idx_N2O_DUST) = 0d0
    get_kevap70(idx_NH2CN_DUST) = 0d0
    get_kevap70(idx_NO2_DUST) = 0d0
    get_kevap70(idx_PH_DUST) = 0d0
    get_kevap70(idx_PH2_DUST) = 0d0
    get_kevap70(idx_PN_DUST) = 0d0
    get_kevap70(idx_SIC2_DUST) = 0d0
    get_kevap70(idx_SIC2H_DUST) = 0d0
    get_kevap70(idx_SIC2H2_DUST) = 0d0
    get_kevap70(idx_SIC3_DUST) = 0d0
    get_kevap70(idx_SIC3H_DUST) = 0d0
    get_kevap70(idx_SIC4_DUST) = 0d0
    get_kevap70(idx_SICH2_DUST) = 0d0
    get_kevap70(idx_SICH3_DUST) = 0d0
    get_kevap70(idx_SINC_DUST) = 0d0
    get_kevap70(idx_SIO2_DUST) = 0d0
    get_kevap70(idx_Cj) = 0d0
    get_kevap70(idx_CLj) = 0d0
    get_kevap70(idx_FEj) = 0d0
    get_kevap70(idx_Hj) = 0d0
    get_kevap70(idx_HEj) = 0d0
    get_kevap70(idx_MGj) = 0d0
    get_kevap70(idx_Nj) = 0d0
    get_kevap70(idx_NAj) = 0d0
    get_kevap70(idx_Oj) = 0d0
    get_kevap70(idx_Pj) = 0d0
    get_kevap70(idx_Sj) = 0d0
    get_kevap70(idx_SIj) = 0d0
    get_kevap70(idx_COj) = 0d0
    get_kevap70(idx_H2j) = 0d0
    get_kevap70(idx_NOj) = 0d0
    get_kevap70(idx_O2j) = 0d0
    get_kevap70(idx_CH2j) = 0d0
    get_kevap70(idx_H2Sj) = 0d0
    get_kevap70(idx_HCOj) = 0d0
    get_kevap70(idx_HCSj) = 0d0
    get_kevap70(idx_HNOj) = 0d0
    get_kevap70(idx_NH2j) = 0d0
    get_kevap70(idx_OCSj) = 0d0
    get_kevap70(idx_C2H2j) = 0d0
    get_kevap70(idx_CH3j) = 0d0
    get_kevap70(idx_NH3j) = 0d0
    get_kevap70(idx_C2H2Oj) = 0d0
    get_kevap70(idx_CH2O2j) = 0d0
    get_kevap70(idx_C2H3Nj) = 0d0
    get_kevap70(idx_C2H4j) = 0d0
    get_kevap70(idx_C4H2j) = 0d0
    get_kevap70(idx_H3COj) = 0d0
    get_kevap70(idx_CH4Oj) = 0d0
    get_kevap70(idx_C2H4Oj) = 0d0
    get_kevap70(idx_C3H4j) = 0d0
    get_kevap70(idx_CH5Nj) = 0d0
    get_kevap70(idx_C2H5OHj) = 0d0
    get_kevap70(idx_CH3OCH3j) = 0d0
    get_kevap70(idx_CHj) = 0d0
    get_kevap70(idx_CCLj) = 0d0
    get_kevap70(idx_C2j) = 0d0
    get_kevap70(idx_CLOj) = 0d0
    get_kevap70(idx_CPj) = 0d0
    get_kevap70(idx_CSj) = 0d0
    get_kevap70(idx_CNj) = 0d0
    get_kevap70(idx_NSj) = 0d0
    get_kevap70(idx_PHj) = 0d0
    get_kevap70(idx_POj) = 0d0
    get_kevap70(idx_SICj) = 0d0
    get_kevap70(idx_SINj) = 0d0
    get_kevap70(idx_SISj) = 0d0
    get_kevap70(idx_SOj) = 0d0
    get_kevap70(idx_C3j) = 0d0
    get_kevap70(idx_C2Sj) = 0d0
    get_kevap70(idx_C2Oj) = 0d0
    get_kevap70(idx_CCPj) = 0d0
    get_kevap70(idx_C2Hj) = 0d0
    get_kevap70(idx_HOCj) = 0d0
    get_kevap70(idx_C2Nj) = 0d0
    get_kevap70(idx_CNCj) = 0d0
    get_kevap70(idx_HCPj) = 0d0
    get_kevap70(idx_SIC2j) = 0d0
    get_kevap70(idx_SINCj) = 0d0
    get_kevap70(idx_HPOj) = 0d0
    get_kevap70(idx_HCNj) = 0d0
    get_kevap70(idx_CHSIj) = 0d0
    get_kevap70(idx_SIH2j) = 0d0
    get_kevap70(idx_C3Hj) = 0d0
    get_kevap70(idx_C4j) = 0d0
    get_kevap70(idx_C3Oj) = 0d0
    get_kevap70(idx_C3Sj) = 0d0
    get_kevap70(idx_H2COj) = 0d0
    get_kevap70(idx_H2SIOj) = 0d0
    get_kevap70(idx_HCNHj) = 0d0
    get_kevap70(idx_SIC2Hj) = 0d0
    get_kevap70(idx_SIC3j) = 0d0
    get_kevap70(idx_CH2SIj) = 0d0
    get_kevap70(idx_SIH3j) = 0d0
    get_kevap70(idx_C2H2Nj) = 0d0
    get_kevap70(idx_C2H3j) = 0d0
    get_kevap70(idx_C3H2j) = 0d0
    get_kevap70(idx_H2C3j) = 0d0
    get_kevap70(idx_C4Hj) = 0d0
    get_kevap70(idx_C5j) = 0d0
    get_kevap70(idx_C4Sj) = 0d0
    get_kevap70(idx_PC2Hj) = 0d0
    get_kevap70(idx_C3Nj) = 0d0
    get_kevap70(idx_C4Nj) = 0d0
    get_kevap70(idx_C3HNj) = 0d0
    get_kevap70(idx_HNCj) = 0d0
    get_kevap70(idx_SIC3Hj) = 0d0
    get_kevap70(idx_SIC4j) = 0d0
    get_kevap70(idx_SIC2H2j) = 0d0
    get_kevap70(idx_SICH3j) = 0d0
    get_kevap70(idx_HC2NCHj) = 0d0
    get_kevap70(idx_C3H3j) = 0d0
    get_kevap70(idx_H3C3j) = 0d0
    get_kevap70(idx_C5Hj) = 0d0
    get_kevap70(idx_C6j) = 0d0
    get_kevap70(idx_C2H3Oj) = 0d0
    get_kevap70(idx_C2H5j) = 0d0
    get_kevap70(idx_C3H3Nj) = 0d0
    get_kevap70(idx_C5H2j) = 0d0
    get_kevap70(idx_C4H3j) = 0d0
    get_kevap70(idx_C6Hj) = 0d0
    get_kevap70(idx_C7j) = 0d0
    get_kevap70(idx_CH4Nj) = 0d0
    get_kevap70(idx_C5HNj) = 0d0
    get_kevap70(idx_C7Hj) = 0d0
    get_kevap70(idx_C8j) = 0d0
    get_kevap70(idx_COOCH4j) = 0d0
    get_kevap70(idx_C2H5Oj) = 0d0
    get_kevap70(idx_C8Hj) = 0d0
    get_kevap70(idx_C9j) = 0d0
    get_kevap70(idx_C5H3j) = 0d0
    get_kevap70(idx_C6H2j) = 0d0
    get_kevap70(idx_C6H3j) = 0d0
    get_kevap70(idx_C2H6COj) = 0d0
    get_kevap70(idx_C9Hj) = 0d0
    get_kevap70(idx_C10j) = 0d0
    get_kevap70(idx_C7H3j) = 0d0
    get_kevap70(idx_C8H2j) = 0d0
    get_kevap70(idx_C8H3j) = 0d0
    get_kevap70(idx_HCLj) = 0d0
    get_kevap70(idx_HSj) = 0d0
    get_kevap70(idx_NHj) = 0d0
    get_kevap70(idx_OHj) = 0d0
    get_kevap70(idx_PNj) = 0d0
    get_kevap70(idx_S2j) = 0d0
    get_kevap70(idx_SIHj) = 0d0
    get_kevap70(idx_SIOj) = 0d0
    get_kevap70(idx_H2Oj) = 0d0
    get_kevap70(idx_HNSIj) = 0d0
    get_kevap70(idx_S2Hj) = 0d0
    get_kevap70(idx_PH2j) = 0d0
    get_kevap70(idx_H2CSj) = 0d0
    get_kevap70(idx_H2S2j) = 0d0
    get_kevap70(idx_HSIOj) = 0d0
    get_kevap70(idx_C4Pj) = 0d0
    get_kevap70(idx_HCO2j) = 0d0
    get_kevap70(idx_PCH3j) = 0d0
    get_kevap70(idx_CH4j) = 0d0
    get_kevap70(idx_C2NHj) = 0d0
    get_kevap70(idx_SIH4j) = 0d0
    get_kevap70(idx_NH4j) = 0d0
    get_kevap70(idx_H2NCj) = 0d0
    get_kevap70(idx_C3H2Nj) = 0d0
    get_kevap70(idx_C7H2j) = 0d0
    get_kevap70(idx_C5H4j) = 0d0
    get_kevap70(idx_C7HNj) = 0d0
    get_kevap70(idx_C9H2j) = 0d0
    get_kevap70(idx_C7H4j) = 0d0
    get_kevap70(idx_C9HNj) = 0d0
    get_kevap70(idx_N2j) = 0d0
    get_kevap70(idx_CO2j) = 0d0
    get_kevap70(idx_HEHj) = 0d0
    get_kevap70(idx_SO2j) = 0d0
    get_kevap70(idx_C6H5j) = 0d0
    get_kevap70(idx_C5H5j) = 0d0
    get_kevap70(idx_N2Hj) = 0d0
    get_kevap70(idx_NO2j) = 0d0
    get_kevap70(idx_PC2H2j) = 0d0
    get_kevap70(idx_PNH2j) = 0d0
    get_kevap70(idx_PCH2j) = 0d0
    get_kevap70(idx_HC2Sj) = 0d0
    get_kevap70(idx_HC3Sj) = 0d0
    get_kevap70(idx_H3CSj) = 0d0
    get_kevap70(idx_HC4Sj) = 0d0
    get_kevap70(idx_SINH2j) = 0d0
    get_kevap70(idx_SIC2H3j) = 0d0
    get_kevap70(idx_SIC3H2j) = 0d0
    get_kevap70(idx_C2HOj) = 0d0
    get_kevap70(idx_H3Oj) = 0d0
    get_kevap70(idx_H3Sj) = 0d0
    get_kevap70(idx_HOCSj) = 0d0
    get_kevap70(idx_CH5Oj) = 0d0
    get_kevap70(idx_NCOj) = 0d0
    get_kevap70(idx_HNCOj) = 0d0
    get_kevap70(idx_C2N2j) = 0d0
    get_kevap70(idx_H3j) = 0d0
    get_kevap70(idx_O2Hj) = 0d0
    get_kevap70(idx_CH5j) = 0d0
    get_kevap70(idx_H2CLj) = 0d0
    get_kevap70(idx_CH3O2j) = 0d0
    get_kevap70(idx_H2POj) = 0d0
    get_kevap70(idx_PNH3j) = 0d0
    get_kevap70(idx_PCH4j) = 0d0
    get_kevap70(idx_PC2H3j) = 0d0
    get_kevap70(idx_HSISj) = 0d0
    get_kevap70(idx_HSOj) = 0d0
    get_kevap70(idx_HNSj) = 0d0
    get_kevap70(idx_HPNj) = 0d0
    get_kevap70(idx_H2NOj) = 0d0
    get_kevap70(idx_NAH2Oj) = 0d0
    get_kevap70(idx_PH3j) = 0d0
    get_kevap70(idx_SINCHj) = 0d0
    get_kevap70(idx_HSIO2j) = 0d0
    get_kevap70(idx_HSO2j) = 0d0
    get_kevap70(idx_HC3Oj) = 0d0
    get_kevap70(idx_PC3Hj) = 0d0
    get_kevap70(idx_H3S2j) = 0d0
    get_kevap70(idx_H3SIOj) = 0d0
    get_kevap70(idx_PC4Hj) = 0d0
    get_kevap70(idx_NH2CNHj) = 0d0
    get_kevap70(idx_SIC4Hj) = 0d0
    get_kevap70(idx_SICH4j) = 0d0
    get_kevap70(idx_SIH5j) = 0d0
    get_kevap70(idx_C2H4Nj) = 0d0
    get_kevap70(idx_NH2CH2Oj) = 0d0
    get_kevap70(idx_C2H6j) = 0d0
    get_kevap70(idx_C3H4Nj) = 0d0
    get_kevap70(idx_C3H5j) = 0d0
    get_kevap70(idx_C4H4j) = 0d0
    get_kevap70(idx_CH6Nj) = 0d0
    get_kevap70(idx_C5H2Nj) = 0d0
    get_kevap70(idx_C4H4Nj) = 0d0
    get_kevap70(idx_H5C2O2j) = 0d0
    get_kevap70(idx_C2H5OH2j) = 0d0
    get_kevap70(idx_CH3OCH4j) = 0d0
    get_kevap70(idx_C7H2Nj) = 0d0
    get_kevap70(idx_C3H6OHj) = 0d0
    get_kevap70(idx_C6H4Nj) = 0d0
    get_kevap70(idx_C10Hj) = 0d0
    get_kevap70(idx_C9H3j) = 0d0
    get_kevap70(idx_C7H5j) = 0d0
    get_kevap70(idx_C8H4Nj) = 0d0
    get_kevap70(idx_C9H2Nj) = 0d0
    get_kevap70(idx_C6H7j) = 0d0
    get_kevap70(idx_NAH2j) = 0d0
    get_kevap70(idx_PC2H4j) = 0d0
    get_kevap70(idx_C4H5j) = 0d0
    get_kevap70(idx_H2CCLj) = 0d0
    get_kevap70(idx_PC4H2j) = 0d0
    get_kevap70(idx_C6H4j) = 0d0
    get_kevap70(idx_C8H4j) = 0d0
    get_kevap70(idx_C9H4j) = 0d0
    get_kevap70(idx_C4H7j) = 0d0
    get_kevap70(idx_HC4Nj) = 0d0
    get_kevap70(idx_HC4Oj) = 0d0
    get_kevap70(idx_C5Nj) = 0d0
    get_kevap70(idx_H2C4Nj) = 0d0
    get_kevap70(idx_H3C4Nj) = 0d0
    get_kevap70(idx_C7Nj) = 0d0
    get_kevap70(idx_C5H3Nj) = 0d0
    get_kevap70(idx_C10H2j) = 0d0
    get_kevap70(idx_C9Nj) = 0d0
    get_kevap70(idx_C7H3Nj) = 0d0
    get_kevap70(idx_C9H3Nj) = 0d0
    get_kevap70(idx_OCSjH2) = 0d0
    get_kevap70(idx_H2C3Oj) = 0d0
    get_kevap70(idx_H3C3Oj) = 0d0
    get_kevap70(idx_C5H4Nj) = 0d0
    get_kevap70(idx_C8H5j) = 0d0
    get_kevap70(idx_C9H5j) = 0d0
    get_kevap70(idx_H2COHOCH2j) = 0d0
    get_kevap70(idx_H7C2O2j) = 0d0
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
    get_names(3) = "C-"
    get_names(4) = "CN-"
    get_names(5) = "O-"
    get_names(6) = "OH-"
    get_names(7) = "S-"
    get_names(8) = "GRAIN-"
    get_names(9) = "C"
    get_names(10) = "CL"
    get_names(11) = "FE"
    get_names(12) = "H"
    get_names(13) = "HE"
    get_names(14) = "MG"
    get_names(15) = "N"
    get_names(16) = "NA"
    get_names(17) = "O"
    get_names(18) = "P"
    get_names(19) = "S"
    get_names(20) = "SI"
    get_names(21) = "C2"
    get_names(22) = "CCL"
    get_names(23) = "CH"
    get_names(24) = "CLO"
    get_names(25) = "CN"
    get_names(26) = "CO"
    get_names(27) = "CP"
    get_names(28) = "CS"
    get_names(29) = "H2"
    get_names(30) = "HCL"
    get_names(31) = "HS"
    get_names(32) = "MGH"
    get_names(33) = "N2"
    get_names(34) = "NAH"
    get_names(35) = "NH"
    get_names(36) = "NO"
    get_names(37) = "NS"
    get_names(38) = "O2"
    get_names(39) = "OH"
    get_names(40) = "PH"
    get_names(41) = "PN"
    get_names(42) = "PO"
    get_names(43) = "S2"
    get_names(44) = "SIC"
    get_names(45) = "SIH"
    get_names(46) = "SIN"
    get_names(47) = "SIO"
    get_names(48) = "SIS"
    get_names(49) = "SO"
    get_names(50) = "C2H"
    get_names(51) = "C2N"
    get_names(52) = "C2S"
    get_names(53) = "C3"
    get_names(54) = "CCO"
    get_names(55) = "CCP"
    get_names(56) = "CH2"
    get_names(57) = "CO2"
    get_names(58) = "H2O"
    get_names(59) = "H2S"
    get_names(60) = "HCN"
    get_names(61) = "HCO"
    get_names(62) = "HCP"
    get_names(63) = "HCS"
    get_names(64) = "HCSI"
    get_names(65) = "HNC"
    get_names(66) = "HNO"
    get_names(67) = "HNSI"
    get_names(68) = "HPO"
    get_names(69) = "HS2"
    get_names(70) = "N2O"
    get_names(71) = "NAOH"
    get_names(72) = "NH2"
    get_names(73) = "NO2"
    get_names(74) = "O2H"
    get_names(75) = "OCN"
    get_names(76) = "OCS"
    get_names(77) = "PH2"
    get_names(78) = "SIC2"
    get_names(79) = "SIH2"
    get_names(80) = "SINC"
    get_names(81) = "SIO2"
    get_names(82) = "SO2"
    get_names(83) = "C2H2"
    get_names(84) = "C3H"
    get_names(85) = "C3N"
    get_names(86) = "C3O"
    get_names(87) = "C3P"
    get_names(88) = "C3S"
    get_names(89) = "C4"
    get_names(90) = "CH3"
    get_names(91) = "H2CO"
    get_names(92) = "H2CS"
    get_names(93) = "H2O2"
    get_names(94) = "H2S2"
    get_names(95) = "H2SIO"
    get_names(96) = "HCCP"
    get_names(97) = "NH3"
    get_names(98) = "SIC2H"
    get_names(99) = "SIC3"
    get_names(100) = "SICH2"
    get_names(101) = "SIH3"
    get_names(102) = "C2H2N"
    get_names(103) = "C2H2O"
    get_names(104) = "C2H3"
    get_names(105) = "C3H2"
    get_names(106) = "C4H"
    get_names(107) = "C4N"
    get_names(108) = "C4P"
    get_names(109) = "C4S"
    get_names(110) = "C5"
    get_names(111) = "CH2O2"
    get_names(112) = "CH2PH"
    get_names(113) = "CH3N"
    get_names(114) = "CH4"
    get_names(115) = "HC3N"
    get_names(116) = "SIC2H2"
    get_names(117) = "SIC3H"
    get_names(118) = "SIC4"
    get_names(119) = "SICH3"
    get_names(120) = "SIH4"
    get_names(121) = "C2H3N"
    get_names(122) = "C2H4"
    get_names(123) = "C3H3"
    get_names(124) = "C4H2"
    get_names(125) = "C5H"
    get_names(126) = "C5N"
    get_names(127) = "C6"
    get_names(128) = "CH4O"
    get_names(129) = "C2H4O"
    get_names(130) = "C2H5"
    get_names(131) = "C3H3N"
    get_names(132) = "C3H4"
    get_names(133) = "C5H2"
    get_names(134) = "C6H"
    get_names(135) = "C7"
    get_names(136) = "CH5N"
    get_names(137) = "HC5N"
    get_names(138) = "C6H2"
    get_names(139) = "C7H"
    get_names(140) = "C7N"
    get_names(141) = "C8"
    get_names(142) = "CH3C3N"
    get_names(143) = "HCOOCH3"
    get_names(144) = "C2H5OH"
    get_names(145) = "C7H2"
    get_names(146) = "C8H"
    get_names(147) = "C9"
    get_names(148) = "CH3C4H"
    get_names(149) = "CH3OCH3"
    get_names(150) = "HC7N"
    get_names(151) = "C2H6CO"
    get_names(152) = "C8H2"
    get_names(153) = "C9H"
    get_names(154) = "C9N"
    get_names(155) = "C10"
    get_names(156) = "CH3C5N"
    get_names(157) = "C9H2"
    get_names(158) = "CH3C6H"
    get_names(159) = "CH3C7N"
    get_names(160) = "HC9N"
    get_names(161) = "C4H4"
    get_names(162) = "HCNC2"
    get_names(163) = "HC2NC"
    get_names(164) = "HNC3"
    get_names(165) = "NH2CHO"
    get_names(166) = "C4H3"
    get_names(167) = "NH2CN"
    get_names(168) = "C6H6"
    get_names(169) = "H2CN"
    get_names(170) = "GRAIN0"
    get_names(171) = "O3"
    get_names(172) = "FEH"
    get_names(173) = "HNCO"
    get_names(174) = "HC2O"
    get_names(175) = "HCCN"
    get_names(176) = "HC3O"
    get_names(177) = "MGH2"
    get_names(178) = "N2H2"
    get_names(179) = "CHNH"
    get_names(180) = "H2C3O"
    get_names(181) = "H2C3N"
    get_names(182) = "H2C5N"
    get_names(183) = "H2C7N"
    get_names(184) = "H2C9N"
    get_names(185) = "NH2OH"
    get_names(186) = "CH2OH"
    get_names(187) = "C5H3"
    get_names(188) = "H3C5N"
    get_names(189) = "C6H3"
    get_names(190) = "C7H3"
    get_names(191) = "H3C7N"
    get_names(192) = "C8H3"
    get_names(193) = "C9H3"
    get_names(194) = "H3C9N"
    get_names(195) = "CH3NH"
    get_names(196) = "H4C3N"
    get_names(197) = "C5H4"
    get_names(198) = "C6H4"
    get_names(199) = "C7H4"
    get_names(200) = "C8H4"
    get_names(201) = "C9H4"
    get_names(202) = "H5C3N"
    get_names(203) = "C2H6"
    get_names(204) = "C_DUST"
    get_names(205) = "C2_DUST"
    get_names(206) = "C3_DUST"
    get_names(207) = "C2H_DUST"
    get_names(208) = "C3H_DUST"
    get_names(209) = "C2H3_DUST"
    get_names(210) = "C3H3_DUST"
    get_names(211) = "C2N_DUST"
    get_names(212) = "C3N_DUST"
    get_names(213) = "CCO_DUST"
    get_names(214) = "C3O_DUST"
    get_names(215) = "C2S_DUST"
    get_names(216) = "C3S_DUST"
    get_names(217) = "C4_DUST"
    get_names(218) = "C4H_DUST"
    get_names(219) = "C5_DUST"
    get_names(220) = "C5H_DUST"
    get_names(221) = "C6_DUST"
    get_names(222) = "C6H_DUST"
    get_names(223) = "C7_DUST"
    get_names(224) = "C7H_DUST"
    get_names(225) = "C8_DUST"
    get_names(226) = "C8H_DUST"
    get_names(227) = "C9_DUST"
    get_names(228) = "C9H_DUST"
    get_names(229) = "C10_DUST"
    get_names(230) = "CH_DUST"
    get_names(231) = "CH2_DUST"
    get_names(232) = "C2H2_DUST"
    get_names(233) = "CH3_DUST"
    get_names(234) = "CN_DUST"
    get_names(235) = "HS_DUST"
    get_names(236) = "CS_DUST"
    get_names(237) = "H_DUST"
    get_names(238) = "N_DUST"
    get_names(239) = "NH_DUST"
    get_names(240) = "HNC_DUST"
    get_names(241) = "NH2_DUST"
    get_names(242) = "NO_DUST"
    get_names(243) = "O_DUST"
    get_names(244) = "OCN_DUST"
    get_names(245) = "NS_DUST"
    get_names(246) = "S_DUST"
    get_names(247) = "CO_DUST"
    get_names(248) = "O2_DUST"
    get_names(249) = "OH_DUST"
    get_names(250) = "SO_DUST"
    get_names(251) = "C3H2_DUST"
    get_names(252) = "C3H4_DUST"
    get_names(253) = "C4H2_DUST"
    get_names(254) = "C5H2_DUST"
    get_names(255) = "C6H2_DUST"
    get_names(256) = "C7H2_DUST"
    get_names(257) = "C8H2_DUST"
    get_names(258) = "C9H2_DUST"
    get_names(259) = "C2H4_DUST"
    get_names(260) = "HCCN_DUST"
    get_names(261) = "HNO_DUST"
    get_names(262) = "HCN_DUST"
    get_names(263) = "CHNH_DUST"
    get_names(264) = "CH3N_DUST"
    get_names(265) = "HCO_DUST"
    get_names(266) = "C2H5_DUST"
    get_names(267) = "C2H2N_DUST"
    get_names(268) = "CH2NH2_DUST"
    get_names(269) = "H2CO_DUST"
    get_names(270) = "CH3C3N_DUST"
    get_names(271) = "C5N_DUST"
    get_names(272) = "CH3C5N_DUST"
    get_names(273) = "C7N_DUST"
    get_names(274) = "CH3C7N_DUST"
    get_names(275) = "CH2OH_DUST"
    get_names(276) = "C2H5OH_DUST"
    get_names(277) = "CH3OCH3_DUST"
    get_names(278) = "C2H6_DUST"
    get_names(279) = "C2H3N_DUST"
    get_names(280) = "C2H4O_DUST"
    get_names(281) = "CH4_DUST"
    get_names(282) = "H2_DUST"
    get_names(283) = "HC2O_DUST"
    get_names(284) = "C3H3N_DUST"
    get_names(285) = "H4C3N_DUST"
    get_names(286) = "HC3N_DUST"
    get_names(287) = "HC3O_DUST"
    get_names(288) = "C4H3_DUST"
    get_names(289) = "C4H4_DUST"
    get_names(290) = "C5H3_DUST"
    get_names(291) = "C5H4_DUST"
    get_names(292) = "HC5N_DUST"
    get_names(293) = "C6H3_DUST"
    get_names(294) = "C6H4_DUST"
    get_names(295) = "C7H3_DUST"
    get_names(296) = "C7H4_DUST"
    get_names(297) = "HC7N_DUST"
    get_names(298) = "C8H3_DUST"
    get_names(299) = "C8H4_DUST"
    get_names(300) = "C9H3_DUST"
    get_names(301) = "C9H4_DUST"
    get_names(302) = "C9N_DUST"
    get_names(303) = "HC9N_DUST"
    get_names(304) = "CH3NH_DUST"
    get_names(305) = "CH5N_DUST"
    get_names(306) = "CH4O_DUST"
    get_names(307) = "HCS_DUST"
    get_names(308) = "FE_DUST"
    get_names(309) = "FEH_DUST"
    get_names(310) = "H2C3N_DUST"
    get_names(311) = "H2C5N_DUST"
    get_names(312) = "H3C5N_DUST"
    get_names(313) = "H2C7N_DUST"
    get_names(314) = "H3C7N_DUST"
    get_names(315) = "H2C9N_DUST"
    get_names(316) = "H3C9N_DUST"
    get_names(317) = "H2CN_DUST"
    get_names(318) = "H2O2_DUST"
    get_names(319) = "H2O_DUST"
    get_names(320) = "O2H_DUST"
    get_names(321) = "H2S_DUST"
    get_names(322) = "H5C3N_DUST"
    get_names(323) = "C2H2O_DUST"
    get_names(324) = "H2C3O_DUST"
    get_names(325) = "H2CS_DUST"
    get_names(326) = "MG_DUST"
    get_names(327) = "MGH_DUST"
    get_names(328) = "MGH2_DUST"
    get_names(329) = "N2H2_DUST"
    get_names(330) = "N2_DUST"
    get_names(331) = "NA_DUST"
    get_names(332) = "NAH_DUST"
    get_names(333) = "NH3_DUST"
    get_names(334) = "O3_DUST"
    get_names(335) = "HNCO_DUST"
    get_names(336) = "OCS_DUST"
    get_names(337) = "SI_DUST"
    get_names(338) = "SIH_DUST"
    get_names(339) = "SIH2_DUST"
    get_names(340) = "SIH3_DUST"
    get_names(341) = "SIH4_DUST"
    get_names(342) = "SO2_DUST"
    get_names(343) = "HCOOCH3_DUST"
    get_names(344) = "NH2CHO_DUST"
    get_names(345) = "CO2_DUST"
    get_names(346) = "CH2O2_DUST"
    get_names(347) = "NH2OH_DUST"
    get_names(348) = "C4N_DUST"
    get_names(349) = "C4S_DUST"
    get_names(350) = "C6H6_DUST"
    get_names(351) = "CH2NH2"
    get_names(352) = "CH3C4H_DUST"
    get_names(353) = "CH3C6H_DUST"
    get_names(354) = "H2S2_DUST"
    get_names(355) = "HC2NC_DUST"
    get_names(356) = "HCNC2_DUST"
    get_names(357) = "HE_DUST"
    get_names(358) = "HNC3_DUST"
    get_names(359) = "HS2_DUST"
    get_names(360) = "NAOH_DUST"
    get_names(361) = "S2_DUST"
    get_names(362) = "SIC_DUST"
    get_names(363) = "SIO_DUST"
    get_names(364) = "SIS_DUST"
    get_names(365) = "C2H6CO_DUST"
    get_names(366) = "C3P_DUST"
    get_names(367) = "CCP_DUST"
    get_names(368) = "C4P_DUST"
    get_names(369) = "CCL_DUST"
    get_names(370) = "CL_DUST"
    get_names(371) = "P_DUST"
    get_names(372) = "CP_DUST"
    get_names(373) = "CH2PH_DUST"
    get_names(374) = "HCP_DUST"
    get_names(375) = "CLO_DUST"
    get_names(376) = "H2SIO_DUST"
    get_names(377) = "HCCP_DUST"
    get_names(378) = "HCL_DUST"
    get_names(379) = "HCSI_DUST"
    get_names(380) = "HNSI_DUST"
    get_names(381) = "SIN_DUST"
    get_names(382) = "HPO_DUST"
    get_names(383) = "PO_DUST"
    get_names(384) = "N2O_DUST"
    get_names(385) = "NH2CN_DUST"
    get_names(386) = "NO2_DUST"
    get_names(387) = "PH_DUST"
    get_names(388) = "PH2_DUST"
    get_names(389) = "PN_DUST"
    get_names(390) = "SIC2_DUST"
    get_names(391) = "SIC2H_DUST"
    get_names(392) = "SIC2H2_DUST"
    get_names(393) = "SIC3_DUST"
    get_names(394) = "SIC3H_DUST"
    get_names(395) = "SIC4_DUST"
    get_names(396) = "SICH2_DUST"
    get_names(397) = "SICH3_DUST"
    get_names(398) = "SINC_DUST"
    get_names(399) = "SIO2_DUST"
    get_names(400) = "C+"
    get_names(401) = "CL+"
    get_names(402) = "FE+"
    get_names(403) = "H+"
    get_names(404) = "HE+"
    get_names(405) = "MG+"
    get_names(406) = "N+"
    get_names(407) = "NA+"
    get_names(408) = "O+"
    get_names(409) = "P+"
    get_names(410) = "S+"
    get_names(411) = "SI+"
    get_names(412) = "CO+"
    get_names(413) = "H2+"
    get_names(414) = "NO+"
    get_names(415) = "O2+"
    get_names(416) = "CH2+"
    get_names(417) = "H2S+"
    get_names(418) = "HCO+"
    get_names(419) = "HCS+"
    get_names(420) = "HNO+"
    get_names(421) = "NH2+"
    get_names(422) = "OCS+"
    get_names(423) = "C2H2+"
    get_names(424) = "CH3+"
    get_names(425) = "NH3+"
    get_names(426) = "C2H2O+"
    get_names(427) = "CH2O2+"
    get_names(428) = "C2H3N+"
    get_names(429) = "C2H4+"
    get_names(430) = "C4H2+"
    get_names(431) = "H3CO+"
    get_names(432) = "CH4O+"
    get_names(433) = "C2H4O+"
    get_names(434) = "C3H4+"
    get_names(435) = "CH5N+"
    get_names(436) = "C2H5OH+"
    get_names(437) = "CH3OCH3+"
    get_names(438) = "CH+"
    get_names(439) = "CCL+"
    get_names(440) = "C2+"
    get_names(441) = "CLO+"
    get_names(442) = "CP+"
    get_names(443) = "CS+"
    get_names(444) = "CN+"
    get_names(445) = "NS+"
    get_names(446) = "PH+"
    get_names(447) = "PO+"
    get_names(448) = "SIC+"
    get_names(449) = "SIN+"
    get_names(450) = "SIS+"
    get_names(451) = "SO+"
    get_names(452) = "C3+"
    get_names(453) = "C2S+"
    get_names(454) = "C2O+"
    get_names(455) = "CCP+"
    get_names(456) = "C2H+"
    get_names(457) = "HOC+"
    get_names(458) = "C2N+"
    get_names(459) = "CNC+"
    get_names(460) = "HCP+"
    get_names(461) = "SIC2+"
    get_names(462) = "SINC+"
    get_names(463) = "HPO+"
    get_names(464) = "HCN+"
    get_names(465) = "CHSI+"
    get_names(466) = "SIH2+"
    get_names(467) = "C3H+"
    get_names(468) = "C4+"
    get_names(469) = "C3O+"
    get_names(470) = "C3S+"
    get_names(471) = "H2CO+"
    get_names(472) = "H2SIO+"
    get_names(473) = "HCNH+"
    get_names(474) = "SIC2H+"
    get_names(475) = "SIC3+"
    get_names(476) = "CH2SI+"
    get_names(477) = "SIH3+"
    get_names(478) = "C2H2N+"
    get_names(479) = "C2H3+"
    get_names(480) = "C3H2+"
    get_names(481) = "H2C3+"
    get_names(482) = "C4H+"
    get_names(483) = "C5+"
    get_names(484) = "C4S+"
    get_names(485) = "PC2H+"
    get_names(486) = "C3N+"
    get_names(487) = "C4N+"
    get_names(488) = "C3HN+"
    get_names(489) = "HNC+"
    get_names(490) = "SIC3H+"
    get_names(491) = "SIC4+"
    get_names(492) = "SIC2H2+"
    get_names(493) = "SICH3+"
    get_names(494) = "HC2NCH+"
    get_names(495) = "C3H3+"
    get_names(496) = "H3C3+"
    get_names(497) = "C5H+"
    get_names(498) = "C6+"
    get_names(499) = "C2H3O+"
    get_names(500) = "C2H5+"
    get_names(501) = "C3H3N+"
    get_names(502) = "C5H2+"
    get_names(503) = "C4H3+"
    get_names(504) = "C6H+"
    get_names(505) = "C7+"
    get_names(506) = "CH4N+"
    get_names(507) = "C5HN+"
    get_names(508) = "C7H+"
    get_names(509) = "C8+"
    get_names(510) = "COOCH4+"
    get_names(511) = "C2H5O+"
    get_names(512) = "C8H+"
    get_names(513) = "C9+"
    get_names(514) = "C5H3+"
    get_names(515) = "C6H2+"
    get_names(516) = "C6H3+"
    get_names(517) = "C2H6CO+"
    get_names(518) = "C9H+"
    get_names(519) = "C10+"
    get_names(520) = "C7H3+"
    get_names(521) = "C8H2+"
    get_names(522) = "C8H3+"
    get_names(523) = "HCL+"
    get_names(524) = "HS+"
    get_names(525) = "NH+"
    get_names(526) = "OH+"
    get_names(527) = "PN+"
    get_names(528) = "S2+"
    get_names(529) = "SIH+"
    get_names(530) = "SIO+"
    get_names(531) = "H2O+"
    get_names(532) = "HNSI+"
    get_names(533) = "S2H+"
    get_names(534) = "PH2+"
    get_names(535) = "H2CS+"
    get_names(536) = "H2S2+"
    get_names(537) = "HSIO+"
    get_names(538) = "C4P+"
    get_names(539) = "HCO2+"
    get_names(540) = "PCH3+"
    get_names(541) = "CH4+"
    get_names(542) = "C2NH+"
    get_names(543) = "SIH4+"
    get_names(544) = "NH4+"
    get_names(545) = "H2NC+"
    get_names(546) = "C3H2N+"
    get_names(547) = "C7H2+"
    get_names(548) = "C5H4+"
    get_names(549) = "C7HN+"
    get_names(550) = "C9H2+"
    get_names(551) = "C7H4+"
    get_names(552) = "C9HN+"
    get_names(553) = "N2+"
    get_names(554) = "CO2+"
    get_names(555) = "HEH+"
    get_names(556) = "SO2+"
    get_names(557) = "C6H5+"
    get_names(558) = "C5H5+"
    get_names(559) = "N2H+"
    get_names(560) = "NO2+"
    get_names(561) = "PC2H2+"
    get_names(562) = "PNH2+"
    get_names(563) = "PCH2+"
    get_names(564) = "HC2S+"
    get_names(565) = "HC3S+"
    get_names(566) = "H3CS+"
    get_names(567) = "HC4S+"
    get_names(568) = "SINH2+"
    get_names(569) = "SIC2H3+"
    get_names(570) = "SIC3H2+"
    get_names(571) = "C2HO+"
    get_names(572) = "H3O+"
    get_names(573) = "H3S+"
    get_names(574) = "HOCS+"
    get_names(575) = "CH5O+"
    get_names(576) = "NCO+"
    get_names(577) = "HNCO+"
    get_names(578) = "C2N2+"
    get_names(579) = "H3+"
    get_names(580) = "O2H+"
    get_names(581) = "CH5+"
    get_names(582) = "H2CL+"
    get_names(583) = "CH3O2+"
    get_names(584) = "H2PO+"
    get_names(585) = "PNH3+"
    get_names(586) = "PCH4+"
    get_names(587) = "PC2H3+"
    get_names(588) = "HSIS+"
    get_names(589) = "HSO+"
    get_names(590) = "HNS+"
    get_names(591) = "HPN+"
    get_names(592) = "H2NO+"
    get_names(593) = "NAH2O+"
    get_names(594) = "PH3+"
    get_names(595) = "SINCH+"
    get_names(596) = "HSIO2+"
    get_names(597) = "HSO2+"
    get_names(598) = "HC3O+"
    get_names(599) = "PC3H+"
    get_names(600) = "H3S2+"
    get_names(601) = "H3SIO+"
    get_names(602) = "PC4H+"
    get_names(603) = "NH2CNH+"
    get_names(604) = "SIC4H+"
    get_names(605) = "SICH4+"
    get_names(606) = "SIH5+"
    get_names(607) = "C2H4N+"
    get_names(608) = "NH2CH2O+"
    get_names(609) = "C2H6+"
    get_names(610) = "C3H4N+"
    get_names(611) = "C3H5+"
    get_names(612) = "C4H4+"
    get_names(613) = "CH6N+"
    get_names(614) = "C5H2N+"
    get_names(615) = "C4H4N+"
    get_names(616) = "H5C2O2+"
    get_names(617) = "C2H5OH2+"
    get_names(618) = "CH3OCH4+"
    get_names(619) = "C7H2N+"
    get_names(620) = "C3H6OH+"
    get_names(621) = "C6H4N+"
    get_names(622) = "C10H+"
    get_names(623) = "C9H3+"
    get_names(624) = "C7H5+"
    get_names(625) = "C8H4N+"
    get_names(626) = "C9H2N+"
    get_names(627) = "C6H7+"
    get_names(628) = "NAH2+"
    get_names(629) = "PC2H4+"
    get_names(630) = "C4H5+"
    get_names(631) = "H2CCL+"
    get_names(632) = "PC4H2+"
    get_names(633) = "C6H4+"
    get_names(634) = "C8H4+"
    get_names(635) = "C9H4+"
    get_names(636) = "C4H7+"
    get_names(637) = "HC4N+"
    get_names(638) = "HC4O+"
    get_names(639) = "C5N+"
    get_names(640) = "H2C4N+"
    get_names(641) = "H3C4N+"
    get_names(642) = "C7N+"
    get_names(643) = "C5H3N+"
    get_names(644) = "C10H2+"
    get_names(645) = "C9N+"
    get_names(646) = "C7H3N+"
    get_names(647) = "C9H3N+"
    get_names(648) = "OCS+H2"
    get_names(649) = "H2C3O+"
    get_names(650) = "H3C3O+"
    get_names(651) = "C5H4N+"
    get_names(652) = "C8H5+"
    get_names(653) = "C9H5+"
    get_names(654) = "H2COHOCH2+"
    get_names(655) = "H7C2O2+"
    get_names(656) = "CR"
    get_names(657) = "g"
    get_names(658) = "Tgas"
    get_names(659) = "dummy"

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
        n(idx_OHk) + &
        n(idx_H) + &
        n(idx_CH) + &
        n(idx_H2)*2d0 + &
        n(idx_HCL) + &
        n(idx_HS) + &
        n(idx_MGH) + &
        n(idx_NAH) + &
        n(idx_NH) + &
        n(idx_OH) + &
        n(idx_PH) + &
        n(idx_SIH) + &
        n(idx_C2H) + &
        n(idx_CH2)*2d0 + &
        n(idx_H2O)*2d0 + &
        n(idx_H2S)*2d0 + &
        n(idx_HCN) + &
        n(idx_HCO) + &
        n(idx_HCP) + &
        n(idx_HCS) + &
        n(idx_HCSI) + &
        n(idx_HNC) + &
        n(idx_HNO) + &
        n(idx_HNSI) + &
        n(idx_HPO) + &
        n(idx_HS2) + &
        n(idx_NAOH) + &
        n(idx_NH2)*2d0 + &
        n(idx_O2H) + &
        n(idx_PH2)*2d0 + &
        n(idx_SIH2)*2d0 + &
        n(idx_C2H2)*2d0 + &
        n(idx_C3H) + &
        n(idx_CH3)*3d0 + &
        n(idx_H2CO)*2d0 + &
        n(idx_H2CS)*2d0 + &
        n(idx_H2O2)*2d0 + &
        n(idx_H2S2)*2d0 + &
        n(idx_H2SIO)*2d0 + &
        n(idx_HCCP) + &
        n(idx_NH3)*3d0 + &
        n(idx_SIC2H) + &
        n(idx_SICH2)*2d0 + &
        n(idx_SIH3)*3d0 + &
        n(idx_C2H2N)*2d0 + &
        n(idx_C2H2O)*2d0 + &
        n(idx_C2H3)*3d0 + &
        n(idx_C3H2)*2d0 + &
        n(idx_C4H) + &
        n(idx_CH2O2)*2d0 + &
        n(idx_CH2PH)*3d0 + &
        n(idx_CH3N)*3d0 + &
        n(idx_CH4)*4d0 + &
        n(idx_HC3N) + &
        n(idx_SIC2H2)*2d0 + &
        n(idx_SIC3H) + &
        n(idx_SICH3)*3d0 + &
        n(idx_SIH4)*4d0 + &
        n(idx_C2H3N)*3d0 + &
        n(idx_C2H4)*4d0 + &
        n(idx_C3H3)*3d0 + &
        n(idx_C4H2)*2d0 + &
        n(idx_C5H) + &
        n(idx_CH4O)*4d0 + &
        n(idx_C2H4O)*4d0 + &
        n(idx_C2H5)*5d0 + &
        n(idx_C3H3N)*3d0 + &
        n(idx_C3H4)*4d0 + &
        n(idx_C5H2)*2d0 + &
        n(idx_C6H) + &
        n(idx_CH5N)*5d0 + &
        n(idx_HC5N) + &
        n(idx_C6H2)*2d0 + &
        n(idx_C7H) + &
        n(idx_CH3C3N)*3d0 + &
        n(idx_HCOOCH3)*4d0 + &
        n(idx_C2H5OH)*6d0 + &
        n(idx_C7H2)*2d0 + &
        n(idx_C8H) + &
        n(idx_CH3C4H)*4d0 + &
        n(idx_CH3OCH3)*6d0 + &
        n(idx_HC7N) + &
        n(idx_C2H6CO)*6d0 + &
        n(idx_C8H2)*2d0 + &
        n(idx_C9H) + &
        n(idx_CH3C5N)*3d0 + &
        n(idx_C9H2)*2d0 + &
        n(idx_CH3C6H)*4d0 + &
        n(idx_CH3C7N)*3d0 + &
        n(idx_HC9N) + &
        n(idx_C4H4)*4d0 + &
        n(idx_HCNC2) + &
        n(idx_HC2NC) + &
        n(idx_HNC3) + &
        n(idx_NH2CHO)*3d0 + &
        n(idx_C4H3)*3d0 + &
        n(idx_NH2CN)*2d0 + &
        n(idx_C6H6)*6d0 + &
        n(idx_H2CN)*2d0 + &
        n(idx_FEH) + &
        n(idx_HNCO) + &
        n(idx_HC2O) + &
        n(idx_HCCN) + &
        n(idx_HC3O) + &
        n(idx_MGH2)*2d0 + &
        n(idx_N2H2)*2d0 + &
        n(idx_CHNH)*2d0 + &
        n(idx_H2C3O)*2d0 + &
        n(idx_H2C3N)*2d0 + &
        n(idx_H2C5N)*2d0 + &
        n(idx_H2C7N)*2d0 + &
        n(idx_H2C9N)*2d0 + &
        n(idx_NH2OH)*3d0 + &
        n(idx_CH2OH)*3d0 + &
        n(idx_C5H3)*3d0 + &
        n(idx_H3C5N)*3d0 + &
        n(idx_C6H3)*3d0 + &
        n(idx_C7H3)*3d0 + &
        n(idx_H3C7N)*3d0 + &
        n(idx_C8H3)*3d0 + &
        n(idx_C9H3)*3d0 + &
        n(idx_H3C9N)*3d0 + &
        n(idx_CH3NH)*4d0 + &
        n(idx_H4C3N)*4d0 + &
        n(idx_C5H4)*4d0 + &
        n(idx_C6H4)*4d0 + &
        n(idx_C7H4)*4d0 + &
        n(idx_C8H4)*4d0 + &
        n(idx_C9H4)*4d0 + &
        n(idx_H5C3N)*5d0 + &
        n(idx_C2H6)*6d0 + &
        n(idx_C2H_DUST) + &
        n(idx_C3H_DUST) + &
        n(idx_C2H3_DUST)*3d0 + &
        n(idx_C3H3_DUST)*3d0 + &
        n(idx_C4H_DUST) + &
        n(idx_C5H_DUST) + &
        n(idx_C6H_DUST) + &
        n(idx_C7H_DUST) + &
        n(idx_C8H_DUST) + &
        n(idx_C9H_DUST) + &
        n(idx_CH_DUST) + &
        n(idx_CH2_DUST)*2d0 + &
        n(idx_C2H2_DUST)*2d0 + &
        n(idx_CH3_DUST)*3d0 + &
        n(idx_HS_DUST) + &
        n(idx_H_DUST) + &
        n(idx_NH_DUST) + &
        n(idx_HNC_DUST) + &
        n(idx_NH2_DUST)*2d0 + &
        n(idx_OH_DUST) + &
        n(idx_C3H2_DUST)*2d0 + &
        n(idx_C3H4_DUST)*4d0 + &
        n(idx_C4H2_DUST)*2d0 + &
        n(idx_C5H2_DUST)*2d0 + &
        n(idx_C6H2_DUST)*2d0 + &
        n(idx_C7H2_DUST)*2d0 + &
        n(idx_C8H2_DUST)*2d0 + &
        n(idx_C9H2_DUST)*2d0 + &
        n(idx_C2H4_DUST)*4d0 + &
        n(idx_HCCN_DUST) + &
        n(idx_HNO_DUST) + &
        n(idx_HCN_DUST) + &
        n(idx_CHNH_DUST)*2d0 + &
        n(idx_CH3N_DUST)*3d0 + &
        n(idx_HCO_DUST) + &
        n(idx_C2H5_DUST)*5d0 + &
        n(idx_C2H2N_DUST)*2d0 + &
        n(idx_CH2NH2_DUST)*4d0 + &
        n(idx_H2CO_DUST)*2d0 + &
        n(idx_CH3C3N_DUST)*3d0 + &
        n(idx_CH3C5N_DUST)*3d0 + &
        n(idx_CH3C7N_DUST)*3d0 + &
        n(idx_CH2OH_DUST)*3d0 + &
        n(idx_C2H5OH_DUST)*6d0 + &
        n(idx_CH3OCH3_DUST)*6d0 + &
        n(idx_C2H6_DUST)*6d0 + &
        n(idx_C2H3N_DUST)*3d0 + &
        n(idx_C2H4O_DUST)*4d0 + &
        n(idx_CH4_DUST)*4d0 + &
        n(idx_H2_DUST)*2d0 + &
        n(idx_HC2O_DUST) + &
        n(idx_C3H3N_DUST)*3d0 + &
        n(idx_H4C3N_DUST)*4d0 + &
        n(idx_HC3N_DUST) + &
        n(idx_HC3O_DUST) + &
        n(idx_C4H3_DUST)*3d0 + &
        n(idx_C4H4_DUST)*4d0 + &
        n(idx_C5H3_DUST)*3d0 + &
        n(idx_C5H4_DUST)*4d0 + &
        n(idx_HC5N_DUST) + &
        n(idx_C6H3_DUST)*3d0 + &
        n(idx_C6H4_DUST)*4d0 + &
        n(idx_C7H3_DUST)*3d0 + &
        n(idx_C7H4_DUST)*4d0 + &
        n(idx_HC7N_DUST) + &
        n(idx_C8H3_DUST)*3d0 + &
        n(idx_C8H4_DUST)*4d0 + &
        n(idx_C9H3_DUST)*3d0 + &
        n(idx_C9H4_DUST)*4d0 + &
        n(idx_HC9N_DUST) + &
        n(idx_CH3NH_DUST)*4d0 + &
        n(idx_CH5N_DUST)*5d0 + &
        n(idx_CH4O_DUST)*4d0 + &
        n(idx_HCS_DUST) + &
        n(idx_FEH_DUST) + &
        n(idx_H2C3N_DUST)*2d0 + &
        n(idx_H2C5N_DUST)*2d0 + &
        n(idx_H3C5N_DUST)*3d0 + &
        n(idx_H2C7N_DUST)*2d0 + &
        n(idx_H3C7N_DUST)*3d0 + &
        n(idx_H2C9N_DUST)*2d0 + &
        n(idx_H3C9N_DUST)*3d0 + &
        n(idx_H2CN_DUST)*2d0 + &
        n(idx_H2O2_DUST)*2d0 + &
        n(idx_H2O_DUST)*2d0 + &
        n(idx_O2H_DUST) + &
        n(idx_H2S_DUST)*2d0 + &
        n(idx_H5C3N_DUST)*5d0 + &
        n(idx_C2H2O_DUST)*2d0 + &
        n(idx_H2C3O_DUST)*2d0 + &
        n(idx_H2CS_DUST)*2d0 + &
        n(idx_MGH_DUST) + &
        n(idx_MGH2_DUST)*2d0 + &
        n(idx_N2H2_DUST)*2d0 + &
        n(idx_NAH_DUST) + &
        n(idx_NH3_DUST)*3d0 + &
        n(idx_HNCO_DUST) + &
        n(idx_SIH_DUST) + &
        n(idx_SIH2_DUST)*2d0 + &
        n(idx_SIH3_DUST)*3d0 + &
        n(idx_SIH4_DUST)*4d0 + &
        n(idx_HCOOCH3_DUST)*4d0 + &
        n(idx_NH2CHO_DUST)*3d0 + &
        n(idx_CH2O2_DUST)*2d0 + &
        n(idx_NH2OH_DUST)*3d0 + &
        n(idx_C6H6_DUST)*6d0 + &
        n(idx_CH2NH2)*4d0 + &
        n(idx_CH3C4H_DUST)*4d0 + &
        n(idx_CH3C6H_DUST)*4d0 + &
        n(idx_H2S2_DUST)*2d0 + &
        n(idx_HC2NC_DUST) + &
        n(idx_HCNC2_DUST) + &
        n(idx_HNC3_DUST) + &
        n(idx_HS2_DUST) + &
        n(idx_NAOH_DUST) + &
        n(idx_C2H6CO_DUST)*6d0 + &
        n(idx_CH2PH_DUST)*3d0 + &
        n(idx_HCP_DUST) + &
        n(idx_H2SIO_DUST)*2d0 + &
        n(idx_HCCP_DUST) + &
        n(idx_HCL_DUST) + &
        n(idx_HCSI_DUST) + &
        n(idx_HNSI_DUST) + &
        n(idx_HPO_DUST) + &
        n(idx_NH2CN_DUST)*2d0 + &
        n(idx_PH_DUST) + &
        n(idx_PH2_DUST)*2d0 + &
        n(idx_SIC2H_DUST) + &
        n(idx_SIC2H2_DUST)*2d0 + &
        n(idx_SIC3H_DUST) + &
        n(idx_SICH2_DUST)*2d0 + &
        n(idx_SICH3_DUST)*3d0 + &
        n(idx_Hj) + &
        n(idx_H2j)*2d0 + &
        n(idx_CH2j)*2d0 + &
        n(idx_H2Sj)*2d0 + &
        n(idx_HCOj) + &
        n(idx_HCSj) + &
        n(idx_HNOj) + &
        n(idx_NH2j)*2d0 + &
        n(idx_C2H2j)*2d0 + &
        n(idx_CH3j)*3d0 + &
        n(idx_NH3j)*3d0 + &
        n(idx_C2H2Oj)*2d0 + &
        n(idx_CH2O2j)*2d0 + &
        n(idx_C2H3Nj)*3d0 + &
        n(idx_C2H4j)*4d0 + &
        n(idx_C4H2j)*2d0 + &
        n(idx_H3COj)*3d0 + &
        n(idx_CH4Oj)*4d0 + &
        n(idx_C2H4Oj)*4d0 + &
        n(idx_C3H4j)*4d0 + &
        n(idx_CH5Nj)*5d0 + &
        n(idx_C2H5OHj)*6d0 + &
        n(idx_CH3OCH3j)*6d0 + &
        n(idx_CHj) + &
        n(idx_PHj) + &
        n(idx_C2Hj) + &
        n(idx_HOCj) + &
        n(idx_HCPj) + &
        n(idx_HPOj) + &
        n(idx_HCNj) + &
        n(idx_CHSIj) + &
        n(idx_SIH2j)*2d0 + &
        n(idx_C3Hj) + &
        n(idx_H2COj)*2d0 + &
        n(idx_H2SIOj)*2d0 + &
        n(idx_HCNHj)*2d0 + &
        n(idx_SIC2Hj) + &
        n(idx_CH2SIj)*2d0 + &
        n(idx_SIH3j)*3d0 + &
        n(idx_C2H2Nj)*2d0 + &
        n(idx_C2H3j)*3d0 + &
        n(idx_C3H2j)*2d0 + &
        n(idx_H2C3j)*2d0 + &
        n(idx_C4Hj) + &
        n(idx_PC2Hj) + &
        n(idx_C3HNj) + &
        n(idx_HNCj) + &
        n(idx_SIC3Hj) + &
        n(idx_SIC2H2j)*2d0 + &
        n(idx_SICH3j)*3d0 + &
        n(idx_HC2NCHj)*2d0 + &
        n(idx_C3H3j)*3d0 + &
        n(idx_H3C3j)*3d0 + &
        n(idx_C5Hj) + &
        n(idx_C2H3Oj)*3d0 + &
        n(idx_C2H5j)*5d0 + &
        n(idx_C3H3Nj)*3d0 + &
        n(idx_C5H2j)*2d0 + &
        n(idx_C4H3j)*3d0 + &
        n(idx_C6Hj) + &
        n(idx_CH4Nj)*4d0 + &
        n(idx_C5HNj) + &
        n(idx_C7Hj) + &
        n(idx_COOCH4j)*4d0 + &
        n(idx_C2H5Oj)*5d0 + &
        n(idx_C8Hj) + &
        n(idx_C5H3j)*3d0 + &
        n(idx_C6H2j)*2d0 + &
        n(idx_C6H3j)*3d0 + &
        n(idx_C2H6COj)*6d0 + &
        n(idx_C9Hj) + &
        n(idx_C7H3j)*3d0 + &
        n(idx_C8H2j)*2d0 + &
        n(idx_C8H3j)*3d0 + &
        n(idx_HCLj) + &
        n(idx_HSj) + &
        n(idx_NHj) + &
        n(idx_OHj) + &
        n(idx_SIHj) + &
        n(idx_H2Oj)*2d0 + &
        n(idx_HNSIj) + &
        n(idx_S2Hj) + &
        n(idx_PH2j)*2d0 + &
        n(idx_H2CSj)*2d0 + &
        n(idx_H2S2j)*2d0 + &
        n(idx_HSIOj) + &
        n(idx_HCO2j) + &
        n(idx_PCH3j)*3d0 + &
        n(idx_CH4j)*4d0 + &
        n(idx_C2NHj) + &
        n(idx_SIH4j)*4d0 + &
        n(idx_NH4j)*4d0 + &
        n(idx_H2NCj)*2d0 + &
        n(idx_C3H2Nj)*2d0 + &
        n(idx_C7H2j)*2d0 + &
        n(idx_C5H4j)*4d0 + &
        n(idx_C7HNj) + &
        n(idx_C9H2j)*2d0 + &
        n(idx_C7H4j)*4d0 + &
        n(idx_C9HNj) + &
        n(idx_HEHj) + &
        n(idx_C6H5j)*5d0 + &
        n(idx_C5H5j)*5d0 + &
        n(idx_N2Hj) + &
        n(idx_PC2H2j)*2d0 + &
        n(idx_PNH2j)*2d0 + &
        n(idx_PCH2j)*2d0 + &
        n(idx_HC2Sj) + &
        n(idx_HC3Sj) + &
        n(idx_H3CSj)*3d0 + &
        n(idx_HC4Sj) + &
        n(idx_SINH2j)*2d0 + &
        n(idx_SIC2H3j)*3d0 + &
        n(idx_SIC3H2j)*2d0 + &
        n(idx_C2HOj) + &
        n(idx_H3Oj)*3d0 + &
        n(idx_H3Sj)*3d0 + &
        n(idx_HOCSj) + &
        n(idx_CH5Oj)*5d0 + &
        n(idx_HNCOj) + &
        n(idx_H3j)*3d0 + &
        n(idx_O2Hj) + &
        n(idx_CH5j)*5d0 + &
        n(idx_H2CLj)*2d0 + &
        n(idx_CH3O2j)*3d0 + &
        n(idx_H2POj)*2d0 + &
        n(idx_PNH3j)*3d0 + &
        n(idx_PCH4j)*4d0 + &
        n(idx_PC2H3j)*3d0 + &
        n(idx_HSISj) + &
        n(idx_HSOj) + &
        n(idx_HNSj) + &
        n(idx_HPNj) + &
        n(idx_H2NOj)*2d0 + &
        n(idx_NAH2Oj)*2d0 + &
        n(idx_PH3j)*3d0 + &
        n(idx_SINCHj) + &
        n(idx_HSIO2j) + &
        n(idx_HSO2j) + &
        n(idx_HC3Oj) + &
        n(idx_PC3Hj) + &
        n(idx_H3S2j)*3d0 + &
        n(idx_H3SIOj)*3d0 + &
        n(idx_PC4Hj) + &
        n(idx_NH2CNHj)*3d0 + &
        n(idx_SIC4Hj) + &
        n(idx_SICH4j)*4d0 + &
        n(idx_SIH5j)*5d0 + &
        n(idx_C2H4Nj)*4d0 + &
        n(idx_NH2CH2Oj)*4d0 + &
        n(idx_C2H6j)*6d0 + &
        n(idx_C3H4Nj)*4d0 + &
        n(idx_C3H5j)*5d0 + &
        n(idx_C4H4j)*4d0 + &
        n(idx_CH6Nj)*6d0 + &
        n(idx_C5H2Nj)*2d0 + &
        n(idx_C4H4Nj)*4d0 + &
        n(idx_H5C2O2j)*5d0 + &
        n(idx_C2H5OH2j)*7d0 + &
        n(idx_CH3OCH4j)*7d0 + &
        n(idx_C7H2Nj)*2d0 + &
        n(idx_C3H6OHj)*7d0 + &
        n(idx_C6H4Nj)*4d0 + &
        n(idx_C10Hj) + &
        n(idx_C9H3j)*3d0 + &
        n(idx_C7H5j)*5d0 + &
        n(idx_C8H4Nj)*4d0 + &
        n(idx_C9H2Nj)*2d0 + &
        n(idx_C6H7j)*7d0 + &
        n(idx_NAH2j)*2d0 + &
        n(idx_PC2H4j)*4d0 + &
        n(idx_C4H5j)*5d0 + &
        n(idx_H2CCLj)*2d0 + &
        n(idx_PC4H2j)*2d0 + &
        n(idx_C6H4j)*4d0 + &
        n(idx_C8H4j)*4d0 + &
        n(idx_C9H4j)*4d0 + &
        n(idx_C4H7j)*7d0 + &
        n(idx_HC4Nj) + &
        n(idx_HC4Oj) + &
        n(idx_H2C4Nj)*2d0 + &
        n(idx_H3C4Nj)*3d0 + &
        n(idx_C5H3Nj)*3d0 + &
        n(idx_C10H2j)*2d0 + &
        n(idx_C7H3Nj)*3d0 + &
        n(idx_C9H3Nj)*3d0 + &
        n(idx_OCSjH2)*2d0 + &
        n(idx_H2C3Oj)*2d0 + &
        n(idx_H3C3Oj)*3d0 + &
        n(idx_C5H4Nj)*4d0 + &
        n(idx_C8H5j)*5d0 + &
        n(idx_C9H5j)*5d0 + &
        n(idx_H2COHOCH2j)*5d0 + &
        n(idx_H7C2O2j)*7d0
    get_Hnuclei = nH

  end function get_Hnuclei

  !***************************
  function get_zatoms()
    use krome_commons
    implicit none
    integer::get_zatoms(nspec)

    get_zatoms(1) = 0	!E
    get_zatoms(2) = 1	!H-
    get_zatoms(3) = 6	!C-
    get_zatoms(4) = 13	!CN-
    get_zatoms(5) = 8	!O-
    get_zatoms(6) = 9	!OH-
    get_zatoms(7) = 16	!S-
    get_zatoms(8) = 0	!GRAIN-
    get_zatoms(9) = 6	!C
    get_zatoms(10) = 17	!CL
    get_zatoms(11) = 26	!FE
    get_zatoms(12) = 1	!H
    get_zatoms(13) = 2	!HE
    get_zatoms(14) = 12	!MG
    get_zatoms(15) = 7	!N
    get_zatoms(16) = 11	!NA
    get_zatoms(17) = 8	!O
    get_zatoms(18) = 15	!P
    get_zatoms(19) = 16	!S
    get_zatoms(20) = 14	!SI
    get_zatoms(21) = 12	!C2
    get_zatoms(22) = 23	!CCL
    get_zatoms(23) = 7	!CH
    get_zatoms(24) = 25	!CLO
    get_zatoms(25) = 13	!CN
    get_zatoms(26) = 14	!CO
    get_zatoms(27) = 21	!CP
    get_zatoms(28) = 22	!CS
    get_zatoms(29) = 2	!H2
    get_zatoms(30) = 18	!HCL
    get_zatoms(31) = 17	!HS
    get_zatoms(32) = 13	!MGH
    get_zatoms(33) = 14	!N2
    get_zatoms(34) = 12	!NAH
    get_zatoms(35) = 8	!NH
    get_zatoms(36) = 15	!NO
    get_zatoms(37) = 23	!NS
    get_zatoms(38) = 16	!O2
    get_zatoms(39) = 9	!OH
    get_zatoms(40) = 16	!PH
    get_zatoms(41) = 22	!PN
    get_zatoms(42) = 23	!PO
    get_zatoms(43) = 32	!S2
    get_zatoms(44) = 20	!SIC
    get_zatoms(45) = 15	!SIH
    get_zatoms(46) = 21	!SIN
    get_zatoms(47) = 22	!SIO
    get_zatoms(48) = 30	!SIS
    get_zatoms(49) = 24	!SO
    get_zatoms(50) = 13	!C2H
    get_zatoms(51) = 19	!C2N
    get_zatoms(52) = 28	!C2S
    get_zatoms(53) = 18	!C3
    get_zatoms(54) = 20	!CCO
    get_zatoms(55) = 27	!CCP
    get_zatoms(56) = 8	!CH2
    get_zatoms(57) = 22	!CO2
    get_zatoms(58) = 10	!H2O
    get_zatoms(59) = 18	!H2S
    get_zatoms(60) = 14	!HCN
    get_zatoms(61) = 15	!HCO
    get_zatoms(62) = 22	!HCP
    get_zatoms(63) = 23	!HCS
    get_zatoms(64) = 21	!HCSI
    get_zatoms(65) = 14	!HNC
    get_zatoms(66) = 16	!HNO
    get_zatoms(67) = 22	!HNSI
    get_zatoms(68) = 24	!HPO
    get_zatoms(69) = 33	!HS2
    get_zatoms(70) = 22	!N2O
    get_zatoms(71) = 20	!NAOH
    get_zatoms(72) = 9	!NH2
    get_zatoms(73) = 23	!NO2
    get_zatoms(74) = 17	!O2H
    get_zatoms(75) = 21	!OCN
    get_zatoms(76) = 30	!OCS
    get_zatoms(77) = 17	!PH2
    get_zatoms(78) = 26	!SIC2
    get_zatoms(79) = 16	!SIH2
    get_zatoms(80) = 27	!SINC
    get_zatoms(81) = 30	!SIO2
    get_zatoms(82) = 32	!SO2
    get_zatoms(83) = 14	!C2H2
    get_zatoms(84) = 19	!C3H
    get_zatoms(85) = 25	!C3N
    get_zatoms(86) = 26	!C3O
    get_zatoms(87) = 33	!C3P
    get_zatoms(88) = 34	!C3S
    get_zatoms(89) = 24	!C4
    get_zatoms(90) = 9	!CH3
    get_zatoms(91) = 16	!H2CO
    get_zatoms(92) = 24	!H2CS
    get_zatoms(93) = 18	!H2O2
    get_zatoms(94) = 34	!H2S2
    get_zatoms(95) = 24	!H2SIO
    get_zatoms(96) = 28	!HCCP
    get_zatoms(97) = 10	!NH3
    get_zatoms(98) = 27	!SIC2H
    get_zatoms(99) = 32	!SIC3
    get_zatoms(100) = 22	!SICH2
    get_zatoms(101) = 17	!SIH3
    get_zatoms(102) = 21	!C2H2N
    get_zatoms(103) = 22	!C2H2O
    get_zatoms(104) = 15	!C2H3
    get_zatoms(105) = 20	!C3H2
    get_zatoms(106) = 25	!C4H
    get_zatoms(107) = 31	!C4N
    get_zatoms(108) = 39	!C4P
    get_zatoms(109) = 40	!C4S
    get_zatoms(110) = 30	!C5
    get_zatoms(111) = 24	!CH2O2
    get_zatoms(112) = 24	!CH2PH
    get_zatoms(113) = 16	!CH3N
    get_zatoms(114) = 10	!CH4
    get_zatoms(115) = 26	!HC3N
    get_zatoms(116) = 28	!SIC2H2
    get_zatoms(117) = 33	!SIC3H
    get_zatoms(118) = 38	!SIC4
    get_zatoms(119) = 23	!SICH3
    get_zatoms(120) = 18	!SIH4
    get_zatoms(121) = 22	!C2H3N
    get_zatoms(122) = 16	!C2H4
    get_zatoms(123) = 21	!C3H3
    get_zatoms(124) = 26	!C4H2
    get_zatoms(125) = 31	!C5H
    get_zatoms(126) = 37	!C5N
    get_zatoms(127) = 36	!C6
    get_zatoms(128) = 18	!CH4O
    get_zatoms(129) = 24	!C2H4O
    get_zatoms(130) = 17	!C2H5
    get_zatoms(131) = 28	!C3H3N
    get_zatoms(132) = 22	!C3H4
    get_zatoms(133) = 32	!C5H2
    get_zatoms(134) = 37	!C6H
    get_zatoms(135) = 42	!C7
    get_zatoms(136) = 18	!CH5N
    get_zatoms(137) = 38	!HC5N
    get_zatoms(138) = 38	!C6H2
    get_zatoms(139) = 43	!C7H
    get_zatoms(140) = 49	!C7N
    get_zatoms(141) = 48	!C8
    get_zatoms(142) = 34	!CH3C3N
    get_zatoms(143) = 32	!HCOOCH3
    get_zatoms(144) = 26	!C2H5OH
    get_zatoms(145) = 44	!C7H2
    get_zatoms(146) = 49	!C8H
    get_zatoms(147) = 54	!C9
    get_zatoms(148) = 34	!CH3C4H
    get_zatoms(149) = 26	!CH3OCH3
    get_zatoms(150) = 50	!HC7N
    get_zatoms(151) = 32	!C2H6CO
    get_zatoms(152) = 50	!C8H2
    get_zatoms(153) = 55	!C9H
    get_zatoms(154) = 61	!C9N
    get_zatoms(155) = 60	!C10
    get_zatoms(156) = 46	!CH3C5N
    get_zatoms(157) = 56	!C9H2
    get_zatoms(158) = 46	!CH3C6H
    get_zatoms(159) = 58	!CH3C7N
    get_zatoms(160) = 62	!HC9N
    get_zatoms(161) = 28	!C4H4
    get_zatoms(162) = 26	!HCNC2
    get_zatoms(163) = 26	!HC2NC
    get_zatoms(164) = 26	!HNC3
    get_zatoms(165) = 24	!NH2CHO
    get_zatoms(166) = 27	!C4H3
    get_zatoms(167) = 22	!NH2CN
    get_zatoms(168) = 42	!C6H6
    get_zatoms(169) = 15	!H2CN
    get_zatoms(170) = 0	!GRAIN0
    get_zatoms(171) = 24	!O3
    get_zatoms(172) = 27	!FEH
    get_zatoms(173) = 22	!HNCO
    get_zatoms(174) = 21	!HC2O
    get_zatoms(175) = 20	!HCCN
    get_zatoms(176) = 27	!HC3O
    get_zatoms(177) = 14	!MGH2
    get_zatoms(178) = 16	!N2H2
    get_zatoms(179) = 15	!CHNH
    get_zatoms(180) = 28	!H2C3O
    get_zatoms(181) = 27	!H2C3N
    get_zatoms(182) = 39	!H2C5N
    get_zatoms(183) = 51	!H2C7N
    get_zatoms(184) = 63	!H2C9N
    get_zatoms(185) = 18	!NH2OH
    get_zatoms(186) = 17	!CH2OH
    get_zatoms(187) = 33	!C5H3
    get_zatoms(188) = 40	!H3C5N
    get_zatoms(189) = 39	!C6H3
    get_zatoms(190) = 45	!C7H3
    get_zatoms(191) = 52	!H3C7N
    get_zatoms(192) = 51	!C8H3
    get_zatoms(193) = 57	!C9H3
    get_zatoms(194) = 64	!H3C9N
    get_zatoms(195) = 17	!CH3NH
    get_zatoms(196) = 29	!H4C3N
    get_zatoms(197) = 34	!C5H4
    get_zatoms(198) = 40	!C6H4
    get_zatoms(199) = 46	!C7H4
    get_zatoms(200) = 52	!C8H4
    get_zatoms(201) = 58	!C9H4
    get_zatoms(202) = 30	!H5C3N
    get_zatoms(203) = 18	!C2H6
    get_zatoms(204) = 6	!C_DUST
    get_zatoms(205) = 12	!C2_DUST
    get_zatoms(206) = 18	!C3_DUST
    get_zatoms(207) = 13	!C2H_DUST
    get_zatoms(208) = 19	!C3H_DUST
    get_zatoms(209) = 15	!C2H3_DUST
    get_zatoms(210) = 21	!C3H3_DUST
    get_zatoms(211) = 19	!C2N_DUST
    get_zatoms(212) = 25	!C3N_DUST
    get_zatoms(213) = 20	!CCO_DUST
    get_zatoms(214) = 26	!C3O_DUST
    get_zatoms(215) = 28	!C2S_DUST
    get_zatoms(216) = 34	!C3S_DUST
    get_zatoms(217) = 24	!C4_DUST
    get_zatoms(218) = 25	!C4H_DUST
    get_zatoms(219) = 30	!C5_DUST
    get_zatoms(220) = 31	!C5H_DUST
    get_zatoms(221) = 36	!C6_DUST
    get_zatoms(222) = 37	!C6H_DUST
    get_zatoms(223) = 42	!C7_DUST
    get_zatoms(224) = 43	!C7H_DUST
    get_zatoms(225) = 48	!C8_DUST
    get_zatoms(226) = 49	!C8H_DUST
    get_zatoms(227) = 54	!C9_DUST
    get_zatoms(228) = 55	!C9H_DUST
    get_zatoms(229) = 60	!C10_DUST
    get_zatoms(230) = 7	!CH_DUST
    get_zatoms(231) = 8	!CH2_DUST
    get_zatoms(232) = 14	!C2H2_DUST
    get_zatoms(233) = 9	!CH3_DUST
    get_zatoms(234) = 13	!CN_DUST
    get_zatoms(235) = 17	!HS_DUST
    get_zatoms(236) = 22	!CS_DUST
    get_zatoms(237) = 1	!H_DUST
    get_zatoms(238) = 7	!N_DUST
    get_zatoms(239) = 8	!NH_DUST
    get_zatoms(240) = 14	!HNC_DUST
    get_zatoms(241) = 9	!NH2_DUST
    get_zatoms(242) = 15	!NO_DUST
    get_zatoms(243) = 8	!O_DUST
    get_zatoms(244) = 21	!OCN_DUST
    get_zatoms(245) = 23	!NS_DUST
    get_zatoms(246) = 16	!S_DUST
    get_zatoms(247) = 14	!CO_DUST
    get_zatoms(248) = 16	!O2_DUST
    get_zatoms(249) = 9	!OH_DUST
    get_zatoms(250) = 24	!SO_DUST
    get_zatoms(251) = 20	!C3H2_DUST
    get_zatoms(252) = 22	!C3H4_DUST
    get_zatoms(253) = 26	!C4H2_DUST
    get_zatoms(254) = 32	!C5H2_DUST
    get_zatoms(255) = 38	!C6H2_DUST
    get_zatoms(256) = 44	!C7H2_DUST
    get_zatoms(257) = 50	!C8H2_DUST
    get_zatoms(258) = 56	!C9H2_DUST
    get_zatoms(259) = 16	!C2H4_DUST
    get_zatoms(260) = 20	!HCCN_DUST
    get_zatoms(261) = 16	!HNO_DUST
    get_zatoms(262) = 14	!HCN_DUST
    get_zatoms(263) = 15	!CHNH_DUST
    get_zatoms(264) = 16	!CH3N_DUST
    get_zatoms(265) = 15	!HCO_DUST
    get_zatoms(266) = 17	!C2H5_DUST
    get_zatoms(267) = 21	!C2H2N_DUST
    get_zatoms(268) = 17	!CH2NH2_DUST
    get_zatoms(269) = 16	!H2CO_DUST
    get_zatoms(270) = 34	!CH3C3N_DUST
    get_zatoms(271) = 37	!C5N_DUST
    get_zatoms(272) = 46	!CH3C5N_DUST
    get_zatoms(273) = 49	!C7N_DUST
    get_zatoms(274) = 58	!CH3C7N_DUST
    get_zatoms(275) = 17	!CH2OH_DUST
    get_zatoms(276) = 26	!C2H5OH_DUST
    get_zatoms(277) = 26	!CH3OCH3_DUST
    get_zatoms(278) = 18	!C2H6_DUST
    get_zatoms(279) = 22	!C2H3N_DUST
    get_zatoms(280) = 24	!C2H4O_DUST
    get_zatoms(281) = 10	!CH4_DUST
    get_zatoms(282) = 2	!H2_DUST
    get_zatoms(283) = 21	!HC2O_DUST
    get_zatoms(284) = 28	!C3H3N_DUST
    get_zatoms(285) = 29	!H4C3N_DUST
    get_zatoms(286) = 26	!HC3N_DUST
    get_zatoms(287) = 27	!HC3O_DUST
    get_zatoms(288) = 27	!C4H3_DUST
    get_zatoms(289) = 28	!C4H4_DUST
    get_zatoms(290) = 33	!C5H3_DUST
    get_zatoms(291) = 34	!C5H4_DUST
    get_zatoms(292) = 38	!HC5N_DUST
    get_zatoms(293) = 39	!C6H3_DUST
    get_zatoms(294) = 40	!C6H4_DUST
    get_zatoms(295) = 45	!C7H3_DUST
    get_zatoms(296) = 46	!C7H4_DUST
    get_zatoms(297) = 50	!HC7N_DUST
    get_zatoms(298) = 51	!C8H3_DUST
    get_zatoms(299) = 52	!C8H4_DUST
    get_zatoms(300) = 57	!C9H3_DUST
    get_zatoms(301) = 58	!C9H4_DUST
    get_zatoms(302) = 61	!C9N_DUST
    get_zatoms(303) = 62	!HC9N_DUST
    get_zatoms(304) = 17	!CH3NH_DUST
    get_zatoms(305) = 18	!CH5N_DUST
    get_zatoms(306) = 18	!CH4O_DUST
    get_zatoms(307) = 23	!HCS_DUST
    get_zatoms(308) = 26	!FE_DUST
    get_zatoms(309) = 27	!FEH_DUST
    get_zatoms(310) = 27	!H2C3N_DUST
    get_zatoms(311) = 39	!H2C5N_DUST
    get_zatoms(312) = 40	!H3C5N_DUST
    get_zatoms(313) = 51	!H2C7N_DUST
    get_zatoms(314) = 52	!H3C7N_DUST
    get_zatoms(315) = 63	!H2C9N_DUST
    get_zatoms(316) = 64	!H3C9N_DUST
    get_zatoms(317) = 15	!H2CN_DUST
    get_zatoms(318) = 18	!H2O2_DUST
    get_zatoms(319) = 10	!H2O_DUST
    get_zatoms(320) = 17	!O2H_DUST
    get_zatoms(321) = 18	!H2S_DUST
    get_zatoms(322) = 30	!H5C3N_DUST
    get_zatoms(323) = 22	!C2H2O_DUST
    get_zatoms(324) = 28	!H2C3O_DUST
    get_zatoms(325) = 24	!H2CS_DUST
    get_zatoms(326) = 12	!MG_DUST
    get_zatoms(327) = 13	!MGH_DUST
    get_zatoms(328) = 14	!MGH2_DUST
    get_zatoms(329) = 16	!N2H2_DUST
    get_zatoms(330) = 14	!N2_DUST
    get_zatoms(331) = 11	!NA_DUST
    get_zatoms(332) = 12	!NAH_DUST
    get_zatoms(333) = 10	!NH3_DUST
    get_zatoms(334) = 24	!O3_DUST
    get_zatoms(335) = 22	!HNCO_DUST
    get_zatoms(336) = 30	!OCS_DUST
    get_zatoms(337) = 14	!SI_DUST
    get_zatoms(338) = 15	!SIH_DUST
    get_zatoms(339) = 16	!SIH2_DUST
    get_zatoms(340) = 17	!SIH3_DUST
    get_zatoms(341) = 18	!SIH4_DUST
    get_zatoms(342) = 32	!SO2_DUST
    get_zatoms(343) = 32	!HCOOCH3_DUST
    get_zatoms(344) = 24	!NH2CHO_DUST
    get_zatoms(345) = 22	!CO2_DUST
    get_zatoms(346) = 24	!CH2O2_DUST
    get_zatoms(347) = 18	!NH2OH_DUST
    get_zatoms(348) = 31	!C4N_DUST
    get_zatoms(349) = 40	!C4S_DUST
    get_zatoms(350) = 42	!C6H6_DUST
    get_zatoms(351) = 17	!CH2NH2
    get_zatoms(352) = 34	!CH3C4H_DUST
    get_zatoms(353) = 46	!CH3C6H_DUST
    get_zatoms(354) = 34	!H2S2_DUST
    get_zatoms(355) = 26	!HC2NC_DUST
    get_zatoms(356) = 26	!HCNC2_DUST
    get_zatoms(357) = 2	!HE_DUST
    get_zatoms(358) = 26	!HNC3_DUST
    get_zatoms(359) = 33	!HS2_DUST
    get_zatoms(360) = 20	!NAOH_DUST
    get_zatoms(361) = 32	!S2_DUST
    get_zatoms(362) = 20	!SIC_DUST
    get_zatoms(363) = 22	!SIO_DUST
    get_zatoms(364) = 30	!SIS_DUST
    get_zatoms(365) = 32	!C2H6CO_DUST
    get_zatoms(366) = 33	!C3P_DUST
    get_zatoms(367) = 27	!CCP_DUST
    get_zatoms(368) = 39	!C4P_DUST
    get_zatoms(369) = 23	!CCL_DUST
    get_zatoms(370) = 17	!CL_DUST
    get_zatoms(371) = 15	!P_DUST
    get_zatoms(372) = 21	!CP_DUST
    get_zatoms(373) = 24	!CH2PH_DUST
    get_zatoms(374) = 22	!HCP_DUST
    get_zatoms(375) = 25	!CLO_DUST
    get_zatoms(376) = 24	!H2SIO_DUST
    get_zatoms(377) = 28	!HCCP_DUST
    get_zatoms(378) = 18	!HCL_DUST
    get_zatoms(379) = 21	!HCSI_DUST
    get_zatoms(380) = 22	!HNSI_DUST
    get_zatoms(381) = 21	!SIN_DUST
    get_zatoms(382) = 24	!HPO_DUST
    get_zatoms(383) = 23	!PO_DUST
    get_zatoms(384) = 22	!N2O_DUST
    get_zatoms(385) = 22	!NH2CN_DUST
    get_zatoms(386) = 23	!NO2_DUST
    get_zatoms(387) = 16	!PH_DUST
    get_zatoms(388) = 17	!PH2_DUST
    get_zatoms(389) = 22	!PN_DUST
    get_zatoms(390) = 26	!SIC2_DUST
    get_zatoms(391) = 27	!SIC2H_DUST
    get_zatoms(392) = 28	!SIC2H2_DUST
    get_zatoms(393) = 32	!SIC3_DUST
    get_zatoms(394) = 33	!SIC3H_DUST
    get_zatoms(395) = 38	!SIC4_DUST
    get_zatoms(396) = 22	!SICH2_DUST
    get_zatoms(397) = 23	!SICH3_DUST
    get_zatoms(398) = 27	!SINC_DUST
    get_zatoms(399) = 30	!SIO2_DUST
    get_zatoms(400) = 6	!C+
    get_zatoms(401) = 17	!CL+
    get_zatoms(402) = 26	!FE+
    get_zatoms(403) = 1	!H+
    get_zatoms(404) = 2	!HE+
    get_zatoms(405) = 12	!MG+
    get_zatoms(406) = 7	!N+
    get_zatoms(407) = 11	!NA+
    get_zatoms(408) = 8	!O+
    get_zatoms(409) = 15	!P+
    get_zatoms(410) = 16	!S+
    get_zatoms(411) = 14	!SI+
    get_zatoms(412) = 14	!CO+
    get_zatoms(413) = 2	!H2+
    get_zatoms(414) = 15	!NO+
    get_zatoms(415) = 16	!O2+
    get_zatoms(416) = 8	!CH2+
    get_zatoms(417) = 18	!H2S+
    get_zatoms(418) = 15	!HCO+
    get_zatoms(419) = 23	!HCS+
    get_zatoms(420) = 16	!HNO+
    get_zatoms(421) = 9	!NH2+
    get_zatoms(422) = 30	!OCS+
    get_zatoms(423) = 14	!C2H2+
    get_zatoms(424) = 9	!CH3+
    get_zatoms(425) = 10	!NH3+
    get_zatoms(426) = 22	!C2H2O+
    get_zatoms(427) = 24	!CH2O2+
    get_zatoms(428) = 22	!C2H3N+
    get_zatoms(429) = 16	!C2H4+
    get_zatoms(430) = 26	!C4H2+
    get_zatoms(431) = 17	!H3CO+
    get_zatoms(432) = 18	!CH4O+
    get_zatoms(433) = 24	!C2H4O+
    get_zatoms(434) = 22	!C3H4+
    get_zatoms(435) = 18	!CH5N+
    get_zatoms(436) = 26	!C2H5OH+
    get_zatoms(437) = 26	!CH3OCH3+
    get_zatoms(438) = 7	!CH+
    get_zatoms(439) = 23	!CCL+
    get_zatoms(440) = 12	!C2+
    get_zatoms(441) = 25	!CLO+
    get_zatoms(442) = 21	!CP+
    get_zatoms(443) = 22	!CS+
    get_zatoms(444) = 13	!CN+
    get_zatoms(445) = 23	!NS+
    get_zatoms(446) = 16	!PH+
    get_zatoms(447) = 23	!PO+
    get_zatoms(448) = 20	!SIC+
    get_zatoms(449) = 21	!SIN+
    get_zatoms(450) = 30	!SIS+
    get_zatoms(451) = 24	!SO+
    get_zatoms(452) = 18	!C3+
    get_zatoms(453) = 28	!C2S+
    get_zatoms(454) = 20	!C2O+
    get_zatoms(455) = 27	!CCP+
    get_zatoms(456) = 13	!C2H+
    get_zatoms(457) = 15	!HOC+
    get_zatoms(458) = 19	!C2N+
    get_zatoms(459) = 19	!CNC+
    get_zatoms(460) = 22	!HCP+
    get_zatoms(461) = 26	!SIC2+
    get_zatoms(462) = 27	!SINC+
    get_zatoms(463) = 24	!HPO+
    get_zatoms(464) = 14	!HCN+
    get_zatoms(465) = 21	!CHSI+
    get_zatoms(466) = 16	!SIH2+
    get_zatoms(467) = 19	!C3H+
    get_zatoms(468) = 24	!C4+
    get_zatoms(469) = 26	!C3O+
    get_zatoms(470) = 34	!C3S+
    get_zatoms(471) = 16	!H2CO+
    get_zatoms(472) = 24	!H2SIO+
    get_zatoms(473) = 15	!HCNH+
    get_zatoms(474) = 27	!SIC2H+
    get_zatoms(475) = 32	!SIC3+
    get_zatoms(476) = 22	!CH2SI+
    get_zatoms(477) = 17	!SIH3+
    get_zatoms(478) = 21	!C2H2N+
    get_zatoms(479) = 15	!C2H3+
    get_zatoms(480) = 20	!C3H2+
    get_zatoms(481) = 20	!H2C3+
    get_zatoms(482) = 25	!C4H+
    get_zatoms(483) = 30	!C5+
    get_zatoms(484) = 40	!C4S+
    get_zatoms(485) = 28	!PC2H+
    get_zatoms(486) = 25	!C3N+
    get_zatoms(487) = 31	!C4N+
    get_zatoms(488) = 26	!C3HN+
    get_zatoms(489) = 14	!HNC+
    get_zatoms(490) = 33	!SIC3H+
    get_zatoms(491) = 38	!SIC4+
    get_zatoms(492) = 28	!SIC2H2+
    get_zatoms(493) = 23	!SICH3+
    get_zatoms(494) = 27	!HC2NCH+
    get_zatoms(495) = 21	!C3H3+
    get_zatoms(496) = 21	!H3C3+
    get_zatoms(497) = 31	!C5H+
    get_zatoms(498) = 36	!C6+
    get_zatoms(499) = 23	!C2H3O+
    get_zatoms(500) = 17	!C2H5+
    get_zatoms(501) = 28	!C3H3N+
    get_zatoms(502) = 32	!C5H2+
    get_zatoms(503) = 27	!C4H3+
    get_zatoms(504) = 37	!C6H+
    get_zatoms(505) = 42	!C7+
    get_zatoms(506) = 17	!CH4N+
    get_zatoms(507) = 38	!C5HN+
    get_zatoms(508) = 43	!C7H+
    get_zatoms(509) = 48	!C8+
    get_zatoms(510) = 32	!COOCH4+
    get_zatoms(511) = 25	!C2H5O+
    get_zatoms(512) = 49	!C8H+
    get_zatoms(513) = 54	!C9+
    get_zatoms(514) = 33	!C5H3+
    get_zatoms(515) = 38	!C6H2+
    get_zatoms(516) = 39	!C6H3+
    get_zatoms(517) = 32	!C2H6CO+
    get_zatoms(518) = 55	!C9H+
    get_zatoms(519) = 60	!C10+
    get_zatoms(520) = 45	!C7H3+
    get_zatoms(521) = 50	!C8H2+
    get_zatoms(522) = 51	!C8H3+
    get_zatoms(523) = 18	!HCL+
    get_zatoms(524) = 17	!HS+
    get_zatoms(525) = 8	!NH+
    get_zatoms(526) = 9	!OH+
    get_zatoms(527) = 22	!PN+
    get_zatoms(528) = 32	!S2+
    get_zatoms(529) = 15	!SIH+
    get_zatoms(530) = 22	!SIO+
    get_zatoms(531) = 10	!H2O+
    get_zatoms(532) = 22	!HNSI+
    get_zatoms(533) = 33	!S2H+
    get_zatoms(534) = 17	!PH2+
    get_zatoms(535) = 24	!H2CS+
    get_zatoms(536) = 34	!H2S2+
    get_zatoms(537) = 23	!HSIO+
    get_zatoms(538) = 39	!C4P+
    get_zatoms(539) = 23	!HCO2+
    get_zatoms(540) = 24	!PCH3+
    get_zatoms(541) = 10	!CH4+
    get_zatoms(542) = 20	!C2NH+
    get_zatoms(543) = 18	!SIH4+
    get_zatoms(544) = 11	!NH4+
    get_zatoms(545) = 15	!H2NC+
    get_zatoms(546) = 27	!C3H2N+
    get_zatoms(547) = 44	!C7H2+
    get_zatoms(548) = 34	!C5H4+
    get_zatoms(549) = 50	!C7HN+
    get_zatoms(550) = 56	!C9H2+
    get_zatoms(551) = 46	!C7H4+
    get_zatoms(552) = 62	!C9HN+
    get_zatoms(553) = 14	!N2+
    get_zatoms(554) = 22	!CO2+
    get_zatoms(555) = 3	!HEH+
    get_zatoms(556) = 32	!SO2+
    get_zatoms(557) = 41	!C6H5+
    get_zatoms(558) = 35	!C5H5+
    get_zatoms(559) = 15	!N2H+
    get_zatoms(560) = 23	!NO2+
    get_zatoms(561) = 29	!PC2H2+
    get_zatoms(562) = 24	!PNH2+
    get_zatoms(563) = 23	!PCH2+
    get_zatoms(564) = 29	!HC2S+
    get_zatoms(565) = 35	!HC3S+
    get_zatoms(566) = 25	!H3CS+
    get_zatoms(567) = 41	!HC4S+
    get_zatoms(568) = 23	!SINH2+
    get_zatoms(569) = 29	!SIC2H3+
    get_zatoms(570) = 34	!SIC3H2+
    get_zatoms(571) = 21	!C2HO+
    get_zatoms(572) = 11	!H3O+
    get_zatoms(573) = 19	!H3S+
    get_zatoms(574) = 31	!HOCS+
    get_zatoms(575) = 19	!CH5O+
    get_zatoms(576) = 21	!NCO+
    get_zatoms(577) = 22	!HNCO+
    get_zatoms(578) = 26	!C2N2+
    get_zatoms(579) = 3	!H3+
    get_zatoms(580) = 17	!O2H+
    get_zatoms(581) = 11	!CH5+
    get_zatoms(582) = 19	!H2CL+
    get_zatoms(583) = 25	!CH3O2+
    get_zatoms(584) = 25	!H2PO+
    get_zatoms(585) = 25	!PNH3+
    get_zatoms(586) = 25	!PCH4+
    get_zatoms(587) = 30	!PC2H3+
    get_zatoms(588) = 31	!HSIS+
    get_zatoms(589) = 25	!HSO+
    get_zatoms(590) = 24	!HNS+
    get_zatoms(591) = 23	!HPN+
    get_zatoms(592) = 17	!H2NO+
    get_zatoms(593) = 21	!NAH2O+
    get_zatoms(594) = 18	!PH3+
    get_zatoms(595) = 28	!SINCH+
    get_zatoms(596) = 31	!HSIO2+
    get_zatoms(597) = 33	!HSO2+
    get_zatoms(598) = 27	!HC3O+
    get_zatoms(599) = 34	!PC3H+
    get_zatoms(600) = 35	!H3S2+
    get_zatoms(601) = 25	!H3SIO+
    get_zatoms(602) = 40	!PC4H+
    get_zatoms(603) = 23	!NH2CNH+
    get_zatoms(604) = 39	!SIC4H+
    get_zatoms(605) = 24	!SICH4+
    get_zatoms(606) = 19	!SIH5+
    get_zatoms(607) = 23	!C2H4N+
    get_zatoms(608) = 25	!NH2CH2O+
    get_zatoms(609) = 18	!C2H6+
    get_zatoms(610) = 29	!C3H4N+
    get_zatoms(611) = 23	!C3H5+
    get_zatoms(612) = 28	!C4H4+
    get_zatoms(613) = 19	!CH6N+
    get_zatoms(614) = 39	!C5H2N+
    get_zatoms(615) = 35	!C4H4N+
    get_zatoms(616) = 33	!H5C2O2+
    get_zatoms(617) = 27	!C2H5OH2+
    get_zatoms(618) = 27	!CH3OCH4+
    get_zatoms(619) = 51	!C7H2N+
    get_zatoms(620) = 33	!C3H6OH+
    get_zatoms(621) = 47	!C6H4N+
    get_zatoms(622) = 61	!C10H+
    get_zatoms(623) = 57	!C9H3+
    get_zatoms(624) = 47	!C7H5+
    get_zatoms(625) = 59	!C8H4N+
    get_zatoms(626) = 63	!C9H2N+
    get_zatoms(627) = 43	!C6H7+
    get_zatoms(628) = 13	!NAH2+
    get_zatoms(629) = 31	!PC2H4+
    get_zatoms(630) = 29	!C4H5+
    get_zatoms(631) = 25	!H2CCL+
    get_zatoms(632) = 41	!PC4H2+
    get_zatoms(633) = 40	!C6H4+
    get_zatoms(634) = 52	!C8H4+
    get_zatoms(635) = 58	!C9H4+
    get_zatoms(636) = 31	!C4H7+
    get_zatoms(637) = 32	!HC4N+
    get_zatoms(638) = 33	!HC4O+
    get_zatoms(639) = 37	!C5N+
    get_zatoms(640) = 33	!H2C4N+
    get_zatoms(641) = 34	!H3C4N+
    get_zatoms(642) = 49	!C7N+
    get_zatoms(643) = 40	!C5H3N+
    get_zatoms(644) = 62	!C10H2+
    get_zatoms(645) = 61	!C9N+
    get_zatoms(646) = 52	!C7H3N+
    get_zatoms(647) = 64	!C9H3N+
    get_zatoms(648) = 32	!OCS+H2
    get_zatoms(649) = 28	!H2C3O+
    get_zatoms(650) = 29	!H3C3O+
    get_zatoms(651) = 41	!C5H4N+
    get_zatoms(652) = 53	!C8H5+
    get_zatoms(653) = 59	!C9H5+
    get_zatoms(654) = 33	!H2COHOCH2+
    get_zatoms(655) = 35	!H7C2O2+
    get_zatoms(656) = 0	!CR
    get_zatoms(657) = 0	!g
    get_zatoms(658) = 0	!Tgas
    get_zatoms(659) = 0	!dummy

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
        - n(idx_Ck) &
        - n(idx_CNk) &
        - n(idx_Ok) &
        - n(idx_OHk) &
        - n(idx_Sk) &
        - n(idx_GRAINk) &
        + n(idx_Cj) &
        + n(idx_CLj) &
        + n(idx_FEj) &
        + n(idx_Hj) &
        + n(idx_HEj) &
        + n(idx_MGj) &
        + n(idx_Nj) &
        + n(idx_NAj) &
        + n(idx_Oj) &
        + n(idx_Pj) &
        + n(idx_Sj) &
        + n(idx_SIj) &
        + n(idx_COj) &
        + n(idx_H2j) &
        + n(idx_NOj) &
        + n(idx_O2j) &
        + n(idx_CH2j) &
        + n(idx_H2Sj) &
        + n(idx_HCOj) &
        + n(idx_HCSj) &
        + n(idx_HNOj) &
        + n(idx_NH2j) &
        + n(idx_OCSj) &
        + n(idx_C2H2j) &
        + n(idx_CH3j) &
        + n(idx_NH3j) &
        + n(idx_C2H2Oj) &
        + n(idx_CH2O2j) &
        + n(idx_C2H3Nj) &
        + n(idx_C2H4j) &
        + n(idx_C4H2j) &
        + n(idx_H3COj) &
        + n(idx_CH4Oj) &
        + n(idx_C2H4Oj) &
        + n(idx_C3H4j) &
        + n(idx_CH5Nj) &
        + n(idx_C2H5OHj) &
        + n(idx_CH3OCH3j) &
        + n(idx_CHj) &
        + n(idx_CCLj) &
        + n(idx_C2j) &
        + n(idx_CLOj) &
        + n(idx_CPj) &
        + n(idx_CSj) &
        + n(idx_CNj) &
        + n(idx_NSj) &
        + n(idx_PHj) &
        + n(idx_POj) &
        + n(idx_SICj) &
        + n(idx_SINj) &
        + n(idx_SISj) &
        + n(idx_SOj) &
        + n(idx_C3j) &
        + n(idx_C2Sj) &
        + n(idx_C2Oj) &
        + n(idx_CCPj) &
        + n(idx_C2Hj) &
        + n(idx_HOCj) &
        + n(idx_C2Nj) &
        + n(idx_CNCj) &
        + n(idx_HCPj) &
        + n(idx_SIC2j) &
        + n(idx_SINCj) &
        + n(idx_HPOj) &
        + n(idx_HCNj) &
        + n(idx_CHSIj) &
        + n(idx_SIH2j) &
        + n(idx_C3Hj) &
        + n(idx_C4j) &
        + n(idx_C3Oj) &
        + n(idx_C3Sj) &
        + n(idx_H2COj) &
        + n(idx_H2SIOj) &
        + n(idx_HCNHj) &
        + n(idx_SIC2Hj) &
        + n(idx_SIC3j) &
        + n(idx_CH2SIj) &
        + n(idx_SIH3j) &
        + n(idx_C2H2Nj) &
        + n(idx_C2H3j) &
        + n(idx_C3H2j) &
        + n(idx_H2C3j) &
        + n(idx_C4Hj) &
        + n(idx_C5j) &
        + n(idx_C4Sj) &
        + n(idx_PC2Hj) &
        + n(idx_C3Nj) &
        + n(idx_C4Nj) &
        + n(idx_C3HNj) &
        + n(idx_HNCj) &
        + n(idx_SIC3Hj) &
        + n(idx_SIC4j) &
        + n(idx_SIC2H2j) &
        + n(idx_SICH3j) &
        + n(idx_HC2NCHj) &
        + n(idx_C3H3j) &
        + n(idx_H3C3j) &
        + n(idx_C5Hj) &
        + n(idx_C6j) &
        + n(idx_C2H3Oj) &
        + n(idx_C2H5j) &
        + n(idx_C3H3Nj) &
        + n(idx_C5H2j) &
        + n(idx_C4H3j) &
        + n(idx_C6Hj) &
        + n(idx_C7j) &
        + n(idx_CH4Nj) &
        + n(idx_C5HNj) &
        + n(idx_C7Hj) &
        + n(idx_C8j) &
        + n(idx_COOCH4j) &
        + n(idx_C2H5Oj) &
        + n(idx_C8Hj) &
        + n(idx_C9j) &
        + n(idx_C5H3j) &
        + n(idx_C6H2j) &
        + n(idx_C6H3j) &
        + n(idx_C2H6COj) &
        + n(idx_C9Hj) &
        + n(idx_C10j) &
        + n(idx_C7H3j) &
        + n(idx_C8H2j) &
        + n(idx_C8H3j) &
        + n(idx_HCLj) &
        + n(idx_HSj) &
        + n(idx_NHj) &
        + n(idx_OHj) &
        + n(idx_PNj) &
        + n(idx_S2j) &
        + n(idx_SIHj) &
        + n(idx_SIOj) &
        + n(idx_H2Oj) &
        + n(idx_HNSIj) &
        + n(idx_S2Hj) &
        + n(idx_PH2j) &
        + n(idx_H2CSj) &
        + n(idx_H2S2j) &
        + n(idx_HSIOj) &
        + n(idx_C4Pj) &
        + n(idx_HCO2j) &
        + n(idx_PCH3j) &
        + n(idx_CH4j) &
        + n(idx_C2NHj) &
        + n(idx_SIH4j) &
        + n(idx_NH4j) &
        + n(idx_H2NCj) &
        + n(idx_C3H2Nj) &
        + n(idx_C7H2j) &
        + n(idx_C5H4j) &
        + n(idx_C7HNj) &
        + n(idx_C9H2j) &
        + n(idx_C7H4j) &
        + n(idx_C9HNj) &
        + n(idx_N2j) &
        + n(idx_CO2j) &
        + n(idx_HEHj) &
        + n(idx_SO2j) &
        + n(idx_C6H5j) &
        + n(idx_C5H5j) &
        + n(idx_N2Hj) &
        + n(idx_NO2j) &
        + n(idx_PC2H2j) &
        + n(idx_PNH2j) &
        + n(idx_PCH2j) &
        + n(idx_HC2Sj) &
        + n(idx_HC3Sj) &
        + n(idx_H3CSj) &
        + n(idx_HC4Sj) &
        + n(idx_SINH2j) &
        + n(idx_SIC2H3j) &
        + n(idx_SIC3H2j) &
        + n(idx_C2HOj) &
        + n(idx_H3Oj) &
        + n(idx_H3Sj) &
        + n(idx_HOCSj) &
        + n(idx_CH5Oj) &
        + n(idx_NCOj) &
        + n(idx_HNCOj) &
        + n(idx_C2N2j) &
        + n(idx_H3j) &
        + n(idx_O2Hj) &
        + n(idx_CH5j) &
        + n(idx_H2CLj) &
        + n(idx_CH3O2j) &
        + n(idx_H2POj) &
        + n(idx_PNH3j) &
        + n(idx_PCH4j) &
        + n(idx_PC2H3j) &
        + n(idx_HSISj) &
        + n(idx_HSOj) &
        + n(idx_HNSj) &
        + n(idx_HPNj) &
        + n(idx_H2NOj) &
        + n(idx_NAH2Oj) &
        + n(idx_PH3j) &
        + n(idx_SINCHj) &
        + n(idx_HSIO2j) &
        + n(idx_HSO2j) &
        + n(idx_HC3Oj) &
        + n(idx_PC3Hj) &
        + n(idx_H3S2j) &
        + n(idx_H3SIOj) &
        + n(idx_PC4Hj) &
        + n(idx_NH2CNHj) &
        + n(idx_SIC4Hj) &
        + n(idx_SICH4j) &
        + n(idx_SIH5j) &
        + n(idx_C2H4Nj) &
        + n(idx_NH2CH2Oj) &
        + n(idx_C2H6j) &
        + n(idx_C3H4Nj) &
        + n(idx_C3H5j) &
        + n(idx_C4H4j) &
        + n(idx_CH6Nj) &
        + n(idx_C5H2Nj) &
        + n(idx_C4H4Nj) &
        + n(idx_H5C2O2j) &
        + n(idx_C2H5OH2j) &
        + n(idx_CH3OCH4j) &
        + n(idx_C7H2Nj) &
        + n(idx_C3H6OHj) &
        + n(idx_C6H4Nj) &
        + n(idx_C10Hj) &
        + n(idx_C9H3j) &
        + n(idx_C7H5j) &
        + n(idx_C8H4Nj) &
        + n(idx_C9H2Nj) &
        + n(idx_C6H7j) &
        + n(idx_NAH2j) &
        + n(idx_PC2H4j) &
        + n(idx_C4H5j) &
        + n(idx_H2CCLj) &
        + n(idx_PC4H2j) &
        + n(idx_C6H4j) &
        + n(idx_C8H4j) &
        + n(idx_C9H4j) &
        + n(idx_C4H7j) &
        + n(idx_HC4Nj) &
        + n(idx_HC4Oj) &
        + n(idx_C5Nj) &
        + n(idx_H2C4Nj) &
        + n(idx_H3C4Nj) &
        + n(idx_C7Nj) &
        + n(idx_C5H3Nj) &
        + n(idx_C10H2j) &
        + n(idx_C9Nj) &
        + n(idx_C7H3Nj) &
        + n(idx_C9H3Nj) &
        + n(idx_OCSjH2) &
        + n(idx_H2C3Oj) &
        + n(idx_H3C3Oj) &
        + n(idx_C5H4Nj) &
        + n(idx_C8H5j) &
        + n(idx_C9H5j) &
        + n(idx_H2COHOCH2j) &
        + n(idx_H7C2O2j)
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
    get_charges(3) = -1.d0 	!C-
    get_charges(4) = -1.d0 	!CN-
    get_charges(5) = -1.d0 	!O-
    get_charges(6) = -1.d0 	!OH-
    get_charges(7) = -1.d0 	!S-
    get_charges(8) = -1.d0 	!GRAIN-
    get_charges(9) = 0.d0 	!C
    get_charges(10) = 0.d0 	!CL
    get_charges(11) = 0.d0 	!FE
    get_charges(12) = 0.d0 	!H
    get_charges(13) = 0.d0 	!HE
    get_charges(14) = 0.d0 	!MG
    get_charges(15) = 0.d0 	!N
    get_charges(16) = 0.d0 	!NA
    get_charges(17) = 0.d0 	!O
    get_charges(18) = 0.d0 	!P
    get_charges(19) = 0.d0 	!S
    get_charges(20) = 0.d0 	!SI
    get_charges(21) = 0.d0 	!C2
    get_charges(22) = 0.d0 	!CCL
    get_charges(23) = 0.d0 	!CH
    get_charges(24) = 0.d0 	!CLO
    get_charges(25) = 0.d0 	!CN
    get_charges(26) = 0.d0 	!CO
    get_charges(27) = 0.d0 	!CP
    get_charges(28) = 0.d0 	!CS
    get_charges(29) = 0.d0 	!H2
    get_charges(30) = 0.d0 	!HCL
    get_charges(31) = 0.d0 	!HS
    get_charges(32) = 0.d0 	!MGH
    get_charges(33) = 0.d0 	!N2
    get_charges(34) = 0.d0 	!NAH
    get_charges(35) = 0.d0 	!NH
    get_charges(36) = 0.d0 	!NO
    get_charges(37) = 0.d0 	!NS
    get_charges(38) = 0.d0 	!O2
    get_charges(39) = 0.d0 	!OH
    get_charges(40) = 0.d0 	!PH
    get_charges(41) = 0.d0 	!PN
    get_charges(42) = 0.d0 	!PO
    get_charges(43) = 0.d0 	!S2
    get_charges(44) = 0.d0 	!SIC
    get_charges(45) = 0.d0 	!SIH
    get_charges(46) = 0.d0 	!SIN
    get_charges(47) = 0.d0 	!SIO
    get_charges(48) = 0.d0 	!SIS
    get_charges(49) = 0.d0 	!SO
    get_charges(50) = 0.d0 	!C2H
    get_charges(51) = 0.d0 	!C2N
    get_charges(52) = 0.d0 	!C2S
    get_charges(53) = 0.d0 	!C3
    get_charges(54) = 0.d0 	!CCO
    get_charges(55) = 0.d0 	!CCP
    get_charges(56) = 0.d0 	!CH2
    get_charges(57) = 0.d0 	!CO2
    get_charges(58) = 0.d0 	!H2O
    get_charges(59) = 0.d0 	!H2S
    get_charges(60) = 0.d0 	!HCN
    get_charges(61) = 0.d0 	!HCO
    get_charges(62) = 0.d0 	!HCP
    get_charges(63) = 0.d0 	!HCS
    get_charges(64) = 0.d0 	!HCSI
    get_charges(65) = 0.d0 	!HNC
    get_charges(66) = 0.d0 	!HNO
    get_charges(67) = 0.d0 	!HNSI
    get_charges(68) = 0.d0 	!HPO
    get_charges(69) = 0.d0 	!HS2
    get_charges(70) = 0.d0 	!N2O
    get_charges(71) = 0.d0 	!NAOH
    get_charges(72) = 0.d0 	!NH2
    get_charges(73) = 0.d0 	!NO2
    get_charges(74) = 0.d0 	!O2H
    get_charges(75) = 0.d0 	!OCN
    get_charges(76) = 0.d0 	!OCS
    get_charges(77) = 0.d0 	!PH2
    get_charges(78) = 0.d0 	!SIC2
    get_charges(79) = 0.d0 	!SIH2
    get_charges(80) = 0.d0 	!SINC
    get_charges(81) = 0.d0 	!SIO2
    get_charges(82) = 0.d0 	!SO2
    get_charges(83) = 0.d0 	!C2H2
    get_charges(84) = 0.d0 	!C3H
    get_charges(85) = 0.d0 	!C3N
    get_charges(86) = 0.d0 	!C3O
    get_charges(87) = 0.d0 	!C3P
    get_charges(88) = 0.d0 	!C3S
    get_charges(89) = 0.d0 	!C4
    get_charges(90) = 0.d0 	!CH3
    get_charges(91) = 0.d0 	!H2CO
    get_charges(92) = 0.d0 	!H2CS
    get_charges(93) = 0.d0 	!H2O2
    get_charges(94) = 0.d0 	!H2S2
    get_charges(95) = 0.d0 	!H2SIO
    get_charges(96) = 0.d0 	!HCCP
    get_charges(97) = 0.d0 	!NH3
    get_charges(98) = 0.d0 	!SIC2H
    get_charges(99) = 0.d0 	!SIC3
    get_charges(100) = 0.d0 	!SICH2
    get_charges(101) = 0.d0 	!SIH3
    get_charges(102) = 0.d0 	!C2H2N
    get_charges(103) = 0.d0 	!C2H2O
    get_charges(104) = 0.d0 	!C2H3
    get_charges(105) = 0.d0 	!C3H2
    get_charges(106) = 0.d0 	!C4H
    get_charges(107) = 0.d0 	!C4N
    get_charges(108) = 0.d0 	!C4P
    get_charges(109) = 0.d0 	!C4S
    get_charges(110) = 0.d0 	!C5
    get_charges(111) = 0.d0 	!CH2O2
    get_charges(112) = 0.d0 	!CH2PH
    get_charges(113) = 0.d0 	!CH3N
    get_charges(114) = 0.d0 	!CH4
    get_charges(115) = 0.d0 	!HC3N
    get_charges(116) = 0.d0 	!SIC2H2
    get_charges(117) = 0.d0 	!SIC3H
    get_charges(118) = 0.d0 	!SIC4
    get_charges(119) = 0.d0 	!SICH3
    get_charges(120) = 0.d0 	!SIH4
    get_charges(121) = 0.d0 	!C2H3N
    get_charges(122) = 0.d0 	!C2H4
    get_charges(123) = 0.d0 	!C3H3
    get_charges(124) = 0.d0 	!C4H2
    get_charges(125) = 0.d0 	!C5H
    get_charges(126) = 0.d0 	!C5N
    get_charges(127) = 0.d0 	!C6
    get_charges(128) = 0.d0 	!CH4O
    get_charges(129) = 0.d0 	!C2H4O
    get_charges(130) = 0.d0 	!C2H5
    get_charges(131) = 0.d0 	!C3H3N
    get_charges(132) = 0.d0 	!C3H4
    get_charges(133) = 0.d0 	!C5H2
    get_charges(134) = 0.d0 	!C6H
    get_charges(135) = 0.d0 	!C7
    get_charges(136) = 0.d0 	!CH5N
    get_charges(137) = 0.d0 	!HC5N
    get_charges(138) = 0.d0 	!C6H2
    get_charges(139) = 0.d0 	!C7H
    get_charges(140) = 0.d0 	!C7N
    get_charges(141) = 0.d0 	!C8
    get_charges(142) = 0.d0 	!CH3C3N
    get_charges(143) = 0.d0 	!HCOOCH3
    get_charges(144) = 0.d0 	!C2H5OH
    get_charges(145) = 0.d0 	!C7H2
    get_charges(146) = 0.d0 	!C8H
    get_charges(147) = 0.d0 	!C9
    get_charges(148) = 0.d0 	!CH3C4H
    get_charges(149) = 0.d0 	!CH3OCH3
    get_charges(150) = 0.d0 	!HC7N
    get_charges(151) = 0.d0 	!C2H6CO
    get_charges(152) = 0.d0 	!C8H2
    get_charges(153) = 0.d0 	!C9H
    get_charges(154) = 0.d0 	!C9N
    get_charges(155) = 0.d0 	!C10
    get_charges(156) = 0.d0 	!CH3C5N
    get_charges(157) = 0.d0 	!C9H2
    get_charges(158) = 0.d0 	!CH3C6H
    get_charges(159) = 0.d0 	!CH3C7N
    get_charges(160) = 0.d0 	!HC9N
    get_charges(161) = 0.d0 	!C4H4
    get_charges(162) = 0.d0 	!HCNC2
    get_charges(163) = 0.d0 	!HC2NC
    get_charges(164) = 0.d0 	!HNC3
    get_charges(165) = 0.d0 	!NH2CHO
    get_charges(166) = 0.d0 	!C4H3
    get_charges(167) = 0.d0 	!NH2CN
    get_charges(168) = 0.d0 	!C6H6
    get_charges(169) = 0.d0 	!H2CN
    get_charges(170) = 0.d0 	!GRAIN0
    get_charges(171) = 0.d0 	!O3
    get_charges(172) = 0.d0 	!FEH
    get_charges(173) = 0.d0 	!HNCO
    get_charges(174) = 0.d0 	!HC2O
    get_charges(175) = 0.d0 	!HCCN
    get_charges(176) = 0.d0 	!HC3O
    get_charges(177) = 0.d0 	!MGH2
    get_charges(178) = 0.d0 	!N2H2
    get_charges(179) = 0.d0 	!CHNH
    get_charges(180) = 0.d0 	!H2C3O
    get_charges(181) = 0.d0 	!H2C3N
    get_charges(182) = 0.d0 	!H2C5N
    get_charges(183) = 0.d0 	!H2C7N
    get_charges(184) = 0.d0 	!H2C9N
    get_charges(185) = 0.d0 	!NH2OH
    get_charges(186) = 0.d0 	!CH2OH
    get_charges(187) = 0.d0 	!C5H3
    get_charges(188) = 0.d0 	!H3C5N
    get_charges(189) = 0.d0 	!C6H3
    get_charges(190) = 0.d0 	!C7H3
    get_charges(191) = 0.d0 	!H3C7N
    get_charges(192) = 0.d0 	!C8H3
    get_charges(193) = 0.d0 	!C9H3
    get_charges(194) = 0.d0 	!H3C9N
    get_charges(195) = 0.d0 	!CH3NH
    get_charges(196) = 0.d0 	!H4C3N
    get_charges(197) = 0.d0 	!C5H4
    get_charges(198) = 0.d0 	!C6H4
    get_charges(199) = 0.d0 	!C7H4
    get_charges(200) = 0.d0 	!C8H4
    get_charges(201) = 0.d0 	!C9H4
    get_charges(202) = 0.d0 	!H5C3N
    get_charges(203) = 0.d0 	!C2H6
    get_charges(204) = 0.d0 	!C_DUST
    get_charges(205) = 0.d0 	!C2_DUST
    get_charges(206) = 0.d0 	!C3_DUST
    get_charges(207) = 0.d0 	!C2H_DUST
    get_charges(208) = 0.d0 	!C3H_DUST
    get_charges(209) = 0.d0 	!C2H3_DUST
    get_charges(210) = 0.d0 	!C3H3_DUST
    get_charges(211) = 0.d0 	!C2N_DUST
    get_charges(212) = 0.d0 	!C3N_DUST
    get_charges(213) = 0.d0 	!CCO_DUST
    get_charges(214) = 0.d0 	!C3O_DUST
    get_charges(215) = 0.d0 	!C2S_DUST
    get_charges(216) = 0.d0 	!C3S_DUST
    get_charges(217) = 0.d0 	!C4_DUST
    get_charges(218) = 0.d0 	!C4H_DUST
    get_charges(219) = 0.d0 	!C5_DUST
    get_charges(220) = 0.d0 	!C5H_DUST
    get_charges(221) = 0.d0 	!C6_DUST
    get_charges(222) = 0.d0 	!C6H_DUST
    get_charges(223) = 0.d0 	!C7_DUST
    get_charges(224) = 0.d0 	!C7H_DUST
    get_charges(225) = 0.d0 	!C8_DUST
    get_charges(226) = 0.d0 	!C8H_DUST
    get_charges(227) = 0.d0 	!C9_DUST
    get_charges(228) = 0.d0 	!C9H_DUST
    get_charges(229) = 0.d0 	!C10_DUST
    get_charges(230) = 0.d0 	!CH_DUST
    get_charges(231) = 0.d0 	!CH2_DUST
    get_charges(232) = 0.d0 	!C2H2_DUST
    get_charges(233) = 0.d0 	!CH3_DUST
    get_charges(234) = 0.d0 	!CN_DUST
    get_charges(235) = 0.d0 	!HS_DUST
    get_charges(236) = 0.d0 	!CS_DUST
    get_charges(237) = 0.d0 	!H_DUST
    get_charges(238) = 0.d0 	!N_DUST
    get_charges(239) = 0.d0 	!NH_DUST
    get_charges(240) = 0.d0 	!HNC_DUST
    get_charges(241) = 0.d0 	!NH2_DUST
    get_charges(242) = 0.d0 	!NO_DUST
    get_charges(243) = 0.d0 	!O_DUST
    get_charges(244) = 0.d0 	!OCN_DUST
    get_charges(245) = 0.d0 	!NS_DUST
    get_charges(246) = 0.d0 	!S_DUST
    get_charges(247) = 0.d0 	!CO_DUST
    get_charges(248) = 0.d0 	!O2_DUST
    get_charges(249) = 0.d0 	!OH_DUST
    get_charges(250) = 0.d0 	!SO_DUST
    get_charges(251) = 0.d0 	!C3H2_DUST
    get_charges(252) = 0.d0 	!C3H4_DUST
    get_charges(253) = 0.d0 	!C4H2_DUST
    get_charges(254) = 0.d0 	!C5H2_DUST
    get_charges(255) = 0.d0 	!C6H2_DUST
    get_charges(256) = 0.d0 	!C7H2_DUST
    get_charges(257) = 0.d0 	!C8H2_DUST
    get_charges(258) = 0.d0 	!C9H2_DUST
    get_charges(259) = 0.d0 	!C2H4_DUST
    get_charges(260) = 0.d0 	!HCCN_DUST
    get_charges(261) = 0.d0 	!HNO_DUST
    get_charges(262) = 0.d0 	!HCN_DUST
    get_charges(263) = 0.d0 	!CHNH_DUST
    get_charges(264) = 0.d0 	!CH3N_DUST
    get_charges(265) = 0.d0 	!HCO_DUST
    get_charges(266) = 0.d0 	!C2H5_DUST
    get_charges(267) = 0.d0 	!C2H2N_DUST
    get_charges(268) = 0.d0 	!CH2NH2_DUST
    get_charges(269) = 0.d0 	!H2CO_DUST
    get_charges(270) = 0.d0 	!CH3C3N_DUST
    get_charges(271) = 0.d0 	!C5N_DUST
    get_charges(272) = 0.d0 	!CH3C5N_DUST
    get_charges(273) = 0.d0 	!C7N_DUST
    get_charges(274) = 0.d0 	!CH3C7N_DUST
    get_charges(275) = 0.d0 	!CH2OH_DUST
    get_charges(276) = 0.d0 	!C2H5OH_DUST
    get_charges(277) = 0.d0 	!CH3OCH3_DUST
    get_charges(278) = 0.d0 	!C2H6_DUST
    get_charges(279) = 0.d0 	!C2H3N_DUST
    get_charges(280) = 0.d0 	!C2H4O_DUST
    get_charges(281) = 0.d0 	!CH4_DUST
    get_charges(282) = 0.d0 	!H2_DUST
    get_charges(283) = 0.d0 	!HC2O_DUST
    get_charges(284) = 0.d0 	!C3H3N_DUST
    get_charges(285) = 0.d0 	!H4C3N_DUST
    get_charges(286) = 0.d0 	!HC3N_DUST
    get_charges(287) = 0.d0 	!HC3O_DUST
    get_charges(288) = 0.d0 	!C4H3_DUST
    get_charges(289) = 0.d0 	!C4H4_DUST
    get_charges(290) = 0.d0 	!C5H3_DUST
    get_charges(291) = 0.d0 	!C5H4_DUST
    get_charges(292) = 0.d0 	!HC5N_DUST
    get_charges(293) = 0.d0 	!C6H3_DUST
    get_charges(294) = 0.d0 	!C6H4_DUST
    get_charges(295) = 0.d0 	!C7H3_DUST
    get_charges(296) = 0.d0 	!C7H4_DUST
    get_charges(297) = 0.d0 	!HC7N_DUST
    get_charges(298) = 0.d0 	!C8H3_DUST
    get_charges(299) = 0.d0 	!C8H4_DUST
    get_charges(300) = 0.d0 	!C9H3_DUST
    get_charges(301) = 0.d0 	!C9H4_DUST
    get_charges(302) = 0.d0 	!C9N_DUST
    get_charges(303) = 0.d0 	!HC9N_DUST
    get_charges(304) = 0.d0 	!CH3NH_DUST
    get_charges(305) = 0.d0 	!CH5N_DUST
    get_charges(306) = 0.d0 	!CH4O_DUST
    get_charges(307) = 0.d0 	!HCS_DUST
    get_charges(308) = 0.d0 	!FE_DUST
    get_charges(309) = 0.d0 	!FEH_DUST
    get_charges(310) = 0.d0 	!H2C3N_DUST
    get_charges(311) = 0.d0 	!H2C5N_DUST
    get_charges(312) = 0.d0 	!H3C5N_DUST
    get_charges(313) = 0.d0 	!H2C7N_DUST
    get_charges(314) = 0.d0 	!H3C7N_DUST
    get_charges(315) = 0.d0 	!H2C9N_DUST
    get_charges(316) = 0.d0 	!H3C9N_DUST
    get_charges(317) = 0.d0 	!H2CN_DUST
    get_charges(318) = 0.d0 	!H2O2_DUST
    get_charges(319) = 0.d0 	!H2O_DUST
    get_charges(320) = 0.d0 	!O2H_DUST
    get_charges(321) = 0.d0 	!H2S_DUST
    get_charges(322) = 0.d0 	!H5C3N_DUST
    get_charges(323) = 0.d0 	!C2H2O_DUST
    get_charges(324) = 0.d0 	!H2C3O_DUST
    get_charges(325) = 0.d0 	!H2CS_DUST
    get_charges(326) = 0.d0 	!MG_DUST
    get_charges(327) = 0.d0 	!MGH_DUST
    get_charges(328) = 0.d0 	!MGH2_DUST
    get_charges(329) = 0.d0 	!N2H2_DUST
    get_charges(330) = 0.d0 	!N2_DUST
    get_charges(331) = 0.d0 	!NA_DUST
    get_charges(332) = 0.d0 	!NAH_DUST
    get_charges(333) = 0.d0 	!NH3_DUST
    get_charges(334) = 0.d0 	!O3_DUST
    get_charges(335) = 0.d0 	!HNCO_DUST
    get_charges(336) = 0.d0 	!OCS_DUST
    get_charges(337) = 0.d0 	!SI_DUST
    get_charges(338) = 0.d0 	!SIH_DUST
    get_charges(339) = 0.d0 	!SIH2_DUST
    get_charges(340) = 0.d0 	!SIH3_DUST
    get_charges(341) = 0.d0 	!SIH4_DUST
    get_charges(342) = 0.d0 	!SO2_DUST
    get_charges(343) = 0.d0 	!HCOOCH3_DUST
    get_charges(344) = 0.d0 	!NH2CHO_DUST
    get_charges(345) = 0.d0 	!CO2_DUST
    get_charges(346) = 0.d0 	!CH2O2_DUST
    get_charges(347) = 0.d0 	!NH2OH_DUST
    get_charges(348) = 0.d0 	!C4N_DUST
    get_charges(349) = 0.d0 	!C4S_DUST
    get_charges(350) = 0.d0 	!C6H6_DUST
    get_charges(351) = 0.d0 	!CH2NH2
    get_charges(352) = 0.d0 	!CH3C4H_DUST
    get_charges(353) = 0.d0 	!CH3C6H_DUST
    get_charges(354) = 0.d0 	!H2S2_DUST
    get_charges(355) = 0.d0 	!HC2NC_DUST
    get_charges(356) = 0.d0 	!HCNC2_DUST
    get_charges(357) = 0.d0 	!HE_DUST
    get_charges(358) = 0.d0 	!HNC3_DUST
    get_charges(359) = 0.d0 	!HS2_DUST
    get_charges(360) = 0.d0 	!NAOH_DUST
    get_charges(361) = 0.d0 	!S2_DUST
    get_charges(362) = 0.d0 	!SIC_DUST
    get_charges(363) = 0.d0 	!SIO_DUST
    get_charges(364) = 0.d0 	!SIS_DUST
    get_charges(365) = 0.d0 	!C2H6CO_DUST
    get_charges(366) = 0.d0 	!C3P_DUST
    get_charges(367) = 0.d0 	!CCP_DUST
    get_charges(368) = 0.d0 	!C4P_DUST
    get_charges(369) = 0.d0 	!CCL_DUST
    get_charges(370) = 0.d0 	!CL_DUST
    get_charges(371) = 0.d0 	!P_DUST
    get_charges(372) = 0.d0 	!CP_DUST
    get_charges(373) = 0.d0 	!CH2PH_DUST
    get_charges(374) = 0.d0 	!HCP_DUST
    get_charges(375) = 0.d0 	!CLO_DUST
    get_charges(376) = 0.d0 	!H2SIO_DUST
    get_charges(377) = 0.d0 	!HCCP_DUST
    get_charges(378) = 0.d0 	!HCL_DUST
    get_charges(379) = 0.d0 	!HCSI_DUST
    get_charges(380) = 0.d0 	!HNSI_DUST
    get_charges(381) = 0.d0 	!SIN_DUST
    get_charges(382) = 0.d0 	!HPO_DUST
    get_charges(383) = 0.d0 	!PO_DUST
    get_charges(384) = 0.d0 	!N2O_DUST
    get_charges(385) = 0.d0 	!NH2CN_DUST
    get_charges(386) = 0.d0 	!NO2_DUST
    get_charges(387) = 0.d0 	!PH_DUST
    get_charges(388) = 0.d0 	!PH2_DUST
    get_charges(389) = 0.d0 	!PN_DUST
    get_charges(390) = 0.d0 	!SIC2_DUST
    get_charges(391) = 0.d0 	!SIC2H_DUST
    get_charges(392) = 0.d0 	!SIC2H2_DUST
    get_charges(393) = 0.d0 	!SIC3_DUST
    get_charges(394) = 0.d0 	!SIC3H_DUST
    get_charges(395) = 0.d0 	!SIC4_DUST
    get_charges(396) = 0.d0 	!SICH2_DUST
    get_charges(397) = 0.d0 	!SICH3_DUST
    get_charges(398) = 0.d0 	!SINC_DUST
    get_charges(399) = 0.d0 	!SIO2_DUST
    get_charges(400) = 1.d0 	!C+
    get_charges(401) = 1.d0 	!CL+
    get_charges(402) = 1.d0 	!FE+
    get_charges(403) = 1.d0 	!H+
    get_charges(404) = 1.d0 	!HE+
    get_charges(405) = 1.d0 	!MG+
    get_charges(406) = 1.d0 	!N+
    get_charges(407) = 1.d0 	!NA+
    get_charges(408) = 1.d0 	!O+
    get_charges(409) = 1.d0 	!P+
    get_charges(410) = 1.d0 	!S+
    get_charges(411) = 1.d0 	!SI+
    get_charges(412) = 1.d0 	!CO+
    get_charges(413) = 1.d0 	!H2+
    get_charges(414) = 1.d0 	!NO+
    get_charges(415) = 1.d0 	!O2+
    get_charges(416) = 1.d0 	!CH2+
    get_charges(417) = 1.d0 	!H2S+
    get_charges(418) = 1.d0 	!HCO+
    get_charges(419) = 1.d0 	!HCS+
    get_charges(420) = 1.d0 	!HNO+
    get_charges(421) = 1.d0 	!NH2+
    get_charges(422) = 1.d0 	!OCS+
    get_charges(423) = 1.d0 	!C2H2+
    get_charges(424) = 1.d0 	!CH3+
    get_charges(425) = 1.d0 	!NH3+
    get_charges(426) = 1.d0 	!C2H2O+
    get_charges(427) = 1.d0 	!CH2O2+
    get_charges(428) = 1.d0 	!C2H3N+
    get_charges(429) = 1.d0 	!C2H4+
    get_charges(430) = 1.d0 	!C4H2+
    get_charges(431) = 1.d0 	!H3CO+
    get_charges(432) = 1.d0 	!CH4O+
    get_charges(433) = 1.d0 	!C2H4O+
    get_charges(434) = 1.d0 	!C3H4+
    get_charges(435) = 1.d0 	!CH5N+
    get_charges(436) = 1.d0 	!C2H5OH+
    get_charges(437) = 1.d0 	!CH3OCH3+
    get_charges(438) = 1.d0 	!CH+
    get_charges(439) = 1.d0 	!CCL+
    get_charges(440) = 1.d0 	!C2+
    get_charges(441) = 1.d0 	!CLO+
    get_charges(442) = 1.d0 	!CP+
    get_charges(443) = 1.d0 	!CS+
    get_charges(444) = 1.d0 	!CN+
    get_charges(445) = 1.d0 	!NS+
    get_charges(446) = 1.d0 	!PH+
    get_charges(447) = 1.d0 	!PO+
    get_charges(448) = 1.d0 	!SIC+
    get_charges(449) = 1.d0 	!SIN+
    get_charges(450) = 1.d0 	!SIS+
    get_charges(451) = 1.d0 	!SO+
    get_charges(452) = 1.d0 	!C3+
    get_charges(453) = 1.d0 	!C2S+
    get_charges(454) = 1.d0 	!C2O+
    get_charges(455) = 1.d0 	!CCP+
    get_charges(456) = 1.d0 	!C2H+
    get_charges(457) = 1.d0 	!HOC+
    get_charges(458) = 1.d0 	!C2N+
    get_charges(459) = 1.d0 	!CNC+
    get_charges(460) = 1.d0 	!HCP+
    get_charges(461) = 1.d0 	!SIC2+
    get_charges(462) = 1.d0 	!SINC+
    get_charges(463) = 1.d0 	!HPO+
    get_charges(464) = 1.d0 	!HCN+
    get_charges(465) = 1.d0 	!CHSI+
    get_charges(466) = 1.d0 	!SIH2+
    get_charges(467) = 1.d0 	!C3H+
    get_charges(468) = 1.d0 	!C4+
    get_charges(469) = 1.d0 	!C3O+
    get_charges(470) = 1.d0 	!C3S+
    get_charges(471) = 1.d0 	!H2CO+
    get_charges(472) = 1.d0 	!H2SIO+
    get_charges(473) = 1.d0 	!HCNH+
    get_charges(474) = 1.d0 	!SIC2H+
    get_charges(475) = 1.d0 	!SIC3+
    get_charges(476) = 1.d0 	!CH2SI+
    get_charges(477) = 1.d0 	!SIH3+
    get_charges(478) = 1.d0 	!C2H2N+
    get_charges(479) = 1.d0 	!C2H3+
    get_charges(480) = 1.d0 	!C3H2+
    get_charges(481) = 1.d0 	!H2C3+
    get_charges(482) = 1.d0 	!C4H+
    get_charges(483) = 1.d0 	!C5+
    get_charges(484) = 1.d0 	!C4S+
    get_charges(485) = 1.d0 	!PC2H+
    get_charges(486) = 1.d0 	!C3N+
    get_charges(487) = 1.d0 	!C4N+
    get_charges(488) = 1.d0 	!C3HN+
    get_charges(489) = 1.d0 	!HNC+
    get_charges(490) = 1.d0 	!SIC3H+
    get_charges(491) = 1.d0 	!SIC4+
    get_charges(492) = 1.d0 	!SIC2H2+
    get_charges(493) = 1.d0 	!SICH3+
    get_charges(494) = 1.d0 	!HC2NCH+
    get_charges(495) = 1.d0 	!C3H3+
    get_charges(496) = 1.d0 	!H3C3+
    get_charges(497) = 1.d0 	!C5H+
    get_charges(498) = 1.d0 	!C6+
    get_charges(499) = 1.d0 	!C2H3O+
    get_charges(500) = 1.d0 	!C2H5+
    get_charges(501) = 1.d0 	!C3H3N+
    get_charges(502) = 1.d0 	!C5H2+
    get_charges(503) = 1.d0 	!C4H3+
    get_charges(504) = 1.d0 	!C6H+
    get_charges(505) = 1.d0 	!C7+
    get_charges(506) = 1.d0 	!CH4N+
    get_charges(507) = 1.d0 	!C5HN+
    get_charges(508) = 1.d0 	!C7H+
    get_charges(509) = 1.d0 	!C8+
    get_charges(510) = 1.d0 	!COOCH4+
    get_charges(511) = 1.d0 	!C2H5O+
    get_charges(512) = 1.d0 	!C8H+
    get_charges(513) = 1.d0 	!C9+
    get_charges(514) = 1.d0 	!C5H3+
    get_charges(515) = 1.d0 	!C6H2+
    get_charges(516) = 1.d0 	!C6H3+
    get_charges(517) = 1.d0 	!C2H6CO+
    get_charges(518) = 1.d0 	!C9H+
    get_charges(519) = 1.d0 	!C10+
    get_charges(520) = 1.d0 	!C7H3+
    get_charges(521) = 1.d0 	!C8H2+
    get_charges(522) = 1.d0 	!C8H3+
    get_charges(523) = 1.d0 	!HCL+
    get_charges(524) = 1.d0 	!HS+
    get_charges(525) = 1.d0 	!NH+
    get_charges(526) = 1.d0 	!OH+
    get_charges(527) = 1.d0 	!PN+
    get_charges(528) = 1.d0 	!S2+
    get_charges(529) = 1.d0 	!SIH+
    get_charges(530) = 1.d0 	!SIO+
    get_charges(531) = 1.d0 	!H2O+
    get_charges(532) = 1.d0 	!HNSI+
    get_charges(533) = 1.d0 	!S2H+
    get_charges(534) = 1.d0 	!PH2+
    get_charges(535) = 1.d0 	!H2CS+
    get_charges(536) = 1.d0 	!H2S2+
    get_charges(537) = 1.d0 	!HSIO+
    get_charges(538) = 1.d0 	!C4P+
    get_charges(539) = 1.d0 	!HCO2+
    get_charges(540) = 1.d0 	!PCH3+
    get_charges(541) = 1.d0 	!CH4+
    get_charges(542) = 1.d0 	!C2NH+
    get_charges(543) = 1.d0 	!SIH4+
    get_charges(544) = 1.d0 	!NH4+
    get_charges(545) = 1.d0 	!H2NC+
    get_charges(546) = 1.d0 	!C3H2N+
    get_charges(547) = 1.d0 	!C7H2+
    get_charges(548) = 1.d0 	!C5H4+
    get_charges(549) = 1.d0 	!C7HN+
    get_charges(550) = 1.d0 	!C9H2+
    get_charges(551) = 1.d0 	!C7H4+
    get_charges(552) = 1.d0 	!C9HN+
    get_charges(553) = 1.d0 	!N2+
    get_charges(554) = 1.d0 	!CO2+
    get_charges(555) = 1.d0 	!HEH+
    get_charges(556) = 1.d0 	!SO2+
    get_charges(557) = 1.d0 	!C6H5+
    get_charges(558) = 1.d0 	!C5H5+
    get_charges(559) = 1.d0 	!N2H+
    get_charges(560) = 1.d0 	!NO2+
    get_charges(561) = 1.d0 	!PC2H2+
    get_charges(562) = 1.d0 	!PNH2+
    get_charges(563) = 1.d0 	!PCH2+
    get_charges(564) = 1.d0 	!HC2S+
    get_charges(565) = 1.d0 	!HC3S+
    get_charges(566) = 1.d0 	!H3CS+
    get_charges(567) = 1.d0 	!HC4S+
    get_charges(568) = 1.d0 	!SINH2+
    get_charges(569) = 1.d0 	!SIC2H3+
    get_charges(570) = 1.d0 	!SIC3H2+
    get_charges(571) = 1.d0 	!C2HO+
    get_charges(572) = 1.d0 	!H3O+
    get_charges(573) = 1.d0 	!H3S+
    get_charges(574) = 1.d0 	!HOCS+
    get_charges(575) = 1.d0 	!CH5O+
    get_charges(576) = 1.d0 	!NCO+
    get_charges(577) = 1.d0 	!HNCO+
    get_charges(578) = 1.d0 	!C2N2+
    get_charges(579) = 1.d0 	!H3+
    get_charges(580) = 1.d0 	!O2H+
    get_charges(581) = 1.d0 	!CH5+
    get_charges(582) = 1.d0 	!H2CL+
    get_charges(583) = 1.d0 	!CH3O2+
    get_charges(584) = 1.d0 	!H2PO+
    get_charges(585) = 1.d0 	!PNH3+
    get_charges(586) = 1.d0 	!PCH4+
    get_charges(587) = 1.d0 	!PC2H3+
    get_charges(588) = 1.d0 	!HSIS+
    get_charges(589) = 1.d0 	!HSO+
    get_charges(590) = 1.d0 	!HNS+
    get_charges(591) = 1.d0 	!HPN+
    get_charges(592) = 1.d0 	!H2NO+
    get_charges(593) = 1.d0 	!NAH2O+
    get_charges(594) = 1.d0 	!PH3+
    get_charges(595) = 1.d0 	!SINCH+
    get_charges(596) = 1.d0 	!HSIO2+
    get_charges(597) = 1.d0 	!HSO2+
    get_charges(598) = 1.d0 	!HC3O+
    get_charges(599) = 1.d0 	!PC3H+
    get_charges(600) = 1.d0 	!H3S2+
    get_charges(601) = 1.d0 	!H3SIO+
    get_charges(602) = 1.d0 	!PC4H+
    get_charges(603) = 1.d0 	!NH2CNH+
    get_charges(604) = 1.d0 	!SIC4H+
    get_charges(605) = 1.d0 	!SICH4+
    get_charges(606) = 1.d0 	!SIH5+
    get_charges(607) = 1.d0 	!C2H4N+
    get_charges(608) = 1.d0 	!NH2CH2O+
    get_charges(609) = 1.d0 	!C2H6+
    get_charges(610) = 1.d0 	!C3H4N+
    get_charges(611) = 1.d0 	!C3H5+
    get_charges(612) = 1.d0 	!C4H4+
    get_charges(613) = 1.d0 	!CH6N+
    get_charges(614) = 1.d0 	!C5H2N+
    get_charges(615) = 1.d0 	!C4H4N+
    get_charges(616) = 1.d0 	!H5C2O2+
    get_charges(617) = 1.d0 	!C2H5OH2+
    get_charges(618) = 1.d0 	!CH3OCH4+
    get_charges(619) = 1.d0 	!C7H2N+
    get_charges(620) = 1.d0 	!C3H6OH+
    get_charges(621) = 1.d0 	!C6H4N+
    get_charges(622) = 1.d0 	!C10H+
    get_charges(623) = 1.d0 	!C9H3+
    get_charges(624) = 1.d0 	!C7H5+
    get_charges(625) = 1.d0 	!C8H4N+
    get_charges(626) = 1.d0 	!C9H2N+
    get_charges(627) = 1.d0 	!C6H7+
    get_charges(628) = 1.d0 	!NAH2+
    get_charges(629) = 1.d0 	!PC2H4+
    get_charges(630) = 1.d0 	!C4H5+
    get_charges(631) = 1.d0 	!H2CCL+
    get_charges(632) = 1.d0 	!PC4H2+
    get_charges(633) = 1.d0 	!C6H4+
    get_charges(634) = 1.d0 	!C8H4+
    get_charges(635) = 1.d0 	!C9H4+
    get_charges(636) = 1.d0 	!C4H7+
    get_charges(637) = 1.d0 	!HC4N+
    get_charges(638) = 1.d0 	!HC4O+
    get_charges(639) = 1.d0 	!C5N+
    get_charges(640) = 1.d0 	!H2C4N+
    get_charges(641) = 1.d0 	!H3C4N+
    get_charges(642) = 1.d0 	!C7N+
    get_charges(643) = 1.d0 	!C5H3N+
    get_charges(644) = 1.d0 	!C10H2+
    get_charges(645) = 1.d0 	!C9N+
    get_charges(646) = 1.d0 	!C7H3N+
    get_charges(647) = 1.d0 	!C9H3N+
    get_charges(648) = 1.d0 	!OCS+H2
    get_charges(649) = 1.d0 	!H2C3O+
    get_charges(650) = 1.d0 	!H3C3O+
    get_charges(651) = 1.d0 	!C5H4N+
    get_charges(652) = 1.d0 	!C8H5+
    get_charges(653) = 1.d0 	!C9H5+
    get_charges(654) = 1.d0 	!H2COHOCH2+
    get_charges(655) = 1.d0 	!H7C2O2+
    get_charges(656) = 0.d0 	!CR
    get_charges(657) = 0.d0 	!g
    get_charges(658) = 0.d0 	!Tgas
    get_charges(659) = 0.d0 	!dummy

  end function get_charges

  !*****************************
  ! get metallicity using C as reference
  function get_metallicityC(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityC,zC,nH

    nH = get_Hnuclei(n(:))

    zC = n(idx_Ck) &
        + n(idx_CNk) &
        + n(idx_C) &
        + 2d0*n(idx_C2) &
        + n(idx_CCL) &
        + n(idx_CH) &
        + n(idx_CN) &
        + n(idx_CO) &
        + n(idx_CP) &
        + n(idx_CS) &
        + n(idx_SIC) &
        + 2d0*n(idx_C2H) &
        + 2d0*n(idx_C2N) &
        + 2d0*n(idx_C2S) &
        + 3d0*n(idx_C3) &
        + 2d0*n(idx_CCO) &
        + 2d0*n(idx_CCP) &
        + n(idx_CH2) &
        + n(idx_CO2) &
        + n(idx_HCN) &
        + n(idx_HCO) &
        + n(idx_HCP) &
        + n(idx_HCS) &
        + n(idx_HCSI) &
        + n(idx_HNC) &
        + n(idx_OCN) &
        + n(idx_OCS) &
        + 2d0*n(idx_SIC2) &
        + n(idx_SINC) &
        + 2d0*n(idx_C2H2) &
        + 3d0*n(idx_C3H) &
        + 3d0*n(idx_C3N) &
        + 3d0*n(idx_C3O) &
        + 3d0*n(idx_C3P) &
        + 3d0*n(idx_C3S) &
        + 4d0*n(idx_C4) &
        + n(idx_CH3) &
        + n(idx_H2CO) &
        + n(idx_H2CS) &
        + 2d0*n(idx_HCCP) &
        + 2d0*n(idx_SIC2H) &
        + 3d0*n(idx_SIC3) &
        + n(idx_SICH2) &
        + 2d0*n(idx_C2H2N) &
        + 2d0*n(idx_C2H2O) &
        + 2d0*n(idx_C2H3) &
        + 3d0*n(idx_C3H2) &
        + 4d0*n(idx_C4H) &
        + 4d0*n(idx_C4N) &
        + 4d0*n(idx_C4P) &
        + 4d0*n(idx_C4S) &
        + 5d0*n(idx_C5) &
        + n(idx_CH2O2) &
        + n(idx_CH2PH) &
        + n(idx_CH3N) &
        + n(idx_CH4) &
        + 3d0*n(idx_HC3N) &
        + 2d0*n(idx_SIC2H2) &
        + 3d0*n(idx_SIC3H) &
        + 4d0*n(idx_SIC4) &
        + n(idx_SICH3) &
        + 2d0*n(idx_C2H3N) &
        + 2d0*n(idx_C2H4) &
        + 3d0*n(idx_C3H3) &
        + 4d0*n(idx_C4H2) &
        + 5d0*n(idx_C5H) &
        + 5d0*n(idx_C5N) &
        + 6d0*n(idx_C6) &
        + n(idx_CH4O) &
        + 2d0*n(idx_C2H4O) &
        + 2d0*n(idx_C2H5) &
        + 3d0*n(idx_C3H3N) &
        + 3d0*n(idx_C3H4) &
        + 5d0*n(idx_C5H2) &
        + 6d0*n(idx_C6H) &
        + 7d0*n(idx_C7) &
        + n(idx_CH5N) &
        + 5d0*n(idx_HC5N) &
        + 6d0*n(idx_C6H2) &
        + 7d0*n(idx_C7H) &
        + 7d0*n(idx_C7N) &
        + 8d0*n(idx_C8) &
        + 4d0*n(idx_CH3C3N) &
        + 2d0*n(idx_HCOOCH3) &
        + 2d0*n(idx_C2H5OH) &
        + 7d0*n(idx_C7H2) &
        + 8d0*n(idx_C8H) &
        + 9d0*n(idx_C9) &
        + 5d0*n(idx_CH3C4H) &
        + 2d0*n(idx_CH3OCH3) &
        + 7d0*n(idx_HC7N) &
        + 3d0*n(idx_C2H6CO) &
        + 8d0*n(idx_C8H2) &
        + 9d0*n(idx_C9H) &
        + 9d0*n(idx_C9N) &
        + 10d0*n(idx_C10) &
        + 6d0*n(idx_CH3C5N) &
        + 9d0*n(idx_C9H2) &
        + 7d0*n(idx_CH3C6H) &
        + 8d0*n(idx_CH3C7N) &
        + 9d0*n(idx_HC9N) &
        + 4d0*n(idx_C4H4) &
        + 3d0*n(idx_HCNC2) &
        + 3d0*n(idx_HC2NC) &
        + 3d0*n(idx_HNC3) &
        + n(idx_NH2CHO) &
        + 4d0*n(idx_C4H3) &
        + n(idx_NH2CN) &
        + 6d0*n(idx_C6H6) &
        + n(idx_H2CN) &
        + n(idx_HNCO) &
        + 2d0*n(idx_HC2O) &
        + 2d0*n(idx_HCCN) &
        + 3d0*n(idx_HC3O) &
        + n(idx_CHNH) &
        + 3d0*n(idx_H2C3O) &
        + 3d0*n(idx_H2C3N) &
        + 5d0*n(idx_H2C5N) &
        + 7d0*n(idx_H2C7N) &
        + 9d0*n(idx_H2C9N) &
        + n(idx_CH2OH) &
        + 5d0*n(idx_C5H3) &
        + 5d0*n(idx_H3C5N) &
        + 6d0*n(idx_C6H3) &
        + 7d0*n(idx_C7H3) &
        + 7d0*n(idx_H3C7N) &
        + 8d0*n(idx_C8H3) &
        + 9d0*n(idx_C9H3) &
        + 9d0*n(idx_H3C9N) &
        + n(idx_CH3NH) &
        + 3d0*n(idx_H4C3N) &
        + 5d0*n(idx_C5H4) &
        + 6d0*n(idx_C6H4) &
        + 7d0*n(idx_C7H4) &
        + 8d0*n(idx_C8H4) &
        + 9d0*n(idx_C9H4) &
        + 3d0*n(idx_H5C3N) &
        + 2d0*n(idx_C2H6) &
        + n(idx_C_DUST) &
        + 2d0*n(idx_C2_DUST) &
        + 3d0*n(idx_C3_DUST) &
        + 2d0*n(idx_C2H_DUST) &
        + 3d0*n(idx_C3H_DUST) &
        + 2d0*n(idx_C2H3_DUST) &
        + 3d0*n(idx_C3H3_DUST) &
        + 2d0*n(idx_C2N_DUST) &
        + 3d0*n(idx_C3N_DUST) &
        + 2d0*n(idx_CCO_DUST) &
        + 3d0*n(idx_C3O_DUST) &
        + 2d0*n(idx_C2S_DUST) &
        + 3d0*n(idx_C3S_DUST) &
        + 4d0*n(idx_C4_DUST) &
        + 4d0*n(idx_C4H_DUST) &
        + 5d0*n(idx_C5_DUST) &
        + 5d0*n(idx_C5H_DUST) &
        + 6d0*n(idx_C6_DUST) &
        + 6d0*n(idx_C6H_DUST) &
        + 7d0*n(idx_C7_DUST) &
        + 7d0*n(idx_C7H_DUST) &
        + 8d0*n(idx_C8_DUST) &
        + 8d0*n(idx_C8H_DUST) &
        + 9d0*n(idx_C9_DUST) &
        + 9d0*n(idx_C9H_DUST) &
        + 10d0*n(idx_C10_DUST) &
        + n(idx_CH_DUST) &
        + n(idx_CH2_DUST) &
        + 2d0*n(idx_C2H2_DUST) &
        + n(idx_CH3_DUST) &
        + n(idx_CN_DUST) &
        + n(idx_CS_DUST) &
        + n(idx_HNC_DUST) &
        + n(idx_OCN_DUST) &
        + n(idx_CO_DUST) &
        + 3d0*n(idx_C3H2_DUST) &
        + 3d0*n(idx_C3H4_DUST) &
        + 4d0*n(idx_C4H2_DUST) &
        + 5d0*n(idx_C5H2_DUST) &
        + 6d0*n(idx_C6H2_DUST) &
        + 7d0*n(idx_C7H2_DUST) &
        + 8d0*n(idx_C8H2_DUST) &
        + 9d0*n(idx_C9H2_DUST) &
        + 2d0*n(idx_C2H4_DUST) &
        + 2d0*n(idx_HCCN_DUST) &
        + n(idx_HCN_DUST) &
        + n(idx_CHNH_DUST) &
        + n(idx_CH3N_DUST) &
        + n(idx_HCO_DUST) &
        + 2d0*n(idx_C2H5_DUST) &
        + 2d0*n(idx_C2H2N_DUST) &
        + n(idx_CH2NH2_DUST) &
        + n(idx_H2CO_DUST) &
        + 4d0*n(idx_CH3C3N_DUST) &
        + 5d0*n(idx_C5N_DUST) &
        + 6d0*n(idx_CH3C5N_DUST) &
        + 7d0*n(idx_C7N_DUST) &
        + 8d0*n(idx_CH3C7N_DUST) &
        + n(idx_CH2OH_DUST) &
        + 2d0*n(idx_C2H5OH_DUST) &
        + 2d0*n(idx_CH3OCH3_DUST) &
        + 2d0*n(idx_C2H6_DUST) &
        + 2d0*n(idx_C2H3N_DUST) &
        + 2d0*n(idx_C2H4O_DUST) &
        + n(idx_CH4_DUST) &
        + 2d0*n(idx_HC2O_DUST) &
        + 3d0*n(idx_C3H3N_DUST) &
        + 3d0*n(idx_H4C3N_DUST) &
        + 3d0*n(idx_HC3N_DUST) &
        + 3d0*n(idx_HC3O_DUST) &
        + 4d0*n(idx_C4H3_DUST) &
        + 4d0*n(idx_C4H4_DUST) &
        + 5d0*n(idx_C5H3_DUST) &
        + 5d0*n(idx_C5H4_DUST) &
        + 5d0*n(idx_HC5N_DUST) &
        + 6d0*n(idx_C6H3_DUST) &
        + 6d0*n(idx_C6H4_DUST) &
        + 7d0*n(idx_C7H3_DUST) &
        + 7d0*n(idx_C7H4_DUST) &
        + 7d0*n(idx_HC7N_DUST) &
        + 8d0*n(idx_C8H3_DUST) &
        + 8d0*n(idx_C8H4_DUST) &
        + 9d0*n(idx_C9H3_DUST) &
        + 9d0*n(idx_C9H4_DUST) &
        + 9d0*n(idx_C9N_DUST) &
        + 9d0*n(idx_HC9N_DUST) &
        + n(idx_CH3NH_DUST) &
        + n(idx_CH5N_DUST) &
        + n(idx_CH4O_DUST) &
        + n(idx_HCS_DUST) &
        + 3d0*n(idx_H2C3N_DUST) &
        + 5d0*n(idx_H2C5N_DUST) &
        + 5d0*n(idx_H3C5N_DUST) &
        + 7d0*n(idx_H2C7N_DUST) &
        + 7d0*n(idx_H3C7N_DUST) &
        + 9d0*n(idx_H2C9N_DUST) &
        + 9d0*n(idx_H3C9N_DUST) &
        + n(idx_H2CN_DUST) &
        + 3d0*n(idx_H5C3N_DUST) &
        + 2d0*n(idx_C2H2O_DUST) &
        + 3d0*n(idx_H2C3O_DUST) &
        + n(idx_H2CS_DUST) &
        + n(idx_HNCO_DUST) &
        + n(idx_OCS_DUST) &
        + 2d0*n(idx_HCOOCH3_DUST) &
        + n(idx_NH2CHO_DUST) &
        + n(idx_CO2_DUST) &
        + n(idx_CH2O2_DUST) &
        + 4d0*n(idx_C4N_DUST) &
        + 4d0*n(idx_C4S_DUST) &
        + 6d0*n(idx_C6H6_DUST) &
        + n(idx_CH2NH2) &
        + 5d0*n(idx_CH3C4H_DUST) &
        + 7d0*n(idx_CH3C6H_DUST) &
        + 3d0*n(idx_HC2NC_DUST) &
        + 3d0*n(idx_HCNC2_DUST) &
        + 3d0*n(idx_HNC3_DUST) &
        + n(idx_SIC_DUST) &
        + 3d0*n(idx_C2H6CO_DUST) &
        + 3d0*n(idx_C3P_DUST) &
        + 2d0*n(idx_CCP_DUST) &
        + 4d0*n(idx_C4P_DUST) &
        + n(idx_CCL_DUST) &
        + n(idx_CP_DUST) &
        + n(idx_CH2PH_DUST) &
        + n(idx_HCP_DUST) &
        + 2d0*n(idx_HCCP_DUST) &
        + n(idx_HCSI_DUST) &
        + n(idx_NH2CN_DUST) &
        + 2d0*n(idx_SIC2_DUST) &
        + 2d0*n(idx_SIC2H_DUST) &
        + 2d0*n(idx_SIC2H2_DUST) &
        + 3d0*n(idx_SIC3_DUST) &
        + 3d0*n(idx_SIC3H_DUST) &
        + 4d0*n(idx_SIC4_DUST) &
        + n(idx_SICH2_DUST) &
        + n(idx_SICH3_DUST) &
        + n(idx_SINC_DUST) &
        + n(idx_Cj) &
        + n(idx_COj) &
        + n(idx_CH2j) &
        + n(idx_HCOj) &
        + n(idx_HCSj) &
        + n(idx_OCSj) &
        + 2d0*n(idx_C2H2j) &
        + n(idx_CH3j) &
        + 2d0*n(idx_C2H2Oj) &
        + n(idx_CH2O2j) &
        + 2d0*n(idx_C2H3Nj) &
        + 2d0*n(idx_C2H4j) &
        + 4d0*n(idx_C4H2j) &
        + n(idx_H3COj) &
        + n(idx_CH4Oj) &
        + 2d0*n(idx_C2H4Oj) &
        + 3d0*n(idx_C3H4j) &
        + n(idx_CH5Nj) &
        + 2d0*n(idx_C2H5OHj) &
        + 2d0*n(idx_CH3OCH3j) &
        + n(idx_CHj) &
        + n(idx_CCLj) &
        + 2d0*n(idx_C2j) &
        + n(idx_CPj) &
        + n(idx_CSj) &
        + n(idx_CNj) &
        + n(idx_SICj) &
        + 3d0*n(idx_C3j) &
        + 2d0*n(idx_C2Sj) &
        + 2d0*n(idx_C2Oj) &
        + 2d0*n(idx_CCPj) &
        + 2d0*n(idx_C2Hj) &
        + n(idx_HOCj) &
        + 2d0*n(idx_C2Nj) &
        + 2d0*n(idx_CNCj) &
        + n(idx_HCPj) &
        + 2d0*n(idx_SIC2j) &
        + n(idx_SINCj) &
        + n(idx_HCNj) &
        + n(idx_CHSIj) &
        + 3d0*n(idx_C3Hj) &
        + 4d0*n(idx_C4j) &
        + 3d0*n(idx_C3Oj) &
        + 3d0*n(idx_C3Sj) &
        + n(idx_H2COj) &
        + n(idx_HCNHj) &
        + 2d0*n(idx_SIC2Hj) &
        + 3d0*n(idx_SIC3j) &
        + n(idx_CH2SIj) &
        + 2d0*n(idx_C2H2Nj) &
        + 2d0*n(idx_C2H3j) &
        + 3d0*n(idx_C3H2j) &
        + 3d0*n(idx_H2C3j) &
        + 4d0*n(idx_C4Hj) &
        + 5d0*n(idx_C5j) &
        + 4d0*n(idx_C4Sj) &
        + 2d0*n(idx_PC2Hj) &
        + 3d0*n(idx_C3Nj) &
        + 4d0*n(idx_C4Nj) &
        + 3d0*n(idx_C3HNj) &
        + n(idx_HNCj) &
        + 3d0*n(idx_SIC3Hj) &
        + 4d0*n(idx_SIC4j) &
        + 2d0*n(idx_SIC2H2j) &
        + n(idx_SICH3j) &
        + 3d0*n(idx_HC2NCHj) &
        + 3d0*n(idx_C3H3j) &
        + 3d0*n(idx_H3C3j) &
        + 5d0*n(idx_C5Hj) &
        + 6d0*n(idx_C6j) &
        + 2d0*n(idx_C2H3Oj) &
        + 2d0*n(idx_C2H5j) &
        + 3d0*n(idx_C3H3Nj) &
        + 5d0*n(idx_C5H2j) &
        + 4d0*n(idx_C4H3j) &
        + 6d0*n(idx_C6Hj) &
        + 7d0*n(idx_C7j) &
        + n(idx_CH4Nj) &
        + 5d0*n(idx_C5HNj) &
        + 7d0*n(idx_C7Hj) &
        + 8d0*n(idx_C8j) &
        + 2d0*n(idx_COOCH4j) &
        + 2d0*n(idx_C2H5Oj) &
        + 8d0*n(idx_C8Hj) &
        + 9d0*n(idx_C9j) &
        + 5d0*n(idx_C5H3j) &
        + 6d0*n(idx_C6H2j) &
        + 6d0*n(idx_C6H3j) &
        + 3d0*n(idx_C2H6COj) &
        + 9d0*n(idx_C9Hj) &
        + 10d0*n(idx_C10j) &
        + 7d0*n(idx_C7H3j) &
        + 8d0*n(idx_C8H2j) &
        + 8d0*n(idx_C8H3j) &
        + n(idx_H2CSj) &
        + 4d0*n(idx_C4Pj) &
        + n(idx_HCO2j) &
        + n(idx_PCH3j) &
        + n(idx_CH4j) &
        + 2d0*n(idx_C2NHj) &
        + n(idx_H2NCj) &
        + 3d0*n(idx_C3H2Nj) &
        + 7d0*n(idx_C7H2j) &
        + 5d0*n(idx_C5H4j) &
        + 7d0*n(idx_C7HNj) &
        + 9d0*n(idx_C9H2j) &
        + 7d0*n(idx_C7H4j) &
        + 9d0*n(idx_C9HNj) &
        + n(idx_CO2j) &
        + 6d0*n(idx_C6H5j) &
        + 5d0*n(idx_C5H5j) &
        + 2d0*n(idx_PC2H2j) &
        + n(idx_PCH2j) &
        + 2d0*n(idx_HC2Sj) &
        + 3d0*n(idx_HC3Sj) &
        + n(idx_H3CSj) &
        + 4d0*n(idx_HC4Sj) &
        + 2d0*n(idx_SIC2H3j) &
        + 3d0*n(idx_SIC3H2j) &
        + 2d0*n(idx_C2HOj) &
        + n(idx_HOCSj) &
        + n(idx_CH5Oj) &
        + n(idx_NCOj) &
        + n(idx_HNCOj) &
        + 2d0*n(idx_C2N2j) &
        + n(idx_CH5j) &
        + n(idx_CH3O2j) &
        + n(idx_PCH4j) &
        + 2d0*n(idx_PC2H3j) &
        + n(idx_SINCHj) &
        + 3d0*n(idx_HC3Oj) &
        + 3d0*n(idx_PC3Hj) &
        + 4d0*n(idx_PC4Hj) &
        + n(idx_NH2CNHj) &
        + 4d0*n(idx_SIC4Hj) &
        + n(idx_SICH4j) &
        + 2d0*n(idx_C2H4Nj) &
        + n(idx_NH2CH2Oj) &
        + 2d0*n(idx_C2H6j) &
        + 3d0*n(idx_C3H4Nj) &
        + 3d0*n(idx_C3H5j) &
        + 4d0*n(idx_C4H4j) &
        + n(idx_CH6Nj) &
        + 5d0*n(idx_C5H2Nj) &
        + 4d0*n(idx_C4H4Nj) &
        + 2d0*n(idx_H5C2O2j) &
        + 2d0*n(idx_C2H5OH2j) &
        + 2d0*n(idx_CH3OCH4j) &
        + 7d0*n(idx_C7H2Nj) &
        + 3d0*n(idx_C3H6OHj) &
        + 6d0*n(idx_C6H4Nj) &
        + 10d0*n(idx_C10Hj) &
        + 9d0*n(idx_C9H3j) &
        + 7d0*n(idx_C7H5j) &
        + 8d0*n(idx_C8H4Nj) &
        + 9d0*n(idx_C9H2Nj) &
        + 6d0*n(idx_C6H7j) &
        + 2d0*n(idx_PC2H4j) &
        + 4d0*n(idx_C4H5j) &
        + n(idx_H2CCLj) &
        + 4d0*n(idx_PC4H2j) &
        + 6d0*n(idx_C6H4j) &
        + 8d0*n(idx_C8H4j) &
        + 9d0*n(idx_C9H4j) &
        + 4d0*n(idx_C4H7j) &
        + 4d0*n(idx_HC4Nj) &
        + 4d0*n(idx_HC4Oj) &
        + 5d0*n(idx_C5Nj) &
        + 4d0*n(idx_H2C4Nj) &
        + 4d0*n(idx_H3C4Nj) &
        + 7d0*n(idx_C7Nj) &
        + 5d0*n(idx_C5H3Nj) &
        + 10d0*n(idx_C10H2j) &
        + 9d0*n(idx_C9Nj) &
        + 7d0*n(idx_C7H3Nj) &
        + 9d0*n(idx_C9H3Nj) &
        + n(idx_OCSjH2) &
        + 3d0*n(idx_H2C3Oj) &
        + 3d0*n(idx_H3C3Oj) &
        + 5d0*n(idx_C5H4Nj) &
        + 8d0*n(idx_C8H5j) &
        + 9d0*n(idx_C9H5j) &
        + 2d0*n(idx_H2COHOCH2j) &
        + 2d0*n(idx_H7C2O2j)

    zC = max(zC, 0d0)

    get_metallicityC = log10(zC/nH+1d-40) - (-3.5700000000000003)

    phys_metallicity = get_metallicityC

  end function get_metallicityC

  !*****************************
  ! get metallicity using N as reference
  function get_metallicityN(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityN,zN,nH

    nH = get_Hnuclei(n(:))

    zN = n(idx_CNk) &
        + n(idx_N) &
        + n(idx_CN) &
        + 2d0*n(idx_N2) &
        + n(idx_NH) &
        + n(idx_NO) &
        + n(idx_NS) &
        + n(idx_PN) &
        + n(idx_SIN) &
        + n(idx_C2N) &
        + n(idx_HCN) &
        + n(idx_HNC) &
        + n(idx_HNO) &
        + n(idx_HNSI) &
        + 2d0*n(idx_N2O) &
        + n(idx_NH2) &
        + n(idx_NO2) &
        + n(idx_OCN) &
        + n(idx_SINC) &
        + n(idx_C3N) &
        + n(idx_NH3) &
        + n(idx_C2H2N) &
        + n(idx_C4N) &
        + n(idx_CH3N) &
        + n(idx_HC3N) &
        + n(idx_C2H3N) &
        + n(idx_C5N) &
        + n(idx_C3H3N) &
        + n(idx_CH5N) &
        + n(idx_HC5N) &
        + n(idx_C7N) &
        + n(idx_CH3C3N) &
        + n(idx_HC7N) &
        + n(idx_C9N) &
        + n(idx_CH3C5N) &
        + n(idx_CH3C7N) &
        + n(idx_HC9N) &
        + n(idx_HCNC2) &
        + n(idx_HC2NC) &
        + n(idx_HNC3) &
        + n(idx_NH2CHO) &
        + 2d0*n(idx_NH2CN) &
        + n(idx_H2CN) &
        + n(idx_HNCO) &
        + n(idx_HCCN) &
        + 2d0*n(idx_N2H2) &
        + n(idx_CHNH) &
        + n(idx_H2C3N) &
        + n(idx_H2C5N) &
        + n(idx_H2C7N) &
        + n(idx_H2C9N) &
        + n(idx_NH2OH) &
        + n(idx_H3C5N) &
        + n(idx_H3C7N) &
        + n(idx_H3C9N) &
        + n(idx_CH3NH) &
        + n(idx_H4C3N) &
        + n(idx_H5C3N) &
        + n(idx_C2N_DUST) &
        + n(idx_C3N_DUST) &
        + n(idx_CN_DUST) &
        + n(idx_N_DUST) &
        + n(idx_NH_DUST) &
        + n(idx_HNC_DUST) &
        + n(idx_NH2_DUST) &
        + n(idx_NO_DUST) &
        + n(idx_OCN_DUST) &
        + n(idx_NS_DUST) &
        + n(idx_HCCN_DUST) &
        + n(idx_HNO_DUST) &
        + n(idx_HCN_DUST) &
        + n(idx_CHNH_DUST) &
        + n(idx_CH3N_DUST) &
        + n(idx_C2H2N_DUST) &
        + n(idx_CH2NH2_DUST) &
        + n(idx_CH3C3N_DUST) &
        + n(idx_C5N_DUST) &
        + n(idx_CH3C5N_DUST) &
        + n(idx_C7N_DUST) &
        + n(idx_CH3C7N_DUST) &
        + n(idx_C2H3N_DUST) &
        + n(idx_C3H3N_DUST) &
        + n(idx_H4C3N_DUST) &
        + n(idx_HC3N_DUST) &
        + n(idx_HC5N_DUST) &
        + n(idx_HC7N_DUST) &
        + n(idx_C9N_DUST) &
        + n(idx_HC9N_DUST) &
        + n(idx_CH3NH_DUST) &
        + n(idx_CH5N_DUST) &
        + n(idx_H2C3N_DUST) &
        + n(idx_H2C5N_DUST) &
        + n(idx_H3C5N_DUST) &
        + n(idx_H2C7N_DUST) &
        + n(idx_H3C7N_DUST) &
        + n(idx_H2C9N_DUST) &
        + n(idx_H3C9N_DUST) &
        + n(idx_H2CN_DUST) &
        + n(idx_H5C3N_DUST) &
        + 2d0*n(idx_N2H2_DUST) &
        + 2d0*n(idx_N2_DUST) &
        + n(idx_NH3_DUST) &
        + n(idx_HNCO_DUST) &
        + n(idx_NH2CHO_DUST) &
        + n(idx_NH2OH_DUST) &
        + n(idx_C4N_DUST) &
        + n(idx_CH2NH2) &
        + n(idx_HC2NC_DUST) &
        + n(idx_HCNC2_DUST) &
        + n(idx_HNC3_DUST) &
        + n(idx_HNSI_DUST) &
        + n(idx_SIN_DUST) &
        + 2d0*n(idx_N2O_DUST) &
        + 2d0*n(idx_NH2CN_DUST) &
        + n(idx_NO2_DUST) &
        + n(idx_PN_DUST) &
        + n(idx_SINC_DUST) &
        + n(idx_Nj) &
        + n(idx_NOj) &
        + n(idx_HNOj) &
        + n(idx_NH2j) &
        + n(idx_NH3j) &
        + n(idx_C2H3Nj) &
        + n(idx_CH5Nj) &
        + n(idx_CNj) &
        + n(idx_NSj) &
        + n(idx_SINj) &
        + n(idx_C2Nj) &
        + n(idx_CNCj) &
        + n(idx_SINCj) &
        + n(idx_HCNj) &
        + n(idx_HCNHj) &
        + n(idx_C2H2Nj) &
        + n(idx_C3Nj) &
        + n(idx_C4Nj) &
        + n(idx_C3HNj) &
        + n(idx_HNCj) &
        + n(idx_HC2NCHj) &
        + n(idx_C3H3Nj) &
        + n(idx_CH4Nj) &
        + n(idx_C5HNj) &
        + n(idx_NHj) &
        + n(idx_PNj) &
        + n(idx_HNSIj) &
        + n(idx_C2NHj) &
        + n(idx_NH4j) &
        + n(idx_H2NCj) &
        + n(idx_C3H2Nj) &
        + n(idx_C7HNj) &
        + n(idx_C9HNj) &
        + 2d0*n(idx_N2j) &
        + 2d0*n(idx_N2Hj) &
        + n(idx_NO2j) &
        + n(idx_PNH2j) &
        + n(idx_SINH2j) &
        + n(idx_NCOj) &
        + n(idx_HNCOj) &
        + 2d0*n(idx_C2N2j) &
        + n(idx_PNH3j) &
        + n(idx_HNSj) &
        + n(idx_HPNj) &
        + n(idx_H2NOj) &
        + n(idx_SINCHj) &
        + 2d0*n(idx_NH2CNHj) &
        + n(idx_C2H4Nj) &
        + n(idx_NH2CH2Oj) &
        + n(idx_C3H4Nj) &
        + n(idx_CH6Nj) &
        + n(idx_C5H2Nj) &
        + n(idx_C4H4Nj) &
        + n(idx_C7H2Nj) &
        + n(idx_C6H4Nj) &
        + n(idx_C8H4Nj) &
        + n(idx_C9H2Nj) &
        + n(idx_HC4Nj) &
        + n(idx_C5Nj) &
        + n(idx_H2C4Nj) &
        + n(idx_H3C4Nj) &
        + n(idx_C7Nj) &
        + n(idx_C5H3Nj) &
        + n(idx_C9Nj) &
        + n(idx_C7H3Nj) &
        + n(idx_C9H3Nj) &
        + n(idx_C5H4Nj)

    zN = max(zN, 0d0)

    get_metallicityN = log10(zN/nH+1d-40) - (-4.17)

    phys_metallicity = get_metallicityN

  end function get_metallicityN

  !*****************************
  ! get metallicity using O as reference
  function get_metallicityO(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityO,zO,nH

    nH = get_Hnuclei(n(:))

    zO = n(idx_Ok) &
        + n(idx_OHk) &
        + n(idx_O) &
        + n(idx_CLO) &
        + n(idx_CO) &
        + n(idx_NO) &
        + 2d0*n(idx_O2) &
        + n(idx_OH) &
        + n(idx_PO) &
        + n(idx_SIO) &
        + n(idx_SO) &
        + n(idx_CCO) &
        + 2d0*n(idx_CO2) &
        + n(idx_H2O) &
        + n(idx_HCO) &
        + n(idx_HNO) &
        + n(idx_HPO) &
        + n(idx_N2O) &
        + n(idx_NAOH) &
        + 2d0*n(idx_NO2) &
        + 2d0*n(idx_O2H) &
        + n(idx_OCN) &
        + n(idx_OCS) &
        + 2d0*n(idx_SIO2) &
        + 2d0*n(idx_SO2) &
        + n(idx_C3O) &
        + n(idx_H2CO) &
        + 2d0*n(idx_H2O2) &
        + n(idx_H2SIO) &
        + n(idx_C2H2O) &
        + 2d0*n(idx_CH2O2) &
        + n(idx_CH4O) &
        + n(idx_C2H4O) &
        + 2d0*n(idx_HCOOCH3) &
        + n(idx_C2H5OH) &
        + n(idx_CH3OCH3) &
        + n(idx_C2H6CO) &
        + n(idx_NH2CHO) &
        + 3d0*n(idx_O3) &
        + n(idx_HNCO) &
        + n(idx_HC2O) &
        + n(idx_HC3O) &
        + n(idx_H2C3O) &
        + n(idx_NH2OH) &
        + n(idx_CH2OH) &
        + n(idx_CCO_DUST) &
        + n(idx_C3O_DUST) &
        + n(idx_NO_DUST) &
        + n(idx_O_DUST) &
        + n(idx_OCN_DUST) &
        + n(idx_CO_DUST) &
        + 2d0*n(idx_O2_DUST) &
        + n(idx_OH_DUST) &
        + n(idx_SO_DUST) &
        + n(idx_HNO_DUST) &
        + n(idx_HCO_DUST) &
        + n(idx_H2CO_DUST) &
        + n(idx_CH2OH_DUST) &
        + n(idx_C2H5OH_DUST) &
        + n(idx_CH3OCH3_DUST) &
        + n(idx_C2H4O_DUST) &
        + n(idx_HC2O_DUST) &
        + n(idx_HC3O_DUST) &
        + n(idx_CH4O_DUST) &
        + 2d0*n(idx_H2O2_DUST) &
        + n(idx_H2O_DUST) &
        + 2d0*n(idx_O2H_DUST) &
        + n(idx_C2H2O_DUST) &
        + n(idx_H2C3O_DUST) &
        + 3d0*n(idx_O3_DUST) &
        + n(idx_HNCO_DUST) &
        + n(idx_OCS_DUST) &
        + 2d0*n(idx_SO2_DUST) &
        + 2d0*n(idx_HCOOCH3_DUST) &
        + n(idx_NH2CHO_DUST) &
        + 2d0*n(idx_CO2_DUST) &
        + 2d0*n(idx_CH2O2_DUST) &
        + n(idx_NH2OH_DUST) &
        + n(idx_NAOH_DUST) &
        + n(idx_SIO_DUST) &
        + n(idx_C2H6CO_DUST) &
        + n(idx_CLO_DUST) &
        + n(idx_H2SIO_DUST) &
        + n(idx_HPO_DUST) &
        + n(idx_PO_DUST) &
        + n(idx_N2O_DUST) &
        + 2d0*n(idx_NO2_DUST) &
        + 2d0*n(idx_SIO2_DUST) &
        + n(idx_Oj) &
        + n(idx_COj) &
        + n(idx_NOj) &
        + 2d0*n(idx_O2j) &
        + n(idx_HCOj) &
        + n(idx_HNOj) &
        + n(idx_OCSj) &
        + n(idx_C2H2Oj) &
        + 2d0*n(idx_CH2O2j) &
        + n(idx_H3COj) &
        + n(idx_CH4Oj) &
        + n(idx_C2H4Oj) &
        + n(idx_C2H5OHj) &
        + n(idx_CH3OCH3j) &
        + n(idx_CLOj) &
        + n(idx_POj) &
        + n(idx_SOj) &
        + n(idx_C2Oj) &
        + n(idx_HOCj) &
        + n(idx_HPOj) &
        + n(idx_C3Oj) &
        + n(idx_H2COj) &
        + n(idx_H2SIOj) &
        + n(idx_C2H3Oj) &
        + 2d0*n(idx_COOCH4j) &
        + n(idx_C2H5Oj) &
        + n(idx_C2H6COj) &
        + n(idx_OHj) &
        + n(idx_SIOj) &
        + n(idx_H2Oj) &
        + n(idx_HSIOj) &
        + 2d0*n(idx_HCO2j) &
        + 2d0*n(idx_CO2j) &
        + 2d0*n(idx_SO2j) &
        + 2d0*n(idx_NO2j) &
        + n(idx_C2HOj) &
        + n(idx_H3Oj) &
        + n(idx_HOCSj) &
        + n(idx_CH5Oj) &
        + n(idx_NCOj) &
        + n(idx_HNCOj) &
        + 2d0*n(idx_O2Hj) &
        + 2d0*n(idx_CH3O2j) &
        + n(idx_H2POj) &
        + n(idx_HSOj) &
        + n(idx_H2NOj) &
        + n(idx_NAH2Oj) &
        + 2d0*n(idx_HSIO2j) &
        + 2d0*n(idx_HSO2j) &
        + n(idx_HC3Oj) &
        + n(idx_H3SIOj) &
        + n(idx_NH2CH2Oj) &
        + 2d0*n(idx_H5C2O2j) &
        + n(idx_C2H5OH2j) &
        + n(idx_CH3OCH4j) &
        + n(idx_C3H6OHj) &
        + n(idx_HC4Oj) &
        + n(idx_OCSjH2) &
        + n(idx_H2C3Oj) &
        + n(idx_H3C3Oj) &
        + 2d0*n(idx_H2COHOCH2j) &
        + 2d0*n(idx_H7C2O2j)

    zO = max(zO, 0d0)

    get_metallicityO = log10(zO/nH+1d-40) - (-3.3100000000000005)

    phys_metallicity = get_metallicityO

  end function get_metallicityO

  !*****************************
  ! get metallicity using S as reference
  function get_metallicityS(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityS,zS,nH

    nH = get_Hnuclei(n(:))

    zS = n(idx_Sk) &
        + n(idx_S) &
        + n(idx_CS) &
        + n(idx_HS) &
        + n(idx_NS) &
        + 2d0*n(idx_S2) &
        + n(idx_SIS) &
        + n(idx_SO) &
        + n(idx_C2S) &
        + n(idx_H2S) &
        + n(idx_HCS) &
        + 2d0*n(idx_HS2) &
        + n(idx_OCS) &
        + n(idx_SO2) &
        + n(idx_C3S) &
        + n(idx_H2CS) &
        + 2d0*n(idx_H2S2) &
        + n(idx_C4S) &
        + n(idx_C2S_DUST) &
        + n(idx_C3S_DUST) &
        + n(idx_HS_DUST) &
        + n(idx_CS_DUST) &
        + n(idx_NS_DUST) &
        + n(idx_S_DUST) &
        + n(idx_SO_DUST) &
        + n(idx_HCS_DUST) &
        + n(idx_H2S_DUST) &
        + n(idx_H2CS_DUST) &
        + n(idx_OCS_DUST) &
        + n(idx_SO2_DUST) &
        + n(idx_C4S_DUST) &
        + 2d0*n(idx_H2S2_DUST) &
        + 2d0*n(idx_HS2_DUST) &
        + 2d0*n(idx_S2_DUST) &
        + n(idx_SIS_DUST) &
        + n(idx_Sj) &
        + n(idx_H2Sj) &
        + n(idx_HCSj) &
        + n(idx_OCSj) &
        + n(idx_CSj) &
        + n(idx_NSj) &
        + n(idx_SISj) &
        + n(idx_SOj) &
        + n(idx_C2Sj) &
        + n(idx_C3Sj) &
        + n(idx_C4Sj) &
        + n(idx_HSj) &
        + 2d0*n(idx_S2j) &
        + 2d0*n(idx_S2Hj) &
        + n(idx_H2CSj) &
        + 2d0*n(idx_H2S2j) &
        + n(idx_SO2j) &
        + n(idx_HC2Sj) &
        + n(idx_HC3Sj) &
        + n(idx_H3CSj) &
        + n(idx_HC4Sj) &
        + n(idx_H3Sj) &
        + n(idx_HOCSj) &
        + n(idx_HSISj) &
        + n(idx_HSOj) &
        + n(idx_HNSj) &
        + n(idx_HSO2j) &
        + 2d0*n(idx_H3S2j) &
        + n(idx_OCSjH2)

    zS = max(zS, 0d0)

    get_metallicityS = log10(zS/nH+1d-40) - (-4.88)

    phys_metallicity = get_metallicityS

  end function get_metallicityS

  !*****************************
  ! get metallicity using Cl as reference
  function get_metallicityCl(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityCl,zCl,nH

    nH = get_Hnuclei(n(:))

    zCl = n(idx_CL) &
        + n(idx_CCL) &
        + n(idx_CLO) &
        + n(idx_HCL) &
        + n(idx_CCL_DUST) &
        + n(idx_CL_DUST) &
        + n(idx_CLO_DUST) &
        + n(idx_HCL_DUST) &
        + n(idx_CLj) &
        + n(idx_CCLj) &
        + n(idx_CLOj) &
        + n(idx_HCLj) &
        + n(idx_H2CLj) &
        + n(idx_H2CCLj)

    zCl = max(zCl, 0d0)

    get_metallicityCl = log10(zCl/nH+1d-40) - (-6.5)

    phys_metallicity = get_metallicityCl

  end function get_metallicityCl

  !*****************************
  ! get metallicity using Fe as reference
  function get_metallicityFe(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityFe,zFe,nH

    nH = get_Hnuclei(n(:))

    zFe = n(idx_FE) &
        + n(idx_FEH) &
        + n(idx_FE_DUST) &
        + n(idx_FEH_DUST) &
        + n(idx_FEj)

    zFe = max(zFe, 0d0)

    get_metallicityFe = log10(zFe/nH+1d-40) - (-4.5)

    phys_metallicity = get_metallicityFe

  end function get_metallicityFe

  !*****************************
  ! get metallicity using Mg as reference
  function get_metallicityMg(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityMg,zMg,nH

    nH = get_Hnuclei(n(:))

    zMg = n(idx_MG) &
        + n(idx_MGH) &
        + n(idx_MGH2) &
        + n(idx_MG_DUST) &
        + n(idx_MGH_DUST) &
        + n(idx_MGH2_DUST) &
        + n(idx_MGj)

    zMg = max(zMg, 0d0)

    get_metallicityMg = log10(zMg/nH+1d-40) - (-4.4)

    phys_metallicity = get_metallicityMg

  end function get_metallicityMg

  !*****************************
  ! get metallicity using Na as reference
  function get_metallicityNa(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityNa,zNa,nH

    nH = get_Hnuclei(n(:))

    zNa = n(idx_NA) &
        + n(idx_NAH) &
        + n(idx_NAOH) &
        + n(idx_NA_DUST) &
        + n(idx_NAH_DUST) &
        + n(idx_NAOH_DUST) &
        + n(idx_NAj) &
        + n(idx_NAH2Oj) &
        + n(idx_NAH2j)

    zNa = max(zNa, 0d0)

    get_metallicityNa = log10(zNa/nH+1d-40) - (-5.76)

    phys_metallicity = get_metallicityNa

  end function get_metallicityNa

  !*****************************
  ! get metallicity using P as reference
  function get_metallicityP(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityP,zP,nH

    nH = get_Hnuclei(n(:))

    zP = n(idx_P) &
        + n(idx_CP) &
        + n(idx_PH) &
        + n(idx_PN) &
        + n(idx_PO) &
        + n(idx_CCP) &
        + n(idx_HCP) &
        + n(idx_HPO) &
        + n(idx_PH2) &
        + n(idx_C3P) &
        + n(idx_HCCP) &
        + n(idx_C4P) &
        + n(idx_CH2PH) &
        + n(idx_C3P_DUST) &
        + n(idx_CCP_DUST) &
        + n(idx_C4P_DUST) &
        + n(idx_P_DUST) &
        + n(idx_CP_DUST) &
        + n(idx_CH2PH_DUST) &
        + n(idx_HCP_DUST) &
        + n(idx_HCCP_DUST) &
        + n(idx_HPO_DUST) &
        + n(idx_PO_DUST) &
        + n(idx_PH_DUST) &
        + n(idx_PH2_DUST) &
        + n(idx_PN_DUST) &
        + n(idx_Pj) &
        + n(idx_CPj) &
        + n(idx_PHj) &
        + n(idx_POj) &
        + n(idx_CCPj) &
        + n(idx_HCPj) &
        + n(idx_HPOj) &
        + n(idx_PC2Hj) &
        + n(idx_PNj) &
        + n(idx_PH2j) &
        + n(idx_C4Pj) &
        + n(idx_PCH3j) &
        + n(idx_PC2H2j) &
        + n(idx_PNH2j) &
        + n(idx_PCH2j) &
        + n(idx_H2POj) &
        + n(idx_PNH3j) &
        + n(idx_PCH4j) &
        + n(idx_PC2H3j) &
        + n(idx_HPNj) &
        + n(idx_PH3j) &
        + n(idx_PC3Hj) &
        + n(idx_PC4Hj) &
        + n(idx_PC2H4j) &
        + n(idx_PC4H2j)

    zP = max(zP, 0d0)

    get_metallicityP = log10(zP/nH+1d-40) - (-6.59)

    phys_metallicity = get_metallicityP

  end function get_metallicityP

  !*****************************
  ! get metallicity using Si as reference
  function get_metallicitySi(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicitySi,zSi,nH

    nH = get_Hnuclei(n(:))

    zSi = n(idx_SI) &
        + n(idx_SIC) &
        + n(idx_SIH) &
        + n(idx_SIN) &
        + n(idx_SIO) &
        + n(idx_SIS) &
        + n(idx_HCSI) &
        + n(idx_HNSI) &
        + n(idx_SIC2) &
        + n(idx_SIH2) &
        + n(idx_SINC) &
        + n(idx_SIO2) &
        + n(idx_H2SIO) &
        + n(idx_SIC2H) &
        + n(idx_SIC3) &
        + n(idx_SICH2) &
        + n(idx_SIH3) &
        + n(idx_SIC2H2) &
        + n(idx_SIC3H) &
        + n(idx_SIC4) &
        + n(idx_SICH3) &
        + n(idx_SIH4) &
        + n(idx_SI_DUST) &
        + n(idx_SIH_DUST) &
        + n(idx_SIH2_DUST) &
        + n(idx_SIH3_DUST) &
        + n(idx_SIH4_DUST) &
        + n(idx_SIC_DUST) &
        + n(idx_SIO_DUST) &
        + n(idx_SIS_DUST) &
        + n(idx_H2SIO_DUST) &
        + n(idx_HCSI_DUST) &
        + n(idx_HNSI_DUST) &
        + n(idx_SIN_DUST) &
        + n(idx_SIC2_DUST) &
        + n(idx_SIC2H_DUST) &
        + n(idx_SIC2H2_DUST) &
        + n(idx_SIC3_DUST) &
        + n(idx_SIC3H_DUST) &
        + n(idx_SIC4_DUST) &
        + n(idx_SICH2_DUST) &
        + n(idx_SICH3_DUST) &
        + n(idx_SINC_DUST) &
        + n(idx_SIO2_DUST) &
        + n(idx_SIj) &
        + n(idx_SICj) &
        + n(idx_SINj) &
        + n(idx_SISj) &
        + n(idx_SIC2j) &
        + n(idx_SINCj) &
        + n(idx_CHSIj) &
        + n(idx_SIH2j) &
        + n(idx_H2SIOj) &
        + n(idx_SIC2Hj) &
        + n(idx_SIC3j) &
        + n(idx_CH2SIj) &
        + n(idx_SIH3j) &
        + n(idx_SIC3Hj) &
        + n(idx_SIC4j) &
        + n(idx_SIC2H2j) &
        + n(idx_SICH3j) &
        + n(idx_SIHj) &
        + n(idx_SIOj) &
        + n(idx_HNSIj) &
        + n(idx_HSIOj) &
        + n(idx_SIH4j) &
        + n(idx_SINH2j) &
        + n(idx_SIC2H3j) &
        + n(idx_SIC3H2j) &
        + n(idx_HSISj) &
        + n(idx_SINCHj) &
        + n(idx_HSIO2j) &
        + n(idx_H3SIOj) &
        + n(idx_SIC4Hj) &
        + n(idx_SICH4j) &
        + n(idx_SIH5j)

    zSi = max(zSi, 0d0)

    get_metallicitySi = log10(zSi/nH+1d-40) - (-4.49)

    phys_metallicity = get_metallicitySi

  end function get_metallicitySi

end module krome_getphys
