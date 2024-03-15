
!############### MODULE ##############
module krome_user
  implicit none

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2024-03-14 13:09:12
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
  integer,parameter::KROME_idx_Ck = 3	!C-
  integer,parameter::KROME_idx_CNk = 4	!CN-
  integer,parameter::KROME_idx_Ok = 5	!O-
  integer,parameter::KROME_idx_OHk = 6	!OH-
  integer,parameter::KROME_idx_Sk = 7	!S-
  integer,parameter::KROME_idx_GRAINk = 8	!GRAIN-
  integer,parameter::KROME_idx_C = 9	!C
  integer,parameter::KROME_idx_CL = 10	!CL
  integer,parameter::KROME_idx_FE = 11	!FE
  integer,parameter::KROME_idx_H = 12	!H
  integer,parameter::KROME_idx_HE = 13	!HE
  integer,parameter::KROME_idx_MG = 14	!MG
  integer,parameter::KROME_idx_N = 15	!N
  integer,parameter::KROME_idx_NA = 16	!NA
  integer,parameter::KROME_idx_O = 17	!O
  integer,parameter::KROME_idx_P = 18	!P
  integer,parameter::KROME_idx_S = 19	!S
  integer,parameter::KROME_idx_SI = 20	!SI
  integer,parameter::KROME_idx_C2 = 21	!C2
  integer,parameter::KROME_idx_CCL = 22	!CCL
  integer,parameter::KROME_idx_CH = 23	!CH
  integer,parameter::KROME_idx_CLO = 24	!CLO
  integer,parameter::KROME_idx_CN = 25	!CN
  integer,parameter::KROME_idx_CO = 26	!CO
  integer,parameter::KROME_idx_CP = 27	!CP
  integer,parameter::KROME_idx_CS = 28	!CS
  integer,parameter::KROME_idx_H2 = 29	!H2
  integer,parameter::KROME_idx_HCL = 30	!HCL
  integer,parameter::KROME_idx_HS = 31	!HS
  integer,parameter::KROME_idx_MGH = 32	!MGH
  integer,parameter::KROME_idx_N2 = 33	!N2
  integer,parameter::KROME_idx_NAH = 34	!NAH
  integer,parameter::KROME_idx_NH = 35	!NH
  integer,parameter::KROME_idx_NO = 36	!NO
  integer,parameter::KROME_idx_NS = 37	!NS
  integer,parameter::KROME_idx_O2 = 38	!O2
  integer,parameter::KROME_idx_OH = 39	!OH
  integer,parameter::KROME_idx_PH = 40	!PH
  integer,parameter::KROME_idx_PN = 41	!PN
  integer,parameter::KROME_idx_PO = 42	!PO
  integer,parameter::KROME_idx_S2 = 43	!S2
  integer,parameter::KROME_idx_SIC = 44	!SIC
  integer,parameter::KROME_idx_SIH = 45	!SIH
  integer,parameter::KROME_idx_SIN = 46	!SIN
  integer,parameter::KROME_idx_SIO = 47	!SIO
  integer,parameter::KROME_idx_SIS = 48	!SIS
  integer,parameter::KROME_idx_SO = 49	!SO
  integer,parameter::KROME_idx_C2H = 50	!C2H
  integer,parameter::KROME_idx_C2N = 51	!C2N
  integer,parameter::KROME_idx_C2S = 52	!C2S
  integer,parameter::KROME_idx_C3 = 53	!C3
  integer,parameter::KROME_idx_CCO = 54	!CCO
  integer,parameter::KROME_idx_CCP = 55	!CCP
  integer,parameter::KROME_idx_CH2 = 56	!CH2
  integer,parameter::KROME_idx_CO2 = 57	!CO2
  integer,parameter::KROME_idx_H2O = 58	!H2O
  integer,parameter::KROME_idx_H2S = 59	!H2S
  integer,parameter::KROME_idx_HCN = 60	!HCN
  integer,parameter::KROME_idx_HCO = 61	!HCO
  integer,parameter::KROME_idx_HCP = 62	!HCP
  integer,parameter::KROME_idx_HCS = 63	!HCS
  integer,parameter::KROME_idx_HCSI = 64	!HCSI
  integer,parameter::KROME_idx_HNC = 65	!HNC
  integer,parameter::KROME_idx_HNO = 66	!HNO
  integer,parameter::KROME_idx_HNSI = 67	!HNSI
  integer,parameter::KROME_idx_HPO = 68	!HPO
  integer,parameter::KROME_idx_HS2 = 69	!HS2
  integer,parameter::KROME_idx_N2O = 70	!N2O
  integer,parameter::KROME_idx_NAOH = 71	!NAOH
  integer,parameter::KROME_idx_NH2 = 72	!NH2
  integer,parameter::KROME_idx_NO2 = 73	!NO2
  integer,parameter::KROME_idx_O2H = 74	!O2H
  integer,parameter::KROME_idx_OCN = 75	!OCN
  integer,parameter::KROME_idx_OCS = 76	!OCS
  integer,parameter::KROME_idx_PH2 = 77	!PH2
  integer,parameter::KROME_idx_SIC2 = 78	!SIC2
  integer,parameter::KROME_idx_SIH2 = 79	!SIH2
  integer,parameter::KROME_idx_SINC = 80	!SINC
  integer,parameter::KROME_idx_SIO2 = 81	!SIO2
  integer,parameter::KROME_idx_SO2 = 82	!SO2
  integer,parameter::KROME_idx_C2H2 = 83	!C2H2
  integer,parameter::KROME_idx_C3H = 84	!C3H
  integer,parameter::KROME_idx_C3N = 85	!C3N
  integer,parameter::KROME_idx_C3O = 86	!C3O
  integer,parameter::KROME_idx_C3P = 87	!C3P
  integer,parameter::KROME_idx_C3S = 88	!C3S
  integer,parameter::KROME_idx_C4 = 89	!C4
  integer,parameter::KROME_idx_CH3 = 90	!CH3
  integer,parameter::KROME_idx_H2CO = 91	!H2CO
  integer,parameter::KROME_idx_H2CS = 92	!H2CS
  integer,parameter::KROME_idx_H2O2 = 93	!H2O2
  integer,parameter::KROME_idx_H2S2 = 94	!H2S2
  integer,parameter::KROME_idx_H2SIO = 95	!H2SIO
  integer,parameter::KROME_idx_HCCP = 96	!HCCP
  integer,parameter::KROME_idx_NH3 = 97	!NH3
  integer,parameter::KROME_idx_SIC2H = 98	!SIC2H
  integer,parameter::KROME_idx_SIC3 = 99	!SIC3
  integer,parameter::KROME_idx_SICH2 = 100	!SICH2
  integer,parameter::KROME_idx_SIH3 = 101	!SIH3
  integer,parameter::KROME_idx_C2H2N = 102	!C2H2N
  integer,parameter::KROME_idx_C2H2O = 103	!C2H2O
  integer,parameter::KROME_idx_C2H3 = 104	!C2H3
  integer,parameter::KROME_idx_C3H2 = 105	!C3H2
  integer,parameter::KROME_idx_C4H = 106	!C4H
  integer,parameter::KROME_idx_C4N = 107	!C4N
  integer,parameter::KROME_idx_C4P = 108	!C4P
  integer,parameter::KROME_idx_C4S = 109	!C4S
  integer,parameter::KROME_idx_C5 = 110	!C5
  integer,parameter::KROME_idx_CH2O2 = 111	!CH2O2
  integer,parameter::KROME_idx_CH2PH = 112	!CH2PH
  integer,parameter::KROME_idx_CH3N = 113	!CH3N
  integer,parameter::KROME_idx_CH4 = 114	!CH4
  integer,parameter::KROME_idx_HC3N = 115	!HC3N
  integer,parameter::KROME_idx_SIC2H2 = 116	!SIC2H2
  integer,parameter::KROME_idx_SIC3H = 117	!SIC3H
  integer,parameter::KROME_idx_SIC4 = 118	!SIC4
  integer,parameter::KROME_idx_SICH3 = 119	!SICH3
  integer,parameter::KROME_idx_SIH4 = 120	!SIH4
  integer,parameter::KROME_idx_C2H3N = 121	!C2H3N
  integer,parameter::KROME_idx_C2H4 = 122	!C2H4
  integer,parameter::KROME_idx_C3H3 = 123	!C3H3
  integer,parameter::KROME_idx_C4H2 = 124	!C4H2
  integer,parameter::KROME_idx_C5H = 125	!C5H
  integer,parameter::KROME_idx_C5N = 126	!C5N
  integer,parameter::KROME_idx_C6 = 127	!C6
  integer,parameter::KROME_idx_CH4O = 128	!CH4O
  integer,parameter::KROME_idx_C2H4O = 129	!C2H4O
  integer,parameter::KROME_idx_C2H5 = 130	!C2H5
  integer,parameter::KROME_idx_C3H3N = 131	!C3H3N
  integer,parameter::KROME_idx_C3H4 = 132	!C3H4
  integer,parameter::KROME_idx_C5H2 = 133	!C5H2
  integer,parameter::KROME_idx_C6H = 134	!C6H
  integer,parameter::KROME_idx_C7 = 135	!C7
  integer,parameter::KROME_idx_CH5N = 136	!CH5N
  integer,parameter::KROME_idx_HC5N = 137	!HC5N
  integer,parameter::KROME_idx_C6H2 = 138	!C6H2
  integer,parameter::KROME_idx_C7H = 139	!C7H
  integer,parameter::KROME_idx_C7N = 140	!C7N
  integer,parameter::KROME_idx_C8 = 141	!C8
  integer,parameter::KROME_idx_CH3C3N = 142	!CH3C3N
  integer,parameter::KROME_idx_HCOOCH3 = 143	!HCOOCH3
  integer,parameter::KROME_idx_C2H5OH = 144	!C2H5OH
  integer,parameter::KROME_idx_C7H2 = 145	!C7H2
  integer,parameter::KROME_idx_C8H = 146	!C8H
  integer,parameter::KROME_idx_C9 = 147	!C9
  integer,parameter::KROME_idx_CH3C4H = 148	!CH3C4H
  integer,parameter::KROME_idx_CH3OCH3 = 149	!CH3OCH3
  integer,parameter::KROME_idx_HC7N = 150	!HC7N
  integer,parameter::KROME_idx_C2H6CO = 151	!C2H6CO
  integer,parameter::KROME_idx_C8H2 = 152	!C8H2
  integer,parameter::KROME_idx_C9H = 153	!C9H
  integer,parameter::KROME_idx_C9N = 154	!C9N
  integer,parameter::KROME_idx_C10 = 155	!C10
  integer,parameter::KROME_idx_CH3C5N = 156	!CH3C5N
  integer,parameter::KROME_idx_C9H2 = 157	!C9H2
  integer,parameter::KROME_idx_CH3C6H = 158	!CH3C6H
  integer,parameter::KROME_idx_CH3C7N = 159	!CH3C7N
  integer,parameter::KROME_idx_HC9N = 160	!HC9N
  integer,parameter::KROME_idx_C4H4 = 161	!C4H4
  integer,parameter::KROME_idx_HCNC2 = 162	!HCNC2
  integer,parameter::KROME_idx_HC2NC = 163	!HC2NC
  integer,parameter::KROME_idx_HNC3 = 164	!HNC3
  integer,parameter::KROME_idx_NH2CHO = 165	!NH2CHO
  integer,parameter::KROME_idx_C4H3 = 166	!C4H3
  integer,parameter::KROME_idx_NH2CN = 167	!NH2CN
  integer,parameter::KROME_idx_C6H6 = 168	!C6H6
  integer,parameter::KROME_idx_H2CN = 169	!H2CN
  integer,parameter::KROME_idx_GRAIN0 = 170	!GRAIN0
  integer,parameter::KROME_idx_O3 = 171	!O3
  integer,parameter::KROME_idx_FEH = 172	!FEH
  integer,parameter::KROME_idx_HNCO = 173	!HNCO
  integer,parameter::KROME_idx_HC2O = 174	!HC2O
  integer,parameter::KROME_idx_HCCN = 175	!HCCN
  integer,parameter::KROME_idx_HC3O = 176	!HC3O
  integer,parameter::KROME_idx_MGH2 = 177	!MGH2
  integer,parameter::KROME_idx_N2H2 = 178	!N2H2
  integer,parameter::KROME_idx_CHNH = 179	!CHNH
  integer,parameter::KROME_idx_H2C3O = 180	!H2C3O
  integer,parameter::KROME_idx_H2C3N = 181	!H2C3N
  integer,parameter::KROME_idx_H2C5N = 182	!H2C5N
  integer,parameter::KROME_idx_H2C7N = 183	!H2C7N
  integer,parameter::KROME_idx_H2C9N = 184	!H2C9N
  integer,parameter::KROME_idx_NH2OH = 185	!NH2OH
  integer,parameter::KROME_idx_CH2OH = 186	!CH2OH
  integer,parameter::KROME_idx_C5H3 = 187	!C5H3
  integer,parameter::KROME_idx_H3C5N = 188	!H3C5N
  integer,parameter::KROME_idx_C6H3 = 189	!C6H3
  integer,parameter::KROME_idx_C7H3 = 190	!C7H3
  integer,parameter::KROME_idx_H3C7N = 191	!H3C7N
  integer,parameter::KROME_idx_C8H3 = 192	!C8H3
  integer,parameter::KROME_idx_C9H3 = 193	!C9H3
  integer,parameter::KROME_idx_H3C9N = 194	!H3C9N
  integer,parameter::KROME_idx_CH3NH = 195	!CH3NH
  integer,parameter::KROME_idx_H4C3N = 196	!H4C3N
  integer,parameter::KROME_idx_C5H4 = 197	!C5H4
  integer,parameter::KROME_idx_C6H4 = 198	!C6H4
  integer,parameter::KROME_idx_C7H4 = 199	!C7H4
  integer,parameter::KROME_idx_C8H4 = 200	!C8H4
  integer,parameter::KROME_idx_C9H4 = 201	!C9H4
  integer,parameter::KROME_idx_H5C3N = 202	!H5C3N
  integer,parameter::KROME_idx_C2H6 = 203	!C2H6
  integer,parameter::KROME_idx_C_DUST = 204	!C_DUST
  integer,parameter::KROME_idx_C2_DUST = 205	!C2_DUST
  integer,parameter::KROME_idx_C3_DUST = 206	!C3_DUST
  integer,parameter::KROME_idx_C2H_DUST = 207	!C2H_DUST
  integer,parameter::KROME_idx_C3H_DUST = 208	!C3H_DUST
  integer,parameter::KROME_idx_C2H3_DUST = 209	!C2H3_DUST
  integer,parameter::KROME_idx_C3H3_DUST = 210	!C3H3_DUST
  integer,parameter::KROME_idx_C2N_DUST = 211	!C2N_DUST
  integer,parameter::KROME_idx_C3N_DUST = 212	!C3N_DUST
  integer,parameter::KROME_idx_CCO_DUST = 213	!CCO_DUST
  integer,parameter::KROME_idx_C3O_DUST = 214	!C3O_DUST
  integer,parameter::KROME_idx_C2S_DUST = 215	!C2S_DUST
  integer,parameter::KROME_idx_C3S_DUST = 216	!C3S_DUST
  integer,parameter::KROME_idx_C4_DUST = 217	!C4_DUST
  integer,parameter::KROME_idx_C4H_DUST = 218	!C4H_DUST
  integer,parameter::KROME_idx_C5_DUST = 219	!C5_DUST
  integer,parameter::KROME_idx_C5H_DUST = 220	!C5H_DUST
  integer,parameter::KROME_idx_C6_DUST = 221	!C6_DUST
  integer,parameter::KROME_idx_C6H_DUST = 222	!C6H_DUST
  integer,parameter::KROME_idx_C7_DUST = 223	!C7_DUST
  integer,parameter::KROME_idx_C7H_DUST = 224	!C7H_DUST
  integer,parameter::KROME_idx_C8_DUST = 225	!C8_DUST
  integer,parameter::KROME_idx_C8H_DUST = 226	!C8H_DUST
  integer,parameter::KROME_idx_C9_DUST = 227	!C9_DUST
  integer,parameter::KROME_idx_C9H_DUST = 228	!C9H_DUST
  integer,parameter::KROME_idx_C10_DUST = 229	!C10_DUST
  integer,parameter::KROME_idx_CH_DUST = 230	!CH_DUST
  integer,parameter::KROME_idx_CH2_DUST = 231	!CH2_DUST
  integer,parameter::KROME_idx_C2H2_DUST = 232	!C2H2_DUST
  integer,parameter::KROME_idx_CH3_DUST = 233	!CH3_DUST
  integer,parameter::KROME_idx_CN_DUST = 234	!CN_DUST
  integer,parameter::KROME_idx_HS_DUST = 235	!HS_DUST
  integer,parameter::KROME_idx_CS_DUST = 236	!CS_DUST
  integer,parameter::KROME_idx_H_DUST = 237	!H_DUST
  integer,parameter::KROME_idx_N_DUST = 238	!N_DUST
  integer,parameter::KROME_idx_NH_DUST = 239	!NH_DUST
  integer,parameter::KROME_idx_HNC_DUST = 240	!HNC_DUST
  integer,parameter::KROME_idx_NH2_DUST = 241	!NH2_DUST
  integer,parameter::KROME_idx_NO_DUST = 242	!NO_DUST
  integer,parameter::KROME_idx_O_DUST = 243	!O_DUST
  integer,parameter::KROME_idx_OCN_DUST = 244	!OCN_DUST
  integer,parameter::KROME_idx_NS_DUST = 245	!NS_DUST
  integer,parameter::KROME_idx_S_DUST = 246	!S_DUST
  integer,parameter::KROME_idx_CO_DUST = 247	!CO_DUST
  integer,parameter::KROME_idx_O2_DUST = 248	!O2_DUST
  integer,parameter::KROME_idx_OH_DUST = 249	!OH_DUST
  integer,parameter::KROME_idx_SO_DUST = 250	!SO_DUST
  integer,parameter::KROME_idx_C3H2_DUST = 251	!C3H2_DUST
  integer,parameter::KROME_idx_C3H4_DUST = 252	!C3H4_DUST
  integer,parameter::KROME_idx_C4H2_DUST = 253	!C4H2_DUST
  integer,parameter::KROME_idx_C5H2_DUST = 254	!C5H2_DUST
  integer,parameter::KROME_idx_C6H2_DUST = 255	!C6H2_DUST
  integer,parameter::KROME_idx_C7H2_DUST = 256	!C7H2_DUST
  integer,parameter::KROME_idx_C8H2_DUST = 257	!C8H2_DUST
  integer,parameter::KROME_idx_C9H2_DUST = 258	!C9H2_DUST
  integer,parameter::KROME_idx_C2H4_DUST = 259	!C2H4_DUST
  integer,parameter::KROME_idx_HCCN_DUST = 260	!HCCN_DUST
  integer,parameter::KROME_idx_HNO_DUST = 261	!HNO_DUST
  integer,parameter::KROME_idx_HCN_DUST = 262	!HCN_DUST
  integer,parameter::KROME_idx_CHNH_DUST = 263	!CHNH_DUST
  integer,parameter::KROME_idx_CH3N_DUST = 264	!CH3N_DUST
  integer,parameter::KROME_idx_HCO_DUST = 265	!HCO_DUST
  integer,parameter::KROME_idx_C2H5_DUST = 266	!C2H5_DUST
  integer,parameter::KROME_idx_C2H2N_DUST = 267	!C2H2N_DUST
  integer,parameter::KROME_idx_CH2NH2_DUST = 268	!CH2NH2_DUST
  integer,parameter::KROME_idx_H2CO_DUST = 269	!H2CO_DUST
  integer,parameter::KROME_idx_CH3C3N_DUST = 270	!CH3C3N_DUST
  integer,parameter::KROME_idx_C5N_DUST = 271	!C5N_DUST
  integer,parameter::KROME_idx_CH3C5N_DUST = 272	!CH3C5N_DUST
  integer,parameter::KROME_idx_C7N_DUST = 273	!C7N_DUST
  integer,parameter::KROME_idx_CH3C7N_DUST = 274	!CH3C7N_DUST
  integer,parameter::KROME_idx_CH2OH_DUST = 275	!CH2OH_DUST
  integer,parameter::KROME_idx_C2H5OH_DUST = 276	!C2H5OH_DUST
  integer,parameter::KROME_idx_CH3OCH3_DUST = 277	!CH3OCH3_DUST
  integer,parameter::KROME_idx_C2H6_DUST = 278	!C2H6_DUST
  integer,parameter::KROME_idx_C2H3N_DUST = 279	!C2H3N_DUST
  integer,parameter::KROME_idx_C2H4O_DUST = 280	!C2H4O_DUST
  integer,parameter::KROME_idx_CH4_DUST = 281	!CH4_DUST
  integer,parameter::KROME_idx_H2_DUST = 282	!H2_DUST
  integer,parameter::KROME_idx_HC2O_DUST = 283	!HC2O_DUST
  integer,parameter::KROME_idx_C3H3N_DUST = 284	!C3H3N_DUST
  integer,parameter::KROME_idx_H4C3N_DUST = 285	!H4C3N_DUST
  integer,parameter::KROME_idx_HC3N_DUST = 286	!HC3N_DUST
  integer,parameter::KROME_idx_HC3O_DUST = 287	!HC3O_DUST
  integer,parameter::KROME_idx_C4H3_DUST = 288	!C4H3_DUST
  integer,parameter::KROME_idx_C4H4_DUST = 289	!C4H4_DUST
  integer,parameter::KROME_idx_C5H3_DUST = 290	!C5H3_DUST
  integer,parameter::KROME_idx_C5H4_DUST = 291	!C5H4_DUST
  integer,parameter::KROME_idx_HC5N_DUST = 292	!HC5N_DUST
  integer,parameter::KROME_idx_C6H3_DUST = 293	!C6H3_DUST
  integer,parameter::KROME_idx_C6H4_DUST = 294	!C6H4_DUST
  integer,parameter::KROME_idx_C7H3_DUST = 295	!C7H3_DUST
  integer,parameter::KROME_idx_C7H4_DUST = 296	!C7H4_DUST
  integer,parameter::KROME_idx_HC7N_DUST = 297	!HC7N_DUST
  integer,parameter::KROME_idx_C8H3_DUST = 298	!C8H3_DUST
  integer,parameter::KROME_idx_C8H4_DUST = 299	!C8H4_DUST
  integer,parameter::KROME_idx_C9H3_DUST = 300	!C9H3_DUST
  integer,parameter::KROME_idx_C9H4_DUST = 301	!C9H4_DUST
  integer,parameter::KROME_idx_C9N_DUST = 302	!C9N_DUST
  integer,parameter::KROME_idx_HC9N_DUST = 303	!HC9N_DUST
  integer,parameter::KROME_idx_CH3NH_DUST = 304	!CH3NH_DUST
  integer,parameter::KROME_idx_CH5N_DUST = 305	!CH5N_DUST
  integer,parameter::KROME_idx_CH4O_DUST = 306	!CH4O_DUST
  integer,parameter::KROME_idx_HCS_DUST = 307	!HCS_DUST
  integer,parameter::KROME_idx_FE_DUST = 308	!FE_DUST
  integer,parameter::KROME_idx_FEH_DUST = 309	!FEH_DUST
  integer,parameter::KROME_idx_H2C3N_DUST = 310	!H2C3N_DUST
  integer,parameter::KROME_idx_H2C5N_DUST = 311	!H2C5N_DUST
  integer,parameter::KROME_idx_H3C5N_DUST = 312	!H3C5N_DUST
  integer,parameter::KROME_idx_H2C7N_DUST = 313	!H2C7N_DUST
  integer,parameter::KROME_idx_H3C7N_DUST = 314	!H3C7N_DUST
  integer,parameter::KROME_idx_H2C9N_DUST = 315	!H2C9N_DUST
  integer,parameter::KROME_idx_H3C9N_DUST = 316	!H3C9N_DUST
  integer,parameter::KROME_idx_H2CN_DUST = 317	!H2CN_DUST
  integer,parameter::KROME_idx_H2O2_DUST = 318	!H2O2_DUST
  integer,parameter::KROME_idx_H2O_DUST = 319	!H2O_DUST
  integer,parameter::KROME_idx_O2H_DUST = 320	!O2H_DUST
  integer,parameter::KROME_idx_H2S_DUST = 321	!H2S_DUST
  integer,parameter::KROME_idx_H5C3N_DUST = 322	!H5C3N_DUST
  integer,parameter::KROME_idx_C2H2O_DUST = 323	!C2H2O_DUST
  integer,parameter::KROME_idx_H2C3O_DUST = 324	!H2C3O_DUST
  integer,parameter::KROME_idx_H2CS_DUST = 325	!H2CS_DUST
  integer,parameter::KROME_idx_MG_DUST = 326	!MG_DUST
  integer,parameter::KROME_idx_MGH_DUST = 327	!MGH_DUST
  integer,parameter::KROME_idx_MGH2_DUST = 328	!MGH2_DUST
  integer,parameter::KROME_idx_N2H2_DUST = 329	!N2H2_DUST
  integer,parameter::KROME_idx_N2_DUST = 330	!N2_DUST
  integer,parameter::KROME_idx_NA_DUST = 331	!NA_DUST
  integer,parameter::KROME_idx_NAH_DUST = 332	!NAH_DUST
  integer,parameter::KROME_idx_NH3_DUST = 333	!NH3_DUST
  integer,parameter::KROME_idx_O3_DUST = 334	!O3_DUST
  integer,parameter::KROME_idx_HNCO_DUST = 335	!HNCO_DUST
  integer,parameter::KROME_idx_OCS_DUST = 336	!OCS_DUST
  integer,parameter::KROME_idx_SI_DUST = 337	!SI_DUST
  integer,parameter::KROME_idx_SIH_DUST = 338	!SIH_DUST
  integer,parameter::KROME_idx_SIH2_DUST = 339	!SIH2_DUST
  integer,parameter::KROME_idx_SIH3_DUST = 340	!SIH3_DUST
  integer,parameter::KROME_idx_SIH4_DUST = 341	!SIH4_DUST
  integer,parameter::KROME_idx_SO2_DUST = 342	!SO2_DUST
  integer,parameter::KROME_idx_HCOOCH3_DUST = 343	!HCOOCH3_DUST
  integer,parameter::KROME_idx_NH2CHO_DUST = 344	!NH2CHO_DUST
  integer,parameter::KROME_idx_CO2_DUST = 345	!CO2_DUST
  integer,parameter::KROME_idx_CH2O2_DUST = 346	!CH2O2_DUST
  integer,parameter::KROME_idx_NH2OH_DUST = 347	!NH2OH_DUST
  integer,parameter::KROME_idx_C4N_DUST = 348	!C4N_DUST
  integer,parameter::KROME_idx_C4S_DUST = 349	!C4S_DUST
  integer,parameter::KROME_idx_C6H6_DUST = 350	!C6H6_DUST
  integer,parameter::KROME_idx_CH2NH2 = 351	!CH2NH2
  integer,parameter::KROME_idx_CH3C4H_DUST = 352	!CH3C4H_DUST
  integer,parameter::KROME_idx_CH3C6H_DUST = 353	!CH3C6H_DUST
  integer,parameter::KROME_idx_H2S2_DUST = 354	!H2S2_DUST
  integer,parameter::KROME_idx_HC2NC_DUST = 355	!HC2NC_DUST
  integer,parameter::KROME_idx_HCNC2_DUST = 356	!HCNC2_DUST
  integer,parameter::KROME_idx_HE_DUST = 357	!HE_DUST
  integer,parameter::KROME_idx_HNC3_DUST = 358	!HNC3_DUST
  integer,parameter::KROME_idx_HS2_DUST = 359	!HS2_DUST
  integer,parameter::KROME_idx_NAOH_DUST = 360	!NAOH_DUST
  integer,parameter::KROME_idx_S2_DUST = 361	!S2_DUST
  integer,parameter::KROME_idx_SIC_DUST = 362	!SIC_DUST
  integer,parameter::KROME_idx_SIO_DUST = 363	!SIO_DUST
  integer,parameter::KROME_idx_SIS_DUST = 364	!SIS_DUST
  integer,parameter::KROME_idx_C2H6CO_DUST = 365	!C2H6CO_DUST
  integer,parameter::KROME_idx_C3P_DUST = 366	!C3P_DUST
  integer,parameter::KROME_idx_CCP_DUST = 367	!CCP_DUST
  integer,parameter::KROME_idx_C4P_DUST = 368	!C4P_DUST
  integer,parameter::KROME_idx_CCL_DUST = 369	!CCL_DUST
  integer,parameter::KROME_idx_CL_DUST = 370	!CL_DUST
  integer,parameter::KROME_idx_P_DUST = 371	!P_DUST
  integer,parameter::KROME_idx_CP_DUST = 372	!CP_DUST
  integer,parameter::KROME_idx_CH2PH_DUST = 373	!CH2PH_DUST
  integer,parameter::KROME_idx_HCP_DUST = 374	!HCP_DUST
  integer,parameter::KROME_idx_CLO_DUST = 375	!CLO_DUST
  integer,parameter::KROME_idx_H2SIO_DUST = 376	!H2SIO_DUST
  integer,parameter::KROME_idx_HCCP_DUST = 377	!HCCP_DUST
  integer,parameter::KROME_idx_HCL_DUST = 378	!HCL_DUST
  integer,parameter::KROME_idx_HCSI_DUST = 379	!HCSI_DUST
  integer,parameter::KROME_idx_HNSI_DUST = 380	!HNSI_DUST
  integer,parameter::KROME_idx_SIN_DUST = 381	!SIN_DUST
  integer,parameter::KROME_idx_HPO_DUST = 382	!HPO_DUST
  integer,parameter::KROME_idx_PO_DUST = 383	!PO_DUST
  integer,parameter::KROME_idx_N2O_DUST = 384	!N2O_DUST
  integer,parameter::KROME_idx_NH2CN_DUST = 385	!NH2CN_DUST
  integer,parameter::KROME_idx_NO2_DUST = 386	!NO2_DUST
  integer,parameter::KROME_idx_PH_DUST = 387	!PH_DUST
  integer,parameter::KROME_idx_PH2_DUST = 388	!PH2_DUST
  integer,parameter::KROME_idx_PN_DUST = 389	!PN_DUST
  integer,parameter::KROME_idx_SIC2_DUST = 390	!SIC2_DUST
  integer,parameter::KROME_idx_SIC2H_DUST = 391	!SIC2H_DUST
  integer,parameter::KROME_idx_SIC2H2_DUST = 392	!SIC2H2_DUST
  integer,parameter::KROME_idx_SIC3_DUST = 393	!SIC3_DUST
  integer,parameter::KROME_idx_SIC3H_DUST = 394	!SIC3H_DUST
  integer,parameter::KROME_idx_SIC4_DUST = 395	!SIC4_DUST
  integer,parameter::KROME_idx_SICH2_DUST = 396	!SICH2_DUST
  integer,parameter::KROME_idx_SICH3_DUST = 397	!SICH3_DUST
  integer,parameter::KROME_idx_SINC_DUST = 398	!SINC_DUST
  integer,parameter::KROME_idx_SIO2_DUST = 399	!SIO2_DUST
  integer,parameter::KROME_idx_Cj = 400	!C+
  integer,parameter::KROME_idx_CLj = 401	!CL+
  integer,parameter::KROME_idx_FEj = 402	!FE+
  integer,parameter::KROME_idx_Hj = 403	!H+
  integer,parameter::KROME_idx_HEj = 404	!HE+
  integer,parameter::KROME_idx_MGj = 405	!MG+
  integer,parameter::KROME_idx_Nj = 406	!N+
  integer,parameter::KROME_idx_NAj = 407	!NA+
  integer,parameter::KROME_idx_Oj = 408	!O+
  integer,parameter::KROME_idx_Pj = 409	!P+
  integer,parameter::KROME_idx_Sj = 410	!S+
  integer,parameter::KROME_idx_SIj = 411	!SI+
  integer,parameter::KROME_idx_COj = 412	!CO+
  integer,parameter::KROME_idx_H2j = 413	!H2+
  integer,parameter::KROME_idx_NOj = 414	!NO+
  integer,parameter::KROME_idx_O2j = 415	!O2+
  integer,parameter::KROME_idx_CH2j = 416	!CH2+
  integer,parameter::KROME_idx_H2Sj = 417	!H2S+
  integer,parameter::KROME_idx_HCOj = 418	!HCO+
  integer,parameter::KROME_idx_HCSj = 419	!HCS+
  integer,parameter::KROME_idx_HNOj = 420	!HNO+
  integer,parameter::KROME_idx_NH2j = 421	!NH2+
  integer,parameter::KROME_idx_OCSj = 422	!OCS+
  integer,parameter::KROME_idx_C2H2j = 423	!C2H2+
  integer,parameter::KROME_idx_CH3j = 424	!CH3+
  integer,parameter::KROME_idx_NH3j = 425	!NH3+
  integer,parameter::KROME_idx_C2H2Oj = 426	!C2H2O+
  integer,parameter::KROME_idx_CH2O2j = 427	!CH2O2+
  integer,parameter::KROME_idx_C2H3Nj = 428	!C2H3N+
  integer,parameter::KROME_idx_C2H4j = 429	!C2H4+
  integer,parameter::KROME_idx_C4H2j = 430	!C4H2+
  integer,parameter::KROME_idx_H3COj = 431	!H3CO+
  integer,parameter::KROME_idx_CH4Oj = 432	!CH4O+
  integer,parameter::KROME_idx_C2H4Oj = 433	!C2H4O+
  integer,parameter::KROME_idx_C3H4j = 434	!C3H4+
  integer,parameter::KROME_idx_CH5Nj = 435	!CH5N+
  integer,parameter::KROME_idx_C2H5OHj = 436	!C2H5OH+
  integer,parameter::KROME_idx_CH3OCH3j = 437	!CH3OCH3+
  integer,parameter::KROME_idx_CHj = 438	!CH+
  integer,parameter::KROME_idx_CCLj = 439	!CCL+
  integer,parameter::KROME_idx_C2j = 440	!C2+
  integer,parameter::KROME_idx_CLOj = 441	!CLO+
  integer,parameter::KROME_idx_CPj = 442	!CP+
  integer,parameter::KROME_idx_CSj = 443	!CS+
  integer,parameter::KROME_idx_CNj = 444	!CN+
  integer,parameter::KROME_idx_NSj = 445	!NS+
  integer,parameter::KROME_idx_PHj = 446	!PH+
  integer,parameter::KROME_idx_POj = 447	!PO+
  integer,parameter::KROME_idx_SICj = 448	!SIC+
  integer,parameter::KROME_idx_SINj = 449	!SIN+
  integer,parameter::KROME_idx_SISj = 450	!SIS+
  integer,parameter::KROME_idx_SOj = 451	!SO+
  integer,parameter::KROME_idx_C3j = 452	!C3+
  integer,parameter::KROME_idx_C2Sj = 453	!C2S+
  integer,parameter::KROME_idx_C2Oj = 454	!C2O+
  integer,parameter::KROME_idx_CCPj = 455	!CCP+
  integer,parameter::KROME_idx_C2Hj = 456	!C2H+
  integer,parameter::KROME_idx_HOCj = 457	!HOC+
  integer,parameter::KROME_idx_C2Nj = 458	!C2N+
  integer,parameter::KROME_idx_CNCj = 459	!CNC+
  integer,parameter::KROME_idx_HCPj = 460	!HCP+
  integer,parameter::KROME_idx_SIC2j = 461	!SIC2+
  integer,parameter::KROME_idx_SINCj = 462	!SINC+
  integer,parameter::KROME_idx_HPOj = 463	!HPO+
  integer,parameter::KROME_idx_HCNj = 464	!HCN+
  integer,parameter::KROME_idx_CHSIj = 465	!CHSI+
  integer,parameter::KROME_idx_SIH2j = 466	!SIH2+
  integer,parameter::KROME_idx_C3Hj = 467	!C3H+
  integer,parameter::KROME_idx_C4j = 468	!C4+
  integer,parameter::KROME_idx_C3Oj = 469	!C3O+
  integer,parameter::KROME_idx_C3Sj = 470	!C3S+
  integer,parameter::KROME_idx_H2COj = 471	!H2CO+
  integer,parameter::KROME_idx_H2SIOj = 472	!H2SIO+
  integer,parameter::KROME_idx_HCNHj = 473	!HCNH+
  integer,parameter::KROME_idx_SIC2Hj = 474	!SIC2H+
  integer,parameter::KROME_idx_SIC3j = 475	!SIC3+
  integer,parameter::KROME_idx_CH2SIj = 476	!CH2SI+
  integer,parameter::KROME_idx_SIH3j = 477	!SIH3+
  integer,parameter::KROME_idx_C2H2Nj = 478	!C2H2N+
  integer,parameter::KROME_idx_C2H3j = 479	!C2H3+
  integer,parameter::KROME_idx_C3H2j = 480	!C3H2+
  integer,parameter::KROME_idx_H2C3j = 481	!H2C3+
  integer,parameter::KROME_idx_C4Hj = 482	!C4H+
  integer,parameter::KROME_idx_C5j = 483	!C5+
  integer,parameter::KROME_idx_C4Sj = 484	!C4S+
  integer,parameter::KROME_idx_PC2Hj = 485	!PC2H+
  integer,parameter::KROME_idx_C3Nj = 486	!C3N+
  integer,parameter::KROME_idx_C4Nj = 487	!C4N+
  integer,parameter::KROME_idx_C3HNj = 488	!C3HN+
  integer,parameter::KROME_idx_HNCj = 489	!HNC+
  integer,parameter::KROME_idx_SIC3Hj = 490	!SIC3H+
  integer,parameter::KROME_idx_SIC4j = 491	!SIC4+
  integer,parameter::KROME_idx_SIC2H2j = 492	!SIC2H2+
  integer,parameter::KROME_idx_SICH3j = 493	!SICH3+
  integer,parameter::KROME_idx_HC2NCHj = 494	!HC2NCH+
  integer,parameter::KROME_idx_C3H3j = 495	!C3H3+
  integer,parameter::KROME_idx_H3C3j = 496	!H3C3+
  integer,parameter::KROME_idx_C5Hj = 497	!C5H+
  integer,parameter::KROME_idx_C6j = 498	!C6+
  integer,parameter::KROME_idx_C2H3Oj = 499	!C2H3O+
  integer,parameter::KROME_idx_C2H5j = 500	!C2H5+
  integer,parameter::KROME_idx_C3H3Nj = 501	!C3H3N+
  integer,parameter::KROME_idx_C5H2j = 502	!C5H2+
  integer,parameter::KROME_idx_C4H3j = 503	!C4H3+
  integer,parameter::KROME_idx_C6Hj = 504	!C6H+
  integer,parameter::KROME_idx_C7j = 505	!C7+
  integer,parameter::KROME_idx_CH4Nj = 506	!CH4N+
  integer,parameter::KROME_idx_C5HNj = 507	!C5HN+
  integer,parameter::KROME_idx_C7Hj = 508	!C7H+
  integer,parameter::KROME_idx_C8j = 509	!C8+
  integer,parameter::KROME_idx_COOCH4j = 510	!COOCH4+
  integer,parameter::KROME_idx_C2H5Oj = 511	!C2H5O+
  integer,parameter::KROME_idx_C8Hj = 512	!C8H+
  integer,parameter::KROME_idx_C9j = 513	!C9+
  integer,parameter::KROME_idx_C5H3j = 514	!C5H3+
  integer,parameter::KROME_idx_C6H2j = 515	!C6H2+
  integer,parameter::KROME_idx_C6H3j = 516	!C6H3+
  integer,parameter::KROME_idx_C2H6COj = 517	!C2H6CO+
  integer,parameter::KROME_idx_C9Hj = 518	!C9H+
  integer,parameter::KROME_idx_C10j = 519	!C10+
  integer,parameter::KROME_idx_C7H3j = 520	!C7H3+
  integer,parameter::KROME_idx_C8H2j = 521	!C8H2+
  integer,parameter::KROME_idx_C8H3j = 522	!C8H3+
  integer,parameter::KROME_idx_HCLj = 523	!HCL+
  integer,parameter::KROME_idx_HSj = 524	!HS+
  integer,parameter::KROME_idx_NHj = 525	!NH+
  integer,parameter::KROME_idx_OHj = 526	!OH+
  integer,parameter::KROME_idx_PNj = 527	!PN+
  integer,parameter::KROME_idx_S2j = 528	!S2+
  integer,parameter::KROME_idx_SIHj = 529	!SIH+
  integer,parameter::KROME_idx_SIOj = 530	!SIO+
  integer,parameter::KROME_idx_H2Oj = 531	!H2O+
  integer,parameter::KROME_idx_HNSIj = 532	!HNSI+
  integer,parameter::KROME_idx_S2Hj = 533	!S2H+
  integer,parameter::KROME_idx_PH2j = 534	!PH2+
  integer,parameter::KROME_idx_H2CSj = 535	!H2CS+
  integer,parameter::KROME_idx_H2S2j = 536	!H2S2+
  integer,parameter::KROME_idx_HSIOj = 537	!HSIO+
  integer,parameter::KROME_idx_C4Pj = 538	!C4P+
  integer,parameter::KROME_idx_HCO2j = 539	!HCO2+
  integer,parameter::KROME_idx_PCH3j = 540	!PCH3+
  integer,parameter::KROME_idx_CH4j = 541	!CH4+
  integer,parameter::KROME_idx_C2NHj = 542	!C2NH+
  integer,parameter::KROME_idx_SIH4j = 543	!SIH4+
  integer,parameter::KROME_idx_NH4j = 544	!NH4+
  integer,parameter::KROME_idx_H2NCj = 545	!H2NC+
  integer,parameter::KROME_idx_C3H2Nj = 546	!C3H2N+
  integer,parameter::KROME_idx_C7H2j = 547	!C7H2+
  integer,parameter::KROME_idx_C5H4j = 548	!C5H4+
  integer,parameter::KROME_idx_C7HNj = 549	!C7HN+
  integer,parameter::KROME_idx_C9H2j = 550	!C9H2+
  integer,parameter::KROME_idx_C7H4j = 551	!C7H4+
  integer,parameter::KROME_idx_C9HNj = 552	!C9HN+
  integer,parameter::KROME_idx_N2j = 553	!N2+
  integer,parameter::KROME_idx_CO2j = 554	!CO2+
  integer,parameter::KROME_idx_HEHj = 555	!HEH+
  integer,parameter::KROME_idx_SO2j = 556	!SO2+
  integer,parameter::KROME_idx_C6H5j = 557	!C6H5+
  integer,parameter::KROME_idx_C5H5j = 558	!C5H5+
  integer,parameter::KROME_idx_N2Hj = 559	!N2H+
  integer,parameter::KROME_idx_NO2j = 560	!NO2+
  integer,parameter::KROME_idx_PC2H2j = 561	!PC2H2+
  integer,parameter::KROME_idx_PNH2j = 562	!PNH2+
  integer,parameter::KROME_idx_PCH2j = 563	!PCH2+
  integer,parameter::KROME_idx_HC2Sj = 564	!HC2S+
  integer,parameter::KROME_idx_HC3Sj = 565	!HC3S+
  integer,parameter::KROME_idx_H3CSj = 566	!H3CS+
  integer,parameter::KROME_idx_HC4Sj = 567	!HC4S+
  integer,parameter::KROME_idx_SINH2j = 568	!SINH2+
  integer,parameter::KROME_idx_SIC2H3j = 569	!SIC2H3+
  integer,parameter::KROME_idx_SIC3H2j = 570	!SIC3H2+
  integer,parameter::KROME_idx_C2HOj = 571	!C2HO+
  integer,parameter::KROME_idx_H3Oj = 572	!H3O+
  integer,parameter::KROME_idx_H3Sj = 573	!H3S+
  integer,parameter::KROME_idx_HOCSj = 574	!HOCS+
  integer,parameter::KROME_idx_CH5Oj = 575	!CH5O+
  integer,parameter::KROME_idx_NCOj = 576	!NCO+
  integer,parameter::KROME_idx_HNCOj = 577	!HNCO+
  integer,parameter::KROME_idx_C2N2j = 578	!C2N2+
  integer,parameter::KROME_idx_H3j = 579	!H3+
  integer,parameter::KROME_idx_O2Hj = 580	!O2H+
  integer,parameter::KROME_idx_CH5j = 581	!CH5+
  integer,parameter::KROME_idx_H2CLj = 582	!H2CL+
  integer,parameter::KROME_idx_CH3O2j = 583	!CH3O2+
  integer,parameter::KROME_idx_H2POj = 584	!H2PO+
  integer,parameter::KROME_idx_PNH3j = 585	!PNH3+
  integer,parameter::KROME_idx_PCH4j = 586	!PCH4+
  integer,parameter::KROME_idx_PC2H3j = 587	!PC2H3+
  integer,parameter::KROME_idx_HSISj = 588	!HSIS+
  integer,parameter::KROME_idx_HSOj = 589	!HSO+
  integer,parameter::KROME_idx_HNSj = 590	!HNS+
  integer,parameter::KROME_idx_HPNj = 591	!HPN+
  integer,parameter::KROME_idx_H2NOj = 592	!H2NO+
  integer,parameter::KROME_idx_NAH2Oj = 593	!NAH2O+
  integer,parameter::KROME_idx_PH3j = 594	!PH3+
  integer,parameter::KROME_idx_SINCHj = 595	!SINCH+
  integer,parameter::KROME_idx_HSIO2j = 596	!HSIO2+
  integer,parameter::KROME_idx_HSO2j = 597	!HSO2+
  integer,parameter::KROME_idx_HC3Oj = 598	!HC3O+
  integer,parameter::KROME_idx_PC3Hj = 599	!PC3H+
  integer,parameter::KROME_idx_H3S2j = 600	!H3S2+
  integer,parameter::KROME_idx_H3SIOj = 601	!H3SIO+
  integer,parameter::KROME_idx_PC4Hj = 602	!PC4H+
  integer,parameter::KROME_idx_NH2CNHj = 603	!NH2CNH+
  integer,parameter::KROME_idx_SIC4Hj = 604	!SIC4H+
  integer,parameter::KROME_idx_SICH4j = 605	!SICH4+
  integer,parameter::KROME_idx_SIH5j = 606	!SIH5+
  integer,parameter::KROME_idx_C2H4Nj = 607	!C2H4N+
  integer,parameter::KROME_idx_NH2CH2Oj = 608	!NH2CH2O+
  integer,parameter::KROME_idx_C2H6j = 609	!C2H6+
  integer,parameter::KROME_idx_C3H4Nj = 610	!C3H4N+
  integer,parameter::KROME_idx_C3H5j = 611	!C3H5+
  integer,parameter::KROME_idx_C4H4j = 612	!C4H4+
  integer,parameter::KROME_idx_CH6Nj = 613	!CH6N+
  integer,parameter::KROME_idx_C5H2Nj = 614	!C5H2N+
  integer,parameter::KROME_idx_C4H4Nj = 615	!C4H4N+
  integer,parameter::KROME_idx_H5C2O2j = 616	!H5C2O2+
  integer,parameter::KROME_idx_C2H5OH2j = 617	!C2H5OH2+
  integer,parameter::KROME_idx_CH3OCH4j = 618	!CH3OCH4+
  integer,parameter::KROME_idx_C7H2Nj = 619	!C7H2N+
  integer,parameter::KROME_idx_C3H6OHj = 620	!C3H6OH+
  integer,parameter::KROME_idx_C6H4Nj = 621	!C6H4N+
  integer,parameter::KROME_idx_C10Hj = 622	!C10H+
  integer,parameter::KROME_idx_C9H3j = 623	!C9H3+
  integer,parameter::KROME_idx_C7H5j = 624	!C7H5+
  integer,parameter::KROME_idx_C8H4Nj = 625	!C8H4N+
  integer,parameter::KROME_idx_C9H2Nj = 626	!C9H2N+
  integer,parameter::KROME_idx_C6H7j = 627	!C6H7+
  integer,parameter::KROME_idx_NAH2j = 628	!NAH2+
  integer,parameter::KROME_idx_PC2H4j = 629	!PC2H4+
  integer,parameter::KROME_idx_C4H5j = 630	!C4H5+
  integer,parameter::KROME_idx_H2CCLj = 631	!H2CCL+
  integer,parameter::KROME_idx_PC4H2j = 632	!PC4H2+
  integer,parameter::KROME_idx_C6H4j = 633	!C6H4+
  integer,parameter::KROME_idx_C8H4j = 634	!C8H4+
  integer,parameter::KROME_idx_C9H4j = 635	!C9H4+
  integer,parameter::KROME_idx_C4H7j = 636	!C4H7+
  integer,parameter::KROME_idx_HC4Nj = 637	!HC4N+
  integer,parameter::KROME_idx_HC4Oj = 638	!HC4O+
  integer,parameter::KROME_idx_C5Nj = 639	!C5N+
  integer,parameter::KROME_idx_H2C4Nj = 640	!H2C4N+
  integer,parameter::KROME_idx_H3C4Nj = 641	!H3C4N+
  integer,parameter::KROME_idx_C7Nj = 642	!C7N+
  integer,parameter::KROME_idx_C5H3Nj = 643	!C5H3N+
  integer,parameter::KROME_idx_C10H2j = 644	!C10H2+
  integer,parameter::KROME_idx_C9Nj = 645	!C9N+
  integer,parameter::KROME_idx_C7H3Nj = 646	!C7H3N+
  integer,parameter::KROME_idx_C9H3Nj = 647	!C9H3N+
  integer,parameter::KROME_idx_OCSjH2 = 648	!OCS+H2
  integer,parameter::KROME_idx_H2C3Oj = 649	!H2C3O+
  integer,parameter::KROME_idx_H3C3Oj = 650	!H3C3O+
  integer,parameter::KROME_idx_C5H4Nj = 651	!C5H4N+
  integer,parameter::KROME_idx_C8H5j = 652	!C8H5+
  integer,parameter::KROME_idx_C9H5j = 653	!C9H5+
  integer,parameter::KROME_idx_H2COHOCH2j = 654	!H2COHOCH2+
  integer,parameter::KROME_idx_H7C2O2j = 655	!H7C2O2+
  integer,parameter::KROME_idx_CR = 656	!CR
  integer,parameter::KROME_idx_g = 657	!g
  integer,parameter::KROME_idx_Tgas = 658	!Tgas
  integer,parameter::KROME_idx_dummy = 659	!dummy

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

  integer,parameter::krome_nrea=5639
  integer,parameter::krome_nmols=655
  integer,parameter::krome_nspec=659
  integer,parameter::krome_natoms=15
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
  subroutine krome_set_user_Tdust(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_Tdust = argset
  end subroutine krome_set_user_Tdust

  !*******************
  function krome_get_user_Tdust()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_Tdust
    krome_get_user_Tdust = user_Tdust
  end function krome_get_user_Tdust

  !*******************
  subroutine krome_set_user_xdust(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_xdust = argset
  end subroutine krome_set_user_xdust

  !*******************
  function krome_get_user_xdust()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_xdust
    krome_get_user_xdust = user_xdust
  end function krome_get_user_xdust

  !*******************
  subroutine krome_set_user_gsize(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_gsize = argset
  end subroutine krome_set_user_gsize

  !*******************
  function krome_get_user_gsize()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_gsize
    krome_get_user_gsize = user_gsize
  end function krome_get_user_gsize

  !*******************
  subroutine krome_set_user_gsize2(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_gsize2 = argset
  end subroutine krome_set_user_gsize2

  !*******************
  function krome_get_user_gsize2()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_gsize2
    krome_get_user_gsize2 = user_gsize2
  end function krome_get_user_gsize2

  !*******************
  subroutine krome_set_user_crflux(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_crflux = argset
  end subroutine krome_set_user_crflux

  !*******************
  function krome_get_user_crflux()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_crflux
    krome_get_user_crflux = user_crflux
  end function krome_get_user_crflux

  !*******************
  subroutine krome_set_user_Av(argset)
    use krome_commons
    implicit none
    real*8 :: argset
    user_Av = argset
  end subroutine krome_set_user_Av

  !*******************
  function krome_get_user_Av()
    use krome_commons
    implicit none
    real*8 :: krome_get_user_Av
    krome_get_user_Av = user_Av
  end function krome_get_user_Av

  !************************
  !returns the Tdust averaged over the number density
  ! as computed in the tables
  function krome_get_table_Tdust(x,Tgas)
    use krome_commons
    use krome_grfuncs
    implicit none
    real*8 :: Tgas
    real*8 :: x(nmols), krome_get_table_Tdust
    real*8::n(nspec)

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas

    krome_get_table_Tdust = get_table_Tdust(n(:))

  end function krome_get_table_Tdust

  !**********************
  !convert from MOCASSIN abundances to KROME
  ! xmoc(i,j): MOCASSIN matrix (note: cm-3, real*4)
  !  i=species, j=ionization level
  ! imap: matrix position index map, integer
  ! returns KROME abundances (cm-3, real*8)
  function krome_convert_xmoc(xmoc,imap) result(x)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real*4,intent(in):: xmoc(:,:)
    real*8::x(nmols),n(nspec)
    integer,intent(in)::imap(:)

    x(:) = 0d0

    x(idx_C) = xmoc(imap(6), 1)
    x(idx_CL) = xmoc(imap(17), 1)
    x(idx_FE) = xmoc(imap(26), 1)
    x(idx_H) = xmoc(imap(1), 1)
    x(idx_HE) = xmoc(imap(2), 1)
    x(idx_MG) = xmoc(imap(12), 1)
    x(idx_N) = xmoc(imap(7), 1)
    x(idx_NA) = xmoc(imap(11), 1)
    x(idx_O) = xmoc(imap(8), 1)
    x(idx_P) = xmoc(imap(15), 1)
    x(idx_S) = xmoc(imap(16), 1)
    x(idx_SI) = xmoc(imap(14), 1)
    x(idx_Cj) = xmoc(imap(6), 2)
    x(idx_CLj) = xmoc(imap(17), 2)
    x(idx_FEj) = xmoc(imap(26), 2)
    x(idx_Hj) = xmoc(imap(1), 2)
    x(idx_HEj) = xmoc(imap(2), 2)
    x(idx_MGj) = xmoc(imap(12), 2)
    x(idx_Nj) = xmoc(imap(7), 2)
    x(idx_NAj) = xmoc(imap(11), 2)
    x(idx_Oj) = xmoc(imap(8), 2)
    x(idx_Pj) = xmoc(imap(15), 2)
    x(idx_Sj) = xmoc(imap(16), 2)
    x(idx_SIj) = xmoc(imap(14), 2)

    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0
    x(idx_e) = get_electrons(n(:))

  end function krome_convert_xmoc

  !*************************
  !convert from KROME abundances to MOCASSIN
  ! x: KROME abuances (cm-3, real*8)
  ! imap: matrix position index map, integer
  ! xmoc(i,j): MOCASSIN matrix (note: cm-3, real*4)
  !  i=species, j=ionization level
  subroutine krome_return_xmoc(x,imap,xmoc)
    use krome_commons
    implicit none
    real*8,intent(in)::x(nmols)
    real*4,intent(out)::xmoc(:,:)
    integer,intent(in)::imap(:)

    xmoc(:,:) = 0d0

    xmoc(imap(6), 1) = x(idx_C)
    xmoc(imap(17), 1) = x(idx_CL)
    xmoc(imap(26), 1) = x(idx_FE)
    xmoc(imap(1), 1) = x(idx_H)
    xmoc(imap(2), 1) = x(idx_HE)
    xmoc(imap(12), 1) = x(idx_MG)
    xmoc(imap(7), 1) = x(idx_N)
    xmoc(imap(11), 1) = x(idx_NA)
    xmoc(imap(8), 1) = x(idx_O)
    xmoc(imap(15), 1) = x(idx_P)
    xmoc(imap(16), 1) = x(idx_S)
    xmoc(imap(14), 1) = x(idx_SI)
    xmoc(imap(6), 2) = x(idx_Cj)
    xmoc(imap(17), 2) = x(idx_CLj)
    xmoc(imap(26), 2) = x(idx_FEj)
    xmoc(imap(1), 2) = x(idx_Hj)
    xmoc(imap(2), 2) = x(idx_HEj)
    xmoc(imap(12), 2) = x(idx_MGj)
    xmoc(imap(7), 2) = x(idx_Nj)
    xmoc(imap(11), 2) = x(idx_NAj)
    xmoc(imap(8), 2) = x(idx_Oj)
    xmoc(imap(15), 2) = x(idx_Pj)
    xmoc(imap(16), 2) = x(idx_Sj)
    xmoc(imap(14), 2) = x(idx_SIj)

  end subroutine krome_return_xmoc

  !**********************
  !convert number density (cm-3) into column
  ! density (cm-2) using the specific density
  ! column method (see help for option
  ! -columnDensityMethod)
  ! num is the number density, x(:) is the species
  ! array, Tgas is the gas temperature
  ! If the method is not JEANS, x(:) and Tgas
  ! are dummy variables
  function krome_num2col(num,x,Tgas)
    use krome_subs
    use krome_commons
    use krome_getphys
    implicit none
    real*8 :: x(nmols),krome_num2col
    real*8 :: Tgas,num
    real*8::n(nspec)

    n(:) = 0d0
    n(1:nmols) = x(:)
    n(idx_Tgas) = Tgas

    krome_num2col = num2col(num,n(:))

  end function krome_num2col

  !***********************
  !print on screen the current values of all phys variables
  subroutine krome_print_phys_variables()
    use krome_commons
    implicit none

    print *, "Tcmb:", phys_Tcmb
    print *, "zredshift:", phys_zredshift
    print *, "orthoParaRatio:", phys_orthoParaRatio
    print *, "metallicity:", phys_metallicity
    print *, "Tfloor:", phys_Tfloor

  end subroutine krome_print_phys_variables

  !*******************
  subroutine krome_set_Tcmb(arg)
    use krome_commons
    implicit none
    real*8 :: arg
    phys_Tcmb = arg
  end subroutine krome_set_Tcmb

  !*******************
  function krome_get_Tcmb()
    use krome_commons
    implicit none
    real*8 :: krome_get_Tcmb
    krome_get_Tcmb = phys_Tcmb
  end function krome_get_Tcmb

  !*******************
  subroutine krome_set_zredshift(arg)
    use krome_commons
    implicit none
    real*8 :: arg
    phys_zredshift = arg
  end subroutine krome_set_zredshift

  !*******************
  function krome_get_zredshift()
    use krome_commons
    implicit none
    real*8 :: krome_get_zredshift
    krome_get_zredshift = phys_zredshift
  end function krome_get_zredshift

  !*******************
  subroutine krome_set_orthoParaRatio(arg)
    use krome_commons
    implicit none
    real*8 :: arg
    phys_orthoParaRatio = arg
  end subroutine krome_set_orthoParaRatio

  !*******************
  function krome_get_orthoParaRatio()
    use krome_commons
    implicit none
    real*8 :: krome_get_orthoParaRatio
    krome_get_orthoParaRatio = phys_orthoParaRatio
  end function krome_get_orthoParaRatio

  !*******************
  subroutine krome_set_metallicity(arg)
    use krome_commons
    implicit none
    real*8 :: arg
    phys_metallicity = arg
  end subroutine krome_set_metallicity

  !*******************
  function krome_get_metallicity()
    use krome_commons
    implicit none
    real*8 :: krome_get_metallicity
    krome_get_metallicity = phys_metallicity
  end function krome_get_metallicity

  !*******************
  subroutine krome_set_Tfloor(arg)
    use krome_commons
    implicit none
    real*8 :: arg
    phys_Tfloor = arg
  end subroutine krome_set_Tfloor

  !*******************
  function krome_get_Tfloor()
    use krome_commons
    implicit none
    real*8 :: krome_get_Tfloor
    krome_get_Tfloor = phys_Tfloor
  end function krome_get_Tfloor

  ! #IFKROME_useSemenov
  !   !******************************
  !   !this function sets dust variables for Semenov's framework
  !   ! works. it might be incomplete...
  !   subroutine krome_get_dust_variables(ngas,dust_gas_ratio,rho0)
  !     use krome_commons
  !     use krome_subs
  !     use krome_constants
  !     use krome_getphys
  !     use krome_grfuncs
  !     implicit none
  !     real*8 :: rhogas,dmass,d2g
  !     real*8 :: rho0,dust_gas_ratio
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
  subroutine krome_store(x,Tgas,dt)
    use krome_commons
    implicit none
    integer::nfile,i
    real*8 :: x(nmols)
    real*8 :: Tgas,dt

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
  subroutine krome_restore(x,Tgas,dt)
    use krome_commons
    implicit none
    integer::nfile,i
    real*8 :: x(nmols)
    real*8 :: Tgas,dt

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
  subroutine krome_thermo_on()
    use krome_commons
    krome_thermo_toggle = 1
  end subroutine krome_thermo_on

  !****************************
  !switch off the thermal calculation
  subroutine krome_thermo_off()
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
  function krome_get_mu_x(xin)
    use krome_commons
    implicit none
    real*8 :: xin(nmols), krome_get_mu_x
    real*8::n(nmols)
    n(:) = krome_x2n(xin(:),1d0)
    krome_get_mu_x = krome_get_mu(n(:))
  end function krome_get_mu_x

  !****************************
  !return the adiabatic index from mass fractions
  ! and temperature in K
  function krome_get_gamma_x(xin,inTgas)
    use krome_commons
    implicit none
    real*8 :: inTgas
    real*8 :: xin(nmols), krome_get_gamma_x
    real*8::x(nmols),Tgas,rhogas

    Tgas = inTgas
    x(:) = krome_x2n(xin(:),1d0)
    krome_get_gamma_x = krome_get_gamma(x(:),Tgas)

  end function krome_get_gamma_x

  !***************************
  !normalize mass fractions and
  ! set charge to zero
  subroutine krome_consistent_x(x)
    use krome_commons
    use krome_constants
    implicit none
    real*8 :: x(nmols)
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
  subroutine krome_thermo(x,Tgas,dt)
    use krome_commons
    use krome_cooling
    use krome_heating
    use krome_subs
    use krome_tabs
    use krome_constants
    use krome_gadiab
    implicit none
    real*8 :: x(nmols), Tgas
    real*8 :: dt
    real*8::n(nspec),nH2dust,dTgas,k(nrea),krome_gamma

  end subroutine krome_thermo

  !*************************
  !get heating (erg/cm3/s) for a given species
  ! array x(:) and Tgas
  function krome_get_heating(x,inTgas)
    use krome_heating
    use krome_subs
    use krome_commons
    implicit none
    real*8 :: inTgas
    real*8 :: x(nmols), krome_get_heating
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
  subroutine krome_conserveLin_x(x,ref)
    use krome_commons
    use krome_subs
    implicit none
    real*8 :: x(nmols),ref(natoms)

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
  function krome_get_gamma(x,Tgas)
    use krome_subs
    use krome_commons
    use krome_gadiab
    real*8 :: Tgas
    real*8 :: x(nmols), krome_get_gamma
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
  function krome_get_mu(x)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real*8 :: x(nmols), krome_get_mu
    real*8::n(1:nspec)
    n(:) = 0d0
    n(1:nmols) = x(:)
    krome_get_mu = get_mu(n(:))
  end function krome_get_mu

  !***************************
  !get the names of the reactions as a
  ! character*50 array of krome_nrea
  ! elements
  !! !! cannot yet be called from C
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
  function krome_get_Hnuclei(x)
    use krome_commons
    use krome_subs
    use krome_getphys
    real*8::n(nspec)
    real*8 :: krome_get_Hnuclei, x(nmols)
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
  !!  !! cannot yet be called from C
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
    character*4259::krome_get_names_header
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
  !! !! cannot yet be called from C
  function krome_get_index(name)
    use krome_subs
    implicit none
    integer :: krome_get_index
    character*(*) :: name
    krome_get_index = get_index(name)
  end function krome_get_index

  !*******************
  !get the total density of the gas in g/cm3
  ! giving all the number densities n(:)
  function krome_get_rho(n)
    use krome_commons
    real*8 :: krome_get_rho, n(nmols)
    real*8::m(nmols)
    m(:) = krome_get_mass()
    krome_get_rho = sum(m(:)*n(:))
  end function krome_get_rho

  !*************************
  !scale the abundances of the metals contained in n(:)
  ! to Z according to Asplund+2009.
  ! note that this applies only to neutral atoms.
  subroutine krome_scale_Z(x,Z)
    use krome_commons
    use krome_getphys
    real*8 :: x(nmols)
    real*8 :: Z
    real*8::Htot,n(nspec)

    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0

    Htot = get_Hnuclei(n(:))
    x(idx_C) = max(Htot * 1d1**(Z+(-3.5700000000000003)), 1d-40)
    x(idx_N) = max(Htot * 1d1**(Z+(-4.17)), 1d-40)
    x(idx_O) = max(Htot * 1d1**(Z+(-3.3100000000000005)), 1d-40)
    x(idx_NA) = max(Htot * 1d1**(Z+(-5.76)), 1d-40)
    x(idx_MG) = max(Htot * 1d1**(Z+(-4.4)), 1d-40)
    x(idx_SI) = max(Htot * 1d1**(Z+(-4.49)), 1d-40)
    x(idx_P) = max(Htot * 1d1**(Z+(-6.59)), 1d-40)
    x(idx_S) = max(Htot * 1d1**(Z+(-4.88)), 1d-40)
    x(idx_CL) = max(Htot * 1d1**(Z+(-6.5)), 1d-40)
    x(idx_FE) = max(Htot * 1d1**(Z+(-4.5)), 1d-40)

  end subroutine krome_scale_Z

  !*************************
  !set the total metallicity
  ! in terms of Z/Z_solar
  subroutine krome_set_Z(xarg)
    use krome_commons
    real*8 :: xarg

    total_Z = xarg

  end subroutine krome_set_Z

  !*************************
  !set D is in terms of D_solar (D/D_sol).
  subroutine krome_set_dust_to_gas(xarg)
    use krome_commons
    real*8 :: xarg

    dust2gas_ratio = xarg

  end subroutine

  !*************************
  !set the clumping factor
  subroutine krome_set_clump(xarg)
    use krome_commons
    real*8 :: xarg

    clump_factor = xarg

  end subroutine krome_set_clump

  !***********************
  !get the number of electrons assuming
  ! total neutral charge (cations-anions)
  function krome_get_electrons(x)
    use krome_commons
    use krome_subs
    use krome_getphys
    real*8 :: x(nmols), krome_get_electrons
    real*8::n(nspec)
    n(1:nmols) = x(:)
    n(nmols+1:nspec) = 0d0
    krome_get_electrons = get_electrons(n(:))
  end function krome_get_electrons

  !**********************
  !print on screen the first nbest highest reaction fluxes
  subroutine krome_print_best_flux(xin,Tgas,nbest)
    use krome_subs
    use krome_commons
    implicit none
    real*8 :: xin(nmols)
    real*8 :: Tgas
    real*8::x(nmols),n(nspec)
    integer :: nbest
    n(1:nmols) = xin(:)
    n(idx_Tgas) = Tgas
    call print_best_flux(n,Tgas,nbest)

  end subroutine krome_print_best_flux

  !*********************
  !print only the highest fluxes greater than a fraction frac
  ! of the maximum flux
  subroutine krome_print_best_flux_frac(xin,Tgas,frac)
    use krome_subs
    use krome_commons
    implicit none
    real*8 :: xin(nmols)
    real*8 :: Tgas,frac
    real*8::n(nspec)
    n(1:nmols) = xin(:)
    n(idx_Tgas) = Tgas
    call print_best_flux_frac(n,Tgas,frac)

  end subroutine krome_print_best_flux_frac

  !**********************
  !print the highest nbest fluxes for reactions involving
  !a given species using the index idx_find (e.g. krome_idx_H2)
  subroutine krome_print_best_flux_spec(xin,Tgas,nbest,idx_find)
    use krome_subs
    use krome_commons
    implicit none
    real*8 :: xin(nmols)
    real*8 :: Tgas
    real*8::n(nspec)
    integer :: nbest,idx_find
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
  subroutine krome_explore_flux(x,Tgas,ifile,xvar)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    real*8 :: x(nmols)
    real*8 :: Tgas,xvar
    real*8::flux(nrea),fluxmax,sumflux,n(nspec)
    integer :: ifile
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
  subroutine krome_dump_flux(n,Tgas,nfile)
    use krome_commons
    real*8 :: n(nmols)
    real*8 :: Tgas
    real*8::flux(nrea)
    integer :: nfile
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
  subroutine krome_dump_rates(inTmin,inTmax,imax,funit)
    use krome_commons
    use krome_subs
    implicit none
    integer::i,j
    integer :: funit,imax
    real*8 :: inTmin,inTmax
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
  subroutine krome_get_info(x, Tgas)
    use krome_commons
    use krome_subs
    use krome_getphys
    implicit none
    integer::i,charges(nspec)
    real*8 :: x(nmols)
    real*8 :: Tgas
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
  subroutine krome_set_mpi_rank(xarg)
    use krome_commons
    implicit none
    integer :: xarg
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

end module krome_user
