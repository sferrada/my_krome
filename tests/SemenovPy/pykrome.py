# -*- coding: utf-8 -*-
import numpy as np
import ctypes
import numpy.ctypeslib as npctypes

# define aliases for complicated variable types
int_byref = ctypes.POINTER(ctypes.c_int)
dble_byref = ctypes.POINTER(ctypes.c_double)
array_1d_int = npctypes.ndpointer(dtype=np.int64,ndim=1,flags='CONTIGUOUS')
array_1d_double = npctypes.ndpointer(dtype=np.double,ndim=1,flags='CONTIGUOUS')
array_2d_double = npctypes.ndpointer(dtype=np.double,ndim=2,flags='CONTIGUOUS')

class PyKROME(object):
	# NOTE: Fortran is, by default, case-insensitive. As a side effect, all of the
	# symbols in `libkrome.so` are entirely lower-case (regardless of their form in
	# the Fortran files).

	def __init__(self, path_to_lib='.'):
		fortran = npctypes.load_library('libkrome.so',path_to_lib)

		# alias the Fortran functions and subroutines to `lib`.
		self.lib = fortran

		self.krome_idx_E = 0 # E
		self.krome_idx_Hk = 1 # H-
		self.krome_idx_Ck = 2 # C-
		self.krome_idx_CNk = 3 # CN-
		self.krome_idx_Ok = 4 # O-
		self.krome_idx_OHk = 5 # OH-
		self.krome_idx_Sk = 6 # S-
		self.krome_idx_GRAINk = 7 # GRAIN-
		self.krome_idx_C = 8 # C
		self.krome_idx_CL = 9 # CL
		self.krome_idx_FE = 10 # FE
		self.krome_idx_H = 11 # H
		self.krome_idx_HE = 12 # HE
		self.krome_idx_MG = 13 # MG
		self.krome_idx_N = 14 # N
		self.krome_idx_NA = 15 # NA
		self.krome_idx_O = 16 # O
		self.krome_idx_P = 17 # P
		self.krome_idx_S = 18 # S
		self.krome_idx_SI = 19 # SI
		self.krome_idx_C2 = 20 # C2
		self.krome_idx_CCL = 21 # CCL
		self.krome_idx_CH = 22 # CH
		self.krome_idx_CLO = 23 # CLO
		self.krome_idx_CN = 24 # CN
		self.krome_idx_CO = 25 # CO
		self.krome_idx_CP = 26 # CP
		self.krome_idx_CS = 27 # CS
		self.krome_idx_H2 = 28 # H2
		self.krome_idx_HCL = 29 # HCL
		self.krome_idx_HS = 30 # HS
		self.krome_idx_MGH = 31 # MGH
		self.krome_idx_N2 = 32 # N2
		self.krome_idx_NAH = 33 # NAH
		self.krome_idx_NH = 34 # NH
		self.krome_idx_NO = 35 # NO
		self.krome_idx_NS = 36 # NS
		self.krome_idx_O2 = 37 # O2
		self.krome_idx_OH = 38 # OH
		self.krome_idx_PH = 39 # PH
		self.krome_idx_PN = 40 # PN
		self.krome_idx_PO = 41 # PO
		self.krome_idx_S2 = 42 # S2
		self.krome_idx_SIC = 43 # SIC
		self.krome_idx_SIH = 44 # SIH
		self.krome_idx_SIN = 45 # SIN
		self.krome_idx_SIO = 46 # SIO
		self.krome_idx_SIS = 47 # SIS
		self.krome_idx_SO = 48 # SO
		self.krome_idx_C2H = 49 # C2H
		self.krome_idx_C2N = 50 # C2N
		self.krome_idx_C2S = 51 # C2S
		self.krome_idx_C3 = 52 # C3
		self.krome_idx_CCO = 53 # CCO
		self.krome_idx_CCP = 54 # CCP
		self.krome_idx_CH2 = 55 # CH2
		self.krome_idx_CO2 = 56 # CO2
		self.krome_idx_H2O = 57 # H2O
		self.krome_idx_H2S = 58 # H2S
		self.krome_idx_HCN = 59 # HCN
		self.krome_idx_HCO = 60 # HCO
		self.krome_idx_HCP = 61 # HCP
		self.krome_idx_HCS = 62 # HCS
		self.krome_idx_HCSI = 63 # HCSI
		self.krome_idx_HNC = 64 # HNC
		self.krome_idx_HNO = 65 # HNO
		self.krome_idx_HNSI = 66 # HNSI
		self.krome_idx_HPO = 67 # HPO
		self.krome_idx_HS2 = 68 # HS2
		self.krome_idx_N2O = 69 # N2O
		self.krome_idx_NAOH = 70 # NAOH
		self.krome_idx_NH2 = 71 # NH2
		self.krome_idx_NO2 = 72 # NO2
		self.krome_idx_O2H = 73 # O2H
		self.krome_idx_OCN = 74 # OCN
		self.krome_idx_OCS = 75 # OCS
		self.krome_idx_PH2 = 76 # PH2
		self.krome_idx_SIC2 = 77 # SIC2
		self.krome_idx_SIH2 = 78 # SIH2
		self.krome_idx_SINC = 79 # SINC
		self.krome_idx_SIO2 = 80 # SIO2
		self.krome_idx_SO2 = 81 # SO2
		self.krome_idx_C2H2 = 82 # C2H2
		self.krome_idx_C3H = 83 # C3H
		self.krome_idx_C3N = 84 # C3N
		self.krome_idx_C3O = 85 # C3O
		self.krome_idx_C3P = 86 # C3P
		self.krome_idx_C3S = 87 # C3S
		self.krome_idx_C4 = 88 # C4
		self.krome_idx_CH3 = 89 # CH3
		self.krome_idx_H2CO = 90 # H2CO
		self.krome_idx_H2CS = 91 # H2CS
		self.krome_idx_H2O2 = 92 # H2O2
		self.krome_idx_H2S2 = 93 # H2S2
		self.krome_idx_H2SIO = 94 # H2SIO
		self.krome_idx_HCCP = 95 # HCCP
		self.krome_idx_NH3 = 96 # NH3
		self.krome_idx_SIC2H = 97 # SIC2H
		self.krome_idx_SIC3 = 98 # SIC3
		self.krome_idx_SICH2 = 99 # SICH2
		self.krome_idx_SIH3 = 100 # SIH3
		self.krome_idx_C2H2N = 101 # C2H2N
		self.krome_idx_C2H2O = 102 # C2H2O
		self.krome_idx_C2H3 = 103 # C2H3
		self.krome_idx_C3H2 = 104 # C3H2
		self.krome_idx_C4H = 105 # C4H
		self.krome_idx_C4N = 106 # C4N
		self.krome_idx_C4P = 107 # C4P
		self.krome_idx_C4S = 108 # C4S
		self.krome_idx_C5 = 109 # C5
		self.krome_idx_CH2O2 = 110 # CH2O2
		self.krome_idx_CH2PH = 111 # CH2PH
		self.krome_idx_CH3N = 112 # CH3N
		self.krome_idx_CH4 = 113 # CH4
		self.krome_idx_HC3N = 114 # HC3N
		self.krome_idx_SIC2H2 = 115 # SIC2H2
		self.krome_idx_SIC3H = 116 # SIC3H
		self.krome_idx_SIC4 = 117 # SIC4
		self.krome_idx_SICH3 = 118 # SICH3
		self.krome_idx_SIH4 = 119 # SIH4
		self.krome_idx_C2H3N = 120 # C2H3N
		self.krome_idx_C2H4 = 121 # C2H4
		self.krome_idx_C3H3 = 122 # C3H3
		self.krome_idx_C4H2 = 123 # C4H2
		self.krome_idx_C5H = 124 # C5H
		self.krome_idx_C5N = 125 # C5N
		self.krome_idx_C6 = 126 # C6
		self.krome_idx_CH4O = 127 # CH4O
		self.krome_idx_C2H4O = 128 # C2H4O
		self.krome_idx_C2H5 = 129 # C2H5
		self.krome_idx_C3H3N = 130 # C3H3N
		self.krome_idx_C3H4 = 131 # C3H4
		self.krome_idx_C5H2 = 132 # C5H2
		self.krome_idx_C6H = 133 # C6H
		self.krome_idx_C7 = 134 # C7
		self.krome_idx_CH5N = 135 # CH5N
		self.krome_idx_HC5N = 136 # HC5N
		self.krome_idx_C6H2 = 137 # C6H2
		self.krome_idx_C7H = 138 # C7H
		self.krome_idx_C7N = 139 # C7N
		self.krome_idx_C8 = 140 # C8
		self.krome_idx_CH3C3N = 141 # CH3C3N
		self.krome_idx_HCOOCH3 = 142 # HCOOCH3
		self.krome_idx_C2H5OH = 143 # C2H5OH
		self.krome_idx_C7H2 = 144 # C7H2
		self.krome_idx_C8H = 145 # C8H
		self.krome_idx_C9 = 146 # C9
		self.krome_idx_CH3C4H = 147 # CH3C4H
		self.krome_idx_CH3OCH3 = 148 # CH3OCH3
		self.krome_idx_HC7N = 149 # HC7N
		self.krome_idx_C2H6CO = 150 # C2H6CO
		self.krome_idx_C8H2 = 151 # C8H2
		self.krome_idx_C9H = 152 # C9H
		self.krome_idx_C9N = 153 # C9N
		self.krome_idx_C10 = 154 # C10
		self.krome_idx_CH3C5N = 155 # CH3C5N
		self.krome_idx_C9H2 = 156 # C9H2
		self.krome_idx_CH3C6H = 157 # CH3C6H
		self.krome_idx_CH3C7N = 158 # CH3C7N
		self.krome_idx_HC9N = 159 # HC9N
		self.krome_idx_C4H4 = 160 # C4H4
		self.krome_idx_HCNC2 = 161 # HCNC2
		self.krome_idx_HC2NC = 162 # HC2NC
		self.krome_idx_HNC3 = 163 # HNC3
		self.krome_idx_NH2CHO = 164 # NH2CHO
		self.krome_idx_C4H3 = 165 # C4H3
		self.krome_idx_NH2CN = 166 # NH2CN
		self.krome_idx_C6H6 = 167 # C6H6
		self.krome_idx_H2CN = 168 # H2CN
		self.krome_idx_GRAIN0 = 169 # GRAIN0
		self.krome_idx_O3 = 170 # O3
		self.krome_idx_FEH = 171 # FEH
		self.krome_idx_HNCO = 172 # HNCO
		self.krome_idx_HC2O = 173 # HC2O
		self.krome_idx_HCCN = 174 # HCCN
		self.krome_idx_HC3O = 175 # HC3O
		self.krome_idx_MGH2 = 176 # MGH2
		self.krome_idx_N2H2 = 177 # N2H2
		self.krome_idx_CHNH = 178 # CHNH
		self.krome_idx_H2C3O = 179 # H2C3O
		self.krome_idx_H2C3N = 180 # H2C3N
		self.krome_idx_H2C5N = 181 # H2C5N
		self.krome_idx_H2C7N = 182 # H2C7N
		self.krome_idx_H2C9N = 183 # H2C9N
		self.krome_idx_NH2OH = 184 # NH2OH
		self.krome_idx_CH2OH = 185 # CH2OH
		self.krome_idx_C5H3 = 186 # C5H3
		self.krome_idx_H3C5N = 187 # H3C5N
		self.krome_idx_C6H3 = 188 # C6H3
		self.krome_idx_C7H3 = 189 # C7H3
		self.krome_idx_H3C7N = 190 # H3C7N
		self.krome_idx_C8H3 = 191 # C8H3
		self.krome_idx_C9H3 = 192 # C9H3
		self.krome_idx_H3C9N = 193 # H3C9N
		self.krome_idx_CH3NH = 194 # CH3NH
		self.krome_idx_H4C3N = 195 # H4C3N
		self.krome_idx_C5H4 = 196 # C5H4
		self.krome_idx_C6H4 = 197 # C6H4
		self.krome_idx_C7H4 = 198 # C7H4
		self.krome_idx_C8H4 = 199 # C8H4
		self.krome_idx_C9H4 = 200 # C9H4
		self.krome_idx_H5C3N = 201 # H5C3N
		self.krome_idx_C2H6 = 202 # C2H6
		self.krome_idx_C_DUST = 203 # C_DUST
		self.krome_idx_C2_DUST = 204 # C2_DUST
		self.krome_idx_C3_DUST = 205 # C3_DUST
		self.krome_idx_C2H_DUST = 206 # C2H_DUST
		self.krome_idx_C3H_DUST = 207 # C3H_DUST
		self.krome_idx_C2H3_DUST = 208 # C2H3_DUST
		self.krome_idx_C3H3_DUST = 209 # C3H3_DUST
		self.krome_idx_C2N_DUST = 210 # C2N_DUST
		self.krome_idx_C3N_DUST = 211 # C3N_DUST
		self.krome_idx_CCO_DUST = 212 # CCO_DUST
		self.krome_idx_C3O_DUST = 213 # C3O_DUST
		self.krome_idx_C2S_DUST = 214 # C2S_DUST
		self.krome_idx_C3S_DUST = 215 # C3S_DUST
		self.krome_idx_C4_DUST = 216 # C4_DUST
		self.krome_idx_C4H_DUST = 217 # C4H_DUST
		self.krome_idx_C5_DUST = 218 # C5_DUST
		self.krome_idx_C5H_DUST = 219 # C5H_DUST
		self.krome_idx_C6_DUST = 220 # C6_DUST
		self.krome_idx_C6H_DUST = 221 # C6H_DUST
		self.krome_idx_C7_DUST = 222 # C7_DUST
		self.krome_idx_C7H_DUST = 223 # C7H_DUST
		self.krome_idx_C8_DUST = 224 # C8_DUST
		self.krome_idx_C8H_DUST = 225 # C8H_DUST
		self.krome_idx_C9_DUST = 226 # C9_DUST
		self.krome_idx_C9H_DUST = 227 # C9H_DUST
		self.krome_idx_C10_DUST = 228 # C10_DUST
		self.krome_idx_CH_DUST = 229 # CH_DUST
		self.krome_idx_CH2_DUST = 230 # CH2_DUST
		self.krome_idx_C2H2_DUST = 231 # C2H2_DUST
		self.krome_idx_CH3_DUST = 232 # CH3_DUST
		self.krome_idx_CN_DUST = 233 # CN_DUST
		self.krome_idx_HS_DUST = 234 # HS_DUST
		self.krome_idx_CS_DUST = 235 # CS_DUST
		self.krome_idx_H_DUST = 236 # H_DUST
		self.krome_idx_N_DUST = 237 # N_DUST
		self.krome_idx_NH_DUST = 238 # NH_DUST
		self.krome_idx_HNC_DUST = 239 # HNC_DUST
		self.krome_idx_NH2_DUST = 240 # NH2_DUST
		self.krome_idx_NO_DUST = 241 # NO_DUST
		self.krome_idx_O_DUST = 242 # O_DUST
		self.krome_idx_OCN_DUST = 243 # OCN_DUST
		self.krome_idx_NS_DUST = 244 # NS_DUST
		self.krome_idx_S_DUST = 245 # S_DUST
		self.krome_idx_CO_DUST = 246 # CO_DUST
		self.krome_idx_O2_DUST = 247 # O2_DUST
		self.krome_idx_OH_DUST = 248 # OH_DUST
		self.krome_idx_SO_DUST = 249 # SO_DUST
		self.krome_idx_C3H2_DUST = 250 # C3H2_DUST
		self.krome_idx_C3H4_DUST = 251 # C3H4_DUST
		self.krome_idx_C4H2_DUST = 252 # C4H2_DUST
		self.krome_idx_C5H2_DUST = 253 # C5H2_DUST
		self.krome_idx_C6H2_DUST = 254 # C6H2_DUST
		self.krome_idx_C7H2_DUST = 255 # C7H2_DUST
		self.krome_idx_C8H2_DUST = 256 # C8H2_DUST
		self.krome_idx_C9H2_DUST = 257 # C9H2_DUST
		self.krome_idx_C2H4_DUST = 258 # C2H4_DUST
		self.krome_idx_HCCN_DUST = 259 # HCCN_DUST
		self.krome_idx_HNO_DUST = 260 # HNO_DUST
		self.krome_idx_HCN_DUST = 261 # HCN_DUST
		self.krome_idx_CHNH_DUST = 262 # CHNH_DUST
		self.krome_idx_CH3N_DUST = 263 # CH3N_DUST
		self.krome_idx_HCO_DUST = 264 # HCO_DUST
		self.krome_idx_C2H5_DUST = 265 # C2H5_DUST
		self.krome_idx_C2H2N_DUST = 266 # C2H2N_DUST
		self.krome_idx_CH2NH2_DUST = 267 # CH2NH2_DUST
		self.krome_idx_H2CO_DUST = 268 # H2CO_DUST
		self.krome_idx_CH3C3N_DUST = 269 # CH3C3N_DUST
		self.krome_idx_C5N_DUST = 270 # C5N_DUST
		self.krome_idx_CH3C5N_DUST = 271 # CH3C5N_DUST
		self.krome_idx_C7N_DUST = 272 # C7N_DUST
		self.krome_idx_CH3C7N_DUST = 273 # CH3C7N_DUST
		self.krome_idx_CH2OH_DUST = 274 # CH2OH_DUST
		self.krome_idx_C2H5OH_DUST = 275 # C2H5OH_DUST
		self.krome_idx_CH3OCH3_DUST = 276 # CH3OCH3_DUST
		self.krome_idx_C2H6_DUST = 277 # C2H6_DUST
		self.krome_idx_C2H3N_DUST = 278 # C2H3N_DUST
		self.krome_idx_C2H4O_DUST = 279 # C2H4O_DUST
		self.krome_idx_CH4_DUST = 280 # CH4_DUST
		self.krome_idx_H2_DUST = 281 # H2_DUST
		self.krome_idx_HC2O_DUST = 282 # HC2O_DUST
		self.krome_idx_C3H3N_DUST = 283 # C3H3N_DUST
		self.krome_idx_H4C3N_DUST = 284 # H4C3N_DUST
		self.krome_idx_HC3N_DUST = 285 # HC3N_DUST
		self.krome_idx_HC3O_DUST = 286 # HC3O_DUST
		self.krome_idx_C4H3_DUST = 287 # C4H3_DUST
		self.krome_idx_C4H4_DUST = 288 # C4H4_DUST
		self.krome_idx_C5H3_DUST = 289 # C5H3_DUST
		self.krome_idx_C5H4_DUST = 290 # C5H4_DUST
		self.krome_idx_HC5N_DUST = 291 # HC5N_DUST
		self.krome_idx_C6H3_DUST = 292 # C6H3_DUST
		self.krome_idx_C6H4_DUST = 293 # C6H4_DUST
		self.krome_idx_C7H3_DUST = 294 # C7H3_DUST
		self.krome_idx_C7H4_DUST = 295 # C7H4_DUST
		self.krome_idx_HC7N_DUST = 296 # HC7N_DUST
		self.krome_idx_C8H3_DUST = 297 # C8H3_DUST
		self.krome_idx_C8H4_DUST = 298 # C8H4_DUST
		self.krome_idx_C9H3_DUST = 299 # C9H3_DUST
		self.krome_idx_C9H4_DUST = 300 # C9H4_DUST
		self.krome_idx_C9N_DUST = 301 # C9N_DUST
		self.krome_idx_HC9N_DUST = 302 # HC9N_DUST
		self.krome_idx_CH3NH_DUST = 303 # CH3NH_DUST
		self.krome_idx_CH5N_DUST = 304 # CH5N_DUST
		self.krome_idx_CH4O_DUST = 305 # CH4O_DUST
		self.krome_idx_HCS_DUST = 306 # HCS_DUST
		self.krome_idx_FE_DUST = 307 # FE_DUST
		self.krome_idx_FEH_DUST = 308 # FEH_DUST
		self.krome_idx_H2C3N_DUST = 309 # H2C3N_DUST
		self.krome_idx_H2C5N_DUST = 310 # H2C5N_DUST
		self.krome_idx_H3C5N_DUST = 311 # H3C5N_DUST
		self.krome_idx_H2C7N_DUST = 312 # H2C7N_DUST
		self.krome_idx_H3C7N_DUST = 313 # H3C7N_DUST
		self.krome_idx_H2C9N_DUST = 314 # H2C9N_DUST
		self.krome_idx_H3C9N_DUST = 315 # H3C9N_DUST
		self.krome_idx_H2CN_DUST = 316 # H2CN_DUST
		self.krome_idx_H2O2_DUST = 317 # H2O2_DUST
		self.krome_idx_H2O_DUST = 318 # H2O_DUST
		self.krome_idx_O2H_DUST = 319 # O2H_DUST
		self.krome_idx_H2S_DUST = 320 # H2S_DUST
		self.krome_idx_H5C3N_DUST = 321 # H5C3N_DUST
		self.krome_idx_C2H2O_DUST = 322 # C2H2O_DUST
		self.krome_idx_H2C3O_DUST = 323 # H2C3O_DUST
		self.krome_idx_H2CS_DUST = 324 # H2CS_DUST
		self.krome_idx_MG_DUST = 325 # MG_DUST
		self.krome_idx_MGH_DUST = 326 # MGH_DUST
		self.krome_idx_MGH2_DUST = 327 # MGH2_DUST
		self.krome_idx_N2H2_DUST = 328 # N2H2_DUST
		self.krome_idx_N2_DUST = 329 # N2_DUST
		self.krome_idx_NA_DUST = 330 # NA_DUST
		self.krome_idx_NAH_DUST = 331 # NAH_DUST
		self.krome_idx_NH3_DUST = 332 # NH3_DUST
		self.krome_idx_O3_DUST = 333 # O3_DUST
		self.krome_idx_HNCO_DUST = 334 # HNCO_DUST
		self.krome_idx_OCS_DUST = 335 # OCS_DUST
		self.krome_idx_SI_DUST = 336 # SI_DUST
		self.krome_idx_SIH_DUST = 337 # SIH_DUST
		self.krome_idx_SIH2_DUST = 338 # SIH2_DUST
		self.krome_idx_SIH3_DUST = 339 # SIH3_DUST
		self.krome_idx_SIH4_DUST = 340 # SIH4_DUST
		self.krome_idx_SO2_DUST = 341 # SO2_DUST
		self.krome_idx_HCOOCH3_DUST = 342 # HCOOCH3_DUST
		self.krome_idx_NH2CHO_DUST = 343 # NH2CHO_DUST
		self.krome_idx_CO2_DUST = 344 # CO2_DUST
		self.krome_idx_CH2O2_DUST = 345 # CH2O2_DUST
		self.krome_idx_NH2OH_DUST = 346 # NH2OH_DUST
		self.krome_idx_C4N_DUST = 347 # C4N_DUST
		self.krome_idx_C4S_DUST = 348 # C4S_DUST
		self.krome_idx_C6H6_DUST = 349 # C6H6_DUST
		self.krome_idx_CH2NH2 = 350 # CH2NH2
		self.krome_idx_CH3C4H_DUST = 351 # CH3C4H_DUST
		self.krome_idx_CH3C6H_DUST = 352 # CH3C6H_DUST
		self.krome_idx_H2S2_DUST = 353 # H2S2_DUST
		self.krome_idx_HC2NC_DUST = 354 # HC2NC_DUST
		self.krome_idx_HCNC2_DUST = 355 # HCNC2_DUST
		self.krome_idx_HE_DUST = 356 # HE_DUST
		self.krome_idx_HNC3_DUST = 357 # HNC3_DUST
		self.krome_idx_HS2_DUST = 358 # HS2_DUST
		self.krome_idx_NAOH_DUST = 359 # NAOH_DUST
		self.krome_idx_S2_DUST = 360 # S2_DUST
		self.krome_idx_SIC_DUST = 361 # SIC_DUST
		self.krome_idx_SIO_DUST = 362 # SIO_DUST
		self.krome_idx_SIS_DUST = 363 # SIS_DUST
		self.krome_idx_C2H6CO_DUST = 364 # C2H6CO_DUST
		self.krome_idx_C3P_DUST = 365 # C3P_DUST
		self.krome_idx_CCP_DUST = 366 # CCP_DUST
		self.krome_idx_C4P_DUST = 367 # C4P_DUST
		self.krome_idx_CCL_DUST = 368 # CCL_DUST
		self.krome_idx_CL_DUST = 369 # CL_DUST
		self.krome_idx_P_DUST = 370 # P_DUST
		self.krome_idx_CP_DUST = 371 # CP_DUST
		self.krome_idx_CH2PH_DUST = 372 # CH2PH_DUST
		self.krome_idx_HCP_DUST = 373 # HCP_DUST
		self.krome_idx_CLO_DUST = 374 # CLO_DUST
		self.krome_idx_H2SIO_DUST = 375 # H2SIO_DUST
		self.krome_idx_HCCP_DUST = 376 # HCCP_DUST
		self.krome_idx_HCL_DUST = 377 # HCL_DUST
		self.krome_idx_HCSI_DUST = 378 # HCSI_DUST
		self.krome_idx_HNSI_DUST = 379 # HNSI_DUST
		self.krome_idx_SIN_DUST = 380 # SIN_DUST
		self.krome_idx_HPO_DUST = 381 # HPO_DUST
		self.krome_idx_PO_DUST = 382 # PO_DUST
		self.krome_idx_N2O_DUST = 383 # N2O_DUST
		self.krome_idx_NH2CN_DUST = 384 # NH2CN_DUST
		self.krome_idx_NO2_DUST = 385 # NO2_DUST
		self.krome_idx_PH_DUST = 386 # PH_DUST
		self.krome_idx_PH2_DUST = 387 # PH2_DUST
		self.krome_idx_PN_DUST = 388 # PN_DUST
		self.krome_idx_SIC2_DUST = 389 # SIC2_DUST
		self.krome_idx_SIC2H_DUST = 390 # SIC2H_DUST
		self.krome_idx_SIC2H2_DUST = 391 # SIC2H2_DUST
		self.krome_idx_SIC3_DUST = 392 # SIC3_DUST
		self.krome_idx_SIC3H_DUST = 393 # SIC3H_DUST
		self.krome_idx_SIC4_DUST = 394 # SIC4_DUST
		self.krome_idx_SICH2_DUST = 395 # SICH2_DUST
		self.krome_idx_SICH3_DUST = 396 # SICH3_DUST
		self.krome_idx_SINC_DUST = 397 # SINC_DUST
		self.krome_idx_SIO2_DUST = 398 # SIO2_DUST
		self.krome_idx_Cj = 399 # C+
		self.krome_idx_CLj = 400 # CL+
		self.krome_idx_FEj = 401 # FE+
		self.krome_idx_Hj = 402 # H+
		self.krome_idx_HEj = 403 # HE+
		self.krome_idx_MGj = 404 # MG+
		self.krome_idx_Nj = 405 # N+
		self.krome_idx_NAj = 406 # NA+
		self.krome_idx_Oj = 407 # O+
		self.krome_idx_Pj = 408 # P+
		self.krome_idx_Sj = 409 # S+
		self.krome_idx_SIj = 410 # SI+
		self.krome_idx_COj = 411 # CO+
		self.krome_idx_H2j = 412 # H2+
		self.krome_idx_NOj = 413 # NO+
		self.krome_idx_O2j = 414 # O2+
		self.krome_idx_CH2j = 415 # CH2+
		self.krome_idx_H2Sj = 416 # H2S+
		self.krome_idx_HCOj = 417 # HCO+
		self.krome_idx_HCSj = 418 # HCS+
		self.krome_idx_HNOj = 419 # HNO+
		self.krome_idx_NH2j = 420 # NH2+
		self.krome_idx_OCSj = 421 # OCS+
		self.krome_idx_C2H2j = 422 # C2H2+
		self.krome_idx_CH3j = 423 # CH3+
		self.krome_idx_NH3j = 424 # NH3+
		self.krome_idx_C2H2Oj = 425 # C2H2O+
		self.krome_idx_CH2O2j = 426 # CH2O2+
		self.krome_idx_C2H3Nj = 427 # C2H3N+
		self.krome_idx_C2H4j = 428 # C2H4+
		self.krome_idx_C4H2j = 429 # C4H2+
		self.krome_idx_H3COj = 430 # H3CO+
		self.krome_idx_CH4Oj = 431 # CH4O+
		self.krome_idx_C2H4Oj = 432 # C2H4O+
		self.krome_idx_C3H4j = 433 # C3H4+
		self.krome_idx_CH5Nj = 434 # CH5N+
		self.krome_idx_C2H5OHj = 435 # C2H5OH+
		self.krome_idx_CH3OCH3j = 436 # CH3OCH3+
		self.krome_idx_CHj = 437 # CH+
		self.krome_idx_CCLj = 438 # CCL+
		self.krome_idx_C2j = 439 # C2+
		self.krome_idx_CLOj = 440 # CLO+
		self.krome_idx_CPj = 441 # CP+
		self.krome_idx_CSj = 442 # CS+
		self.krome_idx_CNj = 443 # CN+
		self.krome_idx_NSj = 444 # NS+
		self.krome_idx_PHj = 445 # PH+
		self.krome_idx_POj = 446 # PO+
		self.krome_idx_SICj = 447 # SIC+
		self.krome_idx_SINj = 448 # SIN+
		self.krome_idx_SISj = 449 # SIS+
		self.krome_idx_SOj = 450 # SO+
		self.krome_idx_C3j = 451 # C3+
		self.krome_idx_C2Sj = 452 # C2S+
		self.krome_idx_C2Oj = 453 # C2O+
		self.krome_idx_CCPj = 454 # CCP+
		self.krome_idx_C2Hj = 455 # C2H+
		self.krome_idx_HOCj = 456 # HOC+
		self.krome_idx_C2Nj = 457 # C2N+
		self.krome_idx_CNCj = 458 # CNC+
		self.krome_idx_HCPj = 459 # HCP+
		self.krome_idx_SIC2j = 460 # SIC2+
		self.krome_idx_SINCj = 461 # SINC+
		self.krome_idx_HPOj = 462 # HPO+
		self.krome_idx_HCNj = 463 # HCN+
		self.krome_idx_CHSIj = 464 # CHSI+
		self.krome_idx_SIH2j = 465 # SIH2+
		self.krome_idx_C3Hj = 466 # C3H+
		self.krome_idx_C4j = 467 # C4+
		self.krome_idx_C3Oj = 468 # C3O+
		self.krome_idx_C3Sj = 469 # C3S+
		self.krome_idx_H2COj = 470 # H2CO+
		self.krome_idx_H2SIOj = 471 # H2SIO+
		self.krome_idx_HCNHj = 472 # HCNH+
		self.krome_idx_SIC2Hj = 473 # SIC2H+
		self.krome_idx_SIC3j = 474 # SIC3+
		self.krome_idx_CH2SIj = 475 # CH2SI+
		self.krome_idx_SIH3j = 476 # SIH3+
		self.krome_idx_C2H2Nj = 477 # C2H2N+
		self.krome_idx_C2H3j = 478 # C2H3+
		self.krome_idx_C3H2j = 479 # C3H2+
		self.krome_idx_H2C3j = 480 # H2C3+
		self.krome_idx_C4Hj = 481 # C4H+
		self.krome_idx_C5j = 482 # C5+
		self.krome_idx_C4Sj = 483 # C4S+
		self.krome_idx_PC2Hj = 484 # PC2H+
		self.krome_idx_C3Nj = 485 # C3N+
		self.krome_idx_C4Nj = 486 # C4N+
		self.krome_idx_C3HNj = 487 # C3HN+
		self.krome_idx_HNCj = 488 # HNC+
		self.krome_idx_SIC3Hj = 489 # SIC3H+
		self.krome_idx_SIC4j = 490 # SIC4+
		self.krome_idx_SIC2H2j = 491 # SIC2H2+
		self.krome_idx_SICH3j = 492 # SICH3+
		self.krome_idx_HC2NCHj = 493 # HC2NCH+
		self.krome_idx_C3H3j = 494 # C3H3+
		self.krome_idx_H3C3j = 495 # H3C3+
		self.krome_idx_C5Hj = 496 # C5H+
		self.krome_idx_C6j = 497 # C6+
		self.krome_idx_C2H3Oj = 498 # C2H3O+
		self.krome_idx_C2H5j = 499 # C2H5+
		self.krome_idx_C3H3Nj = 500 # C3H3N+
		self.krome_idx_C5H2j = 501 # C5H2+
		self.krome_idx_C4H3j = 502 # C4H3+
		self.krome_idx_C6Hj = 503 # C6H+
		self.krome_idx_C7j = 504 # C7+
		self.krome_idx_CH4Nj = 505 # CH4N+
		self.krome_idx_C5HNj = 506 # C5HN+
		self.krome_idx_C7Hj = 507 # C7H+
		self.krome_idx_C8j = 508 # C8+
		self.krome_idx_COOCH4j = 509 # COOCH4+
		self.krome_idx_C2H5Oj = 510 # C2H5O+
		self.krome_idx_C8Hj = 511 # C8H+
		self.krome_idx_C9j = 512 # C9+
		self.krome_idx_C5H3j = 513 # C5H3+
		self.krome_idx_C6H2j = 514 # C6H2+
		self.krome_idx_C6H3j = 515 # C6H3+
		self.krome_idx_C2H6COj = 516 # C2H6CO+
		self.krome_idx_C9Hj = 517 # C9H+
		self.krome_idx_C10j = 518 # C10+
		self.krome_idx_C7H3j = 519 # C7H3+
		self.krome_idx_C8H2j = 520 # C8H2+
		self.krome_idx_C8H3j = 521 # C8H3+
		self.krome_idx_HCLj = 522 # HCL+
		self.krome_idx_HSj = 523 # HS+
		self.krome_idx_NHj = 524 # NH+
		self.krome_idx_OHj = 525 # OH+
		self.krome_idx_PNj = 526 # PN+
		self.krome_idx_S2j = 527 # S2+
		self.krome_idx_SIHj = 528 # SIH+
		self.krome_idx_SIOj = 529 # SIO+
		self.krome_idx_H2Oj = 530 # H2O+
		self.krome_idx_HNSIj = 531 # HNSI+
		self.krome_idx_S2Hj = 532 # S2H+
		self.krome_idx_PH2j = 533 # PH2+
		self.krome_idx_H2CSj = 534 # H2CS+
		self.krome_idx_H2S2j = 535 # H2S2+
		self.krome_idx_HSIOj = 536 # HSIO+
		self.krome_idx_C4Pj = 537 # C4P+
		self.krome_idx_HCO2j = 538 # HCO2+
		self.krome_idx_PCH3j = 539 # PCH3+
		self.krome_idx_CH4j = 540 # CH4+
		self.krome_idx_C2NHj = 541 # C2NH+
		self.krome_idx_SIH4j = 542 # SIH4+
		self.krome_idx_NH4j = 543 # NH4+
		self.krome_idx_H2NCj = 544 # H2NC+
		self.krome_idx_C3H2Nj = 545 # C3H2N+
		self.krome_idx_C7H2j = 546 # C7H2+
		self.krome_idx_C5H4j = 547 # C5H4+
		self.krome_idx_C7HNj = 548 # C7HN+
		self.krome_idx_C9H2j = 549 # C9H2+
		self.krome_idx_C7H4j = 550 # C7H4+
		self.krome_idx_C9HNj = 551 # C9HN+
		self.krome_idx_N2j = 552 # N2+
		self.krome_idx_CO2j = 553 # CO2+
		self.krome_idx_HEHj = 554 # HEH+
		self.krome_idx_SO2j = 555 # SO2+
		self.krome_idx_C6H5j = 556 # C6H5+
		self.krome_idx_C5H5j = 557 # C5H5+
		self.krome_idx_N2Hj = 558 # N2H+
		self.krome_idx_NO2j = 559 # NO2+
		self.krome_idx_PC2H2j = 560 # PC2H2+
		self.krome_idx_PNH2j = 561 # PNH2+
		self.krome_idx_PCH2j = 562 # PCH2+
		self.krome_idx_HC2Sj = 563 # HC2S+
		self.krome_idx_HC3Sj = 564 # HC3S+
		self.krome_idx_H3CSj = 565 # H3CS+
		self.krome_idx_HC4Sj = 566 # HC4S+
		self.krome_idx_SINH2j = 567 # SINH2+
		self.krome_idx_SIC2H3j = 568 # SIC2H3+
		self.krome_idx_SIC3H2j = 569 # SIC3H2+
		self.krome_idx_C2HOj = 570 # C2HO+
		self.krome_idx_H3Oj = 571 # H3O+
		self.krome_idx_H3Sj = 572 # H3S+
		self.krome_idx_HOCSj = 573 # HOCS+
		self.krome_idx_CH5Oj = 574 # CH5O+
		self.krome_idx_NCOj = 575 # NCO+
		self.krome_idx_HNCOj = 576 # HNCO+
		self.krome_idx_C2N2j = 577 # C2N2+
		self.krome_idx_H3j = 578 # H3+
		self.krome_idx_O2Hj = 579 # O2H+
		self.krome_idx_CH5j = 580 # CH5+
		self.krome_idx_H2CLj = 581 # H2CL+
		self.krome_idx_CH3O2j = 582 # CH3O2+
		self.krome_idx_H2POj = 583 # H2PO+
		self.krome_idx_PNH3j = 584 # PNH3+
		self.krome_idx_PCH4j = 585 # PCH4+
		self.krome_idx_PC2H3j = 586 # PC2H3+
		self.krome_idx_HSISj = 587 # HSIS+
		self.krome_idx_HSOj = 588 # HSO+
		self.krome_idx_HNSj = 589 # HNS+
		self.krome_idx_HPNj = 590 # HPN+
		self.krome_idx_H2NOj = 591 # H2NO+
		self.krome_idx_NAH2Oj = 592 # NAH2O+
		self.krome_idx_PH3j = 593 # PH3+
		self.krome_idx_SINCHj = 594 # SINCH+
		self.krome_idx_HSIO2j = 595 # HSIO2+
		self.krome_idx_HSO2j = 596 # HSO2+
		self.krome_idx_HC3Oj = 597 # HC3O+
		self.krome_idx_PC3Hj = 598 # PC3H+
		self.krome_idx_H3S2j = 599 # H3S2+
		self.krome_idx_H3SIOj = 600 # H3SIO+
		self.krome_idx_PC4Hj = 601 # PC4H+
		self.krome_idx_NH2CNHj = 602 # NH2CNH+
		self.krome_idx_SIC4Hj = 603 # SIC4H+
		self.krome_idx_SICH4j = 604 # SICH4+
		self.krome_idx_SIH5j = 605 # SIH5+
		self.krome_idx_C2H4Nj = 606 # C2H4N+
		self.krome_idx_NH2CH2Oj = 607 # NH2CH2O+
		self.krome_idx_C2H6j = 608 # C2H6+
		self.krome_idx_C3H4Nj = 609 # C3H4N+
		self.krome_idx_C3H5j = 610 # C3H5+
		self.krome_idx_C4H4j = 611 # C4H4+
		self.krome_idx_CH6Nj = 612 # CH6N+
		self.krome_idx_C5H2Nj = 613 # C5H2N+
		self.krome_idx_C4H4Nj = 614 # C4H4N+
		self.krome_idx_H5C2O2j = 615 # H5C2O2+
		self.krome_idx_C2H5OH2j = 616 # C2H5OH2+
		self.krome_idx_CH3OCH4j = 617 # CH3OCH4+
		self.krome_idx_C7H2Nj = 618 # C7H2N+
		self.krome_idx_C3H6OHj = 619 # C3H6OH+
		self.krome_idx_C6H4Nj = 620 # C6H4N+
		self.krome_idx_C10Hj = 621 # C10H+
		self.krome_idx_C9H3j = 622 # C9H3+
		self.krome_idx_C7H5j = 623 # C7H5+
		self.krome_idx_C8H4Nj = 624 # C8H4N+
		self.krome_idx_C9H2Nj = 625 # C9H2N+
		self.krome_idx_C6H7j = 626 # C6H7+
		self.krome_idx_NAH2j = 627 # NAH2+
		self.krome_idx_PC2H4j = 628 # PC2H4+
		self.krome_idx_C4H5j = 629 # C4H5+
		self.krome_idx_H2CCLj = 630 # H2CCL+
		self.krome_idx_PC4H2j = 631 # PC4H2+
		self.krome_idx_C6H4j = 632 # C6H4+
		self.krome_idx_C8H4j = 633 # C8H4+
		self.krome_idx_C9H4j = 634 # C9H4+
		self.krome_idx_C4H7j = 635 # C4H7+
		self.krome_idx_HC4Nj = 636 # HC4N+
		self.krome_idx_HC4Oj = 637 # HC4O+
		self.krome_idx_C5Nj = 638 # C5N+
		self.krome_idx_H2C4Nj = 639 # H2C4N+
		self.krome_idx_H3C4Nj = 640 # H3C4N+
		self.krome_idx_C7Nj = 641 # C7N+
		self.krome_idx_C5H3Nj = 642 # C5H3N+
		self.krome_idx_C10H2j = 643 # C10H2+
		self.krome_idx_C9Nj = 644 # C9N+
		self.krome_idx_C7H3Nj = 645 # C7H3N+
		self.krome_idx_C9H3Nj = 646 # C9H3N+
		self.krome_idx_OCSjH2 = 647 # OCS+H2
		self.krome_idx_H2C3Oj = 648 # H2C3O+
		self.krome_idx_H3C3Oj = 649 # H3C3O+
		self.krome_idx_C5H4Nj = 650 # C5H4N+
		self.krome_idx_C8H5j = 651 # C8H5+
		self.krome_idx_C9H5j = 652 # C9H5+
		self.krome_idx_H2COHOCH2j = 653 # H2COHOCH2+
		self.krome_idx_H7C2O2j = 654 # H7C2O2+
		self.krome_idx_CR = 655 # CR
		self.krome_idx_g = 656 # g
		self.krome_idx_Tgas = 657 # Tgas
		self.krome_idx_dummy = 658 # dummy
		self.krome_names = (
						"E",
						"H-",
						"C-",
						"CN-",
						"O-",
						"OH-",
						"S-",
						"GRAIN-",
						"C",
						"CL",
						"FE",
						"H",
						"HE",
						"MG",
						"N",
						"NA",
						"O",
						"P",
						"S",
						"SI",
						"C2",
						"CCL",
						"CH",
						"CLO",
						"CN",
						"CO",
						"CP",
						"CS",
						"H2",
						"HCL",
						"HS",
						"MGH",
						"N2",
						"NAH",
						"NH",
						"NO",
						"NS",
						"O2",
						"OH",
						"PH",
						"PN",
						"PO",
						"S2",
						"SIC",
						"SIH",
						"SIN",
						"SIO",
						"SIS",
						"SO",
						"C2H",
						"C2N",
						"C2S",
						"C3",
						"CCO",
						"CCP",
						"CH2",
						"CO2",
						"H2O",
						"H2S",
						"HCN",
						"HCO",
						"HCP",
						"HCS",
						"HCSI",
						"HNC",
						"HNO",
						"HNSI",
						"HPO",
						"HS2",
						"N2O",
						"NAOH",
						"NH2",
						"NO2",
						"O2H",
						"OCN",
						"OCS",
						"PH2",
						"SIC2",
						"SIH2",
						"SINC",
						"SIO2",
						"SO2",
						"C2H2",
						"C3H",
						"C3N",
						"C3O",
						"C3P",
						"C3S",
						"C4",
						"CH3",
						"H2CO",
						"H2CS",
						"H2O2",
						"H2S2",
						"H2SIO",
						"HCCP",
						"NH3",
						"SIC2H",
						"SIC3",
						"SICH2",
						"SIH3",
						"C2H2N",
						"C2H2O",
						"C2H3",
						"C3H2",
						"C4H",
						"C4N",
						"C4P",
						"C4S",
						"C5",
						"CH2O2",
						"CH2PH",
						"CH3N",
						"CH4",
						"HC3N",
						"SIC2H2",
						"SIC3H",
						"SIC4",
						"SICH3",
						"SIH4",
						"C2H3N",
						"C2H4",
						"C3H3",
						"C4H2",
						"C5H",
						"C5N",
						"C6",
						"CH4O",
						"C2H4O",
						"C2H5",
						"C3H3N",
						"C3H4",
						"C5H2",
						"C6H",
						"C7",
						"CH5N",
						"HC5N",
						"C6H2",
						"C7H",
						"C7N",
						"C8",
						"CH3C3N",
						"HCOOCH3",
						"C2H5OH",
						"C7H2",
						"C8H",
						"C9",
						"CH3C4H",
						"CH3OCH3",
						"HC7N",
						"C2H6CO",
						"C8H2",
						"C9H",
						"C9N",
						"C10",
						"CH3C5N",
						"C9H2",
						"CH3C6H",
						"CH3C7N",
						"HC9N",
						"C4H4",
						"HCNC2",
						"HC2NC",
						"HNC3",
						"NH2CHO",
						"C4H3",
						"NH2CN",
						"C6H6",
						"H2CN",
						"GRAIN0",
						"O3",
						"FEH",
						"HNCO",
						"HC2O",
						"HCCN",
						"HC3O",
						"MGH2",
						"N2H2",
						"CHNH",
						"H2C3O",
						"H2C3N",
						"H2C5N",
						"H2C7N",
						"H2C9N",
						"NH2OH",
						"CH2OH",
						"C5H3",
						"H3C5N",
						"C6H3",
						"C7H3",
						"H3C7N",
						"C8H3",
						"C9H3",
						"H3C9N",
						"CH3NH",
						"H4C3N",
						"C5H4",
						"C6H4",
						"C7H4",
						"C8H4",
						"C9H4",
						"H5C3N",
						"C2H6",
						"C_DUST",
						"C2_DUST",
						"C3_DUST",
						"C2H_DUST",
						"C3H_DUST",
						"C2H3_DUST",
						"C3H3_DUST",
						"C2N_DUST",
						"C3N_DUST",
						"CCO_DUST",
						"C3O_DUST",
						"C2S_DUST",
						"C3S_DUST",
						"C4_DUST",
						"C4H_DUST",
						"C5_DUST",
						"C5H_DUST",
						"C6_DUST",
						"C6H_DUST",
						"C7_DUST",
						"C7H_DUST",
						"C8_DUST",
						"C8H_DUST",
						"C9_DUST",
						"C9H_DUST",
						"C10_DUST",
						"CH_DUST",
						"CH2_DUST",
						"C2H2_DUST",
						"CH3_DUST",
						"CN_DUST",
						"HS_DUST",
						"CS_DUST",
						"H_DUST",
						"N_DUST",
						"NH_DUST",
						"HNC_DUST",
						"NH2_DUST",
						"NO_DUST",
						"O_DUST",
						"OCN_DUST",
						"NS_DUST",
						"S_DUST",
						"CO_DUST",
						"O2_DUST",
						"OH_DUST",
						"SO_DUST",
						"C3H2_DUST",
						"C3H4_DUST",
						"C4H2_DUST",
						"C5H2_DUST",
						"C6H2_DUST",
						"C7H2_DUST",
						"C8H2_DUST",
						"C9H2_DUST",
						"C2H4_DUST",
						"HCCN_DUST",
						"HNO_DUST",
						"HCN_DUST",
						"CHNH_DUST",
						"CH3N_DUST",
						"HCO_DUST",
						"C2H5_DUST",
						"C2H2N_DUST",
						"CH2NH2_DUST",
						"H2CO_DUST",
						"CH3C3N_DUST",
						"C5N_DUST",
						"CH3C5N_DUST",
						"C7N_DUST",
						"CH3C7N_DUST",
						"CH2OH_DUST",
						"C2H5OH_DUST",
						"CH3OCH3_DUST",
						"C2H6_DUST",
						"C2H3N_DUST",
						"C2H4O_DUST",
						"CH4_DUST",
						"H2_DUST",
						"HC2O_DUST",
						"C3H3N_DUST",
						"H4C3N_DUST",
						"HC3N_DUST",
						"HC3O_DUST",
						"C4H3_DUST",
						"C4H4_DUST",
						"C5H3_DUST",
						"C5H4_DUST",
						"HC5N_DUST",
						"C6H3_DUST",
						"C6H4_DUST",
						"C7H3_DUST",
						"C7H4_DUST",
						"HC7N_DUST",
						"C8H3_DUST",
						"C8H4_DUST",
						"C9H3_DUST",
						"C9H4_DUST",
						"C9N_DUST",
						"HC9N_DUST",
						"CH3NH_DUST",
						"CH5N_DUST",
						"CH4O_DUST",
						"HCS_DUST",
						"FE_DUST",
						"FEH_DUST",
						"H2C3N_DUST",
						"H2C5N_DUST",
						"H3C5N_DUST",
						"H2C7N_DUST",
						"H3C7N_DUST",
						"H2C9N_DUST",
						"H3C9N_DUST",
						"H2CN_DUST",
						"H2O2_DUST",
						"H2O_DUST",
						"O2H_DUST",
						"H2S_DUST",
						"H5C3N_DUST",
						"C2H2O_DUST",
						"H2C3O_DUST",
						"H2CS_DUST",
						"MG_DUST",
						"MGH_DUST",
						"MGH2_DUST",
						"N2H2_DUST",
						"N2_DUST",
						"NA_DUST",
						"NAH_DUST",
						"NH3_DUST",
						"O3_DUST",
						"HNCO_DUST",
						"OCS_DUST",
						"SI_DUST",
						"SIH_DUST",
						"SIH2_DUST",
						"SIH3_DUST",
						"SIH4_DUST",
						"SO2_DUST",
						"HCOOCH3_DUST",
						"NH2CHO_DUST",
						"CO2_DUST",
						"CH2O2_DUST",
						"NH2OH_DUST",
						"C4N_DUST",
						"C4S_DUST",
						"C6H6_DUST",
						"CH2NH2",
						"CH3C4H_DUST",
						"CH3C6H_DUST",
						"H2S2_DUST",
						"HC2NC_DUST",
						"HCNC2_DUST",
						"HE_DUST",
						"HNC3_DUST",
						"HS2_DUST",
						"NAOH_DUST",
						"S2_DUST",
						"SIC_DUST",
						"SIO_DUST",
						"SIS_DUST",
						"C2H6CO_DUST",
						"C3P_DUST",
						"CCP_DUST",
						"C4P_DUST",
						"CCL_DUST",
						"CL_DUST",
						"P_DUST",
						"CP_DUST",
						"CH2PH_DUST",
						"HCP_DUST",
						"CLO_DUST",
						"H2SIO_DUST",
						"HCCP_DUST",
						"HCL_DUST",
						"HCSI_DUST",
						"HNSI_DUST",
						"SIN_DUST",
						"HPO_DUST",
						"PO_DUST",
						"N2O_DUST",
						"NH2CN_DUST",
						"NO2_DUST",
						"PH_DUST",
						"PH2_DUST",
						"PN_DUST",
						"SIC2_DUST",
						"SIC2H_DUST",
						"SIC2H2_DUST",
						"SIC3_DUST",
						"SIC3H_DUST",
						"SIC4_DUST",
						"SICH2_DUST",
						"SICH3_DUST",
						"SINC_DUST",
						"SIO2_DUST",
						"C+",
						"CL+",
						"FE+",
						"H+",
						"HE+",
						"MG+",
						"N+",
						"NA+",
						"O+",
						"P+",
						"S+",
						"SI+",
						"CO+",
						"H2+",
						"NO+",
						"O2+",
						"CH2+",
						"H2S+",
						"HCO+",
						"HCS+",
						"HNO+",
						"NH2+",
						"OCS+",
						"C2H2+",
						"CH3+",
						"NH3+",
						"C2H2O+",
						"CH2O2+",
						"C2H3N+",
						"C2H4+",
						"C4H2+",
						"H3CO+",
						"CH4O+",
						"C2H4O+",
						"C3H4+",
						"CH5N+",
						"C2H5OH+",
						"CH3OCH3+",
						"CH+",
						"CCL+",
						"C2+",
						"CLO+",
						"CP+",
						"CS+",
						"CN+",
						"NS+",
						"PH+",
						"PO+",
						"SIC+",
						"SIN+",
						"SIS+",
						"SO+",
						"C3+",
						"C2S+",
						"C2O+",
						"CCP+",
						"C2H+",
						"HOC+",
						"C2N+",
						"CNC+",
						"HCP+",
						"SIC2+",
						"SINC+",
						"HPO+",
						"HCN+",
						"CHSI+",
						"SIH2+",
						"C3H+",
						"C4+",
						"C3O+",
						"C3S+",
						"H2CO+",
						"H2SIO+",
						"HCNH+",
						"SIC2H+",
						"SIC3+",
						"CH2SI+",
						"SIH3+",
						"C2H2N+",
						"C2H3+",
						"C3H2+",
						"H2C3+",
						"C4H+",
						"C5+",
						"C4S+",
						"PC2H+",
						"C3N+",
						"C4N+",
						"C3HN+",
						"HNC+",
						"SIC3H+",
						"SIC4+",
						"SIC2H2+",
						"SICH3+",
						"HC2NCH+",
						"C3H3+",
						"H3C3+",
						"C5H+",
						"C6+",
						"C2H3O+",
						"C2H5+",
						"C3H3N+",
						"C5H2+",
						"C4H3+",
						"C6H+",
						"C7+",
						"CH4N+",
						"C5HN+",
						"C7H+",
						"C8+",
						"COOCH4+",
						"C2H5O+",
						"C8H+",
						"C9+",
						"C5H3+",
						"C6H2+",
						"C6H3+",
						"C2H6CO+",
						"C9H+",
						"C10+",
						"C7H3+",
						"C8H2+",
						"C8H3+",
						"HCL+",
						"HS+",
						"NH+",
						"OH+",
						"PN+",
						"S2+",
						"SIH+",
						"SIO+",
						"H2O+",
						"HNSI+",
						"S2H+",
						"PH2+",
						"H2CS+",
						"H2S2+",
						"HSIO+",
						"C4P+",
						"HCO2+",
						"PCH3+",
						"CH4+",
						"C2NH+",
						"SIH4+",
						"NH4+",
						"H2NC+",
						"C3H2N+",
						"C7H2+",
						"C5H4+",
						"C7HN+",
						"C9H2+",
						"C7H4+",
						"C9HN+",
						"N2+",
						"CO2+",
						"HEH+",
						"SO2+",
						"C6H5+",
						"C5H5+",
						"N2H+",
						"NO2+",
						"PC2H2+",
						"PNH2+",
						"PCH2+",
						"HC2S+",
						"HC3S+",
						"H3CS+",
						"HC4S+",
						"SINH2+",
						"SIC2H3+",
						"SIC3H2+",
						"C2HO+",
						"H3O+",
						"H3S+",
						"HOCS+",
						"CH5O+",
						"NCO+",
						"HNCO+",
						"C2N2+",
						"H3+",
						"O2H+",
						"CH5+",
						"H2CL+",
						"CH3O2+",
						"H2PO+",
						"PNH3+",
						"PCH4+",
						"PC2H3+",
						"HSIS+",
						"HSO+",
						"HNS+",
						"HPN+",
						"H2NO+",
						"NAH2O+",
						"PH3+",
						"SINCH+",
						"HSIO2+",
						"HSO2+",
						"HC3O+",
						"PC3H+",
						"H3S2+",
						"H3SIO+",
						"PC4H+",
						"NH2CNH+",
						"SIC4H+",
						"SICH4+",
						"SIH5+",
						"C2H4N+",
						"NH2CH2O+",
						"C2H6+",
						"C3H4N+",
						"C3H5+",
						"C4H4+",
						"CH6N+",
						"C5H2N+",
						"C4H4N+",
						"H5C2O2+",
						"C2H5OH2+",
						"CH3OCH4+",
						"C7H2N+",
						"C3H6OH+",
						"C6H4N+",
						"C10H+",
						"C9H3+",
						"C7H5+",
						"C8H4N+",
						"C9H2N+",
						"C6H7+",
						"NAH2+",
						"PC2H4+",
						"C4H5+",
						"H2CCL+",
						"PC4H2+",
						"C6H4+",
						"C8H4+",
						"C9H4+",
						"C4H7+",
						"HC4N+",
						"HC4O+",
						"C5N+",
						"H2C4N+",
						"H3C4N+",
						"C7N+",
						"C5H3N+",
						"C10H2+",
						"C9N+",
						"C7H3N+",
						"C9H3N+",
						"OCS+H2",
						"H2C3O+",
						"H3C3O+",
						"C5H4N+",
						"C8H5+",
						"C9H5+",
						"H2COHOCH2+",
						"H7C2O2+",
						"CR",
						"g",
						"Tgas",
						"dummy"
						)

		self.krome_idx_cool_h2 = 0
		self.krome_idx_cool_h2gp = 1
		self.krome_idx_cool_atomic = 2
		self.krome_idx_cool_cen = 2
		self.krome_idx_cool_hd = 3
		self.krome_idx_cool_z = 4
		self.krome_idx_cool_metal = 4
		self.krome_idx_cool_dh = 5
		self.krome_idx_cool_enthalpic = 5
		self.krome_idx_cool_dust = 6
		self.krome_idx_cool_compton = 7
		self.krome_idx_cool_cie = 8
		self.krome_idx_cool_continuum = 9
		self.krome_idx_cool_cont = 9
		self.krome_idx_cool_exp = 10
		self.krome_idx_cool_expansion = 10
		self.krome_idx_cool_ff = 11
		self.krome_idx_cool_bss = 11
		self.krome_idx_cool_custom = 12
		self.krome_idx_cool_co = 13
		self.krome_idx_cool_zcie = 14
		self.krome_idx_cool_zcienouv = 15
		self.krome_idx_cool_zextend = 16
		self.krome_idx_cool_gh = 17
		self.krome_idx_cool_oh = 18
		self.krome_idx_cool_h2o = 19
		self.krome_idx_cool_hcn = 20
		self.krome_ncools = 21

		self.krome_idx_heat_chem = 0
		self.krome_idx_heat_compress = 1
		self.krome_idx_heat_compr = 1
		self.krome_idx_heat_photo = 2
		self.krome_idx_heat_dh = 3
		self.krome_idx_heat_enthalpic = 3
		self.krome_idx_heat_photoav = 4
		self.krome_idx_heat_av = 4
		self.krome_idx_heat_cr = 5
		self.krome_idx_heat_dust = 6
		self.krome_idx_heat_xray = 7
		self.krome_idx_heat_visc = 8
		self.krome_idx_heat_viscous = 8
		self.krome_idx_heat_custom = 9
		self.krome_idx_heat_zcie = 10
		self.krome_nheats = 11

		self.krome_nrea = 5639
		self.krome_nmols = 655
		self.krome_nspec = 659
		self.krome_natoms = 16
		self.krome_ndust = 0
		self.krome_ndustTypes = 0
		self.krome_nPhotoBins = 0
		self.krome_nPhotoRates = 0

		self.krome_boltzmann_eV = 8.617332478e-5 # eV / K
		self.krome_boltzmann_J = 1.380648e-23 # J / K
		self.krome_boltzmann_erg = 1.380648e-16 # erg / K
		self.krome_iboltzmann_eV = 1e0/self.krome_boltzmann_eV # K / eV
		self.krome_iboltzmann_erg = 1e0/self.krome_boltzmann_erg # K / erg
		self.krome_planck_eV = 4.135667516e-15 # eV s
		self.krome_planck_J = 6.62606957e-34 # J s
		self.krome_planck_erg = 6.62606957e-27 # erg s
		self.krome_iplanck_eV = 1e0/self.krome_planck_eV # 1 / eV / s
		self.krome_iplanck_J = 1e0/self.krome_planck_J # 1 / J / s
		self.krome_iplanck_erg = 1e0/self.krome_planck_erg # 1 / erg / s
		self.krome_gravity = 6.674e-8 # cm3 / g / s2
		self.krome_e_mass = 9.10938188e-28 # g
		self.krome_p_mass = 1.67262158e-24 # g
		self.krome_n_mass = 1.674920e-24 # g
		self.krome_ip_mass = 1e0/self.krome_p_mass # 1/g
		self.krome_clight = 2.99792458e10 # cm/s
		self.krome_pi = 3.14159265359e0 # #
		self.krome_eV_to_erg = 1.60217646e-12 # eV -> erg
		self.krome_ry_to_eV = 13.60569e0 # rydberg -> eV
		self.krome_ry_to_erg = 2.179872e-11 # rydberg -> erg
		self.krome_seconds_per_year = 365e0*24e0*3600e0 # yr -> s
		self.krome_km_to_cm = 1e5 # km -> cm
		self.krome_cm_to_Mpc = 1.e0/3.08e24 # cm -> Mpc
		self.krome_kvgas_erg = 8.e0*self.krome_boltzmann_erg/self.krome_pi/self.krome_p_mass # 
		self.krome_pre_kvgas_sqrt = np.sqrt(8.e0*self.krome_boltzmann_erg/self.krome_pi) # 
		self.krome_pre_planck = 2.e0*self.krome_planck_erg/self.krome_clight**2 # erg/cm2*s3
		self.krome_exp_planck = self.krome_planck_erg / self.krome_boltzmann_erg # s*K
		self.krome_stefboltz_erg = 5.670373e-5 # erg/s/cm2/K4
		self.krome_N_avogadro = 6.0221e23 # #
		self.krome_Rgas_J = 8.3144621e0 # J/K/mol
		self.krome_Rgas_kJ = 8.3144621e-3 # kJ/K/mol
		self.krome_hubble = 0.704e0 # dimensionless
		self.krome_Omega0 = 1.0e0 # dimensionless
		self.krome_Omegab = 0.0456e0 # dimensionless
		self.krome_Hubble0 = 1.e2*self.krome_hubble*self.krome_km_to_cm*self.krome_cm_to_Mpc # 1/s

		fortran.krome_set_user_tdust.restype = None
		fortran.krome_set_user_tdust.argtypes = [ctypes.c_double]
		fortran.krome_get_user_tdust.restype = ctypes.c_double
		fortran.krome_get_user_tdust.argtypes = None
		fortran.krome_set_user_xdust.restype = None
		fortran.krome_set_user_xdust.argtypes = [ctypes.c_double]
		fortran.krome_get_user_xdust.restype = ctypes.c_double
		fortran.krome_get_user_xdust.argtypes = None
		fortran.krome_set_user_gsize.restype = None
		fortran.krome_set_user_gsize.argtypes = [ctypes.c_double]
		fortran.krome_get_user_gsize.restype = ctypes.c_double
		fortran.krome_get_user_gsize.argtypes = None
		fortran.krome_set_user_gsize2.restype = None
		fortran.krome_set_user_gsize2.argtypes = [ctypes.c_double]
		fortran.krome_get_user_gsize2.restype = ctypes.c_double
		fortran.krome_get_user_gsize2.argtypes = None
		fortran.krome_set_user_crflux.restype = None
		fortran.krome_set_user_crflux.argtypes = [ctypes.c_double]
		fortran.krome_get_user_crflux.restype = ctypes.c_double
		fortran.krome_get_user_crflux.argtypes = None
		fortran.krome_set_user_av.restype = None
		fortran.krome_set_user_av.argtypes = [ctypes.c_double]
		fortran.krome_get_user_av.restype = ctypes.c_double
		fortran.krome_get_user_av.argtypes = None

		fortran.krome_set_tcmb.restype = None
		fortran.krome_set_tcmb.argtypes = [ctypes.c_double]
		fortran.krome_get_tcmb.restype = ctypes.c_double
		fortran.krome_get_tcmb.argtypes = None
		fortran.krome_set_zredshift.restype = None
		fortran.krome_set_zredshift.argtypes = [ctypes.c_double]
		fortran.krome_get_zredshift.restype = ctypes.c_double
		fortran.krome_get_zredshift.argtypes = None
		fortran.krome_set_orthopararatio.restype = None
		fortran.krome_set_orthopararatio.argtypes = [ctypes.c_double]
		fortran.krome_get_orthopararatio.restype = ctypes.c_double
		fortran.krome_get_orthopararatio.argtypes = None
		fortran.krome_set_metallicity.restype = None
		fortran.krome_set_metallicity.argtypes = [ctypes.c_double]
		fortran.krome_get_metallicity.restype = ctypes.c_double
		fortran.krome_get_metallicity.argtypes = None
		fortran.krome_set_tfloor.restype = None
		fortran.krome_set_tfloor.argtypes = [ctypes.c_double]
		fortran.krome_get_tfloor.restype = ctypes.c_double
		fortran.krome_get_tfloor.argtypes = None


		# krome.f90 argument and return (result) types
		fortran.krome.restype = None
		fortran.krome_equilibrium.restype = None
		fortran.krome.argtypes = [array_1d_double, dble_byref, dble_byref]
		fortran.krome_equilibrium.argtypes = [array_1d_double, dble_byref]
		fortran.krome.argtypes.append(int_byref)
		fortran.krome_init.restype = None
		fortran.krome_init.argtypes = None
		fortran.krome_get_coe.restype = array_1d_double
		fortran.krome_get_coe.argtypes = [array_1d_double, dble_byref]
		fortran.krome_get_coet.restype = array_1d_double
		fortran.krome_get_coet.argtypes = [dble_byref]
	
		#krome_user.f90 argument and return (result) types
		fortran.krome_get_table_tdust.restype = None
		fortran.krome_get_table_tdust.argtypes = [array_1d_double, dble_byref]
		fortran.krome_num2col.restype = dble_byref
		fortran.krome_num2col.argtypes = [ctypes.c_double, array_1d_double, ctypes.c_double]
		fortran.krome_print_phys_variables.restype = None
		fortran.krome_print_phys_variables.argtypes = None
		fortran.krome_store.restype = None
		fortran.krome_store.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_double]
		fortran.krome_restore.restype = None
		fortran.krome_restore.argtypes = [array_1d_double, dble_byref, dble_byref]
		fortran.krome_thermo_on.restype = None
		fortran.krome_thermo_on.argtypes = None
		fortran.krome_thermo_off.restype = None
		fortran.krome_thermo_off.argtypes = None
		fortran.krome_get_coef.restype = array_1d_double
		fortran.krome_get_coef.argtypes = [ctypes.c_double]
		fortran.krome_get_mu_x.restype = ctypes.c_double
		fortran.krome_get_mu_x.argtypes = [array_1d_double]
		fortran.krome_get_gamma_x.restype = ctypes.c_double
		fortran.krome_get_gamma_x.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_consistent_x.restype = None
		fortran.krome_consistent_x.argtypes = [array_1d_double]
		fortran.krome_n2x.restype = array_1d_double
		fortran.krome_n2x.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_x2n.restype = array_1d_double
		fortran.krome_x2n.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_thermo.restype = None
		fortran.krome_thermo.argtypes = [array_1d_double, array_1d_double, ctypes.c_double]
		fortran.krome_get_heating.restype = ctypes.c_double
		fortran.krome_get_heating.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_get_heating_array.restype = array_1d_double
		fortran.krome_get_heating_array.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_conservelin_x.restype = None
		fortran.krome_conservelin_x.argtypes = [array_1d_double, array_1d_double]
		fortran.krome_conservelingetref_x.restype = ctypes.c_double
		fortran.krome_conservelingetref_x.argtypes = [array_1d_double]
		fortran.krome_conserve.restype = array_1d_double
		fortran.krome_conserve.argtypes = [array_1d_double, array_1d_double]
		fortran.krome_get_gamma.restype = ctypes.c_double
		fortran.krome_get_gamma.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_get_zatoms.restype = ctypes.c_int
		fortran.krome_get_zatoms.argtypes = None
		fortran.krome_get_mu.restype = ctypes.c_double
		fortran.krome_get_mu.argtypes = [array_1d_double]
		## extern char **krome_get_rnames();
		fortran.krome_get_mass.restype = array_1d_double
		fortran.krome_get_mass.argtypes = None
		fortran.krome_get_imass.restype = array_1d_double
		fortran.krome_get_imass.argtypes = None
		fortran.krome_get_hnuclei.restype = ctypes.c_double
		fortran.krome_get_hnuclei.argtypes = [array_1d_double]
		fortran.krome_get_charges.restype = array_1d_double
		fortran.krome_get_charges.argtypes = None
		## extern char **krome_get_names();
		##extern int krome_get_index(char *name);
		fortran.krome_get_rho.restype = ctypes.c_double
		fortran.krome_get_rho.argtypes = [array_1d_double]
		fortran.krome_scale_z.restype = None
		fortran.krome_scale_z.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_set_z.restype = None
		fortran.krome_set_z.argtypes = [ctypes.c_double]
		fortran.krome_set_clump.restype = None
		fortran.krome_set_clump.argtypes = [ctypes.c_double]
		fortran.krome_get_electrons.restype = ctypes.c_double
		fortran.krome_get_electrons.argtypes = [array_1d_double]
		fortran.krome_print_best_flux.restype = None
		fortran.krome_print_best_flux.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_int]
		fortran.krome_print_best_flux_frac.restype = None
		fortran.krome_print_best_flux_frac.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_double]
		fortran.krome_print_best_flux_spec.restype = None
		fortran.krome_print_best_flux_spec.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_int, ctypes.c_int]
		fortran.krome_get_flux.restype = array_1d_double
		fortran.krome_get_flux.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_explore_flux.restype = None
		fortran.krome_explore_flux.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_int, ctypes.c_double]
		fortran.krome_get_qeff.restype = array_1d_double
		fortran.krome_get_qeff.argtypes = None
		fortran.krome_dump_flux.restype = None
		fortran.krome_dump_flux.argtypes = [array_1d_double, ctypes.c_double, ctypes.c_int]
		fortran.krome_dump_rates.restype = None
		fortran.krome_dump_rates.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_int, ctypes.c_int]
		fortran.krome_get_info.restype = None
		fortran.krome_get_info.argtypes = [array_1d_double, ctypes.c_double]
		fortran.krome_get_jacobian.restype = None
		fortran.krome_get_jacobian.argtypes = [ctypes.c_int, array_1d_double, ctypes.c_double, array_1d_double]

