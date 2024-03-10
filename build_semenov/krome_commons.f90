
!############### MODULE ##############
module krome_commons
  implicit none

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2024-03-04 18:23:34
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
  integer,parameter::idx_E=1
  integer,parameter::idx_Hk=2
  integer,parameter::idx_Ck=3
  integer,parameter::idx_CNk=4
  integer,parameter::idx_Ok=5
  integer,parameter::idx_OHk=6
  integer,parameter::idx_Sk=7
  integer,parameter::idx_GRAINk=8
  integer,parameter::idx_C=9
  integer,parameter::idx_CL=10
  integer,parameter::idx_FE=11
  integer,parameter::idx_H=12
  integer,parameter::idx_HE=13
  integer,parameter::idx_MG=14
  integer,parameter::idx_N=15
  integer,parameter::idx_NA=16
  integer,parameter::idx_O=17
  integer,parameter::idx_P=18
  integer,parameter::idx_S=19
  integer,parameter::idx_SI=20
  integer,parameter::idx_C2=21
  integer,parameter::idx_CCL=22
  integer,parameter::idx_CH=23
  integer,parameter::idx_CLO=24
  integer,parameter::idx_CN=25
  integer,parameter::idx_CO=26
  integer,parameter::idx_CP=27
  integer,parameter::idx_CS=28
  integer,parameter::idx_H2=29
  integer,parameter::idx_HCL=30
  integer,parameter::idx_HS=31
  integer,parameter::idx_MGH=32
  integer,parameter::idx_N2=33
  integer,parameter::idx_NAH=34
  integer,parameter::idx_NH=35
  integer,parameter::idx_NO=36
  integer,parameter::idx_NS=37
  integer,parameter::idx_O2=38
  integer,parameter::idx_OH=39
  integer,parameter::idx_PH=40
  integer,parameter::idx_PN=41
  integer,parameter::idx_PO=42
  integer,parameter::idx_S2=43
  integer,parameter::idx_SIC=44
  integer,parameter::idx_SIH=45
  integer,parameter::idx_SIN=46
  integer,parameter::idx_SIO=47
  integer,parameter::idx_SIS=48
  integer,parameter::idx_SO=49
  integer,parameter::idx_C2H=50
  integer,parameter::idx_C2N=51
  integer,parameter::idx_C2S=52
  integer,parameter::idx_C3=53
  integer,parameter::idx_CCO=54
  integer,parameter::idx_CCP=55
  integer,parameter::idx_CH2=56
  integer,parameter::idx_CO2=57
  integer,parameter::idx_H2O=58
  integer,parameter::idx_H2S=59
  integer,parameter::idx_HCN=60
  integer,parameter::idx_HCO=61
  integer,parameter::idx_HCP=62
  integer,parameter::idx_HCS=63
  integer,parameter::idx_HCSI=64
  integer,parameter::idx_HNC=65
  integer,parameter::idx_HNO=66
  integer,parameter::idx_HNSI=67
  integer,parameter::idx_HPO=68
  integer,parameter::idx_HS2=69
  integer,parameter::idx_N2O=70
  integer,parameter::idx_NAOH=71
  integer,parameter::idx_NH2=72
  integer,parameter::idx_NO2=73
  integer,parameter::idx_O2H=74
  integer,parameter::idx_OCN=75
  integer,parameter::idx_OCS=76
  integer,parameter::idx_PH2=77
  integer,parameter::idx_SIC2=78
  integer,parameter::idx_SIH2=79
  integer,parameter::idx_SINC=80
  integer,parameter::idx_SIO2=81
  integer,parameter::idx_SO2=82
  integer,parameter::idx_C2H2=83
  integer,parameter::idx_C3H=84
  integer,parameter::idx_C3N=85
  integer,parameter::idx_C3O=86
  integer,parameter::idx_C3P=87
  integer,parameter::idx_C3S=88
  integer,parameter::idx_C4=89
  integer,parameter::idx_CH3=90
  integer,parameter::idx_H2CO=91
  integer,parameter::idx_H2CS=92
  integer,parameter::idx_H2O2=93
  integer,parameter::idx_H2S2=94
  integer,parameter::idx_H2SIO=95
  integer,parameter::idx_HCCP=96
  integer,parameter::idx_NH3=97
  integer,parameter::idx_SIC2H=98
  integer,parameter::idx_SIC3=99
  integer,parameter::idx_SICH2=100
  integer,parameter::idx_SIH3=101
  integer,parameter::idx_C2H2N=102
  integer,parameter::idx_C2H2O=103
  integer,parameter::idx_C2H3=104
  integer,parameter::idx_C3H2=105
  integer,parameter::idx_C4H=106
  integer,parameter::idx_C4N=107
  integer,parameter::idx_C4P=108
  integer,parameter::idx_C4S=109
  integer,parameter::idx_C5=110
  integer,parameter::idx_CH2O2=111
  integer,parameter::idx_CH2PH=112
  integer,parameter::idx_CH3N=113
  integer,parameter::idx_CH4=114
  integer,parameter::idx_HC3N=115
  integer,parameter::idx_SIC2H2=116
  integer,parameter::idx_SIC3H=117
  integer,parameter::idx_SIC4=118
  integer,parameter::idx_SICH3=119
  integer,parameter::idx_SIH4=120
  integer,parameter::idx_C2H3N=121
  integer,parameter::idx_C2H4=122
  integer,parameter::idx_C3H3=123
  integer,parameter::idx_C4H2=124
  integer,parameter::idx_C5H=125
  integer,parameter::idx_C5N=126
  integer,parameter::idx_C6=127
  integer,parameter::idx_CH4O=128
  integer,parameter::idx_C2H4O=129
  integer,parameter::idx_C2H5=130
  integer,parameter::idx_C3H3N=131
  integer,parameter::idx_C3H4=132
  integer,parameter::idx_C5H2=133
  integer,parameter::idx_C6H=134
  integer,parameter::idx_C7=135
  integer,parameter::idx_CH5N=136
  integer,parameter::idx_HC5N=137
  integer,parameter::idx_C6H2=138
  integer,parameter::idx_C7H=139
  integer,parameter::idx_C7N=140
  integer,parameter::idx_C8=141
  integer,parameter::idx_CH3C3N=142
  integer,parameter::idx_HCOOCH3=143
  integer,parameter::idx_C2H5OH=144
  integer,parameter::idx_C7H2=145
  integer,parameter::idx_C8H=146
  integer,parameter::idx_C9=147
  integer,parameter::idx_CH3C4H=148
  integer,parameter::idx_CH3OCH3=149
  integer,parameter::idx_HC7N=150
  integer,parameter::idx_C2H6CO=151
  integer,parameter::idx_C8H2=152
  integer,parameter::idx_C9H=153
  integer,parameter::idx_C9N=154
  integer,parameter::idx_C10=155
  integer,parameter::idx_CH3C5N=156
  integer,parameter::idx_C9H2=157
  integer,parameter::idx_CH3C6H=158
  integer,parameter::idx_CH3C7N=159
  integer,parameter::idx_HC9N=160
  integer,parameter::idx_C4H4=161
  integer,parameter::idx_HCNC2=162
  integer,parameter::idx_HC2NC=163
  integer,parameter::idx_HNC3=164
  integer,parameter::idx_NH2CHO=165
  integer,parameter::idx_C4H3=166
  integer,parameter::idx_NH2CN=167
  integer,parameter::idx_C6H6=168
  integer,parameter::idx_H2CN=169
  integer,parameter::idx_GRAIN0=170
  integer,parameter::idx_O3=171
  integer,parameter::idx_FEH=172
  integer,parameter::idx_HNCO=173
  integer,parameter::idx_HC2O=174
  integer,parameter::idx_HCCN=175
  integer,parameter::idx_HC3O=176
  integer,parameter::idx_MGH2=177
  integer,parameter::idx_N2H2=178
  integer,parameter::idx_CHNH=179
  integer,parameter::idx_H2C3O=180
  integer,parameter::idx_H2C3N=181
  integer,parameter::idx_H2C5N=182
  integer,parameter::idx_H2C7N=183
  integer,parameter::idx_H2C9N=184
  integer,parameter::idx_NH2OH=185
  integer,parameter::idx_CH2OH=186
  integer,parameter::idx_C5H3=187
  integer,parameter::idx_H3C5N=188
  integer,parameter::idx_C6H3=189
  integer,parameter::idx_C7H3=190
  integer,parameter::idx_H3C7N=191
  integer,parameter::idx_C8H3=192
  integer,parameter::idx_C9H3=193
  integer,parameter::idx_H3C9N=194
  integer,parameter::idx_CH3NH=195
  integer,parameter::idx_H4C3N=196
  integer,parameter::idx_C5H4=197
  integer,parameter::idx_C6H4=198
  integer,parameter::idx_C7H4=199
  integer,parameter::idx_C8H4=200
  integer,parameter::idx_C9H4=201
  integer,parameter::idx_H5C3N=202
  integer,parameter::idx_C2H6=203
  integer,parameter::idx_C_DUST=204
  integer,parameter::idx_C2_DUST=205
  integer,parameter::idx_C3_DUST=206
  integer,parameter::idx_C2H_DUST=207
  integer,parameter::idx_C3H_DUST=208
  integer,parameter::idx_C2H3_DUST=209
  integer,parameter::idx_C3H3_DUST=210
  integer,parameter::idx_C2N_DUST=211
  integer,parameter::idx_C3N_DUST=212
  integer,parameter::idx_CCO_DUST=213
  integer,parameter::idx_C3O_DUST=214
  integer,parameter::idx_C2S_DUST=215
  integer,parameter::idx_C3S_DUST=216
  integer,parameter::idx_C4_DUST=217
  integer,parameter::idx_C4H_DUST=218
  integer,parameter::idx_C5_DUST=219
  integer,parameter::idx_C5H_DUST=220
  integer,parameter::idx_C6_DUST=221
  integer,parameter::idx_C6H_DUST=222
  integer,parameter::idx_C7_DUST=223
  integer,parameter::idx_C7H_DUST=224
  integer,parameter::idx_C8_DUST=225
  integer,parameter::idx_C8H_DUST=226
  integer,parameter::idx_C9_DUST=227
  integer,parameter::idx_C9H_DUST=228
  integer,parameter::idx_C10_DUST=229
  integer,parameter::idx_CH_DUST=230
  integer,parameter::idx_CH2_DUST=231
  integer,parameter::idx_C2H2_DUST=232
  integer,parameter::idx_CH3_DUST=233
  integer,parameter::idx_CN_DUST=234
  integer,parameter::idx_HS_DUST=235
  integer,parameter::idx_CS_DUST=236
  integer,parameter::idx_H_DUST=237
  integer,parameter::idx_N_DUST=238
  integer,parameter::idx_NH_DUST=239
  integer,parameter::idx_HNC_DUST=240
  integer,parameter::idx_NH2_DUST=241
  integer,parameter::idx_NO_DUST=242
  integer,parameter::idx_O_DUST=243
  integer,parameter::idx_OCN_DUST=244
  integer,parameter::idx_NS_DUST=245
  integer,parameter::idx_S_DUST=246
  integer,parameter::idx_CO_DUST=247
  integer,parameter::idx_O2_DUST=248
  integer,parameter::idx_OH_DUST=249
  integer,parameter::idx_SO_DUST=250
  integer,parameter::idx_C3H2_DUST=251
  integer,parameter::idx_C3H4_DUST=252
  integer,parameter::idx_C4H2_DUST=253
  integer,parameter::idx_C5H2_DUST=254
  integer,parameter::idx_C6H2_DUST=255
  integer,parameter::idx_C7H2_DUST=256
  integer,parameter::idx_C8H2_DUST=257
  integer,parameter::idx_C9H2_DUST=258
  integer,parameter::idx_C2H4_DUST=259
  integer,parameter::idx_HCCN_DUST=260
  integer,parameter::idx_HNO_DUST=261
  integer,parameter::idx_HCN_DUST=262
  integer,parameter::idx_CHNH_DUST=263
  integer,parameter::idx_CH3N_DUST=264
  integer,parameter::idx_HCO_DUST=265
  integer,parameter::idx_C2H5_DUST=266
  integer,parameter::idx_C2H2N_DUST=267
  integer,parameter::idx_CH2NH2_DUST=268
  integer,parameter::idx_H2CO_DUST=269
  integer,parameter::idx_CH3C3N_DUST=270
  integer,parameter::idx_C5N_DUST=271
  integer,parameter::idx_CH3C5N_DUST=272
  integer,parameter::idx_C7N_DUST=273
  integer,parameter::idx_CH3C7N_DUST=274
  integer,parameter::idx_CH2OH_DUST=275
  integer,parameter::idx_C2H5OH_DUST=276
  integer,parameter::idx_CH3OCH3_DUST=277
  integer,parameter::idx_C2H6_DUST=278
  integer,parameter::idx_C2H3N_DUST=279
  integer,parameter::idx_C2H4O_DUST=280
  integer,parameter::idx_CH4_DUST=281
  integer,parameter::idx_H2_DUST=282
  integer,parameter::idx_HC2O_DUST=283
  integer,parameter::idx_C3H3N_DUST=284
  integer,parameter::idx_H4C3N_DUST=285
  integer,parameter::idx_HC3N_DUST=286
  integer,parameter::idx_HC3O_DUST=287
  integer,parameter::idx_C4H3_DUST=288
  integer,parameter::idx_C4H4_DUST=289
  integer,parameter::idx_C5H3_DUST=290
  integer,parameter::idx_C5H4_DUST=291
  integer,parameter::idx_HC5N_DUST=292
  integer,parameter::idx_C6H3_DUST=293
  integer,parameter::idx_C6H4_DUST=294
  integer,parameter::idx_C7H3_DUST=295
  integer,parameter::idx_C7H4_DUST=296
  integer,parameter::idx_HC7N_DUST=297
  integer,parameter::idx_C8H3_DUST=298
  integer,parameter::idx_C8H4_DUST=299
  integer,parameter::idx_C9H3_DUST=300
  integer,parameter::idx_C9H4_DUST=301
  integer,parameter::idx_C9N_DUST=302
  integer,parameter::idx_HC9N_DUST=303
  integer,parameter::idx_CH3NH_DUST=304
  integer,parameter::idx_CH5N_DUST=305
  integer,parameter::idx_CH4O_DUST=306
  integer,parameter::idx_HCS_DUST=307
  integer,parameter::idx_FE_DUST=308
  integer,parameter::idx_FEH_DUST=309
  integer,parameter::idx_H2C3N_DUST=310
  integer,parameter::idx_H2C5N_DUST=311
  integer,parameter::idx_H3C5N_DUST=312
  integer,parameter::idx_H2C7N_DUST=313
  integer,parameter::idx_H3C7N_DUST=314
  integer,parameter::idx_H2C9N_DUST=315
  integer,parameter::idx_H3C9N_DUST=316
  integer,parameter::idx_H2CN_DUST=317
  integer,parameter::idx_H2O2_DUST=318
  integer,parameter::idx_H2O_DUST=319
  integer,parameter::idx_O2H_DUST=320
  integer,parameter::idx_H2S_DUST=321
  integer,parameter::idx_H5C3N_DUST=322
  integer,parameter::idx_C2H2O_DUST=323
  integer,parameter::idx_H2C3O_DUST=324
  integer,parameter::idx_H2CS_DUST=325
  integer,parameter::idx_MG_DUST=326
  integer,parameter::idx_MGH_DUST=327
  integer,parameter::idx_MGH2_DUST=328
  integer,parameter::idx_N2H2_DUST=329
  integer,parameter::idx_N2_DUST=330
  integer,parameter::idx_NA_DUST=331
  integer,parameter::idx_NAH_DUST=332
  integer,parameter::idx_NH3_DUST=333
  integer,parameter::idx_O3_DUST=334
  integer,parameter::idx_HNCO_DUST=335
  integer,parameter::idx_OCS_DUST=336
  integer,parameter::idx_SI_DUST=337
  integer,parameter::idx_SIH_DUST=338
  integer,parameter::idx_SIH2_DUST=339
  integer,parameter::idx_SIH3_DUST=340
  integer,parameter::idx_SIH4_DUST=341
  integer,parameter::idx_SO2_DUST=342
  integer,parameter::idx_HCOOCH3_DUST=343
  integer,parameter::idx_NH2CHO_DUST=344
  integer,parameter::idx_CO2_DUST=345
  integer,parameter::idx_CH2O2_DUST=346
  integer,parameter::idx_NH2OH_DUST=347
  integer,parameter::idx_C4N_DUST=348
  integer,parameter::idx_C4S_DUST=349
  integer,parameter::idx_C6H6_DUST=350
  integer,parameter::idx_CH2NH2=351
  integer,parameter::idx_CH3C4H_DUST=352
  integer,parameter::idx_CH3C6H_DUST=353
  integer,parameter::idx_H2S2_DUST=354
  integer,parameter::idx_HC2NC_DUST=355
  integer,parameter::idx_HCNC2_DUST=356
  integer,parameter::idx_HE_DUST=357
  integer,parameter::idx_HNC3_DUST=358
  integer,parameter::idx_HS2_DUST=359
  integer,parameter::idx_NAOH_DUST=360
  integer,parameter::idx_S2_DUST=361
  integer,parameter::idx_SIC_DUST=362
  integer,parameter::idx_SIO_DUST=363
  integer,parameter::idx_SIS_DUST=364
  integer,parameter::idx_C2H6CO_DUST=365
  integer,parameter::idx_C3P_DUST=366
  integer,parameter::idx_CCP_DUST=367
  integer,parameter::idx_C4P_DUST=368
  integer,parameter::idx_CCL_DUST=369
  integer,parameter::idx_CL_DUST=370
  integer,parameter::idx_P_DUST=371
  integer,parameter::idx_CP_DUST=372
  integer,parameter::idx_CH2PH_DUST=373
  integer,parameter::idx_HCP_DUST=374
  integer,parameter::idx_CLO_DUST=375
  integer,parameter::idx_H2SIO_DUST=376
  integer,parameter::idx_HCCP_DUST=377
  integer,parameter::idx_HCL_DUST=378
  integer,parameter::idx_HCSI_DUST=379
  integer,parameter::idx_HNSI_DUST=380
  integer,parameter::idx_SIN_DUST=381
  integer,parameter::idx_HPO_DUST=382
  integer,parameter::idx_PO_DUST=383
  integer,parameter::idx_N2O_DUST=384
  integer,parameter::idx_NH2CN_DUST=385
  integer,parameter::idx_NO2_DUST=386
  integer,parameter::idx_PH_DUST=387
  integer,parameter::idx_PH2_DUST=388
  integer,parameter::idx_PN_DUST=389
  integer,parameter::idx_SIC2_DUST=390
  integer,parameter::idx_SIC2H_DUST=391
  integer,parameter::idx_SIC2H2_DUST=392
  integer,parameter::idx_SIC3_DUST=393
  integer,parameter::idx_SIC3H_DUST=394
  integer,parameter::idx_SIC4_DUST=395
  integer,parameter::idx_SICH2_DUST=396
  integer,parameter::idx_SICH3_DUST=397
  integer,parameter::idx_SINC_DUST=398
  integer,parameter::idx_SIO2_DUST=399
  integer,parameter::idx_Cj=400
  integer,parameter::idx_CLj=401
  integer,parameter::idx_FEj=402
  integer,parameter::idx_Hj=403
  integer,parameter::idx_HEj=404
  integer,parameter::idx_MGj=405
  integer,parameter::idx_Nj=406
  integer,parameter::idx_NAj=407
  integer,parameter::idx_Oj=408
  integer,parameter::idx_Pj=409
  integer,parameter::idx_Sj=410
  integer,parameter::idx_SIj=411
  integer,parameter::idx_COj=412
  integer,parameter::idx_H2j=413
  integer,parameter::idx_NOj=414
  integer,parameter::idx_O2j=415
  integer,parameter::idx_CH2j=416
  integer,parameter::idx_H2Sj=417
  integer,parameter::idx_HCOj=418
  integer,parameter::idx_HCSj=419
  integer,parameter::idx_HNOj=420
  integer,parameter::idx_NH2j=421
  integer,parameter::idx_OCSj=422
  integer,parameter::idx_C2H2j=423
  integer,parameter::idx_CH3j=424
  integer,parameter::idx_NH3j=425
  integer,parameter::idx_C2H2Oj=426
  integer,parameter::idx_CH2O2j=427
  integer,parameter::idx_C2H3Nj=428
  integer,parameter::idx_C2H4j=429
  integer,parameter::idx_C4H2j=430
  integer,parameter::idx_H3COj=431
  integer,parameter::idx_CH4Oj=432
  integer,parameter::idx_C2H4Oj=433
  integer,parameter::idx_C3H4j=434
  integer,parameter::idx_CH5Nj=435
  integer,parameter::idx_C2H5OHj=436
  integer,parameter::idx_CH3OCH3j=437
  integer,parameter::idx_CHj=438
  integer,parameter::idx_CCLj=439
  integer,parameter::idx_C2j=440
  integer,parameter::idx_CLOj=441
  integer,parameter::idx_CPj=442
  integer,parameter::idx_CSj=443
  integer,parameter::idx_CNj=444
  integer,parameter::idx_NSj=445
  integer,parameter::idx_PHj=446
  integer,parameter::idx_POj=447
  integer,parameter::idx_SICj=448
  integer,parameter::idx_SINj=449
  integer,parameter::idx_SISj=450
  integer,parameter::idx_SOj=451
  integer,parameter::idx_C3j=452
  integer,parameter::idx_C2Sj=453
  integer,parameter::idx_C2Oj=454
  integer,parameter::idx_CCPj=455
  integer,parameter::idx_C2Hj=456
  integer,parameter::idx_HOCj=457
  integer,parameter::idx_C2Nj=458
  integer,parameter::idx_CNCj=459
  integer,parameter::idx_HCPj=460
  integer,parameter::idx_SIC2j=461
  integer,parameter::idx_SINCj=462
  integer,parameter::idx_HPOj=463
  integer,parameter::idx_HCNj=464
  integer,parameter::idx_CHSIj=465
  integer,parameter::idx_SIH2j=466
  integer,parameter::idx_C3Hj=467
  integer,parameter::idx_C4j=468
  integer,parameter::idx_C3Oj=469
  integer,parameter::idx_C3Sj=470
  integer,parameter::idx_H2COj=471
  integer,parameter::idx_H2SIOj=472
  integer,parameter::idx_HCNHj=473
  integer,parameter::idx_SIC2Hj=474
  integer,parameter::idx_SIC3j=475
  integer,parameter::idx_CH2SIj=476
  integer,parameter::idx_SIH3j=477
  integer,parameter::idx_C2H2Nj=478
  integer,parameter::idx_C2H3j=479
  integer,parameter::idx_C3H2j=480
  integer,parameter::idx_H2C3j=481
  integer,parameter::idx_C4Hj=482
  integer,parameter::idx_C5j=483
  integer,parameter::idx_C4Sj=484
  integer,parameter::idx_PC2Hj=485
  integer,parameter::idx_C3Nj=486
  integer,parameter::idx_C4Nj=487
  integer,parameter::idx_C3HNj=488
  integer,parameter::idx_HNCj=489
  integer,parameter::idx_SIC3Hj=490
  integer,parameter::idx_SIC4j=491
  integer,parameter::idx_SIC2H2j=492
  integer,parameter::idx_SICH3j=493
  integer,parameter::idx_HC2NCHj=494
  integer,parameter::idx_C3H3j=495
  integer,parameter::idx_H3C3j=496
  integer,parameter::idx_C5Hj=497
  integer,parameter::idx_C6j=498
  integer,parameter::idx_C2H3Oj=499
  integer,parameter::idx_C2H5j=500
  integer,parameter::idx_C3H3Nj=501
  integer,parameter::idx_C5H2j=502
  integer,parameter::idx_C4H3j=503
  integer,parameter::idx_C6Hj=504
  integer,parameter::idx_C7j=505
  integer,parameter::idx_CH4Nj=506
  integer,parameter::idx_C5HNj=507
  integer,parameter::idx_C7Hj=508
  integer,parameter::idx_C8j=509
  integer,parameter::idx_COOCH4j=510
  integer,parameter::idx_C2H5Oj=511
  integer,parameter::idx_C8Hj=512
  integer,parameter::idx_C9j=513
  integer,parameter::idx_C5H3j=514
  integer,parameter::idx_C6H2j=515
  integer,parameter::idx_C6H3j=516
  integer,parameter::idx_C2H6COj=517
  integer,parameter::idx_C9Hj=518
  integer,parameter::idx_C10j=519
  integer,parameter::idx_C7H3j=520
  integer,parameter::idx_C8H2j=521
  integer,parameter::idx_C8H3j=522
  integer,parameter::idx_HCLj=523
  integer,parameter::idx_HSj=524
  integer,parameter::idx_NHj=525
  integer,parameter::idx_OHj=526
  integer,parameter::idx_PNj=527
  integer,parameter::idx_S2j=528
  integer,parameter::idx_SIHj=529
  integer,parameter::idx_SIOj=530
  integer,parameter::idx_H2Oj=531
  integer,parameter::idx_HNSIj=532
  integer,parameter::idx_S2Hj=533
  integer,parameter::idx_PH2j=534
  integer,parameter::idx_H2CSj=535
  integer,parameter::idx_H2S2j=536
  integer,parameter::idx_HSIOj=537
  integer,parameter::idx_C4Pj=538
  integer,parameter::idx_HCO2j=539
  integer,parameter::idx_PCH3j=540
  integer,parameter::idx_CH4j=541
  integer,parameter::idx_C2NHj=542
  integer,parameter::idx_SIH4j=543
  integer,parameter::idx_NH4j=544
  integer,parameter::idx_H2NCj=545
  integer,parameter::idx_C3H2Nj=546
  integer,parameter::idx_C7H2j=547
  integer,parameter::idx_C5H4j=548
  integer,parameter::idx_C7HNj=549
  integer,parameter::idx_C9H2j=550
  integer,parameter::idx_C7H4j=551
  integer,parameter::idx_C9HNj=552
  integer,parameter::idx_N2j=553
  integer,parameter::idx_CO2j=554
  integer,parameter::idx_HEHj=555
  integer,parameter::idx_SO2j=556
  integer,parameter::idx_C6H5j=557
  integer,parameter::idx_C5H5j=558
  integer,parameter::idx_N2Hj=559
  integer,parameter::idx_NO2j=560
  integer,parameter::idx_PC2H2j=561
  integer,parameter::idx_PNH2j=562
  integer,parameter::idx_PCH2j=563
  integer,parameter::idx_HC2Sj=564
  integer,parameter::idx_HC3Sj=565
  integer,parameter::idx_H3CSj=566
  integer,parameter::idx_HC4Sj=567
  integer,parameter::idx_SINH2j=568
  integer,parameter::idx_SIC2H3j=569
  integer,parameter::idx_SIC3H2j=570
  integer,parameter::idx_C2HOj=571
  integer,parameter::idx_H3Oj=572
  integer,parameter::idx_H3Sj=573
  integer,parameter::idx_HOCSj=574
  integer,parameter::idx_CH5Oj=575
  integer,parameter::idx_NCOj=576
  integer,parameter::idx_HNCOj=577
  integer,parameter::idx_C2N2j=578
  integer,parameter::idx_H3j=579
  integer,parameter::idx_O2Hj=580
  integer,parameter::idx_CH5j=581
  integer,parameter::idx_H2CLj=582
  integer,parameter::idx_CH3O2j=583
  integer,parameter::idx_H2POj=584
  integer,parameter::idx_PNH3j=585
  integer,parameter::idx_PCH4j=586
  integer,parameter::idx_PC2H3j=587
  integer,parameter::idx_HSISj=588
  integer,parameter::idx_HSOj=589
  integer,parameter::idx_HNSj=590
  integer,parameter::idx_HPNj=591
  integer,parameter::idx_H2NOj=592
  integer,parameter::idx_NAH2Oj=593
  integer,parameter::idx_PH3j=594
  integer,parameter::idx_SINCHj=595
  integer,parameter::idx_HSIO2j=596
  integer,parameter::idx_HSO2j=597
  integer,parameter::idx_HC3Oj=598
  integer,parameter::idx_PC3Hj=599
  integer,parameter::idx_H3S2j=600
  integer,parameter::idx_H3SIOj=601
  integer,parameter::idx_PC4Hj=602
  integer,parameter::idx_NH2CNHj=603
  integer,parameter::idx_SIC4Hj=604
  integer,parameter::idx_SICH4j=605
  integer,parameter::idx_SIH5j=606
  integer,parameter::idx_C2H4Nj=607
  integer,parameter::idx_NH2CH2Oj=608
  integer,parameter::idx_C2H6j=609
  integer,parameter::idx_C3H4Nj=610
  integer,parameter::idx_C3H5j=611
  integer,parameter::idx_C4H4j=612
  integer,parameter::idx_CH6Nj=613
  integer,parameter::idx_C5H2Nj=614
  integer,parameter::idx_C4H4Nj=615
  integer,parameter::idx_H5C2O2j=616
  integer,parameter::idx_C2H5OH2j=617
  integer,parameter::idx_CH3OCH4j=618
  integer,parameter::idx_C7H2Nj=619
  integer,parameter::idx_C3H6OHj=620
  integer,parameter::idx_C6H4Nj=621
  integer,parameter::idx_C10Hj=622
  integer,parameter::idx_C9H3j=623
  integer,parameter::idx_C7H5j=624
  integer,parameter::idx_C8H4Nj=625
  integer,parameter::idx_C9H2Nj=626
  integer,parameter::idx_C6H7j=627
  integer,parameter::idx_NAH2j=628
  integer,parameter::idx_PC2H4j=629
  integer,parameter::idx_C4H5j=630
  integer,parameter::idx_H2CCLj=631
  integer,parameter::idx_PC4H2j=632
  integer,parameter::idx_C6H4j=633
  integer,parameter::idx_C8H4j=634
  integer,parameter::idx_C9H4j=635
  integer,parameter::idx_C4H7j=636
  integer,parameter::idx_HC4Nj=637
  integer,parameter::idx_HC4Oj=638
  integer,parameter::idx_C5Nj=639
  integer,parameter::idx_H2C4Nj=640
  integer,parameter::idx_H3C4Nj=641
  integer,parameter::idx_C7Nj=642
  integer,parameter::idx_C5H3Nj=643
  integer,parameter::idx_C10H2j=644
  integer,parameter::idx_C9Nj=645
  integer,parameter::idx_C7H3Nj=646
  integer,parameter::idx_C9H3Nj=647
  integer,parameter::idx_OCSjH2=648
  integer,parameter::idx_H2C3Oj=649
  integer,parameter::idx_H3C3Oj=650
  integer,parameter::idx_C5H4Nj=651
  integer,parameter::idx_C8H5j=652
  integer,parameter::idx_C9H5j=653
  integer,parameter::idx_H2COHOCH2j=654
  integer,parameter::idx_H7C2O2j=655
  integer,parameter::idx_CR=656
  integer,parameter::idx_g=657
  integer,parameter::idx_Tgas=658
  integer,parameter::idx_dummy=659
  integer,parameter::nrea=5639
  integer,parameter::nmols=655
  integer,parameter::nspec=659
  integer,parameter::natoms=15
  integer,parameter::ndust=0
  integer,parameter::ndustTypes=0
  integer,parameter::nPhotoBins=0
  integer,parameter::nPhotoRea=0

  !cooling index
  integer,parameter::idx_cool_h2 = 1
  integer,parameter::idx_cool_h2gp = 2
  integer,parameter::idx_cool_atomic = 3
  integer,parameter::idx_cool_cen = 3
  integer,parameter::idx_cool_hd = 4
  integer,parameter::idx_cool_z = 5
  integer,parameter::idx_cool_metal = 5
  integer,parameter::idx_cool_dh = 6
  integer,parameter::idx_cool_enthalpic = 6
  integer,parameter::idx_cool_dust = 7
  integer,parameter::idx_cool_compton = 8
  integer,parameter::idx_cool_cie = 9
  integer,parameter::idx_cool_continuum = 10
  integer,parameter::idx_cool_cont = 10
  integer,parameter::idx_cool_exp = 11
  integer,parameter::idx_cool_expansion = 11
  integer,parameter::idx_cool_ff = 12
  integer,parameter::idx_cool_bss = 12
  integer,parameter::idx_cool_custom = 13
  integer,parameter::idx_cool_co = 14
  integer,parameter::idx_cool_zcie = 15
  integer,parameter::idx_cool_zcienouv = 16
  integer,parameter::idx_cool_zextend = 17
  integer,parameter::idx_cool_gh = 18
  integer,parameter::idx_cool_oh = 19
  integer,parameter::idx_cool_h2o = 20
  integer,parameter::idx_cool_hcn = 21
  integer,parameter::ncools = 21

  !heating index
  integer,parameter::idx_heat_chem = 1
  integer,parameter::idx_heat_compress = 2
  integer,parameter::idx_heat_compr = 2
  integer,parameter::idx_heat_photo = 3
  integer,parameter::idx_heat_dh = 4
  integer,parameter::idx_heat_enthalpic = 4
  integer,parameter::idx_heat_photoav = 5
  integer,parameter::idx_heat_av = 5
  integer,parameter::idx_heat_cr = 6
  integer,parameter::idx_heat_dust = 7
  integer,parameter::idx_heat_xray = 8
  integer,parameter::idx_heat_visc = 9
  integer,parameter::idx_heat_viscous = 9
  integer,parameter::idx_heat_custom = 10
  integer,parameter::idx_heat_zcie = 11
  integer,parameter::nheats = 11

  real*8::arr_k(nrea)

  !commons for rate tables
  !modify ktab_n according to the required precision
  integer,parameter::ktab_n=int(1e3)
  real*8::ktab(nrea,ktab_n),ktab_logTlow, ktab_logTup, ktab_T(ktab_n)
  real*8::inv_ktab_T(ktab_n-1), inv_ktab_idx

  !thermo toggle (when >0 do cooling/heating)
  integer::krome_thermo_toggle
  !$omp threadprivate(krome_thermo_toggle)

  !debug bit flag, print and array with fallback values for extreme environments
  integer:: red_flag
  real*8::n_global(nspec)
  integer, save :: nprint_negative=10
  !$omp threadprivate(n_global,nprint_negative,red_flag)

  !commons for implicit RHS
  integer::arr_r1(nrea)
  integer::arr_r2(nrea)
  integer::arr_p1(nrea)
  integer::arr_p2(nrea)
  integer::arr_p3(nrea)
  integer::arr_p4(nrea)

  !commons for reduction
  integer::arr_u(nrea)
  real*8::arr_flux(nrea)

  !commons for frequency bins

  ! Draine dust absorption data loaded from file, via load_kabs
  ! in krome_photo module
  real*8::find_Av_draine_kabs(nPhotoBins)

  !commons for H2 photodissociation (Solomon)
  ! note: paramters here are set depending on the data
  ! but if you have a different file you should modify them
  integer,parameter::H2pdData_nvibX=15
  integer,parameter::H2pdData_nvibB=37
  real*8::H2pdData_dE(H2pdData_nvibX,H2pdData_nvibB)
  real*8::H2pdData_pre(H2pdData_nvibX,H2pdData_nvibB)
  real*8::H2pdData_EX(H2pdData_nvibX)
  integer::H2pdData_binMap(H2pdData_nvibX,H2pdData_nvibB)

  !commons for dust optical properties

  !square of turbulence velocity for broadening
  real*8::broadeningVturb2

  !mpi rank of process. If 0, ignored
  integer::krome_mpi_rank=0, krome_omp_thread
  !$omp threadprivate(krome_omp_thread)

  !user-defined commons variables from the reaction file
  real*8::user_Tdust,user_xdust,user_gsize,user_gsize2,user_crflux,user_Av
  !$omp threadprivate(user_Tdust,user_xdust,user_gsize,user_gsize2,user_crflux,user_Av)

  !define the common Ebinding
  real*8::Ebinding(nspec)

  !commons for anytab

  !physical commons
  real*8::phys_Tcmb
  real*8::phys_zredshift
  real*8::phys_orthoParaRatio
  real*8::phys_metallicity
  real*8::phys_Tfloor
  !$omp threadprivate(phys_Tcmb)
  !$omp threadprivate(phys_zredshift)
  !$omp threadprivate(phys_orthoParaRatio)
  !$omp threadprivate(phys_metallicity)
  !$omp threadprivate(phys_Tfloor)

  !machine precision
  real*8::krome_epsilon

  !xrayJ21 for tabulated heating and rate
  real*8::J21xray

  !total metallicity relative to solar Z/Z_solar
  real*8::total_Z
  real*8::dust2gas_ratio

  !commons for dust tabs (cool,H2,Tdust)
  integer,parameter::dust_tab_imax=50, dust_tab_jmax=50
  real*8::dust_tab_ngas(dust_tab_imax)
  real*8::dust_tab_Tgas(dust_tab_jmax)
  real*8::dust_mult_Tgas,dust_mult_ngas
  real*8::dust_table_AvVariable_log

  real*8::dust_tab_cool(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_heat(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_Tdust(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_H2(dust_tab_imax, dust_tab_jmax)

  !commons for exp(-a) table
  integer,parameter::exp_table_na=int(1d5)
  real*8,parameter::exp_table_aMax=1d4,exp_table_aMin=0d0
  real*8,parameter::exp_table_multa=(exp_table_na-1) &
      / (exp_table_aMax-exp_table_aMin)
  real*8,parameter::exp_table_da=1d0/exp_table_multa
  real*8::exp_table(exp_table_na)

  !stores the last evaluation of the rates in the fex
  real*8::last_coe(nrea)
  !$omp threadprivate(last_coe)

  !xsecs from file variables

  ! Gibbs free energy data from file variables

  !partition function from file
  integer,parameter::zpart_nCO=641
  integer,parameter::zpart_nH2even=2000
  integer,parameter::zpart_nH2odd=2000
  real*8::zpart_CO(zpart_nCO),minpart_CO,partdT_CO
  real*8::zpart_H2even(zpart_nH2even),minpart_H2even,partdT_H2even
  real*8::zpart_H2odd(zpart_nH2odd),minpart_H2odd,partdT_H2odd

  !Habing flux for the photoelectric heating by dust
  ! and clumping factor for H2 formation
  ! on dust by Jura/Gnedin
  real*8::GHabing,Ghabing_thin,clump_factor
  !$omp threadprivate(GHabing,GHabing_thin)

  !partition functions common vars

  !verbatim reactions
  character*50::reactionNames(nrea)

end module krome_commons
