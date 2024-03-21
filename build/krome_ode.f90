
!############### MODULE ##############
module krome_ode
contains

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

  subroutine fex(neq,tt,nin,dn)
    use krome_commons
    use krome_constants
    use krome_subs
    use krome_cooling
    use krome_heating
    use krome_tabs
    use krome_photo
    use krome_gadiab
    use krome_getphys
    use krome_phfuncs
    use krome_fit
    implicit none
    integer::neq,idust
    real*8::tt,dn(neq),n(neq),k(nrea),krome_gamma
    real*8::gamma,Tgas,vgas,ntot,nH2dust,nd,nin(neq)
    real*8::rr
    integer::i,r1,r2,r3,p1,p2,p3

    n(:) = nin(:)

    nH2dust = 0.d0
    n(idx_CR) = 1.d0
    n(idx_g)  = 1.d0
    n(idx_dummy) = 1.d0

    dn(:) = 0.d0 !initialize differentials
    n(idx_Tgas) = max(n(idx_tgas),2.73d0)
    n(idx_Tgas) = min(n(idx_tgas),1d9)
    Tgas = n(idx_Tgas) !get temperature

    k(:) = coe_tab(n(:)) !compute coefficients

    n(idx_dummy) = 1.d0
    n(idx_g) = 1.d0
    n(idx_CR) = 1.d0
    do i=1,nrea
      r1 = arr_r1(i)
      r2 = arr_r2(i)
      r3 = arr_r3(i)
      p1 = arr_p1(i)
      p2 = arr_p2(i)
      p3 = arr_p3(i)
      rr = k(i)*n(r1)*n(r2)*n(r3)
      dn(r1) = dn(r1) - rr
      dn(r2) = dn(r2) - rr
      dn(r3) = dn(r3) - rr
      dn(p1) = dn(p1) + rr
      dn(p2) = dn(p2) + rr
      dn(p3) = dn(p3) + rr
    end do

    last_coe(:) = k(:)

  end subroutine fex

  !***************************
  subroutine jes(neq, tt, n, j, ian, jan, pdj)
    use krome_commons
    use krome_subs
    use krome_tabs
    use krome_cooling
    use krome_heating
    use krome_constants
    use krome_gadiab
    use krome_getphys
    implicit none
    integer::neq, j, ian, jan, r1, r2, p1, p2, p3, i
    real*8::tt, n(neq), pdj(neq), dr1, dr2, kk,k(nrea),Tgas
    real*8::nn(neq),dn0,dn1,dnn,nH2dust,dn(neq),krome_gamma

    nH2dust = 0.d0
    Tgas = n(idx_Tgas)

    k(:) = last_coe(:) !get rate coefficients

    if(j==1) then
    elseif(j==1) then
      pdj(1) =  &
          -k(1)*n(idx_H)  &
          +2.d0*k(1)*n(idx_H)  &
          -k(2)*n(idx_Hj)  &
          -k(3)*n(idx_Hj)  &
          -k(4)*n(idx_HE)  &
          +2.d0*k(4)*n(idx_HE)  &
          -k(5)*n(idx_HEj)  &
          -k(6)*n(idx_HEj)  &
          -k(7)*n(idx_HEj)  &
          +2.d0*k(7)*n(idx_HEj)  &
          -k(8)*n(idx_HEjj)  &
          -k(9)*n(idx_H)  &
          -k(16)*n(idx_H2)  &
          +k(16)*n(idx_H2)  &
          -k(18)*n(idx_Hk)  &
          +2.d0*k(18)*n(idx_Hk)  &
          -k(23)*n(idx_H2j)  &
          -k(24)*n(idx_H2j)  &
          -k(38)*n(idx_Dj)
      pdj(2) =  &
          +k(9)*n(idx_H)  &
          -k(18)*n(idx_Hk)
      pdj(3) =  &
          -k(1)*n(idx_H)  &
          +k(2)*n(idx_Hj)  &
          +k(3)*n(idx_Hj)  &
          -k(9)*n(idx_H)  &
          +2.d0*k(16)*n(idx_H2)  &
          +k(18)*n(idx_Hk)  &
          +2.d0*k(23)*n(idx_H2j)  &
          +2.d0*k(24)*n(idx_H2j)
      pdj(4) =  &
          -k(4)*n(idx_HE)  &
          +k(5)*n(idx_HEj)  &
          +k(6)*n(idx_HEj)
      pdj(5) =  &
          -k(16)*n(idx_H2)
      pdj(6) =  &
          +k(38)*n(idx_Dj)
      pdj(8) =  &
          +k(1)*n(idx_H)  &
          -k(2)*n(idx_Hj)  &
          -k(3)*n(idx_Hj)
      pdj(9) =  &
          +k(4)*n(idx_HE)  &
          -k(5)*n(idx_HEj)  &
          -k(6)*n(idx_HEj)  &
          -k(7)*n(idx_HEj)  &
          +k(8)*n(idx_HEjj)
      pdj(10) =  &
          -k(23)*n(idx_H2j)  &
          -k(24)*n(idx_H2j)
      pdj(11) =  &
          -k(38)*n(idx_Dj)
      pdj(12) =  &
          +k(7)*n(idx_HEj)  &
          -k(8)*n(idx_HEjj)
    elseif(j==2) then
      pdj(1) =  &
          +k(10)*n(idx_H)  &
          +k(11)*n(idx_H)  &
          -k(18)*n(idx_E)  &
          +2.d0*k(18)*n(idx_E)  &
          +k(19)*n(idx_H)  &
          +k(20)*n(idx_H)  &
          +k(22)*n(idx_Hj)  &
          +k(37)*n(idx_D)
      pdj(2) =  &
          -k(10)*n(idx_H)  &
          -k(11)*n(idx_H)  &
          -k(18)*n(idx_E)  &
          -k(19)*n(idx_H)  &
          -k(20)*n(idx_H)  &
          -k(21)*n(idx_Hj)  &
          -k(22)*n(idx_Hj)  &
          -k(25)*n(idx_H2j)  &
          -k(37)*n(idx_D)
      pdj(3) =  &
          -k(10)*n(idx_H)  &
          -k(11)*n(idx_H)  &
          +k(18)*n(idx_E)  &
          -k(19)*n(idx_H)  &
          +2.d0*k(19)*n(idx_H)  &
          -k(20)*n(idx_H)  &
          +2.d0*k(20)*n(idx_H)  &
          +2.d0*k(21)*n(idx_Hj)  &
          +k(25)*n(idx_H2j)
      pdj(5) =  &
          +k(10)*n(idx_H)  &
          +k(11)*n(idx_H)  &
          +k(25)*n(idx_H2j)
      pdj(6) =  &
          -k(37)*n(idx_D)
      pdj(7) =  &
          +k(37)*n(idx_D)
      pdj(8) =  &
          -k(21)*n(idx_Hj)  &
          -k(22)*n(idx_Hj)
      pdj(10) =  &
          +k(22)*n(idx_Hj)  &
          -k(25)*n(idx_H2j)
    elseif(j==3) then
      pdj(1) =  &
          -k(1)*n(idx_E)  &
          +2.d0*k(1)*n(idx_E)  &
          -k(9)*n(idx_E)  &
          +k(10)*n(idx_Hk)  &
          +k(11)*n(idx_Hk)  &
          +k(19)*n(idx_Hk)  &
          +k(20)*n(idx_Hk)
      pdj(2) =  &
          +k(9)*n(idx_E)  &
          -k(10)*n(idx_Hk)  &
          -k(11)*n(idx_Hk)  &
          -k(19)*n(idx_Hk)  &
          -k(20)*n(idx_Hk)
      pdj(3) =  &
          -k(1)*n(idx_E)  &
          -k(9)*n(idx_E)  &
          -k(10)*n(idx_Hk)  &
          -k(11)*n(idx_Hk)  &
          -k(12)*n(idx_Hj)  &
          -k(13)*n(idx_Hj)  &
          -k(14)*n(idx_H2j)  &
          -k(17)*n(idx_H2)  &
          +3.d0*k(17)*n(idx_H2)  &
          -k(19)*n(idx_Hk)  &
          +2.d0*k(19)*n(idx_Hk)  &
          -k(20)*n(idx_Hk)  &
          +2.d0*k(20)*n(idx_Hk)  &
          -9.d0*k(26)*n(idx_H)*n(idx_H)  &
          +3.d0*k(26)*n(idx_H)*n(idx_H)  &
          -9.d0*k(27)*n(idx_H)*n(idx_H)  &
          +3.d0*k(27)*n(idx_H)*n(idx_H)  &
          -4.d0*k(28)*n(idx_H2)*n(idx_H)  &
          -4.d0*k(29)*n(idx_H2)*n(idx_H)  &
          -k(31)*n(idx_Dj)  &
          -k(36)*n(idx_HD)
      pdj(5) =  &
          +k(10)*n(idx_Hk)  &
          +k(11)*n(idx_Hk)  &
          +k(14)*n(idx_H2j)  &
          -k(17)*n(idx_H2)  &
          +3.d0*k(26)*n(idx_H)*n(idx_H)  &
          +3.d0*k(27)*n(idx_H)*n(idx_H)  &
          -2.d0*k(28)*n(idx_H2)*n(idx_H)  &
          +4.d0*k(28)*n(idx_H2)*n(idx_H)  &
          -2.d0*k(29)*n(idx_H2)*n(idx_H)  &
          +4.d0*k(29)*n(idx_H2)*n(idx_H)  &
          +k(36)*n(idx_HD)
      pdj(6) =  &
          +k(31)*n(idx_Dj)  &
          +k(36)*n(idx_HD)
      pdj(7) =  &
          -k(36)*n(idx_HD)
      pdj(8) =  &
          +k(1)*n(idx_E)  &
          -k(12)*n(idx_Hj)  &
          -k(13)*n(idx_Hj)  &
          +k(14)*n(idx_H2j)  &
          +k(31)*n(idx_Dj)
      pdj(10) =  &
          +k(12)*n(idx_Hj)  &
          +k(13)*n(idx_Hj)  &
          -k(14)*n(idx_H2j)
      pdj(11) =  &
          -k(31)*n(idx_Dj)
    elseif(j==4) then
      pdj(1) =  &
          -k(4)*n(idx_E)  &
          +2.d0*k(4)*n(idx_E)
      pdj(4) =  &
          -k(4)*n(idx_E)
      pdj(9) =  &
          +k(4)*n(idx_E)
    elseif(j==5) then
      pdj(1) =  &
          -k(16)*n(idx_E)  &
          +k(16)*n(idx_E)
      pdj(3) =  &
          +k(15)*n(idx_Hj)  &
          +2.d0*k(16)*n(idx_E)  &
          -k(17)*n(idx_H)  &
          +3.d0*k(17)*n(idx_H)  &
          -2.d0*k(28)*n(idx_H)*n(idx_H)  &
          -2.d0*k(29)*n(idx_H)*n(idx_H)  &
          +k(34)*n(idx_D)  &
          +k(35)*n(idx_D)
      pdj(5) =  &
          -k(15)*n(idx_Hj)  &
          -k(16)*n(idx_E)  &
          -k(17)*n(idx_H)  &
          -k(28)*n(idx_H)*n(idx_H)  &
          +2.d0*k(28)*n(idx_H)*n(idx_H)  &
          -k(29)*n(idx_H)*n(idx_H)  &
          +2.d0*k(29)*n(idx_H)*n(idx_H)  &
          -k(32)*n(idx_Dj)  &
          -k(34)*n(idx_D)  &
          -k(35)*n(idx_D)
      pdj(6) =  &
          -k(34)*n(idx_D)  &
          -k(35)*n(idx_D)
      pdj(7) =  &
          +k(32)*n(idx_Dj)  &
          +k(34)*n(idx_D)  &
          +k(35)*n(idx_D)
      pdj(8) =  &
          -k(15)*n(idx_Hj)  &
          +k(32)*n(idx_Dj)
      pdj(10) =  &
          +k(15)*n(idx_Hj)
      pdj(11) =  &
          -k(32)*n(idx_Dj)
    elseif(j==6) then
      pdj(1) =  &
          +k(37)*n(idx_Hk)
      pdj(2) =  &
          -k(37)*n(idx_Hk)
      pdj(3) =  &
          +k(30)*n(idx_Hj)  &
          +k(34)*n(idx_H2)  &
          +k(35)*n(idx_H2)
      pdj(5) =  &
          -k(34)*n(idx_H2)  &
          -k(35)*n(idx_H2)
      pdj(6) =  &
          -k(30)*n(idx_Hj)  &
          -k(34)*n(idx_H2)  &
          -k(35)*n(idx_H2)  &
          -k(37)*n(idx_Hk)
      pdj(7) =  &
          +k(34)*n(idx_H2)  &
          +k(35)*n(idx_H2)  &
          +k(37)*n(idx_Hk)
      pdj(8) =  &
          -k(30)*n(idx_Hj)
      pdj(11) =  &
          +k(30)*n(idx_Hj)
    elseif(j==7) then
      pdj(3) =  &
          -k(36)*n(idx_H)
      pdj(5) =  &
          +k(33)*n(idx_Hj)  &
          +k(36)*n(idx_H)
      pdj(6) =  &
          +k(36)*n(idx_H)
      pdj(7) =  &
          -k(33)*n(idx_Hj)  &
          -k(36)*n(idx_H)
      pdj(8) =  &
          -k(33)*n(idx_Hj)
      pdj(11) =  &
          +k(33)*n(idx_Hj)
    elseif(j==8) then
      pdj(1) =  &
          -k(2)*n(idx_E)  &
          -k(3)*n(idx_E)  &
          +k(22)*n(idx_Hk)
      pdj(2) =  &
          -k(21)*n(idx_Hk)  &
          -k(22)*n(idx_Hk)
      pdj(3) =  &
          +k(2)*n(idx_E)  &
          +k(3)*n(idx_E)  &
          -k(12)*n(idx_H)  &
          -k(13)*n(idx_H)  &
          +k(15)*n(idx_H2)  &
          +2.d0*k(21)*n(idx_Hk)  &
          +k(30)*n(idx_D)
      pdj(5) =  &
          -k(15)*n(idx_H2)  &
          +k(33)*n(idx_HD)
      pdj(6) =  &
          -k(30)*n(idx_D)
      pdj(7) =  &
          -k(33)*n(idx_HD)
      pdj(8) =  &
          -k(2)*n(idx_E)  &
          -k(3)*n(idx_E)  &
          -k(12)*n(idx_H)  &
          -k(13)*n(idx_H)  &
          -k(15)*n(idx_H2)  &
          -k(21)*n(idx_Hk)  &
          -k(22)*n(idx_Hk)  &
          -k(30)*n(idx_D)  &
          -k(33)*n(idx_HD)
      pdj(10) =  &
          +k(12)*n(idx_H)  &
          +k(13)*n(idx_H)  &
          +k(15)*n(idx_H2)  &
          +k(22)*n(idx_Hk)
      pdj(11) =  &
          +k(30)*n(idx_D)  &
          +k(33)*n(idx_HD)
    elseif(j==9) then
      pdj(1) =  &
          -k(5)*n(idx_E)  &
          -k(6)*n(idx_E)  &
          -k(7)*n(idx_E)  &
          +2.d0*k(7)*n(idx_E)
      pdj(4) =  &
          +k(5)*n(idx_E)  &
          +k(6)*n(idx_E)
      pdj(9) =  &
          -k(5)*n(idx_E)  &
          -k(6)*n(idx_E)  &
          -k(7)*n(idx_E)
      pdj(12) =  &
          +k(7)*n(idx_E)
    elseif(j==10) then
      pdj(1) =  &
          -k(23)*n(idx_E)  &
          -k(24)*n(idx_E)
      pdj(2) =  &
          -k(25)*n(idx_Hk)
      pdj(3) =  &
          -k(14)*n(idx_H)  &
          +2.d0*k(23)*n(idx_E)  &
          +2.d0*k(24)*n(idx_E)  &
          +k(25)*n(idx_Hk)
      pdj(5) =  &
          +k(14)*n(idx_H)  &
          +k(25)*n(idx_Hk)
      pdj(8) =  &
          +k(14)*n(idx_H)
      pdj(10) =  &
          -k(14)*n(idx_H)  &
          -k(23)*n(idx_E)  &
          -k(24)*n(idx_E)  &
          -k(25)*n(idx_Hk)
    elseif(j==11) then
      pdj(1) =  &
          -k(38)*n(idx_E)
      pdj(3) =  &
          -k(31)*n(idx_H)
      pdj(5) =  &
          -k(32)*n(idx_H2)
      pdj(6) =  &
          +k(31)*n(idx_H)  &
          +k(38)*n(idx_E)
      pdj(7) =  &
          +k(32)*n(idx_H2)
      pdj(8) =  &
          +k(31)*n(idx_H)  &
          +k(32)*n(idx_H2)
      pdj(11) =  &
          -k(31)*n(idx_H)  &
          -k(32)*n(idx_H2)  &
          -k(38)*n(idx_E)
    elseif(j==12) then
      pdj(1) =  &
          -k(8)*n(idx_E)
      pdj(9) =  &
          +k(8)*n(idx_E)
      pdj(12) =  &
          -k(8)*n(idx_E)
    elseif(j==13) then
    elseif(j==14) then
    elseif(j==15) then

    elseif(j==16) then
    end if

    return
  end subroutine jes

  !*************************
  subroutine jex(neq,t,n,ml,mu,pd,npd)
    use krome_commons
    use krome_tabs
    use krome_cooling
    use krome_heating
    use krome_constants
    use krome_subs
    use krome_gadiab
    implicit none
    real*8::n(neq),pd(neq,neq),t,k(nrea),dn0,dn1,dnn,Tgas
    real*8::krome_gamma,nn(neq),nH2dust
    integer::neq,ml,mu,npd

    Tgas = n(idx_Tgas)
    npd = neq
    k(:) = coe_tab(n(:))
    pd(:,:) = 0d0
    krome_gamma = gamma_index(n(:))

    !d[E_dot]/d[E]
    pd(1,1) =  &
        -k(1)*n(idx_H)  &
        +2.d0*k(1)*n(idx_H)  &
        -k(2)*n(idx_Hj)  &
        -k(3)*n(idx_Hj)  &
        -k(4)*n(idx_HE)  &
        +2.d0*k(4)*n(idx_HE)  &
        -k(5)*n(idx_HEj)  &
        -k(6)*n(idx_HEj)  &
        -k(7)*n(idx_HEj)  &
        +2.d0*k(7)*n(idx_HEj)  &
        -k(8)*n(idx_HEjj)  &
        -k(9)*n(idx_H)  &
        -k(16)*n(idx_H2)  &
        +k(16)*n(idx_H2)  &
        -k(18)*n(idx_Hk)  &
        +2.d0*k(18)*n(idx_Hk)  &
        -k(23)*n(idx_H2j)  &
        -k(24)*n(idx_H2j)  &
        -k(38)*n(idx_Dj)

    !d[H-_dot]/d[E]
    pd(2,1) =  &
        +k(9)*n(idx_H)  &
        -k(18)*n(idx_Hk)

    !d[H_dot]/d[E]
    pd(3,1) =  &
        -k(1)*n(idx_H)  &
        +k(2)*n(idx_Hj)  &
        +k(3)*n(idx_Hj)  &
        -k(9)*n(idx_H)  &
        +2.d0*k(16)*n(idx_H2)  &
        +k(18)*n(idx_Hk)  &
        +2.d0*k(23)*n(idx_H2j)  &
        +2.d0*k(24)*n(idx_H2j)

    !d[HE_dot]/d[E]
    pd(4,1) =  &
        -k(4)*n(idx_HE)  &
        +k(5)*n(idx_HEj)  &
        +k(6)*n(idx_HEj)

    !d[H2_dot]/d[E]
    pd(5,1) =  &
        -k(16)*n(idx_H2)

    !d[D_dot]/d[E]
    pd(6,1) =  &
        +k(38)*n(idx_Dj)

    !d[H+_dot]/d[E]
    pd(8,1) =  &
        +k(1)*n(idx_H)  &
        -k(2)*n(idx_Hj)  &
        -k(3)*n(idx_Hj)

    !d[HE+_dot]/d[E]
    pd(9,1) =  &
        +k(4)*n(idx_HE)  &
        -k(5)*n(idx_HEj)  &
        -k(6)*n(idx_HEj)  &
        -k(7)*n(idx_HEj)  &
        +k(8)*n(idx_HEjj)

    !d[H2+_dot]/d[E]
    pd(10,1) =  &
        -k(23)*n(idx_H2j)  &
        -k(24)*n(idx_H2j)

    !d[D+_dot]/d[E]
    pd(11,1) =  &
        -k(38)*n(idx_Dj)

    !d[HE++_dot]/d[E]
    pd(12,1) =  &
        +k(7)*n(idx_HEj)  &
        -k(8)*n(idx_HEjj)

    !d[E_dot]/d[H-]
    pd(1,2) =  &
        +k(10)*n(idx_H)  &
        +k(11)*n(idx_H)  &
        -k(18)*n(idx_E)  &
        +2.d0*k(18)*n(idx_E)  &
        +k(19)*n(idx_H)  &
        +k(20)*n(idx_H)  &
        +k(22)*n(idx_Hj)  &
        +k(37)*n(idx_D)

    !d[H-_dot]/d[H-]
    pd(2,2) =  &
        -k(10)*n(idx_H)  &
        -k(11)*n(idx_H)  &
        -k(18)*n(idx_E)  &
        -k(19)*n(idx_H)  &
        -k(20)*n(idx_H)  &
        -k(21)*n(idx_Hj)  &
        -k(22)*n(idx_Hj)  &
        -k(25)*n(idx_H2j)  &
        -k(37)*n(idx_D)

    !d[H_dot]/d[H-]
    pd(3,2) =  &
        -k(10)*n(idx_H)  &
        -k(11)*n(idx_H)  &
        +k(18)*n(idx_E)  &
        -k(19)*n(idx_H)  &
        +2.d0*k(19)*n(idx_H)  &
        -k(20)*n(idx_H)  &
        +2.d0*k(20)*n(idx_H)  &
        +2.d0*k(21)*n(idx_Hj)  &
        +k(25)*n(idx_H2j)

    !d[H2_dot]/d[H-]
    pd(5,2) =  &
        +k(10)*n(idx_H)  &
        +k(11)*n(idx_H)  &
        +k(25)*n(idx_H2j)

    !d[D_dot]/d[H-]
    pd(6,2) =  &
        -k(37)*n(idx_D)

    !d[HD_dot]/d[H-]
    pd(7,2) =  &
        +k(37)*n(idx_D)

    !d[H+_dot]/d[H-]
    pd(8,2) =  &
        -k(21)*n(idx_Hj)  &
        -k(22)*n(idx_Hj)

    !d[H2+_dot]/d[H-]
    pd(10,2) =  &
        +k(22)*n(idx_Hj)  &
        -k(25)*n(idx_H2j)

    !d[E_dot]/d[H]
    pd(1,3) =  &
        -k(1)*n(idx_E)  &
        +2.d0*k(1)*n(idx_E)  &
        -k(9)*n(idx_E)  &
        +k(10)*n(idx_Hk)  &
        +k(11)*n(idx_Hk)  &
        +k(19)*n(idx_Hk)  &
        +k(20)*n(idx_Hk)

    !d[H-_dot]/d[H]
    pd(2,3) =  &
        +k(9)*n(idx_E)  &
        -k(10)*n(idx_Hk)  &
        -k(11)*n(idx_Hk)  &
        -k(19)*n(idx_Hk)  &
        -k(20)*n(idx_Hk)

    !d[H_dot]/d[H]
    pd(3,3) =  &
        -k(1)*n(idx_E)  &
        -k(9)*n(idx_E)  &
        -k(10)*n(idx_Hk)  &
        -k(11)*n(idx_Hk)  &
        -k(12)*n(idx_Hj)  &
        -k(13)*n(idx_Hj)  &
        -k(14)*n(idx_H2j)  &
        -k(17)*n(idx_H2)  &
        +3.d0*k(17)*n(idx_H2)  &
        -k(19)*n(idx_Hk)  &
        +2.d0*k(19)*n(idx_Hk)  &
        -k(20)*n(idx_Hk)  &
        +2.d0*k(20)*n(idx_Hk)  &
        -9.d0*k(26)*n(idx_H)*n(idx_H)  &
        +3.d0*k(26)*n(idx_H)*n(idx_H)  &
        -9.d0*k(27)*n(idx_H)*n(idx_H)  &
        +3.d0*k(27)*n(idx_H)*n(idx_H)  &
        -4.d0*k(28)*n(idx_H2)*n(idx_H)  &
        -4.d0*k(29)*n(idx_H2)*n(idx_H)  &
        -k(31)*n(idx_Dj)  &
        -k(36)*n(idx_HD)

    !d[H2_dot]/d[H]
    pd(5,3) =  &
        +k(10)*n(idx_Hk)  &
        +k(11)*n(idx_Hk)  &
        +k(14)*n(idx_H2j)  &
        -k(17)*n(idx_H2)  &
        +3.d0*k(26)*n(idx_H)*n(idx_H)  &
        +3.d0*k(27)*n(idx_H)*n(idx_H)  &
        -2.d0*k(28)*n(idx_H2)*n(idx_H)  &
        +4.d0*k(28)*n(idx_H2)*n(idx_H)  &
        -2.d0*k(29)*n(idx_H2)*n(idx_H)  &
        +4.d0*k(29)*n(idx_H2)*n(idx_H)  &
        +k(36)*n(idx_HD)

    !d[D_dot]/d[H]
    pd(6,3) =  &
        +k(31)*n(idx_Dj)  &
        +k(36)*n(idx_HD)

    !d[HD_dot]/d[H]
    pd(7,3) =  &
        -k(36)*n(idx_HD)

    !d[H+_dot]/d[H]
    pd(8,3) =  &
        +k(1)*n(idx_E)  &
        -k(12)*n(idx_Hj)  &
        -k(13)*n(idx_Hj)  &
        +k(14)*n(idx_H2j)  &
        +k(31)*n(idx_Dj)

    !d[H2+_dot]/d[H]
    pd(10,3) =  &
        +k(12)*n(idx_Hj)  &
        +k(13)*n(idx_Hj)  &
        -k(14)*n(idx_H2j)

    !d[D+_dot]/d[H]
    pd(11,3) =  &
        -k(31)*n(idx_Dj)

    !d[E_dot]/d[HE]
    pd(1,4) =  &
        -k(4)*n(idx_E)  &
        +2.d0*k(4)*n(idx_E)

    !d[HE_dot]/d[HE]
    pd(4,4) =  &
        -k(4)*n(idx_E)

    !d[HE+_dot]/d[HE]
    pd(9,4) =  &
        +k(4)*n(idx_E)

    !d[E_dot]/d[H2]
    pd(1,5) =  &
        -k(16)*n(idx_E)  &
        +k(16)*n(idx_E)

    !d[H_dot]/d[H2]
    pd(3,5) =  &
        +k(15)*n(idx_Hj)  &
        +2.d0*k(16)*n(idx_E)  &
        -k(17)*n(idx_H)  &
        +3.d0*k(17)*n(idx_H)  &
        -2.d0*k(28)*n(idx_H)*n(idx_H)  &
        -2.d0*k(29)*n(idx_H)*n(idx_H)  &
        +k(34)*n(idx_D)  &
        +k(35)*n(idx_D)

    !d[H2_dot]/d[H2]
    pd(5,5) =  &
        -k(15)*n(idx_Hj)  &
        -k(16)*n(idx_E)  &
        -k(17)*n(idx_H)  &
        -k(28)*n(idx_H)*n(idx_H)  &
        +2.d0*k(28)*n(idx_H)*n(idx_H)  &
        -k(29)*n(idx_H)*n(idx_H)  &
        +2.d0*k(29)*n(idx_H)*n(idx_H)  &
        -k(32)*n(idx_Dj)  &
        -k(34)*n(idx_D)  &
        -k(35)*n(idx_D)

    !d[D_dot]/d[H2]
    pd(6,5) =  &
        -k(34)*n(idx_D)  &
        -k(35)*n(idx_D)

    !d[HD_dot]/d[H2]
    pd(7,5) =  &
        +k(32)*n(idx_Dj)  &
        +k(34)*n(idx_D)  &
        +k(35)*n(idx_D)

    !d[H+_dot]/d[H2]
    pd(8,5) =  &
        -k(15)*n(idx_Hj)  &
        +k(32)*n(idx_Dj)

    !d[H2+_dot]/d[H2]
    pd(10,5) =  &
        +k(15)*n(idx_Hj)

    !d[D+_dot]/d[H2]
    pd(11,5) =  &
        -k(32)*n(idx_Dj)

    !d[E_dot]/d[D]
    pd(1,6) =  &
        +k(37)*n(idx_Hk)

    !d[H-_dot]/d[D]
    pd(2,6) =  &
        -k(37)*n(idx_Hk)

    !d[H_dot]/d[D]
    pd(3,6) =  &
        +k(30)*n(idx_Hj)  &
        +k(34)*n(idx_H2)  &
        +k(35)*n(idx_H2)

    !d[H2_dot]/d[D]
    pd(5,6) =  &
        -k(34)*n(idx_H2)  &
        -k(35)*n(idx_H2)

    !d[D_dot]/d[D]
    pd(6,6) =  &
        -k(30)*n(idx_Hj)  &
        -k(34)*n(idx_H2)  &
        -k(35)*n(idx_H2)  &
        -k(37)*n(idx_Hk)

    !d[HD_dot]/d[D]
    pd(7,6) =  &
        +k(34)*n(idx_H2)  &
        +k(35)*n(idx_H2)  &
        +k(37)*n(idx_Hk)

    !d[H+_dot]/d[D]
    pd(8,6) =  &
        -k(30)*n(idx_Hj)

    !d[D+_dot]/d[D]
    pd(11,6) =  &
        +k(30)*n(idx_Hj)

    !d[H_dot]/d[HD]
    pd(3,7) =  &
        -k(36)*n(idx_H)

    !d[H2_dot]/d[HD]
    pd(5,7) =  &
        +k(33)*n(idx_Hj)  &
        +k(36)*n(idx_H)

    !d[D_dot]/d[HD]
    pd(6,7) =  &
        +k(36)*n(idx_H)

    !d[HD_dot]/d[HD]
    pd(7,7) =  &
        -k(33)*n(idx_Hj)  &
        -k(36)*n(idx_H)

    !d[H+_dot]/d[HD]
    pd(8,7) =  &
        -k(33)*n(idx_Hj)

    !d[D+_dot]/d[HD]
    pd(11,7) =  &
        +k(33)*n(idx_Hj)

    !d[E_dot]/d[H+]
    pd(1,8) =  &
        -k(2)*n(idx_E)  &
        -k(3)*n(idx_E)  &
        +k(22)*n(idx_Hk)

    !d[H-_dot]/d[H+]
    pd(2,8) =  &
        -k(21)*n(idx_Hk)  &
        -k(22)*n(idx_Hk)

    !d[H_dot]/d[H+]
    pd(3,8) =  &
        +k(2)*n(idx_E)  &
        +k(3)*n(idx_E)  &
        -k(12)*n(idx_H)  &
        -k(13)*n(idx_H)  &
        +k(15)*n(idx_H2)  &
        +2.d0*k(21)*n(idx_Hk)  &
        +k(30)*n(idx_D)

    !d[H2_dot]/d[H+]
    pd(5,8) =  &
        -k(15)*n(idx_H2)  &
        +k(33)*n(idx_HD)

    !d[D_dot]/d[H+]
    pd(6,8) =  &
        -k(30)*n(idx_D)

    !d[HD_dot]/d[H+]
    pd(7,8) =  &
        -k(33)*n(idx_HD)

    !d[H+_dot]/d[H+]
    pd(8,8) =  &
        -k(2)*n(idx_E)  &
        -k(3)*n(idx_E)  &
        -k(12)*n(idx_H)  &
        -k(13)*n(idx_H)  &
        -k(15)*n(idx_H2)  &
        -k(21)*n(idx_Hk)  &
        -k(22)*n(idx_Hk)  &
        -k(30)*n(idx_D)  &
        -k(33)*n(idx_HD)

    !d[H2+_dot]/d[H+]
    pd(10,8) =  &
        +k(12)*n(idx_H)  &
        +k(13)*n(idx_H)  &
        +k(15)*n(idx_H2)  &
        +k(22)*n(idx_Hk)

    !d[D+_dot]/d[H+]
    pd(11,8) =  &
        +k(30)*n(idx_D)  &
        +k(33)*n(idx_HD)

    !d[E_dot]/d[HE+]
    pd(1,9) =  &
        -k(5)*n(idx_E)  &
        -k(6)*n(idx_E)  &
        -k(7)*n(idx_E)  &
        +2.d0*k(7)*n(idx_E)

    !d[HE_dot]/d[HE+]
    pd(4,9) =  &
        +k(5)*n(idx_E)  &
        +k(6)*n(idx_E)

    !d[HE+_dot]/d[HE+]
    pd(9,9) =  &
        -k(5)*n(idx_E)  &
        -k(6)*n(idx_E)  &
        -k(7)*n(idx_E)

    !d[HE++_dot]/d[HE+]
    pd(12,9) =  &
        +k(7)*n(idx_E)

    !d[E_dot]/d[H2+]
    pd(1,10) =  &
        -k(23)*n(idx_E)  &
        -k(24)*n(idx_E)

    !d[H-_dot]/d[H2+]
    pd(2,10) =  &
        -k(25)*n(idx_Hk)

    !d[H_dot]/d[H2+]
    pd(3,10) =  &
        -k(14)*n(idx_H)  &
        +2.d0*k(23)*n(idx_E)  &
        +2.d0*k(24)*n(idx_E)  &
        +k(25)*n(idx_Hk)

    !d[H2_dot]/d[H2+]
    pd(5,10) =  &
        +k(14)*n(idx_H)  &
        +k(25)*n(idx_Hk)

    !d[H+_dot]/d[H2+]
    pd(8,10) =  &
        +k(14)*n(idx_H)

    !d[H2+_dot]/d[H2+]
    pd(10,10) =  &
        -k(14)*n(idx_H)  &
        -k(23)*n(idx_E)  &
        -k(24)*n(idx_E)  &
        -k(25)*n(idx_Hk)

    !d[E_dot]/d[D+]
    pd(1,11) =  &
        -k(38)*n(idx_E)

    !d[H_dot]/d[D+]
    pd(3,11) =  &
        -k(31)*n(idx_H)

    !d[H2_dot]/d[D+]
    pd(5,11) =  &
        -k(32)*n(idx_H2)

    !d[D_dot]/d[D+]
    pd(6,11) =  &
        +k(31)*n(idx_H)  &
        +k(38)*n(idx_E)

    !d[HD_dot]/d[D+]
    pd(7,11) =  &
        +k(32)*n(idx_H2)

    !d[H+_dot]/d[D+]
    pd(8,11) =  &
        +k(31)*n(idx_H)  &
        +k(32)*n(idx_H2)

    !d[D+_dot]/d[D+]
    pd(11,11) =  &
        -k(31)*n(idx_H)  &
        -k(32)*n(idx_H2)  &
        -k(38)*n(idx_E)

    !d[E_dot]/d[HE++]
    pd(1,12) =  &
        -k(8)*n(idx_E)

    !d[HE+_dot]/d[HE++]
    pd(9,12) =  &
        +k(8)*n(idx_E)

    !d[HE++_dot]/d[HE++]
    pd(12,12) =  &
        -k(8)*n(idx_E)

  end subroutine jex

end module krome_ode
