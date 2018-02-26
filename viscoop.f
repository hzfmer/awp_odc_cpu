CCC   'viscoop.f' COMPUTES INTERNAL REGION STRESSES FOR VISCOELASTIC MATERIALS


      subroutine dstrq(coords,nxt,nyt,nzt,dh,dt,nz)

c     4th order finite-difference of stress components

c     nxt   nodal points in x dir          (integer)(sent)
c     nyt   nodal points in y dir          (integer)(sent)
c     nzt   nodal points in z dir          (integer)(sent)
c     dh    spatial discretization         (real)   (sent)
c     dt    temporal discretization        (real)   (sent)

      use parstat
      implicit none

      integer, dimension(3) :: coords
      integer :: nxt,nyt,nzt,nz
      real :: dh,dt

      call allq(xi(18),xf(18),yi(18),yf(18),zi(18),zf(18),
     +          coords,nxt,nyt,nzt,dh,dt,nz)

      return
      end

      subroutine dstrqc(coords,nxt,nyt,nzt,dh,dt,nz,rank)

c     4th order finite-difference of stress components

c     nxt   nodal points in x dir          (integer)(sent)
c     nyt   nodal points in y dir          (integer)(sent)
c     nzt   nodal points in z dir          (integer)(sent)
c     dh    spatial discretization         (real)   (sent)
c     dt    temporal discretization        (real)   (sent)

      use parstat
      implicit none

      integer, dimension(3) :: coords
      integer :: nxt,nyt,nzt,nz,rank
      real :: dh,dt
 
      call allq(1,nxt,1,nyt,1,nzt,coords,nxt,nyt,nzt,dh,dt,nz)

      return
      end

      subroutine allq(nxb,nxe,nyb,nye,nzb,nze,coords,nxt,nyt,nzt,dh,dt,nz)

c     4th order finite-difference of normal stresses at t+1

c     nxb   starting point for FD in x dir (integer)(sent)
c     nxe   ending point for FD in x dir   (integer)(sent)
c     nyb   starting point for FD in y dir (integer)(sent)
c     nye   ending point for FD in y dir   (integer)(sent)
c     nzb   starting point for FD in z dir (integer)(sent)
c     nze   ending point for FD in z dir   (integer)(sent)
c     dh    spatial discretization         (real)   (sent)
c     dt    temporal discretization        (real)   (sent)

      use parstat
      implicit none

      integer, dimension(3) :: coords
      integer :: nxt,nyt,nzt,nz
      integer :: kk,jj,i,j,k,itx,ity,itz,iii,jjj,kkk,num
      integer :: nxb,nxe,nyb,nye,nzb,nze
      real    :: dh1,dth,dh,dt,c1,c2,dt1,tauu
      real, dimension(nxb:nxe) :: vxl,vxm,vqpa,vqsa
      real, dimension(nxb:nxe) :: vx1,vx2,vt1,vt2,vt3,vt4,vt5,vt6,vt7,va1,vh1,vh2_r4,vh2_r5,vh2_r6,vxmu,vaas1,vtmp,vaas2,vaas3
      real, dimension(nxb:nxe) :: ww,weights_p,weights_s,weights_s1,weights_s2,weights_s3
      real, dimension (2,2,2) :: tau1,tau2

      dh1 = 1./dh
      dth = dt/dh
      c1 = 9./8.
      c2 = -1./24.

      dt1 = 1./dt
      do k=1,2
        do j=1,2
           do i=1,2
              tauu=tau(i,j,k)
c     old way: no exponnetials
c              tau1(i,j,k)=1/((tauu*dt1)+(1./2.))
c              tau2(i,j,k)=(tauu*dt1)-(1./2.)

              tau2(i,j,k)=exp(-dt/tauu)
              tau1(i,j,k)=0.5*(1.-tau2(i,j,k))
           enddo
        enddo
      enddo

      do kk= nzb,nze,kblock
        do jj= nyb,nye,jblock
          do 50 k= kk,min(kk+kblock-1,nze)
            itz = mod(((nz+1)-(coords(3)*nzt+k)),2)+1
              do 50 j= jj,min(jj+jblock-1,nye)
                ity = mod((coords(2)*nyt+j),2)+1
                itx = mod((coords(1)*nxt+nxb),2)+1
c                do i=nxb,nxe
c                  vx1(i)=tau1(itx,ity,itz)
c                  vx2(i)=tau2(itx,ity,itz)
c                  ww(i)=8.*weights(itx,ity,itz)
c                  itx = 3 - itx
c                end do

C == ADDED BY LUIS FOR SGSN DYNAMIC FAULT MODEL
                vxl(nxb:nxe)=8./(lam(nxb:nxe,j,k)+lam(nxb+1:nxe+1,j,k) +
     +               lam(nxb:nxe,j-1,k)+lam(nxb+1:nxe+1,j-1,k) +
     +               lam(nxb:nxe,j,k-1)+lam(nxb+1:nxe+1,j,k-1) +
     +               lam(nxb:nxe,j-1,k-1)+lam(nxb+1:nxe+1,j-1,k-1))

                vxm(nxb:nxe)=8./(mu(nxb:nxe,j,k)+mu(nxb+1:nxe+1,j,k) +
     +               mu(nxb:nxe,j-1,k)+mu(nxb+1:nxe+1,j-1,k) +
     +               mu(nxb:nxe,j,k-1)+mu(nxb+1:nxe+1,j,k-1) +
     +               mu(nxb:nxe,j-1,k-1)+mu(nxb+1:nxe+1,j-1,k-1))

                vxl(nxb:nxe)=vxl(nxb:nxe)+2.*vxm(nxb:nxe)

                vqpa(nxb:nxe)=0.125*(qp(nxb:nxe,j,k)+qp(nxb+1:nxe+1,j,k) +
     +                 qp(nxb:nxe,j-1,k)+qp(nxb+1:nxe+1,j-1,k) +
     +                 qp(nxb:nxe,j,k-1)+qp(nxb+1:nxe+1,j,k-1) +
     +                 qp(nxb:nxe,j-1,k-1)+qp(nxb+1:nxe+1,j-1,k-1))

                vqsa(nxb:nxe)=0.125*(qs(nxb:nxe,j,k)+qs(nxb+1:nxe+1,j,k) +
     +                 qs(nxb:nxe,j-1,k)+qs(nxb+1:nxe+1,j-1,k) +
     +                 qs(nxb:nxe,j,k-1)+qs(nxb+1:nxe+1,j,k-1) +
     +               qs(nxb:nxe,j-1,k-1)+qs(nxb+1:nxe+1,j-1,k-1))
                vaas1(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j,k-1))
                vaas2(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j-1,k))                                                                                               
                vaas3(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb+1:nxe+1,j,k)) 


                do i=nxb,nxe
                  vx1(i)=tau1(itx,ity,itz)
                  vx2(i)=tau2(itx,ity,itz)
c                  ww(i)=8.*weights(itx,ity,itz)

              if (1./vqsa(i) .lt. 200.0) then
c                 print *, 9999999999999
              num=0
              do iii=1,2
                 do jjj=1,2
                    do kkk=1,2
                       weights_los(iii,jjj,kkk)=coeff(1+num)*(vqsa(i)**2)+coeff(2+num)*vqsa(i)
c                    weights_los(iii,jjj,kkk)=coeff(1+num)*(1./vqsa(i))**4.+coeff(2+num)*(1./vqsa(i))**3.+
c     +                     coeff(3+num)*(1./vqsa(i))**2.+coeff(4+num)*(1./vqsa(i))+coeff(5+num);
                     num=num+2
                     end do
                     end do
                     end do

                     weights_s(i)=weights_los(itx,ity,itz)
                  else
                     weights_s(i)=8.*weights(itx,ity,itz)*vqsa(i)
                     endif


              if (1./vqpa(i) .lt. 200.0) then
              num=0
              do iii=1,2
                 do jjj=1,2
                    do kkk=1,2
                       weights_lop(iii,jjj,kkk)=coeff(1+num)*(vqpa(i)**2)+coeff(2+num)*vqpa(i)  
                     num=num+2
                     end do
                     end do
                     end do

                     weights_p(i)=weights_lop(itx,ity,itz)
                  else
                     weights_p(i)=8.*weights(itx,ity,itz)*vqpa(i)
                     endif
cccccccccccccccccccccccccccccccc
              if (1./vaas1(i) .lt. 200.0) then
              num=0
              do iii=1,2
                 do jjj=1,2
                    do kkk=1,2
                       weights_los(iii,jjj,kkk)=coeff(1+num)*(vaas1(i)**2)+coeff(2+num)*vaas1(i)                                                                          
                     num=num+2
                     end do
                     end do
                     end do

c                     weights_s1(i)=weights_los(itx,ity,itz)/vaas(i)
                     weights_s1(i)=weights_los(itx,ity,itz)
                  else
                     weights_s1(i)=8.*weights(itx,ity,itz)*vaas1(i)
                     endif
ccccccccccccccccccccccccccccccccccccccccc
              if (1./vaas2(i) .lt. 200.0) then
              num=0
              do iii=1,2
                 do jjj=1,2
                    do kkk=1,2
                       weights_los(iii,jjj,kkk)=coeff(1+num)*(vaas2(i)**2)+coeff(2+num)*vaas2(i)                                                                             
                     num=num+2
                     end do
                     end do
                     end do

c                     weights_s1(i)=weights_los(itx,ity,itz)/vaas(i)                                                                                                       
                     weights_s2(i)=weights_los(itx,ity,itz)
                  else
                     weights_s2(i)=8.*weights(itx,ity,itz)*vaas2(i)
                     endif
ccccccccccccccccccccccccccccccccccccccccc 
              if (1./vaas3(i) .lt. 200.0) then
              num=0
              do iii=1,2
                 do jjj=1,2
                    do kkk=1,2
                       weights_los(iii,jjj,kkk)=coeff(1+num)*(vaas3(i)**2)+coeff(2+num)*vaas3(i)                                                                             
                     num=num+2
                     end do
                     end do
                     end do

c                     weights_s1(i)=weights_los(itx,ity,itz)/vaas(i)                                                                                                       
                     weights_s3(i)=weights_los(itx,ity,itz)
                  else
                     weights_s3(i)=8.*weights(itx,ity,itz)*vaas3(i)
                     endif
ccccccccccccccccccccccccccccccccccccccccc 

                  itx = 3 - itx
                end do




                vt1(nxb:nxe)=c1*(u1(nxb+1:nxe+1,j,k)-u1(nxb:nxe,j,k))+c2*(u1(nxb+2:nxe+2,j,k)-u1(nxb-1:nxe-1,j,k))
                vt2(nxb:nxe)=c1*(v1(nxb:nxe,j,k)-v1(nxb:nxe,j-1,k))+c2*(v1(nxb:nxe,j+1,k)-v1(nxb:nxe,j-2,k))
                vt3(nxb:nxe)=c1*(w1(nxb:nxe,j,k)-w1(nxb:nxe,j,k-1))+c2*(w1(nxb:nxe,j,k+1)-w1(nxb:nxe,j,k-2))
                vt4(nxb:nxe)=c1*(u1(nxb:nxe,j+1,k)-u1(nxb:nxe,j,k))+c2*(u1(nxb:nxe,j+2,k)-u1(nxb:nxe,j-1,k)) +
     $                 c1*(v1(nxb:nxe,j,k)-v1(nxb-1:nxe-1,j,k))+c2*(v1(nxb+1:nxe+1,j,k)-v1(nxb-2:nxe-2,j,k))
                vt5(nxb:nxe)=c1*(u1(nxb:nxe,j,k+1)-u1(nxb:nxe,j,k))+c2*(u1(nxb:nxe,j,k+2)-u1(nxb:nxe,j,k-1)) +
     $                 c1*(w1(nxb:nxe,j,k)-w1(nxb-1:nxe-1,j,k))+c2*(w1(nxb+1:nxe+1,j,k)-w1(nxb-2:nxe-2,j,k))
                vt6(nxb:nxe)=c1*(v1(nxb:nxe,j,k+1)-v1(nxb:nxe,j,k))+c2*(v1(nxb:nxe,j,k+2)-v1(nxb:nxe,j,k-1))
                vt7(nxb:nxe)=c1*(w1(nxb:nxe,j+1,k)-w1(nxb:nxe,j,k))+c2*(w1(nxb:nxe,j+2,k)-w1(nxb:nxe,j-1,k))

c kbwapril15                vh1(nxb:nxe)=-vxm(nxb:nxe)*vqsa(nxb:nxe)
                vh1(nxb:nxe)=-vxm(nxb:nxe)
                vtmp(nxb:nxe)=vxl(nxb:nxe)*(vt1(nxb:nxe)+vt2(nxb:nxe)+vt3(nxb:nxe))
                !va1(nxb:nxe)=-vqpa(nxb:nxe)*dh1*vtmp(nxb:nxe)/2.
c     KBWapril15                va1(nxb:nxe)=-vqpa(nxb:nxe)*vxl(nxb:nxe)*dh1*(vt1(nxb:nxe)+vt2(nxb:nxe)+vt3(nxb:nxe))/2.
                va1(nxb:nxe)=-vxl(nxb:nxe)*dh1*(vt1(nxb:nxe)+vt2(nxb:nxe)+vt3(nxb:nxe))/2.

                !vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb:nxe,j,k-1))
                !vh2(nxb:nxe)=-vxmu(nxb:nxe,j,k)*vaas(nxb:nxe,j,k)/2.
                !vaas(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j,k-1))

C   NORMAL STRESS XX, YY AND ZZ

                xx(nxb:nxe,j,k)=xx(nxb:nxe,j,k)+dth*(vtmp(nxb:nxe)-2.*vxm*(vt2(nxb:nxe)+vt3(nxb:nxe)))+dt*r1(nxb:nxe,j,k)
                yy(nxb:nxe,j,k)=yy(nxb:nxe,j,k)+dth*(vtmp(nxb:nxe)-2.*vxm*(vt1(nxb:nxe)+vt3(nxb:nxe)))+dt*r2(nxb:nxe,j,k)
                zz(nxb:nxe,j,k)=zz(nxb:nxe,j,k)+dth*(vtmp(nxb:nxe)-2.*vxm*(vt1(nxb:nxe)+vt2(nxb:nxe)))+dt*r3(nxb:nxe,j,k)

                !vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb:nxe,j,k-1))
                !vh2_r4(nxb:nxe)=-0.25*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j,k-1))*vxmu(nxb:nxe)
                vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb:nxe,j,k-1))
c1                vaas(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j,k-1))
c                vh2_r4(nxb:nxe)=-vaas(nxb:nxe)*vxmu(nxb:nxe)/2.
                 vh2_r4(nxb:nxe)=-vxmu(nxb:nxe)/2.
                xy(nxb:nxe,j,k)=xy(nxb:nxe,j,k)+vxmu(nxb:nxe)*dth*vt4(nxb:nxe)+dt*r4(nxb:nxe,j,k)
                !vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb:nxe,j-1,k))
                !vh2_r5(nxb:nxe)=-0.25*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j-1,k))*vxmu(nxb:nxe)
                vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb:nxe,j-1,k))
c2                vaas(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb:nxe,j-1,k))
c                vh2_r5(nxb:nxe)=-vaas(nxb:nxe)*vxmu(nxb:nxe)/2.
                vh2_r5(nxb:nxe)=-vxmu(nxb:nxe)/2.
                xz(nxb:nxe,j,k)=xz(nxb:nxe,j,k)+vxmu(nxb:nxe)*dth*vt5(nxb:nxe)+dt*r5(nxb:nxe,j,k)                                                           
                !vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb+1:nxe+1,j,k))                                                                                   
                !vh2_r6(nxb:nxe)=-0.25*(qs(nxb:nxe,j,k)+qs(nxb+1:nxe+1,j,k))*vxmu(nxb:nxe)                                                                  
                vxmu(nxb:nxe) = 2./(mu(nxb:nxe,j,k)+mu(nxb+1:nxe+1,j,k))
c3                vaas(nxb:nxe) = 0.5*(qs(nxb:nxe,j,k)+qs(nxb+1:nxe+1,j,k))                                                                                   
c                vh2_r6(nxb:nxe)=-vaas(nxb:nxe)*vxmu(nxb:nxe)/2.
                vh2_r6(nxb:nxe)=-vxmu(nxb:nxe)/2.
                yz(nxb:nxe,j,k)=yz(nxb:nxe,j,k)+vxmu(nxb:nxe)*(dth*vt6(nxb:nxe)+dth*vt7(nxb:nxe))+dt*r6(nxb:nxe,j,k)



                r1(nxb:nxe,j,k)=vx2(nxb:nxe)*r1(nxb:nxe,j,k)-
     + (weights_s(nxb:nxe)*vh1(nxb:nxe)*(vt2(nxb:nxe)+ vt3(nxb:nxe))*dh1-
     + weights_p(nxb:nxe)*va1(nxb:nxe))*vx1(nxb:nxe)*2.
                r2(nxb:nxe,j,k)=vx2(nxb:nxe)*r2(nxb:nxe,j,k)-
     + (weights_s(nxb:nxe)*vh1(nxb:nxe)*(vt1(nxb:nxe)+vt3(nxb:nxe))*dh1-           
     + weights_p(nxb:nxe)*va1(nxb:nxe))*vx1(nxb:nxe)*2.
                r3(nxb:nxe,j,k)=vx2(nxb:nxe)*r3(nxb:nxe,j,k)- 
     + (weights_s(nxb:nxe)*vh1(nxb:nxe)*(vt1(nxb:nxe)+vt2(nxb:nxe))*dh1-
     + weights_p(nxb:nxe)*va1(nxb:nxe))*vx1(nxb:nxe)*2.             
                r4(nxb:nxe,j,k)=vx2(nxb:nxe)*r4(nxb:nxe,j,k)+
     + weights_s1(nxb:nxe)*vh2_r4(nxb:nxe)*dh1*vt4(nxb:nxe)*vx1(nxb:nxe)*2.
                r5(nxb:nxe,j,k)=vx2(nxb:nxe)*r5(nxb:nxe,j,k)+
     + weights_s2(nxb:nxe)*vh2_r5(nxb:nxe)*dh1*vt5(nxb:nxe)*vx1(nxb:nxe)*2.
                r6(nxb:nxe,j,k)=vx2(nxb:nxe)*r6(nxb:nxe,j,k)+
     + weights_s3(nxb:nxe)*vh2_r6(nxb:nxe)*dh1*(vt6(nxb:nxe)+vt7(nxb:nxe))*
     + vx1(nxb:nxe)*2.

c                r1(nxb:nxe,j,k)=vx2(nxb:nxe)*r1(nxb:nxe,j,k)-
c     + (weights_s(nxb:nxe)*(vh1(nxb:nxe)*(vt2(nxb:nxe)+ vt3(nxb:nxe))*dh1-
c     + va1(nxb:nxe)))*vx1(nxb:nxe)*2.
c                r2(nxb:nxe,j,k)=vx2(nxb:nxe)*r2(nxb:nxe,j,k)-
c     + (weights_s(nxb:nxe)*(vh1(nxb:nxe)*(vt1(nxb:nxe)+vt3(nxb:nxe))*dh1-
c     + va1(nxb:nxe)))*vx1(nxb:nxe)*2.
c                r3(nxb:nxe,j,k)=vx2(nxb:nxe)*r3(nxb:nxe,j,k)-
c     + (weights_s(nxb:nxe)*(vh1(nxb:nxe)*(vt1(nxb:nxe)+vt2(nxb:nxe))*dh1-
c     + va1(nxb:nxe)))*vx1(nxb:nxe)*2.
c                r4(nxb:nxe,j,k)=vx2(nxb:nxe)*r4(nxb:nxe,j,k)+
c     + (weights_s(nxb:nxe)*vh2_r4(nxb:nxe)*dh1*vt4(nxb:nxe))*vx1(nxb:nxe)*2.
c                r5(nxb:nxe,j,k)=vx2(nxb:nxe)*r5(nxb:nxe,j,k)+
c     + (weights_s(nxb:nxe)*vh2_r5(nxb:nxe)*dh1*vt5(nxb:nxe))*vx1(nxb:nxe)*2.
c                r6(nxb:nxe,j,k)=vx2(nxb:nxe)*r6(nxb:nxe,j,k)+
c     + (weights_s(nxb:nxe)*vh2_r6(nxb:nxe)*dh1*(vt6(nxb:nxe)+vt7(nxb:nxe)))*
c     + vx1(nxb:nxe)*2.

                xx(nxb:nxe,j,k)=xx(nxb:nxe,j,k)+dt*r1(nxb:nxe,j,k)
                yy(nxb:nxe,j,k)=yy(nxb:nxe,j,k)+dt*r2(nxb:nxe,j,k)
                zz(nxb:nxe,j,k)=zz(nxb:nxe,j,k)+dt*r3(nxb:nxe,j,k)
                xy(nxb:nxe,j,k)=xy(nxb:nxe,j,k)+dt*r4(nxb:nxe,j,k)
                xz(nxb:nxe,j,k)=xz(nxb:nxe,j,k)+dt*r5(nxb:nxe,j,k)
                yz(nxb:nxe,j,k)=yz(nxb:nxe,j,k)+dt*r6(nxb:nxe,j,k)
50        continue
        enddo
      enddo

      return
      end

