CCC   'relax.f' CALCULATES RELAXATION ARRAY AND MODULI


      subroutine tausub(taumin,taumax)

CCC   CALCULATES THE RELAXATION TIME ARRAY

      use parstat

      integer, parameter :: ntau=2

      real, dimension(ntau,ntau,ntau) :: tautem
      real taumin,taumax

         tautem(1,1,1)=1.
         tautem(2,1,1)=6.
         tautem(1,2,1)=7.
         tautem(2,2,1)=4.
         tautem(1,1,2)=8.
         tautem(2,1,2)=3.
         tautem(1,2,2)=2.
         tautem(2,2,2)=5.

      do 55 idx = 1,ntau
        do 55 idy = 1,ntau
          do 55 idz = 1,ntau

            tmp=tautem(idx,idy,idz)

             tmp = (tmp-0.5)/8
             tmp = 2.0*tmp - 1.0

             tau(idx,idy,idz) = exp(0.5*(log(taumax*taumin) +
     +                          log(taumax/taumin)*tmp))

55     continue

       return
       end



      subroutine relaxmod(fp,fl,fh,vp,vs,qqp,qqs,tmin,tmax)

CCC   CALCULATE THE EFFECTIVE RELAXATION MODULI (BRADLEY 7/98)
CCC   REPLACES QS, QP ARRAYS WITH THE CALCULATED MODULI

        real tmax,tmin,w0,w1,w2,qpinv,qsinv

        pi=4.*atan(1.)

C radial frequency for the center, upper and lower bounds of the Q range

        w0=2*pi*fp
        w1=2*pi*fl
        w2=2*pi*fh

        tmax=1./w1
        tmin=1./w2

        if (qqp .le. 0.0) then
          qpinv=0.0
          qsinv=0.0
        else
          qpinv=1./qqp
          qsinv=1./qqs
        end if

        qqp = 2./pi*qpinv*(log(tmax/tmin))/
     +       (1.0-2./pi*qpinv*log(w0*tmin))

        qqs = 2./pi*qsinv*(log(tmax/tmin))/
     +       (1.0-2./pi*qsinv*log(w0*tmin))

        return
        end


      subroutine weights_sub(ex)

CCC   sets the weights dependeing on expoennt  

      use parstat

      integer, parameter :: ntau=2

c      real, dimension(ntau,ntau,ntau) :: weights
      real ex

cccccccccccccccccccccccccccccccccccc

c for ex=0.1                                                                                                                                                                                  
c     if ((ex-0.1) .LT. 0.01) then 
      if ((ex .LT. 0.15) .AND. (ex .GT. 0.01)) then
c 1,6,7,4,8,3,2,5   
c 400 hz range for freq-dep q                                                                                                                                                                  
         weights(1,1,1)=0.5574                                                                                                                                                                
          weights(2,1,1)=1.1952                                                                                                                                                               
         weights(1,2,1)=1.2418                                                                                                                                                                
         weights(2,2,1)=1.0234                                                                                                                                                                
          weights(1,1,2)=1.2973                                                                                                                                                               
         weights(2,1,2)=0.8302
         weights(1,2,2)=0.7193
         weights(2,2,2)=1.2744
         end if

c for ex=0.2                                                                                                                                                                                                                                              
c      if ((ex-0.2) .LT. 0.01) then
         if ((ex .LT. 0.25) .AND. (ex .GT. 0.15)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                         
c 400 hz range for freq-dep q                                                                                                                                                                                                                             
         weights(1,1,1)=0.2219                                                                                                                                                                                                                                  
          weights(2,1,1)=1.1793                                                                                                                                                                                                                                 
         weights(1,2,1)=1.2551                                                                                                                                                                                                                                  
         weights(2,2,1)=0.8376                                                                                                                                                                                                                                  
          weights(1,1,2)=1.2698                                                                                                                                                                                                                                 
         weights(2,1,2)=0.5575
         weights(1,2,2)=0.4094
         weights(2,2,2)=1.3413
         end if

c for ex=0.3                                                                                                                                                                                                                                                                                              
c         if ((ex-0.3) .LT. 0.01) then
         if ((ex .LT. 0.35) .AND. (ex .GT. 0.25)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                         
c 400 hz range for freq-dep q                                                                                                                                                                                                                                                                             
         weights(1,1,1)=0.0818                                                                                                                                                                                                                                                                                  
          weights(2,1,1)=1.1669                                                                                                                                                                                                                                                                                 
         weights(1,2,1)=1.2654                                                                                                                                                                                                                                                                                  
         weights(2,2,1)=0.6618                                                                                                                                                                                                                                                                                  
          weights(1,1,2)=1.2488                                                                                                                                                                                                                                                                                 
         weights(2,1,2)=0.3655
         weights(1,2,2)=0.2260
         weights(2,2,2)=1.3982
         end if

c for ex=0.4                                   
         if ((ex .LT. 0.45) .AND. (ex .GT. 0.35)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                                       
c 400 hz range for freq-dep q                                                                                                                                                                                                                                                                                           
         weights(1,1,1)=0.0305                                                                                                                                                                                                                                                                                                
          weights(2,1,1)=1.1576                                                                                                                                                                                                                                                                                               
         weights(1,2,1)=1.2727                                                                                                                                                                                                                                                                                                
         weights(2,2,1)=0.4988                                                                                                                                                                                                                                                                                                
          weights(1,1,2)=1.2337                                                                                                                                                                                                                                                                                               
         weights(2,1,2)=0.2347
         weights(1,2,2)=0.1189
         weights(2,2,2)=1.4460
         end if

c for ex=0.5                          
         if ((ex .LT. 0.55) .AND. (ex .GT. 0.45)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                                                    
c 400 hz range for freq-dep q                                                                                                                                                                                                                                                                                                        
         weights(1,1,1)=0.0193                                                                                                                                                                                                                                                                                                             
          weights(2,1,1)=1.1513                                                                                                                                                                                                                                                                                                            
         weights(1,2,1)=1.2774                                                                                                                                                                                                                                                                                                             
         weights(2,2,1)=0.3503                                                                                                                                                                                                                                                                                                             
          weights(1,1,2)=1.2242                                                                                                                                                                                                                                                                                                            
         weights(2,1,2)=0.1494
         weights(1,2,2)=0.0559
         weights(2,2,2)=1.4852
         end if

c for ex=0.6
         if ((ex .LT. 0.65) .AND. (ex .GT. 0.55)) then
c 1,6,7,4,8,3,2,5

         weights(1,1,1)=0.3112                                                                                                                                    
          weights(2,1,1)=0.8339                                                                                                                                   
         weights(1,2,1)=       0.1616                                                                                                                              
         weights(2,2,1)=1.0117                                                                                                                                    
          weights(1,1,2)=1.6821                                                                                                                                    
         weights(2,1,2)=0.0
         weights(1,2,2)=0.0
         weights(2,2,2)=0.7123

         coeff=(/ 8.0848,   -0.1968,-13.0365,    1.8101,25.4548,   -0.3947,
     + -10.4478 ,   7.6570,-75.9022  ,  6.1787,-13.2235,    6.3697,
     + 35.7155  ,  1.6800, -79.9715 ,  12.7318  /)

         end if

c for ex=0.7              
         if ((ex .LT. 0.75) .AND. (ex .GT. 0.65)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                                                    
c 320 hz range for freq-dep q                                                                                                                                                                                                                                                                                                        
         weights(1,1,1)=0.0021                                                                                                                                                                                                                                                                                                             
          weights(2,1,1)=1.0928                                                                                                                                                                                                                                                                                                            
         weights(1,2,1)=1.3062                                                                                                                                                                                                                                                                                                             
         weights(2,2,1)=0.1546                                                                                                                                                                                                                                                                                                             
          weights(1,1,2)=1.1057                                                                                                                                                                                                                                                                                                            
         weights(2,1,2)=0.0524
         weights(1,2,2)=0.0139
         weights(2,2,2)=1.5676
         end if

c for ex=0.8                                    
         if ((ex .LT. 0.85) .AND. (ex .GT. 0.75)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                                                    
c 320 hz range for freq-dep q                                                                                                                                                                                                                                                                                                        
         weights(1,1,1)=0.0462                                                                                                                                                                                                                                                                                                             
          weights(2,1,1)=0.001                                                                                                                                                                                                                                                                                                            
         weights(1,2,1)=0.4157                                                                                                                                                                                                                                                                                                             
         weights(2,2,1)=0.1585                                                                                                                                                                                                                                                                                                             
          weights(1,1,2)=1.3005                                                                                                                                                                                                                                                                                                            
         weights(2,1,2)=0.001
         weights(1,2,2)=0.001
         weights(2,2,2)=1.4986

         coeff= (/5.1672,0.2129, -46.506,11.7213,-5.8873,1.4279,
     +  -8.2448,0.3455,15.0254,-0.283,0.0,0.0,58.975,0.8131,
     + -108.6828,12.4362/)


         end if

c for ex=0.9    
         if ((ex .LT. 0.95) .AND. (ex .GT. 0.85)) then
c 1,6,7,4,8,3,2,5                                                                                                                                                                                                                                                                                                                    
c 280 hz range for freq-dep q                                                                                                                                                                                                                                                                                                        
         weights(1,1,1)=0.0                                                                                                                                                                                                                                                                                                             
          weights(2,1,1)=1.0532                                                                                                                                                                                                                                                                                                            
         weights(1,2,1)=1.3298                                                                                                                                                                                                                                                                                                             
         weights(2,2,1)=0.000                                                                                                                                                                                                                                                                                                             
          weights(1,1,2)=1.0209                                                                                                                                                                                                                                                                                                            
         weights(2,1,2)=0.0110
         weights(1,2,2)=0.0023
         weights(2,2,2)=1.6299
         end if


         if ((ex .LT. 0.01)) then
c 1,6,7,4,8,3,2,5                                                             
c 280 hz range for freq-dep q                                                                                             
         weights(1,1,1)=0.8867                                                                                                  
          weights(2,1,1)=1.0440                                                                                        
         weights(1,2,1)=0.0423                                                                                                  
         weights(2,2,1)=0.8110                                                                                                  
          weights(1,1,2)=1.7275                                                                                                 
         weights(2,1,2)=0.5615
         weights(1,2,2)=0.8323
         weights(2,2,2)=0.4641

         coeff=(/   -27.8824,    7.4360,  -82.3297,   13.1682,71.5308,    0.5543,
     + -33.7290,    6.0073, -51.9038 ,   8.1580,-1.8237,    4.6954,-27.5813,    6.2756,
     +  14.3820,    3.8960/)

         end if
cccccccccccccccccccccccccccccccccccccc
c      do 55 idx = 1,ntau
c        do 55 idy = 1,ntau
c          do 55 idz = 1,ntau

c            tmp=tautem(idx,idy,idz)

c             tmp = (tmp-0.5)/8
c             tmp = 2.0*tmp - 1.0

c             tau(idx,idy,idz) = exp(0.5*(log(taumax*taumin) +
c     +                          log(taumax/taumin)*tmp))

c 55               continue

       return
       end
