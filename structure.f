
CCC   There are four options:
CCC   0. homogeneous mode
CCC   1. serial IO mode(small case) - the mesh file is specifined in IN3D
CCC   2. MPI IO mode(large case) - the mesh file is specifined in IN3D (!!! NOTE: npx*npy*npz >= nz !!!)
CCC   3. partitioned mode - partioned files mediaxxxxx.bin (where xxxxx is rank) 
CCC      are located at /input_rst/mediapart/
CCC   4. post-processed partitioned mode - partitioned files mediaIxxxxx.bin and
CCC      maxmin file mediaImaxmin.bin are located at /input_rst/mediapart/
CCC
CCC   nvar is for selecting number of variables in a mesh point. 3,5 and 8 is currently possible.
CCC

      subroutine inimesh(rank,ranksize,comm,master,invel,dims,coords,maxdim,
     +                   npx,npy,npz,nxt,nyt,nzt,nx,ny,nz,vpe,vse,dde,taumin,taumax,
     +                   fac,Q0,ex,fp,bsize,y_dt,idyna,nve,readoption,nvar,iost,partdeg,SoCalQ,vp,vs,rho)

CCC   ROUTINE TO INITIALIZE SPACE WITH PREPPED 3D STRUCTURE
      use parstat
      implicit none

      integer, parameter :: filen=180
      character (len=filen) :: mediafile,invel,meshfile

      complex value,value1,M1,sqrtm1 !KW
      real Mu1 ! KW
c      real, dimension(8) :: weights
      integer :: maxdim
      real :: taumax,taumin, fac,Q0,ex, alphak, betak
      integer(i8) ::  p,r,e,f,g,l,m,n
      integer :: npx,npy,npz, nxt,nyt,nzt, nx,ny,nz, nxn,nyn,nzn, nxp,nyp,nzp
      integer(i8) :: nxt_l,nyt_l,nzt_l,ny_l,nx_l,nvar_l
      integer(i8), dimension (3):: dims_l
      integer(i8) :: i,j,k,a,b,c,ii,jj,kk,num,iii,jjj,kkk
      integer :: rank,ranksize,comm,master,err
      integer :: nxb,nxe,nyb,nye,nzb,nze
      integer :: itx,ity,itz
      real :: tmax,tmin,w0,ww1,w2,qpinv,qsinv,pi
      integer, dimension(maxdim) :: dims,coords
      real :: vp,vs,rho

      real, dimension(2) :: vpe,vse,dde,tmpvpe,tmpvse,tmpdde
      real :: fl,fh,fp
      integer :: bsize

      real :: tmp1, tmp2
      real :: vpvs
      real :: rat
      integer :: y_dt,idyna,nve,readoption,SoCalQ

!     for option 1
      integer :: buf, tag

!     for option 2
      integer(KIND=MPI_ADDRESS_KIND),parameter :: EIGHT = 8
      integer(KIND=MPI_ADDRESS_KIND),parameter :: FIVE = 5
      integer(KIND=MPI_ADDRESS_KIND),parameter :: WSIZE = 4

      integer(KIND=MPI_ADDRESS_KIND) :: partdeg_l
      integer :: partdeg, est_partdeg, yindex
      real :: partdeg_f

      integer :: x,y,z,nvar,act_nvar,var_offset,sourcerank,mpirank,zrank,flipped_zrank
      real*8 :: t1,t2,t3,t4,t5,t6,total
      integer :: COMM_IO, COMM_RECV

      integer :: zpos
      
      integer :: varnum(1), bsize2
      real, dimension (:), allocatable :: tmpx
      real, dimension (:,:,:,:,:), allocatable :: tmpx2
      real, dimension (:,:,:,:,:), allocatable :: xyplane
      real, dimension (:,:,:,:), allocatable :: cube

      integer :: merr
      integer(MPI_OFFSET_KIND) :: disp
      integer :: PLANEUNIT, CUBEPLANEUNIT
      integer :: mfh
      integer,dimension(MPI_STATUS_SIZE) :: mystatus

      INTEGER, dimension(:), allocatable :: requests
      INTEGER, dimension(:,:), allocatable :: mpistatus
      
!     for option 3 
      integer :: iost,iwrite

      if (rank.eq.0) print *, "inimesh - ", fl,fh,fp,bsize
      if (readoption.eq.0) then
          if (rank.eq.0) print *, "inimesh - ", vp,vs,rho
      endif
      if (rank.eq.0) print *, "inimesh - ", nve,readoption,nvar,SoCalQ     

      call MPI_BCAST(invel,filen,MPI_CHARACTER,0,comm,merr)        

      ! CASE 0: Homogeneous Mesh Information
      if (readoption.eq.0) then
        if (rank.eq.0) print *, "initmesh - CASE 0, homogeneous mesh"
        pi=4.*atan(1.)
        
c        taumax=1./(0.01)*10.0*fac !K.W. original frequncy range is 0.01 to 40
c        taumin=1./(14)*.1*fac
        call inihomo(nxt,nyt,nzt,vpe,vse,dde,y_dt,idyna,vp,vs,rho)
      else
        allocate(tmpvp(1:nxt,1:nyt,1:nzt))
        allocate(tmpvs(1:nxt,1:nyt,1:nzt))
        allocate(tmpdd(1:nxt,1:nyt,1:nzt))
        allocate(tmppq(1:nxt,1:nyt,1:nzt))
        allocate(tmpsq(1:nxt,1:nyt,1:nzt))

        tmpvp=0
        tmpvs=0
        tmpdd=0
        tmppq=0
        tmpsq=0

        if (nvar.eq.8) then
          var_offset=3
          act_nvar=5
        else if (nvar.eq.5) then
          var_offset=0
          act_nvar=5
        else if (nvar.eq.3) then
          var_offset=0
          act_nvar=3
        end if

        if (rank.eq.0) then ! master
          print *,'nxt,nyt,nzt:',nxt,nyt,nzt
          print *,'nx,ny,nz:',nx,ny,nz
          print *,'nvar:',nvar
          print *,'IOST:',iost
          print *,'partdeg:',partdeg
          print *,'invel:',invel
        end if

        select case (readoption)
          ! CASE 1: Reading Single Mesh File with serial IO (small scale)
          ! Currently nve=1 is supported only
          case (1)
            if (rank.eq.0) print *, "initmesh - CASE 1, serial IO - for small files"

            if (rank.ne.0) then
              call MPI_RECV(buf,1,MPI_INTEGER,rank-1,MPI_ANY_TAG,comm,mystatus,merr)
            else
              close(27)
            end if

            allocate(tmpx(1:nxt*nvar))

            if (nve==1) then
              open(100+rank,file=invel,access='direct',form='unformatted',
     +             recl=nvar*4*nxt)
            end if
            x=rank/(npy*npz)
            y=(rank-x*npz*npy)/npz
            z=mod(rank-x*npy*npz,npz)
            z=(npz-z-1)
            do i=1,nzt
              do j=1,nyt
                m=z*npx*ny*nzt +
     +            y*npx*nyt +
     +            x +
     +            (j-1)*npx +
     +            (i-1)*npx*ny +
     +            1

                !print *, i, j, "-", m
                read(100+rank,rec=m) tmpx

                do k=1,nxt
                  tmpvp(k,j,i) = tmpx((k-1)*nvar+var_offset+1)
                  tmpvs(k,j,i) = tmpx((k-1)*nvar+var_offset+2)
                  tmpdd(k,j,i) = tmpx((k-1)*nvar+var_offset+3)
                  if (nvar.gt.3) then
                    tmppq(k,j,i) = tmpx((k-1)*nvar+var_offset+4)
                    tmpsq(k,j,i) = tmpx((k-1)*nvar+var_offset+5)
                  end if
                end do
              end do
            end do

            close(100+rank)

            deallocate(tmpx)

            if (rank.lt.(ranksize-1)) then
              call MPI_SEND(buf,1,MPI_INTEGER,rank+1,1234,comm,merr)
            end if

            print *, "inimesh - rank", rank

          ! CASE 2: Reading Single Mesh File with MPIIO (large scale)
          case (2)
            if (rank.eq.0) print *, "initmesh - CASE 2, MPI - for big files"

            nx_l=nx
            ny_l=ny
            nvar_l=nvar

            allocate(requests(1:npx*npy+nzt))
            allocate(mpistatus(MPI_STATUS_SIZE,1:npx*npy+nzt))

            zrank=rank   ! 0..(nz-1)
 
            ! maximum 1GB for temporal mesh buffers
            partdeg_f=real(nx_l*ny_l*(nvar_l+act_nvar)*4)/real(1024*1024*1024)
            if (rank.eq.0) print *,partdeg_f,"GB"
            if (partdeg_f.gt.1) then
              est_partdeg=floor(log(partdeg_f)/log(2.))+1
              est_partdeg=2**est_partdeg
            else
              est_partdeg=1
            end if

	    ! verify the ny and npy are divisible by partdeg and npx*npy*npz >= nz*partdeg
            partdeg_l=partdeg
            if (rank.eq.0) print *,"Suggested amount of partitioning", est_partdeg
            if (rank.eq.0) print *,"Actual amount of partitioning", partdeg_l

            if (zrank<nz*partdeg) then
              call MPI_COMM_SPLIT(comm,1,0,COMM_IO,merr)        
              call MPI_FILE_OPEN(COMM_IO,invel,MPI_MODE_RDONLY,MPI_INFO_NULL,mfh,merr)

              ! new data type containing a contiguous data chunks
              call MPI_Type_contiguous(nx_l*(ny_l/partdeg_l)*nvar_l,MPI_REAL,PLANEUNIT,merr)
              call MPI_Type_commit(PLANEUNIT,merr)

              allocate(tmpx2(nvar,1:nxt,1:npx,1:nyt,1:npy/partdeg))        
              allocate(xyplane(act_nvar,1:nxt,1:nyt,1:npx,1:npy/partdeg))

              t1=mpi_wtime()
              disp=int(zrank,MPI_ADDRESS_KIND)*nx_l*(ny_l/partdeg_l)*nvar_l*WSIZE ! inverted z-axis, an absolute offset in bytes
              call MPI_File_set_view(mfh,disp,MPI_REAL,PLANEUNIT,"native",MPI_INFO_NULL,merr)
              disp=0
              if (rank.eq.0) print *, "read_at_all", rank
              call MPI_File_read_at_all(mfh,disp,tmpx2,nx*(ny/partdeg)*nvar,MPI_REAL,mystatus,merr)

              if (rank.eq.0) print *, "read_at_all passed", rank
              t2=mpi_wtime()

              !call MPI_BARRIER(COMM_IO, merr)

              if (rank.eq.0) then ! master
                !write (*,'("Reading time =",f10.3," sec")') t2-t1
                print *,"Reading time =",t2-t1," sec"
              end if

              call MPI_FILE_CLOSE(mfh,merr)
              allocate(cube(act_nvar,1:nxt,1:nyt,1:nzt))

              yindex=mod(zrank,partdeg)
              do i=1,npy/partdeg
                do j=1,npx
                  do k=1,nyt
                    do l=1,nxt
                      xyplane(1,l,k,j,i)=tmpx2(var_offset+1,l,j,k,i)
                      xyplane(2,l,k,j,i)=tmpx2(var_offset+2,l,j,k,i)
                      xyplane(3,l,k,j,i)=tmpx2(var_offset+3,l,j,k,i)
                      if (nvar.gt.3) then
                        xyplane(4,l,k,j,i)=tmpx2(var_offset+4,l,j,k,i)
                        xyplane(5,l,k,j,i)=tmpx2(var_offset+5,l,j,k,i)
                      end if
                    end do
                  end do
                end do
              end do

              t3=mpi_wtime()
              if (rank.eq.0) then ! master          
                !write (*,'("Restructuring time =",f10.3," sec")') t3-t2
                print *,"Reconstructing time =",t3-t2," sec"
              end if
  
              !call MPI_BARRIER(COMM_IO, merr)

              ! DISTRIBUTING.....
              zpos=(zrank/partdeg)/nzt
              do i=yindex*(npy/partdeg)+1,(yindex+1)*npy/partdeg
                do j=1,npx
                  k=(i-yindex*(npy/partdeg)-1)*npx+j
                  
                  ! changing coordinate system: regular X-Y-Z -> MPI Z-Y-X 
                  z=npz-zpos-1   ! flip z-axis of the cubes
                  mpirank=(j-1)*npy*npz+(i-1)*npz + z
                  
                  !if (mod(zrank,nzt).eq.0) print *,"S",rank,mpirank,i,j,z

                  call MPI_ISEND(xyplane(1,1,1,j,i-yindex*(npy/partdeg)),nxt*nyt*act_nvar,MPI_REAL,
     $                           mpirank,mod(zrank/partdeg,nzt)+1,comm,requests(k),merr)
                end do
              end do

              ! RECEIVING..... ! do we need flipping z-axis?
              x=rank/(npy*npz)
              y=(rank-x*npy*npz)/npz
              z=mod(rank-x*npy*npz,npz)
              yindex=y/(npy/partdeg)

              z=npz-z-1   ! flip z-axis of the cubes

              !print *,"SR",rank,sourcerank,x,y,z

              do i=1,nzt
                call MPI_IRECV(cube(1,1,1,i),nxt*nyt*act_nvar,MPI_REAL,
     $                        (z*nzt+i-1)*partdeg+yindex,i,comm,requests(i+npx*npy/partdeg),merr)
              end do
              CALL MPI_WAITALL(npx*npy/partdeg+nzt,requests,mpistatus,merr)

              deallocate(tmpx2)
            else
              call MPI_COMM_SPLIT(comm,2,0,COMM_RECV,merr)        
              allocate(cube(act_nvar,1:nxt,1:nyt,1:nzt))

              ! RECEIVING..... ! do wee need flipping z-axis?
              x=rank/(npy*npz)
              y=(rank-x*npy*npz)/npz
              z=mod(rank-x*npy*npz,npz)
              yindex=y/(npy/partdeg)

              z=npz-z-1   ! flip z-axis of the cubes

              !print *,"R", rank,sourcerank,x,y,z

              do i=1,nzt
                call MPI_IRECV(cube(1,1,1,i),nxt*nyt*act_nvar,MPI_REAL,
     $                        (z*nzt+i-1)*partdeg+yindex,i,comm,requests(i),merr)
              end do
              CALL MPI_WAITALL(nzt,requests,mpistatus,merr)
            end if

            call MPI_BARRIER(comm, merr)
            t4=mpi_wtime()
            if (rank.eq.0) then ! master
              !write (*,'("Reception time =",f10.3," sec")') t4-t3
              print *,"Reception time =",t4-t3," sec"
            end if

            if (zrank<nz*partdeg) then
              deallocate(xyplane)
            end if

            ! For test purpose only
!            do iwrite=0,IOST
!              if (mod(rank,IOST+1).eq.iwrite) then
!                write(mediafile, '(a,i7.7,a)') 'input_rst/meshpart/media', rank,'.bin'
!                open(9, file=mediafile, form='unformatted', access='direct',
!     $               status='replace',recl=bsize*(nxt)*(nyt)*(nzt)*act_nvar)
!                write(9, rec=1) cube
!                close(9)
!              end if
!              call MPI_BARRIER(comm,merr)
!            end do
      
            t5=mpi_wtime()
            if (rank.eq.0) then ! master
              !write (*,'("Writng file time =",f10.3," sec")') t5-t4
              print *,"Writing file time =",t5-t4," sec"
            end if

            do i=1,nzt
              do j=1,nyt
                do k=1,nxt
                  tmpvp(k,j,i)=cube(1,k,j,i)
                  tmpvs(k,j,i)=cube(2,k,j,i)
                  tmpdd(k,j,i)=cube(3,k,j,i)
                  if (nvar.gt.3) then
                    tmppq(k,j,i)=cube(4,k,j,i)
                    tmpsq(k,j,i)=cube(5,k,j,i)
                  end if
                end do
              end do
            end do

            deallocate(cube)
            deallocate(requests)
            deallocate(mpistatus)

          ! CASE 3: Reading partitioned Mesh File (mediaxxxxx.bin)
          case (3)
            if (rank.eq.0) print *, "initmesh - CASE 3, partitioned files"
            allocate(cube(act_nvar,1:nxt,1:nyt,1:nzt))
        
            ! Maximally allow IOST amount of readers
            do iwrite=0,IOST
              if (rank.eq.0) print *,iwrite,"iterations out of ", IOST
              if (mod(rank,IOST+1).eq.iwrite) then
                write(mediafile, '(a,i7.7,a)') 'input_rst/mediapart/media', rank,'.bin'
                if(rank .eq. 0) write( 0, *) 'Read media file original format'
                open(9, file=mediafile, form='unformatted', access='direct',
     $               recl=bsize*nxt*nyt*nzt*nvar, status='old',
     $               iostat=err)
C               read vp,vs,dd,pq,sq
                read(9, rec=1) cube
                close(9)
              end if
              call MPI_BARRIER(comm,merr)  
            end do

            do i=1,nzt
              do j=1,nyt
                do k=1,nxt
                  tmpvp(k,j,i)=cube(1,k,j,i)
                  tmpvs(k,j,i)=cube(2,k,j,i)
                  tmpdd(k,j,i)=cube(3,k,j,i)
                  if (nvar.gt.3) then
                    tmppq(k,j,i)=cube(4,k,j,i)
                    tmpsq(k,j,i)=cube(5,k,j,i)
                  end if
                end do
              end do
            end do

            deallocate(cube)

          ! CASE 4: Reading partitioned Preprocessed Mesh File (mediaIxxxxx.bin and mediaImaxmin.bin)
          case (4)
            if (rank.eq.0) print *, "initmesh - CASE 4, pre-processed partitioned files"
            write(mediafile, '(a,i7.7,a)') 'input_rst/mediapart/mediaI', rank,'.bin'
            if(rank.eq.0) print *, 'Read media file'
            open(9, file=mediafile, form='unformatted', access='direct',
     $          recl=bsize*(nxt+4)*(nyt+4)*(nzt+4)*5, status='old',
     $          iostat=err)
            read(9, rec=1) d1,mu,lam,qp,qs
            close(9)
            pi=4.*atan(1.)
C            taumax=1./(2*pi*fl)
C            taumin=1./(2*pi*fh)

c            taumax=1./(0.01)*10.0*fac !K.W. original frequncy range is 0.1 to 40 
c            taumin=1./(40)*.1*fac
C           11/24/2008 Kwangyoon added code to use global maxmin file named mediamaxmin.bin
C           Local maxmin files are still supported for compatibility purpose
            write(mediafile, '(a,i7.7,a)') 'input_rst/mediapart/mediaImaxmin', rank, '.bin'
            open(9, file=mediafile, form='unformatted', access='direct',
     $           status='old',recl=bsize*6,iostat=err)
            if (err.eq.0) then ! no error
              if(rank.eq.0) write(0, *) 'Reading Local maxmin media file'
              read(9, rec=1) vse(1),vse(2),vpe(1),vpe(2),dde(1),dde(2)
              close(9)
            else
              write(mediafile, '(a)') 'input_rst/mediapart/mediaImaxmin.bin'
              if (rank.eq.0) then
                print *, 'Reading global maxmin media file'
                open(9, file=mediafile, form='unformatted', access='direct',
     $               status='old',recl=bsize*6,iostat=err)
                read(9, rec=1) vse(1),vse(2),vpe(1),vpe(2),dde(1),dde(2)
                close(9)
              end if
              call MPI_BCAST(vse,2,MPI_REAL,master,comm,err)
              call MPI_BCAST(vpe,2,MPI_REAL,master,comm,err)
              call MPI_BCAST(dde,2,MPI_REAL,master,comm,err)
            end if

          case default
            if (rank.eq.0) print *, "unsupported mediarestart option is chosen!!!!"

        end select
      end if

c       weights=weights/(fac**ex)
!     for CASES 1,2,3 and generates dl,mu,lam,qs,qp and vpe,vse,dde
      if ((readoption.gt.0).and.(readoption.lt.4)) then
        
        ! Verify this!!!!
c        if (nvar.eq.3) then
c          tmpsq = 0.05 * tmpvs
c          tmppq = 2.0 * tmpsq
c          tmpsq=50.
c          tmppq=50.
c        end if
c        print *, 'tmpsq,tmppq',tmpsq(1,1,1),tmppq(1,1,1)

        pi=4.*atan(1.)
        w0=2*pi*fp
        !print *, 'w0',w0
C        ww1=2*pi*fl
C        w2=2*pi*fh
c        ww1=0.01*.1*fac
c        w2=100*10*fac

c        taumax=1./ww1
c        taumin=1./w2

      sqrtm1=(0.0,1.0)

        vpe(1)=99999.
        vse(1)=99999.
        dde(1)=99999.
c calculate d1,mu,lam,qp,qs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do i=1,nxt
          do j=1,nyt
            do k=1,nzt

c             cap velocities
            vpvs = tmpvp(i,j,k) / tmpvs(i,j,k)
            if (vpvs .lt. 1.45)  then 
                tmpvs(i,j,k) = tmpvp(i,j,k) / 1.45
            elseif (vpvs .gt. 3) then 
                tmpvs(i,j,k) = tmpvp(i,j,k) / 3.0
            endif
            ! min vp = 725 m/s 
            ! max vp = 1500 m/s
            if (tmpvs(i,j,k) .lt. 500.0) then 
                tmpvs(i,j,k) = 500.0
                tmpvp(i,j,k) = tmpvs(i,j,k) * vpvs
            endif
            ! min vs = 500 m/s
            ! max vs = 1034 m/s
            if (tmpvp(i,j,k) .lt. 1500.0) then
                tmpvp(i,j,k) = 1500.0
c               tmpvs(i,j,k) = tmpvp(i,j,k) / vpvs
            endif
            !  if (tmpvs(i,j,k).lt.500.) then
            
            !    tmpvs(i,j,k) = 500.
            !    tmpvp(i,j,k) = 1700.
            !  endif

            tmpsq(i,j,k) = 0.1 * tmpvs(i,j,k)
            tmppq(i,j,k) = 2.0 * tmpsq(i,j,k)

              if (tmpsq(i,j,k) .lt. 200.0) then
c              weights=weights_lo+(tmpsq(i,j,k)-5)..... extraploation formula
              num=0
              do ii=1,2
                 do jj=1,2
                    do kk=1,2
                    weights_los(ii,jj,kk)=coeff(1+num)/(tmpsq(i,j,k)**2)+coeff(2+num)/(tmpsq(i,j,k))

c                    weights_los(ii,jj,kk)=coeff(1+num)*tmpsq(i,j,k)**4.+coeff(2+num)*tmpsq(i,j,k)**3.+                                                             
c     +                     coeff(3+num)*tmpsq(i,j,k)**2.+coeff(4+num)*tmpsq(i,j,k)+coeff(5+num);  
                     num=num+2
                     end do
                     end do
                     end do

      value=(0.0,0.0)                                                                                                                                                                                                                             
      do ii=1,2                                                                                                                                                                                                                                   
         do jj=1,2                                                                                                                                                                                                                                 
          do kk=1,2             
             value=value+1./(    1.-weights_los(ii,jj,kk)/(1+sqrtm1*w0*tau(ii,jj,kk)))
               end do                                                                                                                                                                                                                              
               end do                                                                                                                                                                                                                              
               end do                                                                                                                                                                                                                              
               value=1./value
                                                                                                                                                                                                                                                                  
c calulate the unrelax modulus corresonding to vp and then the velocity at infinite frequncy                                                                                                                                                       
c should be inverse of real )value
                 Mu1=tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k)/(8.*real(value))                                                                                                                                                                       
c                 Mu1=tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k)/(abs(8.*value))
                 tmpvs(i,j,k)=sqrt(Mu1/tmpdd(i,j,k)) 

                 else


c for VS                                                                                                                                                                                                                                           
      value=(0.0,0.0)                                                                                                                                                                                                                              
        do ii=1,2                                                                                                                                                                                                                                  
         do jj=1,2                                                                                                                                                                                                                                 
          do kk=1,2                                                                                                                                                                                                                                
           value=value+weights(ii,jj,kk)/((sqrtm1*w0*tau(ii,jj,kk)+1)*tmpsq(i,j,k)*fac**ex)                                                                                                                                                       
c             value=value+1./(    1.-weights(ii,jj,kk)/(1+sqrtm1*w0*tau(ii,jj,kk)))   
               end do                                                                                                                                                                                                                              
               end do                                                                                                                                                                                                                              
               end do                                                                                                                                                                                                                              
c             value=1./value                                                                                                                                                                                                                                      
c                 Mu1=tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k)/(1.-real(value))                                                                                                                                                                       
                 Mu1=tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k)/(abs(1.-value)) 
c              Mu1=tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k)/(abs(8.*value)) 
                 tmpvs(i,j,k)=sqrt(Mu1/tmpdd(i,j,k))   

              endif



              if (tmppq(i,j,k) .lt. 200.0) then                                                                                                                                                                                                  
c              weights=weights_lo+(tmppq(i,j,k)-5)..... extraploation formula                                                                                                                                                                      
              num=0                                                                                                                                                                                                                                
              do ii=1,2                                                                                                                                                                                                                            
                 do jj=1,2                                                                                                                                                                                                                         
                    do kk=1,2                                                                                                                                                                                                                      
                       weights_lop(ii,jj,kk)=coeff(1+num)/(tmppq(i,j,k)**2)+coeff(2+num)/(tmppq(i,j,k))
                     num=num+2                                                                                                                                                                                                                     
                     end do                                                                                                                                                                                                                        
                     end do                                                                                                                                                                                                                        
                     end do                                                                                                                                                                                                                       


      value=(0.0,0.0)                                                                                                                                                                                                                              
      do ii=1,2                                                                                                                                                                                                                                    
         do jj=1,2                                                                                                                                                                                                                                
          do kk=1,2                                                                                                                                                                                                                                
c             alphak=1.-weights_lop(ii,jj,kk)/(w0**2.*tau(ii,jj,kk)**2+1.)                                                                                                                                                                      
c             betak=weights_lop(ii,jj,kk)*w0*tau(ii,jj,kk)/(w0**2.*tau(ii,jj,kk)**2+1.)                                                                                                                                                            
c             value=value+(alphak/(alphak**2+betak**2)-sqrtm1*betak/(alphak**2+betak**2))**-1.                                                                                                                                                     
             value=value+1./(    1.-weights_lop(ii,jj,kk)/(1+sqrtm1*w0*tau(ii,jj,kk))) 
               end do                                                                                                                                                                                                                              
               end do                                                                                                                                                                                                                              
               end do                                                              
                value=1./value
                                                                                                                                                                                                                                                   
c calulate the unrelax modulus corresonding to vp and then the velocity at infinite frequncy                                                                                                                                                       
                 Mu1=tmpdd(i,j,k)*tmpvp(i,j,k)*tmpvp(i,j,k)/(8.*real(value))                                                                                                                                                                     
c                  Mu1=tmpdd(i,j,k)*tmpvp(i,j,k)*tmpvp(i,j,k)/(abs(8.*value))
                 tmpvp(i,j,k)=sqrt(Mu1/tmpdd(i,j,k)) 

                 else
      value=(0.0,0.0)
        do ii=1,2                                                                                                                              
         do jj=1,2                                                                                                                             
          do kk=1,2                                                                                                                            
            value=value+weights(ii,jj,kk)/((sqrtm1*w0*tau(ii,jj,kk)+1)*tmppq(i,j,k)*fac**ex)                                                  
c             value=value+1./(    1.-weights(ii,jj,kk)/(1+sqrtm1*w0*tau(ii,jj,kk))) 
               end do                                                                                                                         
               end do                                                                                                                         
               end do 
c               value=1./value  

c kyle: change next 2 lines to case where dispersion is changed
c                tmpvs(i,j,k) = tmpvs(i,j,k)*(1+ ( log(w2/w0) )/(pi*tmpsq(i,j,k)) )
c                tmpvp(i,j,k) = tmpvp(i,j,k)*(1+ ( log(w2/w0) )/(pi*tmppq(i,j,k)) )
c calulate the unrelax modulus corresonding to vp and then the velocity at infinite frequncy
                 Mu1=tmpdd(i,j,k)*tmpvp(i,j,k)*tmpvp(i,j,k)/(abs(1.-value))
c               Mu1=tmpdd(i,j,k)*tmpvp(i,j,k)*tmpvp(i,j,k)/(abs(8.*value))
                 tmpvp(i,j,k)=sqrt(Mu1/tmpdd(i,j,k))

                 endif 


              ! only if SoCalQ flag is on                                                                                                                            
              !if (SoCalQ.eq.1) then 
              !end if ! SoCalQ  
              !if (SoCalQ.eq.1) then 
              !  vpvs=tmpvp(i,j,k)/tmpvs(i,j,k)
              !  if (vpvs .lt. 1.45)  tmpvs(i,j,k)=tmpvp(i,j,k)/1.45
              !end if ! SoCalQ  

              !if (tmpvp(i,j,k).gt.8000) then
              !  rat = 8000./tmpvp(i,j,k)
              !  tmpvs(i,j,k)=tmpvs(i,j,k)*rat
              !  tmpvp(i,j,k)=tmpvp(i,j,k)*rat
              !endif

              if (tmpdd(i,j,k).lt.1700.) tmpdd(i,j,k)=1700. 
              mu(i,j,nzt+1-k) = 1./(tmpdd(i,j,k)*tmpvs(i,j,k)*tmpvs(i,j,k))
              lam(i,j,nzt+1-k) = 1./(tmpdd(i,j,k)*(tmpvp(i,j,k)*tmpvp(i,j,k)-2.*tmpvs(i,j,k)*tmpvs(i,j,k)))
              d1(i,j,nzt+1-k) = tmpdd(i,j,k)


              if(nve==1) then
                if (tmppq(i,j,k) .le. 0.0) then
                  qpinv=0.0
                  qsinv=0.0
                else
                  qpinv=1./tmppq(i,j,k)
                  qsinv=1./tmpsq(i,j,k)
                end if



! KW here I change As and Ap for frequncy dpendent Q
!                tmppq(i,j,k)=tmp1*qpinv/(1.0-tmp2*qpinv)
!                tmpsq(i,j,k)=tmp1*qsinv/(1.0-tmp2*qsinv)

!                tmppq(i,j,k)=qpinv*   (1/Q0)*fac**(-ex)
!                 tmpsq(i,j,k)=qsinv*   (1/Q0)*fac**(-ex)
! Ap and As

!!!!!
 
                tmppq(i,j,k)=qpinv*fac**(-ex)                                                                                      
                 tmpsq(i,j,k)=qsinv*fac**(-ex)
c                 print *, 'tmppq,tmpsq',tmppq(1,1,1),tmpsq(1,1,1)

                qp(i,j,nzt+1-k) = tmppq(i,j,k)
                qs(i,j,nzt+1-k) = tmpsq(i,j,k)
c               qp(i,j,k) = tmppq(i,j,k)
c               qs(i,j,k) = tmpsq(i,j,k)
              endif
              if(tmpvs(i,j,k).lt.vse(1)) vse(1) = tmpvs(i,j,k)
              if(tmpvs(i,j,k).gt.vse(2)) vse(2) = tmpvs(i,j,k)
              if(tmpvp(i,j,k).lt.vpe(1)) vpe(1) = tmpvp(i,j,k)
              if(tmpvp(i,j,k).gt.vpe(2)) vpe(2) = tmpvp(i,j,k)
              if(tmpdd(i,j,k).lt.dde(1)) dde(1) = tmpdd(i,j,k)
              if(tmpdd(i,j,k).gt.dde(2)) dde(2) = tmpdd(i,j,k)
            end do
          end do
        end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c       1,nxt,1,nyt,1,nzt
c        nxb=1
c        nxe=nxt
c        nyb=1
c        nye=nyt
c        nzb=1
c        nze=nzt



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c                 print *, 'Mu1,tmpvp',Mu1,tmpvp(1,1,1) 
c               print *, 'valuevp',value
c               print *, 'valuevs',value
c                 print *, 'mu1:,value,tmpvs,:', Mu1,value,tmpvs(1,1,1) 
c                 print *, 'tmppq,tmpsq',tmppq(1,1,1),tmpsq(1,1,1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        call padmedia(nxt,nyt,nzt)
        call afsmedia(coords,dims,maxdim,nxt,nyt,nzt,nve)

        deallocate(tmpvp)
        deallocate(tmpvs)
        deallocate(tmpdd)
        deallocate(tmppq)
        deallocate(tmpsq)

        call MPI_BARRIER(comm,err)
        call MPI_ALLREDUCE(vse,tmpvse,2,MPI_REAL,MPI_MAX,comm,err)
        call MPI_ALLREDUCE(vpe,tmpvpe,2,MPI_REAL,MPI_MAX,comm,err)
        call MPI_ALLREDUCE(dde,tmpdde,2,MPI_REAL,MPI_MAX,comm,err)
        vse(2)=tmpvse(2)
        vpe(2)=tmpvpe(2)
        dde(2)=tmpdde(2)

        call MPI_ALLREDUCE(vse,tmpvse,2,MPI_REAL,MPI_MIN,comm,err)
        call MPI_ALLREDUCE(vpe,tmpvpe,2,MPI_REAL,MPI_MIN,comm,err)
        call MPI_ALLREDUCE(dde,tmpdde,2,MPI_REAL,MPI_MIN,comm,err)
        vse(1)=tmpvse(1)
        vpe(1)=tmpvpe(1)
        dde(1)=tmpdde(1)
      end if

      return
      end

      subroutine padmedia(nxt,nyt,nzt)
      use parstat
      implicit none

      integer :: nxt,nyt,nzt
      integer :: i,j,k

C     avoids wrong initialization for the outermost cells

      ! 5 Planes (except upper XY-plane, which is duplicated in afsmedia)
      do k=1,nzt
        do j=1,nyt
          lam(0,j,k) = lam(1,j,k)
          lam(nxt+1,j,k) = lam(nxt,j,k)
          mu(0,j,k) = mu(1,j,k)
          mu(nxt+1,j,k) = mu(nxt,j,k)
          d1(0,j,k) = d1(1,j,k)
          d1(nxt+1,j,k) = d1(nxt,j,k)
        end do
      end do

      do k=1,nzt
        do i=1,nxt
          lam(i,0,k) = lam(i,1,k)
          lam(i,nyt+1,k) = lam(i,nyt,k)
          mu(i,0,k) = mu(i,1,k)
          mu(i,nyt+1,k) = mu(i,nyt,k)
          d1(i,0,k) = d1(i,1,k)
          d1(i,nyt+1,k) = d1(i,nyt,k)
        end do
      end do

      do j=1,nyt
        do i=1,nxt
          lam(i,j,0)= lam(i,j,1)
          mu(i,j,0) = mu(i,j,1)
          d1(i,j,0) = d1(i,j,1)
        end do
      end do

      ! 12 border lines
      do i=1,nxt
        lam(i,0,0)= lam(i,1,1)
        mu(i,0,0) = mu(i,1,1)
        d1(i,0,0) = d1(i,1,1)
        lam(i,nyt+1,0)= lam(i,nyt,1)
        mu(i,nyt+1,0) = mu(i,nyt,1)
        d1(i,nyt+1,0) = d1(i,nyt,1)
        lam(i,0,nzt+1)= lam(i,1,nzt)
        mu(i,0,nzt+1) = mu(i,1,nzt)
        d1(i,0,nzt+1) = d1(i,1,nzt)
        lam(i,nyt+1,nzt+1)= lam(i,nyt,nzt)
        mu(i,nyt+1,nzt+1) = mu(i,nyt,nzt)
        d1(i,nyt+1,nzt+1) = d1(i,nyt,nzt)
      end do

      do i=1,nyt
        lam(0,i,0)= lam(1,i,1)
        mu(0,i,0) = mu(1,i,1)
        d1(0,i,0) = d1(1,i,1)
        lam(nxt+1,i,0)= lam(nxt,i,1)
        mu(nxt+1,i,0) = mu(nxt,i,1)
        d1(nxt+1,i,0) = d1(nxt,i,1)
        lam(0,i,nzt+1)= lam(1,i,nzt)
        mu(0,i,nzt+1) = mu(1,i,nzt)
        d1(0,i,nzt+1) = d1(1,i,nzt)
        lam(nxt+1,i,nzt+1)= lam(nxt,i,nzt)
        mu(nxt+1,i,nzt+1) = mu(nxt,i,nzt)
        d1(nxt+1,i,nzt+1) = d1(nxt,i,nzt)
      end do

      do i=1,nzt
        lam(0,0,i)= lam(1,1,i)
        mu(0,0,i) = mu(1,1,i)
        d1(0,0,i) = d1(1,1,i)
        lam(nxt+1,0,i)= lam(nxt,1,i)
        mu(nxt+1,0,i) = mu(nxt,1,i)
        d1(nxt+1,0,i) = d1(nxt,1,i)
        lam(0,nyt+1,i)= lam(1,nyt,i)
        mu(0,nyt+1,i) = mu(1,nyt,i)
        d1(0,nyt+1,i) = d1(1,nyt,i)
        lam(nxt+1,nyt+1,i)= lam(nxt,nyt,i)
        mu(nxt+1,nyt+1,i) = mu(nxt,nyt,i)
        d1(nxt+1,nyt+1,i) = d1(nxt,nyt,i)
      end do

      ! 8 Corners
      lam(0,0,0)= lam(1,1,1)
      mu(0,0,0)= mu(1,1,1)
      d1(0,0,0)= d1(1,1,1)
      lam(nxt+1,0,0)= lam(nxt,1,1)
      mu(nxt+1,0,0)= lam(nxt,1,1)
      d1(nxt+1,0,0)= lam(nxt,1,1)
      lam(0,nyt+1,0)= lam(1,nyt,1)
      mu(0,nyt+1,0)= mu(1,nyt,1)
      d1(0,nyt+1,0)= d1(1,nyt,1)
      lam(0,0,nzt+1)= lam(1,1,nzt)
      mu(0,0,nzt+1)= mu(1,1,nzt)
      d1(0,0,nzt+1)= d1(1,1,nzt)
      lam(nxt+1,0,nzt+1)= lam(nxt,1,nzt)
      mu(nxt+1,0,nzt+1)= mu(nxt,1,nzt)
      d1(nxt+1,0,nzt+1)= d1(nxt,1,nzt)
      lam(nxt+1,nyt+1,0)= lam(nxt,nyt,1)
      mu(nxt+1,nyt+1,0)= mu(nxt,nyt,1)
      d1(nxt+1,nyt+1,0)= d1(nxt,nyt,1)
      lam(0,nyt+1,nzt+1)= lam(1,nyt,nzt)
      mu(0,nyt+1,nzt+1)= mu(1,nyt,nzt)
      d1(0,nyt+1,nzt+1)= d1(1,nyt,nzt)
      lam(nxt+1,nyt+1,nzt+1)= lam(nxt,nyt,nzt)
      mu(nxt+1,nyt+1,nzt+1)= mu(nxt,nyt,nzt)
      d1(nxt+1,nyt+1,nzt+1)= d1(nxt,nyt,nzt)
 
      return
      end


      subroutine afsmedia(coords,dims,maxdim,nxt,nyt,nzt,nve)
CCC   EXTENDS FREE SURFACE MEDIA ABOVE TWO LAYERS
      use parstat
      implicit none
      
      integer :: nxt,nyt,nzt
      integer :: nve,maxdim
      integer :: i,j,k
      
      integer, dimension(maxdim) :: coords,dims
      
      if((coords(3)+1)==dims(3)) then
        do i=1,nxt
          do j=1,nyt
            do k=nzt+1,nzt+1
              d1(i,j,k) = d1(i,j,nzt)
              mu(i,j,k) = mu(i,j,nzt)
              lam(i,j,k) = lam(i,j,nzt)
              if (nve.eq.1) then
                qp(i,j,k) = qp(i,j,nzt)
                qs(i,j,k) = qs(i,j,nzt)
c                weights_s(i,j,k)=weights_s(i,j,nzt)
c                weights_p(i,j,k)=weights_p(i,j,nzt)
              end if
            enddo
          enddo
        enddo
      endif
      
      return
      end

      subroutine inihomo(nxt,nyt,nzt,vpe,vse,dde,y_dt,idyna,vp_in,vs_in,dd_in)
CCC   ROUTINE TO INITIALIZE HOMOGENEOUS STRUCTURE

C     VPE(1)  LOWEST P-VELOCITY               (REAL) (RETURN)
C     VPE(2)  HIGHEST P-VELOCITY              (REAL) (RETURN)
C     VSE(1)  LOWEST S-VELOCITY               (REAL) (RETURN)
C     VSE(2)  HIGHEST S-VELOCITY              (REAL) (RETURN)
C     PV      P VELOCITY                      (REAL)
C     PS      S VELOCITY                      (REAL)
C     DD      DENSITY                         (REAL)

      use parstat

      integer :: nxt,nyt,nzt,y_dt,idyna
      real :: vp,vs,dd,pq,sq
      real :: vp_in, vs_in, dd_in
      real, dimension(2) :: vpe,vse,dde
c     set the vp and vs
      vp=vp_in
      vs=vs_in
      dd=dd_in
c     set attenuation
      pq=100.
      sq=50.

c     vpe(1) = 0.8*vp
      vpe(1) = vp
      vpe(2) = vp
c     vse(1) = 0.8*vs
      vse(1) = vs
      vse(2) = vs
      dde(1) = dd
      dde(2) = dd

      do 60 k=-1,nzt+1
      do 60 j=-1,nyt+1
        if (idyna==1) then
           vp=vpe(2)
           vs=vse(2)
           dd=dde(2)
          if(j.lt.y_dt) then
            vp=vpe(1)
            vs=vse(1)
            dd=dde(1)
          end if
        endif
      do 60 i=-1,nxt+1
        lam(i,j,k)=1./(dd*(vp*vp - 2.*vs*vs))
        mu(i,j,k)=1./(dd*vs*vs)
        d1(i,j,k)=dd
   60 continue

      return
      end
