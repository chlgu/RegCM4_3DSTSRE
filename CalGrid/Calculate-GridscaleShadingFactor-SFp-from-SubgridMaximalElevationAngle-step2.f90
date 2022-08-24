!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate Gridscale SFp, step 2
!Original code by Anning Huang, Nanjing University, anhuang@nju.edu.cn
!Modified by Chunlei Gu, Nanjing University, chlgu@outlook.com
!Reference: Huang, A., Gu, C., Zhang, Y., et al. (2022). Development of a Clear‐Sky 
!           3D Sub‐Grid Terrain Solar Radiative Effect Parameterization Scheme Based
!           on the Mountain Radiation Theory. Journal of Geophysical Research: 
!           Atmospheres, 127(13). https://doi.org/10.1029/2022jd036449
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          implicit none

          integer i,j,idn,iup,jdn,jup,mi,mj,ii,jj,nn,bi,bj
          integer numlat1,numlon1,numlat2,numlon2
          integer numint,inti1,inti2,intj1,intj2,ijtmp
   
          integer kk,pnum,nrec
          real(kind=4) counts(320,240,360,102),SF(320,240,360,101),tmp(320,240,360)
          integer(kind=1) shading(360)
          integer(kind=4),allocatable :: pointx(:),pointy(:) 
          integer judgep(3000,3000)
       
          real tmplon,tmplat          
	      real latp(320,240),lonp(320,240)
          real latd(320,240),latu(320,240),lond(320,240),lonu(320,240)       
          character(len=6):: fname
          character(len=2):: cmi,cmj
          logical alive 

  

           tmp=0.0

          open(51,file='CORDEX-EA_shading_counts.grd',form='unformatted'&
            &,access='direct',recl=320*240*360*4)
          do kk=1,102
          read(51,rec=kk) counts(:,:,:,kk)
          enddo
          close(51)
     
             do i=1,320
                do j=1,240
                   do nn=1,360
                     do kk=1,101
                    tmp(i,j,nn)=tmp(i,j,nn)+counts(i,j,nn,kk)
                    sf(i,j,nn,kk)=tmp(i,j,nn)/counts(i,j,nn,102)
                     enddo
                   enddo
                enddo
                print*,'i=',i
             enddo

          open(51,file='CORDEXEA_SF.grd',form='unformatted'&
            &,access='direct',recl=320*240*360*4)
          do kk=1,101
          write(51,rec=kk) sf(:,:,:,kk)
          enddo
          close(51)

          kk=1
     open(51,file='CORDEXEA_SF_memoryfree.grd',form='unformatted'&
            &,access='direct',recl=101*4)
          do i=1,320
            do j=1,240
              do nn=1,360
          write(51,rec=kk) sf(i,j,nn,:)
          kk=kk+1
              enddo
             enddo
         enddo
          close(51)



       end
