!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate Gridscale SFp, step 1
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
          real(kind=4) counts(320,240,360,102)
          integer(kind=1) shading(360)
          integer(kind=4),allocatable :: pointx(:),pointy(:) 
          integer judgep(3000,3000)
       
          real tmplon,tmplat          
	      real latp(320,240),lonp(320,240)
          real latd(320,240),latu(320,240),lond(320,240),lonu(320,240)       
          character(len=6):: fname
          character(len=2):: cmi,cmj
          logical alive 

  
        counts=0.0
        
       open(53,file='xlat.dat')
         do j=1,240
          read(53,*) latp(:,j)
         enddo 
        close(53)
       
       
       open(55,file='xlon.dat')  
         do j=1,240
             read(55,*) lonp(:,j)
         enddo         
        close(55)
       ! print*,lonp
       ! print*,latp
       
       !   BCC does not need this step
       do i=1,320
         do j=1,240
        lonp(i,j)=lonp(i,j)+180.0
        enddo
      enddo
      ! [for BBC model longitude from 0~360,0~180west,180~360east]
       ![for RegCM4.1 -180~0 west,0~180east
        
   do i=1,320
      do j=1,240
  
         
      !latlon input success        
         iup=i+1
         idn=i-1
         jup=j+1
         jdn=j-1
          if(idn.lt.1) idn=1
          if(iup.gt.320) iup=320
          if(jdn.lt.1) jdn=1
          if(jup.gt.240) jup=240        
           
         latd(i,j)=(latp(i,jdn)+latp(i,j))*0.5
         latu(i,j)=(latp(i,jup)+latp(i,j))*0.5
         lond(i,j)=(lonp(idn,j)+lonp(i,j))*0.5
         lonu(i,j)=(lonp(iup,j)+lonp(i,j))*0.5
         enddo
       enddo
       
      
   do i=1,320
      do j=1,240
 
!if(latp(i,j).lt.36.0.and.latp(i,j).gt.&
!34.0.and.lonp(i,j).gt.250.0.and.lonp(i,j).lt.252.0) then
	  
           print*,'i=',i,'j=',j
            if((latp(i,j).GE.-60.0).AND.(latp(i,j).LE.60.0)) then
 !             print*,latd(i,j),latu(i,j),lond(i,j),lonu(i,j)
               numlat1=int(abs(((latd(i,j)-60)/5-1)))
               numlat2=int(abs(((latu(i,j)-60)/5-1)))
               numlon1=int(lond(i,j)/5+1)
               numlon2=int(lonu(i,j)/5+1)
        
              if(numlat1.LT.1) numlat1=1
              if(numlat2.LT.1) numlat2=1
              if(numlat1.GT.24)numlat1=24
              if(numlat2.GT.24)numlat2=24
              if(numlon1.LT.1) numlon1=1
              if(numlon2.LT.1) numlon2=1
              if(numlon1.GT.72) numlon1=72
              if(numlon2.GT.72) numlon2=72
               
                   
                    
!              open file and select sub-point
               do mi=numlon1,numlon2
                 do mj=numlat2,numlat1
                  write(cmi,'(i2)') mi 
                  write(cmj,'(i2)') mj
                 
                    if(mj.lt.10.and.mi.lt.10) fname='_0'//&
                     &trim(adjustl(cmi))//'_0'//trim(adjustl(cmj))//''
                    if(mj.ge.10.and.mi.lt.10) fname='_0'//&
                     &trim(adjustl(cmi))//'_'//trim(adjustl(cmj))//''
                    if(mj.lt.10.and.mi.ge.10) fname='_'//trim(adjustl(cmi))//&
                     &'_0'//trim(adjustl(cmj))//''
                    if(mj.ge.10.and.mi.ge.10) fname='_'//trim(adjustl(cmi))//&
                     &'_'//trim(adjustl(cmj))//''
                   
                   
                    judgep=0    
                    
           inquire(file='/mnt/e/SRTM_3D/6shadings/out/shading-'//fname//'&
		   point-number.txt',exist=alive)
                   if(alive)then
                    print*, 'exists'		     
                    open(135,file='/mnt/e/SRTM_3D/6shadings/out&
                            &/shading-'//fname//'point-number.txt')
	                read(135,*) pnum
                    close(135) 
                    print*,'pnum=',pnum,'fname=',fname
                    allocate(pointx(pnum),pointy(pnum))  
                  open(135,file='/mnt/e/SRTM_3D/6shadings/out/&
                          &shading-'//fname//'x-y.grd',&
                          &form='unformatted',access='direct',recl=2*4)
                  print*,'1111'   
                  do nrec=1,pnum
                      read(135,rec=nrec) pointx(nrec),pointy(nrec) 
                   !  print*, pointx(nrec),pointy(nrec),nrec
                     judgep(pointx(nrec),pointy(nrec))=nrec
                    enddo
                   close(135)
                    
                    open(135,file='/mnt/e/SRTM_3D/6shadings/out/&
                            &shading-'//fname//'.grd',&
                            &form='unformatted',access='direct',recl=360*1)
                    endif
   !对于没有遮蔽的次网格点赋值为0标记(包括有地形数据但为平地或没地形数据的海洋两部分)，对于有遮蔽的赋值为nrec标记。
                     
                     
                      !because we skip one grid, the grid  number of tile is 
                      !3000*3000,resolution is 0.1/60
                      intj1=int((latd(i,j)-60+mj*5)*60/0.1+1)
                      intj2=int((latu(i,j)-60+mj*5)*60/0.1+1)
                      inti1=int((lond(i,j)-(mi-1)*5)*60/0.1+1)
                      inti2=int((lonu(i,j)-(mi-1)*5)*60/0.1+1)
                      if(intj1.gt.intj2) then
                         ijtmp=intj1
                         intj1=intj2
                         intj2=ijtmp
                       endif

                      if(inti1.gt.inti2) then
                         ijtmp=inti1
                         inti1=inti2
                         inti2=ijtmp
                       endif
                       if(inti1.lt.1) inti1=1
                       if(intj1.lt.1) intj1=1
                       if(inti2.gt.3000) inti2=3000
                       if(intj2.gt.3000) intj2=3000
           print*,'inti1=',inti1,'inti2=',inti2,'intj1=',intj1,'intj2=',intj2
    
                    do ii=inti1,inti2
                      do jj=intj1,intj2    
                        if(judgep(ii,jj).eq.0) then 
                          shading=0
                        else
                          read(135,rec=judgep(ii,jj)) shading(:) 
                       endif
                       do kk=1,360
             counts(i,j,kk,shading(KK)+1)=counts(i,j,kk,shading(KK)+1)+1.0
             counts(i,j,kk,102)=counts(i,j,kk,102)+1.0
                       enddo
                       
                       enddo
                     enddo
!                 enddo jj,enddoii

                
      if(alive)   deallocate(pointx,pointy) 
       if(alive)   close(135)
                
				
	
				
                 enddo
               enddo
 !                enddo mj,endo mi           
            
           
           
           else
            print*,'exceed SRTM boundary'
        endif   
		
		
         		!	endif  !testing for little region
            
            enddo
           enddo
 !         enddo j,endo i  



          open(51,file='CORDEX-EA_shading_counts.grd',form='unformatted'&
            &,access='direct',recl=320*240*360*4)
          do kk=1,102
          write(51,rec=kk) counts(:,:,:,kk)
          enddo
          close(51)
     

       end
