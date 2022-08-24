!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate Gridscale DIFCp, REFCp
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
          
          real tmpa(6000,6000),tmpb(6000,6000)
          real tmpu(6000,6000),tmpv(6000,6000)
          real tmplon,tmplat
          real potu(320,240),potv(320,240)

	      real latp(320,240),lonp(320,240)
          real latd(320,240),latu(320,240),lond(320,240),lonu(320,240)
          
	      real intrd(2000000)
          real intmpu(2000000),intmpv(2000000)


          
          real sumrd,sumtmpu,sumtmpv
          character(len=6):: fname
          character(len=2):: cmi,cmj
        logical alive 
        
        potu=-999000000.0

        
       open(53,file='E:/CORDEX-domain/xlat.dat')
         do j=1,240
 	   	read(53,*) latp(:,j)
         enddo 
        close(53)
       
       
         open(55,file='E:/CORDEX-domain/xlon.dat')  
         do j=1,240
 	   	read(55,*) lonp(:,j)
         enddo         
        close(55)
        print*,lonp
        print*,latp
        
        
        
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
           print*,'i=',i,'j=',j
            if((latp(i,j).GE.-60.0).AND.(latp(i,j).LE.60.0)) then
 !             print*,latd(i,j),latu(i,j),lond(i,j),lonu(i,j)
               numlat1=int(abs(((latd(i,j)-60)/5-1)))
               numlat2=int(abs(((latu(i,j)-60)/5-1)))
               numlon1=int(lond(i,j)/5+1)
               numlon2=int(lonu(i,j)/5+1)
        
              if(numlat1.LT.1) numlat1=1
              if(numlat2.LT.1) numlat2=1
              if(numlat1.GT.24) numlat1=24
              if(numlat2.GT.24) numlat2=24
              if(numlon1.LT.1) numlon1=1
              if(numlon2.LT.1) numlon2=1
              if(numlon1.GT.72) numlon1=72
              if(numlon2.GT.72) numlon2=72
               
                    intrd=0.0
                    intmpu=0.0
                    intmpv=0.0

                    numint=1
                    sumrd=0.0
                    sumtmpu=0.0
                    sumtmpv=0.0

                    
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
                   
                   tmpa=-999.0
                   tmpb=-999.0
                   
                   inquire(file='E:/SRTM_3D/1srtm_h/alfa1'//fname//'.grd',exist=alive)
             
                    if(alive)then
                      print*, 'exists'
                    open(116,file='E:/SRTM_3D/1srtm_h/alfa1'//fname//'.grd',&
                     &form='unformatted',access='direct',&
                     &recl=6000*6000*4)  
                    read(116,rec=1)tmpa
                    close(116)


                open(116,file='E:/SRTM_3D/4skyview/linuxrun/skip/skyview1-'//fname//'threshold-ecoskip.grd',&
                     &form='unformatted',access='direct',&
                     &recl=6000*6000*4)  
                    read(116,rec=1)tmpb(1:6000:2,1:6000:2)
                    close(116)
                       else
                      tmpa=0.0   !the slope of the flat area is zero.
                      tmpb(1:6000:2,1:6000:2)=1.0   !the skyview of the flat area is one.
                      endif
                     
     

                      
                      intj1=int((latd(i,j)-60+mj*5)*60/0.05+1)
                      intj2=int((latu(i,j)-60+mj*5)*60/0.05+1)
                      inti1=int((lond(i,j)-(mi-1)*5)*60/0.05+1)
                      inti2=int((lonu(i,j)-(mi-1)*5)*60/0.05+1)
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
                       if(inti2.gt.6000) inti2=6000
                       if(intj2.gt.6000) intj2=6000
           print*,'inti1=',inti1,'inti2=',inti2,'intj1=',intj1,'intj2=',intj2
    
                    do ii=inti1,inti2
                      do jj=intj1,intj2    
                        tmplat=60-mj*5+(0.05/60)*(jj-1)
                        tmplon=(mi-1)*5+(0.05/60)*(ii-1) 
                        
                      if(tmpb(ii,jj).gt.-10.0)  then    ! exclude the grids whose SVF wasn't calculated.
                      tmpu(ii,jj)=( ( 1.0+cos(tmpa(ii,jj)) )/2.0-tmpb(ii,jj) )/cos(tmpa(ii,jj))
                      tmpv(ii,jj)=(  ( tmpb(ii,jj)+cos(tmpa(ii,jj))*tmpb(ii,jj) )/cos(tmpa(ii,jj)) )*0.5 
					  !take care here, check it again in the RegCM4 code.
                              
                         intrd(numint)=sqrt((tmplat-latp(i,j))**2&
                         &+(tmplon-lonp(i,j))**2)
                         if(intrd(numint).eq.0.0) intrd(numint)=0.0000001
                         intmpu(numint)=tmpu(ii,jj)/intrd(numint)
                         intmpv(numint)=tmpv(ii,jj)/intrd(numint)
                         intrd(numint)=1.0/intrd(numint)
                         numint=numint+1
                        endif
                       enddo
                     enddo
!                 enddo jj,enddoii





                
                 enddo
               enddo
 !                enddo mj,endo mi           
            
                        numint=numint-1                        
                     do nn=1,numint
                       sumrd=intrd(nn)+sumrd
                       sumtmpu=intmpu(nn)+sumtmpu
                       sumtmpv=intmpv(nn)+sumtmpv
                     enddo
!                       print*,'latp=',latp(i,j),'lonp=',lonp(i,j)
!                       print*,'latd=',latd(i,j),'lond=',lond(i,j)
!                       print*,'latu=',latu(i,j),'lonu=',lonu(i,j)
                       
!                       print*,'rd=',sumrd,'sumtmp=',sumtmp
                       potu(i,j)=sumtmpu/sumrd
                       potv(i,j)=sumtmpv/sumrd
!                     !enddo nn
           
           
           else
            potu(i,j)=-999000000.0
          
    
            print*,'1233134'
        endif   
           
            
            enddo
           enddo
 !         enddo j,endo i  



          open(51,file='CORDEXEA_Reflect_CtSECA_SRTM_IDW.grd',form='unformatted'&
            &,access='direct',recl=320*240*4)
          write(51,rec=1) potu
          close(51)
          

          open(51,file='CORDEXEA_SVFcosAsecA_SRTM_IDW.grd',form='unformatted'&
            &,access='direct',recl=320*240*4)
          write(51,rec=1) potv
          close(51)


       end
