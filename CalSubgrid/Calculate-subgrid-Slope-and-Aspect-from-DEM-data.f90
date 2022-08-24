!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate the slope and aspect from the Digital Elevation Data (DEM, e.g. SRTM4)
!Original code by Anning Huang, Nanjing University, anhuang@nju.edu.cn
!Modified by Chunlei Gu, Nanjing University, chlgu@outlook.com
!Reference: Huang, A., Gu, C., Zhang, Y., et al. (2022). Development of a Clear‐Sky 
!           3D Sub‐Grid Terrain Solar Radiative Effect Parameterization Scheme Based
!           on the Mountain Radiation Theory. Journal of Geophysical Research: 
!           Atmospheres, 127(13). https://doi.org/10.1029/2022jd036449
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter(lon=6002,lat=6002) ! for globality (30sec*30sec)
!---------------------------------------------------------------------------
!      slat: the starting latitude
!      resol2: the subgrid resolution in degree
!
      parameter(resol1=0.05/60.0, resol2= resol1)
!
      integer i,j,k
 	real h(lon,lat),areanew(lon,lat)
 	real area(lon,lat),areanew1(lon,lat)	   ! area

	real alfa(lon,lat)   ! terrain slope  
    real beta(lon,lat)   ! terrain orientation
    real slat
    
    
!  for reading data
    
        real tmp(lon-2,lat-2)
        character(len=2):: clat,clon
        character(len=14):: filename
        character(len=6):: outname
        integer loni,latj,lontmp,lattmp
        logical alive
        character(len=50):: ddd(6)
!----------------------------------------------------------------------	 
!    big loop for tiles
!       do loni=1,72 !longitude 
!           do latj=1,24  !latitude
           
        do loni=1,72 !longitude 
           do latj=13,24  !latitude
                  
                
                            
 !  read data          
           !middle  Target data block
           lontmp=loni
           lattmp=latj
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
           outname='_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//''
           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
            outname='_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//''
           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
           outname='_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//''
           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
           outname='_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//''
           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) h(2:lon-1,j+1)
                enddo
                close(116)
!                 do j=lat-2,1,-1
!                 read(116,*) tmp(:,j)
!                 enddo
                 
!                 do i=1,lon-2
!                 do j=1,lat-2
!                 h(i+1,j+1)=tmp(i,j)
!                 enddo  
!                 enddo
 
              else 
              goto 1148  
           endif
           
     
     
            !left-up
           lontmp=loni-1
           lattmp=latj-1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
          
           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
           
           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
       
           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) tmp(:,j)
                enddo
                close(116)
           endif
              h(1,lat)=tmp(lon-2,1)   
     
     
     
           !middle-up
           lontmp=loni
           lattmp=latj-1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
     
           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
    
           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'
    
           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) tmp(:,j)
                enddo
                close(116)
           endif    
               do k=2,lon-1
               h(k,lat)=tmp(k-1,1)
               enddo
               
               
               
     
     
     
            !right-up
           lontmp=loni+1
           lattmp=latj-1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
  
           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) tmp(:,j)
                enddo
                close(116)
           endif   
               h(lon,lat)=tmp(1,1)
     
     
     
     
           !left-middle
           lontmp=loni-1
           lattmp=latj
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) tmp(:,j)
                enddo
                close(116)
           endif    
               do k=2,lat-1
               h(1,k)=tmp(lon-2,k-1)
               enddo
               
               
               
               
               
               
               
               
     
             !right-middle
           lontmp=loni+1
           lattmp=latj
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-2,1,-1
	   	         read(116,*) tmp(:,j)
                enddo
                close(116)
           endif  
               do k=2,lat-1
               h(lon,k)=tmp(1,k-1)
               enddo
               
               
               
               
               
               
               
     
             !left-bottom
           lontmp=loni-1
           lattmp=latj+1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                 read(116,*) tmp(:,lat-2)
                 close(116)
           endif
                h(1,1)=tmp(lon-2,lat-2)
           
           
            !middle-bottom
           lontmp=loni
           lattmp=latj+1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                read(116,*) tmp(:,lat-2)
           endif
                close(116)
               do k=2,lon-1
               h(k,1)=tmp(k-1,lat-2)
               enddo       
           
           
           
            !right-bottom
           lontmp=loni+1
           lattmp=latj+1
           tmp=0.0
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
           filename='srtm_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'

           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//'.asc'

           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           filename='srtm_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//'.asc'
          
           endif  
           inquire(file=''//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file=''//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                read(116,*) tmp(:,lat-2)
                close(116)
           endif         
                 h(lon,1)=tmp(1,lat-2)          
     
            slat=60-latj*5-0.05/60*1   !change according the extending grids number 
     
 !            print*,h















	 
	   alfa=0
	   beta=0



       do i=1,lon
	  do j=1,lat			  
	   if(h(i,j).le.0)h(i,j)=0
        enddo
       enddo
!
!----------------------------------------------------------------------
!
  	 open(114,file='alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-2)*(lat-2)*4)          

  	 open(115,file='beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-2)*(lat-2)*4) 	
 	      !alfa is the slope
		  !beta is the aspect
  
!
!--------------------------------------------------------------------
!         for calculating terrain slope, orientation and new area
     
	 do j=2,lat-1	   
	    
	  n=j

	  print*,n  
		 			
!	 ! fai is the latitude in radian 	
	 fai=(slat+(j-1)*resol2/2.)*3.1415926/180. 	      
		 										 
       do i=2,lon-1	   
	 	      		 
	  m=i    
	     		 
	  dely=6371000.*resol2*3.1415926/180.         ! the grid space in y direction
	  delx=6371000.*cos(fai)*resol1*3.1415926/180 ! the grid space in x direction
        
	  hy= (h(i-1,j+1)+h(i,j+1)+h(i+1,j+1)-h(i-1,j-1)-h(i,j-1)-h(i+1,j-1))/6./dely 

 	  hx= (h(i+1,j+1)+h(i+1,j)+h(i+1,j-1)-h(i-1,j+1)-h(i-1,j)-h(i-1,j-1))/6./delx 

	 	        
	  alfa(m,n) = atan(sqrt(hx*hx+hy*hy)) 	
    
	  if(hx.lt.0)then	  
	     beta(m,n)=3.1415926/2.-atan(hy/hx)	 
	    elseif(hx.gt.0)then	  
	       beta(m,n)=3./2.*3.1415926-atan(hy/hx)   
	    else   

		  if(hy.lt.0)then			    
		     beta(m,n)=0 
            elseif(hy.gt.0)then 
		     beta(m,n)=3.1415926
	       else
 		     beta(m,n)=-999	   	  
	 	    endif 
			 
	    endif

	  enddo
	  enddo		

 	  write(114,rec=1)alfa(2:lon-1,2:lat-1)
	  write(115,rec=1)beta(2:lon-1,2:lat-1)	   
	      
      close(114)
      close(115)	
	  print*,'output successful, net data block' 
      
1148  print*,'Target data block does not exist and we should do nothing'      
      
      enddo  !end big loop  longitude
      
    enddo  !end big loop  latitude

999      end





