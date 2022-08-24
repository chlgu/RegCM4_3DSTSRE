!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate the Maximal Elevation Angle from the Digital Elevation Data (DEM, e.g. SRTM4)
!Original code by Anning Huang, Nanjing University, anhuang@nju.edu.cn
!Modified by Chunlei Gu, Nanjing University, chlgu@outlook.com
!Reference: Huang, A., Gu, C., Zhang, Y., et al. (2022). Development of a Clear‐Sky 
!           3D Sub‐Grid Terrain Solar Radiative Effect Parameterization Scheme Based
!           on the Mountain Radiation Theory. Journal of Geophysical Research: 
!           Atmospheres, 127(13). https://doi.org/10.1029/2022jd036449
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	program ElvAng

	implicit none	
    integer,parameter:: NX=6600
    integer,parameter:: NY=6600  
    
    integer,parameter::lon=6600
    integer,parameter::lat=6600
    
	real,parameter:: reso=0.0008333333333333	     
    real,parameter:: PI=3.1415926

    integer,parameter:: dirNumMAX=360	 ! in degree

    REAL :: dy	     
    real,dimension(NX,NY):: DEM,skyview
    
    real :: slat,maxdem,mindem
	
     
!  for reading data
    
        real  ,allocatable ::  tmpz(:,:)
        character(len=2):: clat,clon
        character(len=14):: filename
        character(len=6):: outname
        integer loni,latj,lontmp,lattmp,m,n,i,j,ii,jj,rmax,rmin,cmax,cmin
        integer(kind=8) pnum,nrec
        logical alive
        character(len=50):: ddd(6)
!----------------------------------------------------------------------	 
!    big loop for tiles
!       do loni=1,72 !longitude 
!           do latj=1,24  !latitude
           
        do loni=47,48 !longitude 
           do latj=2,2  !latitude
   

          allocate(tmpz(lon-600,lat-600))                  
 
 !  read data          
           !middle  Target data block
           lontmp=loni
           lattmp=latj
 
           tmpz=0.0    
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
               do j=lat-600,1,-1
	   	          read(116,*) DEM(301:lon-300,j+300)
               enddo
              close(116)
               

              open(116,file='X:/Skyview/skyview5-'//outname//'threshold-eco.grd',form='unformatted',access='direct',&
              &recl=(lon-600)*(lat-600)*4) 
              read(116,rec=1) skyview(301:lon-300,301:lat-300) 
              close(116)  			  
			  
   
              else 
              deallocate(tmpz) 
              goto 1148  
           endif
           
           
       !    print*,skyview(3000,:)
           
           
           
           
           
           
     
     
            !left-up
           lontmp=loni-1
           lattmp=latj-1
 
           tmpz=0.0  
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo
                close(116)
       
             endif
 
              do m=1,300
                do n=1,300
                 DEM(m,lat-300+n)=tmpz(lon-900+m,n) 
		 
                enddo
              enddo
     
           !middle-up
           lontmp=loni
           lattmp=latj-1
 
           tmpz=0.0  
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo
				close(116)
				
 				
				
           endif    
               do m=1,lon-600
			     do n=1,300
                  DEM(300+m,lat-300+n)=tmpz(m,n)
		   
                 enddo
			   enddo
               
               
               
     
     
     
            !right-up
           lontmp=loni+1
           lattmp=latj-1
 
           tmpz=0.0  
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo
               close(116)
       
             endif
               
              do m=1,300
                do n=1,300
                 DEM(lon-300+m,lat-300+n)=tmpz(m,n) 
			 
                enddo
              enddo
     
     
           !left-middle
           lontmp=loni-1
           lattmp=latj
           tmpz=0.0

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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo

                close(116)  				
           endif    
               do m=1,300
			     do n=1,lat-600
               DEM(m,300+n)=tmpz(lon-900+m,n)

                 enddo
			   enddo
               
               
               
               
               
               
               
               
     
             !right-middle
           lontmp=loni+1
           lattmp=latj
           tmpz=0.0

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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo
 				
				
				
           endif  
               do m=1,300
			     do n=1,lat-600
               DEM(lon-300+m,300+n)=tmpz(m,n)

			     enddo
               enddo
               
               
               
               
               
               
               
     
             !left-bottom
           lontmp=loni-1
           lattmp=latj+1
 
           tmpz=0.0  
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo

	
				
           endif
		        do m=1,300
				 do n=1,300
                DEM(m,n)=tmpz(lon-900+m,lat-900+n)

                 enddo
				enddo
           
            !middle-bottom
           lontmp=loni
           lattmp=latj+1
           tmpz=0.0
 
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo
 			
				
           endif
               do m=1,lon-600
			     do n=1,300
               DEM(300+m,n)=tmpz(m,lat-900+n)
 
			     enddo
               enddo       
           
           
           
            !right-bottom
           lontmp=loni+1
           lattmp=latj+1
 
           tmpz=0.0  
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
           inquire(file='X:/1srtm_h/'//filename//'',exist=alive)
             
           if(alive)then
              print*,filename,' exists'
              open(116,file='X:/1srtm_h/'//filename//'')
                do i=1,6
                 read (116,*) ddd(i)
                 print*,ddd(i)
                end do      
                do j=lat-600,1,-1
	   	         read(116,*) tmpz(:,j)
                enddo

 
           endif 
                 do m=1,300
                    do n=1,300				 
                     DEM(lon-300+m,n)=tmpz(m,lat-900+n) 
 
                    enddo          
                 enddo					
     
     deallocate(tmpz) 
     
 
 !              open(116,file='alfaout'//outname//'.grd',form='unformatted',access='direct',recl=(lon)*(lat)*4) 
  !            write(116,rec=1) slope 
  !            close(116)
              
 !             open(116,file='betaout'//outname//'.grd',form='unformatted',access='direct',recl=(lon)*(lat)*4) 
  !             write(116,rec=1) aspect 
   !           close(116)     
 
 
    !           open(116,file='demout'//outname//'.grd',form='unformatted',access='direct',recl=(lon)*(lat)*4) 
    !           write(116,rec=1) DEM
    !          close(116)  
 
 
      do i=1,6600
      do j=1,6600
     if(DEM(i,j).lt.-2000.0) then
     DEM(i,j)=0.0
     endif
      enddo
     enddo
 
      pnum=0
 
       do i=301,6300,2
      do j=301,6300,2
     if(skyview(i,j).le.0.999) then
     pnum=pnum+1
     endif
      enddo
     enddo
	 
	
    
    
           lontmp=loni
           lattmp=latj
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
    

	   open(135,file='shading-'//outname//'point-number.txt')
	   write(135,*) pnum
	   close(135) 
 
       open(135,file='shading-'//outname//'.grd',form='unformatted',access='direct',recl=dirNumMAX)
	   open(136,file='shading-'//outname//'x-y.grd',form='unformatted',access='direct',recl=4*2)
       nrec=1
  
 
       slat=60-latj*5-0.05/60*300   !change according the extending grids number 


  

   do i=1,12      !sub block
       do j=1,12    !sub block
 !   do i=6,6      !sub block
 !       do j=7,7    !sub block
 
         maxdem=0.0
         mindem=0.0            
            do ii= 1+500*(i-1),600+500*i
               do jj= 1+500*(j-1),600+500*j
               if(DEM(ii,jj).gt.maxdem) maxdem=DEM(ii,jj)
               if(DEM(ii,jj).lt.mindem.and.DEM(ii,jj).gt.0.0) mindem=DEM(ii,jj)
               enddo
            enddo
            print*,'maxdem, mindem  =  ',maxdem,mindem
        maxdem=maxdem-mindem
 
 
        
         rmin=301+500*(j-1)
         rmax=300+500*j
   
         cmin=301+500*(i-1)
         cmax=300+500*i            
            
            
            slat=60-latj*5-0.05/60*300+(j-1)*(500*5/6000)   !change according the extending grids number 
     
 !            print*,h
 


    dy = 6371000.*reso*3.1415926/180.
	print*,dy
   
   
   
	 call calskyview(NX,NY,dem,dy,slat,reso,dirNumMAX,skyview,&
     &rmax,rmin,cmax,cmin,maxdem,nrec)

 !   open(117,file='skyview'//outname//'threshold-eco1111.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4)

 !    write(117,rec=1) skyview(301:6300,301:6300)	     

!	 close(117)
     

               
           enddo      !sub block
        enddo   !sub block


      close(135)
      close(136)




    print*,'output successful, net data block' 










      
1148  print*,'Target data block does not exist and we should do nothing'      
      

      enddo   !latitude
    enddo   !longitude 


    end


   SUBROUTINE calskyview(NX,NY,dem,dy,slat,reso,dirNumMAX,skyview,&
   & rmax,rmin,cmax,cmin,maxdem,nrec)		

   
   
!    Author: by Anning Huang 22 March 2018
!    Inputs:      
!     dem            elevation dem(m)
!     dx             resolution in xaxis (m)
!     dy             resolution in yaxis (m)
!     direction:     azimuth angle (from N clockwise, radiants)

    implicit none
    integer,intent(in)::NX,NY,dirNumMAX,rmax,rmin,cmax,cmin
	integer(kind=1) shading(dirNumMAX)
	integer(kind=4) pointxy(2) 
    real,intent(in)::dy,slat,reso,maxdem
    real,dimension(NX,NY),intent(in)::DEM,skyview
    real,parameter:: PI=3.1415926
	integer IRD    ! Maximal searching points. Searching radius of IRD*RS in m
    real,parameter:: rs=90	  ! interval of searching radius in m

    real:: sinhh,sintemp   
 
    INTEGER::R,C,I,ix,iy,dirnum
	 
    REAL:: SX,CY,ztopo,direction,dx,xd,yd,dh,hd,phi,theta,fai
    
 integer(kind=8) nrec

     
     
     IRD=int(maxdem/(tan(3.14*0.9/180.0)*rs))  ! Adjust the angle to control the accuracy
     if(IRD.gt.300) IRD=300
     if(IRD.lt.4) IRD=4
      print*,'IRD  =  ',IRD
 
    
!   DO R=31,NY-31   		! in Yaxis		
    DO R=rmin,rmax,2   		! in Yaxis	

	   fai = (slat+(2*R-1)*reso/2)*PI/180. 		 
	    dx = dy*cos(fai)    ! grid space in Xaxis in m
    						! dy is the grid space in Yaxis in m
	      
    
!	 DO C=31,NX-31 	        ! in Xaxis
	 DO C=cmin,cmax,2 	        ! in Xaxis
   
       if(skyview(C,R).le.0.999) then
	
    
  !      print*,skyview(C,R)
        do dirnum=1,dirNumMAX
		  sinhh=0.0
  
	      direction = (dirnum-1)*2*PI/dirNumMAX ! The direction angle in rad

     
		  SX = sin(direction)
          CY = cos(direction)	

		  do I=1,IRD	            ! Searching radius 	numbers

		   	    xd = real(I*rs)*SX/dx
				yd = real(I*rs)*cy/dy

   				ix = int(xd)        ! displacements of grid number in xaxis 
				iy = int(yd)	    ! displacements of grid number in yaxis
		    
!------------------------------------------------------------------------------
           IF(SX.EQ.0)then          ! In north or south direction
			    
			  if(CY.GT.0)then	    ! in north direction 
			   ztopo = dem(C,R+iy)*(iy-yd+1)+dem(C,R+iy+1)*(yd-iy)	 			   			  
              endif

			  if(CY.LT.0)then    	! in south direction 
			   ztopo = dem(C,R+iy)*(yd-iy+1)+dem(C,R+iy-1)*(iy-yd)	 			   			  
              endif	 
			  	   
		    ENDIF
  !----------------------------------------------------------------------------
            IF(CY.EQ.0)then         ! In east or west direction	  
			    
			  if(SX.GT.0)then	    ! in east direction 
			   ztopo = dem(C+ix,R)*(ix-xd+1)+dem(C+ix+1,R)*(xd-ix)		 			   			  
              endif

			  if(SX.LT.0)then	    ! in west direction 
			   ztopo = dem(C+ix,R)*(xd-ix+1)+dem(C+ix-1,R)*(ix-xd)	 			   			  
              endif
		   
		    ENDIF
  !--------------------------------------------------------------------------------
		   		
			IF(SX.GT.0)then 	 ! in the 1 or 4 quadrant			    
	
			  if(CY.GT.0)then    ! in the 1 quadrant
				 
				 ztopo = (DEM(C+ix,R+iy)*(iy+1-yd)+DEM(C+ix,R+iy+1)*(yd-iy))*(ix+1-xd)+&  				 
				 	     (DEM(C+ix+1,R+iy)*(iy+1-yd)+DEM(C+ix+1,R+iy+1)*(yd-iy))*(xd-ix)

			  endif

			  if(CY.LT.0)then    ! in the 4 quadrant
				 
				 ztopo = (DEM(C+ix,R+iy)*(yd-iy+1)+DEM(C+ix,R+iy-1)*(iy-yd))*(ix+1-xd)+&  				 
				 	     (DEM(C+ix+1,R+iy)*(yd-iy+1)+DEM(C+ix+1,R+iy-1)*(iy-yd))*(xd-ix)	
			  endif

          	ELSE

  			  if(CY.GT.0)then    ! in the 2 quadrant
				 
				 ztopo = (DEM(C+ix,R+iy)*(iy+1-yd)+DEM(C+ix,R+iy+1)*(yd-iy))*(xd-ix+1)+&  				 
				 	     (DEM(C+ix-1,R+iy)*(iy+1-yd)+DEM(C+ix-1,R+iy+1)*(yd-iy))*(ix-xd)

			  endif

			  if(CY.LT.0)then    ! in the 3 quadrant
				 
				 ztopo = (DEM(C+ix,R+iy)*(yd-iy+1)+DEM(C+ix,R+iy-1)*(iy-yd))*(xd-ix+1)+&  				 
				 	     (DEM(C+ix-1,R+iy)*(yd-iy+1)+DEM(C+ix-1,R+iy-1)*(iy-yd))*(ix-xd)	
			  endif	 

			ENDIF

  			        dh = ztopo-DEM(C,R)  			             
		       sintemp = dh/sqrt((I*rs)**2+dh**2)  
			   			   	
               if(sintemp > sinhh) sinhh = sintemp	
               sintemp = maxdem/sqrt((I*rs)**2+dh**2)
	 			 if(sinhh.ge.sintemp)goto 101	

       	  enddo	 	! do I=1,IRD	            ! Searching radius 	numbers   	
 
          
 101    shading(dirnum)=nint(sinhh*100)

!	print*,'dirnum',dirnum,'shading(dirnum)',shading(dirnum)
	    
	  enddo   ! do dirnum=1,dirNumMAX
      

		pointxy(1)=(C+1-300)/2
        pointxy(2)=(R+1-300)/2
        

		 write(135,rec=nrec) shading 
		 write(136,rec=nrec) pointxy 
	      nrec=nrec+1
!		print*,pointxy
!        print*,shading
        
        
             endif   ! if(skyview(C,R).le.0.999) 
      
      ENDDO	  !DO C=cmin,cmax 	        ! in Xaxis
	

      
    ENDDO    ! DO R=rmin,rmax   		! in Yaxis	
  
  
     

  
  
  
    
 ! print 100, C, R, skyview(cmax,Rmax)  	 
100   format(2i8,5f10.2)

  END SUBROUTINE




