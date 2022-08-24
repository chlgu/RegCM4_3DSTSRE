!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!To calculate the skyview factor from the Digital Elevation Data (DEM, e.g. SRTM4)
!Original code by Anning Huang, Nanjing University, anhuang@nju.edu.cn
!Modified by Chunlei Gu, Nanjing University, chlgu@outlook.com
!Reference: Huang, A., Gu, C., Zhang, Y., et al. (2022). Development of a Clear‐Sky 
!           3D Sub‐Grid Terrain Solar Radiative Effect Parameterization Scheme Based
!           on the Mountain Radiation Theory. Journal of Geophysical Research: 
!           Atmospheres, 127(13). https://doi.org/10.1029/2022jd036449
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	program skiwviewmain

	implicit none	
    integer,parameter:: NX=6600
    integer,parameter:: NY=6600  
    
    integer,parameter::lon=6600
    integer,parameter::lat=6600
    
	real,parameter:: reso=0.0008333333333333	     
    real,parameter:: PI=3.1415926

    integer,parameter:: dirNumMAX=360	 ! in degree

    REAL :: dy	     
    real,dimension(NX,NY):: DEM,slope,aspect,skyview1,skyview2,skyview3,skyview4,skyview5
    
    real :: slat,maxdem,mindem
     
!  for reading data
    
        real  ,allocatable :: tmpa(:,:),tmpb(:,:),tmpz(:,:)
        character(len=2):: clat,clon
        character(len=14):: filename
        character(len=6):: outname
        integer loni,latj,lontmp,lattmp,m,n,i,j,ii,jj,rmax,rmin,cmax,cmin
        logical alive
        character(len=50):: ddd(6)
!----------------------------------------------------------------------	 
!    big loop for tiles
!       do loni=1,72 !longitude 
!           do latj=1,24  !latitude
           
        do loni=52,53 !longitude 
           do latj=4,4   !latitude
   
   
        allocate(tmpa(lon-600,lat-600))               
         allocate(tmpb(lon-600,lat-600))   
          allocate(tmpz(lon-600,lat-600))                  
 
 !  read data          
           !middle  Target data block
           lontmp=loni
           lattmp=latj
           tmpa=0.0
           tmpb=0.0
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
               
              open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
              read(116,rec=1) slope(301:lon-300,301:lat-300) 
              close(116)
              
              open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
              read(116,rec=1) aspect(301:lon-300,301:lat-300) 
              close(116)              
              
              

 
              else 
              deallocate(tmpa,tmpb,tmpz) 
              goto 1148  
           endif
           
           
           
           
           
           
           
           
           
     
     
            !left-up
           lontmp=loni-1
           lattmp=latj-1
           tmpa=0.0
           tmpb=0.0
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
                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)         
             endif
               
              do m=1,300
                do n=1,300
                 DEM(m,lat-300+n)=tmpz(lon-900+m,n) 
                 slope(m,lat-300+n)=tmpa(lon-900+m,n)		
                 aspect(m,lat-300+n)=tmpb(lon-900+m,n)				 
                enddo
              enddo
     
           !middle-up
           lontmp=loni
           lattmp=latj-1
           tmpa=0.0
           tmpb=0.0
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
				
                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
				
           endif    
               do m=1,lon-600
			     do n=1,300
                  DEM(300+m,lat-300+n)=tmpz(m,n)
                  slope(300+m,lat-300+n)=tmpa(m,n)
                  aspect(300+m,lat-300+n)=tmpb(m,n)			   
                 enddo
			   enddo
               
               
               
     
     
     
            !right-up
           lontmp=loni+1
           lattmp=latj-1
           tmpa=0.0
           tmpb=0.0
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
                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)         
             endif
               
              do m=1,300
                do n=1,300
                 DEM(lon-300+m,lat-300+n)=tmpz(m,n) 
                 slope(lon-300+m,lat-300+n)=tmpa(m,n) 
                 aspect(lon-300+m,lat-300+n)=tmpb(m,n) 				 
                enddo
              enddo
     
     
           !left-middle
           lontmp=loni-1
           lattmp=latj
           tmpa=0.0
           tmpb=0.0
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
                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
           endif    
               do m=1,300
			     do n=1,lat-600
               DEM(m,300+n)=tmpz(lon-900+m,n)
			   slope(m,300+n)=tmpa(lon-900+m,n)
			   aspect(m,300+n)=tmpb(lon-900+m,n)
                 enddo
			   enddo
               
               
               
               
               
               
               
               
     
             !right-middle
           lontmp=loni+1
           lattmp=latj
           tmpa=0.0
           tmpb=0.0
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

                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
				
				
           endif  
               do m=1,300
			     do n=1,lat-600
               DEM(lon-300+m,300+n)=tmpz(m,n)
			   slope(lon-300+m,300+n)=tmpa(m,n)
			   aspect(lon-300+m,300+n)=tmpb(m,n)
			     enddo
               enddo
               
               
               
               
               
               
               
     
             !left-bottom
           lontmp=loni-1
           lattmp=latj+1
           tmpa=0.0
           tmpb=0.0
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

                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
				
           endif
		        do m=1,300
				 do n=1,300
                DEM(m,n)=tmpz(lon-900+m,lat-900+n)
				slope(m,n)=tmpa(lon-900+m,lat-900+n)
				aspect(m,n)=tmpb(lon-900+m,lat-900+n)
                 enddo
				enddo
           
            !middle-bottom
           lontmp=loni
           lattmp=latj+1
           tmpa=0.0
           tmpb=0.0
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

                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
				
           endif
               do m=1,lon-600
			     do n=1,300
               DEM(300+m,n)=tmpz(m,lat-900+n)
			   slope(300+m,n)=tmpa(m,lat-900+n)
			   aspect(300+m,n)=tmpb(m,lat-900+n)
			     enddo
               enddo       
           
           
           
            !right-bottom
           lontmp=loni+1
           lattmp=latj+1
           tmpa=0.0
           tmpb=0.0
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

                open(116,file='X:/2alfa/alfa1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                   read(116,rec=1) tmpa 
                close(116)              
                open(116,file='X:/3beta/beta1'//outname//'.grd',form='unformatted',access='direct',recl=(lon-600)*(lat-600)*4) 
                  read(116,rec=1) tmpb
                close(116)  				
				
           endif 
                 do m=1,300
                    do n=1,300				 
                     DEM(lon-300+m,n)=tmpz(m,lat-900+n) 
					 slope(lon-300+m,n)=tmpa(m,lat-900+n)
					 aspect(lon-300+m,n)=tmpb(m,lat-900+n)
                    enddo          
                 enddo					
     
     deallocate(tmpa,tmpb,tmpz) 
     
     
     
     
 
      do i=1,6600
      do j=1,6600
     if(DEM(i,j).lt.-2000.0) then
     DEM(i,j)=0.0
     endif
      enddo
     enddo
 
 
 
     
            slat=60-latj*5-0.05/60*300   !change according the extending grids number 
     
 !            print*,h
       
     skyview1 = -999.0
      skyview2 = -999.0
       skyview3 = -999.0
        skyview4 = -999.0
        skyview5 = -999.0

   do i=1,12    !sub block
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
       
	 call calskyview(NX,NY,dem,maxdem,dy,slat,reso,dirNumMAX,slope,aspect,skyview1,&
     & skyview2,skyview3,skyview4,skyview5,rmax,rmin,cmax,cmin)

 !   open(117,file='skyview'//outname//'threshold-eco1111.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4)

 !    write(117,rec=1) skyview(301:6300,301:6300)	     

!	 close(117)
           
           enddo      !sub block
        enddo   !sub block



           lontmp=loni
           lattmp=latj
           write(clon,'(i2)') lontmp 
           write(clat,'(i2)') lattmp
           if(lattmp.lt.10.and.lontmp.lt.10) then
           outname='_0'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//''
           endif
           if(lattmp.ge.10.and.lontmp.lt.10) then
            outname='_0'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//''
           endif          
           if(lattmp.lt.10.and.lontmp.ge.10) then
           outname='_'//trim(adjustl(clon))//'_0'//trim(adjustl(clat))//''
           endif   
           if(lattmp.ge.10.and.lontmp.ge.10) then
           outname='_'//trim(adjustl(clon))//'_'//trim(adjustl(clat))//''
           endif  



    open(117,file='skyview1-'//outname//'threshold-ecoskip.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4/4)

     write(117,rec=1) skyview1(301:6300:2,301:6300:2)	     

	 close(117)
     
         open(118,file='skyview2-'//outname//'threshold-ecoskip.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4/4)

     write(118,rec=1) skyview2(301:6300:2,301:6300:2)	     

	 close(118)

         open(119,file='skyview3-'//outname//'threshold-ecoskip.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4/4)

     write(119,rec=1) skyview3(301:6300:2,301:6300:2)	     

	 close(119)

        open(120,file='skyview4-'//outname//'threshold-ecoskip.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4/4)

     write(120,rec=1) skyview4(301:6300:2,301:6300:2)	     

	 close(120)


        open(121,file='skyview5-'//outname//'threshold-ecoskip.grd',form='unformatted',access='direct',recl=(NX-600)*(NY-600)*4/4)

     write(121,rec=1) skyview5(301:6300:2,301:6300:2)	     

	 close(121)

    print*,'output successful, net data block' 










      
1148  print*,'Target data block does not exist and we should do nothing'      
      

      enddo   !latitude
    enddo   !longitude 


    end


   SUBROUTINE calskyview(NX,NY,dem,maxdem,dy,slat,reso,dirNumMAX,slope,aspect,skyview1,&
   & skyview2,skyview3,skyview4,skyview5,rmax,rmin,cmax,cmin)														 
!    Author: by Anning Huang 22 March 2018
!    Inputs:      
!     dem            elevation dem(m)
!     dx             resolution in xaxis (m)
!     dy             resolution in yaxis (m)
!     direction:     azimuth angle (from N clockwise, radiants)

    implicit none
    integer,intent(in)::NX,NY,dirNumMAX,rmax,rmin,cmax,cmin
    real,intent(in)::dy,slat,reso,maxdem	
    real,dimension(NX,NY),intent(in)::DEM,slope,aspect

    real,dimension(NX,NY),intent(out)::skyview1,skyview2,skyview3,skyview4,skyview5

    real,parameter:: PI=3.1415926
	integer IRD    ! Maximal searching points. Searching radius of IRD*RS in m
    real,parameter:: rs=90	  ! interval of searching radius in m

    real:: vsky1,vsky2,vsky3,vsky4,vsky5,sinhh,sintemp    
 
    INTEGER::R,C,I,ix,iy,dirnum
	 
    REAL:: SX,CY,ztopo,direction,dx,xd,yd,dh,hd,phi,theta,fai


     
     
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
     

		 
		vsky1 = 0.0 
        vsky2 = 0.0
        vsky3 = 0.0
        vsky4 = 0.0
        vsky5 = 0.0

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

       	  enddo	 	  	 		   
 101          continue

          ! k=1-sinhh
	          
			   theta = slope(C,R)
                 phi = aspect(C,R)

!
!--------------------------------------------------------------------------------------------------------------------------------
!method 1	First choice	 
			       hd = PI/2-asin(sinhh)  				   	 
			  vsky1 = vsky1+cos(theta)*sin(hd)**2+sin(theta)*cos(phi-direction)*(hd-sin(hd)*cos(hd))
			  	 
!reference: Evaluation of Four Sky View Factor Algorithms Using Digital Surface and Elevation Model Data	
  
!--------------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------------
!method 2
			  
              hd = acos(sin(theta)*cos(asin(sinhh))*cos(phi-direction)+cos(theta)*sinhh)  ! is the angle between slope normal and the terrain horizon in azimuth direction				               
              			  vsky2 = vsky2+sin(hd)**2.0 

!reference:	Influences of Topographic Shadows on the Thermal and Hydrological Processes in a Cold Region Mountainous Watershed in Northwest China
!reference:	Radiative transfer over resolved topographic features for high-resolution weather prediction 

!--------------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------------
!method 3
				  if(theta.ne.0)then
				 
				   if(cos(phi-direction).ne.0)then
			         hd = PI/2-asin(sinhh) + PI/2. -atan(-1/tan(theta)/cos(phi-direction))
				   else
				     hd = PI/2
				   endif
		           
				  else
				     hd = PI/2-asin(sinhh)
				  endif
				  					     
              vsky3= vsky3+ sin(hd)**2
              
 !reference: Radiative transfer over resolved topographic features for high-resolution weather prediction              
              
!--------------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------------
!method 4
             vsky4 = vsky4+1-sinhh**2
		  	
!reference: Evaluation of Four Sky View Factor Algorithms Using Digital Surface and Elevation Model Data	
!reference: Radiative transfer over resolved topographic features for high-resolution weather prediction	
!--------------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------------
!method 5
              vsky5 = vsky5+(1-sinhh)

! reference:Sky-View Factor as a Relief Visualization Technique		
!--------------------------------------------------------------------------------------------------------------------------------
!	
	    
	  enddo 
		 
         skyview1(C,R) = vsky1/dirNumMAX	
         skyview2(C,R) = vsky2/dirNumMAX	
         skyview3(C,R) = vsky3/dirNumMAX	
         skyview4(C,R) = vsky4/dirNumMAX	
	     skyview5(C,R) = vsky5/dirNumMAX
!		  print*, C,R, skyview(C,R)
!	   pause

      ENDDO	 
	

      
    ENDDO
    
    
 print 100, C, R, skyview1(cmax,Rmax),skyview2(cmax,Rmax),skyview3(cmax,Rmax),skyview4(cmax,Rmax),skyview5(cmax,Rmax) 	   	 
100   format(2i8,5f10.2)

  END SUBROUTINE




