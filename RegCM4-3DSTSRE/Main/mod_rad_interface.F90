!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
!    This file is part of ICTP RegCM.
!
!    ICTP RegCM is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    ICTP RegCM is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with ICTP RegCM.  If not, see <http://www.gnu.org/licenses/>.
!
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

module mod_rad_interface
!
  use mod_intkinds
  use mod_realkinds
  use mod_dynparam
  use mod_memutil
  use mod_runparams
  use mod_regcm_types
  use mod_ipcc_scenario , only : set_scenario
  use mod_rad_common
  use mod_rad_colmod3 , only : allocate_mod_rad_colmod3 , colmod3
  use mod_rrtmg_driver , only : allocate_mod_rad_rrtmg , rrtmg_driver
  use mod_rad_o3blk , only : allocate_mod_rad_o3blk , o3data , read_o3data
  use mod_rad_aerosol , only : allocate_mod_rad_aerosol
  use mod_rad_radiation , only : allocate_mod_rad_radiation
  use mod_rad_outrad , only : allocate_mod_rad_outrad

  implicit none

  private

  ! Procedures
  public :: allocate_radiation
  public :: init_radiation
  public :: radiation
  public :: inito3
  public :: updateo3

  ! Procedures exported from internal modules
  public :: set_scenario

  ! Data
  public :: o3prof
  public :: gasabsnxt
  public :: gasabstot
  public :: gasemstot
  public :: taucldsp

  type(mod_2_rad) :: m2r
  type(rad_2_mod) :: r2m

  contains

  subroutine allocate_radiation
    implicit none
    call getmem3d(o3prof,jci1,jci2,ici1,ici2,1,kzp1,'rad:o3prof')
    call allocate_mod_rad_aerosol
    call allocate_mod_rad_o3blk
    call allocate_mod_rad_outrad
    if ( irrtm == 1 ) then
      call allocate_mod_rad_rrtmg
    else
      call allocate_mod_rad_radiation
      call allocate_mod_rad_colmod3
      call getmem4d(gasabsnxt,jci1,jci2,ici1,ici2,1,kz,1,4,'rad:gasabsnxt')
      call getmem4d(gasabstot,jci1,jci2,ici1,ici2,1,kzp1,1,kzp1,'rad:gasabstot')
      call getmem3d(gasemstot,jci1,jci2,ici1,ici2,1,kzp1,'rad:gasemstot')
    end if
    if ( ichem == 1 ) then
      call getmem4d(taucldsp,jci1,jci2,ici1,ici2,0,kz,1,nspi,'rad:taucldsp')
    end if
  end subroutine allocate_radiation

  subroutine init_radiation
    use mod_atm_interface
    implicit none
    ! Set pipings from atm_interface to radiation I/O data types

    ! INPUT
    call assignpnt(atms%tb3d,m2r%tatms)
    call assignpnt(atms%qxb3d,m2r%qxatms)
    call assignpnt(atms%rhb3d,m2r%rhatms)
    call assignpnt(atms%chib3d,m2r%chiatms)
    call assignpnt(atms%pb3d,m2r%phatms)
    call assignpnt(atms%pf3d,m2r%pfatms)
    call assignpnt(atms%dzq,m2r%deltaz)
    call assignpnt(atms%ps2d,m2r%psatms)
    call assignpnt(sfs%tgbb,m2r%tg)
    call assignpnt(mddom%xlat,m2r%xlat)
    call assignpnt(mddom%xlon,m2r%xlon)
    call assignpnt(mddom%ldmsk,m2r%ldmsk)
    call assignpnt(coszrs,m2r%coszrs)
    call assignpnt(aldirs,m2r%aldirs)
    call assignpnt(aldifs,m2r%aldifs)
    call assignpnt(aldirl,m2r%aldirl)
    call assignpnt(aldifl,m2r%aldifl)
    call assignpnt(albvs,m2r%albvs)
    call assignpnt(albvl,m2r%albvl)
    call assignpnt(emiss,m2r%emiss)
    call assignpnt(cldfra,m2r%cldfrc)
    call assignpnt(cldlwc,m2r%cldlwc)
    call assignpnt(ptrop,m2r%ptrop)
!ChlGu AnHuang Add 3DSTSRE  20201006
    call assignpnt(coszrs_gu,m2r%coszrs_gu)
    call assignpnt(solar_Az,m2r%solar_Az)
    call assignpnt(mddom%ugg,m2r%ugg)
    call assignpnt(mddom%vgg,m2r%vgg)
    call assignpnt(mddom%wgg,m2r%wgg)
    call assignpnt(mddom%refgg,m2r%refgg)
    call assignpnt(mddom%asvfgg,m2r%asvfgg)
    call assignpnt(mddom%SFgg,m2r%SFgg)
    call assignpnt(mddom%SECAgg,m2r%SECAgg)
    call assignpnt(mddom%TASBgg,m2r%TASBgg)
    call assignpnt(mddom%TACBgg,m2r%TACBgg)
    !ChlGu AnHuang Add 3DSTSRE 20201006
 !print*,'mod_rad_interface.F90 '
    ! OUTPUT
    call assignpnt(solis,r2m%solis)
    call assignpnt(sabveg,r2m%sabveg)
    call assignpnt(sinc,r2m%sinc)
    call assignpnt(solvs,r2m%solvs)
    call assignpnt(solvsd,r2m%solvsd)
    call assignpnt(solvl,r2m%solvl)
    call assignpnt(solvld,r2m%solvld)
    call assignpnt(fsw,r2m%fsw)
    call assignpnt(flw,r2m%flw)
    call assignpnt(flwd,r2m%flwd)
    call assignpnt(heatrt,r2m%heatrt)
  end subroutine init_radiation

  subroutine radiation(iyear,loutrad,labsem)
    implicit none
    integer(ik4) , intent(in) :: iyear
    logical , intent(in) :: loutrad
    logical , intent(in) :: labsem
    if ( irrtm == 1 ) then
      call rrtmg_driver(iyear,loutrad,m2r,r2m)
!      print*,'debug mod_rad_interface.F90 rrtmg'
    else
 !      print*,'debug subroutine radiation in mod_rad_interface.F90,before run colmod3('
      call colmod3(iyear,loutrad,labsem,m2r,r2m)
  !      print*,'debug subroutine radiation in mod_rad_interface.F90'
    end if
  end subroutine radiation

  subroutine inito3
    implicit none
    call o3data(m2r)
  end subroutine inito3

  subroutine updateo3(idatex,scenario)
    implicit none
    type (rcm_time_and_date) , intent(in) :: idatex
    character(len=8) , intent(in) :: scenario
    call read_o3data(idatex,scenario,m2r)
  end subroutine updateo3

end module mod_rad_interface
! vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
