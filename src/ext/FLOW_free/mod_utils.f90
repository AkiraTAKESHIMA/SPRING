module mod_utils
  implicit none

contains
subroutine nextxy(ix,iy,jx,jy,nx,dir)
  implicit none
  integer   :: ix,iy,jx,jy,nx
  integer*1 :: dir

  jx=ix
  jy=iy

  if( dir==2 .or. dir==3 .or. dir==4 )then
    jx=ix+1
    if( jx>nx ) jx=1
  elseif( dir==6 .or. dir==7 .or. dir==8 )then
    jx=ix-1
    if( jx==0 ) jx=nx
  endif

  if( dir==1 .or. dir==2 .or. dir==8 )then
    jy=iy-1
  elseif( dir==4 .or. dir==5 .or. dir==6 )then
    jy=iy+1
  endif
end subroutine nextxy


end module mod_utils
