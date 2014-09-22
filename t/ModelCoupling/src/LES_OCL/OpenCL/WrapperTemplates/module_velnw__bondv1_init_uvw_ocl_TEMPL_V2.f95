module module_velnw__bondv1_init_uvw_ocl
    use module_LES_conversions
    use module_velnw
contains

      subroutine velnw__bondv1_init_uvw_ocl(p,u,v,w,f,g,h,dxs,dys,dzs,dzn,z2,n,ro,dt,im,jm,km)
          use common_sn 

          implicit none

        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(kp+2) , intent(In) :: z2
        integer, intent(In) :: n
        real(kind=4), intent(In) :: ro
        real(kind=4), intent(In) :: dt
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km

! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        integer, dimension(1) :: n_ptr
        integer :: globalrange
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        globalrange = (ip+1)*jp*kp
        n_ptr(1)=n
!$ACC Kernel(velnw__bondv1_init_uvw_kernel), GlobalRange(globalrange), LocalRange(0)
! We can either specify the NDRange or write loops around this call; 
! Or we could determine the NDRange from the loops inside this subroutine, maybe that is better
        call velnw__bondv1_init_uvw( p, uvw, fgh, dxs,dys,dzs,dzn,z2,n_ptr,im, jm, km, dt)
!$ACC End Kernel

         ! Convert back
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)

      end subroutine velnw__bondv1_init_uvw_ocl

end module module_velnw__bondv1_init_uvw_ocl

