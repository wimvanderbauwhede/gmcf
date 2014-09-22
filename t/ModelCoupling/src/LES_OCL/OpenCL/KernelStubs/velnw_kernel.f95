module module_velnw_kernel

contains

      subroutine velnw_kernel(p, uvw, fgh, dxs, dys, dzs, im, jm, km, dt, ro )
      use common_sn 
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: uvw
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: fgh
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), intent(In) :: dt
        real(kind=4), intent(In) :: ro
! 
      end subroutine velnw_kernel




end module module_velnw_kernel

