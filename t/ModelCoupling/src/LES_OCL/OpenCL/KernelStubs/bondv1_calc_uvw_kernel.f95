module module_bondv1_calc_uvw_kernel

contains

      subroutine bondv1_calc_uvw_kernel(uvw, dxs, uout_ptr, im, jm, km, dt)
      use common_sn 
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: uvw
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(1), intent(In) :: uout_ptr
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), intent(In) :: dt
! 
      end subroutine bondv1_calc_uvw_kernel

end module module_bondv1_calc_uvw_kernel

