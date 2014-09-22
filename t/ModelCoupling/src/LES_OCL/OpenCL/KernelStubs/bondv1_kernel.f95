module module_bondv1_kernel

contains

      subroutine bondv1_kernel(uvw, dxs, dzn, z2, uout_ptr, im, jm, km, dt)
      use common_sn 
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: uvw
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(kp+2) , intent(In) :: z2
!        integer, dimension(1), intent(In) :: n_ptr
        real(kind=4), dimension(1), intent(In) :: uout_ptr
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), intent(In) :: dt
! 
      end subroutine bondv1_kernel

end module module_bondv1_kernel

