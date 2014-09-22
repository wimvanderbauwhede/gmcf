module module_feedbf_kernel

contains

      subroutine feedbf_kernel(uvw,uvwsum,fgh,mask1,dt,im,jm,km)
      use common_sn 
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: uvwsum
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        real(kind=4), dimension(0:3,-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: mask1
        real(kind=4), intent(In) :: dt
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
! 
      return
      end




end module module_feedbf_kernel

