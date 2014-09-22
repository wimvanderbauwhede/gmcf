module module_adam_kernel
contains
      subroutine adam_kernel(fgh,fgh_old,im,jm,km)
      use common_sn 
!        character(len=70), intent(In) :: data21
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        real(kind=4), dimension(0:3,ip,jp,kp) , intent(In) :: fgh_old
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
!
end module module_adam_kernel
