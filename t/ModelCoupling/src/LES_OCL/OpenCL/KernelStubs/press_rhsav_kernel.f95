module module_press_kernel
      use module_bondFG ! add_module_decls() line 156
      use module_boundp ! add_module_decls() line 156
contains
      subroutine press_rhsav_kernel( uvw, fgh, rhs, dx1,dy1,dzn, &
      chunks_num, chunks_denom, &
      dt,im, jm, km)
      use common_sn 
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: uvw
        real(kind=4), dimension(kp), intent(InOut) :: chunks_num, chunks_denom

      end subroutine press_rhsav_kernel
!      
end module module_press_kernel
