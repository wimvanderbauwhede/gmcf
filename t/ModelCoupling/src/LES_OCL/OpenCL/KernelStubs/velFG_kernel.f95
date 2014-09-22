module module_velFG_kernel

      use module_vel2 ! add_module_decls() line 156
contains

      subroutine velFG_kernel( uvw, fgh, diu, dzs, dx1, dy1,  dzn, im, jm, km)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        real(kind=4), dimension(0:15,-1:ip+2,0:jp+2,0:kp+2) , intent(InOut) :: diu
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
      
      end subroutine velFG_kernel




end module module_velFG

