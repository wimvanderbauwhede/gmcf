module module_les_kernel
      use module_boundsm ! add_module_decls() line 156
contains
subroutine les_kernel(f,g,h,dx1,dy1,dzn,diu,sm,im,jm,km)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(0:15,-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm
! 
! 
      end subroutine les_kernel
end module module_les_kernel
