module module_press_kernel
      use module_bondFG ! add_module_decls() line 156
      use module_boundp ! add_module_decls() line 156
contains
      subroutine press_sor_kernel( uvw, p, p_scratch, rhs, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s, &
      chunks_num, chunks_denom, val_ptr, nrd_ptr, &
      im, jm, km )
      use common_sn 
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p_scratch
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: uvw
        real(kind=4), dimension(1), intent(In) :: val_ptr
        integer, dimension(1), intent(In) :: nrd_ptr
        real(kind=4), dimension(kp+2), intent(InOut) :: chunks_num, chunks_denom
! 
      end subroutine press_sor_kernel
!      
end module module_press_kernel
