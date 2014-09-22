module module_press_kernel
      use module_bondFG
      use module_adj
contains
      subroutine press_adj_kernel( p, pav, im, jm, km )
      use common_sn
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(1) , intent(In) :: pav
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
      end subroutine press_adj_kernel
!      
end module module_press_kernel
