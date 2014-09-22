module module_les_kernel
      use module_boundsm
contains
subroutine merged_velfg_feedbf_les_calc_sm_kernel(uvw,uvwsum,fgh,mask1,diu,dzs,dx1,dy1,dzn,sm,dt,im,jm,km)
      use common_sn
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: uvwsum
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) , intent(InOut) :: fgh
        real(kind=4), dimension(0:3,-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: mask1
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm
        real(kind=4), dimension(0:15,-1:ip+2,0:jp+2,0:kp+2) , intent(InOut) :: diu
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm
        real(kind=4), intent(In) :: dt
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
! 
      end subroutine merged_velfg_feedbf_les_calc_sm_kernel
end module module_les_kernel
