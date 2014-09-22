module module_LES_combined_kernel
contains
      subroutine LES_combined_kernel( p_scratch, uvw, uvwsum, fgh, fgh_old, &
      rhs, mask1, diu, sm, &
        dxs, dys, dzs, dx1, dy1, dzn, &
#ifndef EXTERNAL_WIND_PROFILE
        z2, &
#else
      wind_profile, &
#endif
        cn1, cn2l, cn2s, cn3l, cn3s, cn4l, cn4s, &
        val_ptr, chunks_num, chunks_denom, n_ptr, state_ptr, dt, im, jm, km)

      use common_sn
        real(kind=4), dimension(00:1,:ip+2,0:jp+2,0:kp+1), intent(In)  :: p_scratch
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1), intent(InOut)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp), intent(InOut) :: uvwsum
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp), intent(InOut)  :: fgh
        real(kind=4), dimension(0:3,ip,jp,kp), intent(InOut)  :: fgh_old
#ifdef EXTERNAL_WIND_PROFILE
        real(kind=4), dimension(-1:jp+1,0:kp+1) :: wind_profile
#endif
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1), intent(In)  :: rhs
        real(kind=4), dimension(0:3,-1:ip+1,-1:jp+1,0:kp+1)  :: mask1
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2)  :: diu
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1), intent(In)  :: sm

        real(kind=4), dimension(0:ip), intent(In)  :: dxs
        real(kind=4), dimension(0:jp), intent(In)  :: dys
        real(kind=4), dimension(-1:kp+2), intent(In)  :: dzs
        real(kind=4), dimension(-1:ip+1), intent(In)  :: dx1
        real(kind=4), dimension(0:jp+1), intent(In)  :: dy1
        real(kind=4), dimension(-1:kp+2), intent(In)  :: dzn
#ifndef EXTERNAL_WIND_PROFILE
        real(kind=4), dimension(kp+2), intent(In)  :: z2
#endif
        real(kind=4), dimension(ip,jp,kp), intent(In)  :: cn1
        real(kind=4), dimension(ip), intent(In)  :: cn2l
        real(kind=4), dimension(ip), intent(In)  :: cn2s
        real(kind=4), dimension(jp), intent(In)  :: cn3l
        real(kind=4), dimension(jp), intent(In)  :: cn3s
        real(kind=4), dimension(kp), intent(In)  :: cn4l
        real(kind=4), dimension(kp), intent(In)  :: cn4s

        real(kind=4), dimension(256), intent(InOut) :: val_ptr
        real(kind=4), dimension(kp), intent(InOut) :: chunks_num, chunks_denom
        integer, dimension(256), intent(InOut) :: n_ptr, state_ptr
        real(kind=4), intent(In) :: dt
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
!
end module module_LES_combined_kernel
