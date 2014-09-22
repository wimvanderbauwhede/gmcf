module module_bondv1_calc_uout_kernel

contains

      subroutine bondv1_calc_uout_kernel(uvw, uout_ptr, aaa_chunks, bbb_chunks, im, jm, km, dt)
      use common_sn 
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: uvw
        real(kind=4), dimension(256) , intent(InOut) :: uout_ptr
        real(kind=4), dimension(jp) , intent(InOut) :: aaa_chunks, bbb_chunks

        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km

! 
      end subroutine bondv1_calc_uout_kernel

end module module_bondv1_calc_uout_kernel

