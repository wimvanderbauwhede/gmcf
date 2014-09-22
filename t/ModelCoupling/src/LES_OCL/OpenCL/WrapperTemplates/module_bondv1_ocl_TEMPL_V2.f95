#define GPU_KERNEL 1
#define CPU_KERNEL 2
module module_bondv1_ocl
    use module_LES_conversions
    use module_bondv1

contains

      subroutine bondv1_ocl(jm,u,z2,dzn,v,w,km,uout,n,im,dt,dxs)
!      subroutine bondv1_ocl(jm,u,z2,dzn,v,w,km,ical,n,im,dt,dxs)
      use common_sn 
      implicit none
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
!        integer, intent(In) :: ical
        real(kind=4), intent(In) :: uout
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(kp+2) , intent(In) :: z2

! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
!        integer, dimension(1) :: n_ptr
        real(kind=4), dimension(1) :: uout_ptr
        integer :: globalrange, localrange
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
!        n_ptr(1)=n
        uout_ptr(1)=uout
        globalrange = jp
        localrange = jp
!$ACC Kernel(bondv1_kernel), GlobalRange(jm), LocalRange(jm)
        call bondv1(uvw,dxs,dzn,z2,uout_ptr,im,jm,km,dt)
!$ACC End Kernel

! Convert back
        call convert_from_uvw(uvw,u,v,w)

    end subroutine bondv1_ocl


      subroutine bondv1_calc_uout_ocl(u,v,w,uout,im,jm,km)
      use common_sn
      implicit none
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w

! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        integer, dimension(1) :: n_ptr
        integer :: globalrange, localrange, nthreads, i, j, k
        real(kind=4), dimension(jp) :: aaa_chunks, bbb_chunks ! ad-hoc
        real(kind=4), dimension(256) :: uout_ptr
        real(kind=4), intent(Out) :: uout
        real(kind=4) :: aaa, bbb
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
!        n_ptr(1)=n

#if KERNEL == GPU_KERNEL
        globalrange = jp
        localrange = jp
#elif KERNEL == CPU_KERNEL
        nthreads = NTH
        globalrange = jp*nthreads
        localrange = nthreads
#endif

!$ACC Kernel(bondv1_calc_uout_kernel), GlobalRange(jm), LocalRange(jm)
        call bondv1_calc_uout(uvw,uout_ptr,aaa_chunks,bbb_chunks,im,jm,km)
!$ACC End Kernel
#if KERNEL == CPU_KERNEL
    do j = 1,jp
        aaa = amax1(aaa,aaa_chunks(j))
        bbb = amin1(bbb,bbb_chunks(j))
    end do
    uout = (aaa + bbb)*0.5
#elif KERNEL == GPU_KERNEL
    uout = uout_ptr(1)
#endif
! Convert back
        call convert_from_uvw(uvw,u,v,w)

    end subroutine bondv1_calc_uout_ocl

 subroutine bondv1_calc_uvw_ocl(u,v,w,dxs,uout,dt,im,jm,km)
      use common_sn
      implicit none
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs


        real(kind=4), intent(In) :: uout
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km

        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w


! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw

        real(kind=4), dimension(1) :: uout_ptr
        integer :: globalrange, localrange
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)

        uout_ptr(1)=uout
        globalrange = (km*jm)+(km+2)*(im+2)+(im+3)*(jm+3)
        localrange = 0
!$ACC Kernel(bondv1_calc_uvw_kernel), GlobalRange(globalrange), LocalRange(localrange)
        call bondv1_calc_uvw(uvw,dxs,uout_ptr,im,jm,km,dt)
!$ACC End Kernel

! Convert back
        call convert_from_uvw(uvw,u,v,w)

    end subroutine bondv1_calc_uvw_ocl

end module module_bondv1_ocl

