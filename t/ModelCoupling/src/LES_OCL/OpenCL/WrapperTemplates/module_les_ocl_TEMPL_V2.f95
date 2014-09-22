! This is the module corresponding to les.f95 in the refactored Fortran code
! The naming is unfortunate, this is just one of the subroutines called by the simulator
! The overall code is module_LES_combined_ocl_TEMPL.f95
module module_les_ocl
    use module_LES_conversions
    use module_les
      use module_boundsm ! add_module_decls() line 156
    contains

    subroutine bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl(f,g,h,u,v,w,usum,vsum,wsum,sm,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,dx1,dy1,dzn,dzs,dxs,uout, bmask1,cmask1,dmask1,dt, im, jm, km)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h

        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm

        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), intent(In) :: uout
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: cmask1
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: dmask1
        real(kind=4), intent(In) :: dt

        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km

        integer :: max_range
        real(kind=4), dimension(1) :: uout_ptr
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: uvwsum
        real(kind=4), dimension(0:3,-1:ip+1,-1:jp+1,0:kp+1)  :: mask1
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_bcdmask1(bmask1,cmask1,dmask1,mask1)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
        uout_ptr(1)=uout
!$ACC Kernel(bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel), GlobalRange(im*jm*km), LocalRange(0)
        call bondv1_calc_uvw__velfg__feedbf__les_calc_sm(uvw,uvwsum,fgh,mask1,diu,dxs,dzs,dx1,dy1,dzn,sm,uout_ptr,dt,im,jm,km)
!$ACC End Kernel

         ! Convert back for velFG
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
        call convert_from_9vec(diu,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9)
         ! Convert back for feedbf
        call convert_from_fgh(uvwsum,usum,vsum,wsum) ! temporary: can pass around as-is!

    end subroutine bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl

    subroutine merged_velfg_feedbf_les_calc_sm_ocl(f,g,h,u,v,w,usum,vsum,wsum,sm,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,dx1,dy1,dzn,dzs, bmask1,cmask1,dmask1,dt, im, jm, km)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h

        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm

        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: cmask1
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: dmask1
        real(kind=4), intent(In) :: dt

        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km

        integer :: max_range
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: uvwsum
        real(kind=4), dimension(0:3,-1:ip+1,-1:jp+1,0:kp+1)  :: mask1
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_bcdmask1(bmask1,cmask1,dmask1,mask1)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)

    !$ACC Kernel(merged_velfg_feedbf_les_calc_sm_kernel), GlobalRange(im*jm*km), LocalRange(0)
    call merged_velfg_feedbf_les_calc_sm(uvw,uvwsum,fgh,mask1,diu,dzs,dx1,dy1,dzn,sm,dt,im,jm,km)
     !$ACC End Kernel

         ! Convert back for velFG
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
        call convert_from_9vec(diu,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9)
         ! Convert back for feedbf
        call convert_from_fgh(uvwsum,usum,vsum,wsum) ! temporary: can pass around as-is!

    end subroutine merged_velfg_feedbf_les_calc_sm_ocl

    subroutine les_calc_sm_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, h)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
        integer :: max_range
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)

! We will split this into three kernels, but the second kernel could also be a host-side subroutine, I think that might be better.
! So, the generator really needs to be able to deal with multiple kernels in a single file. Fun!

! diu in, sm in/out (sm needs to be read in once, in init), fgh in

    !$ACC Kernel(les_calc_sm_kernel), GlobalRange(im*jm*km), LocalRange(0)
    call les_calc_sm(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    !$ACC End Kernel

!    max_range = max(im+3,jm+3,km+2)

! sm in and out, which means it can stay on the device unless we do this calc locally
    ! Kernel(les_kernel_bound_sm), GlobalRange(max_range*max_range), LocalRange(max_range)
!    call les_bound_sm(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    ! End Kernel
! diu, sm and fgh in, fgh out, so here I need to read back fgh for sure, maybe also sm, for avesm
    ! Kernel(les_kernel_calc_visc), GlobalRange(im*jm*km), LocalRange(0)
!    call les_calc_visc(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    ! End Kernel


    end subroutine les_calc_sm_ocl

    subroutine les_bound_sm_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, h)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
        integer :: max_range, globalrange, localrange
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)

! We will split this into three kernels, but the second kernel could also be a host-side subroutine, I think that might be better.
! So, the generator really needs to be able to deal with multiple kernels in a single file. Fun!
#define CPU_KERNEL
#ifndef CPU_KERNEL
    max_range = max(im+3,jm+3,km+2)
globalrange = max_range*max_range
localrange = max_range
#else
globalrange = (jm+3)*(km+2) + (km+2)*(im+2) + (jm+3)*(im+2)
localrange = 0
#endif
! sm in and out, which means it can stay on the device unless we do this calc locally
    !$ACC Kernel(les_bound_sm_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call les_bound_sm(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    !$ACC End Kernel


      end subroutine les_bound_sm_ocl

    subroutine les_calc_visc_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, h)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
        integer :: max_range
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)


! diu, sm and fgh in, fgh out, so here I need to read back fgh for sure, maybe also sm, for avesm
    !$ACC Kernel(les_calc_visc_kernel), GlobalRange(im*jm*km), LocalRange(0)
    call les_calc_visc(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    !$ACC End Kernel

    call convert_from_fgh(fgh,f,g,h)

    end subroutine les_calc_visc_ocl

    subroutine les_calc_visc__adam_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,fold,gold,hold,fghold,f,g, h)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(In) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: fghold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold


        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
        integer :: max_range
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,1:ip,1:jp,1:kp)  :: fgh_old
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2) :: diu
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh_old(fold,gold,hold, fgh_old)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)


! diu, sm and fgh in, fgh out, so here I need to read back fgh for sure, maybe also sm, for avesm
    !$ACC Kernel(les_calc_visc__adam_kernel), GlobalRange(im*jm*km), LocalRange(0)
    call les_calc_visc__adam(fgh,fgh_old,dx1,dy1,dzn,diu,sm,im,jm,km)
    !$ACC End Kernel

    call convert_from_fgh(fgh,f,g,h)
    call convert_from_fgh_old(fgh_old,fold,gold,hold)

    end subroutine les_calc_visc__adam_ocl
end module module_les_ocl
