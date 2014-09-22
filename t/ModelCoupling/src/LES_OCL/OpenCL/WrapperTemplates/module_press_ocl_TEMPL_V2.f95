#define GPU_KERNEL 1
#define CPU_KERNEL 2
module module_press_ocl
    use module_LES_conversions
    use module_press
      use module_bondFG
      use module_boundp
contains
      subroutine press_rhsav_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
      use common_sn
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        character(len=70), intent(In) :: data20
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum
! 
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) :: uvwsum
        integer :: globalrange, localrange, ngroups, k
        real(kind=4), dimension(1) :: ksor
        integer, dimension(1) :: nrd_ptr
        real(kind=4) :: rhsav, area
        real(kind=4), dimension(kp) :: chunks_num,chunks_denom

 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_uvw(u,v,w,uvw)

#if KERNEL == GPU_KERNEL
        globalrange = im*km
        localrange = im
#elif KERNEL == CPU_KERNEL
        globalrange = NTH*km
        localrange = NTH
#else
#error "Value for KERNEL not supported!"
#endif
        ngroups = km
        do k = 1,kp
            chunks_num(k) = 0.0
            chunks_denom(k) = 0.0
        end do
        ksor(1)=0.0;

    !$ACC Kernel(press_rhsav_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call press_rhsav( uvw, fgh, rhs, dx1,dy1,dzn, chunks_num, chunks_denom,dt, im, jm, km)
    !$ACC End Kernel
!         print *, "OCL: RHS_SUM_NOBOUNDS: ", ksor(1) ! DEBUG!
        ! Calc the average over the compute units
        rhsav = 0.0
        area = 0.0
        do k = 1,ngroups
        ! This is OK for CPU_KERNEL (seq)
!        print *, "RHSAV: ",k,chunks_num(k),chunks_denom(k)
            rhsav = rhsav + chunks_num(k)
            area = area + chunks_denom(k)
        end do
        rhsav = rhsav / area
        rhs(0,0,0) = rhsav
!        print *, "RHSAV rhsav: ", rhsav
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_fgh(uvwsum,usum,vsum,wsum)
        call convert_from_uvw(uvw,u,v,w)

    end subroutine press_rhsav_ocl
! --------------------------------------------------------------------------------------------------------------------------------
    subroutine press_sor_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
      use common_sn
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        character(len=70), intent(In) :: data20
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum

!#define APPROX_PAR_SOR
!#define CALCULATED_BOUNDS
#ifndef APPROX_PAR_SOR
integer, parameter  :: nmaxp = 50
#else
integer, parameter  :: nmaxp = 1
#endif
! --only used mac method
      real, parameter  :: pjuge = 0.0001

!      real, parameter  :: omega = 1.
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) :: uvwsum
        integer :: globalrange, localrange, ngroups, nrd, k, iter
        integer, dimension(1) :: nrd_ptr
        real(kind=4), dimension(1) :: val_ptr
        real(kind=4) :: sor, p_sum_nobounds, avg_iter
        real(kind=4), dimension(kp+2) :: chunks_num,chunks_denom
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) :: p_scratch
        p = 0.0
        p_scratch = p
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_uvw(u,v,w,uvw)

!    print *, "SOR rhsav: ", rhs(0,0,0)
    val_ptr(1) = rhs(0,0,0)
#define FIXED_NTH
!#define NTH 8
!#ifdef CALCULATED_BOUNDS
!    globalrange = (jp+2)*(kp+2)
!    localrange = (jp+2)
!#endif
    ngroups = kp+2 ! used to initialise chunks

!    print *, "NGROUPS: ",ngroups

!        print *, "SOR iter ",iter
!        ksor(1)=0.0
        do k = 1,ngroups
            chunks_num(k)=0.0
            chunks_denom(k) = 0.0
        end do
    do iter = 1,nmaxp
#ifndef APPROX_PAR_SOR
#ifndef CALCULATED_BOUNDS
        do nrd = 0,2
#else
        do nrd = 0,1
#endif
#else
            nrd = 0
#endif
            nrd_ptr(1)=nrd
#ifdef CALCULATED_BOUNDS
    globalrange = (jp+2)*(kp+2)
    localrange = (jp+2)
#else
! ndef CALCULATED_BOUNDS
!        print *,"SOR nrd (host): ",nrd
            if (nrd < 2) then
#ifndef FIXED_NTH
                globalrange = jp*kp
                localrange = jp
                ngroups = kp
#else
                globalrange = NTH*kp
                localrange = NTH
                ngroups = kp
#endif
            else
#ifndef FIXED_NTH
                globalrange = (ip+2)*(jp+2);
                localrange = jp+2
                ngroups = ip+2
#else
                globalrange = (ip+2)*NTH;
                localrange = NTH
                ngroups = ip+2
#endif
            end if
#endif

    !$ACC Kernel(press_sor_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call press_sor( uvw, p, p_scratch, rhs, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s, &
    chunks_num, chunks_denom, val_ptr, nrd_ptr, im, jm, km)
    !$ACC End Kernel
#ifndef APPROX_PAR_SOR
        end do ! nrd
#endif
!        p_sum_nobounds = 0.0
! we skip the two outer chunks as they are for the boundary conditions
       sor = 0.0
#ifndef CALCULATED_BOUNDS
        do k = 1,ngroups
#else
        do k = 2,ngroups-1
#endif
            sor = sor + chunks_num(k)
!            p_sum_nobounds = p_sum_nobounds + chunks_denom(k)
        end do

#ifndef APPROX_PAR_SOR
    avg_iter = iter
#else
    avg_iter = avg_iter + sum(chunks_denom)/ngroups
#endif

!#ifndef APPROX_PAR_SOR
!        print *, "iter:",iter," SOR error ",sor
        if (sor < pjuge) goto 7188
    end do ! iter

  7188 continue

! ORIG_KERNEL
!        ksor(1)=0.0
!
!    !$ACC Kernel(press_sor_kernel), GlobalRange(1), LocalRange(1)
!    call press_sor( uvw, uvwsum, fgh, p, rhs, dx1,dy1,dzn, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s, &
!    ksor, chunks_num, chunks_denom, &
!    dt, n, nmax, im, jm, km, nrd_ptr)
!    !$ACC End Kernel
!
!        sor=ksor(1)

        print *, "OCL: Residual SOR error after ",avg_iter," iterations: ",sor
!        print *, "OCL: P_SUM_NOBOUNDS: ",ksor(1)
!        print *, "OCL: P_SUM_NOBOUNDS: ",p_sum_nobounds
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_fgh(uvwsum,usum,vsum,wsum)
        call convert_from_uvw(uvw,u,v,w)

    end subroutine press_sor_ocl
! --------------------------------------------------------------------------------------------------------------------------------
    subroutine press_pav_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
      use common_sn
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        character(len=70), intent(In) :: data20
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum
!
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) :: uvwsum
        integer :: globalrange, localrange, k
        integer, dimension(1) :: nrd
        real(kind=4) :: pav, pco
        real(kind=4), dimension(1) :: ksor
        real(kind=4), dimension(kp) :: chunks_num,chunks_denom

 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_uvw(u,v,w,uvw)
!

#if KERNEL == GPU_KERNEL
        globalrange = ip*kp
        localrange = ip
#elif KERNEL == CPU_KERNEL
        globalrange = NTH*kp
        localrange = NTH
#else
#error "Value for KERNEL not supported!"
#endif
        ngroups = kp
        do k = 1,kp
            chunks_num(k) = 0.0
            chunks_denom(k) = 0.0
        end do
        ksor(1)=0.0;

    !$ACC Kernel(press_pav_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call press_pav( p, dx1,dy1,dzn,chunks_num, chunks_denom, im, jm, km)
    !$ACC End Kernel

        ! Calc the average over the compute units
        pav = 0.0
        pco = 0.0
        do k = 1,ngroups
        ! This is OK for CPU_KERNEL (seq)
!        print *, "PAV: ",k,chunks_num(k),chunks_denom(k) !,ksor(1)
            pav = pav + chunks_num(k)
            pco = pco + chunks_denom(k)
        end do
        pav = pav / pco
!        print *, "OCL: PAV: ",pav
        rhs(0,0,0) = pav

        call convert_from_fgh(fgh,f,g,h)
        call convert_from_fgh(uvwsum,usum,vsum,wsum)
        call convert_from_uvw(uvw,u,v,w)

    end subroutine press_pav_ocl
    ! --------------------------------------------------------------------------------------------------------------------------------
    subroutine press_adj_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
      use common_sn
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        character(len=70), intent(In) :: data20
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum
!
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) :: uvwsum
        integer :: globalrange, localrange, k
        integer, dimension(1) :: nrd
        real(kind=4), dimension(1) :: pav, rhsav, area
        real(kind=4), dimension(kp) :: chunks_num,chunks_denom

        globalrange = im*jm*km
        localrange = 0
        pav(1) = rhs(0,0,0)
!
    !$ACC Kernel(press_adj_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call press_adj(p, pav, im, jm, km)
    !$ACC End Kernel

    end subroutine press_adj_ocl
! --------------------------------------------------------------------------------------------------------------------------------
    subroutine press_boundp_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
      use common_sn
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: cn1
        real(kind=4), dimension(ip) , intent(In) :: cn2l
        real(kind=4), dimension(ip) , intent(In) :: cn2s
        real(kind=4), dimension(jp) , intent(In) :: cn3l
        real(kind=4), dimension(jp) , intent(In) :: cn3s
        real(kind=4), dimension(kp) , intent(In) :: cn4l
        real(kind=4), dimension(kp) , intent(In) :: cn4s
        character(len=70), intent(In) :: data20
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: rhs
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: wsum
!
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp) :: uvwsum
        integer :: globalrange, localrange, max_range
        integer, dimension(1) :: nrd
        real(kind=4), dimension(1) :: ksor
        real(kind=4), dimension(0:kp-1) :: chunks_num,chunks_denom

 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_uvw(u,v,w,uvw)
#if KERNEL == GPU_KERNEL
        max_range = max((im+2),(jm+2),(km+2))
        globalrange = max_range*max_range
        localrange = max_range
#elif KERNEL == CPU_KERNEL
        globalrange = (jm+2)*(km+2) + (km+2)*(im+2) + (jm+2)*(im+2)
        localrange = 0
#else
#error "Value for KERNEL not supported!"
#endif
!
    !$ACC Kernel(press_boundp_kernel), GlobalRange(globalrange), LocalRange(localrange)
    call press_boundp( p, im, jm, km)
    !$ACC End Kernel

        call convert_from_fgh(fgh,f,g,h)
        call convert_from_fgh(uvwsum,usum,vsum,wsum)
        call convert_from_uvw(uvw,u,v,w)

    end subroutine press_boundp_ocl

end module module_press_ocl
