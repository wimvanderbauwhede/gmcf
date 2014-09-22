#define GPU_KERNEL 1
#define CPU_KERNEL 2
module module_press_ocl
    use module_LES_conversions
    use module_press
      use module_bondFG
      use module_boundp
    integer :: init_press_boundp_ocl = 0
    integer :: init_press_adj_ocl = 0
    integer :: init_press_rhsav_ocl = 0
    integer :: init_press_sor_ocl = 0
    integer :: init_press_pav_ocl = 0
contains
      subroutine press_rhsav_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
          use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: press_rhsav_ocl_nunits, press_rhsav_ocl_nthreads, press_rhsav_ocl_globalrange, press_rhsav_ocl_localrange
        integer :: init_press_rhsav_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/press_rhsav_kernel.cl"
        character(len=*), parameter :: kstr="press_rhsav_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: dx1_buf
    integer(8) :: dy1_buf
    integer(8) :: dzn_buf
    integer(8) :: rhs_buf
    integer(8) :: fgh_buf
    integer(8) :: uvw_buf
    integer(8) :: chunks_num_buf
    integer(8) :: chunks_denom_buf
    ! OpenCL buffer size declarations
    integer, dimension(1):: dx1_sz
    integer, dimension(1):: dy1_sz
    integer, dimension(1):: dzn_sz
    integer, dimension(3):: rhs_sz
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: uvw_sz
    integer, dimension(1):: chunks_num_sz
    integer, dimension(1):: chunks_denom_sz
    integer(8) :: cn1_buf
    integer(8) :: cn2l_buf
    integer(8) :: cn2s_buf
    integer(8) :: cn3l_buf
    integer(8) :: cn3s_buf
    integer(8) :: cn4l_buf
    integer(8) :: cn4s_buf
    integer(8) :: p_buf
    integer(8) :: nrd_ptr_buf
    integer(8) :: val_ptr_buf
    integer(8) :: p_scratch_buf
    integer, dimension(3):: cn1_sz
    integer, dimension(1):: cn2l_sz
    integer, dimension(1):: cn2s_sz
    integer, dimension(1):: cn3l_sz
    integer, dimension(1):: cn3s_sz
    integer, dimension(1):: cn4l_sz
    integer, dimension(1):: cn4s_sz
    integer, dimension(3):: p_sz
    integer, dimension(1):: nrd_ptr_sz
    integer, dimension(1):: val_ptr_sz
    integer, dimension(3):: p_scratch_sz
    integer(8) :: pav_buf
    integer, dimension(1):: pav_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
        ksor(1)=0.0
    
    if ( init_ocl_local /= 1 ) then 
      init_press_rhsav_ocl_local = 1
    if ( init_press_rhsav_ocl /= 1 ) then 
      init_press_rhsav_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(press_rhsav_ocl_nunits)
      call oclGetNThreadsHint(press_rhsav_ocl_nthreads)
    
    !  print *, "Compute units:",press_rhsav_ocl_nunits," Threads:",press_rhsav_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    rhs_sz = shape(rhs)
    fgh_sz = shape(fgh)
    uvw_sz = shape(uvw)
    chunks_num_sz = shape(chunks_num)
    chunks_denom_sz = shape(chunks_denom)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(rhs_buf,rhs_sz ,rhs)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadBuffer(uvw_buf,uvw_sz ,uvw)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_num_buf,chunks_num_sz ,chunks_num)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_denom_buf,chunks_denom_sz ,chunks_denom)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, uvw_buf )
    call oclSetFloatArrayArg(1, fgh_buf )
    call oclSetFloatArrayArg(2, rhs_buf )
    call oclSetFloatArrayArg(3, dx1_buf )
    call oclSetFloatArrayArg(4, dy1_buf )
    call oclSetFloatArrayArg(5, dzn_buf )
    call oclSetFloatArrayArg(6, chunks_num_buf )
    call oclSetFloatArrayArg(7, chunks_denom_buf )
    call oclSetFloatConstArg(8, dt )
    call oclSetIntConstArg(9, im )
    call oclSetIntConstArg(10, jm )
    call oclSetIntConstArg(11, km )
    
    end if
    
    press_rhsav_ocl_globalrange = globalrange
    press_rhsav_ocl_localrange = localrange
    if (press_rhsav_ocl_localrange == 0) then
      call padRange(press_rhsav_ocl_globalrange,press_rhsav_ocl_nunits*press_rhsav_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(rhs_buf,rhs_sz,rhs)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
    call oclWrite1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclWrite1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    
    ! call press_rhsav( uvw, fgh, rhs, dx1,dy1,dzn, chunks_num, chunks_denom,dt, im, jm, km)
    call runOcl(press_rhsav_ocl_globalrange,press_rhsav_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(rhs_buf,rhs_sz,rhs)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclRead1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    
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
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: press_sor_ocl_nunits, press_sor_ocl_nthreads, press_sor_ocl_globalrange, press_sor_ocl_localrange
        integer :: init_press_sor_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/press_sor_kernel.cl"
        character(len=*), parameter :: kstr="press_sor_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: dx1_buf
    integer(8) :: dy1_buf
    integer(8) :: dzn_buf
    integer(8) :: rhs_buf
    integer(8) :: fgh_buf
    integer(8) :: uvw_buf
    integer(8) :: chunks_num_buf
    integer(8) :: chunks_denom_buf
    ! OpenCL buffer size declarations
    integer, dimension(1):: dx1_sz
    integer, dimension(1):: dy1_sz
    integer, dimension(1):: dzn_sz
    integer, dimension(3):: rhs_sz
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: uvw_sz
    integer, dimension(1):: chunks_num_sz
    integer, dimension(1):: chunks_denom_sz
    integer(8) :: cn1_buf
    integer(8) :: cn2l_buf
    integer(8) :: cn2s_buf
    integer(8) :: cn3l_buf
    integer(8) :: cn3s_buf
    integer(8) :: cn4l_buf
    integer(8) :: cn4s_buf
    integer(8) :: p_buf
    integer(8) :: nrd_ptr_buf
    integer(8) :: val_ptr_buf
    integer(8) :: p_scratch_buf
    integer, dimension(3):: cn1_sz
    integer, dimension(1):: cn2l_sz
    integer, dimension(1):: cn2s_sz
    integer, dimension(1):: cn3l_sz
    integer, dimension(1):: cn3s_sz
    integer, dimension(1):: cn4l_sz
    integer, dimension(1):: cn4s_sz
    integer, dimension(3):: p_sz
    integer, dimension(1):: nrd_ptr_sz
    integer, dimension(1):: val_ptr_sz
    integer, dimension(3):: p_scratch_sz
    integer(8) :: pav_buf
    integer, dimension(1):: pav_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
                globalrange = (ip+2)*(jp+2)
                localrange = jp+2
                ngroups = ip+2
#else
                globalrange = (ip+2)*NTH
                localrange = NTH
                ngroups = ip+2
#endif
            end if
#endif
    
    if ( init_ocl_local /= 1 ) then 
      init_press_sor_ocl_local = 1
    if ( init_press_sor_ocl /= 1 ) then 
      init_press_sor_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(press_sor_ocl_nunits)
      call oclGetNThreadsHint(press_sor_ocl_nthreads)
    
    !  print *, "Compute units:",press_sor_ocl_nunits," Threads:",press_sor_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    cn1_sz = shape(cn1)
    cn2l_sz = shape(cn2l)
    cn2s_sz = shape(cn2s)
    cn3l_sz = shape(cn3l)
    cn3s_sz = shape(cn3s)
    cn4l_sz = shape(cn4l)
    cn4s_sz = shape(cn4s)
    p_sz = shape(p)
    rhs_sz = shape(rhs)
    uvw_sz = shape(uvw)
    nrd_ptr_sz = shape(nrd_ptr)
    val_ptr_sz = shape(val_ptr)
    chunks_num_sz = shape(chunks_num)
    chunks_denom_sz = shape(chunks_denom)
    p_scratch_sz = shape(p_scratch)
    
    ! Create OpenCL buffers
    call oclMake3DFloatArrayReadBuffer(cn1_buf,cn1_sz ,cn1)
    call oclMake1DFloatArrayReadBuffer(cn2l_buf,cn2l_sz ,cn2l)
    call oclMake1DFloatArrayReadBuffer(cn2s_buf,cn2s_sz ,cn2s)
    call oclMake1DFloatArrayReadBuffer(cn3l_buf,cn3l_sz ,cn3l)
    call oclMake1DFloatArrayReadBuffer(cn3s_buf,cn3s_sz ,cn3s)
    call oclMake1DFloatArrayReadBuffer(cn4l_buf,cn4l_sz ,cn4l)
    call oclMake1DFloatArrayReadBuffer(cn4s_buf,cn4s_sz ,cn4s)
    call oclMake3DFloatArrayReadWriteBuffer(p_buf,p_sz ,p)
    call oclMake3DFloatArrayReadWriteBuffer(rhs_buf,rhs_sz ,rhs)
    call oclMake4DFloatArrayReadBuffer(uvw_buf,uvw_sz ,uvw)
    call oclMake1DIntArrayReadBuffer(nrd_ptr_buf,nrd_ptr_sz ,nrd_ptr)
    call oclMake1DFloatArrayReadBuffer(val_ptr_buf,val_ptr_sz ,val_ptr)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_num_buf,chunks_num_sz ,chunks_num)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_denom_buf,chunks_denom_sz ,chunks_denom)
    call oclMake3DFloatArrayReadWriteBuffer(p_scratch_buf,p_scratch_sz ,p_scratch)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, uvw_buf )
    call oclSetFloatArrayArg(1, p_buf )
    call oclSetFloatArrayArg(2, p_scratch_buf )
    call oclSetFloatArrayArg(3, rhs_buf )
    call oclSetFloatArrayArg(4, cn1_buf )
    call oclSetFloatArrayArg(5, cn2l_buf )
    call oclSetFloatArrayArg(6, cn2s_buf )
    call oclSetFloatArrayArg(7, cn3l_buf )
    call oclSetFloatArrayArg(8, cn3s_buf )
    call oclSetFloatArrayArg(9, cn4l_buf )
    call oclSetFloatArrayArg(10, cn4s_buf )
    call oclSetFloatArrayArg(11, chunks_num_buf )
    call oclSetFloatArrayArg(12, chunks_denom_buf )
    call oclSetFloatArrayArg(13, val_ptr_buf )
    call oclSetIntArrayArg(14, nrd_ptr_buf )
    call oclSetIntConstArg(15, im )
    call oclSetIntConstArg(16, jm )
    call oclSetIntConstArg(17, km )
    
    end if
    
    press_sor_ocl_globalrange = globalrange
    press_sor_ocl_localrange = localrange
    if (press_sor_ocl_localrange == 0) then
      call padRange(press_sor_ocl_globalrange,press_sor_ocl_nunits*press_sor_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite3DFloatArrayBuffer(cn1_buf,cn1_sz,cn1)
    call oclWrite1DFloatArrayBuffer(cn2l_buf,cn2l_sz,cn2l)
    call oclWrite1DFloatArrayBuffer(cn2s_buf,cn2s_sz,cn2s)
    call oclWrite1DFloatArrayBuffer(cn3l_buf,cn3l_sz,cn3l)
    call oclWrite1DFloatArrayBuffer(cn3s_buf,cn3s_sz,cn3s)
    call oclWrite1DFloatArrayBuffer(cn4l_buf,cn4l_sz,cn4l)
    call oclWrite1DFloatArrayBuffer(cn4s_buf,cn4s_sz,cn4s)
    call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
    call oclWrite3DFloatArrayBuffer(rhs_buf,rhs_sz,rhs)
    call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
    call oclWrite1DIntArrayBuffer(nrd_ptr_buf,nrd_ptr_sz,nrd_ptr)
    call oclWrite1DFloatArrayBuffer(val_ptr_buf,val_ptr_sz,val_ptr)
    call oclWrite1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclWrite1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    call oclWrite3DFloatArrayBuffer(p_scratch_buf,p_scratch_sz,p_scratch)
    
    ! call press_sor( uvw, p, p_scratch, rhs, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,     chunks_num, chunks_denom, val_ptr, nrd_ptr, im, jm, km)
    call runOcl(press_sor_ocl_globalrange,press_sor_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(p_buf,p_sz,p)
    call oclRead3DFloatArrayBuffer(rhs_buf,rhs_sz,rhs)
    call oclRead1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclRead1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    call oclRead3DFloatArrayBuffer(p_scratch_buf,p_scratch_sz,p_scratch)
    
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
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: press_pav_ocl_nunits, press_pav_ocl_nthreads, press_pav_ocl_globalrange, press_pav_ocl_localrange
        integer :: init_press_pav_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/press_pav_kernel.cl"
        character(len=*), parameter :: kstr="press_pav_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: dx1_buf
    integer(8) :: dy1_buf
    integer(8) :: dzn_buf
    integer(8) :: rhs_buf
    integer(8) :: fgh_buf
    integer(8) :: uvw_buf
    integer(8) :: chunks_num_buf
    integer(8) :: chunks_denom_buf
    ! OpenCL buffer size declarations
    integer, dimension(1):: dx1_sz
    integer, dimension(1):: dy1_sz
    integer, dimension(1):: dzn_sz
    integer, dimension(3):: rhs_sz
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: uvw_sz
    integer, dimension(1):: chunks_num_sz
    integer, dimension(1):: chunks_denom_sz
    integer(8) :: cn1_buf
    integer(8) :: cn2l_buf
    integer(8) :: cn2s_buf
    integer(8) :: cn3l_buf
    integer(8) :: cn3s_buf
    integer(8) :: cn4l_buf
    integer(8) :: cn4s_buf
    integer(8) :: p_buf
    integer(8) :: nrd_ptr_buf
    integer(8) :: val_ptr_buf
    integer(8) :: p_scratch_buf
    integer, dimension(3):: cn1_sz
    integer, dimension(1):: cn2l_sz
    integer, dimension(1):: cn2s_sz
    integer, dimension(1):: cn3l_sz
    integer, dimension(1):: cn3s_sz
    integer, dimension(1):: cn4l_sz
    integer, dimension(1):: cn4s_sz
    integer, dimension(3):: p_sz
    integer, dimension(1):: nrd_ptr_sz
    integer, dimension(1):: val_ptr_sz
    integer, dimension(3):: p_scratch_sz
    integer(8) :: pav_buf
    integer, dimension(1):: pav_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
        ksor(1)=0.0
    
    if ( init_ocl_local /= 1 ) then 
      init_press_pav_ocl_local = 1
    if ( init_press_pav_ocl /= 1 ) then 
      init_press_pav_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(press_pav_ocl_nunits)
      call oclGetNThreadsHint(press_pav_ocl_nthreads)
    
    !  print *, "Compute units:",press_pav_ocl_nunits," Threads:",press_pav_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    p_sz = shape(p)
    chunks_num_sz = shape(chunks_num)
    chunks_denom_sz = shape(chunks_denom)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(p_buf,p_sz ,p)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_num_buf,chunks_num_sz ,chunks_num)
    call oclMake1DFloatArrayReadWriteBuffer(chunks_denom_buf,chunks_denom_sz ,chunks_denom)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, p_buf )
    call oclSetFloatArrayArg(1, dx1_buf )
    call oclSetFloatArrayArg(2, dy1_buf )
    call oclSetFloatArrayArg(3, dzn_buf )
    call oclSetFloatArrayArg(4, chunks_num_buf )
    call oclSetFloatArrayArg(5, chunks_denom_buf )
    call oclSetIntConstArg(6, im )
    call oclSetIntConstArg(7, jm )
    call oclSetIntConstArg(8, km )
    
    end if
    
    press_pav_ocl_globalrange = globalrange
    press_pav_ocl_localrange = localrange
    if (press_pav_ocl_localrange == 0) then
      call padRange(press_pav_ocl_globalrange,press_pav_ocl_nunits*press_pav_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
    call oclWrite1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclWrite1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    
    ! call press_pav( p, dx1,dy1,dzn,chunks_num, chunks_denom, im, jm, km)
    call runOcl(press_pav_ocl_globalrange,press_pav_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(p_buf,p_sz,p)
    call oclRead1DFloatArrayBuffer(chunks_num_buf,chunks_num_sz,chunks_num)
    call oclRead1DFloatArrayBuffer(chunks_denom_buf,chunks_denom_sz,chunks_denom)
    
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
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: press_adj_ocl_nunits, press_adj_ocl_nthreads, press_adj_ocl_globalrange, press_adj_ocl_localrange
        integer :: init_press_adj_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/press_adj_kernel.cl"
        character(len=*), parameter :: kstr="press_adj_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: dx1_buf
    integer(8) :: dy1_buf
    integer(8) :: dzn_buf
    integer(8) :: rhs_buf
    integer(8) :: fgh_buf
    integer(8) :: uvw_buf
    integer(8) :: chunks_num_buf
    integer(8) :: chunks_denom_buf
    ! OpenCL buffer size declarations
    integer, dimension(1):: dx1_sz
    integer, dimension(1):: dy1_sz
    integer, dimension(1):: dzn_sz
    integer, dimension(3):: rhs_sz
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: uvw_sz
    integer, dimension(1):: chunks_num_sz
    integer, dimension(1):: chunks_denom_sz
    integer(8) :: cn1_buf
    integer(8) :: cn2l_buf
    integer(8) :: cn2s_buf
    integer(8) :: cn3l_buf
    integer(8) :: cn3s_buf
    integer(8) :: cn4l_buf
    integer(8) :: cn4s_buf
    integer(8) :: p_buf
    integer(8) :: nrd_ptr_buf
    integer(8) :: val_ptr_buf
    integer(8) :: p_scratch_buf
    integer, dimension(3):: cn1_sz
    integer, dimension(1):: cn2l_sz
    integer, dimension(1):: cn2s_sz
    integer, dimension(1):: cn3l_sz
    integer, dimension(1):: cn3s_sz
    integer, dimension(1):: cn4l_sz
    integer, dimension(1):: cn4s_sz
    integer, dimension(3):: p_sz
    integer, dimension(1):: nrd_ptr_sz
    integer, dimension(1):: val_ptr_sz
    integer, dimension(3):: p_scratch_sz
    integer(8) :: pav_buf
    integer, dimension(1):: pav_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
        globalrange = im*jm*km
        localrange = 0
        pav(1) = rhs(0,0,0)
!
    
    if ( init_ocl_local /= 1 ) then 
      init_press_adj_ocl_local = 1
    if ( init_press_adj_ocl /= 1 ) then 
      init_press_adj_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(press_adj_ocl_nunits)
      call oclGetNThreadsHint(press_adj_ocl_nthreads)
    
    !  print *, "Compute units:",press_adj_ocl_nunits," Threads:",press_adj_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    p_sz = shape(p)
    pav_sz = shape(pav)
    
    ! Create OpenCL buffers
    call oclMake3DFloatArrayReadWriteBuffer(p_buf,p_sz ,p)
    call oclMake1DFloatArrayReadBuffer(pav_buf,pav_sz ,pav)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, p_buf )
    call oclSetFloatArrayArg(1, pav_buf )
    call oclSetIntConstArg(2, im )
    call oclSetIntConstArg(3, jm )
    call oclSetIntConstArg(4, km )
    
    end if
    
    press_adj_ocl_globalrange = globalrange
    press_adj_ocl_localrange = localrange
    if (press_adj_ocl_localrange == 0) then
      call padRange(press_adj_ocl_globalrange,press_adj_ocl_nunits*press_adj_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
    call oclWrite1DFloatArrayBuffer(pav_buf,pav_sz,pav)
    
    ! call press_adj(p, pav, im, jm, km)
    call runOcl(press_adj_ocl_globalrange,press_adj_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(p_buf,p_sz,p)
    
    end subroutine press_adj_ocl
! --------------------------------------------------------------------------------------------------------------------------------
    subroutine press_boundp_ocl(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n,nmax,data20,usum,vsum,wsum)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: press_boundp_ocl_nunits, press_boundp_ocl_nthreads, press_boundp_ocl_globalrange, press_boundp_ocl_localrange
        integer :: init_press_boundp_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/press_boundp_kernel.cl"
        character(len=*), parameter :: kstr="press_boundp_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: dx1_buf
    integer(8) :: dy1_buf
    integer(8) :: dzn_buf
    integer(8) :: rhs_buf
    integer(8) :: fgh_buf
    integer(8) :: uvw_buf
    integer(8) :: chunks_num_buf
    integer(8) :: chunks_denom_buf
    ! OpenCL buffer size declarations
    integer, dimension(1):: dx1_sz
    integer, dimension(1):: dy1_sz
    integer, dimension(1):: dzn_sz
    integer, dimension(3):: rhs_sz
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: uvw_sz
    integer, dimension(1):: chunks_num_sz
    integer, dimension(1):: chunks_denom_sz
    integer(8) :: cn1_buf
    integer(8) :: cn2l_buf
    integer(8) :: cn2s_buf
    integer(8) :: cn3l_buf
    integer(8) :: cn3s_buf
    integer(8) :: cn4l_buf
    integer(8) :: cn4s_buf
    integer(8) :: p_buf
    integer(8) :: nrd_ptr_buf
    integer(8) :: val_ptr_buf
    integer(8) :: p_scratch_buf
    integer, dimension(3):: cn1_sz
    integer, dimension(1):: cn2l_sz
    integer, dimension(1):: cn2s_sz
    integer, dimension(1):: cn3l_sz
    integer, dimension(1):: cn3s_sz
    integer, dimension(1):: cn4l_sz
    integer, dimension(1):: cn4s_sz
    integer, dimension(3):: p_sz
    integer, dimension(1):: nrd_ptr_sz
    integer, dimension(1):: val_ptr_sz
    integer, dimension(3):: p_scratch_sz
    integer(8) :: pav_buf
    integer, dimension(1):: pav_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
    
    if ( init_ocl_local /= 1 ) then 
      init_press_boundp_ocl_local = 1
    if ( init_press_boundp_ocl /= 1 ) then 
      init_press_boundp_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(press_boundp_ocl_nunits)
      call oclGetNThreadsHint(press_boundp_ocl_nthreads)
    
    !  print *, "Compute units:",press_boundp_ocl_nunits," Threads:",press_boundp_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    p_sz = shape(p)
    
    ! Create OpenCL buffers
    call oclMake3DFloatArrayReadWriteBuffer(p_buf,p_sz ,p)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, p_buf )
    call oclSetIntConstArg(1, im )
    call oclSetIntConstArg(2, jm )
    call oclSetIntConstArg(3, km )
    
    end if
    
    press_boundp_ocl_globalrange = globalrange
    press_boundp_ocl_localrange = localrange
    if (press_boundp_ocl_localrange == 0) then
      call padRange(press_boundp_ocl_globalrange,press_boundp_ocl_nunits*press_boundp_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
    
    ! call press_boundp( p, im, jm, km)
    call runOcl(press_boundp_ocl_globalrange,press_boundp_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(p_buf,p_sz,p)
    
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_fgh(uvwsum,usum,vsum,wsum)
        call convert_from_uvw(uvw,u,v,w)
    end subroutine press_boundp_ocl
end module module_press_ocl
