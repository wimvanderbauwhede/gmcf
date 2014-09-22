#define GPU_KERNEL 1
#define CPU_KERNEL 2
module module_bondv1_ocl
    use module_LES_conversions
    use module_bondv1
    integer :: init_bondv1_calc_uvw_ocl = 0
    integer :: init_bondv1_ocl = 0
    integer :: init_bondv1_calc_uout_ocl = 0
contains
      subroutine bondv1_ocl(jm,u,z2,dzn,v,w,km,uout,n,im,dt,dxs)
          use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: bondv1_ocl_nunits, bondv1_ocl_nthreads, bondv1_ocl_globalrange, bondv1_ocl_localrange
        integer :: init_bondv1_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/bondv1_kernel.cl"
        character(len=*), parameter :: kstr="bondv1_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: dxs_buf
        integer(8) :: dzn_buf
        integer(8) :: z2_buf
        integer(8) :: uvw_buf
        integer(8) :: uout_ptr_buf
        ! OpenCL buffer size declarations
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: z2_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(1):: uout_ptr_sz
        integer(8) :: aaa_chunks_buf
        integer(8) :: bbb_chunks_buf
        integer, dimension(1):: aaa_chunks_sz
        integer, dimension(1):: bbb_chunks_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
!        n_ptr(1)=n
        uout_ptr(1)=uout
        globalrange = jp
        localrange = jp
        
        if ( init_ocl_local /= 1 ) then 
          init_bondv1_ocl_local = 1
        if ( init_bondv1_ocl /= 1 ) then 
          init_bondv1_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(bondv1_ocl_nunits)
          call oclGetNThreadsHint(bondv1_ocl_nthreads)
        
        !  print *, "Compute units:",bondv1_ocl_nunits," Threads:",bondv1_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        dxs_sz = shape(dxs)
        dzn_sz = shape(dzn)
        z2_sz = shape(z2)
        uvw_sz = shape(uvw)
        uout_ptr_sz = shape(uout_ptr)
        
        ! Create OpenCL buffers
        call oclMake1DFloatArrayReadBuffer(dxs_buf,dxs_sz ,dxs)
        call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
        call oclMake1DFloatArrayReadBuffer(z2_buf,z2_sz ,z2)
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake1DFloatArrayReadBuffer(uout_ptr_buf,uout_ptr_sz ,uout_ptr)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, dxs_buf )
        call oclSetFloatArrayArg(2, dzn_buf )
        call oclSetFloatArrayArg(3, z2_buf )
        call oclSetFloatArrayArg(4, uout_ptr_buf )
        call oclSetIntConstArg(5, im )
        call oclSetIntConstArg(6, jm )
        call oclSetIntConstArg(7, km )
        call oclSetFloatConstArg(8, dt )
        
        end if
        
        bondv1_ocl_globalrange = jm
        bondv1_ocl_localrange = jm
        if (bondv1_ocl_localrange == 0) then
          call padRange(bondv1_ocl_globalrange,bondv1_ocl_nunits*bondv1_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite1DFloatArrayBuffer(dxs_buf,dxs_sz,dxs)
        call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
        call oclWrite1DFloatArrayBuffer(z2_buf,z2_sz,z2)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        
        ! call bondv1(uvw,dxs,dzn,z2,uout_ptr,im,jm,km,dt)
        call runOcl(bondv1_ocl_globalrange,bondv1_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        
! Convert back
        call convert_from_uvw(uvw,u,v,w)
    end subroutine bondv1_ocl
      subroutine bondv1_calc_uout_ocl(u,v,w,uout,im,jm,km)
          use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: bondv1_calc_uout_ocl_nunits, bondv1_calc_uout_ocl_nthreads, bondv1_calc_uout_ocl_globalrange, bondv1_calc_uout_ocl_localrange
        integer :: init_bondv1_calc_uout_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/bondv1_calc_uout_kernel.cl"
        character(len=*), parameter :: kstr="bondv1_calc_uout_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: dxs_buf
        integer(8) :: dzn_buf
        integer(8) :: z2_buf
        integer(8) :: uvw_buf
        integer(8) :: uout_ptr_buf
        ! OpenCL buffer size declarations
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: z2_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(1):: uout_ptr_sz
        integer(8) :: aaa_chunks_buf
        integer(8) :: bbb_chunks_buf
        integer, dimension(1):: aaa_chunks_sz
        integer, dimension(1):: bbb_chunks_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
        
        if ( init_ocl_local /= 1 ) then 
          init_bondv1_calc_uout_ocl_local = 1
        if ( init_bondv1_calc_uout_ocl /= 1 ) then 
          init_bondv1_calc_uout_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(bondv1_calc_uout_ocl_nunits)
          call oclGetNThreadsHint(bondv1_calc_uout_ocl_nthreads)
        
        !  print *, "Compute units:",bondv1_calc_uout_ocl_nunits," Threads:",bondv1_calc_uout_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        uvw_sz = shape(uvw)
        aaa_chunks_sz = shape(aaa_chunks)
        bbb_chunks_sz = shape(bbb_chunks)
        uout_ptr_sz = shape(uout_ptr)
        
        ! Create OpenCL buffers
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake1DFloatArrayReadWriteBuffer(aaa_chunks_buf,aaa_chunks_sz ,aaa_chunks)
        call oclMake1DFloatArrayReadWriteBuffer(bbb_chunks_buf,bbb_chunks_sz ,bbb_chunks)
        call oclMake1DFloatArrayReadWriteBuffer(uout_ptr_buf,uout_ptr_sz ,uout_ptr)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, uout_ptr_buf )
        call oclSetFloatArrayArg(2, aaa_chunks_buf )
        call oclSetFloatArrayArg(3, bbb_chunks_buf )
        call oclSetIntConstArg(4, im )
        call oclSetIntConstArg(5, jm )
        call oclSetIntConstArg(6, km )
        
        end if
        
        bondv1_calc_uout_ocl_globalrange = jm
        bondv1_calc_uout_ocl_localrange = jm
        if (bondv1_calc_uout_ocl_localrange == 0) then
          call padRange(bondv1_calc_uout_ocl_globalrange,bondv1_calc_uout_ocl_nunits*bondv1_calc_uout_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite1DFloatArrayBuffer(aaa_chunks_buf,aaa_chunks_sz,aaa_chunks)
        call oclWrite1DFloatArrayBuffer(bbb_chunks_buf,bbb_chunks_sz,bbb_chunks)
        call oclWrite1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        
        ! call bondv1_calc_uout(uvw,uout_ptr,aaa_chunks,bbb_chunks,im,jm,km)
        call runOcl(bondv1_calc_uout_ocl_globalrange,bondv1_calc_uout_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclRead1DFloatArrayBuffer(aaa_chunks_buf,aaa_chunks_sz,aaa_chunks)
        call oclRead1DFloatArrayBuffer(bbb_chunks_buf,bbb_chunks_sz,bbb_chunks)
        call oclRead1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        
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
     use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: bondv1_calc_uvw_ocl_nunits, bondv1_calc_uvw_ocl_nthreads, bondv1_calc_uvw_ocl_globalrange, bondv1_calc_uvw_ocl_localrange
        integer :: init_bondv1_calc_uvw_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/bondv1_calc_uvw_kernel.cl"
        character(len=*), parameter :: kstr="bondv1_calc_uvw_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: dxs_buf
        integer(8) :: dzn_buf
        integer(8) :: z2_buf
        integer(8) :: uvw_buf
        integer(8) :: uout_ptr_buf
        ! OpenCL buffer size declarations
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: z2_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(1):: uout_ptr_sz
        integer(8) :: aaa_chunks_buf
        integer(8) :: bbb_chunks_buf
        integer, dimension(1):: aaa_chunks_sz
        integer, dimension(1):: bbb_chunks_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        uout_ptr(1)=uout
        globalrange = (km*jm)+(km+2)*(im+2)+(im+3)*(jm+3)
        localrange = 0
        
        if ( init_ocl_local /= 1 ) then 
          init_bondv1_calc_uvw_ocl_local = 1
        if ( init_bondv1_calc_uvw_ocl /= 1 ) then 
          init_bondv1_calc_uvw_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(bondv1_calc_uvw_ocl_nunits)
          call oclGetNThreadsHint(bondv1_calc_uvw_ocl_nthreads)
        
        !  print *, "Compute units:",bondv1_calc_uvw_ocl_nunits," Threads:",bondv1_calc_uvw_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        dxs_sz = shape(dxs)
        uvw_sz = shape(uvw)
        uout_ptr_sz = shape(uout_ptr)
        
        ! Create OpenCL buffers
        call oclMake1DFloatArrayReadBuffer(dxs_buf,dxs_sz ,dxs)
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake1DFloatArrayReadBuffer(uout_ptr_buf,uout_ptr_sz ,uout_ptr)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, dxs_buf )
        call oclSetFloatArrayArg(2, uout_ptr_buf )
        call oclSetIntConstArg(3, im )
        call oclSetIntConstArg(4, jm )
        call oclSetIntConstArg(5, km )
        call oclSetFloatConstArg(6, dt )
        
        end if
        
        bondv1_calc_uvw_ocl_globalrange = globalrange
        bondv1_calc_uvw_ocl_localrange = localrange
        if (bondv1_calc_uvw_ocl_localrange == 0) then
          call padRange(bondv1_calc_uvw_ocl_globalrange,bondv1_calc_uvw_ocl_nunits*bondv1_calc_uvw_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite1DFloatArrayBuffer(dxs_buf,dxs_sz,dxs)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        
        ! call bondv1_calc_uvw(uvw,dxs,uout_ptr,im,jm,km,dt)
        call runOcl(bondv1_calc_uvw_ocl_globalrange,bondv1_calc_uvw_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        
! Convert back
        call convert_from_uvw(uvw,u,v,w)
    end subroutine bondv1_calc_uvw_ocl
end module module_bondv1_ocl
