module module_adam_ocl
    use module_LES_conversions
    use module_adam
    integer :: init_adam_ocl = 0
contains
      subroutine adam_ocl(n,nmax,data21,fold,im,jm,km,gold,hold,fghold,f,g,h)
          use oclWrapper
      use common_sn ! create_new_include_statements() line 102
        character(len=70), intent(In) :: data21
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(ip,jp,kp) , intent(In) :: fghold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        integer, intent(In) :: nmax
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,1:ip,1:jp,1:kp)  :: fgh_old
        
        ! OpenCL-specific declarations
        integer :: adam_ocl_nunits, adam_ocl_nthreads, adam_ocl_globalrange, adam_ocl_localrange
        integer :: init_adam_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/adam_kernel.cl"
        character(len=*), parameter :: kstr="adam_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
    
    ! OpenCL buffer declarations
    integer(8) :: fgh_buf
    integer(8) :: fgh_old_buf
    ! OpenCL buffer size declarations
    integer, dimension(4):: fgh_sz
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh_old(fold,gold,hold, fgh_old)
    
    if ( init_ocl_local /= 1 ) then 
      init_adam_ocl_local = 1
    if ( init_adam_ocl /= 1 ) then 
      init_adam_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(adam_ocl_nunits)
      call oclGetNThreadsHint(adam_ocl_nthreads)
    
    !  print *, "Compute units:",adam_ocl_nunits," Threads:",adam_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    fgh_sz = shape(fgh)
    fgh_old_sz = shape(fgh_old)
    
    ! Create OpenCL buffers
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadBuffer(fgh_old_buf,fgh_old_sz ,fgh_old)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, fgh_buf )
    call oclSetFloatArrayArg(1, fgh_old_buf )
    call oclSetIntConstArg(2, im )
    call oclSetIntConstArg(3, jm )
    call oclSetIntConstArg(4, km )
    
    end if
    
    adam_ocl_globalrange = im*jm*km
    adam_ocl_localrange = 0
    if (adam_ocl_localrange == 0) then
      call padRange(adam_ocl_globalrange,adam_ocl_nunits*adam_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(fgh_old_buf,fgh_old_sz,fgh_old)
    
    ! call adam(fgh,fgh_old,im,jm,km)
    call runOcl(adam_ocl_globalrange,adam_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    
    call convert_from_fgh(fgh,f,g,h)
    end subroutine adam_ocl
end module module_adam_ocl
