module module_feedbf_ocl
    use module_LES_conversions
    use module_feedbf
    integer :: init_feedbf_ocl = 0
contains
      subroutine feedbf_ocl(km,jm,im,usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha,dt,beta,fx,fy,fz,f,       g,h)
          use oclWrapper
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), intent(In) :: alpha
        real(kind=4), intent(In) :: beta
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(In) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: cmask1
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: dmask1
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fx
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fy
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fz
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: uvwsum
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:3,-1:ip+1,-1:jp+1,0:kp+1)  :: mask1
        
        ! OpenCL-specific declarations
        integer :: feedbf_ocl_nunits, feedbf_ocl_nthreads, feedbf_ocl_globalrange, feedbf_ocl_localrange
        integer :: init_feedbf_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/feedbf_kernel.cl"
        character(len=*), parameter :: kstr="feedbf_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: fgh_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: mask1_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_bcdmask1(bmask1,cmask1,dmask1,mask1)
! 
! 
        
        if ( init_ocl_local /= 1 ) then 
          init_feedbf_ocl_local = 1
        if ( init_feedbf_ocl /= 1 ) then 
          init_feedbf_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(feedbf_ocl_nunits)
          call oclGetNThreadsHint(feedbf_ocl_nthreads)
        
        !  print *, "Compute units:",feedbf_ocl_nunits," Threads:",feedbf_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        uvw_sz = shape(uvw)
        uvwsum_sz = shape(uvwsum)
        fgh_sz = shape(fgh)
        mask1_sz = shape(mask1)
        
        ! Create OpenCL buffers
        call oclMake4DFloatArrayReadBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake4DFloatArrayReadWriteBuffer(uvwsum_buf,uvwsum_sz ,uvwsum)
        call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
        call oclMake4DFloatArrayReadBuffer(mask1_buf,mask1_sz ,mask1)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, uvwsum_buf )
        call oclSetFloatArrayArg(2, fgh_buf )
        call oclSetFloatArrayArg(3, mask1_buf )
        call oclSetFloatConstArg(4, dt )
        call oclSetIntConstArg(5, im )
        call oclSetIntConstArg(6, jm )
        call oclSetIntConstArg(7, km )
        
        end if
        
        feedbf_ocl_globalrange = im*jm*km
        feedbf_ocl_localrange = 0
        if (feedbf_ocl_localrange == 0) then
          call padRange(feedbf_ocl_globalrange,feedbf_ocl_nunits*feedbf_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
        call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclWrite4DFloatArrayBuffer(mask1_buf,mask1_sz,mask1)
        
        ! call feedbf(uvw,uvwsum,fgh,mask1,dt,im,jm,km)
        call runOcl(feedbf_ocl_globalrange,feedbf_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
        call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        
     
         ! Convert back
        call convert_from_fgh(fgh,f,g,h)
!        call convert_from_uvw(uvw,u,v,w)
        call convert_from_fgh(uvwsum,usum,vsum,wsum) ! temporary: can pass around as-is!
      return
      end
end module module_feedbf_ocl
