module module_velnw_ocl
    use module_LES_conversions
    use module_velnw
    integer :: init_velnw_ocl = 0
contains
      subroutine velnw_ocl(km,jm,im,p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
          use oclWrapper
          use common_sn 
          implicit none
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), intent(In) :: ro
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        
        ! OpenCL-specific declarations
        integer :: velnw_ocl_nunits, velnw_ocl_nthreads, velnw_ocl_globalrange, velnw_ocl_localrange
        integer :: init_velnw_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/velnw_kernel.cl"
        character(len=*), parameter :: kstr="velnw_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: dxs_buf
        integer(8) :: dys_buf
        integer(8) :: dzs_buf
        integer(8) :: p_buf
        integer(8) :: uvw_buf
        integer(8) :: fgh_buf
        ! OpenCL buffer size declarations
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: dys_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(3):: p_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: fgh_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        
        if ( init_ocl_local /= 1 ) then 
          init_velnw_ocl_local = 1
        if ( init_velnw_ocl /= 1 ) then 
          init_velnw_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(velnw_ocl_nunits)
          call oclGetNThreadsHint(velnw_ocl_nthreads)
        
        !  print *, "Compute units:",velnw_ocl_nunits," Threads:",velnw_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        dxs_sz = shape(dxs)
        dys_sz = shape(dys)
        dzs_sz = shape(dzs)
        p_sz = shape(p)
        uvw_sz = shape(uvw)
        fgh_sz = shape(fgh)
        
        ! Create OpenCL buffers
        call oclMake1DFloatArrayReadBuffer(dxs_buf,dxs_sz ,dxs)
        call oclMake1DFloatArrayReadBuffer(dys_buf,dys_sz ,dys)
        call oclMake1DFloatArrayReadBuffer(dzs_buf,dzs_sz ,dzs)
        call oclMake3DFloatArrayReadBuffer(p_buf,p_sz ,p)
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake4DFloatArrayReadBuffer(fgh_buf,fgh_sz ,fgh)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, p_buf )
        call oclSetFloatArrayArg(1, uvw_buf )
        call oclSetFloatArrayArg(2, fgh_buf )
        call oclSetFloatArrayArg(3, dxs_buf )
        call oclSetFloatArrayArg(4, dys_buf )
        call oclSetFloatArrayArg(5, dzs_buf )
        call oclSetIntConstArg(6, im )
        call oclSetIntConstArg(7, jm )
        call oclSetIntConstArg(8, km )
        call oclSetFloatConstArg(9, dt )
        
        end if
        
        velnw_ocl_globalrange = im*jm*km
        velnw_ocl_localrange = 0
        if (velnw_ocl_localrange == 0) then
          call padRange(velnw_ocl_globalrange,velnw_ocl_nunits*velnw_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite1DFloatArrayBuffer(dxs_buf,dxs_sz,dxs)
        call oclWrite1DFloatArrayBuffer(dys_buf,dys_sz,dys)
        call oclWrite1DFloatArrayBuffer(dzs_buf,dzs_sz,dzs)
        call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        
        ! call velnw( p, uvw, fgh, dxs,dys,dzs, im, jm, km, dt)
        call runOcl(velnw_ocl_globalrange,velnw_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        
         ! Convert back
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
! 
      return
      end subroutine velnw_ocl
end module module_velnw_ocl
