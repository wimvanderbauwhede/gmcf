module module_velnw__bondv1_init_uvw_ocl
    use module_LES_conversions
    use module_velnw
    integer :: init_velnw__bondv1_init_uvw_ocl = 0
contains
      subroutine velnw__bondv1_init_uvw_ocl(p,u,v,w,f,g,h,dxs,dys,dzs,dzn,z2,n,ro,dt,im,jm,km)
          use oclWrapper
          use common_sn 
          implicit none
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: h
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(kp+2) , intent(In) :: z2
        integer, intent(In) :: n
        real(kind=4), intent(In) :: ro
        real(kind=4), intent(In) :: dt
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        integer, dimension(1) :: n_ptr
        integer :: globalrange
        
        ! OpenCL-specific declarations
        integer :: velnw__bondv1_init_uvw_ocl_nunits, velnw__bondv1_init_uvw_ocl_nthreads, velnw__bondv1_init_uvw_ocl_globalrange, velnw__bondv1_init_uvw_ocl_localrange
        integer :: init_velnw__bondv1_init_uvw_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/velnw__bondv1_init_uvw_kernel.cl"
        character(len=*), parameter :: kstr="velnw__bondv1_init_uvw_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: p_buf
        integer(8) :: dxs_buf
        integer(8) :: dys_buf
        integer(8) :: dzs_buf
        integer(8) :: dzn_buf
        integer(8) :: z2_buf
        integer(8) :: uvw_buf
        integer(8) :: fgh_buf
        integer(8) :: n_ptr_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: p_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: dys_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: z2_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(1):: n_ptr_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        globalrange = (ip+1)*jp*kp
        n_ptr(1)=n
        
        if ( init_ocl_local /= 1 ) then 
          init_velnw__bondv1_init_uvw_ocl_local = 1
        if ( init_velnw__bondv1_init_uvw_ocl /= 1 ) then 
          init_velnw__bondv1_init_uvw_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(velnw__bondv1_init_uvw_ocl_nunits)
          call oclGetNThreadsHint(velnw__bondv1_init_uvw_ocl_nthreads)
        
        !  print *, "Compute units:",velnw__bondv1_init_uvw_ocl_nunits," Threads:",velnw__bondv1_init_uvw_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        p_sz = shape(p)
        dxs_sz = shape(dxs)
        dys_sz = shape(dys)
        dzs_sz = shape(dzs)
        dzn_sz = shape(dzn)
        z2_sz = shape(z2)
        uvw_sz = shape(uvw)
        fgh_sz = shape(fgh)
        n_ptr_sz = shape(n_ptr)
        
        ! Create OpenCL buffers
        call oclMake3DFloatArrayReadBuffer(p_buf,p_sz ,p)
        call oclMake1DFloatArrayReadBuffer(dxs_buf,dxs_sz ,dxs)
        call oclMake1DFloatArrayReadBuffer(dys_buf,dys_sz ,dys)
        call oclMake1DFloatArrayReadBuffer(dzs_buf,dzs_sz ,dzs)
        call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
        call oclMake1DFloatArrayReadBuffer(z2_buf,z2_sz ,z2)
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake4DFloatArrayReadBuffer(fgh_buf,fgh_sz ,fgh)
        call oclMake1DIntArrayReadBuffer(n_ptr_buf,n_ptr_sz ,n_ptr)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, p_buf )
        call oclSetFloatArrayArg(1, uvw_buf )
        call oclSetFloatArrayArg(2, fgh_buf )
        call oclSetFloatArrayArg(3, dxs_buf )
        call oclSetFloatArrayArg(4, dys_buf )
        call oclSetFloatArrayArg(5, dzs_buf )
        call oclSetFloatArrayArg(6, dzn_buf )
        call oclSetFloatArrayArg(7, z2_buf )
        call oclSetIntArrayArg(8, n_ptr_buf )
        call oclSetIntConstArg(9, im )
        call oclSetIntConstArg(10, jm )
        call oclSetIntConstArg(11, km )
        call oclSetFloatConstArg(12, dt )
        
        end if
        
        velnw__bondv1_init_uvw_ocl_globalrange = globalrange
        velnw__bondv1_init_uvw_ocl_localrange = 0
        if (velnw__bondv1_init_uvw_ocl_localrange == 0) then
          call padRange(velnw__bondv1_init_uvw_ocl_globalrange,velnw__bondv1_init_uvw_ocl_nunits*velnw__bondv1_init_uvw_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite3DFloatArrayBuffer(p_buf,p_sz,p)
        call oclWrite1DFloatArrayBuffer(dxs_buf,dxs_sz,dxs)
        call oclWrite1DFloatArrayBuffer(dys_buf,dys_sz,dys)
        call oclWrite1DFloatArrayBuffer(dzs_buf,dzs_sz,dzs)
        call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
        call oclWrite1DFloatArrayBuffer(z2_buf,z2_sz,z2)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclWrite1DIntArrayBuffer(n_ptr_buf,n_ptr_sz,n_ptr)
        
        ! call velnw__bondv1_init_uvw( p, uvw, fgh, dxs,dys,dzs,dzn,z2,n_ptr,im, jm, km, dt)
        call runOcl(velnw__bondv1_init_uvw_ocl_globalrange,velnw__bondv1_init_uvw_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        
         ! Convert back
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
      end subroutine velnw__bondv1_init_uvw_ocl
end module module_velnw__bondv1_init_uvw_ocl
