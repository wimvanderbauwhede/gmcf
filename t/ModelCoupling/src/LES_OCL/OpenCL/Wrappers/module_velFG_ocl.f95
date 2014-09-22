module module_velFG_ocl
    use module_LES_conversions
    use module_velFG
        integer :: init_velFG_ocl = 0
    contains
    subroutine velFG_ocl(km,jm,im,dx1,cov1,cov2,cov3,dfu1,diu1,diu2,dy1,diu3,dzn,vn,f,cov4,cov5,cov6,dfv1,       diu4,diu5,diu6,g,cov7,cov8,cov9,dfw1,diu7,diu8,diu9,dzs,h,nou1,u,nou5,v,nou9,w,nou2,nou3,       nou4,nou6,nou7,nou8)
        use oclWrapper
        use common_sn 
        implicit none
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
        real(kind=4), dimension(0:ip,jp,kp) , intent(Out) :: dfu1
        real(kind=4), dimension(ip,0:jp,kp) , intent(Out) :: dfv1
        real(kind=4), dimension(ip,jp,kp) , intent(Out) :: dfw1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
        real(kind=4), intent(In) :: vn
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
! -----------------------------------------------------------------------
! Combined arrays for OpenCL kernel
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2)  :: diu
        integer :: localrange, globalrange, ngroups
        
        ! OpenCL-specific declarations
        integer :: velFG_ocl_nunits, velFG_ocl_nthreads, velFG_ocl_globalrange, velFG_ocl_localrange
        integer :: init_velFG_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/velFG_kernel.cl"
        character(len=*), parameter :: kstr="velFG_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: uvw_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        ! OpenCL buffer size declarations
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
        ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
        globalrange=im*jm*km
        localrange = 0
        
        if ( init_ocl_local /= 1 ) then 
          init_velFG_ocl_local = 1
        if ( init_velFG_ocl /= 1 ) then 
          init_velFG_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(velFG_ocl_nunits)
          call oclGetNThreadsHint(velFG_ocl_nthreads)
        
        !  print *, "Compute units:",velFG_ocl_nunits," Threads:",velFG_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        dx1_sz = shape(dx1)
        dy1_sz = shape(dy1)
        dzn_sz = shape(dzn)
        dzs_sz = shape(dzs)
        uvw_sz = shape(uvw)
        fgh_sz = shape(fgh)
        diu_sz = shape(diu)
        
        ! Create OpenCL buffers
        call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
        call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
        call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
        call oclMake1DFloatArrayReadBuffer(dzs_buf,dzs_sz ,dzs)
        call oclMake4DFloatArrayReadBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
        call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, fgh_buf )
        call oclSetFloatArrayArg(2, diu_buf )
        call oclSetFloatArrayArg(3, dzs_buf )
        call oclSetFloatArrayArg(4, dx1_buf )
        call oclSetFloatArrayArg(5, dy1_buf )
        call oclSetFloatArrayArg(6, dzn_buf )
        call oclSetIntConstArg(7, im )
        call oclSetIntConstArg(8, jm )
        call oclSetIntConstArg(9, km )
        
        end if
        
        velFG_ocl_globalrange = globalrange
        velFG_ocl_localrange = 0
        if (velFG_ocl_localrange == 0) then
          call padRange(velFG_ocl_globalrange,velFG_ocl_nunits*velFG_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
        call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
        call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
        call oclWrite1DFloatArrayBuffer(dzs_buf,dzs_sz,dzs)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
        
        ! call velFG( uvw, fgh, diu, dzs, dx1, dy1,  dzn, im, jm, km )
        call runOcl(velFG_ocl_globalrange,velFG_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
        
         ! Convert back
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
        call convert_from_9vec(diu,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9)
    end subroutine velFG_ocl
end module module_velFG_ocl
