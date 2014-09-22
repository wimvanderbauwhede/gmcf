! This is the module corresponding to les.f95 in the refactored Fortran code
! The naming is unfortunate, this is just one of the subroutines called by the simulator
! The overall code is module_LES_combined_ocl_TEMPL.f95
module module_les_ocl
    use module_LES_conversions
    use module_les
      use module_boundsm ! add_module_decls() line 156
        integer :: init_merged_velfg_feedbf_les_calc_sm_ocl = 0
        integer :: init_bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl = 0
        integer :: init_les_calc_visc_ocl = 0
        integer :: init_les_calc_visc__adam_ocl = 0
        integer :: init_les_calc_sm_ocl = 0
        integer :: init_les_bound_sm_ocl = 0
    contains
    subroutine bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl(f,g,h,u,v,w,usum,vsum,wsum,sm,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,dx1,dy1,dzn,dzs,dxs,uout, bmask1,cmask1,dmask1,dt, im, jm, km)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nunits, bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nthreads, bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_globalrange, bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_localrange
        integer :: init_bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel.cl"
        character(len=*), parameter :: kstr="bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_bcdmask1(bmask1,cmask1,dmask1,mask1)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
        uout_ptr(1)=uout
        
        if ( init_ocl_local /= 1 ) then 
          init_bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_local = 1
        if ( init_bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl /= 1 ) then 
          init_bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl = 1
        !  call oclInitOpts(srcstr,kstr,koptsstr)
          call oclInit(srcstr,kstr)
        
          call oclGetMaxComputeUnits(bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nunits)
          call oclGetNThreadsHint(bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nthreads)
        
        !  print *, "Compute units:",bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nunits," Threads:",bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nthreads
        end if
        
        ! OpenCL buffer sizes
        sm_sz = shape(sm)
        dx1_sz = shape(dx1)
        dy1_sz = shape(dy1)
        dzn_sz = shape(dzn)
        dzs_sz = shape(dzs)
        dxs_sz = shape(dxs)
        uout_ptr_sz = shape(uout_ptr)
        fgh_sz = shape(fgh)
        diu_sz = shape(diu)
        uvw_sz = shape(uvw)
        uvwsum_sz = shape(uvwsum)
        mask1_sz = shape(mask1)
        
        ! Create OpenCL buffers
        call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
        call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
        call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
        call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
        call oclMake1DFloatArrayReadBuffer(dzs_buf,dzs_sz ,dzs)
        call oclMake1DFloatArrayReadBuffer(dxs_buf,dxs_sz ,dxs)
        call oclMake1DFloatArrayReadWriteBuffer(uout_ptr_buf,uout_ptr_sz ,uout_ptr)
        call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
        call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
        call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
        call oclMake4DFloatArrayReadWriteBuffer(uvwsum_buf,uvwsum_sz ,uvwsum)
        call oclMake4DFloatArrayReadWriteBuffer(mask1_buf,mask1_sz ,mask1)
        
        ! Set OpenCL argument order
        call oclSetFloatArrayArg(0, uvw_buf )
        call oclSetFloatArrayArg(1, uvwsum_buf )
        call oclSetFloatArrayArg(2, fgh_buf )
        call oclSetFloatArrayArg(3, mask1_buf )
        call oclSetFloatArrayArg(4, diu_buf )
        call oclSetFloatArrayArg(5, dxs_buf )
        call oclSetFloatArrayArg(6, dzs_buf )
        call oclSetFloatArrayArg(7, dx1_buf )
        call oclSetFloatArrayArg(8, dy1_buf )
        call oclSetFloatArrayArg(9, dzn_buf )
        call oclSetFloatArrayArg(10, sm_buf )
        call oclSetFloatArrayArg(11, uout_ptr_buf )
        call oclSetFloatConstArg(12, dt )
        call oclSetIntConstArg(13, im )
        call oclSetIntConstArg(14, jm )
        call oclSetIntConstArg(15, km )
        
        end if
        
        bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_globalrange = im*jm*km
        bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_localrange = 0
        if (bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_localrange == 0) then
          call padRange(bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_globalrange,bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nunits*bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_nthreads)
        end if
        
        ! Copy all arrays required for the full run
        call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
        call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
        call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
        call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
        call oclWrite1DFloatArrayBuffer(dzs_buf,dzs_sz,dzs)
        call oclWrite1DFloatArrayBuffer(dxs_buf,dxs_sz,dxs)
        call oclWrite1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
        call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclWrite4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
        call oclWrite4DFloatArrayBuffer(mask1_buf,mask1_sz,mask1)
        
        ! call bondv1_calc_uvw__velfg__feedbf__les_calc_sm(uvw,uvwsum,fgh,mask1,diu,dxs,dzs,dx1,dy1,dzn,sm,uout_ptr,dt,im,jm,km)
        call runOcl(bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_globalrange,bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl_localrange)
        
        ! Read back Read and ReadWrite arrays
        call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
        call oclRead1DFloatArrayBuffer(uout_ptr_buf,uout_ptr_sz,uout_ptr)
        call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
        call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
        call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
        call oclRead4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
        call oclRead4DFloatArrayBuffer(mask1_buf,mask1_sz,mask1)
        
         ! Convert back for velFG
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
        call convert_from_9vec(diu,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9)
         ! Convert back for feedbf
        call convert_from_fgh(uvwsum,usum,vsum,wsum) ! temporary: can pass around as-is!
    end subroutine bondv1_calc_uvw__velfg__feedbf__les_calc_sm_ocl
    subroutine merged_velfg_feedbf_les_calc_sm_ocl(f,g,h,u,v,w,usum,vsum,wsum,sm,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,dx1,dy1,dzn,dzs, bmask1,cmask1,dmask1,dt, im, jm, km)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: merged_velfg_feedbf_les_calc_sm_ocl_nunits, merged_velfg_feedbf_les_calc_sm_ocl_nthreads, merged_velfg_feedbf_les_calc_sm_ocl_globalrange, merged_velfg_feedbf_les_calc_sm_ocl_localrange
        integer :: init_merged_velfg_feedbf_les_calc_sm_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/merged_velfg_feedbf_les_calc_sm_kernel.cl"
        character(len=*), parameter :: kstr="merged_velfg_feedbf_les_calc_sm_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_uvw(u,v,w,uvw)
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh(usum,vsum,wsum,uvwsum)
        call convert_to_bcdmask1(bmask1,cmask1,dmask1,mask1)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
    
    if ( init_ocl_local /= 1 ) then 
      init_merged_velfg_feedbf_les_calc_sm_ocl_local = 1
    if ( init_merged_velfg_feedbf_les_calc_sm_ocl /= 1 ) then 
      init_merged_velfg_feedbf_les_calc_sm_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(merged_velfg_feedbf_les_calc_sm_ocl_nunits)
      call oclGetNThreadsHint(merged_velfg_feedbf_les_calc_sm_ocl_nthreads)
    
    !  print *, "Compute units:",merged_velfg_feedbf_les_calc_sm_ocl_nunits," Threads:",merged_velfg_feedbf_les_calc_sm_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    sm_sz = shape(sm)
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    dzs_sz = shape(dzs)
    fgh_sz = shape(fgh)
    diu_sz = shape(diu)
    uvw_sz = shape(uvw)
    uvwsum_sz = shape(uvwsum)
    mask1_sz = shape(mask1)
    
    ! Create OpenCL buffers
    call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake1DFloatArrayReadBuffer(dzs_buf,dzs_sz ,dzs)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
    call oclMake4DFloatArrayReadWriteBuffer(uvw_buf,uvw_sz ,uvw)
    call oclMake4DFloatArrayReadWriteBuffer(uvwsum_buf,uvwsum_sz ,uvwsum)
    call oclMake4DFloatArrayReadWriteBuffer(mask1_buf,mask1_sz ,mask1)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, uvw_buf )
    call oclSetFloatArrayArg(1, uvwsum_buf )
    call oclSetFloatArrayArg(2, fgh_buf )
    call oclSetFloatArrayArg(3, mask1_buf )
    call oclSetFloatArrayArg(4, diu_buf )
    call oclSetFloatArrayArg(5, dzs_buf )
    call oclSetFloatArrayArg(6, dx1_buf )
    call oclSetFloatArrayArg(7, dy1_buf )
    call oclSetFloatArrayArg(8, dzn_buf )
    call oclSetFloatArrayArg(9, sm_buf )
    call oclSetFloatConstArg(10, dt )
    call oclSetIntConstArg(11, im )
    call oclSetIntConstArg(12, jm )
    call oclSetIntConstArg(13, km )
    
    end if
    
    merged_velfg_feedbf_les_calc_sm_ocl_globalrange = im*jm*km
    merged_velfg_feedbf_les_calc_sm_ocl_localrange = 0
    if (merged_velfg_feedbf_les_calc_sm_ocl_localrange == 0) then
      call padRange(merged_velfg_feedbf_les_calc_sm_ocl_globalrange,merged_velfg_feedbf_les_calc_sm_ocl_nunits*merged_velfg_feedbf_les_calc_sm_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite1DFloatArrayBuffer(dzs_buf,dzs_sz,dzs)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    call oclWrite4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
    call oclWrite4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
    call oclWrite4DFloatArrayBuffer(mask1_buf,mask1_sz,mask1)
    
    ! call merged_velfg_feedbf_les_calc_sm(uvw,uvwsum,fgh,mask1,diu,dzs,dx1,dy1,dzn,sm,dt,im,jm,km)
    call runOcl(merged_velfg_feedbf_les_calc_sm_ocl_globalrange,merged_velfg_feedbf_les_calc_sm_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    call oclRead4DFloatArrayBuffer(uvw_buf,uvw_sz,uvw)
    call oclRead4DFloatArrayBuffer(uvwsum_buf,uvwsum_sz,uvwsum)
    call oclRead4DFloatArrayBuffer(mask1_buf,mask1_sz,mask1)
    
         ! Convert back for velFG
        call convert_from_fgh(fgh,f,g,h)
        call convert_from_uvw(uvw,u,v,w)
        call convert_from_9vec(diu,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9)
         ! Convert back for feedbf
        call convert_from_fgh(uvwsum,usum,vsum,wsum) ! temporary: can pass around as-is!
    end subroutine merged_velfg_feedbf_les_calc_sm_ocl
    subroutine les_calc_sm_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, h)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: les_calc_sm_ocl_nunits, les_calc_sm_ocl_nthreads, les_calc_sm_ocl_globalrange, les_calc_sm_ocl_localrange
        integer :: init_les_calc_sm_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/les_calc_sm_kernel.cl"
        character(len=*), parameter :: kstr="les_calc_sm_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
! We will split this into three kernels, but the second kernel could also be a host-side subroutine, I think that might be better.
! So, the generator really needs to be able to deal with multiple kernels in a single file. Fun!
! diu in, sm in/out (sm needs to be read in once, in init), fgh in
    
    if ( init_ocl_local /= 1 ) then 
      init_les_calc_sm_ocl_local = 1
    if ( init_les_calc_sm_ocl /= 1 ) then 
      init_les_calc_sm_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(les_calc_sm_ocl_nunits)
      call oclGetNThreadsHint(les_calc_sm_ocl_nthreads)
    
    !  print *, "Compute units:",les_calc_sm_ocl_nunits," Threads:",les_calc_sm_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    sm_sz = shape(sm)
    fgh_sz = shape(fgh)
    diu_sz = shape(diu)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, fgh_buf )
    call oclSetFloatArrayArg(1, dx1_buf )
    call oclSetFloatArrayArg(2, dy1_buf )
    call oclSetFloatArrayArg(3, dzn_buf )
    call oclSetFloatArrayArg(4, diu_buf )
    call oclSetFloatArrayArg(5, sm_buf )
    call oclSetIntConstArg(6, im )
    call oclSetIntConstArg(7, jm )
    call oclSetIntConstArg(8, km )
    
    end if
    
    les_calc_sm_ocl_globalrange = im*jm*km
    les_calc_sm_ocl_localrange = 0
    if (les_calc_sm_ocl_localrange == 0) then
      call padRange(les_calc_sm_ocl_globalrange,les_calc_sm_ocl_nunits*les_calc_sm_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    ! call les_calc_sm(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    call runOcl(les_calc_sm_ocl_globalrange,les_calc_sm_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
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
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: les_bound_sm_ocl_nunits, les_bound_sm_ocl_nthreads, les_bound_sm_ocl_globalrange, les_bound_sm_ocl_localrange
        integer :: init_les_bound_sm_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/les_bound_sm_kernel.cl"
        character(len=*), parameter :: kstr="les_bound_sm_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
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
    
    if ( init_ocl_local /= 1 ) then 
      init_les_bound_sm_ocl_local = 1
    if ( init_les_bound_sm_ocl /= 1 ) then 
      init_les_bound_sm_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(les_bound_sm_ocl_nunits)
      call oclGetNThreadsHint(les_bound_sm_ocl_nthreads)
    
    !  print *, "Compute units:",les_bound_sm_ocl_nunits," Threads:",les_bound_sm_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    sm_sz = shape(sm)
    fgh_sz = shape(fgh)
    diu_sz = shape(diu)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, fgh_buf )
    call oclSetFloatArrayArg(1, dx1_buf )
    call oclSetFloatArrayArg(2, dy1_buf )
    call oclSetFloatArrayArg(3, dzn_buf )
    call oclSetFloatArrayArg(4, diu_buf )
    call oclSetFloatArrayArg(5, sm_buf )
    call oclSetIntConstArg(6, im )
    call oclSetIntConstArg(7, jm )
    call oclSetIntConstArg(8, km )
    
    end if
    
    les_bound_sm_ocl_globalrange = globalrange
    les_bound_sm_ocl_localrange = localrange
    if (les_bound_sm_ocl_localrange == 0) then
      call padRange(les_bound_sm_ocl_globalrange,les_bound_sm_ocl_nunits*les_bound_sm_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    ! call les_bound_sm(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    call runOcl(les_bound_sm_ocl_globalrange,les_bound_sm_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
      end subroutine les_bound_sm_ocl
    subroutine les_calc_visc_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g, h)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: les_calc_visc_ocl_nunits, les_calc_visc_ocl_nthreads, les_calc_visc_ocl_globalrange, les_calc_visc_ocl_localrange
        integer :: init_les_calc_visc_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/les_calc_visc_kernel.cl"
        character(len=*), parameter :: kstr="les_calc_visc_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
! diu, sm and fgh in, fgh out, so here I need to read back fgh for sure, maybe also sm, for avesm
    
    if ( init_ocl_local /= 1 ) then 
      init_les_calc_visc_ocl_local = 1
    if ( init_les_calc_visc_ocl /= 1 ) then 
      init_les_calc_visc_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(les_calc_visc_ocl_nunits)
      call oclGetNThreadsHint(les_calc_visc_ocl_nthreads)
    
    !  print *, "Compute units:",les_calc_visc_ocl_nunits," Threads:",les_calc_visc_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    sm_sz = shape(sm)
    fgh_sz = shape(fgh)
    diu_sz = shape(diu)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, fgh_buf )
    call oclSetFloatArrayArg(1, dx1_buf )
    call oclSetFloatArrayArg(2, dy1_buf )
    call oclSetFloatArrayArg(3, dzn_buf )
    call oclSetFloatArrayArg(4, diu_buf )
    call oclSetFloatArrayArg(5, sm_buf )
    call oclSetIntConstArg(6, im )
    call oclSetIntConstArg(7, jm )
    call oclSetIntConstArg(8, km )
    
    end if
    
    les_calc_visc_ocl_globalrange = im*jm*km
    les_calc_visc_ocl_localrange = 0
    if (les_calc_visc_ocl_localrange == 0) then
      call padRange(les_calc_visc_ocl_globalrange,les_calc_visc_ocl_nunits*les_calc_visc_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    ! call les_calc_visc(fgh,dx1,dy1,dzn,diu,sm,im,jm,km)
    call runOcl(les_calc_visc_ocl_globalrange,les_calc_visc_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    call convert_from_fgh(fgh,f,g,h)
    end subroutine les_calc_visc_ocl
    subroutine les_calc_visc__adam_ocl(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,fold,gold,hold,fghold,f,g, h)
        use oclWrapper
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
        
        ! OpenCL-specific declarations
        integer :: les_calc_visc__adam_ocl_nunits, les_calc_visc__adam_ocl_nthreads, les_calc_visc__adam_ocl_globalrange, les_calc_visc__adam_ocl_localrange
        integer :: init_les_calc_visc__adam_ocl_local
        ! integer :: srclen, klen
        character(len=*), parameter :: srcstr="../OpenCL/Kernels/les_calc_visc__adam_kernel.cl"
        character(len=*), parameter :: kstr="les_calc_visc__adam_kernel"
        ! character(len=*), parameter :: koptsstr=""
        !This is a hook to insert the actual buffer declarations!
        
        ! OpenCL buffer declarations
        integer(8) :: sm_buf
        integer(8) :: dx1_buf
        integer(8) :: dy1_buf
        integer(8) :: dzn_buf
        integer(8) :: dzs_buf
        integer(8) :: dxs_buf
        integer(8) :: uout_ptr_buf
        integer(8) :: fgh_buf
        integer(8) :: diu_buf
        integer(8) :: uvw_buf
        integer(8) :: uvwsum_buf
        integer(8) :: mask1_buf
        ! OpenCL buffer size declarations
        integer, dimension(3):: sm_sz
        integer, dimension(1):: dx1_sz
        integer, dimension(1):: dy1_sz
        integer, dimension(1):: dzn_sz
        integer, dimension(1):: dzs_sz
        integer, dimension(1):: dxs_sz
        integer, dimension(1):: uout_ptr_sz
        integer, dimension(4):: fgh_sz
        integer, dimension(4):: diu_sz
        integer, dimension(4):: uvw_sz
        integer, dimension(4):: uvwsum_sz
        integer, dimension(4):: mask1_sz
    integer(8) :: fgh_old_buf
    integer, dimension(4):: fgh_old_sz
    integer :: init_ocl_local
    init_ocl_local = 0
        
 ! Convert to new format
        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh_old(fold,gold,hold, fgh_old)
        call convert_to_9vec(diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,diu)
! diu, sm and fgh in, fgh out, so here I need to read back fgh for sure, maybe also sm, for avesm
    
    if ( init_ocl_local /= 1 ) then 
      init_les_calc_visc__adam_ocl_local = 1
    if ( init_les_calc_visc__adam_ocl /= 1 ) then 
      init_les_calc_visc__adam_ocl = 1
    !  call oclInitOpts(srcstr,kstr,koptsstr)
      call oclInit(srcstr,kstr)
    
      call oclGetMaxComputeUnits(les_calc_visc__adam_ocl_nunits)
      call oclGetNThreadsHint(les_calc_visc__adam_ocl_nthreads)
    
    !  print *, "Compute units:",les_calc_visc__adam_ocl_nunits," Threads:",les_calc_visc__adam_ocl_nthreads
    end if
    
    ! OpenCL buffer sizes
    dx1_sz = shape(dx1)
    dy1_sz = shape(dy1)
    dzn_sz = shape(dzn)
    sm_sz = shape(sm)
    fgh_sz = shape(fgh)
    fgh_old_sz = shape(fgh_old)
    diu_sz = shape(diu)
    
    ! Create OpenCL buffers
    call oclMake1DFloatArrayReadBuffer(dx1_buf,dx1_sz ,dx1)
    call oclMake1DFloatArrayReadBuffer(dy1_buf,dy1_sz ,dy1)
    call oclMake1DFloatArrayReadBuffer(dzn_buf,dzn_sz ,dzn)
    call oclMake3DFloatArrayReadWriteBuffer(sm_buf,sm_sz ,sm)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_buf,fgh_sz ,fgh)
    call oclMake4DFloatArrayReadWriteBuffer(fgh_old_buf,fgh_old_sz ,fgh_old)
    call oclMake4DFloatArrayReadWriteBuffer(diu_buf,diu_sz ,diu)
    
    ! Set OpenCL argument order
    call oclSetFloatArrayArg(0, fgh_buf )
    call oclSetFloatArrayArg(1, fgh_old_buf )
    call oclSetFloatArrayArg(2, dx1_buf )
    call oclSetFloatArrayArg(3, dy1_buf )
    call oclSetFloatArrayArg(4, dzn_buf )
    call oclSetFloatArrayArg(5, diu_buf )
    call oclSetFloatArrayArg(6, sm_buf )
    call oclSetIntConstArg(7, im )
    call oclSetIntConstArg(8, jm )
    call oclSetIntConstArg(9, km )
    
    end if
    
    les_calc_visc__adam_ocl_globalrange = im*jm*km
    les_calc_visc__adam_ocl_localrange = 0
    if (les_calc_visc__adam_ocl_localrange == 0) then
      call padRange(les_calc_visc__adam_ocl_globalrange,les_calc_visc__adam_ocl_nunits*les_calc_visc__adam_ocl_nthreads)
    end if
    
    ! Copy all arrays required for the full run
    call oclWrite1DFloatArrayBuffer(dx1_buf,dx1_sz,dx1)
    call oclWrite1DFloatArrayBuffer(dy1_buf,dy1_sz,dy1)
    call oclWrite1DFloatArrayBuffer(dzn_buf,dzn_sz,dzn)
    call oclWrite3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclWrite4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclWrite4DFloatArrayBuffer(fgh_old_buf,fgh_old_sz,fgh_old)
    call oclWrite4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    ! call les_calc_visc__adam(fgh,fgh_old,dx1,dy1,dzn,diu,sm,im,jm,km)
    call runOcl(les_calc_visc__adam_ocl_globalrange,les_calc_visc__adam_ocl_localrange)
    
    ! Read back Read and ReadWrite arrays
    call oclRead3DFloatArrayBuffer(sm_buf,sm_sz,sm)
    call oclRead4DFloatArrayBuffer(fgh_buf,fgh_sz,fgh)
    call oclRead4DFloatArrayBuffer(fgh_old_buf,fgh_old_sz,fgh_old)
    call oclRead4DFloatArrayBuffer(diu_buf,diu_sz,diu)
    
    call convert_from_fgh(fgh,f,g,h)
    call convert_from_fgh_old(fgh_old,fold,gold,hold)
    end subroutine les_calc_visc__adam_ocl
end module module_les_ocl
