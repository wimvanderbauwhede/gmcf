module module_LES_tests
    use module_LES_conversions
    contains
! ================================================================================ 
! Subroutines for testing OpenCL kernel and conversion functions
! ================================================================================ 

! Test for conversion functions

    subroutine test_uvw_conversion (u,v,w)
        use params_common_sn
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w
        real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw        
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u2
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v2
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w2
        integer :: ii,jj,kk, ok
        call convert_to_uvw(u,v,w,uvw)
        call convert_from_uvw(uvw,u2,v2,w2)
        ok = 1
        do jj = -1,jp+1
            do ii = 0,ip+1
                do kk = 0,kp+1
                    if ( u(ii,jj,kk) /= u2(ii,jj,kk) ) then
                        print *, "u(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( v(ii,jj,kk) /= v2(ii,jj,kk) ) then
                        print *, "v(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( w(ii,jj,kk) /= w2(ii,jj,kk) ) then
                        print *, "w(",ii,jj,kk,")"
                        ok = 0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if
    end subroutine

    subroutine test_fgh_conversion (f,g,h)
        use params_common_sn
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f,g,h
        real(kind=4), dimension(0:3,0:ip,0:jp,0:kp)  :: fgh
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f2,g2,h2

        integer :: ii,jj,kk, ok
        call convert_to_fgh(f,g,h,fgh)
        call convert_from_fgh(fgh,f2,g2,h2)
        ok = 1
        do jj = 0,jp
            do ii = 0,ip
                do kk = 0,kp
                    if ( f(ii,jj,kk) /= f2(ii,jj,kk) ) then
                        print *, "f(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( g(ii,jj,kk) /= g2(ii,jj,kk) ) then
                        print *, "g(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( h(ii,jj,kk) /= h2(ii,jj,kk) ) then
                        print *, "h(",ii,jj,kk,")"
                        ok = 0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if
    end subroutine
    
    subroutine test_fgh_old_conversion (f,g,h)
        use params_common_sn
        real(kind=4), dimension(ip,jp,kp)  :: f,g,h
        real(kind=4), dimension(0:3,ip,jp,kp)  :: fgh
        real(kind=4), dimension(ip,jp,kp)  :: f2,g2,h2

        integer :: ii,jj,kk, ok
        call convert_to_fgh_old(f,g,h,fgh)
        call convert_from_fgh_old(fgh,f2,g2,h2)
        ok = 1
        do jj = 1,jp
            do ii = 1,ip
                do kk = 1,kp
                    if ( f(ii,jj,kk) /= f2(ii,jj,kk) ) then
                        print *, "f(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( g(ii,jj,kk) /= g2(ii,jj,kk) ) then
                        print *, "g(",ii,jj,kk,")"
                        ok = 0
                    end if
                    if ( h(ii,jj,kk) /= h2(ii,jj,kk) ) then
                        print *, "h(",ii,jj,kk,")"
                        ok = 0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if
    end subroutine    


    subroutine test_9vec_conversion(cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9)
        use params_common_sn
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov1,cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov2, cov3, cov4, cov6, cov7, cov8, cov9
        real(kind=4), dimension(1:16,-1:ip+2,0:jp+2,0:kp+2)  :: cov ! We use 16 positions for alignment!
!        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov1c,cov5c
!        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov2c, cov3c, cov4c, cov6c, cov7c, cov8c, cov9c
        integer :: ii,jj,kk, ll
        logical, dimension(10) :: ok
        call convert_to_9vec(cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,cov)
        ok(10) = .true.
        do kk = 0,kp+2
            do jj = 0,jp+2
                do ii = 0,ip+2
                    ok(1) = cov(1,ii,jj,kk) == cov1(ii,jj,kk)
                    ok(2) = cov(2,ii,jj,kk) == cov2(ii,jj,kk)
                    ok(3) = cov(3,ii,jj,kk) == cov3(ii,jj,kk)
                    ok(4) = cov(4,ii,jj,kk) == cov4(ii,jj,kk)
                    ok(5) = cov(5,ii,jj,kk) == cov5(ii,jj,kk)
                    ok(6) = cov(6,ii,jj,kk) == cov6(ii,jj,kk)
                    ok(7) = cov(7,ii,jj,kk) == cov7(ii,jj,kk)
                    ok(8) = cov(8,ii,jj,kk) == cov8(ii,jj,kk)
                    ok(9) = cov(9,ii,jj,kk) == cov9(ii,jj,kk)
                    do ll = 1,9
                        if (.not. ok(ll)) then
                            print *, "cov",ll
                            ok(10) = .false.
                        end if
                    end do
                end do
            end do
        end do
        if ( ok(10) ) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if
    end subroutine 

    subroutine test_cn234ls_conversion(cn2l,cn2s,cn3l,cn3s,cn4l,cn4s)
        use params_common_sn
        real(kind=4), dimension(ip)  :: cn2l
        real(kind=4), dimension(ip)  :: cn2s
        real(kind=4), dimension(jp)  :: cn3l
        real(kind=4), dimension(jp)  :: cn3s
        real(kind=4), dimension(kp)  :: cn4l
        real(kind=4), dimension(kp)  :: cn4s
        real(kind=4), dimension(1:2*(ip+jp+kp))  :: cn234ls
        integer :: ii,jj,kk, ok
        call convert_to_cn234ls(cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,cn234ls)
        ok = 1
        do ii = 1,ip
            if ( cn2l(ii) /= cn234ls(ii) ) then 
                print *, "cn2l(",ii,")"
                ok=0
            end if
            if ( cn234ls(ip+ii) /= cn2s(ii)) then 
                print *, "cn2s(",ii,")"
                ok=0
            end if
        end do
        do jj = 1,jp
            if ( cn234ls(2*ip+jj) /= cn3l(jj) ) then
                print *, "cn3l(",jj,")"
                ok=0
            end if
            if ( cn234ls(2*ip+jp+jj) /= cn3s(jj) ) then
                print *, "cn3s(",jj,")"
                ok=0
            end if
        end do
        do kk = 1,kp
            if ( cn234ls(2*ip+2*jp+kk) /= cn4l(kk) ) then
                print *, "cn4l(",kk,")"
                ok=0
            end if
            if ( cn234ls(2*ip+2*jp+kp+kk) /= cn4s(kk) ) then
                print *, "cn4s(",k,")"
                ok=0
            end if
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if
    end subroutine 

    ! Got bored, no test for convert_to_mask1 -- and guess what? It segfaults!

! Test for initialise_LES_kernel    
! We call LES_kernel with state=0, read back everything, see if it is still the same as before
    subroutine test_kernel_transfer_p (p,po)    
        use params_common_sn
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) :: p,po
        integer ii,jj,kk, ok
        ok = 1
        do kk = 0,kp+1
            do jj = 0,jp+2
                do ii = 0,ip+2
                    if (p(ii,jj,kk) /= po(ii,jj,kk) ) then
                        print * , 'p( ',ii,jj,kk,')',p(ii,jj,kk) ,'/=', po(ii,jj,kk)
                        ok=0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if                
    end subroutine

    subroutine test_kernel_transfer_uvw (u,v,w,uo,vo,wo)    
        use params_common_sn
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u,uo
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v,vo
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w,wo
        
        integer ii,jj,kk, ok
        ok = 1
        do kk = -1,kp+1
            do jj = 0,jp+1
                do ii = 0,ip+1
                    if (kk/= -1) then
                    if ( u(ii,jj,kk) /= uo(ii,jj,kk) ) then
                        print * , 'u( ',ii,jj,kk,')',u(ii,jj,kk), '/=', uo(ii,jj,kk)
                        ok=0
                    end if
                    if ( v(ii,jj,kk) /= vo(ii,jj,kk) ) then
                        print * , 'v( ',ii,jj,kk,')',v(ii,jj,kk), '/=', vo(ii,jj,kk)
                        ok=0
                    end if
                    end if
                    if ( w(ii,jj,kk) /= wo(ii,jj,kk) ) then
                        print * , 'w( ',ii,jj,kk,')',w(ii,jj,kk), '/=', wo(ii,jj,kk)
                        ok=0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if                
    end subroutine

    subroutine test_kernel_transfer_fgh (f,g,h,fo,go,ho)
        use params_common_sn
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f,fo      
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: g,go
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: h,ho
 
        integer ii,jj,kk, ok
        ok = 1
        do kk = 0,kp
            do jj = 0,jp
                do ii = 0,ip
                    if ( f(ii,jj,kk) /= fo(ii,jj,kk) ) then
                        print * , 'f( ',ii,jj,kk,')'
                        ok=0
                    end if
                    if ( g(ii,jj,kk) /= go(ii,jj,kk) ) then
                        print * , 'g( ',ii,jj,kk,')'
                        ok=0
                    end if
                    if ( h(ii,jj,kk) /= ho(ii,jj,kk) ) then
                        print * , 'h( ',ii,jj,kk,')'
                        ok=0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if                
    end subroutine

    subroutine test_kernel_transfer_fghold (fold,gold,hold,foldo,goldo,holdo)
        use params_common_sn
        real(kind=4), dimension(ip,jp,kp)  :: fold,foldo      
        real(kind=4), dimension(ip,jp,kp)  :: gold,goldo
        real(kind=4), dimension(ip,jp,kp)  :: hold,holdo
 
        integer ii,jj,kk, ok
        ok = 1
        do kk = 1,kp
            do jj = 1,jp
                do ii = 1,ip
                    if ( fold(ii,jj,kk) /= foldo(ii,jj,kk) ) then
                        print * , 'fold( ',ii,jj,kk,')', fold(ii,jj,kk) ,'/=', foldo(ii,jj,kk)
                        ok=0
                    end if
                    if ( gold(ii,jj,kk) /= goldo(ii,jj,kk) ) then
                        print * , 'gold( ',ii,jj,kk,')',gold(ii,jj,kk), '/=', goldo(ii,jj,kk)
                        ok=0
                    end if
                    if ( hold(ii,jj,kk) /= holdo(ii,jj,kk) ) then
                        print * , 'hold( ',ii,jj,kk,')',hold(ii,jj,kk) ,'/=', holdo(ii,jj,kk)
                        ok=0
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!"
        end if                
    end subroutine
    
    subroutine compare_3D_real_arrays (a,ao,lb,ub) 
        use params_common_sn
        integer, dimension(3) :: ub
        integer, dimension(3) :: lb
        real(kind=4), dimension(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) :: a,ao
        integer ii,jj,kk,ok,nok_ct
        real(kind=4) :: rel_error
#ifdef MAX_REL_ERROR
        real, parameter :: max_rel_error = MAX_REL_ERROR
#else        
        real, parameter :: max_rel_error = 0.000005
#endif        
        ok = 1
        nok_ct=0
                do ii = 1,ip
            do jj = 1,jp
        do kk = 1,kp
                    if ( a(ii,jj,kk) /= ao(ii,jj,kk) ) then
                        ! we allow a 5*10^-6 error, ad hoc, seems to be a rounding difference, worse on GPU
                        rel_error = abs( (a(ii,jj,kk) - ao(ii,jj,kk) ) / a(ii,jj,kk) )
                        if ( rel_error > max_rel_error) then
                            print * , '(',ii,jj,kk,')',a(ii,jj,kk) ,'<>',ao(ii,jj,kk) ,' rel. error = ',rel_error
                            ok=0
                            nok_ct=nok_ct+1
                        end if
                    end if
                end do
            end do
        end do
        if (ok == 1) then
            print *, "OK!"
        else
            print *, "NOK!",nok_ct,'/',ip*jp*kp
        end if                                    
    end subroutine

    subroutine uvw_sum_dom (u,v,w)
        use params_common_sn
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w
        integer :: ii,jj,kk
        real(kind=4) :: usum,vsum,wsum
        usum=0.0
        vsum=0.0
        wsum=0.0
        do jj = 1,jp
            do ii = 1,ip
                do kk = 1,kp
        usum=usum+u(ii,jj,kk)
        vsum=vsum+v(ii,jj,kk)
        wsum=wsum+w(ii,jj,kk)
                end do
            end do
        end do
        print *, 'UVWSUM DOM:',usum,vsum,wsum
    end subroutine

    subroutine fgh_sum_dom (f,g,h)
        use params_common_sn
        real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f,g,h
        integer :: ii,jj,kk
        real(kind=4) :: fsum,gsum,hsum
        fsum=0.0
        gsum=0.0
        hsum=0.0
        do jj = 1,jp
            do ii = 1,ip
                do kk = 1,kp
        fsum=fsum+f(ii,jj,kk)
        gsum=gsum+g(ii,jj,kk)
        hsum=hsum+h(ii,jj,kk)
                end do
            end do
        end do
        print *, 'FGHSUM DOM:',fsum,gsum,hsum
    end subroutine

end module module_LES_tests

