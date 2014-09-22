module module_adam_ocl
    use module_LES_conversions
    use module_adam
contains
      subroutine adam_ocl(n,nmax,data21,fold,im,jm,km,gold,hold,fghold,f,g,h)
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
 ! Convert to new format

        call convert_to_fgh(f,g,h,fgh)
        call convert_to_fgh_old(fold,gold,hold, fgh_old)

    !$ACC Kernel(adam_kernel), GlobalRange(im*jm*km), LocalRange(0)
    call adam(fgh,fgh_old,im,jm,km)
    !$ACC End Kernel

    call convert_from_fgh(fgh,f,g,h)

    end subroutine adam_ocl

end module module_adam_ocl
