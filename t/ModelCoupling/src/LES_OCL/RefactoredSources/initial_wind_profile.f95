module module_initial_wind_profile

contains

      subroutine initial_wind_profile(wind_profile,jm,km,z2,dzn)
      use common_sn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn

        integer, intent(In) :: jm
        integer, intent(In) :: km

        real(kind=4), dimension(jp,kp) , intent(InOut) :: wind_profile
        real(kind=4), dimension(kp+2) , intent(In) :: z2
        real(kind=4) :: u_val
! 
! 
! -------------------inflow-------------------
! 
!      Setup for initial wind profile
! 

      do k = 1,78 ! kp = 90 so OK
      do j = 1,jm
        u_val = 5.*((z2(k)+0.5*dzn(k))/600.)**0.2
        wind_profile(j,k) = u_val
      end do
      end do
      do k = 79,km
      do j = 1,jm
        wind_profile(j,k) = wind_profile(j,77)
      end do
      end do
    
      end subroutine initial_wind_profile

end module module_initial_wind_profile

