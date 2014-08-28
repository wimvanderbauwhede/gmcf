subroutine main_routine2(sys, tile, model_id) ! This replaces 'program main'
! Lines marked with ! gprm-coupler / ! end gprm-coupler are additions for coupling

! gprm-coupler
!  use gprmCoupler

  integer(8) , intent(In) :: sys
  integer(8) , intent(In) :: tile
  integer , intent(In) :: model_id
! end gprm-coupler

    print *, "FORTRAN MODEL2", model_id,"main routine called with pointers",sys,tile

end subroutine

