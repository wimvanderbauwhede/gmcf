subroutine main_routine1(sys, tile, model_id) ! This replaces 'program main'

    ! Lines marked with ! gmcf-coupler / ! end gmcf-coupler are additions for coupling

    ! gmcf-coupler
    use gmcfAPI

    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    ! end gmcf-coupler

    ! gmcf-coupler
    integer :: sync_done, pre_post, destination, request_id
    integer, parameter :: GPRM_VAR_NAME_1=1,GPRM_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet
    ! end gmcf-coupler

    integer :: t,t_start,t_stop,t_step
    real(kind=4), dimension(32) :: var_name_1, var_name_2
    t_start=0
    t_stop = 10
    t_step = 1

    ! gmcf-coupler
    ! Init amongst other things gathers info about the time loops, maybe from a config file, need to work this out in detail:
    call gmcfInitCoupler(sys,tile, model_id)
    ! end gmcf-coupler

    print *, "FORTRAN MODEL1", model_id,"main routine called with pointers",sys,tile

    do t = t_start,t_stop,t_step
        print *, "FORTRAN MODEL1", model_id," syncing for time step ",t,"..."
        ! gmcf-coupler
        ! Sync all models by requesting all timesteps, block until all received, and sending your timestep to all who ask, until everyone has asked?
        ! Is this possible? I think it is OK:
        ! Start by sending N-1 requests; read from the FIFO,
        ! block if there is nothing there.
        ! You'll get requests and/or data. For every request, send data; keep going until you've sent data to all and received data from all.
        ! I don't think this will deadlock.

        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model_id,t,sync_done)
            print *, "FORTRAN MODEL1 AFTER gmcfSync()"
!            sync_done=1
            ! if sync is not done, means we had to break out to send data for some request
            !    if (sync_done == 0) then
            !      select case (gmcfRequests(model_id)%data_id) ! <code for the variable var_name, GPRM-style>
            !        case (GPRM_VAR_NAME_1)
            !            call gmcfSend1DFloatArray(var_name_1, size(var_name_1), gmcfRequests(model)%destination,t)
            !      end select
            !    end if
            print *, "FORTRAN MODEL1", model_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL1", model_id," syncing DONE for time step ",t

    end do

    print *, "FORTRAN MODEL1", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine

