subroutine main_routine2(sys, tile, model_id) ! This replaces 'program main'
! Lines marked with ! gprm-coupler / ! end gprm-coupler are additions for coupling

! gprm-coupler
    use gmcfAPI

    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    ! end gmcf-coupler

    ! gmcf-coupler
    integer :: sync_done, pre_post, destination, request_id
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
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

    print *, "FORTRAN MODEL2", model_id,"main routine called with pointers",sys,tile

    do t = t_start,t_stop,t_step
        print *, "FORTRAN MODEL2", model_id," syncing for time step ",t,"..."
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
            print *, "FORTRAN MODEL2 AFTER gmcfSync()"
             if sync is not done, means we had to break out to send data for some request
                if (sync_done == 0) then
                  select case (gmcfRequests(model_id)%data_id) ! <code for the variable var_name, GPRM-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfSend1DFloatArray(var_name_1, size(var_name_1), gmcfRequests(model)%destination,t)
                  end select
                end if
            print *, "FORTRAN MODEL2", model_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL2", model_id," syncing DONE for time step ",t

        ! This is the producer, at this point it can start computing
        ! Question is if any requests to PRE/POST data should be handled here or if they can wait for the sync?
        ! If the request is PRE, it means the current data, if it's POST, it means the new data
        ! If I handle all requests during sync, it might be fine. Alternatively, I could block on PRE requests before
        ! starting a computation. Can this deadlock?
        ! And I can block on POST requests after the computation but before sync.

    end do

    print *, "FORTRAN MODEL2", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine

