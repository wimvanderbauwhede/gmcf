! This is the consumer of a producer/consumer coupled model example.
subroutine main_routine1(sys, tile, model_id) ! This replaces 'program main'

    ! Lines marked with ! gmcf-coupler / ! end gmcf-coupler are additions for coupling

    ! gmcf-coupler
    use gmcfAPI

    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    ! end gmcf-coupler

    ! gmcf-coupler
    integer :: sync_done, pre_post, destination, request_id, has_packets
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet
    ! end gmcf-coupler

    integer :: t,t_start,t_stop,t_step, t_sync, t_sync_step
    real(kind=4), dimension(32) :: var_name_1, var_name_2
    t_start = 0
    t_stop = 200
    t_step = 1
    t_sync = t_start
    t_sync_step = 20

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

        t_sync = t / t_sync_step

        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model_id,t_sync,sync_done)
            print *, "FORTRAN MODEL1 AFTER gmcfSync()"
            ! if sync is not done, means we had to break out to send data for some request
            if (sync_done == 0) then
                select case (gmcfDataRequests(model_id)%data_id) ! <code for the variable var_name, GPRM-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfSend1DFloatArray(var_name_1, size(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(model_id)%destination,t_sync)
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(var_name_2, size(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(model_id)%destination,t_sync)
                end select
            end if
            print *, "FORTRAN MODEL1", model_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL1", model_id," syncing DONE for time step ",t
        ! So now we can do some work. Let's suppose model1 is the LES, and it requests data from model2, WRF.
        ! The data requested consists of 1-D and a 3-D array of floats
        ! could also be called gmcfSendDataRequest
        call gmcfRequestData(model_id,GMCF_VAR_NAME_1, size(var_name_1), DEST_2, PRE, t_sync) ! check if PRE/POST makes some sense here
        call gmcfRequestData(model_id,GMCF_VAR_NAME_2, size(var_name_2), DEST_2, POST, t_sync)
        call gmcfWaitFor(RESPDATA, 2)
        ! and then we read them
        call hasPackets(RESPDATA,has_packets)
        do while (has_packets==1)
            call gprmShiftPending(RESPDATA,packet)
            ! read a packet
            select case (packet%data_id) ! <code for the variable var_name, GPRM-style>
                case (GPRM_VAR_NAME_1)
                    call gprmRead1DFloatArray(var_name_1,size(var_name_1), packet)
                case (GPRM_VAR_NAME_2)
                    call gprmRead3DFloatArray(var_name_2,size(var_name_2), packet)
            end select
            call hasPackets(RESPDATA,has_packets)
        end do
        ! And now we can do work on this data


    end do

    print *, "FORTRAN MODEL1", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine

