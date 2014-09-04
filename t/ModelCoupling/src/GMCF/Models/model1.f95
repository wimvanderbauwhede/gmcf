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
    integer :: sync_done, has_packets, fifo_empty
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet
    ! end gmcf-coupler

    integer :: t,t_start,t_stop,t_step, t_sync, t_sync_step
    real(kind=4), dimension(128) :: var_name_1
    real(kind=4), dimension(128,128,128) :: var_name_2
    t_start = 0
    t_stop = 10 ! 200
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

        t_sync = t ! / t_sync_step

        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model_id,t_sync,sync_done)
            print *, "FORTRAN MODEL1 AFTER gmcfSync()"
#ifndef WV_OK
            ! if sync is not done, means we had to break out to send data for some request
            if (sync_done == 0) then
            print *, "FORTRAN MODEL1 SYNC NOT DONE!"
                select case (gmcfDataRequests(model_id)%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfSend1DFloatArray(model_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(model_id)%source,PRE,t_sync)
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(model_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(model_id)%source,PRE,t_sync)
                end select
            end if
#endif
            print *, "FORTRAN MODEL1", model_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL1", model_id," syncing DONE for time step ",t
        ! So now we can do some work. Let's suppose model1 is the LES, and it requests data from model2, WRF.
        ! The data requested consists of 1-D and a 3-D array of floats
        print *, "FORTRAN MODEL1: sending DREQ 1 from",model_id,'to',DEST_2
        call gmcfRequestData(model_id,GMCF_VAR_NAME_1, size(var_name_1), DEST_2, PRE, t_sync) ! check if PRE/POST makes some sense here
        print *, "FORTRAN MODEL1: sending DREQ 2 from",model_id,'to',DEST_2
        call gmcfRequestData(model_id,GMCF_VAR_NAME_2, size(var_name_2), DEST_2, POST, t_sync)

        call gmcfWaitFor(model_id,RESPDATA, 2)
                print *, "FORTRAN MODEL1: got 2 DRESPs ..."

        ! and then we read them
        call gmcfHasPackets(model_id,RESPDATA,has_packets)
        do while (has_packets==1)
            call gmcfShiftPending(model_id,RESPDATA,packet,fifo_empty)
            ! read a packet
            select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                case (GMCF_VAR_NAME_1)
                    call gmcfRead1DFloatArray(var_name_1,shape(var_name_1), packet)
                case (GMCF_VAR_NAME_2)
                    call gmcfRead3DFloatArray(var_name_2,shape(var_name_2), packet)
            end select
            call gmcfHasPackets(model_id,RESPDATA,has_packets)
        end do
        ! And now we can do work on this data
#ifdef WV_OK
#endif

    end do

    print *, "FORTRAN MODEL1", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine

