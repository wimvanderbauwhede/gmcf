#define MODEL_API
! This is the producer of a producer/consumer coupled model example.
subroutine program_atmosphere_gmcf(sys, tile, model_id) ! This replaces 'program main'
! Lines marked with ! gmcf-coupler / ! end gmcf-coupler are additions for coupling

! gmcf-coupler
    use gmcfAPI
#ifdef MODEL_API
    use gmcfAPImodel2
#endif
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
! end gmcf-coupler
#ifndef MODEL_API
!    ! gmcf-coupler
    integer :: sync_done, has_packets, fifo_empty, t_sync
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet
!    ! end gmcf-coupler
#endif
    integer :: t,t_start,t_stop,t_step, ii, jj, kk
    real(kind=4), dimension(128) :: var_name_1
    real(kind=4), dimension(128,128,128) :: var_name_2
    t_start=0
    t_stop = 11 ! Problem is, it does not finish because the other model has finished
    t_step = 1

    ! gmcf-coupler
    ! Init amongst other things gathers info about the time loops, maybe from a config file, need to work this out in detail:
#ifdef MODEL_API
    call gmcfInitModel2(sys,tile, model_id)
#else
    call gmcfInitCoupler(sys,tile, model_id)
#endif
    ! end gmcf-coupler

    print *, "FORTRAN MODEL2", model_id,"main routine called with pointers",sys,tile
        ! Compute initial
        do ii=1,128
            var_name_1(ii) = 0.0
        do jj=1,128
        do kk=1,128
            var_name_2(ii,jj,kk) = 0.0
        end do
        end do
        end do
        var_name_1(1) = 55.7188
        var_name_2(1,1,1) = 55.7188

        print *, "FORTRAN MODEL2", model_id,"WORK INIT DONE:",sum(var_name_1),sum(var_name_2)

    do t = t_start,t_stop,t_step
        print *, "FORTRAN MODEL2", model_id," syncing for time step ",t,"..."
        ! gmcf-coupler
        ! Sync all models by requesting all timesteps, block until all received, and sending your timestep to all who ask, until everyone has asked?
        ! Is this possible? I think it is OK:
        ! Start by sending N-1 requests; read from the FIFO,
        ! block if there is nothing there.
        ! You'll get requests and/or data. For every request, send data; keep going until you've sent data to all and received data from all.
        ! I don't think this will deadlock.
#ifdef MODEL_API
        ! Sync will synchronise simulation time steps but also handle any pending requests
        call gmcfSyncModel2(t, var_name_1, var_name_2)

        call gmcfPreModel2(var_name_1)
#else
        !$GMC sync(t)
        t_sync = t

        if (gmcfStatus(DEST_1) /= FIN) then

        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model_id,t,sync_done)
            print *, "FORTRAN MODEL2 AFTER gmcfSync()"

            if (sync_done == 0) then
            print *, "FORTRAN MODEL2 SYNC NOT DONE!"
                select case (gmcfDataRequests(model_id)%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfSend1DFloatArray(model_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(model_id)%source,PRE,t_sync)
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(model_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(model_id)%source,PRE,t_sync)
                end select
            end if

            print *, "FORTRAN MODEL2", model_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL2", model_id," syncing DONE for time step ",t
        end if ! FIN
        ! Now, it could be that FIN is reached here for DEST_1, so we should stop here
        if (gmcfStatus(DEST_1) /= FIN) then
        ! This is the producer, at this point it can start computing
        ! Question is if any requests to PRE/POST data should be handled here or if they can wait for the sync?
        ! If the request is PRE, it means the current data, if it's POST, it means the new data
        ! If I handle all requests during sync, it might be fine. Alternatively, I could block on PRE requests before
        ! starting a computation. Can this deadlock?
        ! And I can block on POST requests after the computation but before sync.
        ! I think it is neater to block on PRE/POST requests separately from sync

        ! Wait for one pre data request

        print *,"FORTRAN MODEL2: WAITING FOR REQDATA (PRE) ..."
        call gmcfWaitFor(model_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
        call gmcfShiftPending(model_id,REQDATA,packet, fifo_empty)
        print *,"FORTRAN MODEL2: GOT a REQDATA packet (PRE) from ",packet%source,'to',packet%destination
        select case (packet%data_id)
            case (GMCF_VAR_NAME_1)
                if (packet%pre_post == PRE) then
                    print *,"FORTRAN MODEL2: SENDING RESPDATA (PRE) from",model_id,'to',packet%source,"SANITY:",sum(var_name_1)
                    call gmcfSend1DFloatArray(model_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,packet%source,PRE,t)
                else
                    print *,'FORTRAN WARNING: request was for POST, this is PRE, deferring'
                end if
            case default
                print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
        end select
        end if ! FIN
#endif
        if (gmcfStatus(DEST_1) /= FIN) then
        ! Compute
        do ii=1,128
            var_name_1(ii) = 1e-6*ii / (t_sync+1)
            do jj=1,128
                do kk=1,128
                    var_name_2(ii,jj,kk) = sqrt(1.0*ii*jj*kk)/(t_sync+1)
                end do
            end do
        end do
        var_name_1(1) = 55.7188
        var_name_2(1,1,1) = 55.7188
        print *, "FORTRAN MODEL2", model_id,"WORK DONE:",sum(var_name_1),sum(var_name_2)
        end if ! FIN
#ifndef MODEL_API
        if (gmcfStatus(DEST_1) /= FIN) then
        ! Wait for one post data request
        print *,"FORTRAN MODEL2: WAITING FOR REQDATA (POST) ..."
        ! The problem is, this should not happen if the consumer has finished. Problem is that this could happen
        ! while we are blocking. So we need a status to say "someone finished"

        call gmcfWaitFor(model_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
        call gmcfHasPackets(model_id,REQDATA,has_packets)
        if (has_packets==1) then
        call gmcfShiftPending(model_id,REQDATA,packet,fifo_empty)
        select case (packet%data_id)
            case (GMCF_VAR_NAME_2)
            if (packet%pre_post == POST) then
                    print *,"FORTRAN MODEL2: SENDING RESPDATA (POST) from",model_id,'to',packet%source,"SANITY:",sum(var_name_2)
                    call gmcfSend3DFloatArray(model_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2,packet%source,POST,t)
                else
                    print *,'FORTRAN WARNING: request was for PRE, this is POST. Sending POST value!'
                end if
            case default
                print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
        end select
        end if
        end if ! FIN
#else
        call gmcfPostModel2(var_name_2)
#endif
    end do
    call gmcfFinished(model_id)
    print *, "FORTRAN MODEL2", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine program_atmosphere_gmcf

