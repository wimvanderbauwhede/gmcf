#define GMCF_DEBUG
module gmcfAPImodel2
    use gmcfAPI

    implicit none

    integer :: sync_done, has_packets, fifo_empty, t_sync
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet

    save

contains

    subroutine gmcfSyncModel2(model_id, var_name_1, var_name_2, t)
        integer , intent(In) :: model_id, t
        real(kind=4), dimension(128), intent(In) :: var_name_1
        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2
        t_sync = t

        if (gmcfStatus(DEST_1) /= FIN) then

        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model_id,t_sync,sync_done)
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

    end subroutine gmcfSyncModel2

    subroutine gmcfPreModel2(model_id, var_name_1)
        integer , intent(In) :: model_id
        real(kind=4), dimension(128), intent(In) :: var_name_1

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
                        call gmcfSend1DFloatArray(model_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,packet%source,PRE,t_sync)
                    else
                        print *,'FORTRAN WARNING: request was for POST, this is PRE, deferring'
                    end if
                case default
                    print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
            end select
        end if ! FIN
    end subroutine gmcfPreModel2

    subroutine gmcfPostModel2(model_id, var_name_2)
        integer , intent(In) :: model_id
        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

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
                    call gmcfSend3DFloatArray(model_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2,packet%source,POST,t_sync)
                else
                    print *,'FORTRAN WARNING: request was for PRE, this is POST. Sending POST value!'
                end if
            case default
                print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
        end select
        end if

        end if ! FIN
    end subroutine gmcfPostModel2

end module gmcfAPImodel2
