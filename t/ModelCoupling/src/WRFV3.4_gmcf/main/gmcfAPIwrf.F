#define GMCF_DEBUG
module gmcfAPIwrf
#ifdef FULL_MODEL_API
    use gmcfAPI
#endif
    implicit none

    integer :: wrf_id
    integer :: sync_done, has_packets, fifo_empty, t_sync, t_wrf
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
#ifdef FULL_MODEL_API
    type(gmcfPacket) :: packet
#endif

    save

contains

    subroutine gmcfInitWrf(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
        wrf_id=m_id
        t_wrf = 0
#ifdef FULL_MODEL_API
        call gmcfInitCoupler(sys,tile, wrf_id)
#endif
    end subroutine gmcfInitWrf


    subroutine gmcfSyncWrf ! (var_name_1, var_name_2)

!        real(kind=4), dimension(128), intent(In) :: var_name_1
!        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

        t_sync = t_wrf
        t_wrf = t_wrf + 1
#ifdef FULL_MODEL_API
        if (gmcfStatus(DEST_1) /= FIN) then
        print *, "FORTRAN WRF BEFORE gmcfSync()"
        sync_done=0

        do while(sync_done == 0)
            call gmcfSync(wrf_id,t_sync,sync_done)
            print *, "FORTRAN WRF AFTER gmcfSync()"

            if (sync_done == 0) then
            print *, "FORTRAN WRF SYNC NOT DONE!"
!                select case (gmcfDataRequests(wrf_id)%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfSend1DFloatArray(wrf_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(wrf_id)%source,PRE,t_sync)
!                    case (GMCF_VAR_NAME_2)
!                        call gmcfSend3DFloatArray(wrf_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(wrf_id)%source,PRE,t_sync)
!                end select
            end if

            print *, "FORTRAN WRF", wrf_id," sync loop ",t_wrf,"..."
        end do

        print *, "FORTRAN WRF", wrf_id," syncing DONE for time step ",t_wrf
        end if ! FIN
#endif
    end subroutine gmcfSyncWrf


#if 0
    subroutine gmcfPreWrf(var_name_1)
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

        print *,"FORTRAN WRF: WAITING FOR REQDATA (PRE) ..."
        call gmcfWaitFor(wrf_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
            call gmcfShiftPending(wrf_id,REQDATA,packet, fifo_empty)
            print *,"FORTRAN WRF: GOT a REQDATA packet (PRE) from ",packet%source,'to',packet%destination
            select case (packet%data_id)
                case (GMCF_VAR_NAME_1)
                    if (packet%pre_post == PRE) then
                        print *,"FORTRAN WRF: SENDING RESPDATA (PRE) from",wrf_id,'to',packet%source,"SANITY:",sum(var_name_1)
                        call gmcfSend1DFloatArray(wrf_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,packet%source,PRE,t_sync)
                    else
                        print *,'FORTRAN WARNING: request was for POST, this is PRE, deferring'
                    end if
                case default
                    print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
            end select
        end if ! FIN
    end subroutine gmcfPreWrf

    subroutine gmcfPostWrf(var_name_2)
        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

        if (gmcfStatus(DEST_1) /= FIN) then
        ! Wait for one post data request
        print *,"FORTRAN WRF: WAITING FOR REQDATA (POST) ..."
        ! The problem is, this should not happen if the consumer has finished. Problem is that this could happen
        ! while we are blocking. So we need a status to say "someone finished"

        call gmcfWaitFor(wrf_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
        call gmcfHasPackets(wrf_id,REQDATA,has_packets)
        if (has_packets==1) then
        call gmcfShiftPending(wrf_id,REQDATA,packet,fifo_empty)
        select case (packet%data_id)
            case (GMCF_VAR_NAME_2)
            if (packet%pre_post == POST) then
                    print *,"FORTRAN WRF: SENDING RESPDATA (POST) from",wrf_id,'to',packet%source,"SANITY:",sum(var_name_2)
                    call gmcfSend3DFloatArray(wrf_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2,packet%source,POST,t_sync)
                else
                    print *,'FORTRAN WARNING: request was for PRE, this is POST. Sending POST value!'
                end if
            case default
                print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
        end select
        end if

        end if ! FIN
    end subroutine gmcfPostWrf
#endif
end module gmcfAPIwrf
