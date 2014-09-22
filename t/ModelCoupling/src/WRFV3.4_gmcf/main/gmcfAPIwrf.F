
module gmcfAPIwrf
    use gmcfAPI

    implicit none

    integer :: wrf_id
    integer :: sync_done, has_packets, fifo_empty, t_sync, t_wrf
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    integer, parameter :: LES_WRF_IP=108,LES_WRF_JP=108,LES_WRF_KP=27
    integer, parameter :: WRF_IMS=-3,WRF_JMS=-3,WRF_KMS=1
    integer, parameter :: WRF_IME=104,WRF_JME=104,WRF_KME=27
    type(gmcfPacket) :: packet
    real(4), dimension(4,0:LES_WRF_IP-1,0:LES_WRF_KP-1) :: wind_profile

    save

contains

    subroutine gmcfInitWrf(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
        wrf_id=m_id
        t_wrf = 0
        call gmcfInitCoupler(sys,tile, wrf_id)
    end subroutine gmcfInitWrf


    subroutine gmcfSyncWrf ! (var_name_1, var_name_2)
!        real(4), dimension(128,128), intent(In)  :: wind_profile ! AD-HOC!
!        real(kind=4), dimension(128), intent(In) :: var_name_1
!        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

        t_sync = t_wrf
        t_wrf = t_wrf + 1
        if (gmcfStatus(DEST_1) /= FIN) then
#ifdef GMCF_VERBOSE
        print *, "FORTRAN WRF BEFORE gmcfSync()"
#endif
        sync_done=0

        do while(sync_done == 0)
            call gmcfSync(wrf_id,t_sync,sync_done)
#ifdef GMCF_VERBOSE
            print *, "FORTRAN WRF AFTER gmcfSync()"
#endif
            if (sync_done == 0) then
#ifdef GMCF_VERBOSE
            print *, "FORTRAN WRF SYNC NOT DONE!"
#endif
                select case (gmcfDataRequests(wrf_id)%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfSend1DFloatArray(wrf_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(wrf_id)%source,PRE,t_sync)
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(wrf_id,wind_profile, shape(wind_profile), GMCF_VAR_NAME_2, gmcfDataRequests(wrf_id)%source,PRE,t_sync)
                end select
            end if
#ifdef GMCF_VERBOSE
            print *, "FORTRAN WRF", wrf_id," sync loop ",t_wrf,"..."
#endif
        end do
#ifdef GMCF_VERBOSE
        print *, "FORTRAN WRF", wrf_id," syncing DONE for time step ",t_wrf
#endif
        end if ! FIN
    end subroutine gmcfSyncWrf


#if 0
! No need for Pre!
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
#ifdef GMCF_VERBOSE
        print *,"FORTRAN WRF: WAITING FOR REQDATA (PRE) ..."
#endif
        call gmcfWaitFor(wrf_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
            call gmcfShiftPending(wrf_id,REQDATA,packet, fifo_empty)
#ifdef GMCF_VERBOSE
            print *,"FORTRAN WRF: GOT a REQDATA packet (PRE) from ",packet%source,'to',packet%destination
#endif
            select case (packet%data_id)
                case (GMCF_VAR_NAME_1)
                    if (packet%pre_post == PRE) then
#ifdef GMCF_VERBOSE
                        print *,"FORTRAN WRF: SENDING RESPDATA (PRE) from",wrf_id,'to',packet%source,"SANITY:",sum(var_name_1)
#endif
                        call gmcfSend1DFloatArray(wrf_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,packet%source,PRE,t_sync)
                    else
#ifdef GMCF_VERBOSE
                        print *,'FORTRAN WARNING: request was for POST, this is PRE, deferring'
#endif
                    end if
                case default
#ifdef GMCF_VERBOSE
                    print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
#endif
            end select
        end if ! FIN
    end subroutine gmcfPreWrf
#endif

    subroutine gmcfPostWrf
!        real(kind=4), dimension(128,128), intent(In) :: wind_profile

        if (gmcfStatus(DEST_1) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
        print *,"FORTRAN WRF: WAITING FOR REQDATA (POST) ..."
#endif
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
!#ifdef GMCF_VERBOSE
                    print *,"FORTRAN WRF: SENDING RESPDATA (POST) from",wrf_id,'to',packet%source,"SANITY:",sum(wind_profile)
!#endif
                    call gmcfSend3DFloatArray(wrf_id,wind_profile, shape(wind_profile), GMCF_VAR_NAME_2,packet%source,POST,t_sync)
                else
#ifdef GMCF_VERBOSE
                    print *,'FORTRAN WARNING: request was for PRE, this is POST. Sending POST value!'
#endif
                end if
            case default
#ifdef GMCF_VERBOSE
                print *,'FORTRAN WARNING: request for invalid data:', packet%data_id
#endif
        end select
        end if

        end if ! FIN
    end subroutine gmcfPostWrf

    ! This routine will take the actual WRF variable as argument
    ! Problem is that we also need to know the start inside the mem array, I guess that is ids etc
    subroutine gmcfCreateWindprofileWrf(u,v,w)
        real(4), dimension(WRF_IMS:WRF_IME,WRF_jMS:WRF_JME,WRF_KMS:WRF_KME), intent(In) :: u,v,w

        integer :: j,k
        do j=WRF_JMS,WRF_JME
            do k=WRF_KMS,WRF_KME
                wind_profile(1,j-WRF_JMS,k-WRF_KMS)=4.21 ! u(1,j,k)
                wind_profile(2,j-WRF_JMS,k-WRF_KMS)=4.22 ! v(1,j,k)
                wind_profile(3,j-WRF_JMS,k-WRF_KMS)=4.23 ! w(1,j,k)
                wind_profile(4,j-WRF_JMS,k-WRF_KMS)=0.0 ! for cache alignment
            end do
        end do
    end subroutine gmcfCreateWindprofileWrf

    subroutine gmcfFinishedWrf
        call gmcfFinished(wrf_id)
    end subroutine gmcfFinishedWrf


end module gmcfAPIwrf
