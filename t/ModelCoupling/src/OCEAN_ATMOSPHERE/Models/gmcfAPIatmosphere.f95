
module gmcfAPIatmosphere
    use gmcfAPI

    implicit none

    integer :: atmosphere_id
    integer :: sync_done, has_packets, fifo_empty, t_sync, t_atmosphere
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    integer, parameter :: LES_ATMOSPHERE_IP=108,LES_ATMOSPHERE_JP=108,LES_ATMOSPHERE_KP=27
    integer, parameter :: ATMOSPHERE_IMS=-3,ATMOSPHERE_JMS=-3,ATMOSPHERE_KMS=1
    integer, parameter :: ATMOSPHERE_IME=104,ATMOSPHERE_JME=104,ATMOSPHERE_KME=27
    type(gmcfPacket) :: packet
    real(4), dimension(4,0:LES_ATMOSPHERE_IP-1,0:LES_ATMOSPHERE_KP-1) :: wind_profile

    save

contains

    subroutine gmcfInitAtmosphere(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
        atmosphere_id=m_id
        t_atmosphere = 0
        call gmcfInitCoupler(sys,tile, atmosphere_id)
    end subroutine gmcfInitAtmosphere


    subroutine gmcfSyncAtmosphere ! (var_name_1, var_name_2)
        t_sync = t_atmosphere
        t_wrf = t_atmosphere + 1
        if (gmcfStatus(DEST_1) /= FIN) then
#ifdef GMCF_VERBOSE
        print *, "FORTRAN ATMOSPHERE BEFORE gmcfSync()"
#endif

        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(atmosphere_id,t_sync,sync_done)
#ifdef GMCF_VERBOSE
            print *, "FORTRAN ATMOSPHERE AFTER gmcfSync()"
#endif

            if (sync_done == 0) then
#ifdef GMCF_VERBOSE
            print *, "FORTRAN ATMOSPHERE SYNC NOT DONE!"
#endif

                select case (gmcfDataRequests(atmosphere_id)%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(atmosphere_id,wind_profile, shape(wind_profile), GMCF_VAR_NAME_2, gmcfDataRequests(atmosphere_id)%source,PRE,t_sync)
                end select
            end if
#ifdef GMCF_VERBOSE
            print *, "FORTRAN ATMOSPHERE", wrf_id," sync loop ",t_wrf,"..."
#endif

        end do
#ifdef GMCF_VERBOSE
        print *, "FORTRAN ATMOSPHERE", wrf_id," syncing DONE for time step ",t_wrf
#endif

        end if ! FIN
    end subroutine gmcfSyncAtmosphere

    subroutine gmcfPreAtmosphere

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
        print *,"FORTRAN ATMOSPHERE: WAITING FOR REQDATA (PRE) ..."
#endif
        call gmcfWaitFor(atmosphere_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
            call gmcfShiftPending(atmosphere_id,REQDATA,packet, fifo_empty)
#ifdef GMCF_VERBOSE
            print *,"FORTRAN ATMOSPHERE: GOT a REQDATA packet (PRE) from ",packet%source,'to',packet%destination
#endif
            select case (packet%data_id)
                case (GMCF_VAR_NAME_1)
                    if (packet%pre_post == PRE) then
#ifdef GMCF_VERBOSE
                        print *,"FORTRAN ATMOSPHERE: SENDING RESPDATA (PRE) from",atmosphere_id,'to',packet%source,"SANITY:",sum(var_name_1)
#endif
                        call gmcfSend1DFloatArray(atmosphere_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,packet%source,PRE,t_sync)
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
    end subroutine gmcfPreAtmosphere


    subroutine gmcfPostAtmosphere

        if (gmcfStatus(DEST_1) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
        print *,"FORTRAN ATMOSPHERE: WAITING FOR REQDATA (POST) ..."
#endif
        ! The problem is, this should not happen if the consumer has finished. Problem is that this could happen
        ! while we are blocking. So we need a status to say "someone finished"

        call gmcfWaitFor(atmosphere_id,REQDATA, DEST_1, 1)
        end if ! FIN

        if (gmcfStatus(DEST_1) /= FIN) then
        call gmcfHasPackets(atmosphere_id,REQDATA,has_packets)
        if (has_packets==1) then
        call gmcfShiftPending(atmosphere_id,REQDATA,packet,fifo_empty)
        select case (packet%data_id)
            case (GMCF_VAR_NAME_2)
            if (packet%pre_post == POST) then
!#ifdef GMCF_VERBOSE
                    print *,"FORTRAN ATMOSPHERE: SENDING RESPDATA (POST) from",atmosphere_id,'to',packet%source,"SANITY:",sum(wind_profile)
!#endif
                    call gmcfSend3DFloatArray(atmosphere_id,wind_profile, shape(wind_profile), GMCF_VAR_NAME_2,packet%source,POST,t_sync)
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
    end subroutine gmcfPostAtmosphere

    ! This routine will take the actual ATMOSPHERE variable as argument
    ! Problem is that we also need to know the start inside the mem array, I guess that is ids etc
    subroutine gmcfCreateWindprofileAtmosphere(u,v,w)
        real(4), dimension(ATMOSPHERE_IMS:ATMOSPHERE_IME,ATMOSPHERE_jMS:ATMOSPHERE_JME,ATMOSPHERE_KMS:ATMOSPHERE_KME), intent(In) :: u,v,w

        integer :: j,k
        do j=ATMOSPHERE_JMS,ATMOSPHERE_JME
            do k=ATMOSPHERE_KMS,ATMOSPHERE_KME
                wind_profile(1,j-ATMOSPHERE_JMS,k-ATMOSPHERE_KMS)=4.21 ! u(1,j,k)
                wind_profile(2,j-ATMOSPHERE_JMS,k-ATMOSPHERE_KMS)=4.22 ! v(1,j,k)
                wind_profile(3,j-ATMOSPHERE_JMS,k-ATMOSPHERE_KMS)=4.23 ! w(1,j,k)
                wind_profile(4,j-ATMOSPHERE_JMS,k-ATMOSPHERE_KMS)=0.0 ! for cache alignment
            end do
        end do
    end subroutine gmcfCreateWindprofileAtmosphere

    subroutine gmcfFinishedAtmosphere
        call gmcfFinished(atmosphere_id)
    end subroutine gmcfFinishedAtmosphere


end module gmcfAPIatmosphere
