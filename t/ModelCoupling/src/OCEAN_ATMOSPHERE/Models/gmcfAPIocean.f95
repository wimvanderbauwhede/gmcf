!#define GMCF_VERBOSE
module gmcfAPIocean
    use gmcfAPI

    implicit none

    integer :: ocean_id
    integer :: sync_done, has_packets, fifo_empty
    integer ::  t_sync, t_sync_prev, t_sync_step, t_inter, t_ocean
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, ATMOSPHERE_ID=2
!    integer, parameter :: OCEAN_OCEAN_IP=108,OCEAN_OCEAN_JP=108,OCEAN_OCEAN_KP=27
    integer, parameter :: OCEAN_IP=100,OCEAN_JP=100,OCEAN_KP=26
    type(gmcfPacket) :: packet
    real(4), dimension(1:2,OCEAN_OCEAN_IP,OCEAN_OCEAN_JP) :: wind_profile, wind_profile_prev ! AD-HOC!
    real(4), dimension(OCEAN_OCEAN_IP,OCEAN_OCEAN_KP) :: temp, temp_prev ! AD-HOC!
    save

contains
!$GMCF Init(timesteps=20) !if >1 it means we need to interpolate otherwise not
    subroutine gmcfInitOcean(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
#ifdef GMCF_VERBOSE
        print *, "FORTRAN OCEAN gmcfInitOcean()"
#endif
        ocean_id=m_id
        t_ocean = 0
        t_sync_prev = -1
        t_sync = t_ocean
        t_sync_step = 20
        call gmcfInitCoupler(sys,tile, ocean_id)
    end subroutine gmcfInitOcean

    subroutine gmcfSyncOcean

        t_sync = t_ocean / t_sync_step ! means ocean steps are smaller and more frequent than atmosphere
        t_inter = mod(t_ocean,t_sync_step) ! for interpolation between two steps of atmosphere
        t_ocean = t_ocean + 1

        if (gmcfStatus(ATMOSPHERE_ID) /= FIN) then
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
#ifdef GMCF_VERBOSE
        print *, "FORTRAN OCEAN BEFORE gmcfSync()",t_ocean, t_sync, t_sync_prev
#endif
        if (t_sync == t_sync_prev+1) then
#ifdef GMCF_VERBOSE
        print *, "FORTRAN OCEAN BEFORE gmcfSync()"
#endif
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(ocean_id,t_sync,sync_done)
#ifdef GMCF_VERBOSE
            print *, "FORTRAN OCEAN AFTER gmcfSync()"
            print *, "FORTRAN OCEAN", ocean_id," sync loop ",t_ocean,"..."
#endif
        end do
#ifdef GMCF_VERBOSE
        print *, "FORTRAN OCEAN", ocean_id," syncing DONE for time step ",t_ocean
#endif
        end if ! t_sync
        end if ! FIN
    end subroutine gmcfSyncOcean

    subroutine gmcfPreOcean ! (var_name_1,var_name_1_prev,var_name_2,var_name_2_prev)
!        real(kind=4), dimension(128), intent(InOut) :: var_name_1, var_name_1_prev
!        real(kind=4), dimension(128,128,128), intent(InOut) :: var_name_2, var_name_2_prev
        if (t_sync == t_sync_prev+1) then
            ! So now we can do some work. Let's suppose ocean is the OCEAN, and it requests data from model2, OCEAN.
            ! First overwrite the *prev vars with the current vars

             wind_profile_prev = wind_profile

            ! The data requested consists of 1-D and a 3-D array of floats
!            print *, "FORTRAN OCEAN: sending DREQ 1 from",ocean_id,'to',ATMOSPHERE_ID
!            call gmcfRequestData(ocean_id,GMCF_VAR_NAME_1, size(var_name_1), ATMOSPHERE_ID, PRE, t_sync) ! check if PRE/POST makes some sense here
#ifdef GMCF_VERBOSE
            print *, "FORTRAN OCEAN: sending DREQ 2 from",ocean_id,'to',ATMOSPHERE_ID
#endif
            call gmcfRequestData(ocean_id,GMCF_VAR_NAME_2, size(wind_profile), ATMOSPHERE_ID, POST, t_sync)

            call gmcfWaitFor(ocean_id,RESPDATA, ATMOSPHERE_ID, 1)
#ifdef GMCF_VERBOSE
                    print *, "FORTRAN OCEAN: got 1 DRESP ..."
#endif

            ! and then we read them
            call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
            do while (has_packets==1)
                call gmcfShiftPending(ocean_id,RESPDATA,packet,fifo_empty)
                ! read a packet
                select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfRead1DFloatArray(var_name_1,shape(var_name_1), packet)
                    case (GMCF_VAR_NAME_2)
                        call gmcfRead3DFloatArray(wind_profile,shape(wind_profile), packet)
                end select
                call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
            end do
            print *, "FORTRAN OCEAN: SANITY: ",sum(wind_profile)
#ifdef GMCF_VERBOSE
            print *, "FORTRAN OCEAN: DONE reading DRESP into vars, ready to compute ..."
#endif
            t_sync_prev = t_sync
            print *, "OCEAN time step: ",t_ocean, "SYNC time step: ",t_sync
!        else
!            print *, "FORTRAN OCEAN: skipping time step ",t_ocean, t_sync
        end if ! of t_sync

    end subroutine gmcfPreOcean

    subroutine gmcfPostOcean
!        real(kind=4), dimension(128,128), intent(In) :: wind_profile

        if (gmcfStatus(ATMOSPHERE_ID) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
        print *,"FORTRAN OCEAN: WAITING FOR REQDATA (POST) ..."
#endif
        ! The problem is, this should not happen if the consumer has finished. Problem is that this could happen
        ! while we are blocking. So we need a status to say "someone finished"

        call gmcfWaitFor(wrf_id,REQDATA, ATMOSPHERE_ID, 1)
        end if ! FIN

        if (gmcfStatus(ATMOSPHERE_ID) /= FIN) then
        call gmcfHasPackets(wrf_id,REQDATA,has_packets)
        if (has_packets==1) then
        call gmcfShiftPending(wrf_id,REQDATA,packet,fifo_empty)
        select case (packet%data_id)
            case (GMCF_VAR_NAME_2)
            if (packet%pre_post == POST) then
!#ifdef GMCF_VERBOSE
                    print *,"FORTRAN OCEAN: SENDING RESPDATA (POST) from",wrf_id,'to',packet%source,"SANITY:",sum(wind_profile)
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
    end subroutine gmcfPostOcean

    subroutine gmcfInterpolateWindprofileOcean(u,v)
    ! The values of OCEAN_OCEAN_IP,OCEAN_OCEAN_JP,OCEAN_OCEAN_KP must be known at code generation time!
        real(kind=4), dimension(0:OCEAN_IP+1,-1:OCEAN_JP+1,0:OCEAN_KP+1), intent(InOut)  :: u
        real(kind=4), dimension(0:OCEAN_IP+1,-1:OCEAN_JP+1,0:OCEAN_KP+1), intent(InOut)  :: v
        real(kind=4), dimension(0:OCEAN_IP+1,-1:OCEAN_JP+1,-1:OCEAN_KP+1), intent(InOut)  :: w
        integer :: j,k,jj,kk
        real(4) :: interp_u,interp_v,interp_w
        do jj=3,OCEAN_OCEAN_JP-4 ! OCEAN_OCEAN_JP spans -3 .. 104 so we need j=4,103 if we ignore bounds, 3,105 if we include 1 point
            j = jj - 3
            k = kk + 1
            do kk=0,OCEAN_OCEAN_KP-1 ! this is 1 .. 27 and there are 26 layers so we have no bound for -1 and 0 from OCEAN
            ! I suppose that would be soil anyway ...
                interp_u = ( wind_profile_prev(1,jj,kk)*t_inter + wind_profile(1,jj,kk)*(t_sync_step-t_inter) ) / t_sync_step
                interp_v = ( wind_profile_prev(2,jj,kk)*t_inter + wind_profile(2,jj,kk)*(t_sync_step-t_inter) ) / t_sync_step
                interp_w = ( wind_profile_prev(3,jj,kk)*t_inter + wind_profile(3,jj,kk)*(t_sync_step-t_inter) ) / t_sync_step
                u(0,j,k)=interp_u
                v(0,j,k)=interp_v
                w(0,j,k)=interp_w
                u(1,j,k)=interp_u
                v(1,j,k)=interp_v
                w(1,j,k)=interp_w
            end do
        end do
    end subroutine gmcfInterpolateWindprofileOcean

    subroutine gmcfFinishedOcean
        call gmcfFinished(ocean_id)
    end subroutine gmcfFinishedOcean

end module gmcfAPIles
