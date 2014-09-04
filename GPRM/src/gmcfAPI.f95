! While this module is specific to the coupled system, it is generic with respect to the individual models.
! Needs refining, esp. the way we block on calls
! Also, maybe I want to use pragmas to insert the coupling code
! Also, we don't need model_id and per-model arrays because if my reasoning is correct, the module memory space will be per-thread

! To try this out I need to create a small C++ program that spawns two threads and in each thread we access the variables from a module

! According to http://stackoverflow.com/questions/23743716/simultaneous-calls-of-a-method-in-a-fortran-dll-with-module-variables-in-c-shar
! The variables in the module will be shared after all, so I need to keep the current approach.
! Seems to me that we could actually use this to make sharing easy ...

module gmcfAPI

    implicit none

    integer(8) :: sba_sys ! There is only one System
    integer(8), dimension(NMODELS) :: sba_tile ! I think this needs to be an array

    integer, parameter :: PRE = 0,POST = 1, BLOCKING=1, NON_BLOCKING=0
    integer, parameter :: REQDATA=6, REQTIME=7, RESPDATA=8, RESPTIME=9
!    integer, parameter :: NMODELS = 2 ! FIXME: use MACRO. In GPRM NSERVICES is a const UINT but I can easily change that.

    type gmcfPacket
        integer :: source
        integer :: destination
        integer :: type
        integer :: timestamp
        integer :: pre_post
        integer :: data_id
        integer(8) :: data_sz
        integer(8) :: data_ptr
    end type gmcfPacket

    type(gmcfPacket), dimension(NMODELS) :: gmcfDataRequests
    !        type(gmcfPacket), dimension(NMODELS) :: gmcfData
    integer, dimension(NMODELS,NMODELS) :: sync_status
    integer, dimension(NMODELS) :: sync_counter = NMODELS-1

    save

contains

    subroutine gmcfInitCoupler(sysptr,tileptr,model_id)
        integer(8), intent(In) :: sysptr
        integer(8), intent(In) :: tileptr
        integer, intent(In) :: model_id
        sba_sys= sysptr
        sba_tile(model_id)  = tileptr
    end subroutine gmcfInitCoupler

    subroutine gmcfSync(model_id,time,sync_done)
        integer, intent(In) :: time, model_id
        integer, intent(InOut) :: sync_done
        ! Start by sending N-1 requests for the simulation time (all except yourself)
        integer :: dest,requester, recvfrom, recvtime, fifo_empty, sync_irq

        type(gmcfPacket) :: packet
        sync_counter = NMODELS-1
        ! Send requests for timestamps to all others
        do dest=1,NMODELS
            sync_status(model_id,dest)=0
            if (dest /= model_id) then
                call gmcfSendTimeRequest(model_id,dest,time)
            end if
        end do
        ! Read from the FIFO, block if there is nothing there.
        sync_done = 0
        sync_irq = 0
        do while (sync_done == 0 .and. sync_irq == 0)
            call gmcfReadFromFifo(model_id,packet,fifo_empty) ! this is a blocking call
            ! You'll get requests and/or data.
            select case (packet%type)
                ! For every request, send data;
                case (REQDATA)
                    ! If it's a request for data, send the current values
                    ! send requested data. So I need access to all data in here!
                    ! As long as we don't send the data, we will not receive the time stamp from that model
                    ! So we must send immediately
                    ! I guess we could break out of the sync,
                    ! send the data and then go back to the sync
                    ! If we store the control variables in an array in the module, that could work
                    gmcfDataRequests(model_id)=packet
                    sync_irq = 1
                case (REQTIME)
                    print *, "FORTRAN API SYNC: GOT A REQTIME PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
! I received a request for timestamp, send a RESPTIME to the sender
                    requester = packet%destination
                    call gmcfSendTimestamp(model_id,requester,time)
                case (RESPTIME)
                    print *, "FORTRAN API SYNC: GOT A RESPTIME PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
                    ! If the data is a timestamp, check the sender address and value
                    ! receive time, check
                    recvfrom = packet%source
                    recvtime = packet%timestamp
                    if (recvtime == time .and. sync_status(model_id,recvfrom)==0) then
                        sync_status(model_id,recvfrom)=1
                        sync_counter(model_id) = sync_counter(model_id) - 1
                        if ( sync_counter(model_id) == 0) then
                            sync_done = 1
                        end if
                    end if
                case (RESPDATA)
                    print *, "FORTRAN API SYNC: GOT A RESPDATA PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
                    ! If it's data, presumably this means there is some non-blocking request for it
                    ! receive data. Again, requires access to all data ...
                    ! But we can actually defer this until after the sync,
                    ! Just put these data packets in the data fifo
                    call gmcfPushPending(model_id, packet)
                    ! Then update the current values after the sync
                case default
                    print *, "FORTRAN API SYNC: GOT AN UNEXPECTED PACKET:",   packet%type ," from ", packet%source, " to", packet%destination
            end select
        end do

        ! keep going until you've sent data to all and received data from all.

    end subroutine gmcfSync

    ! This is a blocking call on the main FIFO, no muxing
    subroutine gmcfReadFromFifo(model_id,packet,fifo_empty)
        integer, intent(In) :: model_id
        type(gmcfPacket), intent(Out) :: packet
        integer, intent(Out) :: fifo_empty
        integer :: source, destination, packet_type, timestamp, pre_post, data_id
        integer(8) :: data_sz, data_ptr

        ! So here we need to call into GPRM
        call gmcfreadfromfifoc(sba_sys, sba_tile(model_id), source, destination, packet_type, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
        print *,'FORTRAN API: AFTER gmcfreadfromfifoc'
        packet%type=packet_type
        packet%source=source
        packet%destination=destination
        packet%timestamp=timestamp
        packet%pre_post=pre_post
        packet%data_id=data_id
        packet%data_sz=data_sz
        packet%data_ptr=data_ptr

    end subroutine gmcfReadFromFifo

    subroutine gmcfSendTimeRequest(model_id,dest, timestamp)
        integer, intent(In) :: model_id,dest, timestamp
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, dest, REQTIME, 0, PRE, timestamp,1,0)
    end subroutine gmcfSendTimeRequest

    subroutine gmcfSendTimestamp(model_id,requester, timestamp)
        integer, intent(In) :: model_id,requester, timestamp
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, requester, RESPTIME, 0, PRE, timestamp,1, 0)
    end subroutine gmcfSendTimestamp

    subroutine gmcfWaitFor(model_id,packet_type,npackets)
        integer, intent(In) :: model_id,packet_type,npackets
        ! What we do here is listen for packets and demux them, probably using a C function
        ! We do this until there is a packet in the fifo for packet_type
        print *,"FORTRAN API: gmcfWaitFor(",packet_type,npackets,")"
        call gmcfwaitforpacketsc(sba_sys, sba_tile(model_id), packet_type,npackets)
        ! After this, packets will be in their respective queues, with the packet_type queue guaranteed containing at least one packet
    end subroutine

    subroutine gmcfHasPackets(model_id, packet_type, has_packets)
        integer, intent(In) :: model_id, packet_type
        integer, intent(Out) :: has_packets
        call gmcfcheckfifoc(sba_sys, sba_tile(model_id),packet_type,has_packets);
    end subroutine gmcfHasPackets

    subroutine gmcfShiftPending(model_id, packet_type,packet,fifo_empty)
        integer, intent(In) :: model_id, packet_type
        type(gmcfPacket), intent(Out) :: packet
        integer, intent(Out) :: fifo_empty
        integer :: source, destination, timestamp, pre_post, data_id
        integer(8) :: data_sz, data_ptr

        call gmcfshiftpendingc(sba_sys, sba_tile(model_id), packet_type,source, destination, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
        packet%type=packet_type
        packet%source=source
        packet%destination=destination
        packet%timestamp=timestamp
        packet%pre_post=pre_post
        packet%data_id=data_id
        packet%data_sz=data_sz
        packet%data_ptr=data_ptr
    end subroutine gmcfShiftPending

    subroutine gmcfPushPending(model_id, packet)
        integer, intent(In) :: model_id
        type(gmcfPacket), intent(In) :: packet
        call gmcfpushpendingc(sba_sys, sba_tile(model_id), packet%type,packet%source, packet%destination, packet%timestamp, packet%pre_post, packet%data_id, packet%data_sz, packet%data_ptr)
    end subroutine gmcfPushPending

    subroutine gmcfRequestData(model_id,data_id, data_sz, dest, pre_post, timestamp)
        integer, intent(In) :: model_id, data_id, dest, pre_post, timestamp
        integer, intent(In) :: data_sz
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, dest, REQDATA, data_id, pre_post, timestamp,data_sz,0)
    end subroutine gmcfRequestData
    ! same for other types and sizes, as in OpenCL wrapper

    subroutine gmcfSend1DFloatArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(1), intent(In):: sz
        integer,intent(In) :: data_id
        real,dimension(sz(1)), intent(In) :: array

        integer, intent(In) :: model_id, destination,pre_post,time
        integer :: sz1d
        sz1d = size(array)*4

        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend1DFloatArray

    subroutine gmcfSend3DFloatArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(3), intent(In):: sz
        integer,intent(In) :: data_id
        real,dimension(sz(1), sz(2), sz(3)), intent(In) :: array
        integer, intent(In) :: model_id, destination,pre_post,time
        integer :: sz1d
        sz1d = size(array)*4
        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend3DFloatArray

   subroutine gmcfRead1DFloatArray(array, sz, packet) ! This will have to be a wrapper around a C function
        ! read data packets from the data fifo and update the variables
        ! We do the same as in OpenCL
        ! Then we get the pointer from the packet and cast it
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer :: sz1d
        integer, dimension(1):: sz
        real,dimension(sz(1)) :: array
        real, dimension(size(array)):: array1d
        sz1d = size(array)*4
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
        call gmcffloatarrayfromptrc(ptr,array1d)
        array = reshape(array1d,shape(array))
    end subroutine

    subroutine gmcfRead3DFloatArray(array, sz, packet)
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer :: sz1d
        integer, dimension(3):: sz
        real,dimension(sz(1), sz(2), sz(3)) :: array
        real, dimension(size(array)):: array1d
        sz1d = size(array)*4
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
        call gmcffloatarrayfromptrc(ptr,array1d) ! This ugly function will simply cast the ptr and return it as the array1d
        array = reshape(array1d,shape(array))
    end subroutine

end module gmcfAPI
