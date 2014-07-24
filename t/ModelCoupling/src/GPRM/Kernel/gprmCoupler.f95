! While this module is specific to the coupled system, it is generic with respect to the individual models.
! Needs refining, esp. the way we block on calls
! Also, maybe I want to use pragmas to insert the coupling code
! Also, we don't need model_id and per-model arrays because if my reasoning is correct, the module memory space will be per-thread
! To try this out I need to create a small C++ program that spawns two threads and in each thread we access the variables from a module
! According to http://stackoverflow.com/questions/23743716/simultaneous-calls-of-a-method-in-a-fortran-dll-with-module-variables-in-c-shar
! The variables in the module will be shared after all, so I need to keep the current approach.
! Seems to me that we could actually use this to make sharing easy ...
    module GPRMCoupler

        implicit none

        integer(8) :: sba_sys
        integer(8) :: sba_tile

        integer, parameter :: PRE = 0,POST = 1, BLOCKING=1, NON_BLOCKING=0
        integer, parameter :: REQDATA=0, REQTIME=1, RESPDATA=2, RESPTIME=3
        integer, parameter :: NMODELS = 2 ! OR MACRO like NSERVICES in GPRM

        type gprmPacket
            integer :: source
            integer :: destination
            integer :: type
            integer :: timestamp
            integer :: pre_post
            integer :: data_id
            integer(8) :: data_ptr ! This does not work. Instead, I'll use a C struct and a C function to make it work
        end type gprmPacket

        type(gprmPacket), dimension(NMODELS) :: gprmRequests
!        type(gprmPacket), dimension(NMODELS) :: gprmData
        integer, dimension(NMODELS,NMODELS) :: sync_status
        integer, dimension(NMODELS) :: sync_counter = NMODELS-1

        save

        contains

        subroutine gprmInitCoupler(sysptr,tileptr)
            integer(8), intent(In) :: sysptr
            integer(8), intent(In) :: tileptr
            sba_sys = sysptr
            sba_tile = tileptr
        end subroutine gprmInitCoupler

        subroutine gprmSync(model_id,time,sync_done)
            integer, intent(In) :: time, model_id
            integer, intent(InOut) :: sync_done
! Start by sending N-1 requests for the simulation time (all except yourself)
            integer :: dest,recvfrom, recvtime, fifo_empty, sync_irq


            type(gprmPacket) :: packet
            sync_counter = NMODELS-1

            do dest=1,NMODELS
                if (dest /= model_id) then
                    call gprmSendTimeRequest(dest)
                end if
            end do
! Read from the FIFO, block if there is nothing there.
!            fifo_empty = 0
            sync_done = 0
            sync_irq = 0
            do while (sync_done == 0 .and. sync_irq == 0)
                call gprmReadFromFifo(packet,fifo_empty) ! this is a blocking call
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
                        sync_irq = 1
                    case (REQTIME)
                        ! send time. This should not happen!
                        dest = packet%destination
                        call gprmSendTimeRequest(dest)
                    case (RESPTIME)
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
                        ! If it's data, presumably this means there is some non-blocking request for it
                        ! receive data. Again, requires access to all data ...
                        ! But we can actually defer this until after the sync,
                        ! Just put these data packets in the data fifo
                        ! Then update the current values after the sync
                end select
            end do

            ! keep going until you've sent data to all and received data from all.

        end subroutine gprmSync
        ! This is a call that can either block or not block. if it's non-blocking, the variables will not be used
!        subroutine gprmRequest1DFloatArray(var_code, array, array_size, destination, pre_post, time, blocking)
!           ! STUB!
!        end subroutine gprmRequest1DFloatArray
        ! For non-blocking calls, what will happen is that the data will at some point be available in the data fifo
        ! Basically, we wrap this in a loop until the fifo is empty, get the destination and code from the packet, and use these to match the required data
        ! This again requires some magic with C pointers
        subroutine gprmGet1DFloatArray(array, sz, packet) ! This will have to be a wrapper around a C function
            ! read data packets from the data fifo and update the variables
            ! We do the same as in OpenCL
            ! Then we get the pointer from the packet and cast it
            type(gprmPacket) :: packet
            integer(8):: ptr
            integer :: sz1d
            integer, dimension(1):: sz
            real,dimension(sz(1)) :: array
            real, dimension(size(array)):: array1d
            sz1d = size(array)*4
            ptr = packet%data_ptr
            call gprmarrayfromptrc(ptr,sz1d,array1d)
            array = reshape(array1d,shape(array))
        end subroutine

        subroutine gprmRead3DFloatArray(array, sz, packet)
            type(gprmPacket) :: packet
            integer(8):: ptr
            integer :: sz1d
            integer, dimension(3):: sz
            real,dimension(sz(1), sz(2), sz(3)) :: array
            real, dimension(size(array)):: array1d
            sz1d = size(array)*4
            ptr = packet%data_ptr
            call gprmarrayfromptrc(ptr,sz1d,array1d) ! This ugly function will simply cast the ptr and return it as the array1d
            array = reshape(array1d,shape(array))
        end subroutine

! same for other types and sizes, as in OpenCL wrapper

        subroutine gprmSend1DFloatArray(array, sz, destination,time)
!            integer, dimension(1),intent(In):: sz
!            real,dimension(sz(1)), intent(In) :: array
            integer, intent(In):: sz
            real,dimension(sz), intent(In) :: array

            integer, intent(In) :: destination, time
            integer :: sz1d
            sz1d = size(array)*4
            call gprmsendarrayc(sba_sys, sba_tile, destination, time, sz1d,array)
        end subroutine gprmSend1DFloatArray

        subroutine gprmSend3DFloatArray(array, sz, destination,time)
            integer, dimension(3), intent(In):: sz
            real,dimension(sz(1), sz(2), sz(3)), intent(In) :: array
            integer, intent(In) :: destination, time
            integer :: sz1d
            sz1d = size(array)*4
            call gprmsendarrayc(sba_sys, sba_tile, destination, time, sz1d,array)
        end subroutine gprmSend3DFloatArray
! same for other types and sizes, as in OpenCL wrapper

        subroutine gprmGetNumberOfRequests(nrequests)
            integer, intent(Out) :: nrequests
           ! STUB!
            nrequests = 1
        end subroutine
!
        subroutine gprmShiftPendingRequest(request)
            type(gprmPacket), intent(Out) :: request
            integer :: source
            integer :: destination
            integer :: type
            integer :: timestamp
            integer :: pre_post
            integer :: data_id
            integer(8) :: data_ptr
           ! STUB!
            request%type=REQDATA
            gprmshiftpacketc(REQDATA, source, destination, type, timestamp, pre_post, data_id, data_ptr)
            request%source=source
            request%destination=destination
            request%timestamp=timestamp
            request%pre_post=pre_post
            request%data_ptr=data_ptr
        end subroutine

        subroutine gprmWaitFor(packet_type)
            integer, intent(In) :: packet_type
            ! What we do here is listen for packets and demux them, probably using a C function
            ! We do this until there is a packet in the fifo for packet_type
            call gprmwaitforpacketc(packet_type)
            ! After this, packets will be in their respective queues, with the packet_type queue guaranteed containing at least one packet
        end subroutine

!        subroutine gprmHas(packet_type, has_packet)
!            integer, intent(In) :: packet_type
!            integer, intent(Out) :: has_packet
!            has_packet = 0 ! STUB!
!        end subroutine
    end module GPRMCoupler
