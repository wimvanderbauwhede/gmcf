//==============================================================================
//
// General Types for Gannet -- for use in genPacket.pl
//
//==============================================================================

typedef uint8 Kind_t; //3
typedef uint8 Datatype_t; //1
typedef uint8 Ext_t; //1
typedef uint8 Quoted_t; //1
typedef uint8 Task_t; //2
typedef uint16 Subtask_t; //16

typedef uint8 Packet_type_t; //3
typedef uint8 Prio_t; //2
typedef uint8 Redir_t; //3

typedef uint16 Name_t; //16
typedef uint16 Count_t; //16

typedef uint16 Length_t; //16
typedef uint16 Return_to_t; //16
typedef uint16 To_to_t; //16

typedef Task_t DS_t; //2
typedef Word Symbol_t;
typedef Word_List Packet_t;
typedef Word_List Header_t;
typedef Word_List Payload_t;
typedef List<Packet_t> Packet_List;
typedef Fifo<Packet_t> Packet_Fifo;

