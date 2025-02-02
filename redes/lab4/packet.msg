// packet.msg
packet Packet {
    int source;
    int destination;
    int hopCount;
    int type;
    int UsedBuffer;
    bool duplicated;
    int n_nodes;
}