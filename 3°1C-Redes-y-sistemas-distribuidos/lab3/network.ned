simple Generator
{
    parameters:
        volatile double generationInterval; // sec
        volatile double packetByteSize;
        @display("i=block/source");
    gates:
        output out;
}

simple Queue
{
    parameters:
        volatile double bufferSize;
        @display("i=block/queue;q=buffer");
    gates:
        input in;
        output out;
}

simple Sink
{
    parameters:
        @display("i=block/sink");
    gates:
        input in;
}

simple TransportTx
{
    parameters:
        volatile int bufferSize;
        @display("i=block/queue;q=buffer");
    gates:
        input toApp;
        inout toOut;
}

simple TransportRx
{
    parameters:
        volatile int bufferSize;
        @display("i=block/queue;q=buffer");
    gates:
        output toApp;
        inout toOut;
}

module NodeTx 
{
    parameters:
        @display("i=block/routing");
    gates:
        output out;
    submodules:
        gen: Generator {
            parameters:
                
                @display("p=75,50");
        }
        queue: Queue {
            parameters:
                @display("p=75,190");
        }
    connections:
        gen.out --> queue.in;
        queue.out --> out;
}

module NodeRx
{
    parameters:
        @display("i=block/routing");
    gates:
        input in;
    submodules:
        sink: Sink {
            parameters:
                @display("p=75,50");
        }
        queue: Queue {
            parameters:
                @display("p=75,190");
        }
    connections:
        in --> queue.in;
        // case study 1
        // queue.out --> { datarate = 0.5Mbps; } --> sink.in;
        // case study 2
        queue.out --> { datarate = 1Mbps; } --> sink.in;
}

network Network
{
    @display("bgl=2");
    submodules:
        // gen: Generator {
        //     @display("p=30,30");
        // }
        nodeTx: NodeTx {
            @display("p=30,30");
        }
        queue: Queue {
            @display("p=130,30");
        }
        nodeRx: NodeRx {
            @display("p=230,30");
        }
        // sink: Sink {
        //     @display("p=230,30");
        // }
    connections:
        nodeTx.out --> { datarate = 1Mbps; delay = 100us; } --> queue.in;
        // case study 1
        // queue.out --> { datarate = 1Mbps; delay = 100us; } --> nodeRx.in;
        // case study 2
        queue.out --> { datarate = 0.5Mbps; delay = 100us; } --> nodeRx.in;
}

module NodeTxFV 
{
    parameters:
        @display("i=block/routing");
    gates:
        input in;
        output out;
    submodules:
        gen: Generator {
            parameters:
                
                @display("p=75,50");
        }
        queue: TransportTx {
            parameters:
                @display("p=75, 190");
        }
    connections:
        gen.out --> queue.toApp;
        queue.toOut$o --> out;
        queue.toOut$i <-- in;
}

module NodeRxFV
{
    parameters:
        @display("i=block/routing");
    gates:
        input in;
        output out;
    submodules:
        sink: Sink {
            parameters:
                @display("p=75,50");
        }
        queue: TransportRx {
            parameters:
                @display("p=75, 190");
        }
    connections:
        in --> queue.toOut$i;
        queue.toOut$o --> out;
        // case study 1
        // queue.toApp --> { datarate = 0.5Mbps; } --> sink.in;
        // case study 2
        queue.toApp --> { datarate = 1Mbps; } --> sink.in;
}

network NetworkFV
{
    @display("bgl=2");
    submodules:
        nodeTx: NodeTxFV {
            @display("p=30,30");
        }
        queueZero: Queue {
            @display("p=130,30");
        }
        queueOne: Queue {
            @display("p=130,90");
        }
        nodeRx: NodeRxFV {
            @display("p=230,30");
        }
    connections:
        nodeRx.out --> queueOne.in;
        queueOne.out --> nodeTx.in;
        nodeTx.out --> {  datarate = 1Mbps; delay = 100us; } --> queueZero.in;
        // case study 1
        // queueZero.out --> { datarate = 1Mbps; delay = 100us; } --> nodeRx.in;
        // case study 2
        queueZero.out --> { datarate = 0.5Mbps; delay = 100us; } --> nodeRx.in;
}

