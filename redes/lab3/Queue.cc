#ifndef QUEUE
#define QUEUE

#include <string.h>
#include <omnetpp.h>

using namespace omnetpp;

class Queue: public cSimpleModule {
private:
    cQueue buffer;
    cOutVector packetDropVector;
    cOutVector bufferSizeVector;
    cMessage *endServiceEvent;
    simtime_t serviceTime;
    // cOutVector packetSent;
    int mCounter;
public:
    Queue();
    virtual ~Queue();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(Queue);

Queue::Queue() {
    endServiceEvent = NULL;
}

Queue::~Queue() {
    cancelAndDelete(endServiceEvent);
}

void Queue::initialize() {
    buffer.setName("buffer");
    packetDropVector.setName("packetDrop");
    bufferSizeVector.setName("bufferSize");
    // packetSent.setName("packetSent");
    endServiceEvent = new cMessage("endService");
    mCounter = 0;
}

void Queue::finish() {
    recordScalar("Number of packets on queue", buffer.getLength());
    recordScalar("Number of dropped packets", packetDropVector.getValuesReceived());
}

void Queue::handleMessage(cMessage *msg) {
    // if msg is signaling an endServiceEvent
    mCounter++;
    if (msg == endServiceEvent) {
        // if packet in buffer, send next one
        if (!buffer.isEmpty()) {
            // dequeue packet
            // cMessage *pkt = (cMessage*) buffer.pop();
            cPacket *pkt = (cPacket*) buffer.pop();
            // send packet
            send(pkt, "out");
            // packetSent.record(1);
            // start new service
            serviceTime = pkt->getDuration();
            scheduleAt(simTime() + serviceTime, endServiceEvent);
        }
    } else { // if msg is a data packet
        // check buffer limit
        if (buffer.getLength() >= par("bufferSize").doubleValue()) { // .longValue() esta deprecated
            // drop the packet
            delete msg;
            this->bubble("packet dropped");
            packetDropVector.record(1);
        } else {
            // enqueue the packet
            buffer.insert(msg);
            bufferSizeVector.record(buffer.getLength());
            // if the server is idle
            if (!endServiceEvent->isScheduled()) {
                // start the service
                scheduleAt(simTime() + 0, endServiceEvent);
            }
        }
        
    }
}

#endif /* QUEUE */
