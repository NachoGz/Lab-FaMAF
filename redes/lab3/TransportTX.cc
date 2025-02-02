#ifndef TRANSPORTTX
#define TRANSPORTTX

#include <string.h>
#include <omnetpp.h>

#include "feedBack.h"

using namespace omnetpp;

class TransportTx: public cSimpleModule {
private:
    cQueue buffer;
    cMessage *endServiceEvent;
    simtime_t serviceTime;
    cOutVector packetDropVector;
    cOutVector bufferSizeVector;
    cOutVector sentPacketsVector;
    bool f_stop = false;
    unsigned long packetsToSend;
public:
    TransportTx();
    virtual ~TransportTx();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(TransportTx);

TransportTx::TransportTx() {
    endServiceEvent = NULL;
}

TransportTx::~TransportTx() {
    cancelAndDelete(endServiceEvent);
}

void TransportTx::initialize() {
    packetsToSend = 0;
    buffer.setName("Buffer");
    bufferSizeVector.setName("BufferSize");
    packetDropVector.setName("PacketDrop");
    sentPacketsVector.setName("PacketsSent");
    endServiceEvent = new cMessage("endService");
}

void TransportTx::finish() {
    recordScalar("Number of packets on TransportTx", buffer.getLength());
    recordScalar("Number of packets sent - TransportTx", sentPacketsVector.getValuesStored());
}

void TransportTx::handleMessage(cMessage *msg) {
    if (msg == endServiceEvent){
        //check buffer limit
        if (!buffer.isEmpty() && packetsToSend > 0) {
            // dequeue packet
            cPacket *pkt = (cPacket*) buffer.pop();
            // send packet
            send(pkt, "toOut$o");
            packetsToSend--;
            // start new service
            serviceTime = pkt->getDuration();
            sentPacketsVector.record(sentPacketsVector.getValuesStored() + 1);

            if (!endServiceEvent->isScheduled() && !f_stop){
                scheduleAt(simTime() + serviceTime, endServiceEvent);
            }
        }
    }

    else {
        // feedback "STOP" case
        if (msg->getKind() == 2){
            f_stop = true;
            delete msg;
        }

        // feedback "packet request" case
        else if (msg->getKind() == 3){
            f_stop = false;
            FeedbackPkt* feedbackPkt = (FeedbackPkt*) msg;
            packetsToSend = feedbackPkt->getRemainingBuffer();
            //std::cout << "pkt request: ";
            //std::cout << packetsToSend << '\n';
            delete msg;
        }

        // drop packet in case of full buffer size
        else if (buffer.getLength() >= par("bufferSize").intValue()) {
            //drop packet
            delete msg;
            this->bubble("packet dropped");
            packetDropVector.record(1);

        } else {
            //enqueue the packet
            buffer.insert(msg);
            bufferSizeVector.record(buffer.getLength());
            //if the server is idle
            if (!endServiceEvent->isScheduled() && !f_stop) {
                scheduleAt(simTime(), endServiceEvent);
            }
        }
    }
}

#endif
