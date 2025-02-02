#ifndef TRANSPORTRX
#define TRANSPORTRX

#include <string.h>
#include <omnetpp.h>

#include "feedBack.h"

using namespace omnetpp;

class TransportRx: public cSimpleModule {
private:
    cQueue buffer;
    cMessage *endServiceEvent;
    cMessage *feedbackEvent;
    simtime_t serviceTime;
    simtime_t feedbackTime;
    cOutVector packetDropVector;
    cOutVector bufferSizeVector;
    cOutVector askedPackets;
    cOutVector lostPacketsVector;
    unsigned long packetsToRecieve;
    unsigned int lostPackets;
    unsigned int lastPacket;
    double trustIndex;
    bool isFirstPacket;

    void doFeedback(bool recievedPacket);
public:
    TransportRx();
    virtual ~TransportRx();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(TransportRx);

TransportRx::TransportRx() {
    endServiceEvent = NULL;
    feedbackEvent = NULL;
}

TransportRx::~TransportRx() {
    cancelAndDelete(endServiceEvent);
    cancelAndDelete(feedbackEvent);
}

void TransportRx::initialize() {
    packetsToRecieve = 0;
    lastPacket = 0;
    lostPackets = 0;
    trustIndex = 0.01;
    feedbackTime = 0.2;
    isFirstPacket = true;
    buffer.setName("Buffer");
    bufferSizeVector.setName("BufferSize");
    packetDropVector.setName("PacketDrop");
    askedPackets.setName("PacketsAsked");
    lostPacketsVector.setName("PacketsLost");
    endServiceEvent = new cMessage("endService");
    feedbackEvent = new cMessage("feedbackService");
    // here we schedule the first feedback pkt to start the transactions
    scheduleAt(simTime(), feedbackEvent);
}

void TransportRx::finish() {
    recordScalar("Number of packets on TransportRx", buffer.getLength());
}

void TransportRx::doFeedback(bool recievedPacket) {
    bool forceFeedback = false;

    // If the last packet recieved is equal to the number of packets to recieve
    // it means i haven't recieved a packet since the last feedback cicle.
    if(lastPacket == packetsToRecieve){
        lostPackets++;
        lostPacketsVector.record(1);
    }

    // If the number of lost packets exceds a threshold force a feedbackPacket.
    if(lostPackets > 5){
        forceFeedback = true;
        trustIndex /= 3;
    }

    // If buffer is 90% full, ask to stop sending packets.
    if (buffer.getLength() >= par("bufferSize").intValue() * 90 / 100) {
        FeedbackPkt* feedbackPkt = new FeedbackPkt();
        feedbackPkt->setByteLength(20);
        feedbackPkt->setKind(2);

        // If we didnt lose many packets, our trust increases.
        if(!forceFeedback){
            trustIndex *= 2;
            if(trustIndex > 1){
                trustIndex = 1;
            }
        }
        lostPackets = 0;

        send(feedbackPkt, "toOut$o");
    }

    // If 70% of the buffer is free ask for packets.
    else if ((buffer.getLength() <= par("bufferSize").intValue() * 70 / 100 &&
                packetsToRecieve == 0) || forceFeedback) {
        packetsToRecieve = par("bufferSize").intValue() - buffer.getLength();
        packetsToRecieve *= trustIndex;
        FeedbackPkt* feedbackPkt = new FeedbackPkt();
        feedbackPkt->setByteLength(20);
        feedbackPkt->setRemainingBuffer(packetsToRecieve);
        feedbackPkt->setKind(3);
        send(feedbackPkt, "toOut$o");

        // Ask as many packets as fit in the buffer.
        askedPackets.record(packetsToRecieve);

        // If we didnt lose many packets, our trust increases.
        if(!forceFeedback){
            trustIndex *= 1.5;
            if(trustIndex > 1){
                trustIndex = 1;
            }
        }
        lostPackets = 0;
    }

    if (!recievedPacket) lastPacket = packetsToRecieve;

    if(!feedbackEvent->isScheduled()) {
        scheduleAt(simTime() + feedbackTime, feedbackEvent);
    }
}

void TransportRx::handleMessage(cMessage *msg) {

    if (msg == endServiceEvent) {
        //check buffer limit
        if (!buffer.isEmpty()) {
            // dequeue packet
            cPacket *pkt = (cPacket*) buffer.pop();
            // send packet
            send(pkt, "toApp");
            // start new service
            serviceTime = pkt->getDuration();
            scheduleAt(simTime() + serviceTime, endServiceEvent);
        }
    }

    else if (msg == feedbackEvent) {
        doFeedback(0);
    }

    else {
        packetsToRecieve--;
        if (isFirstPacket){
            feedbackTime = (simTime() - msg->getCreationTime());
        }

        isFirstPacket = false;

        if (buffer.getLength() >= par("bufferSize").intValue()) {
            //drop packet
            delete msg;
            this->bubble("packet dropped");
            cPacket* feedbackPkt = new cPacket("STOP");
            feedbackPkt->setByteLength(20);
            feedbackPkt->setKind(2);
            send(feedbackPkt, "toOut$o");
            packetDropVector.record(1);
            return;
        } else {
            //enqueue the packet
            buffer.insert(msg);
            bufferSizeVector.record(buffer.getLength());
            //if the server is idle
            if (!endServiceEvent->isScheduled()) {
                //start service
                scheduleAt(simTime(), endServiceEvent);
            }
        }

        doFeedback(1);
    }
}

#endif
