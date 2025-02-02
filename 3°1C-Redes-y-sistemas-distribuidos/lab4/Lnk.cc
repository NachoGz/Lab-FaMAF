#ifndef LNK
#define LNK

#include <string.h>
#include <omnetpp.h>
#include <packet_m.h>
#include <iostream>

#define THRESHOLD 100

using namespace omnetpp;

class Lnk: public cSimpleModule {
private:
    cQueue buffer;
    cMessage *endServiceEvent;
    simtime_t serviceTime;
    cOutVector bufferSizeVector;
    cOutVector feedbackRecord;
    simtime_t lastFeedbackTime;
public:
    Lnk();
    virtual ~Lnk();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};

Define_Module(Lnk);

#endif /* LNK */

Lnk::Lnk() {
    endServiceEvent = NULL;
}

Lnk::~Lnk() {
    cancelAndDelete(endServiceEvent);
}

void Lnk::initialize() {
    lastFeedbackTime = simTime();
    endServiceEvent = new cMessage("endService");
    bufferSizeVector.setName("Buffer Size");
    feedbackRecord.setName("Feedback packets count");
}

void Lnk::finish() {
    recordScalar("Feedback packets count", feedbackRecord.getValuesStored());
}

void Lnk::handleMessage(cMessage *msg) {

    if (msg == endServiceEvent) {
        if (!buffer.isEmpty()) {
            // dequeue
            Packet* pkt = (Packet*) buffer.pop();
            bufferSizeVector.record(buffer.getLength());
            // send
            if(pkt->getType() > 0){
                Packet* copy = (Packet*) pkt->dup();
                send(copy, "toNet$o");
            }
            send(pkt, "toOut$o");
            serviceTime = pkt->getDuration();
            scheduleAt(simTime() + serviceTime, endServiceEvent);
        }
    } else { // msg is a packet
        Packet *pkt = (Packet*) msg;
        if (msg->arrivedOn("toNet$i")) {
            // enqueue

            if(buffer.getLength() >= THRESHOLD &&
               (lastFeedbackTime + serviceTime*50) <= simTime()) {
                lastFeedbackTime = simTime();
                Packet *pkt = new Packet("Feedback",this->getParentModule()->getIndex());
                pkt->setSource(this->getParentModule()->getIndex());
                pkt->setByteLength(par("feedbackPktSize"));
                pkt->setUsedBuffer(buffer.getLength());
                pkt->setType(getIndex()+1);
                buffer.insertAfter(buffer.front(), pkt);
                feedbackRecord.record(1);
            }

            buffer.insert(msg);
            bufferSizeVector.record(buffer.getLength());
            if (!endServiceEvent->isScheduled()) {
                // start the service now
                scheduleAt(simTime() + 0, endServiceEvent);
            }
        } else {
            if (pkt->getKind() == -1) {
                if (msg->arrivedOn("toNet$i")) {
                    send(pkt, "toOut$o");
                } else {
                    send(pkt, "toNet$o");
                }
            } else {
                pkt->setHopCount(pkt->getHopCount()+1);
                send(pkt, "toNet$o");
            }
            //msg is from out, send to net
        }
    }
}
