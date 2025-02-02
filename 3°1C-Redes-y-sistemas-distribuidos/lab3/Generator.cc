#ifndef GENERATOR
#define GENERATOR

#include <string.h>
#include <omnetpp.h>

using namespace omnetpp;

class Generator : public cSimpleModule {
private:
    cMessage *sendMsgEvent;
    cStdDev transmissionStats;
    cOutVector packetGen;
public:
    Generator();
    virtual ~Generator();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
};
Define_Module(Generator);

Generator::Generator() {
    sendMsgEvent = NULL;
}

Generator::~Generator() {
    cancelAndDelete(sendMsgEvent);
}

void Generator::initialize() {
    transmissionStats.setName("TotalTransmissions");
    // create the send packet
    sendMsgEvent = new cMessage("sendEvent");
    // schedule the first event at random time
    scheduleAt(par("generationInterval"), sendMsgEvent);
    packetGen.setName("packetGen");
}

void Generator::finish() {
}

void Generator::handleMessage(cMessage *msg) {

    // create new packet
    // cMessage *pkt = new cMessage("packet");
    cPacket *pkt = new cPacket("packet");
    pkt->setByteLength(par("packetByteSize").doubleValue());
    
    // send to the output
    send(pkt, "out");
    packetGen.record(1);
    transmissionStats.collect(1);
    // compute the new departure time
    simtime_t departureTime = simTime() + par("generationInterval");
    // schedule the new packet generation
    scheduleAt(departureTime, sendMsgEvent);
}

#endif /* GENERATOR */
