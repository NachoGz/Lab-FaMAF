#include <string.h>
#include <omnetpp.h>
#include <packet_m.h>

using namespace omnetpp;

class App: public cSimpleModule {
private:
    cMessage *sendMsgEvent;
    simtime_t interArrivalTime;
    int packetByteSize;
    int destination;
    bool initializationComplete;
    cOutVector generatedVector;
    cOutVector delayVector;
    cHistogram delayStats;
public:
    App();
    virtual ~App();
protected:
    virtual void initialize();
    virtual void handleMessage(cMessage *msg);
    virtual void finish();
    void sendPacket();
};

Define_Module(App);

App::App() {
    sendMsgEvent = nullptr;
}

App::~App() {
    cancelAndDelete(sendMsgEvent);
}

void App::initialize() {
    interArrivalTime = par("interArrivalTime");
    packetByteSize = par("packetByteSize");
    destination = par("destination");
    initializationComplete = false;

    sendMsgEvent = new cMessage("sendMsgEvent");

    generatedVector.setName("Generated Packets");
    delayVector.setName("Packet Delay");

    // No programar el evento de envío hasta que se complete la inicialización
}

void App::handleMessage(cMessage *msg) {
    if (msg == sendMsgEvent) {
        if (initializationComplete) {
            // create new packet
            Packet *pkt = new Packet("packet", this->getParentModule()->getIndex());
            pkt->setByteLength(packetByteSize);
            pkt->setSource(this->getParentModule()->getIndex());
            pkt->setDestination(destination);

            // send to net layer
            generatedVector.record(1);
            send(pkt, "toNet$o");

            // compute the new departure time and schedule next sendMsgEvent
            simtime_t departureTime = simTime() + interArrivalTime;
            scheduleAt(departureTime, sendMsgEvent);
        } else {
            // Reprogramar el evento si la inicialización no está completa
            scheduleAt(simTime() + interArrivalTime, sendMsgEvent);
        }
    } else if (msg->getKind() == -2) { // Manejar el mensaje InitComplete
         initializationComplete = true;
         EV << "Initialization complete message received" << endl;

         // Iniciar envio de mensajes
         if (interArrivalTime > 0) {
             sendMsgEvent = new cMessage("sendEvent");
             scheduleAt(simTime() + interArrivalTime, sendMsgEvent);
         }
         delete msg;
    } else {
        // compute delay and record statistics
        simtime_t delay = simTime() - msg->getCreationTime();
        delayStats.collect(delay);
        delayVector.record(delay);
        // delete msg
        delete (msg);
    }
}

void App::sendPacket() {
    Packet *pkt = new Packet("DataPacket");
    pkt->setSource(getParentModule()->getIndex());
    pkt->setDestination(destination);
    pkt->setByteLength(packetByteSize);
    send(pkt, "toNet$o");
}

void App::finish() {
    recordScalar("Packet Delay", delayStats.getMean());
}
