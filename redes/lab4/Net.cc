#ifndef NET
#define NET

#include <string.h>
#include <omnetpp.h>
#include <packet_m.h>
#include <vector>

using namespace omnetpp;

class Net: public cSimpleModule {
private:
    cOutVector sentPackets;
    cOutVector recPackets;
    cOutVector recFeedback;
    cOutVector hopCount;
    std::vector<long> Nodes;
    bool initializationComplete;
    int n_nodes;
public:
    Net();
    virtual ~Net();
protected:
    virtual void initialize();
    virtual void finish();
    virtual void handleMessage(cMessage *msg);
    void sendInitializationComplete(int n_nodes);
};

Define_Module(Net);

#endif /* NET */

Net::Net() {
}

Net::~Net() {
}

void Net::initialize() {
    initializationComplete = false;
    n_nodes = 0;

    sentPackets.setName("Sent Packets");
    recPackets.setName("Received Packets");
    hopCount.setName("Hop Count");
    recFeedback.setName("FeedbackPkt Received");

    int nodeid = this->getParentModule()->getIndex();
    // Iniciar el mensaje EchoPacket en el primer nodo
    if (nodeid == 0) {
        EV << "Node " << nodeid << ": Sending initial EchoPacket" << endl;
        Packet *pkt = new Packet("EchoPacket");
        pkt->setHopCount(0);
        pkt->setKind(-1);
        pkt->setSource(nodeid);
        send(pkt, "toLnk$o", 0);
    }
}

void Net::finish() {
}

void Net::handleMessage(cMessage *msg) {
    Packet *pkt = check_and_cast<Packet *>(msg);
    int current_node = this->getParentModule()->getIndex();
    recPackets.record(1);

    if (pkt->getKind() == -1) {
        EV << "Node " << current_node << ": Received EchoPacket with hopCount " << pkt->getHopCount() << endl;

        // Incrementar el contador de nodos únicos
        n_nodes = std::max(n_nodes, pkt->getHopCount() + 1);
        EV << "Current number of nodes: " << n_nodes << endl;

        if (current_node == 0) {
            EV << "Node " << current_node << ": Initialization Complete" << endl;
            initializationComplete = true;
            Nodes.resize(n_nodes); // Redimensionar el vector Nodes
            EV << "Nodes vector length: " << Nodes.size() << endl;
            sendInitializationComplete(n_nodes);
            delete pkt;
        } else {
            pkt->setHopCount(pkt->getHopCount() + 1);
            EV << "Node " << current_node << ": Incrementing hopCount to " << pkt->getHopCount() << " and forwarding to next node" << endl;
            send(pkt, "toLnk$o", 0); // Enviar al siguiente nodo en el anillo
        }
    } else if (pkt->getKind() == -2) {
        send(pkt, "toApp$o");
        if (current_node == 0) {
        } else {
            Nodes.resize(pkt->getN_nodes());
            EV << "Node " << current_node << ": Received InitComplete message" << endl;
            initializationComplete = true;
            send(pkt->dup(), "toLnk$o", 0);
        }
    } else {
        // Si este nodo es el destino final, enviar a App
        if (pkt->getDestination() == current_node) {
            hopCount.record(pkt->getHopCount());
            send(msg, "toApp$o");
        } else {
            bool destination;
            int mask = 1;
            if (current_node < Nodes.size()) {
                Nodes[pkt->getSource()] = pkt->getUsedBuffer();
            }
            int distance = current_node - pkt->getDestination();
            if (pkt->getHopCount() > 6) {
                send(msg, "toLnk$o", 1);
            } else {
                if (current_node == 0 || current_node == 7) {
                    destination = pkt->getDestination() < n_nodes / 2;
                    if ((Nodes[current_node - 1] - Nodes[current_node + 1]) > 100) {
                        mask = 1;
                    } else if ((Nodes[current_node + 1] - Nodes[current_node - 1]) > 100) {
                        mask = 0;
                    }
                    send(msg, "toLnk$o", destination & mask);
                } else {
                    destination = pkt->getDestination() > current_node;
                    if ((Nodes[current_node - 1] - Nodes[current_node + 1]) > 100) {
                        mask = 1;
                    } else if ((Nodes[current_node + 1] - Nodes[current_node - 1]) > 100) {
                        mask = 0;
                    }
                    send(msg, "toLnk$o", destination & mask);
                }
            }
            sentPackets.record(1);
        }
    }
}

void Net::sendInitializationComplete(int n_nodes) {
    Packet *initCompletePkt = new Packet("InitComplete");
    initCompletePkt->setKind(-2); // Usamos un nuevo kind para diferenciarlos
    initCompletePkt->setN_nodes(n_nodes);
    send(initCompletePkt, "toLnk$o", 0);
}
