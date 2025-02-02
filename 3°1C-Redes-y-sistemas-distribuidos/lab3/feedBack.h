#include <omnetpp.h>
using namespace omnetpp;

class FeedbackPkt: public cPacket {
private:
    long remainingBuffer;
public:
    FeedbackPkt();
    virtual ~FeedbackPkt();
    void setRemainingBuffer(long l);
    long getRemainingBuffer();
protected:
    virtual void initialize();
};
