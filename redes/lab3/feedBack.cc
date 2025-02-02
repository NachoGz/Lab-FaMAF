#ifndef FEEDBACK
#define FEEDBACK

#include <string.h>
#include <omnetpp.h>

#include "feedBack.h"

using namespace omnetpp;

FeedbackPkt::FeedbackPkt() {
}

FeedbackPkt::~FeedbackPkt() {
}

void FeedbackPkt::initialize() {
    remainingBuffer = 0;
}

long FeedbackPkt::getRemainingBuffer() {
    return remainingBuffer;
}

void FeedbackPkt::setRemainingBuffer(long l){
    remainingBuffer = l;
}

#endif
